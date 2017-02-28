#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#ifdef _OSF_SOURCE
#include <fp_class.h>
#endif
#include <atFunctions.h>
#include "ascatool.h"
#include "ytlm.h"
#include "ylog.h"
#include "yfrf.h"

#define OUT_MSG	stdout_MSG
#define ERR_MSG	stderr_MSG

typedef int Long;			/* integer*4 */
typedef unsigned uLong;		/* integer*4 */

enum BitDef {
	bB0=0x80, bB1=0x40, bB2=0x20, bB3=0x10,
	bB4=0x08, bB5=0x04, bB6=0x02, bB7=0x01
};

#define DEFAULT_FRF_PATH	"/frf:/nagatsuki/f1/frf"
#define DEFAULT_FA_PATH		"/frf/attitude:/hazuki/d1/attitude"

static char*
frfFileOpen(double ascatime)
{
	static struct ascatool_frf_files *files = NULL;
	int i;
	if ( null(files) ) {
		char *p = getenv(ENV_FRFDIR);
		if ( null(p) ) p = DEFAULT_FRF_PATH;
		files = ascatool_read_frf_dir(p, 't');
		if ( null(files) ) return NULL;
	}
	for (i = 0; !null(files[i].fn) && files[i].starttime <= ascatime; i++) ;
	if ( 0 == i ) return NULL;
	return files[i-1].fn;
}

static int
FitsNextBlock(FILE *fp)
{
	long skip;
	skip = ftell(fp);
	if ( skip < 0 ) return -1;
	skip %= FTRECSIZE;
	if ( skip ) {
		return fseek(fp, FTRECSIZE-skip, SEEK_CUR);
	}
	return 0;
}

static FitsHead*
ReadFitsHead(FILE *fp)
{
	int i, n;
	FitsHead *fh;
	n = 0;
	fh = malloc(FTRECSIZE);
	if ( null(fh) ) return NULL;
	for (;;) {
		if ( 1 != fread(fh[n].h, FTRECSIZE, 1, fp) ) {
			free(fh);
			return NULL;
		}
		for (i = 0; i < FTRECSIZE/sizeof(*fh); i++) {
			if ( 0 == strncmp("END     ", fh[n++].h, 8) ) break;
		}
		if ( i < FTRECSIZE/sizeof(*fh) ) break;		/* end of header */
		fh = realloc(fh, n*sizeof(*fh)+FTRECSIZE);
		if ( null(fh) ) return NULL;
	}
	fh = realloc(fh, n*sizeof(*fh)+1);
	if ( null(fh) ) return NULL;
	fh[n].h[0] = '\0';
	return fh;
}

static struct yfrf_global frf;

struct yfrf_global*
get_yfrf_global(void)
{
	return &frf;
}

static double
ReadSFtime(unsigned long n)
{
	double t;
	if ( n < 0 ) n = 0;
	if ( frf.nrec <= n ) n = frf.nrec - 1;
	if ( !null(frf.idx) ) {
		memcpy(&t, frf.idx[n].mtime, sizeof(t));
		return ascatool_hostdouble(t);
	}
	if ( fseek(frf.fp, frf.top+n*frf.recsiz, SEEK_SET) < 0 ) {
		ERR_MSG("FRF seek error");
	}
	if ( 1 != fread(&t, sizeof(t), 1, frf.fp) ) {
		ERR_MSG("FRF read error");
	}
	return ascatool_hostdouble(t);
}

static int
timesearch(double tz)
{
	long pos;
	unsigned long ns, nm, ne;
	double ts, tm, te;
	ns = 0;
	ne = frf.nrec - 1;
	ts = ReadSFtime(ns);
	te = ReadSFtime(ne);
	if ( te < tz ) return 4;
	if ( ts < tz ) {
		while ( 1 < ne - ns ) {
			nm = ( ns + ne ) / 2;
			tm = ReadSFtime(nm);
			if ( tm < tz ) {
				ns = nm;
				ts = tm;
			} else {
				ne = nm;
				te = tm;
			}
		}
	}
	frf.sfn = ns;
	pos = null(frf.idx) ? ns * frf.recsiz : ntohl(frf.idx[ns].pos);
	if ( fseek(frf.fp, frf.top+pos, SEEK_SET) < 0 ) {
		ERR_MSG("FRF seek error");
		return 4;
	}
	return 0;
}

int
ascatool_frfopen(char *fn)
{
	int i;
	char *p;
	AtTime ti;
	double ascatime, sec;
	p = ascatool_splitpath(fn);
	if ( 'f' == p[0] && 't' == p[1] ) {
		frf.fn = ascatool_expand_file_name(fn);
		if ( null(frf.fn) ) return 1;
		frf.fp = fopen(frf.fn, "rb");
		if ( null(frf.fp) && p == fn ) {
			int len = strlen(fn);
			p = getenv(ENV_FRFDIR);
			if ( null(p) ) p = DEFAULT_FRF_PATH;
			while ( *p && null(frf.fp) ) {
				for (i = 0; p[i] && ':' != p[i]; i++) ;
				frf.fn = realloc(frf.fn, i+len+2);
				if ( null(frf.fn) ) break;
				strncpy(frf.fn, p, i);
				p += i;
				if ( *p ) p++;
				if ( '/' != frf.fn[i-1] ) frf.fn[i++] = '/';
				strcpy(frf.fn+i, fn);
				frf.fp = fopen(frf.fn, "rb");
			}
		}
		ascatime = frf.endtime = -1;
	} else {
		p = ascatool_scanYYMMDDHHMMSS(fn, &ti); 
		if ( p == fn ) {
			ERR_MSG("%s: wrong format", fn);
			return 4;
		}
		ascatime = attime2asca(ti);
		frf.endtime = -1;
		switch ( *p++ ) {
		case '-':
			p = ascatool_scanYYMMDDHHMMSS(fn=p, &ti);
			if ( p != fn ) {
				frf.endtime = attime2asca(ti);
			}
			break;
		case '+':
			p = ascatool_scanTimeOffset(fn=p, &sec);
			if ( p != fn ) {
				frf.endtime = ascatime + sec;
			}
			break;
		}
		frf.fn = frfFileOpen(ascatime);
		frf.fp = fopen(frf.fn, "rb");
	}
	if ( null(frf.fp) ) return 1;
	frf.h1 = ReadFitsHead(frf.fp);
	if ( null(frf.h1) ) {
		ERR_MSG("not fits file");
		return 4;
	}
	/*for (i = 0; frf.h1[i].h[0]; i++) MSG("%.80s\n", frf.h1[i].h);*/
	frf.h2 = ReadFitsHead(frf.fp);
	if ( null(frf.h2) ) {
		ERR_MSG("not fits file");
		return 4;
	}
	frf.gisfrf = 0;
	for (i = 0; frf.h2[i].h[0]; i++) {
		if ( 0 == strncmp("NAXIS1  ", frf.h2[i].h, 8) ) {
			frf.recsiz = atoi(&frf.h2[i].h[10]);
		} else if ( 0 == strncmp("NAXIS2  ", frf.h2[i].h, 8) ) {
			frf.nrec = atoi(&frf.h2[i].h[10]);
		} else if ( 0 == strncmp("EXTNAME ", frf.h2[i].h, 8) ) {
			if ( 0 == strncmp("'GIS_TELEMETRY'", frf.h2[i].h+10, 15) ) {
				OUT_MSG("This is GIS specific FRF file");
				frf.gisfrf = 1;
			} elif ( 0 == strncmp("'GIS_PACK_DATA'", frf.h2[i].h+10, 15) ) {
				OUT_MSG("This is packed GIS FRF file");
				frf.gisfrf = 2;
			} elif ( 0 == strncmp("'GIS_DATA'", frf.h2[i].h+10, 10) ) {
				OUT_MSG("This is small GIS FRF file");
				frf.gisfrf = 3;
			}				
		}
	}
	frf.top = ftell(frf.fp);
	frf.frn = 0;
	frf.idx = NULL;
	if ( 2 == frf.gisfrf ) {
		if ( fseek(frf.fp, frf.recsiz*frf.nrec-sizeof(frf.pstat), SEEK_CUR) ) {
			ERR_MSG("FRF seek error");
			return 4;
		}
		if ( 1 != fread(&frf.pstat, sizeof(frf.pstat), 1, frf.fp) ) {
			ERR_MSG("FRF read error");
			return 4;
		}
	}
	if ( 0 == fseek(frf.fp, frf.top+frf.recsiz*frf.nrec, SEEK_SET) &&
		 0 == FitsNextBlock(frf.fp) &&
		 !null(frf.h3 = ReadFitsHead(frf.fp)) ) {
		for (i = 0; frf.h3[i].h[0]; i++) {
			if ( 0 == strncmp("NAXIS1  ", frf.h3[i].h, 8) ) {
				frf.recsiz = atoi(&frf.h3[i].h[10]);
			} else if ( 0 == strncmp("NAXIS2  ", frf.h3[i].h, 8) ) {
				frf.nrec = atoi(&frf.h3[i].h[10]);
			}
		}
		frf.idx = malloc(sizeof(*frf.idx)*frf.nrec);
		unless ( null(frf.idx) ) {
			int nrec;
			nrec = fread(frf.idx, sizeof(*frf.idx), frf.nrec, frf.fp);
			if ( frf.nrec == nrec ) {
				OUT_MSG("Index table has been found");
			} else {
				free(frf.idx);
				frf.idx = NULL;
			}
		}
	}
#if 0
	if ( !null(frf.idx) ) {
		for (frf.sfn = 0; frf.sfn < frf.nrec; frf.sfn++) {
			double t;
			FRFINDEX *idx = &frf.idx[frf.sfn];
			memcpy(&t, idx->mtime, sizeof(t));
			if ( asca < ascatool_hostdouble(t) ) {
				if ( fseek(frf.fp, frf.top+ntohl(idx->pos), SEEK_SET) < 0 ) {
					ERR_MSG("FRF seek error");
					return 4;
				} else {
					return 0;
				}
			}
		}
		ERR_MSG("Index table search error");
		return 1;
	}
#endif
	if ( null(frf.idx) && frf.gisfrf ) {
		ERR_MSG("Index table read error");
		return 4;
	}
	if ( ascatime < 0 ) {
		int pos;
		frf.sfn = 0;
		pos = null(frf.idx) ? 0 : ntohl(frf.idx[0].pos);
		if ( fseek(frf.fp, frf.top+pos, SEEK_SET) < 0 ) {
			ERR_MSG("FRF seek error");
			return 4;
		}
		return 0;
	} else {
		return timesearch(ascatime);
	}
}

static int
read_gisrec(void)
{
	static GISREC grec;
	static char wdmap[] = {
		16, 17, 18, 19,
		32, 33, 34, 35,
		48, 49, 50, 51,
		64, 65, 66, 67,
		80, 81, 82, 83,
		96, 97, -1
	};
	FR *frp;
	int i, j, len;
	len = sizeof(grec) - sizeof(grec.gdata);
	if ( 1 != fread(&grec, len, 1, frf.fp) ) {
		ERR_MSG("GIS specific FRF read error");
		return 4;
	}
	memcpy(frf.frec.mtime, grec.mtime, sizeof(frf.frec.mtime));
	memset(frf.frec.sf, 0, sizeof(frf.frec.sf));
	for (i = 0; i < SFFR; i++) {
		frp = &frf.frec.sf[i];
		(*frp)[0] = 0xfa;
		(*frp)[1] = 0xf3;
		(*frp)[2] = 0x20;
		(*frp)[3] = grec.fi[i];
		for (j = 0; 0 <= wdmap[j]; j++) {
			(*frp)[wdmap[j]] = grec.comwd[i][j];
		}
		for (j = 0; j < 8; j++) {
			if ( grec.map[i] & (0x80>>j) ) {
				if ( 1 != fread(&(*frp)[j*16+4], 4, 1, frf.fp) ) {
					ERR_MSG("GIS specific FRF read error");
					return 4;
				}
			}
		}
	}
	return 0;
}

static int
unpack_gis_event(FILE *fp, SF *sfp, WD meth, WD *bcos, char *errmsg)
{
	static PACK_DATA pack;
	int i, j;
	meth &= 3;
	if ( 3 == meth ) return 0;		/* SYNC off */
	for (i = 0; i < SFFR; i++) {
		FR *frp = &(*sfp)[i];
		(*frp)[0] = 0xfa;
		(*frp)[1] = 0xf3;
		(*frp)[2] = 0x20;
		(*frp)[3] = (*sfp)[0][3] + i;
		if ( NULL != bcos ) {
			(*frp)[97] = bcos[(*frp)[3]%(2*SFFR)];
		}
	}
	if ( 1 == meth ) {
		PACK_METH_1 *p1 = &pack.m1;
		if ( 1 != fread(&p1->map, sizeof(p1->map), 1, fp) ) {
			ERR_MSG(errmsg);
			return 4;
		}
		for (i = 0; i < SFFR; i++) {
			FR *frp = &(*sfp)[i];
			for (j = 0; j < 8; j++) {
				if ( p1->map[i] & (0x80>>j) ) {
					if ( 1 != fread(&(*frp)[j*16+4], 4, 1, fp) ) {
						ERR_MSG(errmsg);
						return 4;
					}
				}
			}
		}
	} elif ( meth ) {
		PACK_METH_2 *p2 = &pack.m2;
		if ( 1 != fread(&p2->ndata, sizeof(p2->ndata), 1, fp) ) {
			ERR_MSG(errmsg);
			return 4;
		}
		for (i = 0; i < 2; i++) {
			for (j = 0; j < p2->ndata[i]; j++) {
				WD pos = fgetc(fp);
				FR *frp = &(*sfp)[i*32+pos/8];
				if ( 1 != fread(&(*frp)[(pos%8)*16+4], 4, 1, fp) ) {
					ERR_MSG(errmsg);
					return 4;
				}
			}
		}
	}
	return 0;
}

static int
read_packrec(void)
{
	static char err[] = "packed GIS FRF read error";
	static struct {
		WD hk[2];	/* HK temperature */
		WD meth;	/* b0-3:bit-rate, b4-5:FI, method 0:none, 3:sync */
		int pack;	/* for alignment */
	} pack;
	int i, j, len;
	len = sizeof(pack) - sizeof(pack.pack);
	if ( 1 != fread(&pack, len, 1, frf.fp) ) {
		ERR_MSG(err);
		return 4;
	}
	memcpy(frf.frec.mtime, frf.idx[frf.sfn].mtime, sizeof(frf.frec.mtime));
	memset(frf.frec.sf, 0, sizeof(frf.frec.sf));
	frf.frec.sf[42][33] = pack.hk[0];
	frf.frec.sf[44][33] = pack.hk[1];
	frf.frec.sf[15][32] = pack.meth & 0xf0;
	frf.frec.sf[0][3] = ( pack.meth / 4 ) * SFFR;
	frf.frec.sf[26][33] = frf.pstat.hk[0];
	frf.frec.sf[27][33] = frf.pstat.hk[1];
	frf.frec.sf[28][33] = frf.pstat.hk[2];
	frf.frec.sf[29][33] = frf.pstat.hk[3];
	frf.frec.sf[30][33] = frf.pstat.hk[4];
	frf.frec.sf[31][33] = frf.pstat.hk[5];
	frf.frec.sf[43][33] = frf.pstat.hk[6];
	frf.frec.sf[1][32] = frf.pstat.stat[0];
	frf.frec.sf[17][32] = frf.pstat.stat[1];
	frf.frec.sf[33][32] = frf.pstat.stat[2];
	i = unpack_gis_event(frf.fp, &frf.frec.sf, pack.meth, frf.pstat.bcos, err);
	return i;
}

union lWD {
	WD w[SFFR];
	Long l[SFFR/4];
};

WD __bcostmpl[7][SFFR] = {
	{		/* even SF spread discri on */
		192,160,159,159,218,218, 52, 52,247, 48,  0,  0,250,250, 90, 90,
		  0,  0,  1,  1,255,255,255,255,255,255,255,255,  0,  0,  0,  0,
		  0,  0,  0,  0,120,120,120,120,121,122,122,121,255,255,255,255,
		  0,  0,255,255,  0,  0,255,255,  0,  0,  0,  0,  0,  0,  1,  1
	}, {	/* odd SF spread discri on */
		192,160,159,159,218,218, 52, 52,247, 48,  0,  0,250,250, 90, 90,
		  0,  0,  1,  1,  0,  0,255,255,  0,  0,  0,  0,  0,  0,  0,  0,
		 80, 80,239,232,226,224,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
		  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1
	}, {	/* even SF spread discri off */
		192,160,159,159,218,218, 52, 52,247, 48,  0,  0,250,250, 88, 88,
		  0,  0,  1,  1,255,255,255,255,255,255,255,255,  0,  0,  0,  0,
		  0,  0,  0,  0,120,120,120,120,121,122,122,121,255,255,255,255,
		  0,  0,255,255,  0,  0,255,255,  0,  0,  0,  0,  0,  0,  1,  1
	}, {	/* odd SF spread discri off */
		192,160,159,159,218,218, 52, 52,247, 48,  0,  0,250,250, 88, 88,
		  0,  0,  1,  1,  0,  0,255,255,  0,  0,  0,  0,  0,  0,  0,  0,
		 80, 80,240,240,128,128,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
		  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1
	}, {	/* GIS off */
		  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
		  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
		  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
		  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
	}, {	/* sync off */
		 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
		 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
		 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
		 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17
	}, {	/* ??? */
		255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
		255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
		255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
		255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
	}
};

static int
unpack_bcos(FILE *fp, SF *sfp)
{
	static int nbcos = SFFR/4;
	int i, c;
	union lWD bcos;
	c = fgetc(fp);
	if ( EOF == c ) return -1;
	if ( c % 8 ) {
		/* packed */
		memcpy(&bcos, &__bcostmpl[c%8-1], SFFR);
		if ( c / 8 ) {
			WD ww[2];
			unsigned mask;
			union lWD buf;
			Long *p = buf.l;
			if ( 1 != fread(ww, 2, 1, fp) ) return -1;
			if ( 4 != fread(p, c/8, 4, fp) ) return -1;
			mask = ( ww[0] << 8 ) + ww[1];
			for (i = 0; mask; i++) {
				if ( mask & 1 ) bcos.l[i] = *p++;
				mask >>= 1;
			}
		}
	} else {
		/* not packed */
		if ( 1 != fread(&bcos.w[1], SFFR-1, 1, fp) ) return -1;
		bcos.w[0] = bcos.w[9];
		bcos.w[9] = c;
	}
	for (i = 0; i < SFFR; i++) (*sfp)[i][97] = bcos.w[i];
	return 0;
}

/* bit field operation till 16bit */
static unsigned
bbget(WD *buf, int *posp, int nb)
{
	Long tmpb;
	int pos = *posp;
	*posp += nb;
	buf = &buf[pos/8];
	pos &= 7;
	tmpb = buf[0];
	tmpb <<= 16;
	tmpb |= (buf[1]<<8) | buf[2];
	tmpb <<= pos;
	tmpb &= 0xffffff;
	tmpb >>= 24 - nb;
	return tmpb;
}

static void
unpack_one_gismon(int nmon, int monb, int mon[], WD *buf, int *pos, int nb)
{
	int i, mask;
	mon[0] = bbget(buf, pos, monb);
	mask = ( 1 << monb ) - 1;
	for (i = 1; i < nmon; i++) {
		mon[i] = ( mon[i-1] + bbget(buf, pos, nb) ) & mask;
	}
}

static int
unpack_gismon(FILE *fp, SF *sfp)
{
	int i, j, n, len, pos, nbs[16+2+1], mon[SFFR];
	WD buf[512];
	len = 16*4 + 2*4 + 1*5;
	if ( 1 != fread(buf, (len+7)/8, 1, fp) ) return -1;
	n = pos = 0;
	for (i = 0; i < 16; i++) {
		len += 8 + ( nbs[n++] = bbget(buf, &pos, 4) ) * (SFFR/4-1);
	}
	for (i = 0; i < 2; i++) {
		len += 8 + ( nbs[n++] = bbget(buf, &pos, 4) ) * (SFFR/2-1);
	}
	len += 16 + ( nbs[n++] = bbget(buf, &pos, 5) ) * (SFFR-1);
	if ( 1 != fread(&buf[(pos+7)/8], (len+7)/8-(pos+7)/8, 1, fp) ) return -1;
	for (i = n = 0; i < 4; i++) {
		static char mon_word[4] = { 51, 65, 66, 67 };
		int wpos = mon_word[i];
		unpack_one_gismon(SFFR/4, 8, mon, buf, &pos, nbs[n++]);
		unpack_one_gismon(SFFR/4, 8, &mon[SFFR/4], buf, &pos, nbs[n++]);
		unpack_one_gismon(SFFR/4, 8, &mon[2*SFFR/4], buf, &pos, nbs[n++]);
		unpack_one_gismon(SFFR/4, 8, &mon[3*SFFR/4], buf, &pos, nbs[n++]);
		for (j = 0; j < SFFR/4; j++) {
			(*sfp)[4*j+0][wpos] = mon[j];
			(*sfp)[4*j+1][wpos] = mon[SFFR/4+j];
			(*sfp)[4*j+2][wpos] = mon[2*SFFR/4+j];
			(*sfp)[4*j+3][wpos] = mon[3*SFFR/4+j];
		}
	}
	unpack_one_gismon(SFFR/2, 8, mon, buf, &pos, nbs[n++]);
	unpack_one_gismon(SFFR/2, 8, &mon[SFFR/2], buf, &pos, nbs[n++]);
	for (j = 0; j < SFFR/2; j++) {
		(*sfp)[2*j+0][64] = mon[j];
		(*sfp)[2*j+1][64] = mon[SFFR/2+j];
	}
	/*printf("nbs[n]=%d\n", nbs[n]);*/
	unpack_one_gismon(SFFR, 16, mon, buf, &pos, nbs[n++]);
	for (j = 0; j < SFFR; j++) {
		(*sfp)[j][48] = mon[j] >> 8;
		(*sfp)[j][49] = mon[j];
		/*printf("%d%c", mon[j], (SFFR-1==j)?'\n':',');*/
	}
	return 0;
}

static int
read_Ggisrec(void)
{
	static char errmsg[] = "small GIS FRF read error";
	static GGISREC grec;
	int i, j, len;
	FILE *fp = frf.fp;
	if ( 1 != fread(&grec, sizeof(grec), 1, frf.fp) ) {
		ERR_MSG(errmsg);
		return 4;
	}
	memcpy(frf.frec.mtime, frf.idx[frf.sfn].mtime, sizeof(frf.frec.mtime));
	memset(frf.frec.sf, 0, sizeof(frf.frec.sf));
	frf.frec.sf[8*0+2][34] = frf.frec.sf[8*1+2][34] =
	frf.frec.sf[8*2+2][34] = frf.frec.sf[8*3+2][34] =
	frf.frec.sf[8*4+2][34] = frf.frec.sf[8*5+2][34] =
	frf.frec.sf[8*6+2][34] = frf.frec.sf[8*7+2][34] =
	frf.frec.sf[0][32] = grec.dpmode&(bB0|bB3|bB4|bB5|bB6|bB7);	/* DP mode */
	frf.frec.sf[49][34] = (grec.dpmode<<1)&(bB0|bB1);	/* RBM status flag */
	frf.frec.sf[01][32] = grec.status[0];
	frf.frec.sf[17][32] = grec.status[1];
	frf.frec.sf[33][32] = grec.status[2];
	frf.frec.sf[26][33] = grec.hk[0];	/* S2 HVLV */
	frf.frec.sf[27][33] = grec.hk[1];	/* S2 HVHV */
	frf.frec.sf[28][33] = grec.hk[2];	/* S2 HVHI */
	frf.frec.sf[29][33] = grec.hk[3];	/* S3 HVLV */
	frf.frec.sf[30][33] = grec.hk[4];	/* S3 HVHV */
	frf.frec.sf[31][33] = grec.hk[5];	/* S3 HVHI */
	frf.frec.sf[42][33] = grec.hk[6];	/* S2 temp */
	frf.frec.sf[43][33] = grec.hk[7];	/* RBMtemp */
	frf.frec.sf[44][33] = grec.hk[8];	/* S3 temp */
	frf.frec.sf[15][32] = grec.meth & 0xf0;			/* bit rate */
	frf.frec.sf[00][03] = (grec.meth/4) * SFFR;		/* FI */
	if ( unpack_bcos(fp, &frf.frec.sf) || unpack_gismon(fp, &frf.frec.sf) ) {
		ERR_MSG(errmsg);
		return 4;
	}
	return unpack_gis_event(fp, &frf.frec.sf, grec.meth, NULL, errmsg);
}


int
ascatool_frfread(FR *frp, ADTIME *time_ptr)
{
	static double t, dt;
	ASCATIME ascatime;
	AtTime attime;
	int class;
	
	if ( 0 == frf.frn ) {
		if ( frf.nrec <= frf.sfn ) return 1;
		if ( 1 == frf.gisfrf ) {
			if ( read_gisrec() ) return 4;
		} elif ( 2 == frf.gisfrf ) {
			if ( read_packrec() ) return 4;
		} elif ( 3 == frf.gisfrf ) {
			if ( read_Ggisrec() ) return 4;
		} else if ( 1 != fread(&frf.frec, sizeof(frf.frec), 1, frf.fp) ) {
			ERR_MSG("FRF read error");
			return 4;
		}
		frf.sfn++;
		frf.eti = (frf.frec.eti[0]<<8) + frf.frec.eti[1];
		frf.eti <<= 16;
		frf.eti += (frf.frec.eti[2]<<8) + frf.frec.eti[3];
		memcpy(&t, frf.frec.mtime, sizeof(t));
		t = ascatool_hostdouble(t);
#ifdef _OSF_SOURCE
		class = fp_class(t);
		if ( FP_POS_NORM != class ) {
			ERR_MSG("invalid ascatime (class=%d) found at sf=%d.", class, frf.sfn);
			t = 0;
		}
#endif
		if ( 0 < frf.endtime && frf.endtime < t ) return 1;
		switch ( frf.frec.sf[15][32] ) {
		case BITL: dt = 1.0; break;
		case BITM: dt = 0.25; break;
		case BITH: dt = 0.03125; break;
		default: dt = 0.03125;
		}
	}
	memcpy(frp, frf.frec.sf[frf.frn], sizeof(*frp));
	
	ascatime = t + dt*frf.frn;
	if ( TIME_IS_ASCATIME == _TIME_TYPE_ ) {
		*(ASCATIME*)time_ptr = ascatime;
	} else {
		if ( TIME_IS_MJD == _TIME_TYPE_ ) {
			*(MJD*)time_ptr = asca2mjd(ascatime);
		} else {
			asca2attime(ascatime, &attime);

			/* There is no Y2K problem here: v4.3
			 * the function call is traced back to sfGetASCA(), 
			 * an ascatime is eventually returned, and Y2k issue is handled there */
			attime.yr %= 100;
			if ( TIME_IS_ATTIME == _TIME_TYPE_ ) {
				*(AtTime*)time_ptr = attime;
			} else {
				time_ptr->year = attime.yr;
				time_ptr->month = attime.mo;
				time_ptr->day = attime.dy;
				time_ptr->hour = attime.hr;
				time_ptr->minute = attime.mn;
				time_ptr->second = attime.sc;
				time_ptr->msec = attime.ms;
			}
		}
	}
	
	frf.frn++;
	frf.frn %= SFFR;
	return 0;
}

int
ascatool_frfclose(void)
{
	fclose(frf.fp);
	if ( !null(frf.h1) ) free(frf.h1);
	if ( !null(frf.h2) ) free(frf.h2);
	if ( !null(frf.h3) ) free(frf.h3);
	if ( !null(frf.idx) ) free(frf.idx);
	return 0;
}

long
ascatool_frfsfnum(void)
{
	return frf.sfn;
}


/*
int
main(int argc, char **argv)
{
	FR fr;
	AtTime ti;
	if ( argc < 2 ) {
		OUT_MSG("usage: %s yymmddhhmmss", argv[0]);
		exit(1);
	}
	frfopen(argv[1]);
	for (;;) {
		frfread(&fr, &ti);
		printf("%02u/%02u/%02u %02u:%02u:%02u.%03.0f FI=%02X\n",
			   ti.yr, ti.mo, ti.dy, ti.hr, ti.mn, ti.sc, ti.ms, fr[3]);
		getchar();
	}
}
*/
