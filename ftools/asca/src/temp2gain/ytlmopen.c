/*
 * ytlmopen.c
 *
 * telemetry file access routine, by frame
 *
 */

/*#define QLSEEK		/* Don't close data file at QL */
/*#define QLSEEK_CNTL	/* Don't close cntl file at QL */

unsigned _QLSKIP = 0;	/* if not zero, skip frames at QL */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <atFunctions.h>
#include "ascatool.h"
#include "ytlm.h"

#define OUT_MSG   stdout_MSG
#define ERR_MSG   stderr_MSG

#define xmalloc(siz)	((void*)malloc(siz))
#define xrealloc(p,siz)	((void*)realloc(p, siz))
#define bcd2int(Bcd)	((((Bcd)>>4)&0xf)*10+((Bcd)&0xf))

#define	CTLFILE	"cntl"
#define	RLFILE	"real/cntl"
#define	PBFILE	"stor/cntl"
#define	MTFILE	"bkup/cntl"
#define DEFPATH			"./:/astrod/"
#define PATHDELIM		':'
#define dfclose(fp)		(ytlm.compress?pclose(fp):fclose(fp))

static struct {
	gBOOLEAN ql;			/* TRUE, if opened as QL mode */
	gBOOLEAN onefile;		/* TRUE, if pass open */
	gBOOLEAN compress;		/* TRUE, if data file is compressed */
	char *cfn;				/* normally "/astrod/[real,bkup,stor]/cntl" */
	struct kanrirec *krec;	/* contents of kanri-records */
	long jrecp;				/* current jyoho-records */
	long jrecsize;			/* sizeof jyoho-records */
	struct jyohorec *jrec;	/* contents of jyoho-records */
	FILE *fp;				/* file pointer to a current reading file */
	long frn;				/* number of frames read */
	AtTime endtim;			/* end time */
	char *sfuse;			/* flags if use this SF or not */
} ytlm = {
	gFALSE, gFALSE, gFALSE,
	NULL, NULL, 0, 0, NULL, NULL, 0,
	{ 1990, 1, 1, 0, 0, 0, 0.0 },
	NULL
};

static struct jyohorec*
allocjrec(void)
{
	struct jyohorec *p;
	if ( null(ytlm.jrec) ) {
		p = xmalloc( sizeof(ytlm.jrec[0])*(size_t)ytlm.krec->recmen );
	} else {
		p = xrealloc(ytlm.jrec,sizeof(ytlm.jrec[0])*(size_t)ytlm.krec->recmen);
	}
	unless ( null(p) ) {
		ytlm.jrecsize = ytlm.krec->recmen;
		ytlm.jrec = p;
	}
	return p;
}

static void
bcd2attime(unsigned char *bcdt, AtTime *att)
{
	att->yr = bcd2int(bcdt[0]);
	att->mo = bcd2int(bcdt[1]);
	att->dy = bcd2int(bcdt[2]);
	att->hr = bcd2int(bcdt[3]);
	att->mn = bcd2int(bcdt[4]);
	att->sc = bcd2int(bcdt[5]);
	att->ms = bcd2int(bcdt[6])*10 + bcd2int(bcdt[7])/10.0;
}

static int
cmpattime(AtTime *t1, AtTime *t2)
{
	if ( t1->yr > t2->yr ) return 1;
	if ( t1->yr < t2->yr ) return -1;
	if ( t1->mo > t2->mo ) return 1;
	if ( t1->mo < t2->mo ) return -1;
	if ( t1->dy > t2->dy ) return 1;
	if ( t1->dy < t2->dy ) return -1;
	if ( t1->hr > t2->hr ) return 1;
	if ( t1->hr < t2->hr ) return -1;
	if ( t1->mn > t2->mn ) return 1;
	if ( t1->mn < t2->mn ) return -1;
	if ( t1->sc > t2->sc ) return 1;
	if ( t1->sc < t2->sc ) return -1;
	if ( t1->ms > t2->ms ) return 1;
	if ( t1->ms < t2->ms ) return -1;
	return 0;
}

static void
addattime(AtTime *ji, double sec)
{
	int isec = (int)sec;
	ji->ms += ( sec - isec );
	ji->sc += isec;
	while ( 1000 <= ji->ms ) {
		ji->ms -= 1000;
		ji->sc++;
	}
	while ( 60 <= ji->sc ) {
		ji->sc -= 60;
		ji->mn++;
	}
	while ( 60 <= ji->mn ) {
		ji->mn -= 60;
		ji->hr++;
	}
	while ( 24 <= ji->hr ) {
		ji->hr -= 60;
		ji->dy++;
	}
}

static void
totday2attime(AtTime *att)
{
  static int _ndays[2][12] = {
    { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
    { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
  };
  int year, *ndays;

  for (;;) {

    /****************************************************
     * A potential Y2K problem,
     * but temp2gain is not calling this function;
     *****************************************************/
    year = att->yr + 1900;

    ndays = _ndays[0];

    if ( (0 == year%4 && year%100) || 0 == year%400 ) 
      ndays = _ndays[1];      /* a leap yr */

    att->mo = 1;
    for (;;) {
      if ( att->dy <= ndays[att->mo-1] ) return;
      att->dy -= ndays[att->mo-1];
      if ( 12 == att->mo ) {
	att->yr++;
	break;
      } else {
	att->mo++;
      }
    }
  }
}

static char*
search_file(char *fn, char *search_path)
{
	static char buf[256];
	char *p, *q;
	p = search_path;
	for (;;) {
		q = buf;
		until ( '\0' == *p || PATHDELIM == *p ) *q++ = *p++;
		strcpy(q, fn);
		if ( 0 == access(buf, R_OK) ) break;
		if ( '\0' == *p ) return NULL;
		p++;
	}
	return buf;
}

static FILE*
dfileopen(char *fn)
{
	int len;
	char buf[256];
	strcpy(buf, ytlm.cfn);
	strcpy(ascatool_splitpath(buf), ascatool_splitpath(fn));
	len = strlen(buf);
	while ( len > 0 ) {
		len--;
		if ( buf[len] > SPC ) break;
		buf[len] = '\0';
	}
	OUT_MSG("opening %s", buf);
	{
		FILE *fp = fopen(buf, "r");
		char buf2[300];
		unless ( null(fp) ) {
			ytlm.compress = gFALSE;
			return fp;
		}
		strcat(buf, ".Z");
		if ( access(buf, R_OK) ) {
			ERR_MSG("dfileopen: %s: open failed", buf);
			return NULL;
		}
		ytlm.compress = gTRUE;
		sprintf(buf2, "uncompress < %s", buf);
		fp = popen(buf2, "r");
		if ( null(fp) ) {
			ERR_MSG("dfileopen: %s: open failed", buf);
		}
		return fp;
	}
}

#define ntohlkrec(kp)	((kp)->recmen=ntohl((kp)->recmen),(kp)->newdno=ntohl((kp)->newdno))
#define	ntohljrec(jp)	((jp)->datano = ntohl((jp)->datano))

static int
qlopen(void)
{
	static char buf[256];
	int len;
	unsigned i;
	char *p;
	FILE *fp;
	ytlm.ql = gTRUE;
	ytlm.cfn = buf;
	p = getenv(ENV_ASTROD_ROOT);
	if ( null(p) ) p = DEFPATH;
	ytlm.cfn = search_file(RLFILE, p);
	if ( null(ytlm.cfn) ) {
		ytlm.cfn = search_file(CTLFILE, p);
		if ( null(ytlm.cfn) ) return 4;
	}
	fp = fopen(ytlm.cfn, "rb");
	if ( null(fp) ) return 4;
	OUT_MSG("cntl-file=%s", ytlm.cfn);
	unless ( 1 == fread(ytlm.krec, sizeof(ytlm.krec[0]), 1, fp) ) {
		fclose(fp);
		return 4;
	}
	ntohlkrec(ytlm.krec);
	unless ( ytlm.krec->qlflg ) {
		fclose(fp);
		return 2;	/* QL not running */
	}
	if ( null( allocjrec() ) ) {
		fclose(fp);
		return 4;
	}
	len = fread(ytlm.jrec,sizeof(ytlm.jrec[0]),(size_t)ytlm.krec->recmen,fp);
	if ( fclose(fp) ) return 4;
	unless ( ytlm.krec->recmen == len ) return 4;
	for (i = 0; i < ytlm.krec->recmen; i++) ntohljrec(ytlm.jrec + i);
	ytlm.jrecp = ytlm.krec->newdno - 2;
	ytlm.frn = ytlm.jrec[(size_t)ytlm.jrecp].datano;
	return 0;
}

/* yymmddnnnn:n1-n2,n3-n4 */
static int
pathopen(char *path)
{
	unsigned i, j, n;
	char c, *p;
	for (ytlm.jrecp = 0; ytlm.jrecp < ytlm.jrecsize; ytlm.jrecp++) {
		if ( 0 == strncmp(path, ytlm.jrec[(size_t)ytlm.jrecp].pathno, 10) ) {
			break;
		}
	}
	unless ( ytlm.jrecp < ytlm.jrecsize ) return 1;
	ytlm.fp = dfileopen(ytlm.jrec[(size_t)ytlm.jrecp].logname);
	if ( null(ytlm.fp) ) return 4;
	ytlm.frn = 0;
	ytlm.onefile = gTRUE;
	if ( ':' != path[10] ) return 0;
	n = ( ytlm.jrec[(size_t)ytlm.jrecp].datano + SFFR - 1 ) / SFFR;
	if ( null(ytlm.sfuse) ) {
		ytlm.sfuse = malloc(n);
	} else {
		ytlm.sfuse = realloc(ytlm.sfuse, n);
	}
	if ( null(ytlm.sfuse) ) return 0;	/* no sf selection */
	memset(ytlm.sfuse, 0, n);
	i = 0;
	p = path + 11;
	if ( isdigit(*p) ) {
		sscanf(p, "%u", &i);
		while ( isdigit(*p) ) p++;
	}
	while ( *p && i < n ) {
		c = *p++;
		j = n;
		if ( isdigit(*p) ) {
			sscanf(p, "%u", &j);
			while ( isdigit(*p) ) p++;
		}
		switch ( c ) {
		case '-':
			while ( i < n && i <= j ) ytlm.sfuse[i++] = 1;
			break;
		default:
			i = j;
			ytlm.sfuse[i++] = 1;
		}
	}
	return 0;
}

static void
gettime(struct framedata *fdp, AtTime *ji)
{
	int totd_j;
	struct jyohorec *jp = ytlm.jrec + (size_t)ytlm.jrecp;
	ji->yr = (jp->pathno[0]&15)*10 + (jp->pathno[1]&15);
	totd_j = (jp->totday[0]&15)*100+(jp->totday[1]&15)*10+(jp->totday[2]&15);
	ji->dy = bcd2int(fdp->today[0])*100 + bcd2int(fdp->today[1]);
#if 0		/* for some reason, totd_j is always 0 at inazuma */
	unless ( totd_j-160 < t->day && t->day < totd_j+160 ) {
		if ( totd_j < ji->day ) {
			ji->year--;
		} else {
			ji->year++;
		}
	}
#endif
	totday2attime(ji);
	ji->hr = bcd2int(fdp->ftime[0]);
	ji->mn = bcd2int(fdp->ftime[1]);
	ji->sc = bcd2int(fdp->ftime[2]);
	ji->ms = bcd2int(fdp->fms[0])*10 + bcd2int(fdp->fms[1])/10.0;
}

static int
fgettime(long f, AtTime *ji)
{
	char buf[7];
	if ( fseek(ytlm.fp, 144L*(f+4), SEEK_SET) ) return 4;
	unless (sizeof(buf) == fread(buf, sizeof(buf[0]), sizeof(buf), ytlm.fp)) {
		return 4;
	}
	gettime((struct framedata*)buf, ji);
	return 0;
}

/* yymmddhhmmss+time_offs */
static int
timeopen(char *time)
{
	char *p;
	AtTime t, ts, tm, te;
	double mjd;
	long fs, fm, fe;
	double secoff;
	p = ascatool_scanYYMMDDHHMMSS(time, &t);
	switch ( *p++ ) {
	case '-':
		p = ascatool_scanYYMMDDHHMMSS(time=p, &ts);
		if ( p != time ) ytlm.endtim = ts;
		break;
	case '+':
		p = ascatool_scanTimeOffset(time=p, &secoff);
		if ( p != time ) {
			ytlm.endtim = t;
			addattime(&ytlm.endtim, secoff);
		}
		break;
	}
	if ( p == time ) return 1;
	for (ytlm.jrecp = 0; ytlm.jrecp < ytlm.jrecsize; ytlm.jrecp++) {
		bcd2attime(ytlm.jrec[(size_t)ytlm.jrecp].strtime, &ts);
		bcd2attime(ytlm.jrec[(size_t)ytlm.jrecp].endtime, &te);
		ts.ms = te.ms = 0;
		if ( cmpattime(&ts, &t) <= 0 && cmpattime(&t, &te) <= 0 ) break;
	}
	unless ( ytlm.jrecp < ytlm.jrecsize ) return 1;
	ytlm.fp = dfileopen(ytlm.jrec[(size_t)ytlm.jrecp].logname);
	if ( null(ytlm.fp) ) return 4;
	if ( ytlm.compress ) {
		struct framedata fdata;
		unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) return 4;
		unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) return 4;
		unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) return 4;
		unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) return 4;
		fs = 0;
		fe = ytlm.jrec[(size_t)ytlm.jrecp].datano;
		do {
			unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) return 4;
			gettime(&fdata, &ts);
			ts.ms = 0;
			fs++;
		} while ( cmpattime(&t, &ts) && fs < fe );
		ytlm.frn = fs;
		return 0;
	}
	fs = 0;
	fe = ytlm.jrec[(size_t)ytlm.jrecp].datano - 1;
	if ( fgettime(fs, &ts) ) return 4;
	if ( fgettime(fe, &te) ) return 4;
	ts.ms = te.ms = 0;
	unless ( cmpattime(&t, &te) <= 0 ) return 1;
	until ( fs == fe || fs+1 == fe ) {
		if ( 0 == cmpattime(&ts, &t) ) break;
		if ( 0 == cmpattime(&te, &t) ) {
			fs = fe;
			ts = te;
			break;
		}
		fm = fs + ( fe - fs ) / 2;
		if ( fgettime(fm, &tm) ) return 4;
		tm.ms = 0;
		unless ( cmpattime(&ts, &tm) <= 0 ) return 1;
		unless ( cmpattime(&tm, &te) <= 0 ) return 1;
		if ( cmpattime(&tm, &t) < 0 ) {
			fs = fm;
			ts = tm;
		} else {
			fe = fm;
			te = tm;
		}
	}
	ytlm.frn = fs;
	if ( fs ) {
		if ( fseek(ytlm.fp, 144L*(fs+4), SEEK_SET) ) return 4;
	} else {
		if ( fseek(ytlm.fp, 0, SEEK_SET) ) return 4;
	}
	return 0;
}

int
ascatool_tlmopen(char *fn)
{
	char *p;
	FILE *fp;
	int len;
	unsigned i;
	ytlm.endtim.yr = -1;
	if ( null(ytlm.krec) ) {
		ytlm.krec = xmalloc( sizeof(*ytlm.krec) );
		if ( null(ytlm.krec) ) return 4;
	}
	if ( !null(ytlm.fp) && dfclose(ytlm.fp) ) return 4;
	ytlm.fp = NULL;
	if ( 0 == strcasecmp("ql", fn) ) return qlopen();
	ytlm.ql = gFALSE;
	ytlm.onefile = gFALSE;
	switch ( *fn ) {
	case 'r': case 'R': ytlm.cfn = RLFILE; fn++; break;
	case 's': case 'S': ytlm.cfn = PBFILE; fn++; break;
	case 'b': case 'B': ytlm.cfn = MTFILE; fn++; break;
	default: ytlm.cfn = CTLFILE; break;
	}
	p = getenv(ENV_ASTROD_ROOT);
	if ( null(p) ) p = DEFPATH;
	ytlm.cfn = search_file(ytlm.cfn, p);
	if ( null(ytlm.cfn) ) return 4;
	fp = fopen(ytlm.cfn, "rb");
	if ( null(fp) ) return 4;
	unless ( 1 == fread(ytlm.krec, sizeof(ytlm.krec[0]), 1, fp) ) {
		fclose(fp);
		return 4;
	}
	ntohlkrec(ytlm.krec);
	if ( null( allocjrec() ) ) {
		fclose(fp);
		return 4;
	}
	len = fread(ytlm.jrec,sizeof(ytlm.jrec[0]),(size_t)ytlm.krec->recmen,fp);
	if ( fclose(fp) ) return 4;
	unless ( ytlm.krec->recmen == len ) return 4;
	for (i = 0; i < ytlm.krec->recmen; i++) ntohljrec(ytlm.jrec + i);
	len = strlen(fn);
	if ( len < 10 ) return 4;
	if ( isdigit(fn[10]) ) return timeopen(fn);
	return pathopen(fn);
}

static int
dlread(FR *frp, ADTIME *time_ptr)
{
	static struct framedata fdata;
	ADTIME adt;
	AtTime attime;
	
	until ( ytlm.frn < ytlm.jrec[(size_t)ytlm.jrecp].datano ) {
		if ( ytlm.onefile ) return 1;	/* end of data */
		if ( ytlm.krec->newdno - 2 == ytlm.jrecp ) return 1; /* end of data */
		ytlm.jrecp = ( ytlm.jrecp + 1 ) % ytlm.krec->recmen;
		if ( !null(ytlm.fp) && dfclose(ytlm.fp) ) {
			ERR_MSG("dlread: file close error");
			return 4;
		}
		ytlm.fp = dfileopen(ytlm.jrec[(size_t)ytlm.jrecp].logname);
		if ( null(ytlm.fp) ) return 4;
		ytlm.frn = 0;
	}
	if ( 0 == ytlm.frn ) {
		if ( ytlm.compress ) {
			unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) return 4;
			unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) return 4;
			unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) return 4;
			unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) return 4;
		} else {
			if ( fseek(ytlm.fp,4L*sizeof(struct msgdata),SEEK_SET) ) {
				ERR_MSG("dlread: seek error");
				return 4;
			}
		}
	}
	if ( !null(ytlm.sfuse) && 0 == ytlm.sfuse[ytlm.frn/SFFR] ) {
		/* skip sub frames */
		long i, j, n;
		n = ( ytlm.jrec[(size_t)ytlm.jrecp].datano + SFFR - 1 ) / SFFR;
		for (i = ytlm.frn/SFFR; i < n && 0 == ytlm.sfuse[i]; i++) ;
		unless ( i < n ) return 1;		/* end of data */
		if ( ytlm.compress ) {
			while ( ytlm.frn < i * SFFR ) {
				unless ( 1 == fread(&fdata, sizeof(fdata), 1, ytlm.fp) ) {
					return 4;
				}
			}
		} else {
			if ( fseek(ytlm.fp, (i*SFFR-ytlm.frn)*sizeof(fdata), SEEK_CUR) ) {
				return 4;
			}
			ytlm.frn = i * SFFR;
		}
	}
	unless ( 1 == fread(&fdata, 16, 1, ytlm.fp) ) {
		ERR_MSG("dlread: frame header read error");
		return 4;
	}
	unless ( 1 == fread(frp, sizeof(*frp), 1, ytlm.fp) ) {
		fseek(ytlm.fp, -16, SEEK_CUR);
		ERR_MSG("dlread: frame data read error");
		return 4;
	}
	ytlm.frn++;
	gettime(&fdata, &attime);
	
	if ( TIME_IS_ADTIME == _TIME_TYPE_ ) {
		adt.year = attime.yr;
		adt.month = attime.mo;
		adt.day = attime.dy;
		adt.hour = attime.hr;
		adt.minute = attime.mn;
		adt.second = attime.sc;
		adt.msec = attime.ms;
		*time_ptr = adt;
	} else {
		if ( TIME_IS_ATTIME == _TIME_TYPE_ ) {
			*(AtTime*)time_ptr = attime;
		} else {
			if ( TIME_IS_MJD == _TIME_TYPE_ ) {
				atMJulian(&attime, (MJD*)time_ptr);
			} else {
				*(ASCATIME*)time_ptr = attime2asca(attime);
			}
		}
	}
	
	if ( 0 <= ytlm.endtim.yr && 0 < cmpattime(&attime,&ytlm.endtim) ) return 1;
	return 0;
}

static int
qlread(FR *frp, ADTIME *adt)
{
	static FILE *fp = NULL;
	unsigned i;
	for (;;) {
		if ( ytlm.frn < ytlm.jrec[(size_t)ytlm.jrecp].datano ) {
			return dlread(frp, adt);
		}
		if ( ytlm.jrecp == ytlm.krec->newdno - 2 ) break;
		ytlm.jrecp = ( ytlm.jrecp + 1 ) % ytlm.krec->recmen;
		if ( !null(ytlm.fp) && dfclose(ytlm.fp) ) {
			ERR_MSG("qlread: file close error");
			return 4;
		}
		ytlm.fp = dfileopen(ytlm.jrec[(size_t)ytlm.jrecp].logname);
		if ( null(ytlm.fp) ) return 4;
		ytlm.frn = 0;
	}
#ifdef QLSEEK_CNTL
	if ( null(fp) ) {
		fp = fopen(ytlm.cfn, "rb");
		if ( null(fp) ) return 4;
	} else {
		fflush(fp);
	}
#else
	fp = fopen(ytlm.cfn, "rb");
	if ( null(fp) ) {
		ERR_MSG("qlread: cntl file open error");
		return 4;
	}
#endif
	unless ( 1 == fread(ytlm.krec, sizeof(ytlm.krec[0]), 1, fp) ) {
		ERR_MSG("qlread: cntl file krec read error");
		fclose(fp);
		fp = NULL;
		return 4;
	}
	unless ( ytlm.krec->qlflg ) {
		fclose(fp);
		fp = NULL;
		return 1;	/* QL end */
	}
	ntohlkrec(ytlm.krec);
	if ( ytlm.jrecsize < ytlm.krec->recmen && null(allocjrec()) ) {
		ERR_MSG("qlread: jrec alloc error");
		fclose(fp);
		fp = NULL;
		return 4;
	}
	i = (unsigned)ytlm.jrecp;
	for (;;) {
		if ( fseek(fp, 144L*(i+1), SEEK_SET) ) {
			ERR_MSG("qlread: cntl file seek error");
			fclose(fp);
			fp = NULL;
			return 4;
		}
		unless ( 1 == fread(ytlm.jrec+i, sizeof(ytlm.jrec[0]), 1, fp) ) {
			ERR_MSG("qlread: cntl file jrec read error");
			fclose(fp);
			fp = NULL;
			return 4;
		}
		ntohljrec(ytlm.jrec+i);
		if ( i == ytlm.krec->newdno - 2 ) break;
		i = ( i + 1 ) % (size_t)ytlm.krec->recmen;
	}
#ifndef QLSEEK_CNTL
	if ( fclose(fp) ) {
		ERR_MSG("qlread: cntl file close error");
		return 4;
	}
#endif
	if ( ytlm.frn < ytlm.jrec[(size_t)ytlm.jrecp].datano ) {
		if ( _QLSKIP ) {
			if ( ytlm.frn + _QLSKIP < ytlm.jrec[(size_t)ytlm.jrecp].datano ) {
				ytlm.frn = ytlm.jrec[(size_t)ytlm.jrecp].datano - 1;
			}
		}
#ifdef QLSEEK
		if ( null(ytlm.fp) ) {
			ytlm.fp = dfileopen(ytlm.jrec[(size_t)ytlm.jrecp].logname);
			if ( null(ytlm.fp) ) {
				ERR_MSG("qlread: data file open error");
				return 4;
			}
		}
		if ( ytlm.frn ) {
			fflush(ytlm.fp);
			if ( fseek(ytlm.fp, 144L*(ytlm.frn+4), SEEK_SET) ) {
				ERR_MSG("qlread: data file seek error");
				return 4;
			}
		}
#else
		if ( !null(ytlm.fp) && dfclose(ytlm.fp) ) {
			ERR_MSG("qlread: data file close error");
			return 4;
		}
		ytlm.fp = dfileopen(ytlm.jrec[(size_t)ytlm.jrecp].logname);
		if ( null(ytlm.fp) ) {
			ERR_MSG("qlread: data file open error");
			return 4;
		}
		if ( ytlm.frn ) {
			if ( fseek(ytlm.fp, 144L*(ytlm.frn+4), SEEK_SET) ) {
				ERR_MSG("qlread: data file seek error");
				return 4;
			}
		}
#endif
		return dlread(frp, adt);
	}
	return 3;	/* no data */
}

int
ascatool_tlmread(FR *frp, ADTIME *time_ptr)
{
	AtTime attime;
	if ( ytlm.ql ) return qlread(frp, time_ptr);
	return dlread(frp, time_ptr);
}

int
ascatool_tlmclose(void)
{
	if ( !null(ytlm.fp) && dfclose(ytlm.fp) ) return 4;
	if ( !null(ytlm.sfuse) ) {
		free(ytlm.sfuse);
		ytlm.sfuse = NULL;
	}
	ytlm.fp = NULL;
	return 0;
}

long
ascatool_tlmsfnum(void)
{
	long i, n;
	if ( NULL == ytlm.sfuse ) return ytlm.frn/SFFR;
	n = ( ytlm.jrec[(size_t)ytlm.jrecp].datano + SFFR - 1 ) / SFFR;
	for (i = ytlm.frn/SFFR; i < n && 0 == ytlm.sfuse[i]; i++) ;
	return i;
}

char*
ascatool_tlmpass(void)
{
	static char buf[11];
	strncpy(buf, ytlm.jrec[(size_t)ytlm.jrecp].pathno, 10);
	buf[10] = '\0';
	return buf;
}
