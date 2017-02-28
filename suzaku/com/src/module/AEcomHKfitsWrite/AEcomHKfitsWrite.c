/*
 AEcomHKfitsWrite.c

	1999/04/26 Y.ISHISAKI	version 1.2
		made from XRS8secHKfitsWrite.c

	1999/11/19 Y.ISHISAKI	version 1.3
		add '*.CAL' keywords
		split AOCU packets
		change EXTNAME

	1999/12/27 Y.ISHISAKI
		remove OPTIC*

	2000/01/27 Y.ISHISAKI	version 1.4
		replace '.' -> '_', check "()" in fix_keyword()
		check signed values in SIB ("SN") and flag as 'n'
		replace "msec"->"ms", "sec"->"s", "deg/sec"->"deg/s" in check_kanji()

	2000/02/05 Y.ISHISAKI	version 1.5
		bug fix on SIB open fail message
		change message in reading SIB for VCDU

	2004/03/14 Y.ISHISAKI	version 1.6
		use AtTimeD (since atFunctions-2.2) instead of AtTime
		include unistd.h for unlink()
		remove unused variables
		set hduclas1 = "TEMPORALDATA" & hduclas2 = "HKP"

	2004/06/19 Y.ISHISAKI	version 1.7
		bug fix in calculating calibrated values (change v_cal += into =)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "xrsTelemFormat.h"
#include "comFitsHeaderUtil.h"

static char pname[] = "AEcomHKfitsWrite";

char AEcomHKfitsWrite_version[] = "version 1.7";
static COM_STD_KEYS stdkeys;

struct sib_data {
	int colnum;				/* FITS column number */
	char name[32];			/* FITS column name, "" for end */
	int flag;				/* 'S' for status, 'N' for analog */
	int apid, sh, wd, sb, tb;	/* apid, 2-head, word, start-bit, total-bit */
	char unit[16];		/* unit, e.g. "sec", "deg" (only for analog data) */
	int np;				/* order of polynomial */
	double *pol;		/* scaling polynomial coefficients */
	char *pol_comment;	/* comment for scaled value */
	char *comment;		/* comment */
	char *stat_msg;		/* e.g. "3 {0b00|RUN|} {0b01|CMDRST|} {0b11|PWRON|}" */
	char version[12];	/* e.g. "19980826" */
	int keynum;			/* position of keyword in header (1st keyword = 1) */
	int keynum_cal;		/* position of cal-keyword in header */
};

static struct {
	int nhk;
	char filename[256];
	char aocu_sib_file[256];
	char comm_sib_file[256];
	char xrs_sib_file[256];
	char comment_file[256];
	struct sib_data *sibp;
	struct comment_data {
		char *name;
		int name_len;
		char *comment;
	} *cmtp;
} com;

static struct {
	char *ttype, *tform, *tunit;
} fixedCol[] = {
	{ "TIME", "1D", "s" },
	{ "YYYYMMDD", "1J", "" },
	{ "HHMMSS", "1J", "" },
	{ "CCSDS_HEADER", "6B", "" },
	{ "ETI", "2V", "1/4096 s" }
};

#define MAX_HKDATA	64

static struct hkfits {
	int apid, sh;
	char name[8];
	char extname[32];
	char comment[80];
	fitsfile *fp;
	char tmpfile[256];
	long irow;
	int nf;
	double tstart;
	double tstop;
} hkdata[MAX_HKDATA];

static char *
basename(char *filename)
{
/*	"directryname/filename" -> "filename" */
	char *p;
	for (p = filename; *p; p++) {
		if ( *p == '/' ) {
			filename = p + 1;
		}
	}
	return filename;
}

/* parse "p4E4:s81,w78,b2@14" */
static int
parse_sib_data_pos(char *s, int *apid, int *sh, int *wd, int *sb, int *tb)
{
	char *p = s;
	char *q = s;

	if ( 'p' != *s ) {
		return -1;
	}
	s++;
	while ( *s ) {
		if ( ':' == *s ) {
			q = s;
		}
		if ( ',' == *s ) {
			*s++ = '\0';
			if ( p == q ) {
				*sh = -1;	/* no ':', no secondary header */
				if ( 1 != sscanf(p+1, "%x", apid) ) {
					return -1;
				}
				p = s;
				goto wd;
			} else {
				*q = '\0';	/* exist ':', exist secondary header */
				if ( 1 != sscanf(p+1, "%x", apid) ) {
					return -1;
				}
				if ( 's' != *(q+1) || 1 != sscanf(q+2, "%x", sh) ) {
					return -1;
				}
				p = s;
				goto wd;
			}
		}
		s++;
	}
	return -1;

 wd:
	if ( 'w' != *s ) {
		return -1;
	}
	s++;
	while ( *s ) {
		if ( ',' == *s ) {
			*s++ = '\0';
			if ( 1 != sscanf(p+1, "%d", wd) ) {
				return -1;
			}
			p = s;
			goto sb;
		}
		s++;
	}
	return -1;

 sb:
	if ( 'b' != *s ) {
		return -1;
	}
	s++;
	while ( *s ) {
		if ( '@' == *s ) {
			*s++ = '\0';
			if ( 1 != sscanf(p+1, "%d", sb) ) {
				return -1;
			}
			p = s;
			goto tb;
		}
		s++;
	}
	return -1;

 tb:
	if ( 1 != sscanf(p, "%d", tb) ) {
		return -1;
	}
	return 0;
}

static int
fix_keyword(char *p)
{
	while ( *p ) {
		if ( '/' == *p || '&' == *p || '#' == *p || '.' == *p ) {
			*p = '_';
		} else if ( '-' == *p ) {
			if ( '0' <= *(p+1) && *(p+1) <= '9' ) {
				*p = 'M';
			} else {
				*p = '_';
			}
		} else if ( '+' == *p ) {
			if ( '0' <= *(p+1) && *(p+1) <= '9' ) {
				*p = 'P';
			} else {
				*p = '_';
			}
		} else if ( '(' == *p ) {
			*p = '_';
		} else if ( ')' == *p ) {
			if ( '\0' == *(p+1) ) {
				*p = '\0';
			} else {
				*p = '_';
			}
		}
		p++;
	}
	return 0;
}

static struct sib_data *
read_sib_data(struct sib_data *sibp, char *fn)
{
	FILE *fp;
	int isib, nalloc;
	struct sib_data sibbuf;

	if ( NULL == sibp ) {
		isib = 0;
		nalloc = 1000;
		sibp = malloc(nalloc * sizeof(*sibp));
		if ( NULL == sibp ) {
			fprintf(stderr, "\
%s: malloc() failed for SIB (%d)\n", pname, nalloc);
			return NULL;
		}
		sibp[isib].flag = 0;
	} else {
		for (isib = 0; sibp[isib].name[0]; isib++) {
			;
		}
		nalloc = isib + 1;
	}

	fp = fopen(fn, "r");
	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: SIB '%s' open failed\n", pname, fn);
		return NULL;
	}

	for (;;) {
		int ic, iw, nw, pos_if, flag_vcdu;
		int c1, c2, c3;
		char line[1024];
		char *word[128];
		int parenflag = 0;

/* read one line */
		ic = iw = 0;
		word[iw++] = line;
		/* search for "\n//" */
		c1 = fgetc(fp);
		if ( EOF == c1 ) break;
		c2 = fgetc(fp);
		if ( EOF == c2 ) break;
		for (;;) {
			c3 = fgetc(fp);
			if ( EOF == c3 ) break;
			if ( '\n' == c1 && '/' == c2 && '/' == c3 ) break;
			c1 = c2;
			c2 = c3;
		}
		if ( EOF == c3 ) break;

		for (;;) {
			int ch = fgetc(fp);
			if ( EOF == ch || ';' == ch ) {
				line[ic] = '\0';
				break;
			}
			/* ignore "\n    " */
			if ( '\n' == ch ) {
				for (;;) {
					ch = fgetc(fp);
					if ( ' ' != ch ) {
						break;
					}
				}
				if ( EOF == ch || ';' == ch ) {
					line[ic] = '\0';
					break;
				}
			}
			if ( '{' == ch ) {
				parenflag = 1;
				continue;
			} else if ( '}' == ch ) {
				parenflag = 0;
				continue;
			}
			if ( parenflag ) {
				if ( '|' == ch ) {
					ch = ':';
				}
			}
			if ( ic < sizeof(line)-1 ) {
				line[ic] = ch;
				if ( '|' == ch ) {
					line[ic] = '\0';
					if ( iw < sizeof(word)/sizeof(*word) ) {
						word[iw] = line + ic + 1;
						iw++;
					}
				}
				ic++;
			}
		}

/* read one line (end) */

		nw = iw;

/* get name */
		if ( 1 < nw && word[1][0] ) {
			strncpy(sibbuf.name, word[1], sizeof(sibbuf.name));
		} else {
			goto sib_error;

		sib_error:
			fflush(stdout);
			fflush(stderr);
			fprintf(stderr, "\
%s: SIB read error at:\n\
%s|%s|%s\n", pname, word[0], word[1], word[2]);
			fflush(stderr);
			continue;
		}

/* get comment */
		if ( 2 < nw && word[2][0] ) {
			sibbuf.comment = strdup(word[2]);
		} else {
			sibbuf.comment = NULL;
		}

/* check if STATUS or ANALOG data */
		if ( 4 < nw && 0 == strcmp("ST", word[4]) ) {
			sibbuf.flag = 'S';		/* status */
		} else if ( 4 < nw && 0 == strcmp("SN", word[4]) ) {
			sibbuf.flag = 'n';		/* signed analog value */
		} else if ( 4 < nw && 0 == strcmp("NN", word[4]) ) {
			sibbuf.flag = 'N';		/* unsigned analog value */
		} else {
			fprintf(stderr, "word[4]='%s'\n", word[4]);
			goto sib_error;
		}

/* parse "p4E4:s81,w78,b2@14" */
		pos_if = 0;
		flag_vcdu = 0;
		for (iw = 7; iw+1 < nw; iw++) {
			if ( 0 == strcmp("IF", word[iw]) ) {
				pos_if = iw;
			}
			if ( 0 == strcmp("EXT", word[iw]) ) {
				if ( 0 == strncmp("t,w", word[iw+1], 3) ) {
					/* vcdu data */
					flag_vcdu = 1;
					break;
				}
				if ( parse_sib_data_pos(word[iw+1], &sibbuf.apid, &sibbuf.sh,
									&sibbuf.wd, &sibbuf.sb, &sibbuf.tb) < 0 ) {
					fprintf(stderr, "word[%d]='%s'\n", iw+1, word[iw+1]);
					goto sib_error;
				}
				/* needs special treatment for AOCU packets (apid=0x448) */
				if ( pos_if && 0x448 == sibbuf.apid && -1 == sibbuf.sh ) {
					if ( 0 == strcmp("1*2", word[pos_if+1]) &&
						 0 == strncmp("2AOCU_A0_PKTID1:EQ:CM-AOCS",
									  word[pos_if+2], 26) ) {
						sibbuf.sh = 0x80 + atoi(word[pos_if+2] + 26);
					}
/*					printf("\
%s: pos_if=%d, word[%d]='%s', word[%d]='%s', sh=%xh\n",
						   pname, pos_if,
						   pos_if+1, word[pos_if+1], pos_if+2, word[pos_if+2],
						   sibbuf.sh);*/
				}
				break;
			}
		}
		if ( flag_vcdu ) {
			printf("\
%s: SIB item for VCDU (%s) is ignored at:\n\
%s|%s|%s\n", pname, word[iw+1], word[0], word[1], word[2]);
			continue;
		}
		if ( iw+1 < nw ) {
			;
		} else {
			printf("\
%s: SIB skipped at:\n\
%s|%s|%s\n", pname, word[0], word[1], word[2]);
			continue;
		}

/* get unit */
		sibbuf.unit[0] = '\0';
		if ( iw+3 < nw && 0 == strcmp("POL", word[iw+2]) ) {
			strncpy(sibbuf.unit, word[iw+3], sizeof(sibbuf.unit));
		}

/* get scaling polynomial coefficients */
		sibbuf.np = 0;
		if ( iw+5 < nw &&
			 0 == strcmp("POL", word[iw+2]) ) {
			int n, np;
			double p[6];
			n = sscanf(word[iw+5], "%d%lf%lf%lf%lf%lf%lf", &np,
					   p, p+1, p+2, p+3, p+4, p+5);
			if ( n != np + 1 ) {
				fprintf(stderr, "\
%s: Warning: Polynomial coefficients read error at:\n\
%s POL '%s'\n", pname, sibbuf.name, word[iw+5]);
			} else if ( 2 == np && 0.0 == p[0] && 1.0 == p[1] ) {
				/* no scaling */
				;
			} else {
/*				printf("%s %s %s\n", sibbuf.name, word[iw+2], word[iw+5]);*/
				sibbuf.np = np;
				sibbuf.pol = malloc(sizeof(*p)*np + strlen(word[iw+5]) + 1);
				if ( NULL == sibbuf.pol ) {
					fprintf(stderr, "\
%s: malloc() failed for polynomial coefficients of %s\n", pname, sibbuf.name);
					return NULL;
				}
				memcpy(sibbuf.pol, p, sizeof(*p)*np);
				sibbuf.pol_comment = (char *)&sibbuf.pol[np];
				strcpy(sibbuf.pol_comment, word[iw+5]);
			}
		}

/* get status message */
		sibbuf.stat_msg = NULL;
		if ( 10 < nw && 0 == strcmp("OTHER", word[10]) ) {
			sibbuf.stat_msg = malloc(strlen(word[13])+1);
			if ( sibbuf.stat_msg ) {
				strcpy(sibbuf.stat_msg, word[13]);
			}
		}

/* get version */
		strcpy(sibbuf.version, "unknown");
		for (iw = 9; iw < nw - 1; iw++) {
			if ( 0 == strcmp("VER", word[iw]) ) {
				strncpy(sibbuf.version, word[iw+1], sizeof(sibbuf.version));
				break;
			}
		}

/* arrange memory */
		if ( nalloc <= isib ) {
			if ( nalloc ) {
				nalloc = nalloc + 100;
				sibp = realloc(sibp, sizeof(*sibp)*nalloc);
			} else {
				nalloc = 1000;
				sibp = malloc(sizeof(*sibp)*nalloc);
			}
			if ( NULL == sibp ) {
				fprintf(stderr, "\
%s: too many SIB items (%d)\n", pname, isib);
				fclose(fp);
				return NULL;
			}
		}

		fix_keyword(sibbuf.name);
		sibp[isib] = sibbuf;
		isib++;
	}

	fclose(fp);

	if ( 0 == isib ) {
		return NULL;
	}

	sibp = realloc(sibp, sizeof(*sibp)*(isib+1));
	if ( NULL == sibp ) {
		fprintf(stderr, "\
%s: realloc() failed for SIB (%d)\n", pname, isib);
		return NULL;
	}

	sibp[isib].name[0] = '\0';
	return sibp;
}

static struct comment_data *
read_comment_file(struct comment_data *cmtp, char *comment_file)
{
	FILE *fp;
	int icmt, nalloc, len;
	char *p, buf[1024];

	fp = fopen(comment_file, "r");
	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: comment file '%s' open failed (ignored)\n", pname, comment_file);
		return cmtp;
	}

	if ( NULL == cmtp ) {
		icmt = 0;
		nalloc = 10000;
		cmtp = malloc(nalloc * sizeof(*cmtp));
		if ( NULL == cmtp ) {
			fprintf(stderr, "\
%s: malloc() failed for comment file (%d)\n", pname, nalloc);
			return NULL;
		}
		cmtp[icmt].name = NULL;
	} else {
		for (icmt = 0; NULL != cmtp[icmt].name; icmt++) {
			;
		}
		nalloc = icmt + 1;
	}

	icmt = 0;
	for (;;) {
		if ( NULL == fgets(buf, sizeof(buf), fp) ) {
			break;
		}
		if ( '#' == *buf || ';' == *buf || '/' == *buf ) {
		/* ignore as a comment line */
			continue;
		}
		len = strlen(buf);
		if ( '\n' == buf[len-1] ) {
			buf[len-1] = '\0';
			len--;
		}
		if ( '\0' == buf[0] ) {
			continue;
		}
		cmtp[icmt].name = p = malloc(len+1);
		if ( NULL == p ) {
			fprintf(stderr, "\
%s: out of memory in read_comment_data()\n", pname);
		}
		for (strcpy(p, buf); *p; p++) {
			if ( '\t' == *p || ' ' == *p ) {
				*p++ = '\0';
				while ( *p && ( '\t' == *p || ' ' == *p ) ) {
					p++;
				}
				cmtp[icmt].comment = p;
				break;
			}
		}

		if ( *p ) {
			fix_keyword(cmtp[icmt].name);
			cmtp[icmt].name_len = strlen(cmtp[icmt].name);
			icmt++;
			if ( nalloc <= icmt ) {
				nalloc += 1000;
				cmtp = realloc(cmtp, nalloc * sizeof(*cmtp));
				if ( NULL == cmtp ) {
					fprintf(stderr, "\
%s: realloc() failed for comment file (%d)\n", pname, nalloc);
					return NULL;
				}
			}
		} else {
			fprintf(stderr, "\
%s: invalid comment line: %s", pname, buf);
			free(cmtp[icmt].name);
		}
	}
	cmtp[icmt].name = NULL;
	cmtp = realloc(cmtp, (icmt+1) * sizeof(*cmtp));
	fclose(fp);

	printf("... read %d items\n", icmt);

	return cmtp;
}

static int
check_kanji(char *s, int iconv)
{
	char *p = s;

	if ( 0 == iconv ) {
		while (*p) {
			if ( *p & 0x80 ) {
				return 1;
			}
			p++;
		}
		return 0;
	}

	if ( 0 == strcmp(s, "¡î") ) {	/* "¡î" */
		strcpy(s, "degC");
		return 1;
	}

	if ( 0 == strcmp(s, "msec") ) {
		strcpy(s, "ms");
		return 1;
	}

	if ( 0 == strcmp(s, "sec") ) {
		strcpy(s, "s");
		return 1;
	}

	if ( 0 == strcmp(s, "deg/sec") ) {
		strcpy(s, "deg/s");
		return 1;
	}

	iconv = 0;
	while (*p) {
		if ( *p & 0x80 ) {
			iconv = 1;
			*p = 'K';
			if ( *(p+1) ) {
				*(p+1) = 'j';
			}
		}
		p++;
	}

	return iconv;
}

static int
modify_comment(fitsfile *fp, char *ttype, char *name, char *comment,int keynum)
{
	int i;
	char card[81];
	int istat = 0;

	fits_modify_comment(fp, ttype, comment, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_modify_comment failed (%d) at:\n\
%s / %s\n", pname, istat, name, comment);
		return -1;
	}

	fits_read_record(fp, keynum, card, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_record failed (%d)\n", pname, istat);
		return -1;
	}

	for (i = strlen(card); i < 80; i++) {
		card[i] = ' ';		/* pad spaces */
	}
	for (i = 12 + strlen(name); i < 80; i++) {
		if ( card[i] == comment[0] &&
			 0 == strncmp(card+i, comment, 80-i) ) {
			char *commentleft;
			int naddkeys;
			int commentlen = strlen(comment);
			int writtenlen = 80 - i;
			if ( commentlen <= writtenlen ) {
				break;		/* comment ok */
			}
			naddkeys = ((commentlen - writtenlen) + 69) / 70;
			commentleft = comment + writtenlen;
			for (i = 0; i < naddkeys; i++) {
				sprintf(card, "COMMENT   %-70.70s", commentleft+70*i);
				fits_insert_record(fp, keynum+i+1, card, &istat);
				if ( istat ) {
					fprintf(stderr, "\
%s: fits_insert_record failed (%d)\n", pname, istat);
					return -1;
				}
			}
			return naddkeys;
		}
	}

	return 0;
}

static int
create_ae_hkfits(struct hkfits *hp, struct sib_data *sibp)
{
	int isib, ikey, len;
	int istat = 0;
	char **ttype;
	char **tform;
	char **tunit;
	char buf[80];
	int i, ic, nf, tfield;
	int tbltype = BINARY_TBL;
	long naxis2 = 0;

	if ( '\0' == com.filename[0] ) {
		strcpy(hp->tmpfile, tempnam(".", "ae")+2);
		sprintf(hp->tmpfile + strlen(hp->tmpfile), "com_%s.hk", hp->name);
	} else {
		strcpy(hp->tmpfile, com.filename);
		len = strlen(com.filename);
		if ( 3 < len && 0 == strcmp(".hk", &hp->tmpfile[len-3]) ) {
			len = len - 3;
		}
		sprintf(&hp->tmpfile[len], "_%s.hk", hp->name);
	}

	printf("\
%s: creating temporary file '%s'\n", pname, hp->tmpfile);
	unlink(hp->tmpfile);
	fits_create_file(&hp->fp, hp->tmpfile, &istat);
	hp->irow = 1;
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_create_file failed (%d)\n", pname, istat);
		return -1;
	}

	fits_create_img(hp->fp, BYTE_IMG, 0, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_create_img failed (%d)\n", pname, istat);
		return -1;
	}
	stdkeys.hduclas1 = NULL;
	if ( AEwriteCOMStdKeys(hp->fp, &stdkeys, 0, &istat) != ANL_TRUE ) {
		fprintf(stderr, "\
%s: AEwriteXRSStdkeys() failed (%d)\n", pname, istat);
		return -1;
	}

	tfield = hp->nf = nf = sizeof(fixedCol)/sizeof(*fixedCol);

	for (isib = 0; sibp[isib].name[0]; isib++) {
		if ( sibp[isib].apid == hp->apid && sibp[isib].sh == hp->sh ) {
			tfield++;
			if ( sibp[isib].np ) {	/* exists scaling coefficients */
				tfield++;
			}
		}
	}
/*	printf("tfield=%d\n", tfield);*/

	ttype = malloc(3*sizeof(*ttype)*tfield);
	tform = ttype + tfield;
	tunit = tform + tfield;
	if ( NULL == ttype ) {
		fprintf(stderr, "\
%s: malloc() failed (tfield=%d)\n", pname, tfield);
		return -1;
	}

	ic = 0;
	for (i = 0; i < nf; i++) {
		ttype[ic] = fixedCol[ic].ttype;
		tform[ic] = fixedCol[ic].tform;
		tunit[ic] = fixedCol[ic].tunit;
		ic++;
	}
	for (isib = 0; sibp[isib].name[0]; isib++) {
		struct sib_data *p = &sibp[isib];
		int tb = p->tb;
		char **ttp = &ttype[ic];
		char **tfp = &tform[ic];
		char **tup = &tunit[ic];

		if ( p->apid != hp->apid || p->sh != hp->sh ) {
			continue;
		}
		p->colnum = ic + 1;
		*ttp = p->name;
		if ( 'S' == p->flag ) {
			if ( tb <= 1 ) {
				*tfp = "1B";
			} else if ( tb <= 8 ) {
				*tfp = "1B";
			} else if ( tb <= 16 ) {
				*tfp = "1I";
			} else if ( tb <= 32 ) {
				*tfp = "1J";
			} else {
				fprintf(stderr, "\
%s: too big status SIB value (name=%s)\n", pname, p->name);
				return -1;
			}
			*tup = "";
		} else if ( 'n' == p->flag ) {	/* signed analog value */
			if ( tb <= 16 ) {
				*tfp = "1I";
			} else if ( tb <= 32 ) {
				*tfp = "1J";
			} else {
				fprintf(stderr, "\
%s: too big analog SIB value (name=%s)\n", pname, p->name);
				return -1;
			}
			check_kanji(p->unit, 1);
			*tup = p->unit;
		} else if ( 'N' == p->flag ) {	/* unsigned analog value */
			if ( tb <= 8 ) {
				*tfp = "1B";
			} else if ( tb < 16 ) {
				*tfp = "1I";
			} else if ( tb == 16 ) {
				*tfp = "1U";
			} else if ( tb < 32 ) {
				*tfp = "1J";
			} else if ( tb == 32 ) {
				*tfp = "1V";
			} else if ( tb <= 40 ) {
				*tfp = "5B";
			} else if ( tb <= 48 ) {
				*tfp = "6B";
			} else if ( tb <= 56 ) {
				*tfp = "7B";
			} else {
				fprintf(stderr, "\
%s: too big analog SIB value (name=%s)\n", pname, p->name);
				return -1;
			}
			check_kanji(p->unit, 1);
			*tup = p->unit;
		}
		ic++;

		if ( p->np ) {
			ttype[ic] = malloc(strlen(ttype[ic-1]) + 5);
			if ( NULL == ttype[ic] ) {
				fprintf(stderr, "\
%s: malloc() failed for ttype='%s_CAL'\n", pname, ttype[ic-1]);
				return -1;
			}
			sprintf(ttype[ic], "%s_CAL", ttype[ic-1]);
			if ( tb <= 24 ) {
				tform[ic] = "1E";
			} else {
				tform[ic] = "1D";
				/*printf("tform='1D' for %s\n", ttype[ic]);*/
			}
			tunit[ic] = tunit[ic-1];
			ic++;
		}
	}

	fits_create_tbl(hp->fp, tbltype, naxis2, tfield, ttype, tform, tunit,
					hp->extname, &istat);
/*	for (isib = 0; isib < tfield; isib++) {
		printf("%d '%s'\t'%s'\t'%s'\n",
			   isib+1, ttype[isib], tform[isib], tunit[isib]);
	}*/

	if ( istat ) {
		fprintf(stderr, "\
%s: fits_create_tbl failed (%d)\n", pname, istat);
		for (isib = 1; isib <= tfield; isib+=10) {
			int hdutype;
			int istat = 0;
			fits_create_tbl(hp->fp, tbltype, naxis2, isib,
							ttype, tform, tunit, hp->extname, &istat);
			if ( istat ) break;
			fits_delete_hdu(hp->fp, &hdutype, &istat);
		}
		fprintf(stderr, "\
%s: Error around %d '%s'\t'%s'\t'%s'\n",
			   pname, isib, ttype[isib-1], tform[isib-1], tunit[isib-1]);
		return -1;
	}

	fits_modify_comment(hp->fp, "EXTNAME", hp->comment, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_modify_comment failed (%d)\n", pname, istat);
		return -1;
	}

	isib = 0;
	ikey = 1;
	for (ikey = 1; ; ikey++) {
		int colnum;
		char card[81];
		fits_read_record(hp->fp, ikey, card, &istat);
		if ( istat ) {
			istat = 0;
			break;
		}
		if ( 0 == strncmp("TTYPE", card, 5) &&
			 1 == sscanf(card+5, "%d", &colnum) ) {
			for (isib = 0; sibp[isib].name[0]; isib++) {
				struct sib_data *p = &sibp[isib];
				if ( p->apid == hp->apid &&
					 p->sh == hp->sh &&
					 colnum == p->colnum ) {
					p->keynum = ikey;
					break;
				}
				if ( p->apid == hp->apid &&
					 p->sh == hp->sh &&
					 p->np &&
					 colnum == p->colnum + 1 ) {
					p->keynum_cal = ikey;
					break;
				}
			}
		}
	}

	for (isib = 0; sibp[isib].name[0]; isib++) {
		int naddkeys;
		char comment[1024];
		struct sib_data *p = &sibp[isib];

		if ( p->apid != hp->apid || p->sh != hp->sh ) {
			continue;
		}

		sprintf(buf, "TTYPE%d", p->colnum);
		if ( NULL == p->comment ) {
			comment[0] = '\0';
		} else {
			strcpy(comment, p->comment);
			if ( ('N' == p->flag || 'n' == p->flag) && *p->unit ) {
				sprintf(comment, "%s (%s)", p->comment, p->unit);
			} else if ( p->stat_msg ) {
				sprintf(comment, "%s [%s]", p->comment, p->stat_msg);
			}
		}
		if ( NULL != com.cmtp ) {
			int icmt;
			int name_len = strlen(p->name);
			for (icmt = 0; NULL != com.cmtp[icmt].name; icmt++) {
				if ( com.cmtp[icmt].name_len == name_len &&
					 com.cmtp[icmt].name[0] == p->name[0] &&
					 0 == strcmp(com.cmtp[icmt].name, p->name) ) {
/*					printf("\
%s: %s\n-> %s\n", p->name, comment, com.cmtp[icmt].comment);*/
					strcpy(comment, com.cmtp[icmt].comment);
					break;
				}
			}
		}

		if ( '\0' == comment[0] ) {
			fprintf(stderr, "\
%s: Warning: NO COMMENT WORDS FOR %s\n", pname, p->name);
			strcpy(comment,  "NO COMMENT WORDS IN SIB");
		}

		if ( check_kanji(comment, 0) ) {
			fprintf(stderr, "\
%s: Warning: found KANJI comment at:\n\
%s / %s\n", pname, p->name, comment);
			check_kanji(comment, 1);
		}

		naddkeys = modify_comment(hp->fp, buf, p->name, comment, p->keynum);
		if ( naddkeys < 0 ) {
			return -1;
		} else if ( 0 < naddkeys ) {
			if ( p->np ) {
				p->keynum_cal += naddkeys;
			}
			for (i = isib + 1; sibp[i].name[0]; i++) {
				if ( sibp[i].apid == hp->apid && sibp[i].sh == hp->sh ) {
					sibp[i].keynum += naddkeys;
					if ( sibp[i].np ) {
						sibp[i].keynum_cal += naddkeys;
					}
				}
			}
		}

		if ( 0 == p->np ) {
			continue;
		}

		sprintf(buf, "TTYPE%d", p->colnum + 1);
		sprintf(comment, "POL%s", p->pol_comment);
		naddkeys= modify_comment(hp->fp, buf, p->name, comment, p->keynum_cal);
		if ( naddkeys < 0 ) {
			return -1;
		} else if ( 0 < naddkeys ) {
			for (i = isib + 1; sibp[i].name[0]; i++) {
				if ( sibp[i].apid == hp->apid && sibp[i].sh == hp->sh ) {
					sibp[i].keynum += naddkeys;
					if ( sibp[i].np ) {
						sibp[i].keynum_cal += naddkeys;
					}
				}
			}
		}

	}

	free(ttype);

	fits_write_comment(hp->fp, "\
Number of possible values of the parameters and meanings of these \
values are indicated in the square brackets on the comment field. \
For example, if a parameter has [2 1:ON: 0:OFF:] on the comment field, \
this parameter takes the value either 1 (for ON) or 0 (for OFF).", &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_comment failed (%d)\n", pname, istat);
		return -1;
	}

	stdkeys.hduclas1 = "TEMPORALDATA";
	stdkeys.hduclas2 = "HKP";
	if ( AEwriteCOMStdKeys(hp->fp, &stdkeys, 0, &istat) != ANL_TRUE ) {
		fprintf(stderr, "\
%s: AEwriteCOMStdkeys() failed (%d)\n", pname, istat);
		return -1;
	}

	return 0;
}

void
AEcomHKfitsWrite_startup(int *status)
{
	strcpy(com.aocu_sib_file, "sib/tlm/tlmuni/AOCU/table");
	strcpy(com.comm_sib_file, "sib/tlm/tlmuni/COMM/table");
	strcpy(com.xrs_sib_file,  "sib/tlm/tlmuni/XRS/table");
	com.nhk = 0;

	strcpy(com.comment_file,  "sib/aste_comhk_comment.list");

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "AOCU_SIB_FILE", com.aocu_sib_file);
	printf("%20s   '%s'\n", "COMM_SIB_FILE", com.comm_sib_file);
	printf("%20s   '%s'\n", "XRS_SIB_FILE",  com.xrs_sib_file);
	printf("%20s   '%s'\n", "COMMENT_FILE",  com.comment_file);
}

void
AEcomHKfitsWrite_set_file_name(char *fn)
{
	strcpy(com.filename, fn);
}

void
AEcomHKfitsWrite_com(int *status)
{
#define NVAL	6
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"AOCU_SIB_FILE",
		"COMM_SIB_FILE",
		"XRS_SIB_FILE",
		"COMMENT_FILE",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"AOCU SIB file name",
		"COMM SIB file name",
		"XRS SIB file name",
		"comment file name",
		"exit from this menu"
	};
	int nreply = 1;
	int answer[2];

	for (;;) {
		char *p;
		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("AOCU_SIB_FILE", p) ) {
			CLtxtrd(p, com.aocu_sib_file, sizeof(com.aocu_sib_file));
		} else if ( 0 == strcmp("COMM_SIB_FILE", p) ) {
			CLtxtrd(p, com.comm_sib_file, sizeof(com.comm_sib_file));
		} else if ( 0 == strcmp("XRS_SIB_FILE", p) ) {
			CLtxtrd(p, com.xrs_sib_file, sizeof(com.xrs_sib_file));
		} else if ( 0 == strcmp("COMMENT_FILE", p) ) {
			CLtxtrd(p, com.comment_file, sizeof(com.comment_file));
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
AEcomHKfitsWrite_init(int *status)
{
	static struct {
		int apid, sh;
		char *extname;
		char *comment;
	} names[] = {
		{ 0x428,   -1, "428_DHU_HK_D",	"DHU Digital data" },
		{ 0x430,   -1, "430_DHU_HK_A",	"DHU Analog data, including HCE" },
		{ 0x438,   -1, "438_DHU_HK_1",	"DHU HK packet" },
		{ 0x448,   -1, "448_AOCU_HK",	"AOCS HK packet common part" },
		{ 0x448, 0x81, "448_AOCU_1_HK",	"AOCS-1 (IRU) HK packet" },
		{ 0x448, 0x82, "448_AOCU_2_HK",	"AOCS-2 (NSAS,GAS) HK packet" },
		{ 0x448, 0x83, "448_AOCU_3_HK",	"AOCS-3 (STT Track) HK packet" },
		{ 0x448, 0x84, "448_AOCU_4_HK",	"AOCS-4 (STT Map) HK packet" },
		{ 0x448, 0x85, "448_AOCU_5_HK",	"AOCS-5 (ACM,RCS) HK packet" },
		{ 0x448, 0x86, "448_AOCU_6_HK",	"AOCS-6 (MW,MTQ) HK packet" },
		{ 0x448, 0x87, "448_AOCU_7_HK",	"AOCS-7 (AOCP1) HK packet" },
		{ 0x448, 0x88, "448_AOCU_8_HK",	"AOCS-8 (AOCP2) HK packet" },
		{ 0x448, 0x89, "448_AOCU_9_HK",	"AOCS-9 (AOCP3) HK packet" },
		{ 0x448, 0x8a, "448_AOCU_10_HK","AOCS-10 (SW PAGE) HK packet" },
		{ 0x240,   -1, "240_AOCU_DMP",	"AOCU Memory Dump, including STT" },
		{ 0x460,   -1, "460_TCI_HK",	"TCI EPT/INS data" },
		{ 0x4c0,   -1, "4C0_DP_HK",			"DP/XRS-DE HK (RAM)" },
		{ 0x4d0,   -1, "4D0_DP_ROM_HK",		"DP/XRS-DE HK (ROM)" },
		{ 0x2c1,   -1, "2C1_DP_MEM_DMP",	"DP Memory Dump" },
		{ 0x4c2,   -1, "4C2_DP_IO_DMP", 	"DP I/O Dump (RAM)" },
		{ 0x4d2,   -1, "4D2_DP_ROM_IO_DMP",	"DP I/O Dump (ROM)" },
		{ 0x4c3,   -1, "4C3_DP_ECC_DMP",	"DP ECC Dump (RAM)" },
		{ 0x4d3,   -1, "4D3_DP_ROM_ECC_DMP","DP ECC Dump (ROM)" }
	};
	int i, j, ihk, na, ns;
	struct hkfits *tmpbuf;
	char buf[80];

	show_parameter();

/*	com.sibp = read_sib_data(NULL, "sib/tlm/tlmuni/XIS/table");
	for (i = 0; com.sibp[i].name[0]; i++) {
		struct sib_data *p = &com.sibp[i];
		printf("%c %s %3x %3d %d %2d %s\t/ %s", p->flag, p->version,
			p->apid, p->wd, p->sb, p->tb, p->name, p->comment);

		switch (p->flag) {
		case 'N':
			if ( *p->unit ) {
				printf(" (%s)", p->unit);
			}
			na++;
			break;
		case 'S':
			ns++; break;
		default:
			;
		}

		if ( p->stat_msg ) {
			printf(" [%s]", p->stat_msg);
		}
		printf("\n");
	}
*/

/*	com.sibp = read_sib_data(NULL, "sib/tlm/tlmuni/HXD/table");
	for (i = 0; com.sibp[i].name[0]; i++) {
		struct sib_data *p = &com.sibp[i];
		printf("%c %s %3x %3d %d %2d %s\t/ %s", p->flag, p->version,
			p->apid, p->wd, p->sb, p->tb, p->name, p->comment);

		switch (p->flag) {
		case 'N':
			if ( *p->unit ) {
				printf(" (%s)", p->unit);
			}
			na++;
			break;
		case 'S':
			ns++; break;
		default:
			;
		}

		if ( p->stat_msg ) {
			printf(" [%s]", p->stat_msg);
		}
		printf("\n");
	}
*/
	com.sibp = NULL;
	printf("AOCU SIB: '%s'\n", com.aocu_sib_file);
	com.sibp = read_sib_data(com.sibp, com.aocu_sib_file);

	printf("COMM SIB: '%s'\n", com.comm_sib_file);
	com.sibp = read_sib_data(com.sibp, com.comm_sib_file);

	printf("XRS SIB: '%s'\n", com.xrs_sib_file);
	com.sibp = read_sib_data(com.sibp, com.xrs_sib_file);

	com.cmtp = NULL;
	printf("COMMENT FILE: '%s'\n", com.comment_file);
	com.cmtp = read_comment_file(com.cmtp, com.comment_file);

	na = ns = 0;
	for (i = 0; com.sibp[i].name[0]; i++) {
		struct sib_data *p = &com.sibp[i];

		if ( APID_XRS_ACHE_HK == p->apid ||
			 APID_XRS_CAPA_HK == p->apid || APID_XRS_CAPB_HK == p->apid ||
			 APID_XRS_CDPA_HK == p->apid || APID_XRS_CDPB_HK == p->apid ||
			 APID_XRS_ACHE_ECHO == p->apid ||
			 APID_XRS_CAPA_ECHO == p->apid || APID_XRS_CAPB_ECHO == p->apid ||
			 APID_XRS_CDPA_ECHO == p->apid || APID_XRS_CDPB_ECHO == p->apid ) {
			continue;
		}

		for (ihk = 0; ihk < com.nhk; ihk++) {
			if ( hkdata[ihk].apid == p->apid &&
				 hkdata[ihk].sh == p->sh ) {
				break;
			}
		}
		if ( com.nhk <= ihk ) {
			if ( MAX_HKDATA <= com.nhk ) {
				fprintf(stderr, "\
%s: too many APIDs (n=%d). ignored apid=%03x\n", pname, com.nhk, p->apid);
				continue;
			}
			com.nhk++;
			hkdata[ihk].apid = p->apid;
			hkdata[ihk].sh = p->sh;
			if ( -1 == p->sh ) {
				sprintf(hkdata[ihk].name, "%03x", p->apid);
				sprintf(hkdata[ihk].extname, "APID_%03X_HK", p->apid);
			} else {
				sprintf(hkdata[ihk].name, "%03xs%02x", p->apid, p->sh);
				sprintf(hkdata[ihk].extname,"APID_%03XS%02X_HK",p->apid,p->sh);
			}
			hkdata[ihk].tstart = 0.0;
			hkdata[ihk].tstop  = 0.0;
		}

/*		printf("%c %s %3x %3d %d %2d %s\t/ %s", p->flag, p->version,
			p->apid, p->wd, p->sb, p->tb, p->name, p->comment);

		switch (p->flag) {
		case 'N':
			if ( *p->unit ) {
				printf(" (%s)", p->unit);
			}
			na++;
			break;
		case 'S':
			ns++; break;
		default:
			;
		}

		if ( p->stat_msg ) {
			printf(" [%s]", p->stat_msg);
		}
		printf("\n");
*/
	}
/*	printf("ns=%d, na=%d\n", ns, na);*/

/* sort hkdata */
	tmpbuf = malloc(sizeof(*tmpbuf)*com.nhk);
	memcpy(tmpbuf, hkdata, sizeof(*tmpbuf)*com.nhk);
	for (i = j = 0; i < sizeof(names)/sizeof(*names); i++) {
		int apid = names[i].apid;
		int sh = names[i].sh;
		for (ihk = 0; ihk < com.nhk; ihk++) {
			if ( apid == tmpbuf[ihk].apid && sh == tmpbuf[ihk].sh ) {
				hkdata[j] = tmpbuf[ihk];
				tmpbuf[ihk].name[0] = '\0';
				strcpy(hkdata[j].extname, names[i].extname);
				sprintf(hkdata[j].comment, "APID=%xh, ", apid);
				strcat(hkdata[j].comment, names[i].comment);
				j++;
				break;
			}
		}
	}
	for (ihk = 0; ihk < com.nhk; ihk++) {
		if ( tmpbuf[ihk].name[0] ) {
			int apid = tmpbuf[ihk].apid;
			int sh = tmpbuf[ihk].sh;
			hkdata[j] = tmpbuf[ihk];
			if ( -1 == sh ) {
				sprintf(hkdata[j].extname, "APID_%03X_HK", apid);
			} else {
				sprintf(hkdata[j].extname,"APID_%03XS%02X_HK", apid, sh);
			}
			sprintf(hkdata[j].comment, "APID=%xh, ", apid);
			strcat(hkdata[j].comment, "unknown packet");
			j++;
		}
	}
	free(tmpbuf);

	for (ihk = 0; ihk < com.nhk; ihk++) {
		printf("\
extname='%s' %s\n", hkdata[ihk].extname, hkdata[ihk].comment);
	}

	AEsetDefaultKeywordValues(&stdkeys);
	sprintf(buf, "ANL: %s: %s", pname, AEcomHKfitsWrite_version);
	stdkeys.creator = strdup(buf);

	*status = ANL_OK;
}

void
AEcomHKfitsWrite_his(int *status)
{
	*status = ANL_OK;
}

void
AEcomHKfitsWrite_bgnrun(int *status)
{
	int ihk, used, index, len;

	com.filename[0] = '\0';
	if ( ANL_OK == BnkKey("AEpacketRPTread:FILE_NAME", &index) ) {
		BnkfGetM("AEpacketRPTread:FILE_NAME",
				 sizeof(com.filename), &used, com.filename);
		if ( 0 < used ) {
			com.filename[used] = '\0';
			strcpy(com.filename, basename(com.filename));
			stdkeys.tlmfile = strdup(com.filename);
			len = strlen(com.filename);
			if ( 4 < len && 0 == strcmp(".rpt", &com.filename[len-4]) ) {
				com.filename[len-4] = '\0';
			}
			strcat(com.filename, "_com.hk");
		}
	}

	for (ihk = 0; ihk < com.nhk; ihk++) {
		if ( create_ae_hkfits(&hkdata[ihk], com.sibp) ) {
			*status = ANL_QUIT;
			return;
		}
	}

	*status = ANL_OK;
}

void
AEcomHKfitsWrite_ana(int *nevent, int *eventid, int *status)
{
	int used, apid, sh, pksize;
	double aetime;
	AtTimeD attime;
	int yyyymmdd, hhmmss;
	unsigned char *packet;
	int ihk, isib, istat;
	unsigned int ti;
	int icol;
	long irow;
	fitsfile *fp;
	struct hkfits *hp;

/* check APID and SH */
	BnkfGetM("ASTE:PACKET_APID", sizeof(apid), &used, &apid);
	BnkfGetM("ASTE:PACKET:PTR", sizeof(packet), &used, &packet);
	BnkfGetM("ASTE:PACKET:SIZE", sizeof(pksize), &used, &pksize);
	ti = (((((packet[6]<<8) | packet[7])<<8) | packet[8]) <<8) | packet[9];
	sh = packet[10];

	ihk = 0;

 again:
	hp = NULL;
	while ( ihk < com.nhk ) {
		if ( apid == hkdata[ihk].apid ) {
			if ( -1 == hkdata[ihk].sh || sh == hkdata[ihk].sh ) {
				hp = &hkdata[ihk];
				ihk++;
				break;
			}
		}
		ihk++;
	}
	if ( NULL == hp ) {
		*status = ANL_OK;
		return;
	}

	BnkfGetM("ASTE:PACKET_AETIME", sizeof(aetime), &used, &aetime);
	aste2attimeD(aetime, &attime);
	yyyymmdd = attime.yr*10000 + attime.mo*100 + attime.dy;
	hhmmss = attime.hr*10000 + attime.mn*100 + attime.sc;

	if ( 0.0 != hp->tstart || 0.0 != hp->tstop ) {
		if ( aetime < hp->tstart ) {
			hp->tstart = aetime;
		}
		if ( hp->tstop < aetime ) {
			hp->tstop = aetime;
		}
	} else {
		hp->tstart = hp->tstop = aetime;
	}

#define bitmask(n)	(((n)>=32)?(~0):(1<<(n))-1)

	istat = 0;
	fp = hp->fp;
	irow = hp->irow;
	icol = 1;
	fits_write_col_dbl (fp, icol++, irow, 1, 1, &aetime, &istat);
	fits_write_col_int (fp, icol++, irow, 1, 1, &yyyymmdd, &istat);
	fits_write_col_int (fp, icol++, irow, 1, 1, &hhmmss, &istat);
	fits_write_col_byt (fp, icol++, irow, 1, 6, packet, &istat);
	fits_write_col_uint(fp, icol++, irow, 1, 1, &ti, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_col failed (%d) at fixed column\n", pname, istat);
	}

	for (isib = 0; com.sibp[isib].name[0]; isib++) {
		struct sib_data *p = &com.sibp[isib];
		int pos, bit, tb, v;
		unsigned int u;
		double vv;

		if ( p->apid != hp->apid || p->sh != hp->sh ) {
			continue;
		}
		if ( 32 < p->tb ) {
			fits_write_col_byt(hp->fp, p->colnum, hp->irow,
							   1, (p->tb+7)/8, &packet[p->wd], &istat);
		} else {
			pos = p->wd + p->sb/8;
			bit = p->sb % 8;
			tb = p->tb;
			u = (unsigned)(packet[pos]<<8) + packet[pos+1];
			u <<= 16;
			u |= (unsigned)(packet[pos+2]<<8) + packet[pos+3];
			u = ( u >> (32 - bit - tb) ) & bitmask(tb);
			if ( 'n' == p->flag ) {		/* signed value */
				if ( u & (1 << (tb-1)) ) {
					v = u | (~bitmask(tb));
				} else {
					v = u;
				}
				fits_write_col_int(fp, p->colnum, irow, 1, 1, &v, &istat);
				vv = v;
			} else {
				fits_write_col_uint(fp, p->colnum, irow, 1, 1, &u, &istat);
				vv = u;
			}

			if ( p->np ) {
				int ip, np = p->np;
				double v_cal = 0.0;
				for (ip = 0; ip < np; ip++) {
					v_cal = vv * v_cal + p->pol[np - ip - 1];
				}
				fits_write_col_dbl(fp, p->colnum+1, irow, 1,1, &v_cal, &istat);
			}

		}
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_write_col failed (%d) at %s=%d, colnum=%d\n",
					pname, istat, p->name, v, p->colnum);
			break;
		}
	}

/* check cfitsio error */
	if ( istat ) {
		fprintf(stderr, "%s: fits_write_col failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

	hp->irow++;
	goto again;
}

void
AEcomHKfitsWrite_endrun(int *status)
{
	int i;
	int istat = 0;

	for (i = 0; i < com.nhk; i++) {
		fitsfile *fp = hkdata[i].fp;
		if ( ffmkyj(fp, "NAXIS2", hkdata[i].irow-1, "&", &istat) ) {
		  fprintf(stderr, "\
%s: Error in ffmkyj() (status=%d)\n",pname,istat);
		  return;
		}
		stdkeys.tstart = hkdata[i].tstart;
		stdkeys.tstop = hkdata[i].tstop;
		if ( AEupdateStdTimeKeys(fp, &stdkeys, 0, &istat) != ANL_TRUE ){
			fprintf(stderr, "\
%s: Error in updateStdTimeKeys() (status=%d)\n",pname, istat);
			return;
		}
		/* data and checksum */
		if ( fits_write_date(fp, &istat) ) {
			fprintf(stderr, "\
%s: fits_write_date() failed (status=%d)\n", pname, istat);
			return;
		}
		if (fits_write_chksum(fp, &istat)) {
			fprintf(stderr, "\
%s: fits_write_chksum failed (%d)\n", pname, istat);
			return;
		}
	}

	stdkeys.tstart = hkdata[0].tstart;
	stdkeys.tstop = hkdata[0].tstop;

	for (i = 1; i < com.nhk; i++) {
		printf("%s: merging '%s'\n", pname, hkdata[i].tmpfile);
		fits_flush_file(hkdata[i].fp, &istat);	/* required for cfitsio2.031 */
		fits_copy_hdu(hkdata[i].fp, hkdata[0].fp, 0, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_copy_hdu failed (%d)\n", pname, istat);
			istat = 0;
		}

		printf("\
%s: deleted '%s'\n", pname, hkdata[i].tmpfile);
		fits_delete_file(hkdata[i].fp, &istat);

		if ( 0.0 != hkdata[i].tstart && hkdata[i].tstart < stdkeys.tstart ) {
			stdkeys.tstart = hkdata[i].tstart;
		}
		if ( 0.0 != hkdata[i].tstop && stdkeys.tstop < hkdata[i].tstop ) {
			stdkeys.tstop = hkdata[i].tstop;
		}
	}

/* update primary HDU */
	if ( AEupdateStdTimeKeys(hkdata[0].fp, &stdkeys, 1, &istat) != ANL_TRUE ){
		fprintf(stderr, "\
%s: Error in updateStdTimeKeys() (status=%d)\n",pname, istat);
		return;
	}
	if ( fits_write_date(hkdata[0].fp, &istat) ) {
		fprintf(stderr, "\
%s: fits_write_date() failed (status=%d)\n", pname, istat);
	  return;
	}
	if ( fits_write_chksum(hkdata[0].fp, &istat) ) {
		fprintf(stderr, "\
%s: fits_write_chksum failed (%d)\n", pname, istat);
		return;
	}

	if ( fits_close_file(hkdata[0].fp, &istat) ) {
		fprintf(stderr, "\
%s: fits_close_file('%s') failed (status=%d)\n",
				pname, hkdata[0].tmpfile, istat);
		return;
	}

	if ( *com.filename ) {
		unlink(com.filename);
		rename(hkdata[0].tmpfile, com.filename);
		printf("\
%s: renamed to '%s'\n", pname, com.filename);
	}

	*status = ANL_OK;
}

void
AEcomHKfitsWrite_exit(int *status)
{
	*status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
