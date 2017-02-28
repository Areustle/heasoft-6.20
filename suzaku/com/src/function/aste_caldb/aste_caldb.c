/*
  aste_caldb: CALDB support routines for SUZAKU

	2006-07-02	Y.ISHISAKI	version 1.0

	2006-09-15	Y.ISHISAKI	version 1.1
		add aste_caldb_find_leapfile()

	2006-05-14	Y.ISHISAKI	version 1.2
		add aste_caldb_find_rigidity(), aste_caldb_find_candidate()
		check if o_leapfile is empty in find_leapfile()
		accept e.g. "CALDB;${LHEA_DATA}/leapsec.fits" aste_caldb_find_leapfile

	2006-05-28	Y.ISHISAKI	version 1.3
		add aste_caldb_find(), adopted from xisrsp_get_caldb_file()
		do not show error in find_leapfile(), find_rigidity()
		only accept ';' (semicolon) in check_delimiters()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "cli.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_time.h"
#include "aste_caldb.h"
#include "hdcal.h"

static char pname[] = "aste_caldb-1.3";

typedef struct {
	char *fname;
	long extno;
} FNAME_EXTNO;

int
aste_caldb_init(CALDB_INFO *caldb)
{
	caldb->telescop	= NULL;
	caldb->instrume	= NULL;
	caldb->detnam	= NULL;
	caldb->filter	= NULL;
	caldb->codename	= NULL;
	caldb->expr		= NULL;
	caldb->date0	= NULL;
	caldb->time0	= NULL;
	caldb->date1	= NULL;
	caldb->time1	= NULL;
	caldb->nfound	= 0;
	caldb->status	= 0;
	caldb->filename	= NULL;
	caldb->fnames	= NULL;
	caldb->extno	= NULL;

	return 0;
}

int
aste_caldb_free(CALDB_INFO *caldb)
{
	if ( NULL != caldb->filename ) {
		free(caldb->filename);
	}
	aste_caldb_init(caldb);

	return 0;
}

static int
compare_fname_extno(const void *P1, const void *P2)
{
#define p1	((FNAME_EXTNO *)P1)
#define p2	((FNAME_EXTNO *)P2)

	int f, e;

	f = - strcmp(p1->fname, p2->fname);		/* reverse sort by fname */
	e = p1->extno - p2->extno;				/* normal sort by extno */
	if ( f < 0 ) {
		return -1;
	} else if ( 0 < f ) {
		return +1;
	} else if ( e < 0 ) {
		return -1;
	} else if ( 0 < e ) {
		return +1;
	}

#undef p1
#undef p2

	return 0;
}

int
aste_caldb_get(CALDB_INFO *caldb)
{
#define MAX_FILENAME_LEN	4096
	char filename_buffer[MAX_FILENAME_LEN];
	char *filename, **fnames, **online;
	char *fnames1, *online1;
	char online_buffer[MAX_FILENAME_LEN];
	long *extno, extno_buffer;
	int i, nret, nfound;
	FNAME_EXTNO *sortlist;

	CALDB_INFO a = *caldb;

	if ( NULL == a.telescop || '\0' == *a.telescop ) {
		a.telescop = aste_telescop();
	}
/*	if ( NULL == a.instrume || '\0' == *a.instrume ) {
		a.instrume = "-";
	}*/
	if ( NULL == a.detnam || '\0' == *a.detnam ) {
		a.detnam = "-";
	}
	if ( NULL == a.filter || '\0' == *a.filter ) {
		a.filter = "-";
	}
/*	if ( NULL == a.codename || '\0' == *a.codename ) {
		a.codename = "-";
	}*/
	if ( NULL == a.expr || '\0' == *a.expr ) {
		a.expr = "-";
	}
	if ( NULL == a.date0 || '\0' == *a.date0 ) {
		a.date0 = "2005-08-17";	/* DATE & TIME when both XIS & HXD is ON */
	}
	if ( NULL == a.time0 || '\0' == *a.time0 ) {
		a.time0 = "11:00:00";	/* DATE & TIME when both XIS & HXD is ON */
	}
	if ( NULL == a.date1 || '\0' == *a.date1 ) {
		a.date1 = a.date0;
	}
	if ( NULL == a.time1 || '\0' == *a.time1 ) {
		a.time1 = a.time0;
	}

	fnames = &fnames1;
	online = &online1;
	fnames[0] = filename_buffer;
	online[0] = online_buffer;
	extno = &extno_buffer;

	HDgtcalf(a.telescop, a.instrume, a.detnam, a.filter, a.codename,
		a.date0, a.time0, a.date1, a.time1, a.expr,
		1, MAX_FILENAME_LEN, fnames, extno, online,
		&nret, &nfound, &a.status);

/* no candidate */
	if ( a.status || 0 == nfound ) {
	error:
		caldb->nfound = 0;
		caldb->status = a.status;
		caldb->filename = NULL;
		caldb->fnames = NULL;
		caldb->extno = NULL;
		return a.status;
	}

/* allocate memory */
	filename = malloc( nfound *
		( MAX_FILENAME_LEN + sizeof(*a.fnames) + sizeof(*a.extno) ) );
	if ( NULL == filename ) {
		a.status = -1;
		goto error;
	}

	fnames = (char **)&filename[ nfound * MAX_FILENAME_LEN ];
	for (i = 0; i < nfound; i++) {
		fnames[i] = &filename[i * MAX_FILENAME_LEN];
	}
	extno = (long *)&fnames[nfound];

/* signle candidate */
	if ( 1 == nfound ) {
		strcpy(filename, filename_buffer);
		*extno = extno_buffer;
		goto skip;
	}

/* multiple candidates, allocate temporary memory for online */
	online = malloc( nfound *
		( MAX_FILENAME_LEN + sizeof(*online) + sizeof(*sortlist) ) );
	if ( NULL == online ) {
		free(filename);
		a.status = -1;
		goto error;
	}

	sortlist = (FNAME_EXTNO *)&online[nfound];
	online[0] = (char *)&sortlist[nfound];
	for (i = 1; i < nfound; i++) {
		online[i] = &online[0][i * MAX_FILENAME_LEN];
	}

	HDgtcalf(a.telescop, a.instrume, a.detnam, a.filter, a.codename,
		a.date0, a.time0, a.date1, a.time1, a.expr,
		nfound, MAX_FILENAME_LEN, fnames, extno, online,
		&nret, &a.nfound, &a.status);

	if ( a.status ) {
		free(filename);
		free(online);
		goto error;
	}

	if ( nfound != a.nfound || nfound != nret ) {
		anl_msg_error("\
%s: nfound not match (%d!=%d or %d) in the second try\n",
			pname, nfound, a.nfound, nret);
		a.status = -1;
		goto error;
	}

	for (i = 0; i < nfound; i++) {
		strcpy(online[i], fnames[i]);
		sortlist[i].fname = online[i];
		sortlist[i].extno = extno[i];
	}

	qsort(sortlist, nfound, sizeof(*sortlist), compare_fname_extno);

	for (i = 0; i < nfound; i++) {
		strcpy(fnames[i], sortlist[i].fname);
		extno[i] = sortlist[i].extno;
	}

	free(online);	/* ignore online */

 skip:

	caldb->nfound = nfound;
	caldb->status = a.status;
	caldb->filename = filename;
	caldb->fnames = fnames;
	caldb->extno = extno;

	return a.status;
}

static int
check_delimiters(int c)
{
	if ( '\0' == c || ';' == c /*|| ',' == c || ' ' == c || '\t' == c */ ) {
		return ANL_TRUE;
	}
	return ANL_FALSE;
}

static int
split_path(char *filelist, int n, char *outbuf, int outbuf_size)
{
	int i;
	char *p = filelist;

	for (i = 0; i < n; i++) {
		for (;;) {
			if ( check_delimiters(*p) ) {
				break;
			}
			p++;
		}
		if ( '\0' == *p ) {
			break;
		}
		p++;	/* skip delimeter */
	}

	if ( '\0' == *p ) {
		if ( NULL != outbuf && 0 < outbuf_size ) {
			outbuf[0] = '\0';
		}
		return -1;	/* n-th file not found */
	}

	if ( NULL == outbuf && outbuf_size <= 0 ) {
		return 0;	/* successfull split, but no output */
	}

	for (i = 0; i < outbuf_size; i++) {
		if ( check_delimiters(*p) ) {
			break;
		}
		outbuf[i] = *p;
		p++;
	}

	if ( i < outbuf_size ) {
		outbuf[i] = '\0';
		return 0;	/* successfull split */
	}

	outbuf[outbuf_size - 1] = '\0';
	anl_msg_warning("\
%s: WARNING: too long file name, truncated to '%s'\n", pname, outbuf);

	return 0;
}

static char *
find_leapfile(char *o_leapfile)
{
	static char ENV_LHEA_DATA[] = "LHEA_DATA";
	static char LEAPSEC_FITS[] = "leapsec.fits";

	char *lhea_data, *leapfile;
	CALDB_INFO caldb;

	if ( NULL == o_leapfile || '\0' == *o_leapfile ) {
		anl_msg_error("\
%s: find_leapfile() failed, because file name is empty\n", pname);
		return NULL;
	}

	if ( 0 != CLstricmp("CALDB", o_leapfile) &&
		 0 != CLstricmp("AUTO", o_leapfile) ) {
		return o_leapfile;
	}

	aste_caldb_init(&caldb);
	caldb.telescop = "GEN";
	caldb.instrume = "INS";
	caldb.codename = "LEAPSECS";
	aste_caldb_get(&caldb);

	if ( 0 != caldb.status || 0 == caldb.nfound ) {
		if ( 0 == CLstricmp("CALDB", o_leapfile) ) {
/*			anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
				pname, caldb.codename, caldb.status);*/
			return NULL;
		}

		anl_msg_warning("\
%s: WARNING: no CALDB entry for '%s' (status=%d),\n\
    searching 'leapsec.fits' at $LHEA_DATA\n",
				pname, caldb.codename, caldb.status);
		lhea_data = getenv(ENV_LHEA_DATA);
		if ( NULL == lhea_data ) {
			lhea_data = "";
		}
		leapfile = malloc( strlen(lhea_data) + 1 + sizeof(LEAPSEC_FITS) );
		if ( NULL == leapfile ) {
			anl_msg_error("\
%s: malloc() failed for leapfile\n", pname);
		}
		sprintf(leapfile, "%s/%s", lhea_data, LEAPSEC_FITS);
		return leapfile;
	}

	if ( 1 != caldb.nfound ) {
		anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
			pname, caldb.nfound, caldb.codename);
	}

	return caldb.filename;		/* return allocated string */
}

static char *
find_rigidity(char *o_rigidity)
{
	CALDB_INFO caldb;

	if ( NULL == o_rigidity || '\0' == *o_rigidity ) {
		anl_msg_error("\
%s: find_rigidity() failed, because file name is empty\n", pname);
		return NULL;
	}

	if ( 0 != CLstricmp("CALDB", o_rigidity) ) {
		return o_rigidity;
	}

/* caldb support */
	aste_caldb_init(&caldb);
	caldb.telescop = "GEN";
	caldb.instrume = "INS";
	caldb.codename = "RIGIDITY";
	aste_caldb_get(&caldb);
	if ( 0 != caldb.status || 0 == caldb.nfound ) {
/*		anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
			pname, caldb.codename, caldb.status);*/
		return NULL;
	}
	if ( 1 != caldb.nfound ) {
		anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
			pname, caldb.nfound, caldb.codename);
	}

	return caldb.filename;
}

char *
aste_caldb_find_candidate(char *o_candidate, char *(*find_caldb)(char *))
{
	fitsfile *fp;
	int i, istat, candidate_size;
	char *p, *candidate;

	if ( NULL == o_candidate || '\0' == *o_candidate ) {
		candidate = (*find_caldb)(o_candidate);
		if ( NULL == candidate ) goto no_candidates;
		return candidate;
	}

	p = o_candidate;
	for (;;) {
		if ( check_delimiters(*p) ) {
			break;
		}
		p++;
	}
	if ( '\0' == *p ) {
/* no delimiters in o_candidate[] */
		candidate = (*find_caldb)(o_candidate);	/* call original routine */
		if ( NULL == candidate ) goto no_candidates;
		return candidate;
	}

/* exists delimiters */
	candidate_size = strlen(o_candidate) + 1;
	candidate = strdup(o_candidate);
	if ( NULL == candidate ) {
		anl_msg_error("\
%s: malloc(size=%d) failed in find_candidate()\n", pname, candidate_size);
		return NULL;
	}

	for (i = 0; ; i++) {
		if ( split_path(o_candidate, i, candidate, candidate_size) ) {
			break;		/* no next candiates */
		}
		if ( '\0' == *candidate ) {
			continue;	/* empty string, e.g. ";;" */
		}
		p = (*find_caldb)(candidate);
		if ( NULL == p ) {
			continue;	/* go to next candidates */
		}
		istat = 0;
		fits_open_file(&fp, p, READONLY, &istat);	/* try read only open */
		if ( istat ) {
			continue;	/* file open error, go to next candidates */
		}
		fits_close_file(fp, &istat);				/* ignore error */
		if ( p != candidate ) {
			free(candidate);
		}
		return p;		/* found existing candidate */
	}

	free(candidate);

 no_candidates:
	anl_msg_error("\
%s: no possible candidates in '%s'\n", pname, o_candidate);

	return NULL;
}

char *
aste_caldb_find_leapfile(char *o_leapfile)
{
	return aste_caldb_find_candidate(o_leapfile, find_leapfile);
}

char *
aste_caldb_find_rigidity(char *o_rigidity)
{
	return aste_caldb_find_candidate(o_rigidity, find_rigidity);
}

char *
aste_caldb_find(char *instrume, char *codename, char *o_filename)
{
	CALDB_INFO caldb;

	if ( 0 != CLstricmp("CALDB", o_filename) ) {
		return o_filename;
	}

	aste_caldb_init(&caldb);
	caldb.instrume = instrume;
	caldb.codename = codename;
	aste_caldb_get(&caldb);

	if ( 0 != caldb.status || 0 == caldb.nfound ) {
		anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n", pname, codename, caldb.status);
		return NULL;
	}

	if ( 1 != caldb.nfound ) {
		anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
			pname, caldb.nfound, codename);
	}

	return caldb.filename;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
