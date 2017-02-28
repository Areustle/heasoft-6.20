/*
 XRSpixelCoincidence.c

	1999/12/09 Y.ISHISAKI	version 1.0

	2000/01/26 Y.ISHISAKI	version 1.1
		change to work with less memory

	2000/01/26 Y.ISHISAKI	version 1.2
		in the first place, check event file sorted or not

	2003/09/30 Y.ISHISAKI	version 1.2-headas
		modified for HEADAS environment

	2004/01/08 Y.ISHISAKI	version 1.3
		error outputs on stderr
		print raw number on time inversion

	2005/02/16 Y.ISHISAKI	version 1.4
		renamed to XRSpixelCoincidence
		BnkPut "XRS:EVENT:OROW" in XRSpixelCoincidence_endrun()
		increase width for summary information

	2005/04/28 Y.ISHISAKI	version 1.5
		bug fix in BnkPut "XRS:EVENT:OROW" when events are in time order
		free allocated memories

	2005/04/28 Y.ISHISAKI	version 1.6
		try qsort() after failed to sort for myself

	2005/07/26 Y.ISHISAKI	version 2.0
		new parameters, delta_t, delta_t_pair/first/last, total_duration,
			energy_lo_cut,energy_lo_ka_pair,energy_hi_ka_pair,energy_lo_kb_pair
		calculate PIX_COINCIDENCE, in investigate_pixco()
		write PIXCO statistics in HISTORY

	2005/07/29 Y.ISHISAKI	version 2.1
		compare with irow after TIME in compare_time_of_rows()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "bnk.h"
#include "cli.h"
#include "com.h"
#include "fitsio.h"
#include "pil.h"
#include "headas.h"

static char pname[] = "XRSpixelCoincidence";
char XRSpixelCoincidence_version[] = "version 2.1";

static struct {
	double delta_t;
	double delta_t_pair;
	double delta_t_first;
	double delta_t_last;
	double total_duration;
	double energy_lo_cut;
	double energy_lo_ka_pair;
	double energy_hi_ka_pair;
	double energy_lo_kb_pair;

	int calpixel;
	unsigned int statistics[256];
} com;

static struct {
	int PIXEL;
	int TIME;
	int PIXCO;
	int TYPE;
	int EPI;
} col;

static struct {
	int n_idx;
	unsigned int *idx;
	unsigned char *PIXEL;
	double *TIME;
	signed char *PIXCO;
	float *EPI;
} v;

#define GetTIME(fp, irow, val) \
if ( NULL == v.TIME ) {\
	fits_read_col_dbl(fp, col.TIME, irow, 1, 1, 0., &val, &anul, &istat);\
	if ( istat ) {\
		fprintf(stderr, "\
%s: fits_read_col('TIME') failed at irow=%ld\n", pname, (long)irow);\
		goto error;\
	}\
} else {\
	val = v.TIME[irow-1];\
}

#define GetEPI(fp, irow, val) \
if ( NULL == v.EPI ) {\
	fits_read_col_dbl(fp, col.EPI, irow, 1, 1, 0, &val, &anul, &istat);\
	if ( istat ) {\
		fprintf(stderr, "\
%s: fits_read_col('EPI') failed at irow=%ld\n", pname, (long)irow);\
		goto error;\
	}\
} else {\
	val = v.EPI[irow-1];\
}

#define GetPIXEL(fp, irow, val) \
if ( NULL == v.PIXEL ) {\
	fits_read_col_int(fp, col.PIXEL, irow, 1, 1, 0, &val, &anul, &istat);\
	if ( istat ) {\
		fprintf(stderr, "\
%s: fits_read_col('PIXEL') failed at irow=%ld\n", pname, (long)irow);\
		goto error;\
	}\
} else { \
	val = v.PIXEL[irow-1];\
}

#define GetPIXCO(fp, irow, val) \
if ( NULL == v.PIXCO ) {\
	fits_read_col_int(fp, col.PIXCO, irow, 1, 1, 0, &val, &anul, &istat);\
	if ( istat ) {\
		fprintf(stderr, "\
%s: fits_read_col('PIX_COINCIDENCE') failed at irow=%ld\n", pname, (long)irow);\
		goto error;\
	}\
} else {\
	val = v.PIXCO[irow-1];\
}

#define PutPIXCO(fp, irow, val) \
if ( NULL == v.PIXCO ) {\
	fits_write_col_int(fp, col.PIXCO, irow, 1, 1, &val, &istat);\
	if ( istat ) {\
		fprintf(stderr, "\
%s: fits_write_col('PIX_COINCIDENCE') failed at irow=%ld\n", pname, (long)irow);\
		goto error;\
	}\
} else {\
	v.PIXCO[irow-1] = val;\
}

fitsfile *ifp_base;
static unsigned int *rows_base;
static int qsort_error_flag;

static int
compare_time_of_rows(unsigned int *row1, unsigned int *row2)
{
	int anul;
	long irow1, irow2;
	double t1, t2;

	int istat = 0;

	irow1 = *row1;
	GetTIME(ifp_base, irow1, t1);

	irow2 = *row2;
	GetTIME(ifp_base, irow2, t2);

	if ( t1 < t2 ) {
		return -1;
	} else if ( t1 > t2 ) {
		return +1;
	}

	if ( irow1 < irow2 ) {
		return -1;
	} else if ( irow1 > irow2 ) {
		return +1;
	}

	return 0.0;

 error:
	qsort_error_flag = istat;
	return 0.0;
}

static int
sort_events(fitsfile *ifp, int nevent, unsigned int *rows)
{
	ifp_base = ifp;
	rows_base = rows;
	qsort_error_flag = 0;
	qsort(rows, nevent, sizeof(*rows), compare_time_of_rows);
	if ( qsort_error_flag ) {
		goto error;
	}
	return 0;

 error:
	return qsort_error_flag;
}

static int
investigate_pixco(fitsfile *fp)
{
	int i, i0, i1, pix0, pix1, anul, pixco;
	double t0, t1, t0_tot, t1_tot, t1_fst, t0_lst, epi;

	int istat = 0;

/* test 'delta_t' */
	for (i = 0; i < v.n_idx - 1; i++) {
		GetTIME(fp, v.idx[i], t0);
		GetTIME(fp, v.idx[i+1], t1);
		if ( t1 <= t0 + com.delta_t ) {
			pixco = 2;		/* tentatively mark as paired events */
			PutPIXCO(fp, v.idx[i], pixco);
			PutPIXCO(fp, v.idx[i+1], pixco);
		}
	}

/* test 'delta_t_first', 'delta_t_last', 'total_duration' */
	i = 0;
	while ( i < v.n_idx ) {
		GetPIXCO(fp, v.idx[i], pixco);
		if ( 2 != pixco ) {
			i++;
			continue;
		}

/* remember starting position */
		i0 = i;

/* search for the end of consecutive cluster */
		for (i1 = i0 + 1; i1 < v.n_idx; i1++) {
			GetPIXCO(fp, v.idx[i1], pixco);
			if ( 2 != pixco ) {
				break;
			}
		}

/* set next position */
		i = i1;

/* search forward 'total_duration' & 'delta_t_last' */
		GetTIME(fp, v.idx[i0], t0_tot);
		GetTIME(fp, v.idx[i-1], t0_lst);
		while ( i1 < v.n_idx ) {
			GetTIME(fp, v.idx[i1], t1);
			if ( t1 <= t0_tot + com.total_duration ||
				 t1 <= t0_lst + com.delta_t_last ) {
				GetPIXCO(fp, v.idx[i1], pixco);
				if ( pixco < 2 ) {
					pixco = 3;		/* set to 3 as gray zone */
					PutPIXCO(fp, v.idx[i1], pixco);
				}
				i1++;
				continue;
			}
			break;
		}

/* search backward for 'total_duration' & 'delta_t_first' */
		i1 = i - 1;	/* back to end postion */
		GetTIME(fp, v.idx[i1], t1_tot);
		GetTIME(fp, v.idx[i0], t1_fst);
		while ( 0 < i0 ) {
			i0--;
			GetTIME(fp, v.idx[i0], t0);
			if ( t1_tot <= t0 + com.total_duration ||
				 t1_fst <= t0 + com.delta_t_first ) {
				GetPIXCO(fp, v.idx[i0], pixco);
				if ( pixco < 2 ) {
					pixco = 3;		/* set to 3 as gray zone */
					PutPIXCO(fp, v.idx[i0], pixco);
				}
				continue;
			}
			break;
		}

	}

/* count up member of consecutive cluster */
	i = 0;
	while ( i < v.n_idx ) {
		GetPIXCO(fp, v.idx[i], pixco);
		if ( pixco < 2 ) {
			i++;
			continue;
		}

/* remember starting position */
		i0 = i;

/* search for the end of consecutive cluster */
		for (i1 = i0 + 1; i1 < v.n_idx; i1++) {
			GetPIXCO(fp, v.idx[i1], pixco);
			if ( pixco < 2 ) {
				break;
			}
		}

/* set next position */
		i = i1;

/* calculate pixco */
		pixco = i1 - i0;
		if ( 31 < pixco ) {
			pixco = 32;
		} else if ( 2 == pixco ) {
/* check for paired event */
			i1--;
			GetTIME(fp, v.idx[i0], t0);
			GetTIME(fp, v.idx[i1], t1);
			if ( com.delta_t_pair < t1 - t0 ) {
				pixco = 1;		/* set to clean */
			} else {
				GetPIXEL(fp, v.idx[i0], pix0);
				GetPIXEL(fp, v.idx[i1], pix1);
				if ( pix0 != com.calpixel && pix1 != com.calpixel ) {
					pixco = 2;	/* paired events without CAL pixel */
				} else {
					pixco = -1;	/* likely false coincidence with CAL pixel */
					if ( pix0 == com.calpixel ) {
	GetEPI(fp, v.idx[i0], epi);
	if ( epi < com.energy_lo_ka_pair ||
		(com.energy_hi_ka_pair < epi && epi < com.energy_lo_kb_pair) ) {
		pixco = -2;	/* likely escape event of CAL pixel */
	}
					}
					if ( pix1 == com.calpixel ) {
	GetEPI(fp, v.idx[i1], epi);
	if ( epi < com.energy_lo_ka_pair ||
		(com.energy_hi_ka_pair < epi && epi < com.energy_lo_kb_pair) ) {
		pixco = -2;	/* likely escape event of CAL pixel */
	}
					}
				}
			}
		}

/* set pixco */
		while ( i0 < i ) {
			PutPIXCO(fp, v.idx[i0], pixco);
			i0++;
		}

	}

	return 0;

 error:
	return -1;
}

void
XRSpixelCoincidence_startup(int *status)
{
	v.TIME  = NULL;
	v.PIXCO = NULL;
	v.PIXEL = NULL;

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   %.3fe-3 sec\n", "DELTA_T", 1000*com.delta_t);
	printf("%20s   %.3fe-3 sec\n", "DELTA_T_PAIR", 1000*com.delta_t_pair);
	printf("%20s   %.3fe-3 sec\n", "DELTA_T_FIRST", 1000*com.delta_t_first);
	printf("%20s   %.3fe-3 sec\n", "DELTA_T_LAST", 1000*com.delta_t_last);
	printf("%20s   %.3fe-3 sec\n", "TOTAL_DURATION", 1000*com.total_duration);
	printf("%20s   %.1f eV\n", "ENERGY_LO_CUT", com.energy_lo_cut);
	printf("%20s   %.1f eV\n", "ENERGY_LO_Ka_PAIR", com.energy_lo_ka_pair);
	printf("%20s   %.1f eV\n", "ENERGY_HI_Ka_PAIR", com.energy_hi_ka_pair);
	printf("%20s   %.1f eV\n", "ENERGY_LO_Kb_PAIR", com.energy_lo_kb_pair);
}

void
XRSpixelCoincidence_com(int *status)
{
#define NVAL	11
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"DELTA_T",
		"DELTA_T_PAIR",
		"DELTA_T_FIRST",
		"DELTA_T_LAST",
		"TOTAL_DURATION",
		"ENERGY_LO_CUT",
		"ENERGY_LO_Ka_PAIR",
		"ENERGY_HI_Ka_PAIR",
		"ENERGY_LO_Kb_PAIR",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"delta time for each interval (s)",
		"delta time for paired events (s)",
		"delta time for the first interval (s)",
		"delta time for the last interval (s)",
		"total duration of a consecutive cluster (s)",
		"low energy cut in the pre-processing (eV)",
		"low E cut for escape of Mn-Ka on CAL for paired events (eV)",
		"high E cut for escape of Mn-Ka on CAL for paired events (eV)",
		"low E cut for escape of Mn-Kb on CAL for paired events (eV)",
		"exit from this menu"
	};
	int nreply = 1;
	int answer[2];

	if ( *status ) {	/* ftools */
		*status = ANL_QUIT;
		if (
PILGetReal("delta_t", &com.delta_t) ||
PILGetReal("delta_t_pair", &com.delta_t_pair) ||
PILGetReal("delta_t_first", &com.delta_t_first) ||
PILGetReal("delta_t_last", &com.delta_t_last) ||
PILGetReal("total_duration", &com.total_duration) ||
PILGetReal("energy_lo_cut", &com.energy_lo_cut) ||
PILGetReal("energy_lo_ka_pair", &com.energy_lo_ka_pair) ||
PILGetReal("energy_hi_ka_pair", &com.energy_hi_ka_pair) ||
PILGetReal("energy_lo_kb_pair", &com.energy_lo_kb_pair) ||
			 0 ) {
			return;
		}
		*status = ANL_OK;;
		return;
	}

	for (;;) {
		char *p;
		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("DELTA_T", p) ) {
			CLfdprd(p, &com.delta_t);
		} else if ( 0 == strcmp("DELTA_T_PAIR", p) ) {
			CLfdprd(p, &com.delta_t_pair);
		} else if ( 0 == strcmp("DELTA_T_FIRST", p) ) {
			CLfdprd(p, &com.delta_t_first);
		} else if ( 0 == strcmp("DELTA_T_LAST", p) ) {
			CLfdprd(p, &com.delta_t_last);
		} else if ( 0 == strcmp("TOTAL_DURATION", p) ) {
			CLfdprd(p, &com.total_duration);
		} else if ( 0 == strcmp("ENERGY_LO_CUT", p) ) {
			CLfdprd(p, &com.energy_lo_cut);
		} else if ( 0 == strcmp("ENERGY_LO_Ka_PAIR", p) ) {
			CLfdprd(p, &com.energy_lo_ka_pair);
		} else if ( 0 == strcmp("ENERGY_HI_Ka_PAIR", p) ) {
			CLfdprd(p, &com.energy_hi_ka_pair);
		} else if ( 0 == strcmp("ENERGY_LO_Kb_PAIR", p) ) {
			CLfdprd(p, &com.energy_lo_kb_pair);
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
XRSpixelCoincidence_init(int *status)
{
	show_parameter();
	printf("\n");

	*status = ANL_OK;
}

void
XRSpixelCoincidence_his(int *status)
{
	*status = ANL_OK;
}

void
XRSpixelCoincidence_bgnrun(int *status)
{
	fitsfile *fp;
	int used = 0;

	BnkGet("XRS:EVENT:OFP", sizeof(fp), &used, &fp);
	if ( used == sizeof(fp) ) {
		int istat = 0;
		char buf[80];

		sprintf(buf, "%s %s", pname, XRSpixelCoincidence_version);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  delta_t=%.3fe-3 (s)", com.delta_t*1000);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  delta_t_pair=%.3fe-3 (s)", com.delta_t_pair*1000);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  delta_t_first=%.3fe-3 (s)", com.delta_t_first*1000);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  delta_t_last=%.3fe-3 (s)", com.delta_t_last*1000);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  total_duration=%.3fe-3 (s)", com.total_duration*1000);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  energy_lo_cut=%.1f (eV)", com.energy_lo_cut);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  energy_lo_ka_pair=%.1f (eV)", com.energy_lo_ka_pair);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  energy_hi_ka_pair=%.1f (eV)", com.energy_hi_ka_pair);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  energy_lo_kb_pair=%.1f (eV)", com.energy_lo_kb_pair);
		fits_write_history(fp, buf, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
			*status = ANL_QUIT;
			return;
		}
	}

	*status = ANL_OK;
}

void
XRSpixelCoincidence_ana(int *nevent, int *eventid, int *status)
{
	*status = ANL_QUIT;	/* do nothing here */
}

void
XRSpixelCoincidence_endrun(int *status)
{
	int used, istat, anul;
	long irow, orow, nevent, naxis1;
	fitsfile *ifp, *ofp;
	char *infile, *key, type[2], history[80];
	unsigned char *rowbuf;
	int i, pixco;
	double t0, t1;
	double epi;

	char *type_ptr[1];		/* used for reading TYPE column */
	type_ptr[0] = type;

/* get input/output fits file pointer */
	BnkGet("XRS:EVENT:IFP", sizeof(ifp), &used, &ifp);
	BnkGet("XRS:EVENT:OFP", sizeof(ofp), &used, &ofp);
	BnkGet("XRS:EVENT:IFILE_NAME:PTR", sizeof(infile), &used, &infile);

	istat = 0;

	fflush(NULL); printf("\
%s:\n\
   reading '%s'\n", pname, infile);

/* get CALPIXEL keyword */
	if (
fits_read_key(ifp, TINT, key="CALPIXEL", &com.calpixel, NULL, &istat) ||
		 0 ) {
		fprintf(stderr, "\
%s: fits_read_key('%s') failed (%d)\n", pname, key, istat);
		goto error;
	}
	printf("\
   CALPIXEL = %d\n", com.calpixel);
	fflush(NULL);

/* get column numbers */
	if (
fits_get_colnum(ifp, CASESEN, key="TIME", &col.TIME, &istat) ||
fits_get_colnum(ifp, CASESEN, key="EPI", &col.EPI, &istat) ||
fits_get_colnum(ifp, CASESEN, key="PIXEL", &col.PIXEL, &istat) ||
fits_get_colnum(ifp, CASESEN, key="TYPE", &col.TYPE, &istat) ||
fits_get_colnum(ifp, CASESEN, key="PIX_COINCIDENCE", &col.PIXCO, &istat) ) {
		fprintf(stderr, "\
%s: fits_get_colnum('%s') failed\n", pname, key);
		goto error;
	}

/* get number of rows */
	fits_read_key_lng(ifp, "NAXIS2", &nevent, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key_lng(\"NAXIS2\") failed (%d)\n", pname, istat);
		goto error;
	}

/* put number of output rows
  in order not to be truncated in XRSeventFitsRD_exit */
	orow = nevent + 1;
	BnkfPutM("XRS:EVENT:OROW", sizeof(orow), &orow);

/* allocate memory for v.idx area (required) */
	v.idx = malloc(nevent * sizeof(*v.idx));
	if ( NULL == v.idx ) {
		fprintf(stderr, "\
%s: malloc(%ld) failed for v.idx area\n", pname, nevent * sizeof(*v.idx));
		goto error;
	}

/* allocate memory for TIME column (not required) */
	v.TIME = malloc(nevent * sizeof(*v.TIME));
	if ( NULL != v.TIME ) {
fits_read_col_dbl(ifp, col. TIME, 1, 1, nevent, 0., v.TIME, &anul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_read_col_dbl(\"TIME\") failed (%d)\n", pname, istat);
			goto error;
		}
	}
	fflush(NULL); printf("\
   testing if %ld events are in TIME order ...\n", nevent);
	fflush(NULL);
	GetTIME(ifp, 1L, t0);
	for (irow = 2; irow <= nevent; irow++) {
		GetTIME(ifp, irow, t1);
		if ( t1 < t0 ) {
			break;
		}
		t0 = t1;
	}
	if ( nevent < irow ) {
/* already sorted */
		fflush(NULL); printf("\
   okay, already sorted.\n");
		fflush(NULL);
		fits_copy_data(ifp, ofp, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_copy_data() failed (%d)\n", pname, istat);
			goto error;
		}
		goto skip;
	}

/* try sorting for myself */
	fflush(NULL); printf("\
   sorting %ld events ...\n", nevent);
	fflush(NULL);
	for (irow = 1; irow <= nevent; irow++) {
		v.idx[irow-1] = irow;
	}
	if ( sort_events(ifp, nevent, v.idx) ) {
		goto error;
	}

	fits_read_key_lng(ifp, "NAXIS1", &naxis1, NULL, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key_lng(\"NAXIS1\") failed (%d)\n", pname, istat);
		goto error;
	}

	rowbuf = malloc(naxis1);
	if ( NULL == rowbuf ) {
		fprintf(stderr, "\
%s: malloc(%ld) failed for row buffer\n", pname, naxis1);
		goto error;
	}

	fits_flush_file(ofp, &istat);
	fflush(NULL); printf("\
   writing %ld events ...\n", nevent);
	fflush(NULL);
	for (orow = 1; orow <= nevent; orow++) {
		irow = v.idx[orow-1];
		fits_read_tblbytes(ifp, irow, 1, naxis1, rowbuf, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_read_tblbytes(irow=%ld) failed (%d)\n", pname, irow, istat);
			goto error;
		}
		fits_write_tblbytes(ofp, orow, 1, naxis1, rowbuf, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_write_tblbytes(orow=%ld) failed (%d)\n", pname, orow, istat);
			goto error;
		}
	}
	free(rowbuf);

/* read TIME column again from sorted file */
	if ( NULL != v.TIME ) {
fits_read_col_dbl(ofp, col. TIME, 1, 1, nevent, 0., v.TIME, &anul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_read_col_dbl(\"TIME\") failed (%d)\n", pname, istat);
			goto error;
		}
	}

 skip:

	fits_flush_file(ofp, &istat);
	if ( istat ) {
		goto error;
	}

/* allocate memory for PIXEL & PIX_COINCIDENCE (not required) */
	v.PIXCO = malloc(nevent * sizeof(*v.PIXCO));
	v.PIXEL = malloc(nevent * sizeof(*v.PIXEL));

	if ( NULL != v.PIXEL ) {
fits_read_col_byt(ofp, col.PIXEL, 1, 1, nevent, 0, v.PIXEL, &anul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_read_col_byt(\"PIXEL\") failed (%d)\n", pname, istat);
			goto error;
		}
	}

/* pre-screening with TYPE & EPI */
	v.n_idx = 0;
	for (irow = 1; irow <= nevent; irow++) {
fits_read_col_str(ofp, col.TYPE, irow, 1, 1, "", type_ptr, &anul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_read_col(\"TYPE\") failed at irow=%ld (%d)\n", pname, irow, istat);
			goto error;
		}
		if ( 'B' == type[0] || 'N' == type[0] ) {
			pixco = -32;	/* neglected by xrspixco */
			PutPIXCO(ofp, irow, pixco);
			continue;
		}
fits_read_col_dbl(ofp, col.EPI, irow, 1, 1, 0, &epi, &anul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_read_col(\"EPI\") failed at irow=%ld (%d)\n", pname, irow, istat);
			goto error;
		}
		if ( epi <= com.energy_lo_cut ) {
			pixco = -32;	/* neglected by xrspixco */
			PutPIXCO(ofp, irow, pixco);
			continue;
		}
		pixco = 1; 			/* tentatively mark as clean */
		PutPIXCO(ofp, irow, pixco);
		v.idx[v.n_idx] = irow;
		v.n_idx++;
	}
	printf("\
   investigating pixel coincidence ...\n");
	fflush(NULL);
	investigate_pixco(ofp);

/* write back results */
	if ( NULL != v.PIXCO ) {
		printf("\
   writing results ...\n");
		fflush(NULL);
		for (irow = 1; irow <= nevent; irow++) {
			pixco = v.PIXCO[irow-1];
fits_write_col_int(ofp, col.PIXCO, irow, 1, 1, &pixco, &istat);
			if ( istat ) {
				fprintf(stderr, "\
%s: fits_write_col(\"PIXCO\") failed at irow=%ld (%d)\n", pname, irow, istat);
				goto error;
			}
		}
	}

/* calculate statistics */
	printf("\
   calculating statistics ...\n");
	fflush(NULL);
	for (i = 0; i < 256; i++) {
		com.statistics[i] = 0;
	}
	for (irow = 1; irow <= nevent; irow++) {
		GetPIXCO(ofp, irow, pixco);
		com.statistics[pixco & 255]++;
	}

	printf("\
   finished.\n");
	fflush(NULL);

/* print statistics */
	printf("\n\
PIX_COINCIDENCE	 NUM_EVENTS\n");
	printf("%15d%10d\n", -32, com.statistics[(-32)&255]);
	for (i = -2; i <= 32; i++) {
		printf("%15d%10d\n", i, com.statistics[i&255]);
	}
	fflush(NULL);

	strcpy(history, "\
  ===============================================================");
	fits_write_history(ofp, history, &istat);
	strcpy(history, "\
   PIXCO     NUM   PIXCO     NUM   PIXCO     NUM   PIXCO     NUM");
	fits_write_history(ofp, history, &istat);
	strcpy(history, "\
  ---------------------------------------------------------------");
	fits_write_history(ofp, history, &istat);

	sprintf(history, "   %3d:%9d   %3d:%9d   %3d:%9d   %3d:%9d",
		-32, com.statistics[(-32)&255],
		 -2, com.statistics[(-2)&255],
		 -1, com.statistics[(-1)&255],
		  0, com.statistics[0]);
	fits_write_history(ofp, history, &istat);
	for (i = 0; i < 8; i++) {
		sprintf(history, "   %3d:%9d   %3d:%9d   %3d:%9d   %3d:%9d",
				i*4+1, com.statistics[i*4+1],
				i*4+2, com.statistics[i*4+2],
				i*4+3, com.statistics[i*4+3],
				i*4+4, com.statistics[i*4+4]);
		fits_write_history(ofp, history, &istat);
	}
	strcpy(history, "\
  ---------------------------------------------------------------");
	fits_write_history(ofp, history, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

/* free allocated memory */
	if ( NULL != v.PIXEL ) free(v.PIXEL);
	if ( NULL != v.PIXCO ) free(v.PIXCO);
	if ( NULL != v.TIME )  free(v.TIME);

	*status = ANL_OK;
	return;

 error:
	if ( NULL != v.PIXEL ) free(v.PIXEL);
	if ( NULL != v.PIXCO ) free(v.PIXCO);
	if ( NULL != v.TIME )  free(v.TIME);

	fits_delete_file(ofp, &istat);

	*status = ANL_QUIT;

	return;
}

void
XRSpixelCoincidence_exit(int *status)
{
	*status = ANL_OK;
}
