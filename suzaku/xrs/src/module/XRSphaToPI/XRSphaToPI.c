/*
 XRSphaToPI.c

	1999/08/06 Y.ISHISAKI	version 1.0

        9 Nov 99  KCG modifies to read new format of gain history files
	          as well as to read in the relevant ascii gain template
		  files....
		  for now, if "none" is typed in for a particular grades
		  ascii template file, we will not update the PI column for
		  those flavor of events...
	10 Nov 99 kcg adds gain trimming tool to handle cases where the
	          background to the K Kalpha line is influencing the gain
		  correction.

	1999/12/09 Y.ISHISAKI	version 1.2

	2003/09/30 Y.ISHISAKI	version 1.2-headas
		modifications for HEADAS

	2004/01/08 Y.ISHISAKI	version 1.3
		accept gainhist=none for no gain drift correction
		do not discard events of AntiCo, Clipped, etc

	2004/08/12 Y.ISHISAKI	version 1.4
		no random number generation if rand_seed=0
		do not initialize random number, if already initialized
		dither by [0, 1.) and floor it, corresponding PI=0 to [0, 1.) eV

	2005/05/09 Y.ISHISAKI	version 1.6
		support FITS gain file, e.g. xrs_gain_2005-05-07try2.fits
		change parameters. newly added are gain_file, gain_hist_file
		use functions xrs_gain_file_read(), xrs_gain_param_get() in xrs_gain.c
		add 0.5 to PHA when rand_seed=0
		write PI_ESCAL keyword in event FITS header

	2005/07/06 Y.ISHISAKI	version 1.7
		parameter names gain_hist_file -> driftfile, gain_file -> gainfile
		use aefits_del_write_key_fixdbl() to write PI_ESCAL
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "fitsio.h"
#include "aste_rand.h"
#include "atFunctions.h"
#include "aeFitsHeaderUtil.h"
#include "xrs_gain.h"
#include "pil.h"
#include "headas.h"

static char pname[] = "XRSphaToPI";
char XRSphaToPI_version[] = "version 1.7";

static struct {
	char gainfile[PIL_LINESIZE];
	char driftfile[PIL_LINESIZE];
	int rand_seed;
	double rand_skip;
	fitsfile *fp;
	long irow, nrow;
	double gh[32];
	double gain_trim;
	double *t0, *t1;
	XRS_GAINS *gs;
} com;

void
XRSphaToPI_startup(int *status)
{
	com.t0 = com.t1 = NULL;
	com.rand_seed = 7;
	com.rand_skip = 0.0;

	*status = ANL_OK;
}

static void
show_parameter(void)
{
  printf("\n");
  printf("%s: *** show parameter ***\n", pname);
  printf("\n");
  printf("%20s   '%s'\n", "GAINFILE", com.gainfile);
  printf("%20s   '%s'\n", "DRIFTFILE", com.driftfile);
  printf("%20s   %d\n", "RAND_SEED", com.rand_seed);
  printf("%20s   %.0f\n", "RAND_SKIP", com.rand_skip);
  printf("%20s   %.6f\n", "GAIN_TRIM", com.gain_trim);

}

void
XRSphaToPI_com(int *status)
{
#define NVAL	7
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"GAINFILE",
		"DRIFTFILE",
		"RAND_SEED",
		"RAND_SKIP",
		"GAIN_TRIM",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"gain file name",
		"gain history file name",
		"random number seed",
		"random number skip count",
		"gain trimming factor",
		"exit from this menu"
	};
	int nreply = 1;
	int answer[2];

	if ( *status ) {	/* ftools */

		if (
PILGetFname("gainfile",  com.gainfile) ||
PILGetFname("driftfile", com.driftfile) ||
PILGetInt  ("rand_seed", &com.rand_seed) ||
PILGetReal ("rand_skip", &com.rand_skip) ||
PILGetReal ("gain_trim", &com.gain_trim) ||
			 0 ) {
			*status = ANL_QUIT;
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
		} else if ( 0 == strcmp("GAINFILE", p) ) {
			CLtxtrd(p, com.gainfile, sizeof(com.gainfile));
		} else if ( 0 == strcmp("DRIFTFILE", p) ) {
			CLtxtrd(p, com.driftfile, sizeof(com.driftfile));
		} else if ( 0 == strcmp("RANDOM_SEED", p) ) {
			CLintrd(p, &com.rand_seed);
		} else if ( 0 == strcmp("SKIP_RANDOM", p) ) {
			CLfdprd(p, &com.rand_skip);
		} else if ( 0 == strcmp("GAIN_TRIM", p) ) {
			CLfdprd(p, &com.gain_trim);
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

static struct {
	int start, stop, pixel[32];
} columnNo;

static int
read_gainhist(void)
{
	int i;
	int anynul;

	for (i = 0; i < 32; i++) {
		int istat = 0;
		/*  kcg note.,..  sould the 5 be a 1 ???? below..*/
		fits_read_col_dbl(com.fp, columnNo.pixel[i], com.irow,
						  1, 1, 0.0, &com.gh[i], &anynul, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_read_col_dbl('PIXEL%02d') failed (%d)\n", pname, i, istat);
			return -1;
		}
	}

	return 0;
}

static int
open_gainhist(void)
{
	int i;
	fitsfile *fp;
	int hdutype, anynul;
	char comment[80];
	int hdunum = 2;
	int casesen = TRUE;
	int istat = 0;

	fits_open_file(&fp, com.driftfile, READONLY, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_open_file failed (%d)\n", pname, istat);
		return -1;
	}
	fits_movabs_hdu(fp, hdunum, &hdutype, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_movabs_hdu failed (%d)\n", pname, istat);
		return -1;
	} else if ( BINARY_TBL != hdutype ) {
		fprintf(stderr, "\
%s: hdutype is not BINARY_TBL (%d)\n", pname, istat);
		return -1;
	}
	fits_read_key_lng(fp, "NAXIS2", &com.nrow, comment, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_key('NAXIS2') failed (%d)\n", pname, istat);
		return -1;
	}
	fits_get_colnum(fp, casesen, "START", &columnNo.start, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_get_colnum('START') failed (%d)\n", pname, istat);
		return -1;
	}
	fits_get_colnum(fp, casesen, "STOP", &columnNo.stop, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_get_colnum('STOP') failed (%d)\n", pname, istat);
		return -1;
	}

	com.irow = 1;
	for (i = 0; i < 32; i++) {
		char columnName[32];
		sprintf(columnName, "PIXEL%02d", i);
		fits_get_colnum(fp, casesen, columnName, &columnNo.pixel[i], &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_get_colnum('%s') failed (%d)\n", pname, columnName, istat);
			return -1;
		}
	}
	com.t0 = malloc(2 * com.nrow * sizeof(*com.t0));
	com.t1 = com.t0 + com.nrow;
	if ( NULL == com.t0 ) {
		fprintf(stderr, "\
%s: malloc() failed for START/STOP (NAXIS2=%ld)\n", pname, com.nrow);
		return -1;
	}
	fits_read_col_dbl(fp, columnNo.start,
					  1, 1, com.nrow, 0.0, com.t0, &anynul, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_col_dbl('START') failed (%d)\n", pname, istat);
		return -1;
	}
	fits_read_col_dbl(fp, columnNo.stop,
					  1, 1, com.nrow, 0.0, com.t1, &anynul, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_col_dbl('STOP') failed (%d)\n", pname, istat);
		return -1;
	}

	com.fp = fp;
	if ( read_gainhist() ) {
		return -1;
	}

	return 0;
}

void
XRSphaToPI_init(int *status)
{
	int i;

	show_parameter();
	printf("\n"); fflush(NULL);

	com.gs = xrs_gain_file_read(com.gainfile, NULL);
	if ( NULL == com.gs ) {
		*status = ANL_QUIT;
		return;
	}

/* Next read in gain history file */
	if ( 0 == strcasecmp("none", com.driftfile) ) {
		com.fp = NULL;
		for (i = 0; i < 32; i++) {
			com.gh[i] = 1.0;
		}
	} else if ( open_gainhist() ) {
		*status = ANL_QUIT;
		return;
	}

	if ( 0 == com.rand_seed ) {
		fflush(NULL); printf("\
%s: rand_seed=0, no random number generation\n", pname);
		fflush(NULL);
	} else if ( 0 != aste_rndseed() ) {
		fflush(NULL); printf("\
%s: random number already initialized, skip initialization\n", pname);
		fflush(NULL);
	} else {
		aste_rndtsini(com.rand_seed);
		aste_drndtsn_skipd(com.rand_skip);
	}

	*status = ANL_OK;
}

void
XRSphaToPI_his(int *status)
{
	*status = ANL_OK;
}

void
XRSphaToPI_bgnrun(int *status)
{
	fitsfile *fp;
	int used = 0;

	BnkGet("XRS:EVENT:OFP", sizeof(fp), &used, &fp);
	if ( used == sizeof(fp) && NULL != fp ) {
		int istat = 0;
		char buf[1024];

		sprintf(buf, "%s %s", pname, XRSphaToPI_version);
		fits_write_history(fp, buf, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
			*status = ANL_QUIT;
			return;
		}

		sprintf(buf, "  gainfile='%s'", com.gainfile);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  driftfile='%s'", com.driftfile);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  rand_seed=%d", com.rand_seed);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  rand_skip=%.0f", com.rand_skip);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "  gain_trim=%.6f", com.gain_trim);
		fits_write_history(fp, buf, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
			*status = ANL_QUIT;
			return;
		}

		aefits_del_write_key_fixdbl(fp, "PI_ESCAL", com.gs->pi_escal, 4,
			"energy scale of PI [eV/chan]", &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: aefits_del_write_key_fixdbl('PI_ESCAL') failed (%d)\n", pname, istat);
			*status = ANL_QUIT;
			return;
		}

	}

	*status = ANL_OK;
}

void
XRSphaToPI_ana(int *nevent, int *eventid, int *status)
{
	int ip, used, pixel, pha, pi;
	double aetime, t0, t1;
	double ph, upi, epi;
	XRS_GAIN_PARAMS *g;

	BnkfGetM("XRS:TIME", sizeof(aetime), &used, &aetime);
	BnkfGetM("XRS:PIXEL", sizeof(pixel), &used, &pixel);
	BnkfGetM("XRS:PHA", sizeof(pha), &used, &pha);

	if ( NULL != com.fp ) {
		t0 = com.t0[com.irow-1];
		t1 = com.t1[com.irow-1];
		if ( aetime < t0 || t1 < aetime ) {
			for (com.irow = 1; com.irow <= com.nrow; com.irow++) {
				t0 = com.t0[com.irow-1];
				t1 = com.t1[com.irow-1];
				if ( t0 <= aetime && aetime <= t1 ) {
					if ( read_gainhist() ) {
						*status = ANL_QUIT;
						return;
					}
					break;
				}
			}
			if ( com.nrow < com.irow ) {
				fprintf(stderr, "\
%s: t=%f exceeds gain history file time region\n", pname, aetime);
				*status = ANL_QUIT;
				return;
			}
		}
	}

	g = xrs_gain_param_get(com.gs, pixel, aetime, NULL);
	if ( NULL == g ) {
		*status = ANL_QUIT;
		return;
	}

	if ( 0 == com.rand_seed ) {
		ph = pha + 0.5;
	} else {
		ph = pha + aste_drndts();
	}

	upi = 0.0;
	for (ip = g->np; 0 <= ip; ip--) {
		upi += upi * ph + g->p[ip];
	}
	upi *= com.gain_trim;

	epi = upi * com.gh[pixel];

	ph = floor(epi / com.gs->pi_escal);
	if ( ph < -16384.0 ) {
		pi = -16384;
	} else if ( 32767.0 < ph ) {
		pi = 32767;
	} else {
		pi = (int)ph;
	}

	BnkfPutM("XRS:PI", sizeof(pi), &pi);
	BnkfPutM("XRS:UPI", sizeof(upi), &upi);
	BnkfPutM("XRS:EPI", sizeof(epi), &epi);

	*status = ANL_OK;
	return;
}

void
XRSphaToPI_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRSphaToPI_exit(int *status)
{
	if ( NULL != com.fp ) {
		int istat = 0;
		fits_close_file(com.fp, &istat);
		com.fp = NULL;
	}
	com.rand_skip = aste_drndtsn_gen();
	fflush(NULL); printf("\
%s: number of Random number generated = %.0f\n", pname, com.rand_skip);
	fflush(NULL);

	*status = ANL_OK;
}
