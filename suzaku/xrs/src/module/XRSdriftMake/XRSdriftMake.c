/*
  XRSdriftMake.c

	1999/08/06 Y.ISHISAKI	version 1.0

	1999/08/20 Y.ISHISAKI	version 1.1
		add leapfile, clobber parameters
		call mission_time_init() in XRSdriftMake_init


		27 Oct 99
		kcg tries to remember the changes he made before...
		to make XRSdriftMake use a 4th order poly...

		4 Nov 99
		kcg adds some filler to the shell- so now it actually makes a
		gain history file...this will be a simpe thing...


		8 Nov 99
		addding some fairly serious modifications.
		Now this programn will read in 3 gain files (hi, mid, and lo res)
		it will ignore secondaries, antico, clipped events...
		it will output 4 gain history files:
		0) one based on hires data alone
		1) one based on midres data alone
		2) one based on lores data alone
		3) one based on all of the above.


		9 Nov 99
		This seems to actually be working now- inspite of my
		pure stupidity...

		must keep in mind the following:
		1) not correcting for rate dependences on hires gain yet
		2) not dealing with glitches or coming out of GCC to well
		   yet.
		   and use that instead to correct the gain...

                There are still some problems with the basic functioning...
		for one, it does not seem to be updating the midres and lores guys...


	1999/12/09 Y.ISHISAKI	version 1.2
		bug fix around tstart & tstop
		add gain_trim parameter

	2004/03/14 Y.ISHISAKI	version 1.3
		use AtTimeD (since atFunctions-2.2) instead of AtTime
		include "com.h", <unistd.h>
		remove "cfortran.h"
		remove unused variablaes

	2004/12/20 N.OTA	version 1.4
		modify the program to the ASTRO-E2 XRS specifications
		1) change the initial "current_gains" factor from 1000.0 to 1.0
		2) use a pi range of 5870 < pi < 5910 to calc the mean PI channel
		   every 100 events
		3) use a weighted average of seven MnKa lines (ref. Holzer et al.),
		   5894.264 eV instead of 3312.9 eV

	2005/01/12 N.OTA	version 1.5
		1) include "aste_coord.h",
		   and change the "TELESCOPE" name from "ASTRO-E"
		   to aste_telescope() (i.e. ASTRO-E2)
		2) add pixref parameter and the pixel selection,
		   if ( pixel == cal.pixref ){...}

	2005/03/31 N.OTA	version 1.5.3
		1) add 4 parameters: pimin, pimax, numpimax, and pimean.
		   accumelates numpimax events within pimin < pi < pimax.
		   the weighted average of MnKa lines can be now
		   specified by pimean(eV).
		2) use event types Hp and Mp to calculate the mean PI channel for MnKa
		3) declare ph0 and pi0 as double.
		   add a random number between 0 and 1 to pha
		   and convert it to pi in the same manner as done in xrspi.

	2005/05/09 Y.ISHISAKI	version 1.6
		support FITS gain file, e.g. xrs_gain_2005-05-07try2.fits
		change parameters. newly added are
			gain_file, gain_hist_file, use_Hp, use_Mp, use_Ms, use_Lp, use_Ls
		new functions xrs_gain_file_read(), xrs_gain_param_get() in xrs_gain.c

	2005/07/06 Y.ISHISAKI	version 1.7
		rename XRSgainhistMake -> XRSdriftMake
		parameter names gain_hist_file -> driftfile, gain_file -> gainfile
		copy FITS keywords from input event file
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
#include "aste_coord.h"
#include "aste_rand.h"
#include "xrs_gain.h"
#include "aeFitsHeaderUtil.h"
#include "xrsFitsHeaderUtil.h"
#include "pil.h"
#include "headas.h"

static char pname[] = "XRSdriftMake";
char XRSdriftMake_version[] = "version 1.7";

static XRS_STD_KEYS stdkeys;

static struct {
	char driftfile[PIL_LINESIZE];
	char gainfile[PIL_LINESIZE];
	double gain_trim;
	int use_Hp, use_Mp, use_Ms, use_Lp, use_Ls;
	char leapfile[PIL_LINESIZE];
	int clobber;

	XRS_GAINS *gs;
	char *infile;		/* input event file name */
	fitsfile *ifp;		/* input event file */
	fitsfile *ofp;		/* output drift file */
	long irow;
	double tstart, tstop;
	int rand_seed;
	double rand_skip;

	double t0, t1;
	int numpi;
	double sumpi;
	double cur_gain;

} com;

/* N.O. adds a new structure */
static struct {
	int pixref;
	int pimin, pimax;
	int numpimax;
	double pimean;
} cal;

static char extname[] = "XRS_GAIN_DRIFT";

#define NFIELD	34
static char *ttype[NFIELD] = {
	"START",
	"STOP",
	"PIXEL00",
	"PIXEL01",
	"PIXEL02",
	"PIXEL03",
	"PIXEL04",
	"PIXEL05",
	"PIXEL06",
	"PIXEL07",
	"PIXEL08",
	"PIXEL09",
	"PIXEL10",
	"PIXEL11",
	"PIXEL12",
	"PIXEL13",
	"PIXEL14",
	"PIXEL15",
	"PIXEL16",
	"PIXEL17",
	"PIXEL18",
	"PIXEL19",
	"PIXEL20",
	"PIXEL21",
	"PIXEL22",
	"PIXEL23",
	"PIXEL24",
	"PIXEL25",
	"PIXEL26",
	"PIXEL27",
	"PIXEL28",
	"PIXEL29",
	"PIXEL30",
	"PIXEL31"
};

static char *tform[NFIELD] = {
	"1D",					/* START */
	"1D",					/* STOP */
	"1D", "1D", "1D", "1D", "1D", "1D", "1D", "1D",
	"1D", "1D", "1D", "1D", "1D", "1D", "1D", "1D",
	"1D", "1D", "1D", "1D", "1D", "1D", "1D", "1D",
	"1D", "1D", "1D", "1D", "1D", "1D", "1D", "1D"
};

static char *tunit[NFIELD] = {
	"s",					/* START */
	"s",					/* STOP */
	"", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", "",
	"", "", "", "", "", "", "", ""
};

static char *comments[NFIELD] = {
	"gain parameters valid start time (s)",	/* START */
	"gain parameters valid stop time (s)", /* STOP */
	"gain parameters for PIXEL00",
	"gain parameters for PIXEL01",
	"gain parameters for PIXEL02",
	"gain parameters for PIXEL03",
	"gain parameters for PIXEL04",
	"gain parameters for PIXEL05",
	"gain parameters for PIXEL06",
	"gain parameters for PIXEL07",
	"gain parameters for PIXEL08",
	"gain parameters for PIXEL09",
	"gain parameters for PIXEL10",
	"gain parameters for PIXEL11",
	"gain parameters for PIXEL12",
	"gain parameters for PIXEL13",
	"gain parameters for PIXEL14",
	"gain parameters for PIXEL15",
	"gain parameters for PIXEL16",
	"gain parameters for PIXEL17",
	"gain parameters for PIXEL18",
	"gain parameters for PIXEL19",
	"gain parameters for PIXEL20",
	"gain parameters for PIXEL21",
	"gain parameters for PIXEL22",
	"gain parameters for PIXEL23",
	"gain parameters for PIXEL24",
	"gain parameters for PIXEL25",
	"gain parameters for PIXEL26",
	"gain parameters for PIXEL27",
	"gain parameters for PIXEL28",
	"gain parameters for PIXEL29",
	"gain parameters for PIXEL30",
	"gain parameters for PIXEL31"
};

static int
create_drift_file(void)
{
	static int tbltype = BINARY_TBL;
	static int nf = NFIELD;
#undef NFIELD
	fitsfile *ofp;

	int istat = 0;

	if ( com.clobber ) {
		unlink(com.driftfile);
	}

	fits_create_file(&ofp, com.driftfile, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_create_file('%s') failed (%d)\n", pname, com.driftfile, istat);
		return istat;
	}

	fits_create_tbl(ofp, tbltype, 0, nf, ttype, tform, tunit, extname, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_create_tbl() failed (%d)\n", pname, istat);
		return istat;
	}

	istat = aefits_modify_table_comments(ofp, nf, comments);
	if ( istat ) {
		return istat;
	}

/* write & copy standard header keywords */
	istat = xrsWriteStdKeys(ofp, &stdkeys);
	if ( istat ) {
		fprintf(stderr, "\
%s: ERROR in xrsWriteStdKeys() (status=%d)\n", pname, istat);
		return istat;
	}

	stdkeys.keyname.datamode = NULL;
	stdkeys.keyname.hduclas1 = NULL;
	stdkeys.keyname.hduclas2 = NULL;
	stdkeys.keyname.creator  = NULL;
	stdkeys.keyname.mtype1   = NULL;
	stdkeys.keyname.mform1   = NULL;
	stdkeys.keyname.mtype2   = NULL;
	stdkeys.keyname.mform2   = NULL;
	stdkeys.keyname.mtype3   = NULL;
	stdkeys.keyname.mform3   = NULL;
	istat = xrsCopyStdKeys(com.ifp, ofp, &stdkeys);
	if ( istat ) {
		fprintf(stderr, "\
%s: ERROR in xrsCopyStdKeys() (status=%d)\n", pname, istat);
		return istat;
	}

	com.ofp = ofp;

	return 0;
}

void
XRSdriftMake_startup(int *status)
{
	com.gain_trim = 1.0;
	com.clobber = 1;

	com.use_Hp = 1;
	com.use_Mp = 1;
	com.use_Ms = 0;
	com.use_Lp = 0;
	com.use_Ls = 0;

	com.rand_seed = 7; /* N.O. */
	com.rand_skip = 0.0; /* N.O. */

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "DRIFTFILE", com.driftfile);
	printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES" : "NO");
	printf("%20s   '%s'\n", "GAINFILE", com.gainfile);
	printf("%20s   %.6f\n", "GAIN_TRIM", com.gain_trim);
	printf("%20s   %s\n", "USE_Hp", com.use_Hp ? "YES" : "NO");
	printf("%20s   %s\n", "USE_Mp", com.use_Mp ? "YES" : "NO");
	printf("%20s   %s\n", "USE_Ms", com.use_Ms ? "YES" : "NO");
	printf("%20s   %s\n", "USE_Lp", com.use_Lp ? "YES" : "NO");
	printf("%20s   %s\n", "USE_Ls", com.use_Ls ? "YES" : "NO");
	printf("%20s   '%s'\n", "LEAPFILE", com.leapfile);
	printf("%20s   %02d\n", "REFERENCE_PIXEL", cal.pixref);	/* N.O.*/
	printf("%20s   %d\n", "PI_MIN", cal.pimin);	/* N.O.*/
	printf("%20s   %d\n", "PI_MAX", cal.pimax);	/* N.O.*/
	printf("%20s   %.6f\n", "PI_MEAN", cal.pimean);	/* N.O.*/
	printf("%20s   %d\n", "NUMPI_MAX", cal.numpimax);	/* N.O.*/
	printf("%20s   %d\n", "RANDOM_SEED", com.rand_seed);	/* N.O.*/
	printf("%20s   %.0f\n", "SKIP_RANDOM", com.rand_skip);	/* N.O.*/
}

void
XRSdriftMake_com(int *status)
{
#define NVAL	19
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"DRIFTFILE",
		"CLOBBER",
		"GAINFILE",
		"GAIN_TRIM",
		"USE_Hp",
		"USE_Mp",
		"USE_Ms",
		"USE_Lp",
		"USE_Ls",
		"LEAPFILE",
		"REFERENCE_PIXEL", /* N.O. */
		"PI_MIN", /* N.O. */
		"PI_MAX", /* N.O. */
		"PI_MEAN", /* N.O. */
		"NUMPI_MAX", /* N.O. */
		"RANDOM_SEED", /* N.O. */
		"SKIP_RANDOM", /* N.O. */
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"output gain drift file",
		"overwrite output file if exists",
		"gain file name",
		"gain trimming factor",
		"use Hp events",
		"use Mp events",
		"use Ms events",
		"use Lp events",
		"use Ls events",
		"leap seconds table file name",
		"reference pixel to calc mean pi", /* N.O. */
		"minimum pi to calc mean pi", /* N.O. */
		"maximum pi to calc mean pi", /* N.O. */
		"mean pi of mnka", /* N.O. */
		"maximum numpi to calc mean pi", /* N.O. */
		"random number seed", /* N.O. */
		"random number skip count", /* N.O. */
		"exit from this menu"
	};

	int nreply = 1;
	int answer[2];

	if ( *status ) {	/* ftools */

		if (
PILGetFname("driftfile", com.driftfile) ||
PILGetBool("clobber", &com.clobber) ||
PILGetFname("gainfile", com.gainfile) ||
PILGetReal("gain_trim", &com.gain_trim) ||
PILGetBool("use_Hp", &com.use_Hp) ||
PILGetBool("use_Mp", &com.use_Mp) ||
PILGetBool("use_Ms", &com.use_Ms) ||
PILGetBool("use_Lp", &com.use_Lp) ||
PILGetBool("use_Ls", &com.use_Ls) ||
PILGetFname("leapfile", com.leapfile) ||
PILGetInt("pixref", &cal.pixref) || /* N.O. */
PILGetInt("pimin", &cal.pimin) || /* N.O. */
PILGetInt("pimax", &cal.pimax) || /* N.O. */
PILGetReal("pimean", &cal.pimean) || /* N.O. */
PILGetInt("numpimax", &cal.numpimax) || /* N.O. */
PILGetInt("rand_seed", &com.rand_seed) || /* N.O. */
PILGetReal("rand_skip", &com.rand_skip) || /* N.O. */
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
		/* kcg fooling around here */
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("DRIFTFILE", p) ) {
			CLtxtrd(p, com.driftfile, sizeof(com.driftfile));
		} else if ( 0 == strcmp("CLOBBER", p) ) {
			CLlogrd(p, &com.clobber);
		} else if ( 0 == strcmp("GAINFILE", p) ) {
			CLtxtrd(p, com.gainfile, sizeof(com.gainfile));
		} else if ( 0 == strcmp("GAIN_TRIM", p) ) {
			CLfdprd(p, &com.gain_trim);
		} else if ( 0 == strcmp("USE_Hp", p) ) {
			CLlogrd(p, &com.use_Hp);
		} else if ( 0 == strcmp("USE_Mp", p) ) {
			CLlogrd(p, &com.use_Mp);
		} else if ( 0 == strcmp("USE_Ms", p) ) {
			CLlogrd(p, &com.use_Ms);
		} else if ( 0 == strcmp("USE_Lp", p) ) {
			CLlogrd(p, &com.use_Lp);
		} else if ( 0 == strcmp("USE_Ls", p) ) {
			CLlogrd(p, &com.use_Ls);
		} else if ( 0 == strcmp("LEAPFILE", p) ) {
			CLtxtrd(p, com.driftfile, sizeof(com.leapfile));
		} else if ( 0 == strcmp("REFERENCE_PIXEL", p) ) {  /* N.O. */
			CLintrd(p, &cal.pixref);
		} else if ( 0 == strcmp("PI_MIN", p) ) {  /* N.O. */
			CLintrd(p, &cal.pimin);
		} else if ( 0 == strcmp("PI_MAX", p) ) {  /* N.O. */
			CLintrd(p, &cal.pimax);
		} else if ( 0 == strcmp("PI_MEAN", p) ) {  /* N.O. */
			CLfdprd(p, &cal.pimean);
		} else if ( 0 == strcmp("NUMPI_MAX", p) ) {  /* N.O. */
			CLintrd(p, &cal.numpimax);
		} else if ( 0 == strcmp("RANDOM_SEED", p) ) { /* N.O. */
			CLintrd(p, &com.rand_seed);
		} else if ( 0 == strcmp("SKIP_RANDOM", p) ) { /* N.O. */
			CLfdprd(p, &com.rand_skip);
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
XRSdriftMake_init(int *status)
{
	static char creator[80];	/* must be statically declared */

	int used;

	int istat = 0;

	show_parameter();

	BnkGet("XRS:EVENT:IFP", sizeof(com.ifp), &used, &com.ifp);
	BnkGet("XRS:EVENT:IFILE_NAME:PTR", sizeof(com.infile), &used, &com.infile);

	if ( *com.leapfile && NULL == mission_time_init(NULL) ) {
		fflush(NULL); printf("\n"); fflush(NULL);
		mission_time_init(com.leapfile);
	}
	fflush(NULL); printf("\n"); fflush(NULL);

	com.gs = xrs_gain_file_read(com.gainfile, NULL);
	if ( NULL == com.gs ) {
		*status = ANL_QUIT;
		return;
	}

	xrsSetDefaultKeywordValues(&stdkeys);
	stdkeys.hduclas1 = "TEMPORALDATA";
	stdkeys.hduclas2 = "HKP";
	sprintf(creator, "ANL %s %s", pname, XRSdriftMake_version);
	stdkeys.creator = creator;

	istat = create_drift_file();
	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}

/* N.O. */
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

	com.irow = 0;
	com.tstart = com.tstop = 0.0;
	com.t0 = com.t1 = 0.0;
	com.numpi = 0;
	com.sumpi = 0.0;
	com.cur_gain = 1.0 * com.gain_trim;

	*status = ANL_OK;
}

void
XRSdriftMake_his(int *status)
{
	*status = ANL_OK;
}

void
XRSdriftMake_bgnrun(int *status)
{
	*status = ANL_OK;
}

static int
write_gains(void)
{
	int i;

	int istat = 0;

	com.irow++;

fits_write_col_dbl(com.ofp, 1, com.irow, 1, 1, &com.t0, &istat);
fits_write_col_dbl(com.ofp, 2, com.irow, 1, 1, &com.t1, &istat);

	for (i = 0; i < 32; i++) {
fits_write_col_dbl(com.ofp, 3+i, com.irow, 1, 1, &com.cur_gain, &istat);
	}

	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_col_dbl() failed (%d)\n", pname, istat);
		return -1;
	}

	return 0;
}

void
XRSdriftMake_ana(int *nevent, int *eventid, int *status)
{
	int ip, used, pha, pixel;
	char type[2];
	double aetime;
	double ph, upi, epi;
	XRS_GAIN_PARAMS *g;

	BnkfGetM("XRS:TIME", sizeof(aetime), &used, &aetime);
	BnkfGetM("XRS:PHA", sizeof(pha), &used, &pha);
	BnkfGetM("XRS:PIXEL", sizeof(pixel), &used, &pixel);
	BnkfGetM("XRS:TYPE", sizeof(type), &used, type);

/* update tstart & tstop */

	if ( 0.0 == com.tstart || aetime < com.tstart ) {
		com.tstart = aetime;
	}
	if ( 0.0 == com.tstop || com.tstop < aetime ) {
		com.tstop = aetime;
	}

	if ( pixel != cal.pixref ) {
		*status = ANL_DISCARD;
		return;
	}

	if ( ( 0 == com.use_Hp && 'H' == type[0] && 'p' == type[1] ) ||
		 ( 0 == com.use_Mp && 'M' == type[0] && 'p' == type[1] ) ||
		 ( 0 == com.use_Ms && 'M' == type[0] && 's' == type[1] ) ||
		 ( 0 == com.use_Lp && 'L' == type[0] && 'p' == type[1] ) ||
		 ( 0 == com.use_Ls && 'L' == type[0] && 's' == type[1] ) ) {
		*status = ANL_DISCARD;
		return;
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

	if ( 0 != g->method ) {
		fprintf(stderr, "\
%s: unknown method=%d for PIXEL=%d, TIME=%.3f\n",
			pname, g->method, pixel, aetime);
		*status = ANL_QUIT;
		return;
	}

	upi = 0.0;
	for (ip = g->np; 0 <= ip; ip--) {
		upi += upi * ph + g->p[ip];
	}
	epi = com.cur_gain * upi;

	if ( epi < cal.pimin || cal.pimax < epi ) {
		*status = ANL_DISCARD;
		return;
	}

	com.sumpi += epi;
	com.numpi++;

	if ( com.numpi == cal.numpimax ) {
		com.cur_gain = com.numpi * cal.pimean * com.cur_gain / com.sumpi;

		com.t1 = aetime;
		com.numpi = 0;
		com.sumpi = 0.0;

		if ( write_gains() ) {
			*status = ANL_QUIT;
			return;
		}

		com.t0 = aetime;
	}

	*status = ANL_OK;
}

void
XRSdriftMake_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRSdriftMake_exit(int *status)
{
	char history[80];

	int istat = 0;

/* write last gain history row */
	if ( com.t0 < com.tstop ) {
		com.t1 = com.tstop;
		if ( write_gains() ) {
			*status = ANL_QUIT;
			return;
		}
	}

/* write first gain history row */
	fits_write_col_dbl(com.ofp, 1, 1, 1, 1, &com.tstart, &istat);

#if 0
/* update tstart & tstop */
	stdkeys.tstart = com.tstart;
	stdkeys.tstop = com.tstop;
	stdkeys.ontime = 0.0;
	istat = xrsFinalizeStdKeys(com.ofp, &stdkeys, com.irow, pname);
	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}
#else

/* add DATE */
	fits_write_date(com.ofp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_date() failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

/* add CHECKSUM */
	fits_write_chksum(com.ofp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

	istat = aefits_write_module_history(com.ofp, pname);
	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}

#endif

/* write parameter values */
	sprintf(history, "  infile='%s'", com.infile);
	fits_write_history(com.ofp, history, &istat);
	sprintf(history, "  driftfile='%s'", com.driftfile);
	fits_write_history(com.ofp, history, &istat);
	sprintf(history, "  clobber=%s", com.clobber ? "yes" : "no");
	fits_write_history(com.ofp, history, &istat);
	sprintf(history, "  gainfile='%s'", com.gainfile);
	fits_write_history(com.ofp, history, &istat);
	sprintf(history, "  gain_trim=%.6f", com.gain_trim);
	fits_write_history(com.ofp, history, &istat);
	sprintf(history, "  use_Hp=%s, use_Mp=%s, use_Ms=%s, use_Lp=%s, use_Ls=%s",
		com.use_Hp ? "yes" : "no",
		com.use_Mp ? "yes" : "no", com.use_Ms ? "yes" : "no",
		com.use_Lp ? "yes" : "no", com.use_Ls ? "yes" : "no");
	fits_write_history(com.ofp, history, &istat);
	sprintf(history, "  leapfile='%s'", com.leapfile);
	fits_write_history(com.ofp, history, &istat);
	sprintf(history, "  rand_seed=%d, rand_skip=%.0f",
		com.rand_seed, com.rand_skip);
	fits_write_history(com.ofp, history, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

/* close drift file */
	fits_close_file(com.ofp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_close_file() failed (%d)\n", pname, istat);
		*status = ANL_QUIT;
		return;
	}

/* print information message */
	fflush(NULL); printf("\
%s: gain history file '%s' created\n", pname, com.driftfile);
	fflush(NULL);

	*status = ANL_OK;
}


/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
