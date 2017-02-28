/*
 SimASTE_Util.c
   SimASTE utility functions

  2006-08-05 version 2.2	Y.ISHISAKI
	SimASTE_write_history_pname(), SimASTE_write_timestamp()
	SimASTE_write_photon_detect_info(), SimASTE_write_random_number_info()
	SimASTE_update_time_keys(), SimASTE_write_std_keys(), SimASTE_write_gti()

  2007-05-28 version 2.4	Y.ISHISAKI
	change unit "sec" -> "s" in SimASTE_write_gti()

  2008-04-05 version 2.5	Y.ISHISAKI
	write RADECSYS, EQUINOX keywords in SimASTE_write_std_keys()
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include "fitsio.h"
#include "anl.h"
#include "cli.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_rand.h"
#include "aste_time.h"
#include "aste_gti.h"
#include "aeFitsHeaderUtil.h"
#include "SimASTE.h"

static char pname[] = "SimASTE_Util";
char SimASTE_Util_version[] = "version 2.5";

int
SimASTE_write_history_pname(fitsfile *fp, char *pname)
{
	char history[FLEN_VALUE];
	int istat = 0;

	sprintf(history, "[%s]", pname);
	fits_write_history(fp, history, &istat);

	return istat;
}

int
SimASTE_write_timestamp(fitsfile *fp)
{
    char history[FLEN_COMMENT];
    char *task_name, *task_version;
    int timeref;
    char date_str[FLEN_VALUE];
    int istat = 0;

/* get the task name */
    task_name = anl_task_name();
	task_version = anl_task_version();

/* get the current date */
	fits_get_system_time(date_str, &timeref, &istat);

/* construct history string */
	*history = '\0';
	strncat(history, "*** ", sizeof(history)-strlen(history)-1);
    strncat(history, task_name, sizeof(history)-strlen(history)-1);
    strncat(history, " version ", sizeof(history)-strlen(history)-1);
    strncat(history, task_version, sizeof(history)-strlen(history)-1);
    strncat(history, " finished at ", sizeof(history)-strlen(history)-1);
    strncat(history, date_str, sizeof(history)-strlen(history)-1);
	strncat(history, " ***", sizeof(history)-strlen(history)-1);

/*write out the history */
	fits_write_history(fp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
    }

    return 0;
}

int
SimASTE_write_photon_detect_info(fitsfile *fp)
{
	char *k;
	int used;
	double n_photon, n_detect, n_weisum;
	int istat = 0;

	BnkGet("SimASTE:N_PHOTON", sizeof(n_photon), &used, &n_photon);
	BnkGet("SimASTE:N_DETECT", sizeof(n_detect), &used, &n_detect);
	BnkGet("SimASTE:N_WEISUM", sizeof(n_weisum), &used, &n_weisum);

	if ( fits_write_key_fixdbl(fp, k="N_PHOTON", n_photon, 0, "\
number of input photons generated", &istat) ||
		 fits_write_key_fixdbl(fp, k="N_DETECT", n_detect, 0, "\
number of events detected", &istat) ||
		 fits_write_key_fixdbl(fp, k="N_WEISUM", n_weisum, 9, "\
weighted sum of the detected events", &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	return istat;
}

int
SimASTE_write_random_number_info(fitsfile *fp)
{
	int rand_seed, used;
	double rand_skip, rand_ngen;
	char *k;
    int istat = 0;

	BnkGet("SimASTE:RANDOM_SEED", sizeof(rand_seed), &used, &rand_seed);
	BnkGet("SimASTE:RANDOM_SKIP", sizeof(rand_skip), &used, &rand_skip);
	rand_ngen = aste_drndtsn_gen();

	if ( fits_write_key_lng(fp, k="RANDSEED", rand_seed, "\
random number seed", &istat) ||
		 fits_write_key_fixdbl(fp, k="RANDSKIP", rand_skip, 0, "\
random number skip", &istat) ||
		 fits_write_key_fixdbl(fp, k="RANDNGEN", rand_ngen, 0, "\
random number generated in this task", &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	return istat;
}

/* update the data/time keywords acording to tstart and tstop */
int
SimASTE_update_time_keys(
	fitsfile *fp, double tstart, double tstop, double expo)
{
	char *k, date_obs[FLEN_VALUE], date_end[FLEN_VALUE];
	int istat = 0;

	aefits_aetime2datestr(tstart, date_obs);
	aefits_aetime2datestr(tstop, date_end);

	if (
fits_update_key_fixdbl(fp, k="TSTART", tstart, 15, NULL, &istat) ||
fits_update_key_fixdbl(fp, "TSTOP", tstop, 15, NULL, &istat) ||
fits_update_key_fixdbl(fp, "ONTIME", expo, 15, NULL, &istat) ||
fits_update_key_str(fp, "DATE-OBS", date_obs, NULL, &istat) ||
fits_update_key_str(fp, "DATE-END", date_end, NULL, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	return istat;
}

int
SimASTE_write_std_keys(fitsfile *fp, AE_STD_KEYS *v)
{
	char *k, *username;
	char creator[FLEN_VALUE], observer[FLEN_VALUE];
	struct AE_STD_KEYS_NAME    *n = &v->keyname;
	struct AE_STD_KEYS_COMMENT *c = &v->comment;

	int istat = 0;

	username = getlogin();
	if ( NULL == username ) {
		username = getenv("LOGNAME");
		if ( NULL == username ) {
			username = getenv("USER");
			if ( NULL == username ) {
				username = "";
			}
		}
	}
	observer[sizeof(observer)-1] = '\0';
	strncpy(observer, username, sizeof(observer)-1);
	CLstrupc(observer);
	sprintf(creator, "%s version %s", anl_task_name(), anl_task_version());

	if (
fits_write_key_str   (fp, k="OBJECT", v->object,
				   "name of the observed object", &istat) ||
fits_write_key_str   (fp, k="OBSERVER", observer,
				   "login name who ran this simulation", &istat) ||
fits_write_key_str   (fp, k="CREATOR", creator,
				   "software that created this file", &istat) ||
fits_write_key_str   (fp, k="RADECSYS", v->radecsys, c->radecsys, &istat) ||
fits_write_key_lng   (fp, k="EQUINOX",  v->equinox,  c->equinox,  &istat) ||
fits_write_key_null  (fp, k="DATE-OBS",
					"start date of the observation (UTC)", &istat) ||
fits_write_key_null  (fp, k="DATE-END",
					"end date of the observation (UTC)", &istat) ||
fits_write_key_str   (fp, k=n->telescop, v->telescop,   c->telescop, &istat) ||
fits_write_key_str   (fp, k=n->instrume, v->instrume,   c->instrume, &istat) ||
				0 ) {
		goto error;
	} else if ( NULL != v->filter && (
fits_write_key_str   (fp, k=n->filter,   v->filter,     c->filter,   &istat) ||
									   0 ) ) {
		goto error;
	} else if (
fits_write_key_str   (fp, k=n->obs_mode, v->obs_mode,   c->obs_mode, &istat) ||
fits_write_key_str   (fp, k=n->datamode, v->datamode,   c->datamode, &istat) ||
fits_write_key_fixdbl(fp, k=n->tstart,   v->tstart, 6,  c->tstart,   &istat) ||
fits_write_key_fixdbl(fp, k=n->tstop,    v->tstop,  6,  c->tstop,    &istat) ||
fits_write_key_fixdbl(fp, k=n->ontime,   v->ontime, 6,  c->ontime,   &istat) ||
fits_write_key_str   (fp, k=n->timesys,  v->timesys,    c->timesys,  &istat) ||
fits_write_key_lng   (fp, k=n->mjdrefi,  v->mjdrefi,    c->mjdrefi,  &istat) ||
fits_write_key_fixdbl(fp, k=n->mjdreff,  v->mjdreff, 17,c->mjdreff,  &istat) ||
fits_write_key_str   (fp, k=n->timeref,  v->timeref,    c->timeref,  &istat) ||
fits_write_key_str   (fp, k=n->timeunit, v->timeunit,   c->timeunit, &istat) ||
fits_write_key_str   (fp, k=n->tassign,  v->tassign,    c->tassign,  &istat) ||
fits_write_key_log   (fp, k=n->clockapp, v->clockapp,   c->clockapp, &istat) ||
fits_write_key_dbl   (fp, k=n->timedel,  v->timedel, 5, c->timedel,  &istat) ||
		 0 ) {
		goto error;
	}

	return istat;

 error:
	anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
	return istat;
}

int
SimASTE_write_gti(fitsfile *fp, GTI_DATA *gp, AE_STD_KEYS *v)
{
	static char *extname = "GTI";  /* simulation gti */
	static char *ttype[] ={ "START", "STOP" };
	static char *tform[]= { "1D", "1D" };
	static char *tunit[] = {"s", "s"};
	static int tblt = BINARY_TBL;
	static int ncol = 2;

	int icol;
	int ngti = gp->ngti;
	double *start = gp->start;
	double *stop = gp->stop;
	double expo = gp->ontime;
	int istat = 0;

/* create a GTI extension */
	if (
fits_create_tbl(fp, tblt, 0, ncol, ttype, tform, tunit, extname, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_create_tbl('%s') failed (%d)\n", pname, extname, istat);
		return istat;
	}

/* write header keywords */
	if (
 (istat = SimASTE_write_std_keys(fp, v)) ||
 (istat = SimASTE_update_time_keys(fp, start[0], stop[ngti-1], expo)) ||
 (istat = SimASTE_write_timestamp(fp)) ||
		 0 ) {
		return istat;
	}

/* write the GTI data */
	if (
fits_write_col_dbl(fp, icol=1, 1, 1, ngti, start, &istat) ||
fits_write_col_dbl(fp, icol=2, 1, 1, ngti, stop, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_col_dbl('%s') failed (%d)\n", pname, ttype[icol-1], istat);
		return istat;
	}

	return istat;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
