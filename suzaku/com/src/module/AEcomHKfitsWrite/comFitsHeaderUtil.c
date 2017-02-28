/*
 comFitsHeaderUtil.c

	1999/11/18 Y.ISHISAKI	version 1.3
		arrange source code format
		delete strdup()
		rename function name (add 'AE' in head)
		TIMESYS keywords change to string of "2000-01-01T00:00:00.0"

	1999/12/27 Y.ISHISAKI
		remove OPTIC*

	2000/01/27 Y.ISHISAKI	version 1.4
		check BNK of ASTE:FFF_ORIGIN

	2004/03/14 Y.ISHISAKI	version 1.5
		use AtTimeD (since atFunctions-2.2) instead of AtTime
		change "RA_NOM", "DEC_NOM" -> "RA_PNT", "DEC_PNT"
		remove "DATAMODE" keyword for HK files
		passing pointer of COM_STD_KEYS for
			AEwriteCOMStdKeys(), AEupdateStdTimeKeys()
		add hduclas1, hduclas2 in COM_STD_KEYS
		write HDUCLASS only when HDUCLAS1 != NULL
		change TIME-SYSTEM keywords

MJDREF  =              51544.0 / MJD corresponding to SC clock start (2000.0)
TIMESYS = '2000-01-01T00:00:00.0' / Time is measured from 2000 Jan 1 00:00 UT
TIMEUNIT= 's       '           / unit for time related keywords
	|
	V
TIMESYS = 'TT      '           / Time system
MJDREFI =              51544.0 / int MJD at SC clock start (2000.0 UT)
MJDREFF = 7.42858796300000E-04 / fractional part of MJDREF
TIMEUNIT= 's       '           / unit for time related keywords

	2004/09/23 Y.ISHISAKI	version 1.7
		use aste_telescop() to get TELESCOP keyword
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_coord.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "fitsio.h"

#define _COM_FITS_HEADER_UTIL_FUNC_
#include "comFitsHeaderUtil.h"

static char pname[] = "comFitsHeaderUtil";

/* STD HDU header keyword */
void
AEsetDefaultKeywordValues(COM_STD_KEYS *stdkeys)
{
	static char fff_origin[256];
	int istat, used;

	stdkeys->telescope = aste_telescop();
	stdkeys->instrument = "COM";
	stdkeys->obs_mode = "DEFAULT";
	stdkeys->object = "DEFAULT";
	stdkeys->observer = "DEFAULT";
	stdkeys->date_obs = "DEFAULT";
	stdkeys->time_obs = "DEFAULT";
	stdkeys->date_end = "DEFAULT";
	stdkeys->time_end = "DEFAULT";
	stdkeys->tstart = 0.0;
	stdkeys->tstop = 0.0;
	stdkeys->ontime = 0.0;
	stdkeys->telapse = 0.0;
	stdkeys->radecsys = "FK5";
	stdkeys->equinox = 2000;
	stdkeys->ra_pnt = 0.0;
	stdkeys->dec_pnt = 0.0;
	stdkeys->timesys = "TT";
	stdkeys->mjdrefi = 51544;
	stdkeys->mjdreff = 7.42858796300000E-04;
	stdkeys->timeref = "LOCAL";
	stdkeys->timeunit = "s";
	stdkeys->tassign = "SATELLITE";
	stdkeys->clockapp = 1;
	stdkeys->timedel = 0.0;
	stdkeys->timepixr = 0.0;
	stdkeys->tierrela = 0.0;
	stdkeys->tierabso = 0.0;
	stdkeys->tlmfile = "DEFAULT";
	stdkeys->hduclass = "OGIP";

	istat = BnkGet("ASTE:FFF_ORIGIN", sizeof(fff_origin)-1, &used, fff_origin);
	if ( istat || 0 == used ) {
		stdkeys->origin = "JPN-ISAS";	/* default */
	} else {
		fff_origin[used] = '\0';
		stdkeys->origin = fff_origin;
    }
}

/* write common keywords:
 * these are common keywords not only for for primary HDU but also for other extentions
 */
int
AEwriteCOMStdKeys(fitsfile * fp, COM_STD_KEYS *keys, int hdunum, int *status)
{
	int hdutype;

	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: NULL pointer in writeCOMStdKeys\n", pname);
		return ANL_FALSE;
	}

	/* move to the specified xtention unless hdunum=0 */
	if ( 0 != hdunum ) {
		if ( fits_movabs_hdu(fp, hdunum, &hdutype, status) ) {
			fprintf(stderr, "\
%s: fits_movabs_hdu failed (status=%d)\n", pname, *status);
			return ANL_FALSE;
		}
	}

	if (fits_write_key_str(fp, "TELESCOP", keys->telescope,
						   "Telescope (mission) name", status)) {
		fprintf(stderr,"\
Error in writing keyword: TELESCOP (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "INSTRUME", keys->instrument,
						   "instrument name", status)) {
		fprintf(stderr,"\
Error in writing keyword: INSTRUME (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "OBS_MODE", keys->obs_mode,
						   "Observation mode", status)) {
		fprintf(stderr,"\
Error in writing keyword: OBS_MODE (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "OBJECT",   keys->object,
						   "Name of observed object", status)) {
		fprintf(stderr,"\
Error in writing keyword: OBJECT (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "OBSERVER", keys->observer,
						   "Principal Investigator", status)){
		fprintf(stderr,"\
Error in writing keyword: OBSERVER (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "DATE-OBS", keys->date_obs,
						   "Start date of observations", status)){
		fprintf(stderr,"\
Error in writing keyword: DATE-OBS (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "TIME-OBS", keys->time_obs,
						   "Start time of observations", status)){
		fprintf(stderr,"\
Error in writing keyword: TIME-OBS (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "DATE-END", keys->date_end,
						   "End date of observations", status)) {
		fprintf(stderr,"\
Error in writing keyword: DATE-END (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "TIME-END", keys->time_end,
						   "End time of observations", status)){
		fprintf(stderr,"\
Error in writing keyword: TIME-END (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_fixdbl(fp, "TSTART",   keys->tstart, 8,
						   "time start", status)){
		fprintf(stderr,"\
Error in writing keyword: TSTART (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_fixdbl(fp, "TSTOP",    keys->tstart, 8,
						   "time stop", status)){
		fprintf(stderr,"\
Error in writing keyword: TSTOP (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_fixdbl(fp, "TELAPSE",   keys->telapse, 8,
						   "elapsed time: TSTOP-TSTART", status)){
		fprintf(stderr,"\
Error in writing keyword: ONTIME (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_fixdbl(fp, "ONTIME",   keys->tstart, 8,
						   "ontime: sum of all GTIs", status)){
		fprintf(stderr,"\
Error in writing keyword: ONTIME (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "RADECSYS", keys->radecsys,
						   "World Coordinate System", status)){
		fprintf(stderr,"\
Error in writing keyword: RADECSY (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_lng(fp, "EQUINOX",  keys->equinox,
						   "Equinox for coordinate system", status)) {
		fprintf(stderr,"\
Error in writing keyword: EQUINOX (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_fixflt(fp, "RA_PNT",   keys->ra_pnt, 5,
						   "Nominal Pointing R.A.", status)) {
		fprintf(stderr,"\
Error in writing keyword: RA_PNT (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_fixflt(fp, "DEC_PNT",  keys->dec_pnt, 5,
						   "Nominal Pointing declination", status)){
		fprintf(stderr,"\
Error in writing keyword: DEC_PNT (status=%d)\n", *status);
		return ANL_FALSE;
	}

	if (fits_write_key_str(fp, "TIMESYS", keys->timesys,
						   "Time system", status)) {
		fprintf(stderr, "\
%s: Error in writing keyword: TIMESYS (status=%d)\n", pname, *status);
		return ANL_FALSE;
	}

	if (fits_write_key_fixdbl(fp, "MJDREFI", keys->mjdrefi, 1,
					"int MJD at SC clock start (2000.0 UT)", status)) {
		fprintf(stderr, "\
%s: Error in writing keyword: MJDREFI (status=%d)\n", pname, *status);
		return ANL_FALSE;
	}

	if (fits_write_key_dbl(fp, "MJDREFF", keys->mjdreff, 14,
					"fractional part of MJDREF", status)) {
		fprintf(stderr, "\
%s: Error in writing keyword: MJDREFF (status=%d)\n", pname, *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "TIMEREF",  keys->timeref,
			"Barycentric correction not applied to times", status)){
		fprintf(stderr,"\
Error in writing keyword: TIMEREF (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "TIMEUNIT", keys->timeunit,
						   "unit for time related keywords", status)){
		fprintf(stderr,"\
Error in writing keyword: TIMEUNIT (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "TASSIGN",  keys->tassign,
						   "TASSIGN", status)) {
		fprintf(stderr,"\
Error in writing keyword: TASSIGN (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_log(fp, "CLOCKAPP", keys->clockapp,
						   "CLOCKAPP", status)){
		fprintf(stderr,"\
Error in writing keyword: OBJECT (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_flt(fp, "TIMEDEL",  keys->timedel, 5,
				   "finest time resolution (time between frames)", status)){
		fprintf(stderr,"\
Error in writing keyword: TIMEDEL (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_flt(fp, "TIMEPIXR", keys->timepixr, 1,
						   "times refer to the beginning of the bin", status)){
		fprintf(stderr,"\
Error in writing keyword: TIMEPIXR (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_flt(fp, "TIERRELA", keys->tierrela, 1,
						   "short-term clock stability", status)){
		fprintf(stderr,"\
Error in writing keyword: TIERRELA (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_flt(fp, "TIERABSO", keys->tierabso, 1,
						   "absolute precision of the clock", status)){
		fprintf(stderr,"\
Error in writing keyword: TIERABSO (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "CREATOR",  keys->creator,
						   "by COM hardware/software team",status)){
		fprintf(stderr,"\
Error in writing keyword: CREATOR (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if (fits_write_key_str(fp, "ORIGIN",   keys->origin,
						   "origin of fits file", status)){
		fprintf(stderr,"\
Error in writing keyword: ORIGIN (status=%d)\n", *status);
		return ANL_FALSE;
	}
	if ( NULL != keys->hduclas1 && '\0' != keys->hduclas1 ) {
		if (fits_write_key_str(fp, "HDUCLASS", keys->hduclass,
					"format conforms to OGIP/GSFC conventions", status)) {
			fprintf(stderr, "\
%s: Error in writing keyword: HDUCLASS (status=%d)\n", pname, *status);
			return ANL_FALSE;
		}
		if (fits_write_key_str(fp, "HDUCLAS1", keys->hduclas1,
					"extension containing", status)) {
			fprintf(stderr, "\
%s: Error in writing keyword: HDUCLAS1 (status=%d)\n", pname, *status);
			return ANL_FALSE;
		}
		if (fits_write_key_str(fp, "HDUCLAS2", keys->hduclas2,
					"extension containing", status)) {
			fprintf(stderr, "\
%s: Error in writing keyword: HDUCLAS2 (status=%d)\n", pname, *status);
			return ANL_FALSE;
		}
	}	
	if (fits_write_key_str(fp, "TLM_FILE",   keys->tlmfile,
						   "Name of input telemetry file", status)){
		fprintf(stderr,"\
Error in writing keyword: TLM_FILE (status=%d)\n", *status);
		return ANL_FALSE;
	}

	return ANL_TRUE;
}

/* update time keywords */
int
AEupdateStdTimeKeys(fitsfile *fp, COM_STD_KEYS *keys, int hdunum, int *status)
{
	int hdutype;
	char date_obs[11], date_end[11], time_obs[9], time_end[9];
	AtTimeD attime;

	/* calculate date and time */
	aste2attimeD(keys->tstart, &attime);
	sprintf(time_obs,"%02d:%02d:%02d", attime.hr, attime.mn, attime.sc);
	sprintf(date_obs,"%04d-%02d-%02d", attime.yr, attime.mo, attime.dy);

	aste2attimeD(keys->tstop, &attime);
	sprintf(time_end,"%02d:%02d:%02d", attime.hr, attime.mn, attime.sc);
	sprintf(date_end,"%04d-%02d-%02d", attime.yr, attime.mo, attime.dy);

	keys->telapse = keys->tstop - keys->tstart;

/* if ontime if not caululated, set it to telapse;
 * this is true if there is only one GTI;
 * if more than one GTIs, it should be updated at the end*/
	if ( 0.0 == keys->ontime ) {
		keys->ontime = keys->telapse;
	}

	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: NULL pointer in updateStdTimeKeys \n",pname);
		return ANL_FALSE;
	}

/* move to the specified xtention unless hdunum=0 */
	if ( 0 != hdunum ) {
		if ( fits_movabs_hdu(fp, hdunum, &hdutype, status) ) {
			fprintf(stderr, "\
%s: fits_movabs_hdu failed (status=%d)\n",pname, *status);
			return ANL_FALSE;
		}
	}

	if (fits_update_key_str(fp, "DATE-OBS", date_obs, "&", status)) {
		fprintf(stderr, "\
%s: fits_update_key_str DATE-OBS failed (%d)\n", pname, *status);
		return ANL_FALSE;
	}

	if (fits_update_key_str(fp, "TIME-OBS", time_obs, "&", status)) {
		fprintf(stderr, "\
%s: fits_update_key_str TIME-OBS failed (%d)\n", pname, *status);
		return ANL_FALSE;
	}

	if (fits_update_key_str(fp, "DATE-END", date_end, "&", status)) {
		fprintf(stderr, "\
%s: fits_update_key_str DATE-END failed (%d)\n", pname, *status);
		return ANL_FALSE;
	}

	if (fits_update_key_str(fp, "TIME-END", time_end, "&", status)) {
		fprintf(stderr, "\
%s: fits_update_key_str TIME-END failed (%d)\n", pname, *status);
		return ANL_FALSE;
	}

	if (fits_update_key_fixdbl(fp, "TSTART", keys->tstart, 8, "&", status)) {
		fprintf(stderr, "\
%s: fits_update_key_fixdbl TSTART failed (%d)\n", pname, *status);
		return ANL_FALSE;
	}

	if (fits_update_key_fixdbl(fp, "TSTOP", keys->tstop, 8, "&", status)) {
		fprintf(stderr, "\
%s: fits_update_key_fixdbl TSTOP failed (%d)\n", pname, *status);
		return ANL_FALSE;
	}

	if (fits_update_key_fixdbl(fp, "TELAPSE", keys->telapse, 8,"&",status)) {
		fprintf(stderr, "\
%s: fits_update_key_fixdbl TELAPSE failed (%d)\n", pname, *status);
		return ANL_FALSE;
	}

	if (fits_update_key_fixdbl(fp, "ONTIME", keys->ontime, 8, "&", status)) {
		fprintf(stderr, "\
%s: fits_update_key_fixdbl ONTIME failed (%d)\n", pname, *status);
		return ANL_FALSE;
	}

	return ANL_TRUE;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
