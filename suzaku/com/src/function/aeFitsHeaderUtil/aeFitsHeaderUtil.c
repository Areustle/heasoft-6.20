/*
	aeFitsHeaderUtil.c

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

	2005/04/06 Y.ISHISAKI	version 1.8
		split from AEcomHKfitsWrite-1.7
		change function names, AE..() -> ae..()
		stop BnkGet("FFF_ORIGIN");
		modify comments

    2005/05/15 Y.ISHISAKI	version 2.0
		add aefits_write_name_vers(), aefits_modify_comment(),
		aefits_modify_table_comments()

	2005/06/03 Y.ISHISAKI	version 2.1
		telescope -> telescop, instrument -> instrume
		add ra_obj, dec_obj, ra_nom, dec_nom, pa_nom, mean_ea1/2/3
		write null keywords for ra/dec_obj/nom/pnt, mean_ea1/2/3
		add aeGetFFFname(), aeGetFFForigin(), aefits_write_module_history()

	2005/06/07 Y.ISHISAKI	version 2.2
		bug fix in MJDREFF
MJDREFF = 0.00074287037037037 / fractional part of the MJD reference (64.184 s)

	2005/06/14 Y.ISHISAKI	version 2.3
		don't write RA_PNT, DEC_PNT keywords, which is not defined for COM
		variables & comments of 'ra_pnt', 'dec_pnt' are left for aeaspect
		rename tlmfile -> tlm_file, add att_file, orb_file, teldef
		add aefits_basename()

	2005/06/22 Y.ISHISAKI	version 2.4
		add tim_file
		declare aeFitsHeaderUtil_version[]
		reserve morekeys=80 for header keywords in aeWriteStdKeys()
		use aefits_basename() instead of local basename() in aeGetFFFname()
		set v->tim_file in aeGetFFFname()

	2005/06/26,07/04,07/05 Y.ISHISAKI	version 2.5
		set HDUCLAS2 comment according to HDUCLASn values
		add aefits_delta_phi(),aefits_mjd_tt2mission(),aefits_mission2mjd_tt()
		set keyname in aeSetDefaultKeywordValues()
		add aeCopyStdKeys()

	2005/07/10 Y.ISHISAKI	version 2.6
		check if v->hduclas1 == NULL for HDUCLAS2 comments in aeWriteStdKeys()

	2005/06/26 Y.ISHISAKI	version 2.7
		change the FFF naming convention '_0_com' -> 'com_0' in aeGetFFFname()

	2005/08/12 Y.ISHISAKI	version 2.8
		change the FFF naming convention 'aeCiYYMMDDn.hk' in aeGetFFFname()

	2005/10/08 Y.ISHISAKI	version 2.9
		add SEQ_NUM, OBS_REM
		ORIGIN='ISAS/JAXA' by default
		keyword order changed

	2005/10/18 Y.ISHISAKI	version 3.0
		rename SEQ_NUM -> OBS_ID, add LEAPFILE, re-order keywords
		initial string values 'DEFAULT' -> '      '
		origin = 'ISAS/JAXA' by default in aeGetFFForigin()
		set v->leapfile in aeGetFFFname()

	2005/10/23 Y.ISHISAKI	version 3.1
		initialize used = 0 in aeGetFFFname() & aeGetFFForigin()
		add name_size--; name[name_size] = '\0'; in aeGetFFFname()
		remove INSTRUME,TIMEDEL,TIMEPIXR,TIERRELA,TIERABSO in aeWriteStdKeys()

	2006/08/01 Y.ISHISAKI	version 3.2
		move aefits_mjd_tt2mission(), aefits_mission2mjd_tt() to astetool-1.80
		aefits_mjd_tt2mission(), aefits_mission2mjd_tt() remains but obsolete
		call aste_mjdrefi(), aste_mjdreff() for MJDREFI, MJDREFF
		add aefits_datestr2attimeD(), aefits_attimeD2datestr()
		add aefits_degToRAstr(), aefits_degToDECstr()

	2006/08/07 Y.ISHISAKI	version 3.3
		set FILTER keyword comment in aeSetDefaultKeywordValues()

	2006/09/17 Y.ISHISAKI	version 3.4
		add aefits_write_tool_version()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "anl.h"
#include "anl_misc.h"
#include "bnk.h"
#include "evs.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_coord.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "aeFitsHeaderUtil";
char aeFitsHeaderUtil_version[] = "3.4";

/* STD HDU header keyword */
void
aeSetDefaultKeywordValues(AE_STD_KEYS *stdkeys)
{
	struct AE_STD_KEYS_NAME    *keyname = &stdkeys->keyname;
	struct AE_STD_KEYS_COMMENT *comment = &stdkeys->comment;

	keyname->telescop = "TELESCOP";
	stdkeys->telescop = aste_telescop();
	comment->telescop = "telescope (mission) name";

	keyname->instrume = "INSTRUME";
	stdkeys->instrume = "COMMON";
	comment->instrume = "instrument name";

	keyname->obs_mode = "OBS_MODE";
	stdkeys->obs_mode = "";
	comment->obs_mode = "observation mode (e.g. POINTING/SLEW)";

	keyname->datamode = "DATAMODE";
	stdkeys->datamode = "HOUSEKEEPING";	/* does not written for HK files */
	comment->datamode = "data mode";

	keyname->obs_id   = "OBS_ID";
	stdkeys->obs_id   = "";
	comment->obs_id   = "Observation Identifier";

	keyname->filter   = "FILTER";
	stdkeys->filter   = NULL;			/* not write FILTER by default */
	comment->filter   = "filter name";

	keyname->observer = "OBSERVER";
	stdkeys->observer = "";
	comment->observer = "Principal Investigator";

	keyname->object   = "OBJECT";
	stdkeys->object   = "";
	comment->object   = "name of observed object";

	keyname->obs_rem  = "OBS_REM";
	stdkeys->obs_rem  = "";
	comment->obs_rem  = "remark on observation";

	keyname->ra_obj   = "RA_OBJ";
	stdkeys->ra_obj   = -999.0;
	comment->ra_obj   = "planned target R.A.(deg)";

	keyname->dec_obj  = "DEC_OBJ";
	stdkeys->dec_obj  = -999.0;
	comment->dec_obj  = "planned target DEC.(deg)";

	keyname->ra_pnt   = "RA_PNT";
	stdkeys->ra_pnt   = -999.0;
	comment->ra_pnt   = "average optical axis location R.A.(deg)";

	keyname->dec_pnt  = "DEC_PNT";
	stdkeys->dec_pnt  = -999.0;
	comment->dec_pnt  = "average optical axis location DEC.(deg)";

	keyname->ra_nom   = "RA_NOM";
	stdkeys->ra_nom   = -999.0;
	comment->ra_nom   = "nominal satellite pointing direction R.A.(deg)";

	keyname->dec_nom  = "DEC_NOM";
	stdkeys->dec_nom  = -999.0;
	comment->dec_nom  = "nominal satellite pointing direction DEC.(deg)";

	keyname->pa_nom   = "PA_NOM";
	stdkeys->pa_nom   = -999.0;
	comment->pa_nom   = "nominal position angle from north to DETY(deg)";

	keyname->mean_ea1 = "MEAN_EA1";
	stdkeys->mean_ea1 = -999.0;
	comment->mean_ea1 = "mean of the 1st ZYZ-Euler angle (deg)";

	keyname->mean_ea2 = "MEAN_EA2";
	stdkeys->mean_ea2 = -999.0;
	comment->mean_ea2 = "mean of the 2nd ZYZ-Euler angle (deg)";

	keyname->mean_ea3 = "MEAN_EA3";
	stdkeys->mean_ea3 = -999.0;
	comment->mean_ea3 = "mean of the 3rd ZYZ-Euler angle (deg)";

	keyname->radecsys = "RADECSYS";
	stdkeys->radecsys = "FK5";
	comment->radecsys = "World Coordinate System";

	keyname->equinox  = "EQUINOX";
	stdkeys->equinox  = 2000;
	comment->equinox  = "equinox for coordinate system";

	keyname->date_obs = "DATE-OBS";
	stdkeys->date_obs = "";
	comment->date_obs = "start date of observations (UT)";

	keyname->time_obs = "TIME-OBS";
	stdkeys->time_obs = "";
	comment->time_obs = "start time of observations (UT)";

	keyname->date_end = "DATE-END";
	stdkeys->date_end = "";
	comment->date_end = "end date of observations (UT)";

	keyname->time_end = "TIME-END";
	stdkeys->time_end = "";
	comment->time_end = "end time of observations (UT)";

	keyname->tstart   = "TSTART";
	stdkeys->tstart   = 0.0;
	comment->tstart   = "time start";

	keyname->tstop    = "TSTOP";
	stdkeys->tstop    = 0.0;
	comment->tstop    = "time stop";

	keyname->ontime   = "ONTIME";
	stdkeys->ontime   = 0.0;
	comment->ontime   = "on time = sum of all GTIs";

	keyname->telapse  = "TELAPSE";
	stdkeys->telapse  = 0.0;
	comment->telapse  = "elapsed time = TSTOP - TSTART";

	keyname->timesys  = "TIMESYS";
	stdkeys->timesys  = "TT";
	comment->timesys  = "time system (TT:Terrestrial Time)";

	keyname->mjdrefi  = "MJDREFI";
	stdkeys->mjdrefi  = aste_mjdrefi();
	comment->mjdrefi  = "integer part of the MJD reference (2000.0 UT)";

	keyname->mjdreff  = "MJDREFF";
	stdkeys->mjdreff  = aste_mjdreff();
	comment->mjdreff  = "fractional part of the MJD reference (64.184 s)";

	keyname->timeref  = "TIMEREF";
	stdkeys->timeref  = "LOCAL";
	comment->timeref  = "LOCAL: barycentric correction not applied";

	keyname->timeunit = "TIMEUNIT";
	stdkeys->timeunit = "s";
	comment->timeunit = "unit for the time related keywords";

	keyname->tassign  = "TASSIGN";
	stdkeys->tassign  = "SATELLITE";
	comment->tassign  = "SATELLITE: times assigned on satellite";

	keyname->clockapp = "CLOCKAPP";
	stdkeys->clockapp = 1;
	comment->clockapp = "clock correction applied or not";

	keyname->timedel  = "TIMEDEL";
	stdkeys->timedel  = 0.0;
	comment->timedel  = "finest time resolution (time between frames)";

	keyname->timepixr = "TIMEPIXR";
	stdkeys->timepixr = 0.0;
	comment->timepixr = "0:times refer to beginning of bin, 0.5:mid";

	keyname->tierrela = "TIERRELA";
	stdkeys->tierrela = 0.0;
	comment->tierrela = "short-term clock stability";

	keyname->tierabso = "TIERABSO";
	stdkeys->tierabso = 0.0;
	comment->tierabso = "absolute precision of the clock";

	keyname->hduclass = "HDUCLASS";
	stdkeys->hduclass = "OGIP";
	comment->hduclass = "format conforms to OGIP/GSFC conventions";

	keyname->hduclas1 = "HDUCLAS1";
	stdkeys->hduclas1 = NULL;
	comment->hduclas1 = "type of data (e.g. EVENTS/TEMPORALDATA)";

	keyname->hduclas2 = "HDUCLAS2";
	stdkeys->hduclas2 = NULL;
	comment->hduclas2 = "extension containing";

	keyname->tlm_file = "TLM_FILE";
	stdkeys->tlm_file = "";
	comment->tlm_file = "name of input telemetry file";

	keyname->tim_file = "TIM_FILE";
	stdkeys->tim_file = "";
	comment->tim_file = "name of the time assignment file";

	keyname->att_file = "ATT_FILE";
	stdkeys->att_file = "";
	comment->att_file = "name of the satellite attitude file";

	keyname->orb_file = "ORB_FILE";
	stdkeys->orb_file = "";
	comment->orb_file = "name of the satellite orbit file";

	keyname->leapfile = "LEAPFILE";
	stdkeys->leapfile = "";
	comment->leapfile = "name of the leap second file";

	keyname->teldef   = "TELDEF";
	stdkeys->teldef   = "";
	comment->teldef   = "name of the telescope definition file";

	keyname->creator  = "CREATOR";
	stdkeys->creator  = "";
	comment->creator  = "software that created this file";

	keyname->origin   = "ORIGIN";
	stdkeys->origin   = "ISAS/JAXA";
	comment->origin   = "origin of FITS file";
}

/* write common keywords */
int
aeWriteStdKeys(fitsfile *fp, AE_STD_KEYS *v)
{
	char *k;
	int istat = 0;
	int morekeys = 80;	/* reserve space for header keywords */
	struct AE_STD_KEYS_COMMENT *c = &v->comment;

	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: fitsptr == NULL in aeWriteStdKeys()\n", pname);
		return -1;
	}

/*
   details for the HDUCLASn keywords, see
   http://legacy.gsfc.nasa.gov/docs/heasarc/ofwg/docs/ofwg_recomm/hduclas.html
*/
	if ( NULL == v->hduclas1 ) {
		;	/* no HDUCLASn keywords */
	} else if ( 0 == strcmp("TEMPORALDATA", v->hduclas1) ) {
		if ( 0 == strcmp("HKP", v->hduclas2) ) {
			c->hduclas2 = "housekeeping parameters";
		} else if ( 0 == strcmp("TSI", v->hduclas2) ) {
			c->hduclas2 = "temporal status indicators";
		} else if ( 0 == strcmp("ASPECT", v->hduclas2) ) {
			c->hduclas2 = "spacecraft aspect data";
		} else if ( 0 == strcmp("EPHEM",  v->hduclas2) ) {
			c->hduclas2 = "spacecraft orbit information";
		} else if ( 0 == strcmp("EVRATE", v->hduclas2) ) {
			c->hduclas2 = "accepted, rejected event rates, etc.";
		}
	} else if ( 0 == strcmp("EVENTS", v->hduclas1) ) {
		if ( 0 == strcmp("ALL", v->hduclas2) ) {
			c->hduclas2 = "photon event list, includes all photons";
		} else if ( 0 == strcmp("ACCEPTED", v->hduclas2) ) {
			c->hduclas2 = "only accepted photons included in table";
		} else if ( 0 == strcmp("REJECTED", v->hduclas2) ) {
			c->hduclas2 = "only rejected photons included in table";
		}
	} else if ( 0 == strcmp("GTI", v->hduclas1) ) {
		if ( 0 == strcmp("ALL", v->hduclas2) ) {
			c->hduclas2 = "Good Time Intervals, without screening";
		} else if ( 0 == strcmp("STANDARD", v->hduclas2) ) {
			c->hduclas2 = "Good Time Intervals, with std screening";
		}
	}

	if (
fits_write_key_str   (fp, k="TELESCOP", v->telescop, c->telescop, &istat) ||
/*
fits_write_key_str   (fp, k="INSTRUME", v->instrume, c->instrume, &istat) ||
*/
fits_write_key_str   (fp, k="OBS_MODE", v->obs_mode, c->obs_mode, &istat) ||
/*
fits_write_key_str   (fp, k="DATAMODE", v->datamode, c->datamode, &istat) ||
*/
fits_write_key_str   (fp, k="OBS_ID",   v->obs_id,   c->obs_id,   &istat) ||
fits_write_key_str   (fp, k="OBSERVER", v->observer, c->observer, &istat) ||
fits_write_key_str   (fp, k="OBJECT",   v->object,   c->object,   &istat) ||
fits_write_key_str   (fp, k="OBS_REM",  v->obs_rem,  c->obs_rem,  &istat) ||
fits_write_key_null  (fp, k="RA_OBJ",                c->ra_obj,   &istat) ||
fits_write_key_null  (fp, k="DEC_OBJ",               c->dec_obj,  &istat) ||
/*
fits_write_key_null  (fp, k="RA_PNT",                c->ra_pnt,   &istat) ||
fits_write_key_null  (fp, k="DEC_PNT",               c->dec_pnt,  &istat) ||
*/
fits_write_key_null  (fp, k="RA_NOM",                c->ra_nom,   &istat) ||
fits_write_key_null  (fp, k="DEC_NOM",               c->dec_nom,  &istat) ||
fits_write_key_null  (fp, k="PA_NOM",                c->pa_nom,   &istat) ||
fits_write_key_null  (fp, k="MEAN_EA1",              c->mean_ea1, &istat) ||
fits_write_key_null  (fp, k="MEAN_EA2",              c->mean_ea2, &istat) ||
fits_write_key_null  (fp, k="MEAN_EA3",              c->mean_ea3, &istat) ||
fits_write_key_str   (fp, k="RADECSYS", v->radecsys, c->radecsys, &istat) ||
fits_write_key_lng   (fp, k="EQUINOX",  v->equinox,  c->equinox,  &istat) ||
fits_write_key_str   (fp, k="DATE-OBS", v->date_obs, c->date_obs, &istat) ||
fits_write_key_str   (fp, k="TIME-OBS", v->time_obs, c->time_obs, &istat) ||
fits_write_key_str   (fp, k="DATE-END", v->date_end, c->date_end, &istat) ||
fits_write_key_str   (fp, k="TIME-END", v->time_end, c->time_end, &istat) ||
fits_write_key_fixdbl(fp, k="TSTART",   v->tstart,   8, c->tstart,  &istat) ||
fits_write_key_fixdbl(fp, k="TSTOP",    v->tstop,    8, c->tstop,   &istat) ||
fits_write_key_fixdbl(fp, k="TELAPSE",  v->telapse,  8, c->telapse, &istat) ||
fits_write_key_fixdbl(fp, k="ONTIME",   v->ontime,   8, c->ontime,  &istat) ||
fits_write_key_str   (fp, k="TIMESYS",  v->timesys,  c->timesys,  &istat) ||
fits_write_key_lng   (fp, k="MJDREFI",  v->mjdrefi,  c->mjdrefi,  &istat) ||
fits_write_key_fixdbl(fp, k="MJDREFF",  v->mjdreff,  17, c->mjdreff, &istat) ||
fits_write_key_str   (fp, k="TIMEREF",  v->timeref,  c->timeref,  &istat) ||
fits_write_key_str   (fp, k="TIMEUNIT", v->timeunit, c->timeunit, &istat) ||
fits_write_key_str   (fp, k="TASSIGN",  v->tassign,  c->tassign,  &istat) ||
fits_write_key_log   (fp, k="CLOCKAPP", v->clockapp, c->clockapp, &istat) ||
/*
fits_write_key_dbl   (fp, k="TIMEDEL",  v->timedel,  5, c->timedel,  &istat) ||
fits_write_key_fixdbl(fp, k="TIMEPIXR", v->timepixr, 1, c->timepixr, &istat) ||
fits_write_key_dbl   (fp, k="TIERRELA", v->tierrela, 1, c->tierrela, &istat) ||
fits_write_key_dbl   (fp, k="TIERABSO", v->tierabso, 1, c->tierabso, &istat) ||
*/
		0 ) {
		goto error;
	}

	if ( NULL != v->hduclas1 && '\0' != v->hduclas1[0] ) {
		if (
fits_write_key_str   (fp, k="HDUCLASS", v->hduclass, c->hduclass, &istat) ||
fits_write_key_str   (fp, k="HDUCLAS1", v->hduclas1, c->hduclas1, &istat) ||
fits_write_key_str   (fp, k="HDUCLAS2", v->hduclas2, c->hduclas2, &istat) ||
			0 ) {
			goto error;
		}
	}

	if (
fits_write_key_str   (fp, k="TLM_FILE", v->tlm_file, c->tlm_file, &istat) ||
fits_write_key_str   (fp, k="TIM_FILE", v->tim_file, c->tim_file, &istat) ||
fits_write_key_str   (fp, k="ATT_FILE", v->att_file, c->att_file, &istat) ||
fits_write_key_str   (fp, k="ORB_FILE", v->orb_file, c->orb_file, &istat) ||
fits_write_key_str   (fp, k="LEAPFILE", v->leapfile, c->leapfile, &istat) ||
/*
fits_write_key_str   (fp, k="TELDEF",   v->teldef,   c->teldef,   &istat) ||
*/
fits_write_key_str   (fp, k="CREATOR",  v->creator,  c->creator,  &istat) ||
		0 ) {
		goto error;
	}

	if ( NULL != v->origin ) {
fits_write_key_str   (fp, k="ORIGIN",   v->origin,   c->origin,   &istat);
		if ( istat ) goto error;
	}

fits_set_hdrsize(fp, morekeys, &istat);
	if ( istat ) {
		fprintf(stderr,"\
%s: fits_set_hdrsize(morekeys=%d) failed (%d)\n", pname, morekeys, istat);
		return istat;
	}

 error:
	if ( istat ) {
		fprintf(stderr,"\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	return 0;
}

/* copy common keywords */
int
aeCopyStdKeys(fitsfile *ifp, fitsfile *ofp, AE_STD_KEYS *v)
{
	static char fname[] = "aeCopyStdKeys";

	int ik, nk;
	char **kp, *k;
	char card[FLEN_CARD];

	int istat = 0;

	nk = sizeof(v->keyname) / sizeof(char *);
	kp = (char **)&v->keyname;

	for (ik = 0; ik < nk; ik++) {

		k = kp[ik];
		if ( NULL == k ) {
			continue;
		}

		fits_read_card(ifp, k, card, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: WARNING: fits_read_card('%s') failed (%d), ignored\n", fname, k, istat);
			istat = 0;
			continue;
		}

		fits_update_card(ofp, k, card, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_update_card('%s') failed (%d)\n", fname, k, istat);
			return istat;
		}

	}

	return 0;
}

/* update time keywords */
int
aeUpdateStdTimeKeys(fitsfile *fp, AE_STD_KEYS *v)
{
	char comment_tstart[80], comment_tstop[80];
	char *k, date_obs[11], date_end[11], time_obs[9], time_end[9];
	AtTimeD attime;
	int istat = 0;

	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: fitsptr == NULL in aeUpdateStdTimeKeys()\n", pname);
		return -1;
	}

/* calculate date and time */
	aste2attimeD(v->tstart, &attime);
	sprintf(time_obs,"%02d:%02d:%02d", attime.hr, attime.mn, attime.sc);
	sprintf(date_obs,"%04d-%02d-%02d", attime.yr, attime.mo, attime.dy);
	sprintf(comment_tstart, "time start: %sT%s.%06d (UT)",
		date_obs, time_obs, (int)(1e6 * attime.ss));

	aste2attimeD(v->tstop, &attime);
	sprintf(time_end,"%02d:%02d:%02d", attime.hr, attime.mn, attime.sc);
	sprintf(date_end,"%04d-%02d-%02d", attime.yr, attime.mo, attime.dy);
	sprintf(comment_tstop, "time stop:  %sT%s.%06d (UT)",
		date_end, time_end, (int)(1e6 * attime.ss));

	v->telapse = v->tstop - v->tstart;

/*
   If ontime if not caululated, set it to telapse.
   This is true if there is only one GTI.
   If more than one GTIs, it should be updated at the end.
*/
	if ( 0.0 == v->ontime ) {
		v->ontime = v->telapse;
	}

	if (
fits_update_key_str(fp, k="DATE-OBS", date_obs, "&", &istat) ||
fits_update_key_str(fp, k="TIME-OBS", time_obs, "&", &istat) ||
fits_update_key_str(fp, k="DATE-END", date_end, "&", &istat) ||
fits_update_key_str(fp, k="TIME-END", time_end, "&", &istat) ||
fits_update_key_fixdbl(fp, k="TSTART", v->tstart, 8, comment_tstart, &istat) ||
fits_update_key_fixdbl(fp, k="TSTOP", v->tstop, 8, comment_tstop, &istat) ||
fits_update_key_fixdbl(fp, k="TELAPSE", v->telapse, 8, "&", &istat) ||
fits_update_key_fixdbl(fp, k="ONTIME", v->ontime, 8, "&", &istat) ||
		0 ) {
		fprintf(stderr, "\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	return 0;
}

/* finalize common keywords */
int
aeFinalizeStdKeys(fitsfile *fp, AE_STD_KEYS *v, long nrow, char *pname)
{
	int istat = 0;

/* update NAXIS2 */
	fits_modify_key_lng(fp, "NAXIS2", nrow, "&", &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_modify_key('NAXIS2') failed (%d)\n", pname, istat);
		return istat;
	}

/* update TSTART, TSTOP, etc */
	istat = aeUpdateStdTimeKeys(fp, v);
	if ( istat ) {
		fprintf(stderr, "\
%s: Error in aeUpdateStdTimeKeys() (status=%d)\n", pname, istat);
		return istat;
	}

/* add DATE */
	fits_write_date(fp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_date() failed (%d)\n", pname, istat);
		return istat;
	}

/* add CHECKSUM */
	fits_write_chksum(fp, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
		return istat;
	}

	istat = aefits_write_module_history(fp, pname);

	return istat;
}

void
aeGetFFFname(AE_STD_KEYS *v, char *name, int name_size, char *ext)
{
	static char leapfile[1024];
	int used, len;
	char *rpt_file;

	name_size--;
	name[name_size] = '\0';

	BnkfGetM("ASTE:RPT:IFILE_NAME:PTR", sizeof(rpt_file), &used, &rpt_file);
	if ( used == sizeof(rpt_file) ) {
		v->tlm_file = aefits_basename(rpt_file);
		strncpy(name, v->tlm_file, name_size);
		len = strlen(name);
		if ( strlen("aeCiYYMMDDn_N.rpt") == len && '_' == name[11] ) {
			/* force "aeCiYYMMDDn.hk" */
			strncpy(name+11, ext, name_size-11);
		} else if ( 4 < len && 0 == strcmp(".rpt", &name[len-4]) ) {
			/* traditional way, aeYYYYMMDD_HHMM_HHMM_com */
			name[len-4] = '\0';
			strncat(name, "_com", name_size);
			strncat(name, ext, name_size);
		} else {
			/* something else, filename_com */
			strncat(name, "_com", name_size);
			strncat(name, ext, name_size);
		}
	} else {
		strncpy(name, "ae_com", name_size);
		strncat(name, ext, name_size);
	}

	BnkfGetM("ASTE:TIM_FILE:PTR", sizeof(v->tim_file), &used, &v->tim_file);
	if ( used == sizeof(v->tim_file) ) {
		v->tim_file = aefits_basename(v->tim_file);
	} else {
		v->tim_file = "none";
	}

	used = 0;
	BnkfGetM("ASTE:LEAPSEC_FILE", sizeof(leapfile), &used, leapfile);
	leapfile[used] = '\0';
	if ( used ) {
		v->leapfile = aefits_basename(leapfile);
	} else {
		v->leapfile = "none";
	}
}

void
aeGetFFForigin(AE_STD_KEYS *v)
{
	static char fff_origin[80];		/* must be statically declared */
	int used = 0;

	BnkfGetM("ASTE:FFF_ORIGIN", sizeof(fff_origin)-1, &used, fff_origin);
	if ( 0 == used ) {
		v->origin = "ISAS/JAXA";	/* default */
	} else {
		fff_origin[used] = '\0';
		v->origin = fff_origin;
    }
}

/************************************************************************
int aefits_write_module_history()	: write ANL modules in FITS header

Input:
	fitsfile *fp		: fits file pointer to be written
	char *pname			: message header string

Return_Values:
	0					: success
	others				: CFITSIO error
************************************************************************/
int
aefits_write_module_history(fitsfile *fp, char *pname)
{
	char buf[256], datestr[20], waku[80];
	char *task_name, *task_version;
	int i, flag, num_module, timeref, len;

	int istat = 0;

	task_name = anl_task_name();
	task_version = anl_task_version();
	num_module = anl_module_num();
	fits_get_system_time(datestr, &timeref, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_get_system_time() failed (%d)\n", pname, istat);
		return istat;
	}

	sprintf(buf, " %s version %s at %s", task_name, task_version, datestr);
	len = strlen(buf) + 1;
	if ( 71 < len ) len = 71;
	memset(waku, '-', len);
	waku[len] = '\0';

	fits_write_history(fp, waku, &istat);
	fits_write_history(fp, buf, &istat);
	fits_write_history(fp, waku, &istat);

	for (i = 0; i < num_module; i++) {
		flag = anl_routine_flag(i);
		if ( '-' == flag ) {
			continue;
		}
		sprintf(buf, "%-24s%s", anl_routine_name(i), anl_routine_version(i));
		fits_write_history(fp, buf, &istat);
	}

	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history() failed (%d)\n", pname, istat);
	}

	return istat;
}

/************************************************************************
int aefits_write_tool_version()	: write tool version in FITS header

HISTORY + pname      version N.n

Input:
	fitsfile *fp		: fits file pointer to be written
	char *pname			: tool name
	char *version		: version string

Return_Values:
	0					: success
	others				: CFITSIO error
************************************************************************/
int
aefits_write_tool_version(fitsfile *fp, char *pname, char *version)
{
	char *format, buf[256];

	int istat = 0;

	if ( '0' <= *version && *version <= '9' ) {
		format = "+ %-22sversion %s";
	} else {
		format = "+ %-22s%s";
	}
	sprintf(buf, format, pname, version);

	fits_write_history(fp, buf, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history() failed (%d)\n", pname, istat);
	}

	return istat;
}

/************************************************************************
int aefits_write_name_vers()	: write name & version into fits header,

HISTORY ---------------------------------------------------------
HISTORY AEtimePacketFitsWrite version 2.4 at 2005-05-15T10:51:21
HISTORY ---------------------------------------------------------

Input:
	fitsfile *fp		: fits file pointer to be written
	char *name			: name of task/module
	char *version		: version of task/module

Return_Values:
	0					: success
	others				: CFITSIO error
************************************************************************/
int
aefits_write_name_vers(fitsfile *fp, char *name, char *version)
{
	int timeref, len;
	char datestr[20], waku[80], buf[256];

	int istat = 0;

	fits_get_system_time(datestr, &timeref, &istat);
	if ( 0 == strncmp("version ", version, 8) ) {
		sprintf(buf, " %s %s at %s", name, version, datestr);
	} else {
		sprintf(buf, " %s version %s at %s", name, version, datestr);
	}
	len = strlen(buf) + 1;
	if ( 71 < len ) len = 71;
	memset(waku, '-', len);
	waku[len] = '\0';
	fits_write_history(fp, waku, &istat);
	fits_write_history(fp, buf, &istat);
	fits_write_history(fp, waku, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	return 0;
}

/************************************************************************
int aefits_modify_comment()	: modify comment of the specified keyword

	This function is similar to fits_modify_comment(),
	but long comment is automatically wrapped to the next line.

Input:
	fitsfile *fp		: fits file pointer to be modified
	char *key			: header keyword to modify comment
	char *comment		: new comment words

Return_Values:
	0					: success
	others				: CFITSIO error
************************************************************************/
int
aefits_modify_comment(fitsfile *fp, char *key, char *comment)
{
	int ikey, i;
	int keypos;
	char buf[16], card[81];

	int istat = 0;

	if ( NULL == fp || NULL == key || NULL == comment ) {
		return 0;
	}

	sprintf(buf, "%-8s=", key);

	for (ikey = 1; ; ikey++) {
		fits_read_record(fp, ikey, card, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: key='%s' not found in header\n", pname, key);
			return -1;
		}
		if ( 0 == strncmp(buf, card, 9) ) {
			keypos = ikey;
			break;
		}
	}

	fits_modify_comment(fp, key, comment, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_modify_comment('%s') failed (%d)\n", pname, key, istat);
		return istat;
	}

	fits_read_record(fp, keypos, card, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_read_record() failed (%d) for '%s' at keypos=%d\n",
			pname, istat, key, keypos);
		return istat;
	}

	for (i = strlen(card); i < 80; i++) {
		card[i] = ' ';		/* pad spaces */
	}

	for (i = 12; i < 80; i++) {
		if ( card[i] == comment[0] && 0 == strncmp(card+i, comment, 80-i) ) {
			char *commentleft;
			int naddkeys;
			int commentlen = strlen(comment);
			int writtenlen = 80 - i;
			if ( commentlen <= writtenlen ) {
				break;	/* comment ok */
			}
			naddkeys = ((commentlen - writtenlen) + 69) / 70;
			commentleft = comment + writtenlen;
			for (i = 0; i < naddkeys; i++) {
				sprintf(card, "COMMENT   %-70.70s", commentleft+70*i);
				fits_insert_record(fp, keypos+i+1, card, &istat);
				if ( istat ) {
					fprintf(stderr, "\
%s: fits_insert_record() failed (%d)\n", pname, istat);
					return istat;
				}
			}
			break;
		}
	}

	return 0;
}

/************************************************************************
int aefits_modify_table_comments()	: modify comment of the table TTYPEnnn

Input:
	fitsfile *fp		: fits file pointer to be modified
	int tfield			: number of table columns
	char *comments[]	: new comment words

Return_Values:
	0					: success
	-1					: invalid number of tfield
	others				: CFITSIO error
************************************************************************/
int
aefits_modify_table_comments(fitsfile *fp, int tfield, char *comments[])
{
	static int keypos[999];	/* maximum number of FITS table columns is 999 */

	int ikey, ico, i;
	char key[16], card[81];

	int istat = 0;

	if ( tfield <= 0 || 999 < tfield ) {
		fprintf(stderr, "\
%s: invalid number of tfield=%d\n", pname, tfield);
		return -1;
	}

	ico = 1;
	for (ikey = 1; ; ikey++) {
		fits_read_record(fp, ikey, card, &istat);
		if ( istat ) {
			istat = 0;
			break;
		}
		if ( 0 == strncmp("TTYPE", card, 5) &&
			 1 == sscanf(card+5, "%d", &ico) ) {
			if ( 0 < ico && ico - 1 < tfield ) {
				keypos[ico-1] = ikey;
			}
		}
	}

	for (ico = 0; ico < tfield; ico++) {
		if ( NULL == comments[ico] ) {
			continue;
		}
		sprintf(key, "TTYPE%d", ico+1);
		fits_modify_comment(fp, key, comments[ico], &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_modify_comment('%s') failed (%d)\n", pname, key, istat);
			return istat;
		}

		fits_read_record(fp, keypos[ico], card, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: fits_read_record() failed (%d) for TTYPE%d at keypos=%d\n",
				pname, istat, ico+1, keypos[ico]);
			return istat;
		}
		for (i = strlen(card); i < 80; i++) {
			card[i] = ' ';		/* pad spaces */
		}
		for (i = 12; i < 80; i++) {
			if ( card[i] == comments[ico][0] &&
				 0 == strncmp(card+i, comments[ico], 80-i) ) {
				char *commentleft;
				int naddkeys;
				int commentlen = strlen(comments[ico]);
				int writtenlen = 80 - i;
				if ( commentlen <= writtenlen ) {
					break;	/* comment ok */
				}
				naddkeys = ((commentlen - writtenlen) + 69) / 70;
				commentleft = comments[ico] + writtenlen;
				for (i = 0; i < naddkeys; i++) {
					sprintf(card, "COMMENT   %-70.70s", commentleft+70*i);
					fits_insert_record(fp, keypos[ico]+i+1, card, &istat);
					if ( istat ) {
						fprintf(stderr, "\
%s: fits_insert_record() failed (%d)\n", pname, istat);
						return istat;
					}
				}
				for (i = ico + 1; i < tfield; i++) {
					keypos[i] += naddkeys;
				}
				break;
			}
		}
	}

	return 0;
}

/************************************************************************
int aefits_del_write_key()	: write key after delete previous one

Input:
	fitsfile *fp		: fits file pointer to be modified
	int datatype		: specifies the data type of the value
	char *keyname		: name of a keyword (8 char max, null-terminated)
	void *value			: keyword value
	char *comment		: keyword comment field (72 char max, null-terminated)

Output:
	int *status			: returned error status code (0 = OK)
************************************************************************/
int
aefits_del_write_key(
	fitsfile *fp, int datatype, char *keyname, void *value, char *comment,
	int *status)
{
	if ( *status ) return *status;

	while ( 0 == *status ) {
		fits_delete_key(fp, keyname, status);
	}

	if ( KEY_NO_EXIST != *status ) {
		return *status;
	}

	*status = 0;
	fits_write_key(fp, datatype, keyname, value, comment, status);

	return *status;
}

/************************************************************************
int aefits_del_write_key_fixdbl()	: write key after delete previous one

Input:
	fitsfile *fp		: fits file pointer to be modified
	char *keyname		: name of a keyword (8 char max, null-terminated)
	double numval		: numerical data value
	int decimals		: number of decimal places to be displayed
	char *comment		: keyword comment field (72 char max, null-terminated)

Output:
	int *status			: returned error status code (0 = OK)
************************************************************************/
int
aefits_del_write_key_fixdbl(
	fitsfile *fp, char *keyname, double numval, int decimals, char *comment,
	int *status)
{
	if ( *status ) return *status;

	while ( 0 == *status ) {
		fits_delete_key(fp, keyname, status);
	}

	if ( KEY_NO_EXIST != *status ) {
		return *status;
	}

	*status = 0;
	fits_write_key_fixdbl(fp, keyname, numval, decimals, comment, status);

	return *status;
}

/************************************************************************
int aefits_basename()	: get file name without directory

Input:
	char *path			: new comment words

Return_Values:
	char *basename		: the  component following the final '/'
************************************************************************/
char *
aefits_basename(char *path)
{
	char *p;

	for (p = path; *path; path++) {
		if ( '/' == *path ) {
			p = path + 1;
		}
	}

	return p;
}

/************************************************************************
double aefits_delta_phi()	: calculate angular differnce

Input:
	double phi1, phi0	: angles in degree

Return_Values:
	double delta_phi	: (phi1 - phi0), in the range of (-180.0, 180.0]
************************************************************************/
double
aefits_delta_phi(double phi1, double phi0)
{
	double delta_phi = phi1 - phi0;

	while ( 180.0 < delta_phi ) {
		delta_phi -= 180.0;
	}

	while ( delta_phi <= -180.0 ) {
		delta_phi += 180.0;
	}

	return delta_phi;
}

/************************************************************************
double aefits_mjd_tt2mission()	: convert MJD-TT into mission time (OBSOLETE)

Input:
	double mjd_tt		: Modified Julain Date (dy) in TT (Terrestrial Time)
	int    mjdrefi		: integer part of the MJD reference
	double mjdreff		: fractional part of the MJD reference

Return_Values:
	double mission_time	: mission time (s)
************************************************************************/
double
aefits_mjd_tt2mission(double mjd_tt, int mjdrefi, double mjdreff)
{
	static double DAYSEC = 24 * 60 * 60;

	double mission_time;

	mission_time = (mjd_tt - mjdrefi) * DAYSEC;	/* must be calculated first */
	mission_time -= mjdreff * DAYSEC;

	return mission_time;
}

/************************************************************************
double aefits_mission2mjd_tt()	: convert mission time int MJD-TT (OBSOLETE)

Input:
	double mission_time	: mission time (s)
	int    mjdrefi		: integer part of the MJD reference
	double mjdreff		: fractional part of the MJD reference

Return_Values:
	double mjd_tt		: Modified Julain Date (dy) in TT (Terrestrial Time)
************************************************************************/
double
aefits_mission2mjd_tt(double mission_time, int mjdrefi, double mjdreff)
{
	static double DAYSEC = 24 * 60 * 60;

	double mjd_tt;

	mjd_tt = mission_time / DAYSEC + mjdreff;	/* must be calculated first */
	mjd_tt += mjdrefi;

	return mjd_tt;
}

/************************************************************************
double aefits_datestr2attimeD()	:

		convert date string '2000-01-01T00:00:00.000' into AtTimeD

Input:
	char *datestrIN		: input date string '2000-01-01T00:00:00.000'

Output:
	AtTimeD *attimeOUT	: output AtTimeD structure

Return_Values:
	0					: success
	-1					: out of memory
	-2					: invalid format
************************************************************************/
int
aefits_datestr2attimeD(char *datestrIN, AtTimeD *attimeOUT)
{
	char *p;
	char *datestr;
	AtTimeD attime;
	double aetime;
	int num_read;

	datestr = strdup(datestrIN);
	if ( NULL == datestr ) {
		return -1;
	}

/* ignore non-number chars */
	for (p = datestr; *p; p++) {
		if ( (*p < '0' || '9' < *p) && '.' != *p ) {
			*p = ' ';
		}
	}

/* initialize attime */
	attime.yr = 2000;
	attime.mo = 1;
	attime.dy = 1;
	attime.hr = 0;
	attime.mn = 0;
	attime.sc = 0;
	attime.ss = 0.0;

	num_read = sscanf(datestr, "%d %d %d %d %d %lf",
		&attime.yr, &attime.mo, &attime.dy,
		&attime.hr, &attime.mn, &attime.ss);

	if ( 0 == num_read ) {
		free(datestr);
		return -2;

	} else if ( 1 == num_read ) {
		aetime = atof(datestr);
		aste2attimeD(aetime, &attime);

	} else if ( 6 == num_read ) {
		attime.sc = (int)floor(attime.ss);
		attime.ss = attime.ss - attime.sc;

	}

	*attimeOUT = attime;
	free(datestr);

	return 0;
}

/************************************************************************
char *aefits_attimeD2datestr() :

		convert AtTimeD into date string '2000-01-01T00:00:00.000'

Input:
	AtTimeD *attimeIN	: input AtTimeD structure

Output:
	char *datestrOUT	: output date string '2000-01-01T00:00:00.000'

Return_Values:
	char *datestrOUT	: same as argument
************************************************************************/
char *
aefits_attimeD2datestr(AtTimeD *attimeIN, char *datestrOUT)
{
	int us;
	AtTimeD attime;

	attime = *attimeIN;

	if ( 0.0 == attime.ss ) {
		sprintf(datestrOUT, "%04d-%02d-%02dT%02d:%02d:%02d",
			attime.yr, attime.mo, attime.dy,
			attime.hr, attime.mn, attime.sc);
	} else {

		us = floor( 1e6 * attime.ss );
		while ( 1000000 < us ) {
			us -= 1000000;
			attime.sc += 1;
		}
		while ( us < 0 ) {
			us += 1000000;
			attime.sc -= 1;
		}

		sprintf(datestrOUT, "%04d-%02d-%02dT%02d:%02d:%02d.%06d",
			attime.yr, attime.mo, attime.dy,
			attime.hr, attime.mn, attime.sc, us);
	}

	return datestrOUT;
}

/************************************************************************
double aefits_datestr2aetime()	:

		convert date string '2000-01-01T00:00:00.000' into Astro-E time

Input:
	char *datestrIN		: input date string '2000-01-01T00:00:00.000'

Output:
	double *aetime		: output Astro-E time

Return_Values:
	0					: success
	-1					: invalid format
************************************************************************/
int
aefits_datestr2aetime(char *datestrIN, double *aetime)
{
	int istat;
	AtTimeD attime;

	istat = aefits_datestr2attimeD(datestrIN, &attime);
	if ( istat ) {
		return istat;
	}
	*aetime = attimeD2aste(&attime);

	return 0;
}

/************************************************************************
char *aefits_aetime2datestr() :

		convert Astro-E time into date string '2000-01-01T00:00:00.000'

Input:
	double aetime		: input Astro-E time

Output:
	char *datestrOUT	: output date string '2000-01-01T00:00:00.000'

Return_Values:
	char *datestrOUT	: same as argument
************************************************************************/
char *
aefits_aetime2datestr(double aetime, char *datestrOUT)
{
	AtTimeD attime;

	aste2attimeD(aetime, &attime);
	return aefits_attimeD2datestr(&attime, datestrOUT);
}

/************************************************************************
char *aefits_degToRAstr() :	convert deg into R.A. string 'NNhNNmNN.Ns'

Input:
	double deg			: Right Ascension in degree

Output:
	char *outstr		: output R.A. string 'NNhNNmNN.Ns'

Return_Values:
	char *outstr		: same as argument
************************************************************************/
char *
aefits_degToRAstr(double deg, char *outstr)
{
	AtRightAscension ra;
	int subsec;

	while ( deg < 0.0 ) {
		deg += 360.0;
	}

	while ( 360.0 <= deg ) {
		deg -= 360.0;
	}

	atDegToRA(deg, &ra);
	subsec = (int)( ra.sec * 10 + 0.5 );
	if ( 600 <= subsec ) {
		subsec = subsec - 600;
		ra.min = ra.min + 1;
		if ( 60 <= ra.min ) {
			ra.min = ra.min - 60;
			ra.hour = ra.hour + 1;
			if ( 24 <= ra.hour ) {
				ra.hour = ra.hour - 24;
			}
		}
	}
	sprintf(outstr, "%dh%02dm%04.1fs", ra.hour, ra.min, subsec/10.0);

	return outstr;
}

/************************************************************************
char *aefits_degToDECstr() : convert deg into DEC. string '+NNdNNmNNs'

Input:
	double deg			: Declination in degree

Output:
	char *outstr		: output DEC. string '+NNdNNmNNs'

Return_Values:
	char *outstr		: same as argument
************************************************************************/
char *
aefits_degToDECstr(double deg, char *outstr)
{
	int sig, sec;
	AtDeclination dec;

	while ( deg < -180.0 ) {
		deg += 180.0;
	}

	while ( +180.0 < deg ) {
		deg -= 180.0;
	}

	if ( deg < -90.0 ) {
		deg = - 180.0 - deg;	/* -91.0 -> -89.0 */
	}

	if ( +90.0 < deg ) {
		deg = 180.0 - deg;		/* 91.0 -> 89.0 */
	}

	atDegToDec(deg, &dec);
	sig = ( 0 < dec.sign ) ? '+' : '-';
	sec = (int)(dec.sec + 0.5);
	if ( 60 <= sec ) {
		sec = sec - 60;
		dec.min = dec.min + 1;
		if ( 60 <= dec.min ) {
			dec.min = dec.min - 60;
			dec.deg = dec.deg + 1;
		}
	}
	sprintf(outstr, "%c%dd%02dm%02ds", sig, dec.deg, dec.min, sec);

	return outstr;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
