/*
 xrsFitsHeaderUtil.c

	1999/11/19 Y.ISHISAKI	version 1.1
		TIMESYS keywords change to string of "2000-01-01T00:00:00.0"
		rename function name (add 'XRS' in head)
		add OPTIC2, OPTIC3, OPTIC10, OPTIC11

	1999/12/14 Y.ISHISAKI	version 1.2
		remove OPTIC2, OPTIC3, OPTIC10, OPTIC11

	2000/01/27 Y.ISHISAKI	version 1.3
		check BNK of ASTE:FFF_ORIGIN

	2004/01/05 Y.ISHISAKI	version 1.4
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

	2004/03/14 Y.ISHISAKI	version 1.5
		use AtTimeD (since atFunctions-2.2) instead of AtTime
		change "RA_NOM", "DEC_NOM" -> "RA_PNT", "DEC_PNT"
		remove "DATAMODE" keyword for HK files
		passing pointer of XRS_STD_KEYS for
			XRSwriteXRSStdKeys(), XRSupdateStdTimeKeys()
		add hduclas1, hduclas2 in XRS_STD_KEYS
		write HDUCLASS only when HDUCLAS1 != NULL

	2004/09/22 Y.ISHISAKI	version 1.6
		use aste_telescop() to get TELESCOP keyword

	2005/04/29 Y.ISHISAKI	version 1.7
		change function names
		add XRS_STD_KEYS_COMMENT *comment in structure
		modified to use ASTE:RPT:IFILE_NAME:PTR in xrsGetFFFname()
		reserve morekeys=80 for header keywords in xrsWriteStdKeys()
		add xrsGetFFForigin(), xrsFinalizeStdKeys(), xrsWriteModuleHistory()

	2005/04/29 Y.ISHISAKI	version 1.8
		ignore modules with flag='-' in xrsWriteModuleHistory()

	2005/06/03 Y.ISHISAKI	version 1.9
		telescope -> telescop, instrument -> instrume
		add ra_obj, dec_obj, ra_nom, dec_nom, pa_nom, mean_ea1/2/3
		write null keywords for ra/dec_obj/nom/pnt, mean_ea1/2/3

	2005/06/07 Y.ISHISAKI	version 2.0
		bug fix in MJDREFF
MJDREFF = 0.00074287037037037 / fractional part of the MJD reference (64.184 s)
		del xrsWriteModuleHistory(), use aefits_write_module_history() instead

	2005/06/19-21 Y.ISHISAKI	version 2.1
		add ATT_FILE, ORB_FILE, TELDEF, GATEVALV
		use aste_instrume() to get INSTRUME
		use aefits_basename() instead of local basename() in xrsGetFFFname()
		support for new RPT naming of 'aeCiYYMMDDn_N.rpt' in xrsGetFFFname()
		set v->tim_file in xrsGetFFFname()

	2005/06/26 Y.ISHISAKI	version 2.2
		set HDUCLAS2 comment according to HDUCLASn values

	2005/06/26 Y.ISHISAKI	version 2.3
		set keyname in xrsSetDefaultKeywordValues()
		add xrsCopyStdKeys()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "anl_misc.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_coord.h"
#include "aeFitsHeaderUtil.h"
#include "xrsTelemFormat.h"
#include "xrsFitsHeaderUtil.h"

static char pname[] = "xrsFitsHeaderUtil";
char xrsFitsHeaderUtil_version[] = "2.2";

/* STD HDU header keyword */
void
xrsSetDefaultKeywordValues(XRS_STD_KEYS *stdkeys)
{
	struct XRS_STD_KEYS_NAME    *keyname = &stdkeys->keyname;
	struct XRS_STD_KEYS_COMMENT *comment = &stdkeys->comment;

	keyname->telescop = "TELESCOP";
	stdkeys->telescop = aste_telescop();
	comment->telescop = "telescope (mission) name";

	keyname->instrume = "INSTRUME";
	stdkeys->instrume = aste_instrume(ASTE_XRS_ID);
	comment->instrume = "instrument name";

	keyname->obs_mode = "OBS_MODE";
	stdkeys->obs_mode = "DEFAULT";
	comment->obs_mode = "observation mode";

	keyname->datamode = "DATAMODE";
	stdkeys->datamode = "HOUSEKEEPING";	/* does not written for HK files */
	comment->datamode = "XRS standard datamode";

	keyname->filter   = "FILTER";
	stdkeys->filter   = "DEFAULT";
	comment->filter   = "filter wheel used";

	keyname->gatevalv = "GATEVALV";
	stdkeys->gatevalv = "DEFAULT";
	comment->gatevalv = "whether gate valve of Ne dewar is OPEN or CLOSED";

	keyname->object   = "OBJECT";
	stdkeys->object   = "DEFAULT";
	comment->object   = "name of observed object";

	keyname->observer = "OBSERVER";
	stdkeys->observer = "DEFAULT";
	comment->observer = "Principal Investigator";

	keyname->date_obs = "DATE-OBS";
	stdkeys->date_obs = "DEFAULT";
	comment->date_obs = "start date of observations (UT)";

	keyname->time_obs = "TIME-OBS";
	stdkeys->time_obs = "DEFAULT";
	comment->time_obs = "start time of observations (UT)";

	keyname->date_end = "DATE-END";
	stdkeys->date_end = "DEFAULT";
	comment->date_end = "end date of observations (UT)";

	keyname->time_end = "TIME-END";
	stdkeys->time_end = "DEFAULT";
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

	keyname->radecsys = "RADECSYS";
	stdkeys->radecsys = "FK5";
	comment->radecsys = "World Coordinate System";

	keyname->equinox  = "EQUINOX";
	stdkeys->equinox  = 2000;
	comment->equinox  = "equinox for coordinate system";

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

	keyname->timesys  = "TIMESYS";
	stdkeys->timesys  = "TT";
	comment->timesys  = "time system (TT:Terrestrial Time)";

	keyname->mjdrefi  = "MJDREFI";
	stdkeys->mjdrefi  = 51544;
	comment->mjdrefi  = "integer part of the MJD reference (2000.0 UT)";

	keyname->mjdreff  = "MJDREFF";
	stdkeys->mjdreff  = 0.00074287037037037;	/* 64.184/86400 */
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
	stdkeys->timedel  = 5.08626e-6;		/* 1/12288/16 for EVENTS */
	comment->timedel  = "finest time resolution (time between frames)";

	keyname->timepixr = "TIMEPIXR";
	stdkeys->timepixr = 0.5;
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
	stdkeys->hduclas1 = "TEMPORALDATA";	/* "EVENTS" for event FITS */
	comment->hduclas1 = "type of data (e.g. EVENTS/TEMPORALDATA)";

	keyname->hduclas2 = "HDUCLAS2";
	stdkeys->hduclas2 = "HKP";			/* "ALL" for event FITS */
	comment->hduclas2 = "housekeeping parameters";

	keyname->tlm_file = "TLM_FILE";
	stdkeys->tlm_file = "DEFAULT";
	comment->tlm_file = "name of input telemetry file";

	keyname->tim_file = "TIM_FILE";
	stdkeys->tim_file = "DEFAULT";
	comment->tim_file = "name of the time assignment file";

	keyname->att_file = "ATT_FILE";
	stdkeys->att_file = "DEFAULT";
	comment->att_file = "name of the satellite attitude file";

	keyname->orb_file = "ORB_FILE";
	stdkeys->orb_file = "DEFAULT";
	comment->orb_file = "name of the satellite orbit file";

	keyname->teldef   = "TELDEF";
	stdkeys->teldef   = "DEFAULT";
	comment->teldef   = "name of the telescope definition file";

	keyname->creator  = "CREATOR";
	stdkeys->creator  = "DEFAULT";
	comment->creator  = "software that created this file";

	keyname->origin   = "ORIGIN";
	stdkeys->origin   = NULL;		/* to set value, use xrsGetFFForigin() */
	comment->origin   = "origin of FITS file";

/* DM keywords */
	keyname->mtype1   = "MTYPE1";
	stdkeys->mtype1   = "DET";
	comment->mtype1   = "DM keyword: DET coordinates";

	keyname->mform1   = "MFORM1";
	stdkeys->mform1   = "DETX,DETY";
	comment->mform1   = "DM keyword: DET X and Y coordinates";

	keyname->mtype2   = "MTYPE2";
	stdkeys->mtype2   = "FOC";
	comment->mtype2   = "DM keyword: FOC coordinates";

	keyname->mform2   = "MFORM2";
	stdkeys->mform2   = "FOCX,FOCY";
	comment->mform2   = "DM keyword: FOC X and Y coordinates";

	keyname->mtype3   = "MTYPE3";
	stdkeys->mtype3   = "SKY";
	comment->mtype3   = "DM keyword: SKY coordinates";

	keyname->mform3   = "MFORM3";
	stdkeys->mform3   = "X,Y";
	comment->mform3   = "DM keyword: SKY X and Y coordinates";

}

/* write common keywords */
int
xrsWriteStdKeys(fitsfile *fp, XRS_STD_KEYS *v)
{
	char *k;
	int istat = 0;
	int morekeys = 80;	/* reserve space for header keywords */
	struct XRS_STD_KEYS_COMMENT *c = &v->comment;

	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: fitsptr == NULL in xrsWriteStdKeys()\n", pname);
		return -1;
	}

/*
   details for the HDUCLASn keywords, see
   http://legacy.gsfc.nasa.gov/docs/heasarc/ofwg/docs/ofwg_recomm/hduclas.html
*/
	if ( 0 == strcmp("TEMPORALDATA", v->hduclas1) ) {
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
fits_write_key_str   (fp, k="INSTRUME", v->instrume, c->instrume, &istat) ||
fits_write_key_str   (fp, k="OBS_MODE", v->obs_mode, c->obs_mode, &istat) ||
		0 ) {
		goto error;
	}
	if ( 0 != strcmp("HOUSEKEEPING", v->datamode) ) {
fits_write_key_str   (fp, k="DATAMODE", v->datamode, c->datamode, &istat);
		if ( istat ) goto error;
	}
	if (
fits_write_key_str   (fp, k="FILTER",   v->filter,   c->filter,   &istat) ||
fits_write_key_str   (fp, k="GATEVALV", v->gatevalv, c->gatevalv, &istat) ||
fits_write_key_str   (fp, k="OBJECT",   v->object,   c->object,   &istat) ||
fits_write_key_str   (fp, k="OBSERVER", v->observer, c->observer, &istat) ||
fits_write_key_str   (fp, k="DATE-OBS", v->date_obs, c->date_obs, &istat) ||
fits_write_key_str   (fp, k="TIME-OBS", v->time_obs, c->time_obs, &istat) ||
fits_write_key_str   (fp, k="DATE-END", v->date_end, c->date_end, &istat) ||
fits_write_key_str   (fp, k="TIME-END", v->time_end, c->time_end, &istat) ||
fits_write_key_fixdbl(fp, k="TSTART",   v->tstart,   8, c->tstart,  &istat) ||
fits_write_key_fixdbl(fp, k="TSTOP",    v->tstop,    8, c->tstop,   &istat) ||
fits_write_key_fixdbl(fp, k="TELAPSE",  v->telapse,  8, c->telapse, &istat) ||
fits_write_key_fixdbl(fp, k="ONTIME",   v->ontime,   8, c->ontime,  &istat) ||
fits_write_key_str   (fp, k="RADECSYS", v->radecsys, c->radecsys, &istat) ||
fits_write_key_lng   (fp, k="EQUINOX",  v->equinox,  c->equinox,  &istat) ||
fits_write_key_null  (fp, k="RA_OBJ",                c->ra_obj,   &istat) ||
fits_write_key_null  (fp, k="DEC_OBJ",               c->dec_obj,  &istat) ||
fits_write_key_null  (fp, k="RA_PNT",                c->ra_pnt,   &istat) ||
fits_write_key_null  (fp, k="DEC_PNT",               c->dec_pnt,  &istat) ||
fits_write_key_null  (fp, k="RA_NOM",                c->ra_nom,   &istat) ||
fits_write_key_null  (fp, k="DEC_NOM",               c->dec_nom,  &istat) ||
fits_write_key_null  (fp, k="PA_NOM",                c->pa_nom,   &istat) ||
fits_write_key_null  (fp, k="MEAN_EA1",              c->mean_ea1, &istat) ||
fits_write_key_null  (fp, k="MEAN_EA2",              c->mean_ea2, &istat) ||
fits_write_key_null  (fp, k="MEAN_EA3",              c->mean_ea3, &istat) ||
fits_write_key_str   (fp, k="TIMESYS",  v->timesys,  c->timesys,  &istat) ||
fits_write_key_lng   (fp, k="MJDREFI",  v->mjdrefi,  c->mjdrefi,  &istat) ||
fits_write_key_fixdbl(fp, k="MJDREFF",  v->mjdreff,  17, c->mjdreff, &istat) ||
fits_write_key_str   (fp, k="TIMEREF",  v->timeref,  c->timeref,  &istat) ||
fits_write_key_str   (fp, k="TIMEUNIT", v->timeunit, c->timeunit, &istat) ||
fits_write_key_str   (fp, k="TASSIGN",  v->tassign,  c->tassign,  &istat) ||
fits_write_key_log   (fp, k="CLOCKAPP", v->clockapp, c->clockapp, &istat) ||
fits_write_key_dbl   (fp, k="TIMEDEL",  v->timedel,  5, c->timedel,  &istat) ||
fits_write_key_fixdbl(fp, k="TIMEPIXR", v->timepixr, 1, c->timepixr, &istat) ||
fits_write_key_dbl   (fp, k="TIERRELA", v->tierrela, 1, c->tierrela, &istat) ||
fits_write_key_dbl   (fp, k="TIERABSO", v->tierabso, 1, c->tierabso, &istat) ||
		0 ) {
		goto error;
	}

	if ( NULL != v->hduclas1 && '\0' != v->hduclas1 ) {
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
fits_write_key_str   (fp, k="TELDEF",   v->teldef,   c->teldef,   &istat) ||
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
xrsCopyStdKeys(fitsfile *ifp, fitsfile *ofp, XRS_STD_KEYS *v)
{
	static char fname[] = "xrsCopyStdKeys";

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
xrsUpdateStdTimeKeys(fitsfile *fp, XRS_STD_KEYS *v)
{
	char comment_tstart[80], comment_tstop[80];
	char *k, date_obs[11], date_end[11], time_obs[9], time_end[9];
	AtTimeD attime;
	int istat = 0;

	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: fitsptr == NULL in xrsUpdateStdTimeKeys()\n", pname);
		return -1;
	}

/* calculate date and time */
	aste2attimeD(v->tstart, &attime);
	sprintf(date_obs,"%04d-%02d-%02d", attime.yr, attime.mo, attime.dy);
	sprintf(time_obs,"%02d:%02d:%02d", attime.hr, attime.mn, attime.sc);
	sprintf(comment_tstart, "time start: %sT%s.%06d (UT)",
		date_obs, time_obs, (int)(1e6 * attime.ss));

	aste2attimeD(v->tstop, &attime);
	sprintf(date_end,"%04d-%02d-%02d", attime.yr, attime.mo, attime.dy);
	sprintf(time_end,"%02d:%02d:%02d", attime.hr, attime.mn, attime.sc);
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
fits_update_key_fixdbl(fp, k="TELAPSE", v->telapse, 8,"&",&istat) ||
fits_update_key_fixdbl(fp, k="ONTIME", v->ontime, 8, "&", &istat) ||
		0 ) {
		fprintf(stderr, "\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	return 0;
}

int
xrsWriteDMcoorKeys(fitsfile *fp, XRS_STD_KEYS *v)
{
	char *k;
	int istat = 0;
	struct XRS_STD_KEYS_COMMENT *c = &v->comment;

	if ( NULL == fp ) {
		fprintf(stderr, "\
%s: fitsptr == NULL in xrsWriteDMcoorKeys()\n", pname);
		return -1;
	}

	if (
fits_write_key_str(fp, k="MTYPE1", v->mtype1, c->mtype1, &istat) ||
fits_write_key_str(fp, k="MFORM1", v->mform1, c->mform1, &istat) ||
fits_write_key_str(fp, k="MTYPE2", v->mtype2, c->mtype2, &istat) ||
fits_write_key_str(fp, k="MFORM2", v->mform2, c->mform2, &istat) ||
fits_write_key_str(fp, k="MTYPE3", v->mtype3, c->mtype3, &istat) ||
fits_write_key_str(fp, k="MFORM3", v->mform3, c->mform3, &istat) ||
		0 ) {
		fprintf(stderr,"\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	return 0;
}

/* finalize common keywords */
int
xrsFinalizeStdKeys(fitsfile *fp, XRS_STD_KEYS *v, long nrow, char *pname)
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
	istat = xrsUpdateStdTimeKeys(fp, v);
	if ( istat ) {
		fprintf(stderr, "\
%s: Error in xrsUpdateStdTimeKeys() (status=%d)\n", pname, istat);
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
xrsGetFFFname(XRS_STD_KEYS *v, char *name, int name_size, char *ext)
{
	int used, len;
	char *rpt_file;

	BnkfGetM("ASTE:RPT:IFILE_NAME:PTR", sizeof(rpt_file), &used, &rpt_file);
	if ( used == sizeof(rpt_file) ) {
		v->tlm_file = aefits_basename(rpt_file);
		strncpy(name, v->tlm_file, name_size);
		len = strlen(name);
		if ( strlen("aeCiYYMMDDn_N.rpt") == len ) {
			strcpy(name + 12, "0");		/* force "aeCiYYMMDDn_0" */
		} else if ( 4 < len && 0 == strcmp(".rpt", &name[len-4]) ) {
			name[len-4] = '\0';
		}
		strncat(name, "_xrs", name_size);
		strncat(name, ext, name_size);
	} else {
		strncpy(name, "ae_xrs", name_size);
		strncat(name, ext, name_size);
	}

	BnkfGetM("ASTE:TIM_FILE:PTR", sizeof(v->tim_file), &used, &v->tim_file);
	if ( used == sizeof(v->tim_file) ) {
		v->tim_file = aefits_basename(v->tim_file);
	} else {
		v->tim_file = "none";
	}
}

void
xrsGetFFForigin(XRS_STD_KEYS *v)
{
	static char fff_origin[80];		/* must be statically declared */
	int used;

	BnkfGetM("ASTE:FFF_ORIGIN", sizeof(fff_origin)-1, &used, fff_origin);
	if ( 0 == used ) {
		v->origin = "JPN-ISAS";	/* default */
	} else {
		fff_origin[used] = '\0';
		v->origin = fff_origin;
    }
}
