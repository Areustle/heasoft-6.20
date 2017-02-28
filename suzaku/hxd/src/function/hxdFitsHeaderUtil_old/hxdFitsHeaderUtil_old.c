/*
 * changed for Astro-E2,  by Y.Terada 2003-10-27
 *         MJDREF changed by Y.Terada 2003-12-20
 *         add CREDITS    by Y.Terada 2004-03-09
 *         debug          by Y.Terada 2004-03-11
 *         for atFunctions v2.2 by Y.Terada 2004-03-12
 *         change telescope by M.Suzuki,Y.Terada, 2004-09-15
 *         telescop keyword, using astetool v1.27
 *                          by Y.Terada 2004-09-28
 *         incerased the comment buffer size for credit,
 *                          by Y.Terada 2005-02-07
 *         add detnam keyword
 *                          by Y.Terada 2005-05-17
 *         add RA_OBJ, DEC_OBJ, RA_PNT, DEC_PNT
 *                          by Y.Terada 2005-05-18
 *         add ROLL_NOM     by Y.Terada 2005-05-23
 *         modefy RA DEC keywords, (version 1.1, by Ebisawa-san)
 *                  updated by Y.Terada 2005-05-25
 *         update TIMEDEL   by Y.Terada 2005-06-03
 *         change MJD_REF   by Y.Terada 2005-06-07
 *         add OBS_ID, OBS_REM for in-orbit version
 *         remove 'DEFAULT' value, NULL on TIMEDEL
 *                          by Y.Terada 2005-10-18
 *         HK/EVT/CALDB keywords
 *                          by Y.Terada 2005-10-19
 *         read TSTART etc in HEADSER
 *                          by Y.Terada 2005-10-22
 *         delete DATAMODE, TIMEDEL, TIMEPIXR, TIERRELA, TIERABSO in HK
 *                          by Y.Terada 2005-10-24
 *         fix-floating in TSTART etc, add teldef keyword
 *         Null value in TIERABSO, TIERRELA.
 *                          by Y.Terada 2005-10-26
 *         debug in writeCredits 
 *                          by Y.Terada 2005-11-02
 *         add writeParameter_xxx, write PIL values
 *                          by Y.Terada 2005-11-04
 *         debug writeParameter_xxx
 *                          by Y.Terada 2005-11-05
 *         debug temp[1] in hxdFitsHeader_extruct_credits()
 *                          by M.Kokubun 2005-11-07
 *         debug in hxdFitsHeader_extruct_credits()
 *                          by M.Ozaki,Y.Ishisaki,Y.Terada 2005-11-08
 *         debug in writing param, hxdpinlin      Y.Terada 2006-04-28
 *         add obs_id                             Y.Terada 2006-07-11
 *         add NOM_PMT                            Y.Terada 2007-04-12
 *         support CALDB access function in HIS   Y.Terada 2007-04-27
 *         CALDB access function for BST          Y.Terada 2007-05-06
 *         CALDB access function for TRN          Y.Terada 2007-05-10
 *         for hxdpi v2.1.0                       Y.Terada 2007-05-13
 *         hxdbsttime                             Y.Terada 2008-10-23
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fitsio.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "atFunctions.h"
#include "aste_coord.h"

#include "hxdFitsHeaderUtil_old.h"
#include "hxdFitsCommentUtil.h"

static char *pname = "hxdFitsHeaderUtil_old";

static HXD_FITS_HEADER_CREDITS_BUF credits_buf;

/* STD HDU header keyword */
void hxdFitsHeader_old_setDefaultKeywordValues(HXD_STD_KEYS *stdkeys)
{
/*stdkeys->telescope=strdup("ASTRO-E");*/
/*stdkeys->telescope=strdup("ASTRO-E2");*/
/*stdkeys->telescope=strdup("Astro-E2");*/
/*  stdkeys->telescope=strdup( aste_telescop() );*/
  sprintf(stdkeys->telescope, aste_telescop());
  sprintf(stdkeys->instrument, "HXD");
  sprintf(stdkeys->detnam,   "        ");
  sprintf(stdkeys->obs_mode, "        ");
  sprintf(stdkeys->datamode, "        ");
  sprintf(  stdkeys->object, "        ");
  sprintf(  stdkeys->observer, "        ");
  sprintf(  stdkeys->obs_id,   "        ");
  sprintf(  stdkeys->date_obs, "        ");
  sprintf(  stdkeys->time_obs, "        ");
  sprintf(  stdkeys->date_end, "        ");
  sprintf(  stdkeys->time_end, "        ");
  stdkeys->tstart=0.0;
  stdkeys->tstop=0.0;
  stdkeys->ontime=0.0;
  stdkeys->telapse=0.0;
  sprintf(stdkeys->radecsys, "FK5");
  stdkeys->equinox=2000;
  stdkeys->write_radec=0; /** for FFF **/
  stdkeys->ra_obj=0.0;
  stdkeys->dec_obj=0.0;
  stdkeys->ra_nom=0.0;
  stdkeys->dec_nom=0.0;
  stdkeys->roll_nom=0.0;
  stdkeys->ra_pnt=0.0;
  stdkeys->dec_pnt=0.0;
  stdkeys->mean_ea1=0.0;
  stdkeys->mean_ea2=0.0;
  stdkeys->mean_ea3=0.0;
/*stdkeys->mjdref=51544;*/
  sprintf( stdkeys->timeref, "LOCAL");
  stdkeys->mjd_refi=51544;                 /** New **/
  stdkeys->mjd_reff=0.00074287037037037;  /** New **/
/*stdkeys->timesys=strdup("2000-01-01T00:00:00.0"); */
  sprintf(stdkeys->timesys, "TT      ");     /** changed **/
  sprintf(stdkeys->timeunit,"s");
  sprintf(stdkeys->tassign, "SATELLITE");
  stdkeys->clockapp=1;
/*  stdkeys->timedel=0.0; */
  stdkeys->timepixr=0.0; /* time is defined in leading edge of a time bin.*/
  stdkeys->tierrela=0.0; /* NULL VALUE */
  stdkeys->tierabso=0.0; /* NULL VALUE */
  sprintf(stdkeys->hduclass, "OGIP");
  stdkeys->hduclass_ev_hk = HXD_FITS_HEADER_UTIL_HDUCLASS_HK;
  sprintf(stdkeys->tlmfile, "        ");
  sprintf(stdkeys->timfile, "        ");
  sprintf(stdkeys->attfile, "        ");
  sprintf(stdkeys->orbfile, "        ");
  sprintf(stdkeys->leapfile,"        ");
  sprintf(stdkeys->teldef,  "        ");
  sprintf(stdkeys->credits, "HXD team");
  sprintf(stdkeys->task_name, "UNDEFINED");
  sprintf(stdkeys->task_version, "UNDEFINED");
  {
    int size;
    char str[256];

    BnkfGetM("ASTE:FFF_ORIGIN",sizeof(str),&size, str);
    str[size]='\0';
    if (str == NULL || strlen(str) == 0 ){
      /* stdkeys->origin=strdup("UNKOWN"); */
      sprintf(stdkeys->origin, "UNKOWN");
    } else {
      /* stdkeys->origin=strdup(str); */
      sprintf(stdkeys->origin, str);
    }
  }		
  /*    stdkeys->origin[sizeof(stdkeys->origin+1)]='\0';  */

}


/* write common keywords:
 * these are common keywords not only for for primary HDU but also for other
 * extentions
 */

int hxdFitsHeader_old_writeHXDStdKeys (fitsfile * fp, HXD_STD_KEYS stdkeys,
				   int hdunum, int *status)
{
  
  int hdutype;
  
  char comment[73], hduclas1[10];
    
  if (fp == (fitsfile *)NULL) {
    fprintf(stderr, "%s: NULL pointer in writeHXDStdKeys() \n",pname);
    return ANL_NG;
  }
  
  /* move to the specified extention unless hdunum=0 */
  if (hdunum != 0) {
    if (fits_movabs_hdu(fp, hdunum, &hdutype, status)) {
      fprintf(stderr, "%s_writeHXDStdKeys: fits_movabs_hdu %d failed (status=%d)\n",
	      pname, hdunum, *status);
      return ANL_NG;
    }
  }
  if (fits_write_key_str(fp, "TELESCOP", stdkeys.telescope, 
			 "Telescope (mission) name", status)) {
    fprintf(stderr,"Error in writing keyword: TELESCOP (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "INSTRUME", stdkeys.instrument, 
			 "instrument name", status)) {
    fprintf(stderr,"Error in writing keyword: INSTRUME (status=%d)\n",*status);
    return ANL_NG;
  }

  if (stdkeys.use_detnam) {
    if (fits_write_key_str(fp, "DETNAM", stdkeys.detnam, 
			   "detector name", status)) {
      fprintf(stderr,"Error in writing keyword: DETNAM (status=%d)\n",*status);
      return ANL_NG;
    }
  }

  if (fits_write_key_str(fp, "OBS_MODE", stdkeys.obs_mode, 
			 "Observation mode", status)) {
    fprintf(stderr,"Error in writing keyword: OBS_MODE (status=%d)\n",*status);
    return ANL_NG;
  }
  if (stdkeys.hduclass_ev_hk == HXD_FITS_HEADER_UTIL_HDUCLASS_EVENT){
    if (fits_write_key_str(fp, "DATAMODE", stdkeys.datamode, 
			   "Data mode", status)) {
      fprintf(stderr,"Error in writing keyword: DATAMODE (status=%d)\n",
	      *status);
      return ANL_NG;
    }
  }
  if (fits_write_key_str(fp, "OBS_ID", stdkeys.obs_id, 
			 "Observation Identifier", status)){
    fprintf(stderr,"Error in writing keyword: OBS_ID (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "OBSERVER", stdkeys.observer, 
			 "Principal Investigator", status)){
    fprintf(stderr,"Error in writing keyword: OBSERVER (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "OBJECT",   stdkeys.object,  
			 "Name of observed object", status)) {
    fprintf(stderr,"Error in writing keyword: OBJECT (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "OBS_REM", "        ", 
			 "remark on observation", status)){
    fprintf(stderr,"Error in writing keyword: OBS_REM (status=%d)\n",*status);
    return ANL_NG;
  }

  if (stdkeys.write_radec == 0) {
    if (fits_write_key_null(fp, "RA_OBJ","Planned target R.A. (deg)", status)) {
      fprintf(stderr,"Error in writing keyword: RA_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "DEC_OBJ", "Planned target DEC. (deg)", status)){
      fprintf(stderr,"Error in writing keyword: RA_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "RA_PNT", 
			    "average optical axis location R.A. (deg)", 
			    status)) {
      fprintf(stderr,"Error in writing keyword: RA_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "DEC_PNT",
			    "average optical axis location DEC. (deg)", 
			    status)) {
      fprintf(stderr,"Error in writing keyword: RA_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "RA_NOM",
			    "Nominal satellite pointing direction R.A. (deg)", 
			    status)) {
      fprintf(stderr,"Error in writing keyword: RA_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "DEC_NOM",
			    "Nominal satellite pointing direction DEC. (deg)", 
			    status)){
      fprintf(stderr,"Error in writing keyword: DEC_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "PA_NOM",
			    "Nominal position angle from North to DETY (deg)", 
			    status)){
      fprintf(stderr,"Error in writing keyword: PA_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "NOM_PNT",
			    "XIS:(0.0,0.0), HXD:(-3.5,0.0), SPECIAL:others", 
			    status)){
      fprintf(stderr,"Error in writing keyword: NOM_PNT (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "MEAN_EA1",
			    "Mean of the 1st ZYZ-Euler angle (deg)", 
			    status)){
      fprintf(stderr,"Error in writing keyword: MEAN_EA1 (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "MEAN_EA2",
			    "Mean of the 2nd ZYZ-Euler angle (deg)", 
			    status)){
    fprintf(stderr,"Error in writing keyword: MEAN_EA2 (status=%d)\n",*status);
    return ANL_NG;
    }
    if (fits_write_key_null(fp, "MEAN_EA3",
			    "Mean of the 3ed ZYZ-Euler angle (deg)", 
			    status)){
      fprintf(stderr,"Error in writing keyword: MEAN_EA3 (status=%d)\n",*status);
      return ANL_NG;
    }
  } else if (stdkeys.write_radec == 1){
    if (fits_write_key_dbl(fp, "RA_OBJ", stdkeys.ra_obj,6,
			   "Planned target R.A. (deg)", status)) {
      fprintf(stderr,"Error in writing keyword: RA_OBJ (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_dbl(fp, "DEC_OBJ", stdkeys.dec_obj,6,
			   "Planned target DEC. (deg)", status)){
      fprintf(stderr,"Error in writing keyword: DEC_OBJ (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_dbl(fp, "RA_PNT", stdkeys.ra_pnt,6,
			    "average optical axis location R.A. (deg)", 
			    status)) {
      fprintf(stderr,"Error in writing keyword: RA_PNT (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_dbl(fp, "DEC_PNT",stdkeys.dec_pnt,6,
			    "average optical axis location DEC. (deg)", 
			    status)) {
      fprintf(stderr,"Error in writing keyword: DEC_PNT (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_dbl(fp, "RA_NOM",stdkeys.ra_nom,6,
			    "Nominal satellite pointing direction R.A. (deg)", 
			    status)) {
      fprintf(stderr,"Error in writing keyword: RA_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_dbl(fp, "DEC_NOM",stdkeys.dec_nom,6,
			    "Nominal satellite pointing direction DEC. (deg)", 
			    status)){
      fprintf(stderr,"Error in writing keyword: DEC_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_dbl(fp, "PA_NOM",stdkeys.roll_nom,6,
			    "Nominal position angle from North to DETY (deg)", 
			    status)){
      fprintf(stderr,"Error in writing keyword: PA_NOM (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_dbl(fp, "MEAN_EA1",stdkeys.mean_ea1,6,
			    "Mean of the 1st ZYZ-Euler angle (deg)", 
			    status)){
      fprintf(stderr,"Error in writing keyword: MEAN_EA1 (status=%d)\n",*status);
      return ANL_NG;
    }
    if (fits_write_key_dbl(fp, "MEAN_EA2",stdkeys.mean_ea2,6,
			    "Mean of the 2nd ZYZ-Euler angle (deg)", 
			    status)){
    fprintf(stderr,"Error in writing keyword: MEAN_EA2 (status=%d)\n",*status);
    return ANL_NG;
    }
    if (fits_write_key_dbl(fp, "MEAN_EA3",stdkeys.mean_ea3,6,
			   "Mean of the 3ed ZYZ-Euler angle (deg)", 
			   status)){
      fprintf(stderr,"Error in writing keyword: MEAN_EA3 (status=%d)\n",*status);
      return ANL_NG;
    }
  }

  if (fits_write_key_str(fp, "RADECSYS", stdkeys.radecsys, 
			 "World Coordinate System", status)){
    fprintf(stderr,"Error in writing keyword: RADECSY (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_lng(fp, "EQUINOX",  stdkeys.equinox, 
			 "Equinox for coordinate system", status)) {
    fprintf(stderr,"Error in writing keyword: EQUINOX (status=%d)\n",*status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "DATE-OBS", stdkeys.date_obs, 
			 "Start date of observations", status)){
    fprintf(stderr,"Error in writing keyword: DATE-OBS (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "TIME-OBS", stdkeys.time_obs, 
			 "Start time of observations", status)){
    fprintf(stderr,"Error in writing keyword: TIME-OBS (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "DATE-END", stdkeys.date_end, 
			 "End date of observations", status)) {
    fprintf(stderr,"Error in writing keyword: DATE-END (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "TIME-END", stdkeys.time_end, 
			 "End time of observations", status)){
    fprintf(stderr,"Error in writing keyword: TIME-END (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_fixdbl(fp, "TSTART",   stdkeys.tstart, 6, 
			 "time start", status)){
    fprintf(stderr,"Error in writing keyword: TSTART (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_fixdbl(fp, "TSTOP",    stdkeys.tstart, 6,  
			 "time stop", status)){
    fprintf(stderr,"Error in writing keyword: TSTOP (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_fixdbl(fp, "TELAPSE",   stdkeys.telapse, 6, 
			 "elapsed time: TSTOP-TSTART", status)){
    fprintf(stderr,"Error in writing keyword: ONTIME (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_fixdbl(fp, "ONTIME",   stdkeys.tstart, 6, 
			 "ontime: sum of all GTIs", status)){
    fprintf(stderr,"Error in writing keyword: ONTIME (status=%d)\n",*status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "TIMESYS",  stdkeys.timesys,
			 "Time system", status)){
    fprintf(stderr,"Error in writing keyword: TIMESYS (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_lng(fp, "MJDREFI",   stdkeys.mjd_refi,
			 "integer part of the MJD reference (2000.0 UT)",
			 status)){
    fprintf(stderr,"Error in writing keyword: MJDREFI (status=%d)\n",*status);
    return ANL_NG;
  }

  if (fits_write_key_fixdbl(fp, "MJDREFF",   stdkeys.mjd_reff, 17, 
			    "fractional part of the MJD reference (64.184 s)",
			    status)){
    fprintf(stderr,"Error in writing keyword: MJDREFF (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "TIMEREF",  stdkeys.timeref, 
			 "Barycentric correction not applied to times",
			 status)){
    fprintf(stderr,"Error in writing keyword: TIMEREF (status=%d)\n",*status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "TIMEUNIT", stdkeys.timeunit, 
			 "unit for time related keywords", status)){
    fprintf(stderr,"Error in writing keyword: TIMEUNIT (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_str(fp, "TASSIGN",  stdkeys.tassign, 
			 "TASSIGN", status)) {
    fprintf(stderr,"Error in writing keyword: TASSIGN (status=%d)\n",*status);
    return ANL_NG;
  }
  if (fits_write_key_log(fp, "CLOCKAPP", stdkeys.clockapp, 
			 "CLOCKAPP", status)){
    fprintf(stderr,"Error in writing keyword: OBJECT (status=%d)\n",*status);
    return ANL_NG;
  }
  if (stdkeys.hduclass_ev_hk != HXD_FITS_HEADER_UTIL_HDUCLASS_HK){
    if (fits_write_key_null(fp, "TIMEDEL",
			    "finest time resolution (time between frames)", 
			    status)){
      fprintf(stderr,"Error in writing keyword: TIMEDEL (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    if (fits_write_key_flt(fp, "TIMEPIXR", stdkeys.timepixr, 1, 
			   "0:times refer to the beginning of bin, 0.5:middle",
			   status)){
      fprintf(stderr,"Error in writing keyword: TIMEPIXR (status=%d)\n",
	      *status);
      return ANL_NG;
    }

    if (fits_write_key_flt(fp, "TIERRELA", stdkeys.tierrela, 1, 
			   "short-term clock stability", status)){
      fprintf(stderr,"Error in writing keyword: TIERRELA (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    if (fits_write_key_flt(fp, "TIERABSO", stdkeys.tierabso, 1, 
			   "absolute precision of the clock", status)){
      fprintf(stderr,"Error in writing keyword: TIERABSO (status=%d)\n",
	      *status);
      return ANL_NG;
    }

    /*
    if (fits_write_key_null(fp, "TIERRELA","short-term clock stability",
			    status)) {
      fprintf(stderr,"Error in writing keyword: TIERRELA (status=%d)\n",
	      *status);
      return ANL_NG;
    }

    if (fits_write_key_null(fp, "TIERABSO","absolute precision of the clock",
			    status)) {
      fprintf(stderr,"Error in writing keyword: (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    */
  }

  if (fits_write_key_str(fp, "HDUCLASS", stdkeys.hduclass, 
			 "format conforms to OGIP/GSFC conventions", status)){
    fprintf(stderr,"Error in writing keyword: HDUCLASS (status=%d)\n",*status);
    return ANL_NG;
  }

  /** add **/
  if (stdkeys.hduclass_ev_hk == HXD_FITS_HEADER_UTIL_HDUCLASS_EVENT){
    if (fits_write_key_str(fp, "HDUCLAS1", "EVENTS",
			   "type of data (e.g. EVENTS/TEMPORALDATA)",
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS1 (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    if (fits_write_key_str(fp, "HDUCLAS2", "ALL", 
			   "photon event list, includes all photons",
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS2 (status=%d)\n",
	      *status);
      return ANL_NG;
    }

  } else if (stdkeys.hduclass_ev_hk == HXD_FITS_HEADER_UTIL_HDUCLASS_HK){
    if (fits_write_key_str(fp, "HDUCLAS1", "TEMPORALDATA",
			   "type of data (e.g. EVENTS/TEMPORALDATA)", 
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS1 (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    if (fits_write_key_str(fp, "HDUCLAS2", "HKP", "housekeeping parameters",
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS2 (status=%d)\n",
	      *status);
      return ANL_NG;
    }

  } else if (stdkeys.hduclass_ev_hk == HXD_FITS_HEADER_UTIL_HDUCLASS_PHA){
    if (fits_write_key_str(fp, "HDUCLAS1", "SPECTRUM",
			   "PHA dataset (OGIP memo OGIP-92-007)", 
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS1 (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    if (fits_write_key_str(fp, "HDUCLAS1", "SPECTRUM",
			   "PHA dataset (OGIP memo OGIP-92-007)", 
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS1 (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    if (fits_write_key_str(fp, "HDUVERS1", "1.2.0",
			   "bsolete - included for backwards compatibility", 
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS1 (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    if (fits_write_key_str(fp, "HDUVERS", "1.2.0", 
			   "Version of format (OGIP memo OGIP-92-007)",
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS2 (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    if (fits_write_key_str(fp, "HDUCLAS3", "COUNT", 
			   "PHA data stored as Counts (not count/s)",
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS3 (status=%d)\n",
	      *status);
      return ANL_NG;
    }

  } else if (stdkeys.hduclass_ev_hk == HXD_FITS_HEADER_UTIL_HDUCLASS_PHA_PRI){
    if (fits_write_key_str(fp, "HDUCLAS1", "IMAGE",
			   "Extension contains an image", 
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS1 (status=%d)\n",
	      *status);
      return ANL_NG;
    }

    if (fits_write_key_str(fp, "HDUNAME", "WMAP", 
			   "HDU Name", status)){
      fprintf(stderr,"Error in writing keyword: HDUNAME (status=%d)\n",
	      *status);
      return ANL_NG;
    }

    if (fits_write_key_str(fp, "HDUCLAS2", "WMAP", 
			   "Extension contains a weighted map",
			   status)){
      fprintf(stderr,"Error in writing keyword: HDUCLAS2 (status=%d)\n",
	      *status);
      return ANL_NG;
    }

    if (fits_write_key_str(fp, "CONTENT", "SPECTRUM", 
			   "spectrum file contains time intervals and event",
			   status)){
      fprintf(stderr,"Error in writing keyword: CONTENT (status=%d)\n",
	      *status);
      return ANL_NG;
    }


  } else {
    if (fits_write_key_null(fp, "HDUCLAS1",
			    "type of data (e.g. EVENTS/TEMPORALDATA)",
			    status)) {
      fprintf(stderr,"Error in writing keyword: HDUCLAS1 (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    if (fits_write_key_null(fp, "HDUCLAS2",
			    "event or house keeping",
			    status)) {
      fprintf(stderr,"Error in writing keyword: HDUCLAS2 (status=%d)\n",
	      *status);
      return ANL_NG;
    }
    
  }

  if (fits_write_key_str(fp, "TLM_FILE",   stdkeys.tlmfile,
			 "Name of input telemetry file", status)){
    fprintf(stderr,"Error in writing keyword: TLM_FILE (status=%d)\n",*status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "TIM_FILE",   stdkeys.timfile,
			 "name of the time assignment file", status)){
    fprintf(stderr,"Error in writing keyword: TIM_FILE (status=%d)\n",*status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "ATT_FILE",   stdkeys.attfile,
			 "name of the satellite attitude file", status)){
    fprintf(stderr,"Error in writing keyword: ATT_FILE (status=%d)\n",*status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "ORB_FILE",   stdkeys.orbfile,
			 "name of the satellite orbit file", status)){
    fprintf(stderr,"Error in writing keyword: ORB_FILE (status=%d)\n",*status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "LEAPFILE",   stdkeys.leapfile,
			 "name of the leap second file", status)){
    fprintf(stderr,"Error in writing keyword: LEAPFILE (status=%d)\n",*status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "TELDEF", stdkeys.teldef,
			  "name of the telescope definition file", status)) {
    fprintf(stderr,"Error in writing keyword: TELDEF (status=%d)\n",
	    *status);
    return ANL_NG;
  }

  if (fits_write_key_str(fp, "CREATOR",  stdkeys.creator, 
			 "by HXD team",status)){
    fprintf(stderr,"Error in writing keyword: CREATOR (status=%d)\n",*status);
    return ANL_NG;
  }
  
  if (fits_write_key_str(fp, "ORIGIN",   stdkeys.origin, 
			 "origin of fits file", status)){
    fprintf(stderr,"Error in writing keyword: ORIGIN=%s (status=%d)\n",
	    stdkeys.origin,*status);
    return ANL_NG;
  }
  
  hxdFitsComment_write_single ( fp, "CREATOR ", "by HXD team", status);
  
  if(*status){
    fprintf(stderr,"hxdFitsComment_write_single ('CREATOR') failed\n",
	    *status);
    return ANL_NG;
  }

  hxdFitsHeader_old_writeCredits(fp, stdkeys.task_name, stdkeys.task_version,
			     stdkeys.credits, 
			     HXD_FITS_HEADER_UTIL_CREDIT_CREATE,
			     status);
  if(*status){
    fprintf(stderr,"hxdFitsHeader_old_writeCredits failed (%d)\n",
	    *status);
    return ANL_NG;
  }

  return ANL_OK;
  
}

void
hxdFitsHeader_old_writeCredits(fitsfile * fp, char * task_name, 
			   char * task_version, char * credits, 
			   int write_mode, int *status){
  char history[1024];
  int ii;
  int anl_status;

  switch ( write_mode ) {
  case HXD_FITS_HEADER_UTIL_CREDIT_CREATE:
    sprintf(history, "   Created by %s, version %s:", 
	    task_name, task_version);
    break;
  case HXD_FITS_HEADER_UTIL_CREDIT_DUPLICATE:
    sprintf(history, "   Duplicated and processed by %s, version %s:", 
	    task_name, task_version);
    break;
  case HXD_FITS_HEADER_UTIL_CREDIT_OVERWRITE:
  default:
    sprintf(history, "   Processed by %s, version %s:",
	    task_name, task_version);
    break;
  }

  fits_write_history(fp, history, status);
  if(*status){
    fprintf(stderr,"%s: fits_write_history failed (%d), (%s)\n", 
	    pname, *status, history);
    return;
  }

  anl_status = hxdFitsHeader_old_extruct_credits(credits);
  if (anl_status != ANL_OK) return;

  for (ii=0; ii<credits_buf.number; ii++){
    sprintf(history, "     %s", credits_buf.comments[ii]);
    fits_write_history(fp, history, status);
    /* fprintf(stderr,"%s: write history: %s\n", pname, history); */
    if(*status){
      fprintf(stderr,"%s: fits_write_history failed (%d) (%s)\n", 
	      pname, *status, history);
      return;
    }
  }

  return;
}

/* update time keywords */
int hxdFitsHeader_old_updateStdTimeKeys(fitsfile *fp, HXD_STD_KEYS stdkeys,
				    int hdunum, int *status) 
{
  int hdutype;
  char comment[] = "&";
  
  char date_obs[11], date_end[11], time_obs[9], time_end[9];
  /* AtTime attime; */
  AtTimeD attime;
  
  /* calculate date and time */
  /* aste2attime(stdkeys.tstart, &attime); */
  aste2attimeD(stdkeys.tstart, &attime);
  sprintf(time_obs,"%02d:%02d:%02d",attime.hr, attime.mn, attime.sc);
  sprintf(date_obs,"%04d-%02d-%02d",attime.yr, attime.mo, attime.dy);
  
  /* aste2attime(stdkeys.tstop, &attime); */
  aste2attimeD(stdkeys.tstop, &attime);
  sprintf(time_end,"%02d:%02d:%02d",attime.hr, attime.mn, attime.sc);
  sprintf(date_end,"%04d-%02d-%02d",attime.yr, attime.mo, attime.dy);
  
  stdkeys.telapse=stdkeys.tstop-stdkeys.tstart;
  
  /* if ontime if not caululated, set it to telapse;
   * this is true if there is only one GTI;
   * if more than one GTIs, it should be updated at the end*/
  if ((int)stdkeys.ontime == 0) stdkeys.ontime=stdkeys.telapse;
  
  if (fp == (fitsfile *)NULL) {
    fprintf(stderr, "%s: NULL pointer in updateStdTimeKeys \n",pname);
    return ANL_NG;
  }
  
  /* move to the specified extention unless hdunum=0 */
  if (hdunum != 0) {
    if (fits_movabs_hdu(fp, hdunum, &hdutype, status)) {
      fprintf(stderr, "%s_updateStdTimeKeys: fits_movabs_hdu %d failed (status=%d)\n",pname,hdunum, *status);
      return ANL_NG;
    }
  }
  
  if (fits_update_key_str(fp, "DATE-OBS", date_obs, "&", status)) {
    fprintf(stderr, "%s: fits_update_key_str DATE-OBS failed (%d)\n", pname,
	    *status);
    return ANL_NG;
  }
  
  if (fits_update_key_str(fp, "TIME-OBS", time_obs, "&", status)) {
    fprintf(stderr, "%s: fits_update_key_str TIME-OBS failed (%d)\n", pname,
	    *status);
    return ANL_NG;
  }
  
  if (fits_update_key_str(fp, "DATE-END", date_end, "&", status)) {
    fprintf(stderr, "%s: fits_update_key_str DATE-END failed (%d)\n", pname,
	    *status);
    return ANL_NG;
  }
  
  if (fits_update_key_str(fp, "TIME-END", time_end, "&", status)) {
    fprintf(stderr, "%s: fits_update_key_str TIME-END failed (%d)\n", pname,
	    *status);
    return ANL_NG;
  }
  
  if (fits_update_key_fixdbl (fp, "TSTART", stdkeys.tstart,
			      6, comment, status)){
    fprintf(stderr, "%s: ffmkys TSTART failed (%d)\n", pname, *status);
    return ANL_NG;
  }
  if (fits_update_key_fixdbl (fp, "TSTOP", stdkeys.tstop,
			      6, comment, status)){
    fprintf(stderr, "%s: ffmkys TSTOP failed (%d)\n", pname, *status);
    return ANL_NG;
  }
  if (fits_update_key_fixdbl (fp, "TELAPSE", stdkeys.telapse,
			      6, comment, status)){
    fprintf(stderr, "%s: ffmkys TELAPSE failed (%d)\n", pname, *status);
    return ANL_NG;
  }
  if (fits_update_key_fixdbl (fp, "ONTIME", stdkeys.ontime,
			     6, comment, status)){
    fprintf(stderr, "%s: ffmkys ONTIME failed (%d)\n", pname, *status);
    return ANL_NG;
  }
  
  return ANL_OK;
  
}


int hxdFitsHeader_old_timeUpdate (HXD_STD_KEYS *stdkeys, double aetime)
{

  if ( 0 == (int) stdkeys->tstart || aetime < stdkeys->tstart ) {
    stdkeys->tstart = aetime;
  }
  
  if ( 0 == (int) stdkeys->tstop || stdkeys->tstop < aetime ) {    
    stdkeys->tstop = aetime;
  }
  
  return ANL_OK;
  
}

/******* static function ************/
static int
hxdFitsHeader_old_extruct_credits( char* credits){
  /* The format of "credits" is like this:
     AEpacketRPTread     version 2.3
     AEpacketCount       version 1.1
     HXDpacketCount      version 0.0.3
     HXDeventExtract     version 4.0.3
     HXDread             version 3.0.1
  */
  int status = ANL_OK;
  int src_length; 
  int search_pos; 
  size_t chk_length = 1;
  char temp[2];         
  char outbuf[128];     
  /*char separate_char[1];*/
  int block_num = 0;
  int line_num = 0;
  static int called = 0;

  if (called) return status;  called = 1;

  /** -- initialize -- **/
  /*separate_char[0]='\0';  sprintf(separate_char, "\n");*/
  outbuf[0]='\0';  sprintf(outbuf, "");

  /** -- check length -- **/
  src_length = (int) strlen(credits);
  if (src_length == 0) {
    credits_buf.number = 0;
    return status;
  } else if (src_length < 0){
    fprintf(stderr, "%s: illegal CREDITS\n", pname);
    status = ANL_NG;
    return status;
  } else if(src_length>FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES){
    /* credit is limitted by FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES bytes. */
    src_length = FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES;
    fprintf(stderr, "%s: illegal CREDITS (%s)\n", pname, credits);
  }

  /** -- search credit[] -- **/
  /*
  for(search_pos=0; search_pos<src_length;search_pos++){
    temp[0] = credits[search_pos];    temp[1] ='\0';
    if (strcmp(temp, "\n")){ 
      strcat(outbuf, temp);
    } else {
      if((int)strlen(outbuf)>FITS_KEY_MAX_LENGTH){
	outbuf[FITS_KEY_MAX_LENGTH] = '\0';
      }
      strcpy(credits_buf.comments[line_num], outbuf);
      
      line_num ++;
      outbuf[0] = '\0';
      sprintf(outbuf, "");
      block_num ++;
    }
  }*/

  for (search_pos=0; search_pos<src_length && line_num<CREDITS_MAX_LINES; ){
    char *p;
    size_t a_line_size;
    
    p = strchr(&credits[search_pos], '\n'); /* searching the EOL */
    if ( NULL == p ) {
      a_line_size = strlen(&credits[search_pos]);
    } else {
      a_line_size = p - &credits[search_pos];
    }
    if ( a_line_size < FITS_KEY_MAX_LENGTH ) {
      memcpy(credits_buf.comments[line_num],&credits[search_pos], a_line_size);
      credits_buf.comments[line_num][a_line_size] = '\0';
      search_pos += a_line_size + 1;
    } else {
      a_line_size = FITS_KEY_MAX_LENGTH - 1;
      memcpy(credits_buf.comments[line_num],&credits[search_pos], a_line_size);
      credits_buf.comments[line_num][a_line_size] = '\0';
      search_pos += a_line_size;
    }
    line_num++;
    if ( NULL == p ) break;
  }
  /** end of search **/

  if(line_num > CREDITS_MAX_LINES) line_num = CREDITS_MAX_LINES;

  credits_buf.number = line_num;

  return status;
}

int 
hxdFitsHeader_old_writeTIMEDEL(fitsfile *fp, int extension_id, double timedel_val){
  int stat = HXD_FITS_HEADER_UTIL_OK;
  int istat = 0;
  
  char comment[] = "&";
  int hdutype;
  
  if (fits_movabs_hdu(fp, extension_id, &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",
	    pname, istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }
  
  if (fits_update_key_dbl (fp, "TIMEDEL", timedel_val,
			   14, comment, &istat)){
    fprintf(stderr, "%s: ffmkys TIMEDEL failed (%d)\n", pname, istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }
  
  return stat;
}

void hxdFitsHeader_old_mallocSTDKEYS(HXD_STD_KEYS *stdkeys){
  stdkeys->telescope  = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->instrument = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->detnam     = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->obs_mode   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->datamode   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->object     = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->observer   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->obs_id     = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->date_obs   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->time_obs   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->date_end   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->time_end   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->radecsys   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->timeref    = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->timesys    = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->timeunit   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->tassign    = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->creator    = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->origin     = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->hduclass   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->tlmfile    = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->timfile    = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->attfile    = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->orbfile    = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->leapfile   = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->teldef     = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->credits    = malloc(sizeof(char)* 
			       FITS_KEY_MAX_LENGTH*CREDITS_MAX_LINES);
  stdkeys->task_name  = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
  stdkeys->task_version = malloc(sizeof(char)* FITS_KEY_MAX_LENGTH);
}

void hxdFitsHeader_old_freeSTDKEYS(HXD_STD_KEYS *stdkeys){
  free( stdkeys->telescope   );
  free( stdkeys->instrument );
  free( stdkeys->detnam     );
  free( stdkeys->obs_mode   );
  free( stdkeys->datamode   );
  free( stdkeys->object     );
  free( stdkeys->observer   );
  free( stdkeys->date_obs   );
  free( stdkeys->time_obs   );
  free( stdkeys->date_end   );
  free( stdkeys->time_end   );
  free( stdkeys->radecsys   );
  free( stdkeys->timeref    );
  free( stdkeys->timesys    );
  free( stdkeys->timeunit   );
  free( stdkeys->tassign    );
  free( stdkeys->creator    );
  free( stdkeys->origin    );
  free( stdkeys->hduclass   );
  free( stdkeys->tlmfile    );
  free( stdkeys->timfile    );
  free( stdkeys->attfile    );
  free( stdkeys->orbfile    );
  free( stdkeys->leapfile   );
  free( stdkeys->teldef     );
  free( stdkeys->credits    );
  free( stdkeys->task_name  );
  free( stdkeys->task_version);
}

int 
hxdFitsHeader_old_readTIME(fitsfile* fp,
		       double* tstart,  double* tstop,
		       double* telapse, double* ontime){
  int stat = HXD_FITS_HEADER_UTIL_OK;
  int istat  = 0;
  char comment[80];
  double dbl_val;
  int hdutype;
  int extension_id = 1;

  if (fits_movabs_hdu(fp, extension_id, &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",
	    pname, istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  fits_read_key_dbl(fp, "TSTART", &dbl_val, comment, &istat);
  if(istat){
    fprintf(stderr, "read TSTART error (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }
  *tstart = dbl_val;

  fits_read_key_dbl(fp, "TSTOP", &dbl_val, comment, &istat);
  if(istat){
    fprintf(stderr, "read TSOP error (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }
  *tstop = dbl_val;

  fits_read_key_dbl(fp, "TELAPSE", &dbl_val, comment, &istat);
  if(istat){
    fprintf(stderr, "read TELAPSE error (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }
  *telapse = dbl_val;

  fits_read_key_dbl(fp, "ONTIME", &dbl_val, comment, &istat);
  if(istat){
    fprintf(stderr, "read ONTIME error (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  *ontime = dbl_val;

  return stat;
}

int 
hxdFitsHeader_old_writeParamer_wel(fitsfile *fp, int extension_id){
  int stat = HXD_FITS_HEADER_UTIL_OK;
  int istat = 0;
  int size;
  char history     [HXDFITSHEADER_LINESIZE];
  int  hdutype;

  /**** paremeters in HXDeventFitsRead ****/
  char read_iomode [HXDFITSHEADER_LINESIZE];
  int  time_change, grade_change, pi_pmt_change, pi_pin_change, gtimode;
  char gti_time    [HXDFITSHEADER_LINESIZE];
  char input_name  [HXDFITSHEADER_LINESIZE];
  char create_name [HXDFITSHEADER_LINESIZE];
  int  use_pwh_mode;
  /**** paremeters in HXDfwelTime ****/
  int  time_convert_mode;
  char tim_filename     [HXDFITSHEADER_LINESIZE];
  /**** paremeters in HXDpi ****/
  char pin_gainhist_name[HXDFITSHEADER_LINESIZE];
  char gso_gainhist_name[HXDFITSHEADER_LINESIZE];
  char hxdgsolin_fname  [HXDFITSHEADER_LINESIZE];
  char hxdpinlin_fname  [HXDFITSHEADER_LINESIZE];
  /**** paremeters in HXDgrade ****/
  char hxdgrade_psdsel_fname[HXDFITSHEADER_LINESIZE];
  char hxdgrade_pinthres_fname[HXDFITSHEADER_LINESIZE];
  /**** paremeters for all ****/
  char leapsec_name[HXDFITSHEADER_LINESIZE];

  /** flow control **/
  int  iomode;
  int  hxdtime_yn, hxdmkgainhist_yn, hxdpi_yn, hxdgrade_yn;
  int  piltype_caldb_leapsec;
  int  piltype_caldb_pinghf;
  int  piltype_caldb_gsoghf;
  int  piltype_caldb_pinlin;
  int  piltype_caldb_gsolin;
  int  piltype_caldb_psdsel;
  int  piltype_caldb_pinthr;

  enum{
    HXD_READONLY,
    HXD_OVERWRITE,
    HXD_CREATE
  };


  /** ------------------ init --------------------- **/
  if (fits_movabs_hdu(fp, extension_id, &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",
	    pname, istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  BnkGet( "HXD:ftools:hxdtime_yn",       sizeof(int), &size, &hxdtime_yn);
  BnkGet( "HXD:ftools:hxdmkgainhist_yn", sizeof(int), 
	  &size, &hxdmkgainhist_yn);
  BnkGet( "HXD:ftools:hxdpi_yn",         sizeof(int), &size, &hxdpi_yn);
  BnkGet( "HXD:ftools:hxdgrade_yn",      sizeof(int), &size, &hxdgrade_yn);
  if(hxdtime_yn){
    sprintf(history, "    ------- parameters in hxdtime  -------   ");
  } else if(hxdmkgainhist_yn){
    sprintf(history, "    ------- parameters in hxdmkgainhist -------   ");
  } else if(hxdpi_yn){
    sprintf(history, "    ------- parameters in hxdpi    -------   ");
  } else if(hxdgrade_yn){
    sprintf(history, "    ------- parameters in hxdgrade -------   ");
  } else {
    sprintf(history, "    ------- parameters -------   ");
  }
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  /** ------------------ read io mode --------------------- **/
  size = -1;
  BnkGet( "HXD:PIL:read_iomode",   
	  sizeof(char)*HXDFITSHEADER_LINESIZE,
	  &size, read_iomode);
  if (size<0) size=0;
  read_iomode[size]='\0';

  sprintf(history, "      read_iomode = '%s'", read_iomode);
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history read_iomode failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  BnkGet( "HXDeventFitsRead:IOMODE", sizeof(int), &size, &iomode);
  if (iomode == HXD_OVERWRITE) {
    /** ------------------ time change --------------------- **/
    BnkGet( "HXD:PIL:time_change",   sizeof(int),
	    &size, &time_change);
    if (time_change){
      sprintf(history, "      time_change = yes");
    } else {
      sprintf(history, "      time_change = no");
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history time_change failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ pi pmt change --------------------- **/
    BnkGet( "HXD:PIL:pi_pmt_change", sizeof(int),
	    &size, &pi_pmt_change);
    if (pi_pmt_change){
      sprintf(history, "      pi_pmt_change = yes");
    } else {
      sprintf(history, "      pi_pmt_change = no");
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history pi_pmt_change failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ pi pin change --------------------- **/
    BnkGet( "HXD:PIL:pi_pin_change", sizeof(int),
	    &size, &pi_pin_change);
    if (pi_pin_change){
      sprintf(history, "      pi_pin_change = yes");
    } else {
      sprintf(history, "      pi_pin_change = no");
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history pi_pin_change failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ grade change --------------------- **/
    BnkGet( "HXD:PIL:grade_change",  sizeof(int),
	    &size, &grade_change);
    if (grade_change){
      sprintf(history, "      grade_change = yes");
    } else {
      sprintf(history, "      grade_change = no");
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history grade_change failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

  } /** end of overwrite mode **/

  /** ------------------ gti mode --------------------- **/
  BnkGet( "HXD:PIL:gtimode",       sizeof(int),
	  &size, &gtimode);
  if(gtimode){
    sprintf(history, "      gtimode = yes");
  } else {
    sprintf(history, "      gtimode = no");
  }
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history gtimode failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  /** ------------------ gti time --------------------- **/
  if(gtimode){
    size = -1;
    BnkGet( "HXD:PIL:gti_time",      
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, gti_time);      
    if (size<0) size=0;
    gti_time[size]='\0';

    sprintf(history, "      gti_time = %s", gti_time);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history fgti_time ailed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }

  /** ------------------ input name --------------------- **/
  size = -1;
  BnkGet( "HXD:PIL:input_name",    
	  sizeof(char)*HXDFITSHEADER_LINESIZE,
	  &size, input_name);    
  if (size<0) size=0;
  input_name[size]='\0';

  sprintf(history, "      input_name = %s", input_name);
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history input_name failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  /** ------------------ create name --------------------- **/
  if (iomode == HXD_CREATE) {
    size = -1;
    BnkGet( "HXD:PIL:create_name",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, create_name);   
    if (size<0) size=0;
    create_name[size]='\0';

    sprintf(history, "      create_name = %s", create_name);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history create_name failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }

  /** ------------------ use PWH mode --------------------- **/
  BnkGet( "HXD:PIL:use_pwh_mode",  
	  sizeof(int), &size, &use_pwh_mode);  
  if (use_pwh_mode){
    sprintf(history, "      use_pwh_mode = yes");
  } else {
    sprintf(history, "      use_pwh_mode = no");
  }
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history use_pwh_mode failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  /** hxdtime:  time conv mode **/
  if(hxdtime_yn){
    /** ------------------ time conv mode --------------------- **/
    BnkGet( "HXD:PIL:time_convert_mode",  sizeof(int), 
	    &size, &time_convert_mode);
    sprintf(history, "      time_convert_mode = %d", time_convert_mode);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
    /** ------------------ tim file name  --------------------- **/
    size = -1;
    BnkGet( "HXD:PIL:tim_filename",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, tim_filename);
    if (size<0) size=0;
    tim_filename[size]='\0';
    
    sprintf(history, "      tim_filename = '%s'", tim_filename);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history tim_filename failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ leapsec name  --------------------- **/
    size = -1;
    BnkGet("HXD:PIL:leapsec_name", 
	   sizeof(char)*HXDFITSHEADER_LINESIZE, &size, leapsec_name);
    if (size<0) size=0;
    leapsec_name[size] = '\0';
    
    size = -1;
    BnkGet("HXD:PIL:CALDB_TYPE:LEAPSEC",sizeof(int), &size, 
	   &piltype_caldb_leapsec);
    if(piltype_caldb_leapsec){
      sprintf(history, "      leapsec_name = %s (CALDB)", leapsec_name);
    } else{
      sprintf(history, "      leapsec_name = %s", leapsec_name);
    }
    fits_write_history(fp, history, &istat);
  }

  /** mkgainhist **/
  if(hxdmkgainhist_yn){
    /** **/
  }

  /** hxdpi **/
  if(hxdpi_yn){
    /** ------------------ pin_gainhist_name  --------------------- **/
    size = -1;
    BnkGet("HXD:PIL:hxd_pinghf_fname", 
	   sizeof(char)*HXDFITSHEADER_LINESIZE,
	   &size, pin_gainhist_name);
    if (size<0) size=0;
    pin_gainhist_name[size] = '\0';

    size = -1;
    BnkGet("HXD:PIL:CALDB_TYPE:pinghf",sizeof(int), &size, 
	   &piltype_caldb_pinghf);
    if(piltype_caldb_pinghf){
      sprintf(history, "      hxd_pinghf_fname = %s (CALDB)", 
	      pin_gainhist_name);
    } else {
      sprintf(history, "      hxd_pinghf_fname = %s", pin_gainhist_name);
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, 
	      "fits_write_history pin_gainhist_name failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ gso_gainhist_name  --------------------- **/
    size = -1;
    BnkGet("HXD:PIL:hxd_gsoght_fname", 
	   sizeof(char)*HXDFITSHEADER_LINESIZE,
	   &size, gso_gainhist_name);
    if (size<0) size=0;
    gso_gainhist_name[size] = '\0';

    size = -1;
    BnkGet("HXD:PIL:CALDB_TYPE:gsoght",sizeof(int), &size, 
	   &piltype_caldb_gsoghf);
    if(piltype_caldb_gsoghf){
      sprintf(history, "      hxd_gsoght_fname = %s (CALDB)", 
	      gso_gainhist_name);
    } else {
      sprintf(history, "      hxd_gsoght_fname = %s", gso_gainhist_name);
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, 
	      "fits_write_history gso_gainhist_name failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ hxdgsolin_fname  --------------------- **/
    size = -1;
    BnkGet("HXD:PIL:hxd_gsolin_fname", 
	   sizeof(char)*HXDFITSHEADER_LINESIZE,
	   &size, hxdgsolin_fname);
    if (size<0) size=0;
    hxdgsolin_fname[size] = '\0';

    size = -1;
    BnkGet("HXD:PIL:CALDB_TYPE:gsolin",sizeof(int), &size, 
	   &piltype_caldb_gsolin);
    if(piltype_caldb_gsolin){
      sprintf(history, "      hxd_gsolin_fname = %s (CALDB)", hxdgsolin_fname);
    } else {
      sprintf(history, "      hxd_gsolin_fname = %s", hxdgsolin_fname);
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, 
	      "fits_write_history hxdgsolin_fname failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ hxdpinlin_fname  --------------------- **/
    size = -1;
    BnkGet("HXD:PIL:hxd_pinlin_fname", 
	   sizeof(char)*HXDFITSHEADER_LINESIZE,
	   &size, hxdpinlin_fname);
    if (size<0) size=0;
    hxdpinlin_fname[size] = '\0';

    size = -1;
    BnkGet("HXD:PIL:CALDB_TYPE:pinlin",sizeof(int), &size, 
	   &piltype_caldb_pinlin);
    if(piltype_caldb_pinlin){
      sprintf(history, "      hxd_pinlin_fname = %s (CALDB)", hxdpinlin_fname);
    } else {
      sprintf(history, "      hxd_pinlin_fname = %s", hxdpinlin_fname);
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, 
	      "fits_write_history hxdpinlin_fname failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

  }

  /** hxdgrade **/
  if(hxdgrade_yn){
    /** ------------------ grade psdsel fname  -------------------- **/
    size = -1;
    BnkGet("HXD:PIL:hxdgrade_psdsel_fname", 
	   sizeof(char)*HXDFITSHEADER_LINESIZE,
	   &size, hxdgrade_psdsel_fname);
    if (size<0) size=0;
    hxdgrade_psdsel_fname[size] = '\0';
    size = -1;
    BnkGet("HXD:PIL:CALDB_TYPE:psdsel",sizeof(int), &size, 
	   &piltype_caldb_psdsel);
    if(piltype_caldb_psdsel){
      sprintf(history, "      hxdgrade_psdsel_fname = %s (CALDB)", 
	      hxdgrade_psdsel_fname);
    } else {
      sprintf(history, "      hxdgrade_psdsel_fname = %s", 
	      hxdgrade_psdsel_fname);
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, 
	      "fits_write_history hxdgrade_psdsel_fname failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }


    size = -1;
    BnkGet("HXD:PIL:hxdgrade_pinthres_fname",
	   sizeof(char)*HXDFITSHEADER_LINESIZE,
	   &size, hxdgrade_pinthres_fname);
    if (size<0) size=0;
    hxdgrade_pinthres_fname[size] = '\0';
    size = -1;
    BnkGet("HXD:PIL:CALDB_TYPE:pinthr",sizeof(int), &size, 
	   &piltype_caldb_pinthr);
    if(piltype_caldb_pinthr){
      sprintf(history, "      hxdgrade_pinthres_fname = %s (CALDB)", 
	      hxdgrade_pinthres_fname);
    } else {
      sprintf(history, "      hxdgrade_pinthres_fname = %s", 
	      hxdgrade_pinthres_fname);
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, 
	      "fits_write_history hxdgrade_pinthres_fname failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }


  /** end **/
  sprintf(history, "    --------------------------------------   ");
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history hxdtime failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  /** write GSOOLDPI keyword to indicate that hxdpi_old was used **/
  if (hxdpi_yn) {
    fits_update_key_log(fp, "GSOOLDPI", 1,
                        "hxdpi_old used in creating this file?", &istat);
    if (istat) {
      fprintf(stderr, "fits_update_key_log(GSOOLDPI) failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }

  return stat;
}

int 
hxdFitsHeader_old_writeParamer_scl(fitsfile *fp, int extension_id){
  int stat = HXD_FITS_HEADER_UTIL_OK;
  int istat = 0;
  int size;
  char history     [HXDFITSHEADER_LINESIZE];
  int  hdutype;

  int hxdscltime_yn;
  char input_name  [HXDFITSHEADER_LINESIZE];
  int  time_convert_mode;
  char tim_filename     [HXDFITSHEADER_LINESIZE];
  
  /** ------------------ init --------------------- **/
  if (fits_movabs_hdu(fp, extension_id, &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",
	    pname, istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  BnkGet( "HXD:ftools:hxdscltime_yn", sizeof(int), &size, &hxdscltime_yn);
  if(hxdscltime_yn){
    sprintf(history, "    ------- parameters in hxdscltime  -------   ");
  } else {
    sprintf(history, "    ------- parameters -------   ");
  }
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history hxdtime failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG; 
    return stat;
  }

  /** ------------------ input_name --------------------- **/
  size = -1;
  BnkGet( "HXD:PIL:input_name", 
	  sizeof(char)*HXDFITSHEADER_LINESIZE,
	  &size, input_name);
  if (size<0) size=0;
  input_name[size]='\0';
  sprintf(history, "      input_name = '%s'", input_name);
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history input_name failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  if(hxdscltime_yn){
    /** ------------------ time conv mode --------------------- **/
    BnkGet( "HXD:PIL:time_convert_mode",  sizeof(int), 
	    &size, &time_convert_mode);
    sprintf(history, "      time_convert_mode = %d", time_convert_mode);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
    /** ------------------ tim file name  --------------------- **/
    size = -1;
    BnkGet( "HXD:PIL:tim_filename",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, tim_filename);
    if (size<0) size=0;
    tim_filename[size]='\0';
    
    sprintf(history, "      tim_filename = '%s'", tim_filename);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history tim_filename failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }

  /** end **/
  sprintf(history, "    --------------------------------------   ");
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history hxdtime failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }
  return stat;
}

int 
hxdFitsHeader_old_writeParamer_trn(fitsfile *fp, int extension_id){
  int stat = HXD_FITS_HEADER_UTIL_OK;
  int istat = 0;
  int size;
  char history     [HXDFITSHEADER_LINESIZE];
  int  hdutype;

  /**** paremeters in HXDtrnFitsRead ****/
  char read_iomode [HXDFITSHEADER_LINESIZE];
  int  time_change, quality_change, pi_change, gtimode;
  char gti_time    [HXDFITSHEADER_LINESIZE];
  char input_name  [HXDFITSHEADER_LINESIZE];
  char create_name [HXDFITSHEADER_LINESIZE];

  /**** paremeters in HXDftrnTime ****/
  int time_convert_mode;
  char tim_filename     [HXDFITSHEADER_LINESIZE];
  /**** paremeters in HXDtrnpi ****/
  char trn_bintbl_name  [HXDFITSHEADER_LINESIZE];
  char trn_gainhist_name[HXDFITSHEADER_LINESIZE];

  /** flow control **/
  int  iomode;
  int  hxdwamtime_yn, hxdmkwamgainhist_yn, hxdwampi_yn, hxdwamgrade_yn;
  enum{
    HXD_READONLY,
    HXD_OVERWRITE,
    HXD_CREATE
  };
  int  piltype_caldb_wampht;

  /** ------------------ init --------------------- **/
  if (fits_movabs_hdu(fp, extension_id, &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",
	    pname, istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  BnkGet( "HXD:ftools:hxdwamtime_yn",       sizeof(int), &size, 
	  &hxdwamtime_yn);
  BnkGet( "HXD:ftools:hxdmkwamgainhist_yn", sizeof(int), &size, 
	  &hxdmkwamgainhist_yn);
  BnkGet( "HXD:ftools:hxdwampi_yn",         sizeof(int), &size, 
	  &hxdwampi_yn);
  BnkGet( "HXD:ftools:hxdwamgrade_yn",      sizeof(int), &size, 
	  &hxdwamgrade_yn);
  if(hxdwamtime_yn){
    sprintf(history, "    ------- parameters in hxdwamtime  -------   ");
  } else if(hxdmkwamgainhist_yn){
    sprintf(history, "    ------- parameters in hxdmkwamgainhist -------   ");
  } else if(hxdwampi_yn){
    sprintf(history, "    ------- parameters in hxdwampi    -------   ");
  } else if(hxdwamgrade_yn){
    sprintf(history, "    ------- parameters in hxdwamgrade -------   ");
  } else {
    sprintf(history, "    ------- parameters -------   ");
  }
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  /** ------------------ read io mode --------------------- **/
  size = -1;
  BnkGet( "HXD:PIL:read_iomode",   
	  sizeof(char)*HXDFITSHEADER_LINESIZE,
	  &size, read_iomode);
  if (size<0) size=0;
  read_iomode[size]='\0';

  sprintf(history, "      read_iomode = '%s'", read_iomode);
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history read_iomode failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  BnkGet( "HXDtrnFitsRead:IOMODE", sizeof(int), &size, &iomode);
  if (iomode == HXD_OVERWRITE) {
    /** ------------------ time change --------------------- **/
    BnkGet( "HXD:PIL:time_change",   sizeof(int),
	    &size, &time_change);
    if (time_change){
      sprintf(history, "      time_change = yes");
    } else {
      sprintf(history, "      time_change = no");
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history time_change failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ pi change --------------------- **/
    BnkGet( "HXD:PIL:pi_change", sizeof(int),
	    &size, &pi_change);
    if (pi_change){
      sprintf(history, "      pi_change = yes");
    } else {
      sprintf(history, "      pi_change = no");
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history pi_change failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ grade change --------------------- **/
    BnkGet( "HXD:PIL:quality_change",  sizeof(int),
	    &size, &quality_change);
    if (quality_change){
      sprintf(history, "      quality_change = yes");
    } else {
      sprintf(history, "      quality_change = no");
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history quality_change failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  } /** end of overwrite mode **/

  /** ------------------ gti mode --------------------- **/
  BnkGet( "HXD:PIL:gtimode",       sizeof(int),
	  &size, &gtimode);
  if(gtimode){
    sprintf(history, "      gtimode = yes");
  } else {
    sprintf(history, "      gtimode = no");
  }
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history gtimode failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  /** ------------------ gti time --------------------- **/
  if(gtimode){
    size = -1;
    BnkGet( "HXD:PIL:gti_time",      
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, gti_time);      
    if (size<0) size=0;
    gti_time[size]='\0';

    sprintf(history, "      gti_time = %s", gti_time);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history fgti_time ailed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }

  /** ------------------ input name --------------------- **/
  size = -1;
  BnkGet( "HXD:PIL:input_name",    
	  sizeof(char)*HXDFITSHEADER_LINESIZE,
	  &size, input_name);    
  if (size<0) size=0;
  input_name[size]='\0';

  sprintf(history, "      input_name = %s", input_name);
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history input_name failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  /** ------------------ create name --------------------- **/
  if (iomode == HXD_CREATE) {
    size = -1;
    BnkGet( "HXD:PIL:create_name",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, create_name);   
    if (size<0) size=0;
    create_name[size]='\0';

    sprintf(history, "      create_name = %s", create_name);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history create_name failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }

  /** hxdwamtime **/
  if(hxdwamtime_yn){
    /** ------------------ time conv mode --------------------- **/
    BnkGet( "HXD:PIL:time_convert_mode",   sizeof(int),
	    &size, &time_convert_mode);
    sprintf(history, "      time_convert_mode = %d", time_convert_mode);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history time_convert_mode failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
    /** ------------------ tim file name  --------------------- **/
    size = -1;
    BnkGet( "HXD:PIL:tim_filename",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, tim_filename);
    if (size<0) size=0;
    tim_filename[size]='\0';
    
    sprintf(history, "      tim_filename = '%s'", tim_filename);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history tim_filename failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }

  /** hxdmkwamgainhist **/
  if(hxdmkwamgainhist_yn){
    /* do nothing */
  }

  /** hxdwampi **/
  if(hxdwampi_yn){
    /** ------------------ tim file name  --------------------- **/
    size = -1;
    BnkGet( "HXD:PIL:trn_bintbl_name",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, trn_bintbl_name);
    if (size<0) size=0;
    trn_bintbl_name[size]='\0';
    
    BnkGet("HXD:PIL:CALDB_TYPE:wampht",sizeof(int), &size, 
	   &piltype_caldb_wampht);
    if(piltype_caldb_wampht){
      sprintf(history, "      trn_bintbl_name = '%s' (CALDB)", 
	      trn_bintbl_name);
    } else {
      sprintf(history, "      trn_bintbl_name = '%s'", trn_bintbl_name);
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history trn_bintbl_name failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ tim file name  --------------------- **/
    size = -1;
    BnkGet( "HXD:PIL:trn_gainhist_name",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, trn_gainhist_name);
    if (size<0) size=0;
    trn_gainhist_name[size]='\0';
    
    sprintf(history, "      trn_gainhist_name = '%s'",trn_gainhist_name);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history trn_gainhist_name failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }
  /** hxdwamgrade **/
  if(hxdwamgrade_yn){
    /* do nothing */
  }

  /** end **/
  sprintf(history, "    --------------------------------------   ");
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history hxdtime failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  return stat;
}

int 
hxdFitsHeader_old_writeParamer_bst(fitsfile *fp, int extension_id){
  int stat = HXD_FITS_HEADER_UTIL_OK;
  int istat = 0;
  int size;
  char history     [HXDFITSHEADER_LINESIZE];
  int  hdutype;

  /**** paremeters in HXDbstFitsRead ****/
  char read_iomode [HXDFITSHEADER_LINESIZE];
  char input_name  [HXDFITSHEADER_LINESIZE];
  char create_name [HXDFITSHEADER_LINESIZE];

  /**** paremeters in HXDfbstTime ****/
  int  time_convert_mode;
  char tim_filename     [HXDFITSHEADER_LINESIZE];
  char bstidt_filename  [HXDFITSHEADER_LINESIZE];

  int piltype_caldb_bstidt;

  /** flow control **/
  int  iomode;
  int hxdbsttime_yn;
  enum{
    HXD_READONLY,
    HXD_OVERWRITE,
    HXD_CREATE
  };


  /** ------------------ init --------------------- **/
  if (fits_movabs_hdu(fp, extension_id, &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",
	    pname, istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  BnkGet( "HXD:ftools:hxdbsttime_yn",       sizeof(int), &size, 
	  &hxdbsttime_yn);
  if(hxdbsttime_yn){
    sprintf(history, "    ------- parameters in hxdbsttime  -------   ");
  } else {
    sprintf(history, "    ------- parameters -------   ");
  }
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  /** ------------------ read io mode --------------------- **/
  size = -1;
  BnkGet( "HXD:PIL:read_iomode",   
	  sizeof(char)*HXDFITSHEADER_LINESIZE,
	  &size, read_iomode);
  if (size<0) size=0;
  read_iomode[size]='\0';

  sprintf(history, "      read_iomode = '%s'", read_iomode);
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history read_iomode failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }
  /** ------------------ input name --------------------- **/
  size = -1;
  BnkGet( "HXD:PIL:input_name",    
	  sizeof(char)*HXDFITSHEADER_LINESIZE,
	  &size, input_name);    
  if (size<0) size=0;
  input_name[size]='\0';

  sprintf(history, "      input_name = %s", input_name);
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history input_name failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }

  BnkGet( "HXDbstFitsRead:IOMODE", sizeof(int), &size, &iomode);
  /** ------------------ create name --------------------- **/
  if (iomode == HXD_CREATE) {
    size = -1;
    BnkGet( "HXD:PIL:create_name",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, create_name);   
    if (size<0) size=0;
    create_name[size]='\0';

    sprintf(history, "      create_name = %s", create_name);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history create_name failed (%d)\n", istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }
  }

  if(hxdbsttime_yn){
    /** ------------------ time conv mode --------------------- **/
    BnkGet( "HXD:PIL:time_convert_mode",   sizeof(int),
	    &size, &time_convert_mode);
    sprintf(history, "      time_convert_mode = %d", time_convert_mode);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history time_convert_mode failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ tim file name  --------------------- **/
    size = -1;
    BnkGet( "HXD:PIL:tim_filename",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, tim_filename);
    if (size<0) size=0;
    tim_filename[size]='\0';
    
    sprintf(history, "      tim_filename = '%s'", tim_filename);
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history tim_filename failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

    /** ------------------ bstidt name  --------------------- **/
    size = -1;
    BnkGet( "HXD:PIL:bstidt_filename",   
	    sizeof(char)*HXDFITSHEADER_LINESIZE,
	    &size, bstidt_filename);
    if (size<0) size=0;
    bstidt_filename[size]='\0';
    
    BnkGet("HXD:PIL:CALDB_TYPE:bstidt",sizeof(int), &size, 
	   &piltype_caldb_bstidt);
    if(piltype_caldb_bstidt){
      sprintf(history, "      bstidt_filename = '%s'(CALDB)", bstidt_filename);
    } else {
      sprintf(history, "      bstidt_filename = '%s'", bstidt_filename);
    }
    fits_write_history(fp, history, &istat);
    if(istat) {
      fprintf(stderr, "fits_write_history bstidt_filename failed (%d)\n",
	      istat);
      stat = HXD_FITS_HEADER_UTIL_NG;
      return stat;
    }

  }

  /** end **/
  sprintf(history, "    --------------------------------------   ");
  fits_write_history(fp, history, &istat);
  if(istat) {
    fprintf(stderr, "fits_write_history hxdtime failed (%d)\n", istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }
  return stat;

}

int 
hxdFitsHeader_old_writeParamer_arf(fitsfile *fp, int extension_id){
  int stat = HXD_FITS_HEADER_UTIL_OK;
  int istat = 0;
  int size;
  char history     [HXDFITSHEADER_LINESIZE];
  int  hdutype;
  /** ------------------ init --------------------- **/
  if (fits_movabs_hdu(fp, extension_id, &hdutype, &istat)) {
    fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",
	    pname, istat);
    stat = HXD_FITS_HEADER_UTIL_NG;
    return stat;
  }
  
  return stat;
 
}
