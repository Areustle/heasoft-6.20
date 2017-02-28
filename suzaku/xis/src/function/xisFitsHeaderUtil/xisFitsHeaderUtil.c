/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:55 1999 by E. Miyata*/
/**************************************/
/***************************************
  xisFitsHeaderUtil
	ver1.0 created by Ping-san

	ver1.1   1999.09.01    Emi Miyata
	replaced APID with AEID

	ver1.2   1999.11.14    Emi Miyata
	modify HDCLAS[1-2] keywords as GSFC's request
	add OPTIC** keywords as GSFC's request

	ver1.3   2000.01.28    Emi Miyata
	modify ORIGIN (to use AEpacketRPT 2.3)

	ver1.5   2000.02.04    Emi Miyata
	skip error in writing tlm_file

	ver1.6   2000.02.08    Emi Miyata
	fix XRS -> XIS in header file :-)
	fix defaults -> default in switch :-)
	add aste_time.h

	ver1.7   2000.04.28    Emi Miyata
	support XISDL

	ver 1.8  2004.02.05    Hironori Matsumoto
	Change the keyword of TIMESYSTEM
	   delete MJDREF
	   add    MJDREFI, MJDREFF
	   change TIMESYS

        ver 2.0 2004.03.14  Hironori Matsumoto
        AtTime ---> AtTimeD
        aste2attime ---> aste2attimeD

	ver 2.1 2004.09.15 Hironori Matsumoto
	telescope を ASTRO-E から Astro-E2 へ

	ver2.2 2005.6.8 H. Matsumoto
	BNK AEpacketRPTread:FILE_NAME から BNK ASTE:RPT:IFILE_NAME:PTR への変更に対応。
	MJDREFI, MJDREFF の数値、コメントを変更

	ver2.3 2005.6.20 H. Matsumoto
	TELESCOP 名を char *aste_telescop(void); を利用して作成
	keyword のコメントを aeFitsHeaderUtil.c を参考に変更
	構造体 (xisFitsHeaderUtil.h) XIS_STD_KEYS のフィールド名変更に対応
	setDefaultKeywordValues() でデフォルトのキーワード名まで入るように変更。

        ver 2.4 2005/07/08 Hironori Matsumoto
        xisFitsHeaderUtil.h のありかをこのディレクトリにする

        ver 2.5 2005/10/19 Hironori Matsumoto
	standard keyword の追加、順序変更
	TSCAL で始まるキーワードの除去関数追加

        ver 2.6 2005/10/31 Y.ISHISAKI
	stop using strdup()
        set TIM_FILE from BNK "ASTE:TIM_FILE:PTR"
        use basename() for TLM_FILE, TIM_FILE, LEAPFILE
        blank keywords for RA/DEC_OBJ/PNT/NOM, PA_NOM, MEAN_EA[1-3]

        ver 2.7 2006/08/25 Y.ISHISAKI
	basename() -> aefits_basename()

        ver2.8 2006/09/15 Hironori Matsumoto
        updateStdTimeKeys で ONTIME への書き込みに、stdkeys.ontime を使用。

        ver2.9 2007/04/23 Hironori Matsumoto
        header keyword NOM_PNT を追加。
        コメントはaeaspectで書き込まれるので、ここでは何も書かない。
*************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_coord.h" /* for aste_telescop */
#include "xisEventFitsUtil.h"
#include "aeFitsHeaderUtil.h"

#if defined(__XISQL__) || defined(__XISREAD__)
#else
#include "xisDESim.h"
#endif

#define _XIS_FITS_HEADER_UTIL_FUNC_
#define OPTIC_DEFAULT              -999.0

#define pname "xisFitsHeaderUtil"


/* STD HDU header keyword */
void setDefaultKeywordValues(XIS_STD_KEYS *stdkeys)
{
  /* aste_telescop 関数を使っておけば、打ち上げ後の名称変更にもスムー
     ズに対応できる */
  stdkeys->telescop=aste_telescop();
  stdkeys->telescop_comment="telescope (mission) name";

  stdkeys->instrume="XIS";
  stdkeys->instrume_comment="instrument name";

  stdkeys->obs_mode="     ";
  stdkeys->obs_mode_comment="observation mode (e.g. POINTING/SLEW)";

  stdkeys->datamode="STANDARD";
  stdkeys->datamode_comment="datamode";

  stdkeys->xis_aeid="     ";
  stdkeys->xis_aeid_comment="ID of Analog Electronics of XIS";

  stdkeys->editmode="     ";
  stdkeys->editmode_comment="how detected events are edited by the DE";

  stdkeys->clk_mode="     ";
  stdkeys->clk_mode_comment="how the CCD clocks are driven by the AE";

  stdkeys->obsid="       ";
  stdkeys->obsid_comment="Observation Identifier";

  stdkeys->observer="       ";
  stdkeys->observer_comment="Principal Investigator";

  stdkeys->object="       ";
  stdkeys->object_comment="name of observed object";

  stdkeys->obsrem="       ";
  stdkeys->obsrem_comment="remark on observation";

  stdkeys->ra_obj=0.0;
  stdkeys->ra_obj_comment="planned target R.A.(deg)";

  stdkeys->dec_obj=0.0;
  stdkeys->dec_obj_comment="planned target DEC.(deg)";

  stdkeys->ra_pnt=0.0;
  stdkeys->ra_pnt_comment="average optical axis location R.A.(deg)";

  stdkeys->dec_pnt=0.0;
  stdkeys->dec_pnt_comment="average optical axis location DEC.(deg)";

  stdkeys->ra_nom=0.0;
  stdkeys->ra_nom_comment="nominal satellite pointing direction R.A.(deg)";

  stdkeys->dec_nom=0.0;
  stdkeys->dec_nom_comment="nominal satellite pointing direction DEC.(deg)";

  stdkeys->pa_nom   = 0.0;
  stdkeys->pa_nom_comment="nominal position angle from north to DETY(deg)";

  stdkeys->mean_ea1 = 0.0;
  stdkeys->mean_ea1_comment = "mean of the 1st ZYZ-Euler angle (deg)";

  stdkeys->mean_ea2 = 0.0;
  stdkeys->mean_ea2_comment = "mean of the 2nd ZYZ-Euler angle (deg)";

  stdkeys->mean_ea3 = 0.0;
  stdkeys->mean_ea3_comment = "mean of the 3rd ZYZ-Euler angle (deg)";

  stdkeys->radecsys="FK5";
  stdkeys->radecsys_comment="World Coordinate System";

  stdkeys->equinox=2000;
  stdkeys->equinox_comment="equinox for coordinate system";

  stdkeys->nom_pnt="       ";
  stdkeys->nom_pnt_comment="  ";

  stdkeys->date_obs="       ";
  stdkeys->date_obs_comment="start date of observations (UT)";

  stdkeys->time_obs="       ";
  stdkeys->time_obs_comment="start time of observations (UT)";

  stdkeys->date_end="       ";
  stdkeys->date_end_comment="end date of observations (UT)";

  stdkeys->time_end="       ";
  stdkeys->time_end_comment="end time of observations (UT)";

  stdkeys->tstart=0.0;
  stdkeys->tstart_comment="time start";

  stdkeys->tstop=0.0;
  stdkeys->tstop_comment="time stop";

  stdkeys->telapse=0.0;
  stdkeys->telapse_comment="elapsed time = TSTOP - TSTART";

  stdkeys->ontime=0.0;
  stdkeys->ontime_comment="on time = sum of all GTIs";

  stdkeys->timesys="TT";
  stdkeys->timesys_comment="time system (TT:Terrestrial Time)";

  stdkeys->mjdrefi=51544;
  stdkeys->mjdrefi_comment="integer part of the MJD reference (2000.0 UT)";

  stdkeys->mjdreff=0.00074287037037037;
  stdkeys->mjdreff_comment="fractional part of the MJD reference (64.184 s)";

  stdkeys->timeref="LOCAL";
  stdkeys->timeref_comment="LOCAL: barycentric correction not applied";

  stdkeys->timeunit="s";
  stdkeys->timeunit_comment="unit for the time related keywords";

  stdkeys->tassign="SATELLITE";
  stdkeys->tassign_comment="SATELLITE: times assigned on satellite";

  stdkeys->clockapp=1;
  stdkeys->clockapp_comment="clock correction applied or not";

  stdkeys->timedel=8.0;
  stdkeys->timedel_comment="finest time resolution (time between frames)";

  stdkeys->timepixr=0.5;
  stdkeys->timepixr_comment="0:times refer to beginning of bin, 0.5:mid";

  stdkeys->tierrela=0.0;
  stdkeys->tierrela_comment="short-term clock stability";

  stdkeys->tierabso=0.0;
  stdkeys->tierabso_comment="absolute precision of the clock";

  stdkeys->hduclass="OGIP";
  stdkeys->hduclass_comment="format conforms to OGIP/GSFC conventions";

  stdkeys->hduclas1="     ";
  stdkeys->hduclas1_comment="type of data (e.g. EVENTS/TEMPORALDATA)";

  stdkeys->hduclas2="     ";
  stdkeys->hduclas2_comment="photon event list, includes all photons";

  stdkeys->tlm_file="       ";
  stdkeys->tlm_file_comment="name of input telemetry file";

  stdkeys->tim_file="       ";
  stdkeys->tim_file_comment="name of the time assignment file";

  stdkeys->att_file="       ";
  stdkeys->att_file_comment="name of the satellite attitude file";

  stdkeys->orb_file="       ";
  stdkeys->orb_file_comment="name of the satellite orbit file";

  stdkeys->leap_file="       ";
  stdkeys->leap_file_comment="name of the leap second file";

  stdkeys->teldef="       ";
  stdkeys->teldef_comment="name of the telescope definition file";

  stdkeys->creator="       ";
  stdkeys->creator_comment="software that created this file";

  stdkeys->origin="       ";
  stdkeys->origin_comment="origin of FITS file";



#if defined(__XISQL__) || defined(__XISREAD__)

  {
    int size;
    static char str[FLEN_VALUE];
    BnkfGetM("ASTE:FFF_ORIGIN",sizeof(str)-1,&size,str);
    if (str == NULL || size == 0) {
      stdkeys->origin="ISAS/JAXA";
    } else {
      str[size] = '\0';
      stdkeys->origin=str;
    }
  }

  {
    int size;
    char *rptname = NULL;
    BnkfGetM ("ASTE:RPT:IFILE_NAME:PTR", sizeof(rptname), &size, &rptname);
    if ( NULL == rptname || '\0' == *rptname ) {
      stdkeys->tlm_file = "       ";
    } else {
      stdkeys->tlm_file = aefits_basename(rptname);
    }
    stdkeys->mjdref=51544;	/* MJDREF: this entry is obsolate, but remains for backword compatibility */

  }

  {
    int size;
    char *tim_file = NULL;
    BnkfGetM ("ASTE:TIM_FILE:PTR", sizeof(tim_file), &size, &tim_file);
    if ( NULL == tim_file || '\0' == *tim_file ) {
      stdkeys->tim_file = "       ";
    } else {
      stdkeys->tim_file = aefits_basename(tim_file);
    }
  }

  {
    int index;
    int size;
    static char str[1024];
    if (BnkKey("ASTE:LEAPSEC_FILE", &index) == ANL_OK) {
      BnkfGetM("ASTE:LEAPSEC_FILE", sizeof(str)-1, &size, str);
      if (str == NULL || size == 0) {
	stdkeys->leap_file="     ";
      } else {
	str[size] = '\0';
	stdkeys->leap_file=aefits_basename(str);
      }
    }
  }
#else
  {
    int size;
    XISsimSetup  *simSetup;
    BnkGet("XIS:SIM_SETUP:PTR", sizeof(XISsimSetup *), &size, &simSetup);
    stdkeys->tlm_file = simSetup->fits_fname;
  }
  stdkeys->origin="OSAKA";
#endif
}






/* write common keywords:
 * these are common keywords not only for for primary HDU but also
 * for event and other xtentions
 */
int writeXISStdKeys (FITS_EXT_STRUCT * ext,
		     XIS_STD_KEYS stdkeys,
		     int hdunum,
		     int *status)
{
  int hdutype;
  int fitsIsInitiallyClosed=0;  /* 1: closed --> close it at the end;   *
			         * 0: open --> leave it open at the end */

  fitsfile * fp = ext->fitsd;
  if (fp == (fitsfile *)NULL) {
    if (fits_open_file (&fp, ext->filename, READWRITE, status)){
      fprintf(stderr, "%s: fits_open_file failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
    fitsIsInitiallyClosed=1;
  }

  /* move to the specified xtention unless hdunum=0 */
  if (hdunum != 0) {
    if (fits_movabs_hdu(fp, hdunum, &hdutype, status)) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
  }


  if (fits_write_key_str(fp, "TELESCOP", stdkeys.telescop,
			 stdkeys.telescop_comment, status)) {
    fprintf(stderr,"Error in writing keyword: TELESCOP (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "INSTRUME", stdkeys.instrume,
			 stdkeys.instrume_comment, status)) {
    fprintf(stderr,"Error in writing keyword: INSTRUME (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "OBS_MODE", stdkeys.obs_mode,
			 stdkeys.obs_mode_comment, status)) {
    fprintf(stderr,"Error in writing keyword: OBS_MODE (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "DATAMODE", stdkeys.datamode,
			 stdkeys.datamode_comment, status)) {
    fprintf(stderr,"Error in writing keyword: DATAMODE (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "XIS-AEID", stdkeys.xis_aeid,
			 stdkeys.xis_aeid_comment, status)) {
    fprintf(stderr,"Error in writing keyword: XIS-AEID (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp,"EDITMODE", stdkeys.editmode,
			 stdkeys.editmode_comment,  status)) {
    fprintf(stderr,"Error in writing keyword: EDITMODE (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp,"CLK_MODE", stdkeys.clk_mode,
			 stdkeys.clk_mode_comment,  status)) {
    fprintf(stderr,"Error in writing keyword: CLK_MODE (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "OBS_ID", stdkeys.obsid,
			 stdkeys.obsid_comment, status)) {
    fprintf(stderr,"Error in writing keyword: OBS_ID (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "OBSERVER", stdkeys.observer,
			 stdkeys.observer_comment, status)){
    fprintf(stderr,"Error in writing keyword: OBSERVER (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "OBJECT", stdkeys.object,
			 stdkeys.object_comment, status)) {
    fprintf(stderr,"Error in writing keyword: OBJECT (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "OBS_REM", stdkeys.obsrem,
			 stdkeys.obsrem_comment, status)) {
    fprintf(stderr,"Error in writing keyword: OBS_REM (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "RA_OBJ", stdkeys.ra_obj_comment, status)) {
    fprintf(stderr, "Error in writing keyword: RA_OBJ (status=%d)\n", *status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "DEC_OBJ", stdkeys.dec_obj_comment, status)) {
    fprintf(stderr,"Error in writing keyword: DEC_OBJ (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "RA_PNT", stdkeys.ra_pnt_comment, status)) {
    fprintf(stderr, "Error in writing keyword: RA_PNT (status=%d)\n", *status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "DEC_PNT", stdkeys.dec_pnt_comment, status)){
    fprintf(stderr,"Error in writing keyword: DEC_PNT (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "RA_NOM", stdkeys.ra_nom_comment, status)) {
    fprintf(stderr, "Error in writing keyword: RA_NOM (status=%d)\n", *status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "DEC_NOM",  stdkeys.dec_nom_comment, status)){
    fprintf(stderr,"Error in writing keyword: DEC_NOM (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "PA_NOM", stdkeys.pa_nom_comment, status)){
    fprintf(stderr,"Error in writing keyword: PA_NOM (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "MEAN_EA1", stdkeys.mean_ea1_comment, status)){
    fprintf(stderr,"Error in writing keyword: MEAN_EA1 (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "MEAN_EA2", stdkeys.mean_ea2_comment, status)){
    fprintf(stderr,"Error in writing keyword: MEAN_EA2 (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_null(fp, "MEAN_EA3", stdkeys.mean_ea3_comment, status)){
    fprintf(stderr,"Error in writing keyword: MEAN_EA3 (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "RADECSYS", stdkeys.radecsys,
			 stdkeys.radecsys_comment, status)){
    fprintf(stderr,"Error in writing keyword: RADECSYS (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_lng(fp, "EQUINOX",  stdkeys.equinox,
			 stdkeys.equinox_comment, status)) {
    fprintf(stderr,"Error in writing keyword: EQUINOX (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "NOM_PNT", stdkeys.nom_pnt,
			 stdkeys.nom_pnt_comment, status)){
    fprintf(stderr,"Error in writing keyword: NOM_PNT (status=%d)\n",*status);
    return ANL_FALSE;
  }


  if (fits_write_key_str(fp, "DATE-OBS", stdkeys.date_obs,
			 stdkeys.date_obs_comment, status)){
    fprintf(stderr,"Error in writing keyword: DATE-OBS (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "TIME-OBS", stdkeys.time_obs,
			 stdkeys.time_obs_comment, status)){
    fprintf(stderr,"Error in writing keyword: TIME-OBS (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "DATE-END", stdkeys.date_end,
			 stdkeys.date_end_comment, status)) {
    fprintf(stderr,"Error in writing keyword: DATE-END (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "TIME-END", stdkeys.time_end,
			 stdkeys.time_end_comment, status)){
    fprintf(stderr,"Error in writing keyword: TIME-END (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_fixdbl(fp, "TSTART",   stdkeys.tstart, 6,
			    stdkeys.tstart_comment, status)){
    fprintf(stderr,"Error in writing keyword: TSTART (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_fixdbl(fp, "TSTOP",    stdkeys.tstop, 6,
			    stdkeys.tstop_comment, status)){
    fprintf(stderr,"Error in writing keyword: TSTOP (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_fixdbl(fp, "TELAPSE",   stdkeys.telapse, 6,
			    stdkeys.telapse_comment, status)){
    fprintf(stderr,"Error in writing keyword: TELAPSE (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_fixdbl(fp, "ONTIME",   stdkeys.ontime, 6,
			    stdkeys.ontime_comment, status)){
    fprintf(stderr,"Error in writing keyword: ONTIME (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "TIMESYS",  stdkeys.timesys,
			 stdkeys.timesys_comment, status)){
    fprintf(stderr,"Error in writing keyword: TIMESYS (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_lng(fp, "MJDREFI",   stdkeys.mjdrefi,
			 stdkeys.mjdrefi_comment, status)){
    fprintf(stderr,"Error in writing keyword: MJDREFI (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_fixdbl(fp, "MJDREFF",   stdkeys.mjdreff, 17,
			    stdkeys.mjdreff_comment, status)){
    fprintf(stderr,"Error in writing keyword: MJDREFF (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "TIMEREF",  stdkeys.timeref,
			 stdkeys.timeref_comment, status)){
    fprintf(stderr,"Error in writing keyword: TIMEREF (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "TIMEUNIT", stdkeys.timeunit,
			 stdkeys.timeunit_comment, status)){
    fprintf(stderr,"Error in writing keyword: TIMEUNIT (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "TASSIGN",  stdkeys.tassign,
			 stdkeys.tassign_comment, status)) {
    fprintf(stderr,"Error in writing keyword: TASSIGN (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_log(fp, "CLOCKAPP", stdkeys.clockapp,
			 stdkeys.clockapp_comment, status)){
    fprintf(stderr,"Error in writing keyword: OBJECT (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_dbl(fp, "TIMEDEL",  stdkeys.timedel, 5,
			 stdkeys.timedel_comment, status)){
    fprintf(stderr,"Error in writing keyword: TIMEDEL (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_fixdbl(fp, "TIMEPIXR", stdkeys.timepixr, 1,
			    stdkeys.timepixr_comment, status)){
    fprintf(stderr,"Error in writing keyword: TIMEPIXR (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_dbl(fp, "TIERRELA", stdkeys.tierrela, 1,
			 stdkeys.tierrela_comment, status)){
    fprintf(stderr,"Error in writing keyword: TIERRELA (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_dbl(fp, "TIERABSO", stdkeys.tierabso, 1,
			 stdkeys.tierabso_comment, status)){
    fprintf(stderr,"Error in writing keyword: TIERABSO (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "HDUCLASS", stdkeys.hduclass,
			 stdkeys.hduclass_comment, status)){
    fprintf(stderr,"Error in writing keyword: HDUCLASS (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "HDUCLAS1", stdkeys.hduclas1,
			 stdkeys.hduclas1_comment, status)){
    fprintf(stderr,"Error in writing keyword: HDUCLAS1 (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "HDUCLAS2", stdkeys.hduclas2,
			 stdkeys.hduclas2_comment, status)){
    fprintf(stderr,"Error in writing keyword: HDUCLAS2 (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "TLM_FILE", stdkeys.tlm_file,
			 stdkeys.tlm_file_comment, status)) {
    fprintf(stderr,"Error in writing keyword: TLM_FILE (%s,status=%d)\n",
	    stdkeys.tlm_file, *status);
    *status=0;
    /*    return ANL_FALSE;*/
  }

  if (fits_write_key_str(fp, "TIM_FILE", stdkeys.tim_file,
			 stdkeys.tim_file_comment, status)) {
    fprintf(stderr,"Error in writing keyword: TIM_FILE (%s,status=%d)\n",
	    stdkeys.tlm_file, *status);
  }

  if (fits_write_key_str(fp, "ATT_FILE", stdkeys.att_file,
			 stdkeys.att_file_comment, status)) {
    fprintf(stderr,"Error in writing keyword: ATT_FILE (%s,status=%d)\n",
	    stdkeys.tlm_file, *status);
  }

  if (fits_write_key_str(fp, "ORB_FILE", stdkeys.orb_file,
			 stdkeys.orb_file_comment, status)) {
    fprintf(stderr,"Error in writing keyword: ORB_FILE (%s,status=%d)\n",
	    stdkeys.tlm_file, *status);
  }

  if (fits_write_key_str(fp, "LEAPFILE", stdkeys.leap_file,
			 stdkeys.leap_file_comment, status)) {
    fprintf(stderr,"Error in writing keyword: LEAP_FILE (%s,status=%d)\n",
	    stdkeys.leap_file, *status);
  }

  if (fits_write_key_str(fp, "TELDEF", stdkeys.teldef,
			 stdkeys.teldef_comment, status)) {
    fprintf(stderr,"Error in writing keyword: TELDEF (%s,status=%d)\n",
	    stdkeys.teldef, *status);
  }

  if (fits_write_key_str(fp, "CREATOR",  stdkeys.creator,
			 stdkeys.creator_comment, status)){
    fprintf(stderr,"Error in writing keyword: CREATOR (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp, "ORIGIN",   stdkeys.origin,
			 stdkeys.origin_comment, status)){
    fprintf(stderr,"Error in writing keyword: ORIGIN (status=%d)\n",*status);
    return ANL_FALSE;
  }

  if (fitsIsInitiallyClosed) {
    if (fits_close_file(fp, status)) {
      fprintf(stderr,"%s: Error closing %s (%d)\n",pname, ext->filename, *status);
      return ANL_FALSE;
    }
  }

  return ANL_TRUE;
}







int updateStdTimeKeys (FITS_EXT_STRUCT * ext,
		       XIS_STD_KEYS stdkeys,
		       int hdunum,
		       int *status)
{

  int hdutype;
  char comment[] = "&";
  int fitsIsInitiallyClosed=0;  /* 1: closed --> close it at the end;   *
			         * 0: open --> leave it open at the end */

  char date_obs[11], date_end[11], time_obs[9], time_end[9];
  AtTimeD attime;

  fitsfile * fp = ext->fitsd;

  /* calculate date and time */
  aste2attimeD(stdkeys.tstart, &attime);
  sprintf(time_obs,"%02d:%02d:%02d",attime.hr, attime.mn, attime.sc);
  sprintf(date_obs,"%04d-%02d-%02d",attime.yr, attime.mo, attime.dy);

  aste2attimeD(stdkeys.tstop, &attime);
  sprintf(time_end,"%02d:%02d:%02d",attime.hr, attime.mn, attime.sc);
  sprintf(date_end,"%04d-%02d-%02d",attime.yr, attime.mo, attime.dy);

  stdkeys.telapse=stdkeys.tstop-stdkeys.tstart;

  /* if ontime if not caululated, set it to telapse;
   * this is true if there is only one GTI;
   * if more than one GTI, ontime should be calculated before calling this func(),
   * or it should be updated at the end
   */
  if ((int)stdkeys.ontime == 0) stdkeys.ontime=stdkeys.telapse;

  if (fp == (fitsfile *)NULL) {
    if (fits_open_file (&fp, ext->filename, READWRITE, status)){
      fprintf(stderr, "%s: fits_open_file failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
    fitsIsInitiallyClosed=1;
  }

  /* move to the specified xtention unless hdunum=0 */
  if (hdunum != 0) {
    if (fits_movabs_hdu(fp, hdunum, &hdutype, status)) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
  }

  if (fits_update_key_str(fp, "DATE-OBS", date_obs, comment, status)) {
    fprintf(stderr, "%s: fits_update_key_str DATE-OBS failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }

  if (fits_update_key_str(fp, "TIME-OBS", time_obs, comment, status)) {
    fprintf(stderr, "%s: fits_update_key_str TIME-OBS failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }

  if (fits_update_key_str(fp, "DATE-END", date_end, comment, status)) {
    fprintf(stderr, "%s: fits_update_key_str DATE-END failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }

  if (fits_update_key_str(fp, "TIME-END", time_end, comment, status)) {
    fprintf(stderr, "%s: fits_update_key_str TIME-END failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }

  if (fits_update_key_fixdbl(fp, "TSTART", stdkeys.tstart, 6, comment, status)){
    fprintf(stderr, "%s: ffmkys TSTART failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }
  if (fits_update_key_fixdbl(fp, "TSTOP", stdkeys.tstop, 6, comment, status)){
    fprintf(stderr, "%s: ffmkys TSTOP failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }
  if (fits_update_key_fixdbl(fp, "TELAPSE", stdkeys.telapse, 6, comment, status)){
    fprintf(stderr, "%s: ffmkys TELAPSE failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }
  if (fits_update_key_fixdbl(fp, "ONTIME", stdkeys.ontime, 6, comment, status)){
    fprintf(stderr, "%s: ffmkys ONTIME failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }

  if (fitsIsInitiallyClosed) {
    if (fits_close_file(fp, status)) {
      fprintf(stderr,"%s: Error closing %s (%d)\n",pname, ext->filename, *status);
      return ANL_FALSE;
    }
  }

  return ANL_TRUE;
}







int updateHduclas(FITS_EXT_STRUCT *ext,
		  int hdunum,
		  int extID,
		  int *status)
{

  int hdutype;
  char hduclas1[20], comment[73];
  int fitsIsInitiallyClosed=0;  /* 1: closed -- close it at the end;   *
			         * 0: open -- leave it open at the end */

  fitsfile * fp = ext->fitsd;

  if (fp == (fitsfile *)NULL) {
    if (fits_open_file (&fp, ext->filename, READWRITE, status)){
      fprintf(stderr, "%s: fits_open_file failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
    fitsIsInitiallyClosed=1;
  }

  /* move to the specified xtention unless hdunum=0 */
  if (hdunum != 0) {
    if (fits_movabs_hdu(fp, hdunum, &hdutype, status)) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
  }

  switch(extID) {
  case EVENT_EXTENSION_ID:
    strcpy(hduclas1,"EVENTS");
    strcpy(comment,"&");
    break;
  case FRAME_EXTENSION_ID:
  case EXPOSURE_EXTENSION_ID:
  case LOST_EVENT_EXTENSION_ID:
    strcpy(hduclas1,"TEMPORALDATA");
    strcpy(comment,"&");
    break;
  case GTI_EXTENSION_ID:
    strcpy(hduclas1, "GTI");
    strcpy(comment,"&");
    break;
  default:
    fprintf(stderr,"%s: extention ID (%d) out of range\n",pname,extID);
    fflush(stderr);
    return ANL_FALSE;
  }

  if (fits_update_key_str(fp, "HDUCLAS1", hduclas1, comment, status)) {
    fprintf(stderr,"%s: fits_update_key_str() HDUCLAS1 failed (%d)\n",pname,*status);
    return ANL_FALSE;
  }

  /* write HDUCLAS2 only for event extension */
  if (extID == EVENT_EXTENSION_ID) {
    strcpy(comment,"photon event list, includes all photons");
    if (fits_update_key_str(fp, "HDUCLAS2", "ALL", comment, status)) {
      fprintf(stderr,"%s: fits_update_key_str() HDUCLAS2 failed (%d)\n",pname,*status);
      return ANL_FALSE;
    }
  }
  else {
    if (fits_delete_key (fp, "HDUCLAS2", status)) {
      fprintf(stderr,"%s: fits_dlete_key for HDUCLAS2 failed (%d)\n", pname, *status);
    }
  }


  if (fitsIsInitiallyClosed) {
    if (fitsclose (&fp) != ANL_TRUE) return ANL_FALSE;
  }

  return ANL_TRUE;
}







int write_DM_coor_keys(FITS_EXT_STRUCT *ext,
		       int hdunum,
		       int coor_flag,
		       int *status)
{

  /********************************************************************************
   * coor_flag (coordinates flag):
   *       == 1 --> write all M keywords
   *                editmode: 5x5, 3x3, top3, Timing

   *       == 2 --> write only RAW AND ACT
   *                (some event diagnosis modes do not have FOC, SKY coordinates)
   *                editmode: DarkUpdate_DarkInit
   *
   *       == 3 --> write only RAW AND ACT for lost area
   ********************************************************************************/

  int hdutype;
  int fitsIsInitiallyClosed=0;  /* 1: closed -- close it at the end;   *
			         * 0: open -- leave it open at the end */
  fitsfile * fp = ext->fitsd;

  if (fp == (fitsfile *)NULL) {
    if (fits_open_file (&fp, ext->filename, READWRITE, status)){
      fprintf(stderr, "%s: fits_open_file failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
    fitsIsInitiallyClosed=1;
  }

  /* move to the specified xtention unless hdunum=0 */
  if (hdunum != 0) {
    if (fits_movabs_hdu(fp, hdunum, &hdutype, status)) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
  }

  if (coor_flag == 1 || coor_flag == 2) {
    if (fits_write_key_str(fp, "MTYPE1", "RAW", "DM keyword: RAW coordinates", status)) {
      fprintf(stderr, "%s: fits_write_key_str MTYPE1  failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MFORM1", "RAWX,RAWY", "DM keyword: RAW X and Y coordinates ", status)) {
      fprintf(stderr, "%s: fits_write_key_str  MFORM1 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }

    if (fits_write_key_str(fp, "MTYPE2", "ACT", "DM keyword: ACT coordinates", status)) {
      fprintf(stderr, "%s: fits_write_key_str MTYPE2  failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MFORM2", "ACTX,ACTY", "DM keyword: ACT X and Y coordinates ", status)) {
      fprintf(stderr, "%s: fits_write_key_str  MFORM2 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
  }

  if (coor_flag == 1) {

    if (fits_write_key_str(fp, "MTYPE3", "DET", "DM keyword: DET coordinates", status)) {
      fprintf(stderr, "%s: fits_write_key_str MTYPE3  failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MFORM3", "DETX,DETY", "DM keyword: DET X and Y coordinates ", status)) {
      fprintf(stderr, "%s: fits_write_key_str  MFORM3 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }

    if (fits_write_key_str(fp, "MTYPE4", "FOC", "DM keyword: FOC coordinates", status)) {
      fprintf(stderr, "%s: fits_write_key_str MTYPE4  failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MFORM4", "FOCX,FOCY", "DM keyword: FOC X and Y coordinates ", status)) {
      fprintf(stderr, "%s: fits_write_key_str  MFORM4 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }

    if (fits_write_key_str(fp, "MTYPE5", "SKY", "DM keyword: SKY coordinates", status)) {
      fprintf(stderr, "%s: fits_write_key_str MTYPE5  failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MFORM5", "X,Y", "DM keyword: SKY X and Y coordinates ", status)) {
      fprintf(stderr, "%s: fits_write_key_str  MFORM5 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }

    /* write OPTIC keywords of DET/FOC/SKY only for event extension */
    if (fits_write_key_dbl(fp, "OPTIC6", OPTIC_DEFAULT, 15,
			   "Optical axis X in detector coords (pixels)",
			   status)) {
      fprintf(stderr, "%s: fits_write_key_dbl  OPTIC6 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_dbl(fp, "OPTIC7", OPTIC_DEFAULT, 15,
			   "Optical axis Y in detector coords (pixels)",
			   status)) {
      fprintf(stderr, "%s: fits_write_key_dbl  OPTIC7 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }

    if (fits_write_key_dbl(fp, "OPTIC8", OPTIC_DEFAULT, 15,
			   "Optical axis X in foc coords (pixels)", status)) {
      fprintf(stderr, "%s: fits_write_key_dbl  OPTIC8 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_dbl(fp, "OPTIC9", OPTIC_DEFAULT, 15,
			   "Optical axis Y in foc coords (pixels)", status)) {
      fprintf(stderr, "%s: fits_write_key_dbl  OPTIC9 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }

    if (fits_write_key_dbl(fp, "OPTIC10", OPTIC_DEFAULT, 15,
			   "File mean optical axis X in sky coords (pixels)",
			   status)) {
      fprintf(stderr, "%s: fits_write_key_dbl  OPTIC10 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_dbl(fp, "OPTIC11", OPTIC_DEFAULT, 15,
			   "File mean optical axis Y in sky coords (pixels)",
			   status)) {
      fprintf(stderr, "%s: fits_write_key_dbl  OPTIC11 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }

  }

  /* this if for coordinaates in area extention */
  if (coor_flag == 3) {
    if (fits_write_key_str(fp, "MTYPE1", "RAW_ST_LOST_AREA", "DM keyword: LOST AREA ST RAW coordinates", status)) {
      fprintf(stderr, "%s: fits_write_key_str MTYPE1  failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MFORM1", "RAWX_ST_LOST_AREA,RAWY_ST_LOST_AREA", "DM keyword: LOST AREA ST RAW X and Y coordinates ", status)) {
      fprintf(stderr, "%s: fits_write_key_str  MFORM1 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }

    if (fits_write_key_str(fp, "MTYPE2", "RAW_END_LOST_AREA", "DM keyword: LOST AREA END RAW coordinates", status)) {
      fprintf(stderr, "%s: fits_write_key_str MTYPE2  failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MFORM2", "RAWX_END_LOST_AREA,RAWY_ST_LOST_AREA", "DM keyword: LOST AREA END RAW X and Y coordinates ", status)) {
      fprintf(stderr, "%s: fits_write_key_str  MFORM2 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }

    if (fits_write_key_str(fp, "MTYPE3", "ACT_ST_LOST_AREA", "DM keyword: LOST AREA ST ACT coordinates", status)) {
      fprintf(stderr, "%s: fits_write_key_str MTYPE3  failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MFORM3", "ACTX_ST_LOST_AREA,ACTY_ST_LOST_AREA", "DM keyword: LOST AREA START X and Y coordinates ", status)) {
      fprintf(stderr, "%s: fits_write_key_str  MFORM3 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MTYPE4", "ACT_END_LOST_AREA", "DM keyword: LOST AREA END ACT coordinates", status)) {
      fprintf(stderr, "%s: fits_write_key_str MTYPE4  failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
    if (fits_write_key_str(fp, "MFORM4", "ACTX_END_LOST_AREA,ACTY_END_LOST_AREA", "DM keyword: LOST AREA END X and Y coordinates ", status)) {
      fprintf(stderr, "%s: fits_write_key_str  MFORM4 failed (%d)\n", pname, *status);
      return ANL_FALSE;
    }
  }

  if (fitsIsInitiallyClosed) {
    if (fitsclose (&fp) != ANL_TRUE) return ANL_FALSE;
  }

  return ANL_TRUE;
}








int write_DM_GTI_keys(FITS_EXT_STRUCT *ext,
		      int hdunum,
		      int *status)
{

  int hdutype;
  int fitsIsInitiallyClosed=0;  /* 1: closed -- close it at the end;   *
			         * 0: open -- leave it open at the end */
  fitsfile * fp = ext->fitsd;

  if (fp == (fitsfile *)NULL) {
    if (fits_open_file (&fp, ext->filename, READWRITE, status)){
      fprintf(stderr, "%s: fits_open_file failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
    fitsIsInitiallyClosed=1;
  }

  /* move to the specified xtention unless hdunum=0 */
  if (hdunum != 0) {
    if (fits_movabs_hdu(fp, hdunum, &hdutype, status)) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
  }

  if (fits_write_key_str(fp,"MTYPE1", "TIME", "Data type",status)) {
    fprintf(stderr, "%s:  ffpkys for MTYPE1 failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp,"MFORM1", "START,STOP", "Names of start and stop columns",status)) {
    fprintf(stderr, "%s:  ffpkys for MFORM1 failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }

  if (fits_write_key_str(fp,"METYP1", "R", "Data descryptor type: Range,",status)) {
    fprintf(stderr, "%s:  ffpkys for MFORM1 failed (%d)\n", pname, *status);
    return ANL_FALSE;
  }

  if (fitsIsInitiallyClosed) {
    if (fitsclose (&fp) != ANL_TRUE) return ANL_FALSE;
  }

  return ANL_TRUE;
}







int updateDatamodeKey(FITS_EXT_STRUCT * ext,
		      int hdunum,
		      char *datamode,
		      int *status)
{
  int hdutype;
  char comment[] = "&";

  int fitsIsInitiallyClosed=0;  /* 1: closed -- close it at the end;   *
			         * 0: open -- leave it open at the end */
  fitsfile * fp = ext->fitsd;

  if (fp == (fitsfile *)NULL) {
    if (fits_open_file (&fp, ext->filename, READWRITE, status)){
      fprintf(stderr, "%s: fits_open_file failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
    fitsIsInitiallyClosed=1;
  }

  /* move to the specified xtention unless hdunum=0 */
  if (hdunum != 0) {
    if (fits_movabs_hdu(fp, hdunum, &hdutype, status)) {
      fprintf(stderr, "%s: fits_movabs_hdu failed (status=%d)\n",pname, *status);
      return ANL_FALSE;
    }
  }

  if (fits_update_key_str(fp, "DATAMODE", datamode, comment, status)) {
    fprintf(stderr,"%s: fits_update_key_str() DATAMODE failed (%d)\n",pname,*status);
    return ANL_FALSE;
  }

  if (fitsIsInitiallyClosed) {
    if (fitsclose (&fp) != ANL_TRUE) return ANL_FALSE;
  }

  return ANL_TRUE;
}





/* TSCAL で始まるキーワードを消去 */

int delete_TSCAL_keys(FITS_EXT_STRUCT *ext)
{
  int hdutype;
  int istat = 0;

  int i; /* ループ用 */

  /* fits_get_hdrspace 用 */
  int keynumbers; /* keyword はいくつ存在するか */
  int morekeys; /* あといくつキーワードを追加できるか */

  /* fits_read_keyn 用 */
  char keyname[128]; /* keyword name */
  char value[128];   /* keyword value */
  char comment[128]; /* keyword comment */

  int tscal_num = 0; /* TSCALE で始まるキーワード数 */
  char tscal_list[128][32]; /* TSCALE で始まるキーワードのリスト */


  if ( NULL == ext->fitsd ) {
    fits_open_file(&ext->fitsd, ext->filename, READWRITE, &istat);
  }

  fits_movabs_hdu(ext->fitsd, 2, &hdutype, &istat);

  /* キーワードの数をサーチ */
  fits_get_hdrspace(ext->fitsd, &keynumbers, &morekeys, &istat);

  /* キーワードを読み込んでいく */
  for(i=1; i<=keynumbers; i++) {
    fits_read_keyn(ext->fitsd, i, keyname, value, comment, &istat);

    /* もしキーワードが TSCAL で始まっていたら、その番号と名前を記録 */
    if (strncmp(keyname, "TSCAL", 5) == 0 ) {
      tscal_num++;
      strcpy (tscal_list[tscal_num], keyname);
    }
  }

  /* TSCAL で始まるキーワード消去 */
  for (i=1; i<=tscal_num; i++) {
    fits_delete_key (ext->fitsd, tscal_list[i], &istat);
  }

  fits_close_file (ext->fitsd, &istat);

  ext->fitsd = NULL;


  if ( istat ) {
    printf ("Error in deleteing TSCAL keywords\n");
    return ANL_FALSE;
  }

  return ANL_TRUE;
}
