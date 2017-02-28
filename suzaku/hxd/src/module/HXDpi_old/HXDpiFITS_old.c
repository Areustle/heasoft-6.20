/*
 *   HXDpiFITS for FITS
 *       version 0.3.7 (Y.Terada) 1999-12-24
 *         change param name for the third release of hxd-ftools.
 *       version 0.4.0 2003-07-23 for HEADAS, 
 *         by Y Terada, H Takahashi, M Suzuki
 *       version 0.5.1 2005-01-25
 *         by Y.Terada, T.Kisisita, S.Hirakuri
 *         support HK access (add HXDgethkInit)
 *         support hxdcaldbUtil (delete HXDtableFitsInit)
 *       version 0.5.2 2005-02-02
 *         support Fits I/O
 *         by T.Kishishita
 *       version 0.5.7 (Y.Terada) 2005-06-09
 *              new aste_gethk version 2.3
 *       version 0.5.7 (S.Hirakuri) 2005-06-14
 *              support WEL:TIME = 0.0 event
 *       version 0.6.2 (Y.Terada) 2005-11-05
 *              put PIL parameters in Bnk, debug
 *       version 0.6.3 (Y.Terada) 2005-11-08
 *              shorten Bnk name
 *       version 0.6.5 (H.Takahashi) 2006-02-04
 *              Invarid value 0.0 -> define
 *       version 2.0.0 (Y.Terada)    2006-09-10
 *              new format for version 2.0.x.x process
 *       version 2.0.1 (Y.Terada)    2007-04-27
 *              CALDB auto access function
 *       version 2.1.0 (M.Kokubun)   2007-05-08
 *              GHF -> GHT replacement
 *       version 2.1.1 (Y.Terada)    2007-05-13
 *              debug
 *       version 2.1.2 (M.Kokubun)    2007-05-20
 *              debug (static hk/ehk_data)
 *       version 2.1.3 (M.Kokubun)    2007-06-12
 *              debug (TINT -> TDOUBLE/TFLOAT)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "fitsio.h"
#include "cfortran.h"
#include "HXD.h"
/*#include <pfile.h>*/

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

#include "aste_gethk.h"
#include "hxdtableFitsUtil.h"
#include "hxdpiUtil_old.h"
#include "hxdFitsHeaderUtil_old.h"
#include "aste_caldb.h"
#define DEBUG 0

#define FILELIST_MAX_LENGTH PIL_LINESIZE

#define HXD_TIME_INVALID 0.0

char HXDpiFITS_old_version[] = "version 2.1.3";

static char pname[] = "HXDpiFITS";

static struct{
  ASTE_HK *pin_gainhistory;
  ASTE_HK *gso_gainhistory;
  ASTE_HK *aste_hk;
  char hxd_gsoght_fname[FILELIST_MAX_LENGTH];
  char hxd_gsolin_fname[FILELIST_MAX_LENGTH];
  char hxd_pinghf_fname[FILELIST_MAX_LENGTH];
  char hxd_pinlin_fname[FILELIST_MAX_LENGTH];
  char o_hxd_gsoght_fname[FILELIST_MAX_LENGTH];
  char o_hxd_gsolin_fname[FILELIST_MAX_LENGTH];
  char o_hxd_pinghf_fname[FILELIST_MAX_LENGTH];
  char o_hxd_pinlin_fname[FILELIST_MAX_LENGTH];
  int  caldb_type_pinghf;
  int  caldb_type_gsoght;
  int  caldb_type_pinlin;
  int  caldb_type_gsolin;
  int hk_id[HXDPIUTIL_HK_COLNUM];
  int ehk_id[HXDPIUTIL_EHK_COLNUM];
} com;

enum{
    HXD_TEMP_W10_CAL, HXD_TEMP_W11_CAL, HXD_TEMP_W12_CAL,
    HXD_TEMP_W13_CAL, HXD_TEMP_W20_CAL, HXD_TEMP_W21_CAL,
    HXD_TEMP_W22_CAL, HXD_TEMP_W23_CAL, HXD_TEMP_W00_CAL,
    HXD_TEMP_W01_CAL, HXD_TEMP_W02_CAL, HXD_TEMP_W03_CAL,
    HXD_TEMP_W30_CAL, HXD_TEMP_W31_CAL, HXD_TEMP_W32_CAL,
    HXD_TEMP_W33_CAL, HXD_TEMP_T10_CAL, HXD_TEMP_T12_CAL,
    HXD_TEMP_T14_CAL, HXD_TEMP_T21_CAL, HXD_TEMP_T23_CAL,
    HXD_TEMP_HV_W2_CAL, HXD_TEMP_HV_P1_CAL,
    HXD_TEMP_HV_T1_CAL, HXD_TEMP_T00_CAL,
    HXD_TEMP_T02_CAL,   HXD_TEMP_T04_CAL,
    HXD_TEMP_T31_CAL,   HXD_TEMP_T33_CAL,
    HXD_TEMP_HV_W0_CAL, HXD_TEMP_HV_P0_CAL,
    HXD_TEMP_HV_T3_CAL, HXD_TEMP_CAP4_CAL,
    HXD_TEMP_CAP3_CAL,  HXD_TEMP_BODY4_CAL,
    HXD_TEMP_BODY3_CAL, HXD_TEMP_BTM3_CAL,
    HXD_TEMP_BTM4_CAL,  HXD_TEMP_BAR3_CAL,
    HXD_TEMP_CENTER_CAL,HXD_TEMP_CAP2_CAL,
    HXD_TEMP_CAP1_CAL,  HXD_TEMP_BODY2_CAL,
    HXD_TEMP_BODY1_CAL, HXD_TEMP_BTM1_CAL,
    HXD_TEMP_BTM2_CAL,  HXD_TEMP_BAR1_CAL, HXD_TEMP_BAR2_CAL
};

enum{
    TIME, YYYYMMDD, HHMMSS, EULER1, EULER2, EULER3,
    FOC_RA, FOC_DEC, FOC_ROLL, DLT_RA, DLT_DEC, DLT_ROLL,
    ANG_DIST, SAT_ALT, SAT_LON, SAT_LAT, ELV, DYE_ELV, NTE_ELV,
    SUN_ALT, T_DY_NT, TN_DY_NT, COR, SAA, T_SAA, TN_SAA, SAA_HXD,
    T_SAA_HXD, TN_SAA_HXD, ZGMAG_ANG, ZGMAG_PHI, ZE_ANG, ZE_PHI
};

void
HXDpiFITS_old_startup(int *status)
{
  BnkDef("HXD:PIL:hxd_gsoght_fname", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXD:PIL:hxd_gsolin_fname", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXD:PIL:hxd_pinghf_fname", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXD:PIL:hxd_pinlin_fname", sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXD:PIL:CALDB_TYPE:gsoght", sizeof(int));
  BnkDef("HXD:PIL:CALDB_TYPE:gsolin", sizeof(int));
  BnkDef("HXD:PIL:CALDB_TYPE:pinghf", sizeof(int));
  BnkDef("HXD:PIL:CALDB_TYPE:pinlin", sizeof(int));

  /*
  com.hxd_gsoght_fname = com.o_hxd_gsoght_fname;
  com.hxd_gsolin_fname = com.o_hxd_gsolin_fname;
  com.hxd_pinghf_fname = com.o_hxd_pinghf_fname;
  com.hxd_pinlin_fname = com.o_hxd_pinlin_fname;
  */
  com.caldb_type_gsoght = 0 ;
  com.caldb_type_gsolin = 0 ;
  com.caldb_type_pinghf = 0 ;
  com.caldb_type_pinlin = 0 ;

  *status = ANL_OK;
}

void
HXDpiFITS_old_com(int *status)
{

  CALDB_INFO caldb;
  int string_size;
  char *k;

  if ( *status ) { /* ftools */

    *status = 0;

    aste_caldb_init(&caldb);

    /**************** hxd_pinghf_fname ******************/
    *status = PILGetFname(k="hxd_pinghf_fname", com.o_hxd_pinghf_fname);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname PIN GHF error (%d)\n", pname, *status);
      *status = ANL_QUIT;
      return; /*exit(-1);*/
    } else {
      /*-- caldb access --*/
      if (0==CLstricmp("CALDB", com.o_hxd_pinghf_fname) ){
	com.caldb_type_pinghf = 1;
	caldb.telescop = "SUZAKU";        /* TELESCOP */
	caldb.instrume = "HXD";           /* INSTRUME */
	caldb.detnam   = "WELL_PIN";      /* DETNAM   */
	caldb.codename = "GAIN_HISTORY";  /* CCNM0001 */
	aste_caldb_get(&caldb);
	if (caldb.status != 0 || caldb.nfound == 0){
	  anl_msg_error("%s: no CALDB entry for '%s-%s' (status=%d)\n",
			pname, caldb.detnam, caldb.codename, caldb.status);
	  *status = ANL_QUIT;
	  return;
	}
	if (caldb.nfound != 1){
	  anl_msg_warning("%s: multiple CALDB entry for '%s' (nfound=%d)\n",
			pname, caldb.codename, caldb.nfound);
	}
	strcpy(com.hxd_pinghf_fname, caldb.filename);
      } else{
	strcpy(com.hxd_pinghf_fname, com.o_hxd_pinghf_fname);
      }

      /*-- Put filename --*/
      string_size = strlen(com.hxd_pinghf_fname);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning hxd_pinghf_fname is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }

      BnkPut("HXD:PIL:hxd_pinghf_fname", string_size, 
	     com.hxd_pinghf_fname);
      BnkPut("HXD:PIL:CALDB_TYPE:pinghf",sizeof(int),&com.caldb_type_pinghf);
      if(DEBUG) fprintf(stderr,"COM:pinghf=%s (size=%d)\n",
			com.hxd_pinghf_fname,string_size);
    }

    /**************** hxd_gsoght_fname ******************/
    *status = PILGetFname(k="hxd_gsoght_fname", com.o_hxd_gsoght_fname);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname GSO GHT error (%d)\n", pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      /*-- caldb access --*/
      if (0==CLstricmp("CALDB", com.o_hxd_gsoght_fname) ){
	com.caldb_type_gsoght = 1;
	caldb.telescop = "SUZAKU";        /* TELESCOP */
	caldb.instrume = "HXD";           /* INSTRUME */
	caldb.detnam   = "WELL_GSO";      /* DETNAM   */
	caldb.codename = "GAIN_HIST_PARAM";  /* CCNM0001 */
	caldb.expr     = "ENERG(348,348)";   /* CBDnnnnn */
	aste_caldb_get(&caldb);
	if (caldb.status != 0 || caldb.nfound == 0){
	  anl_msg_error("%s: no CALDB entry for '%s-%s' (status=%d)\n",
			pname, caldb.detnam, caldb.codename, caldb.status);
	  *status = ANL_QUIT;
	  return;
	}
	if (caldb.nfound != 3){
	  anl_msg_warning("%s: multiple CALDB entry for '%s' (nfound=%d)\n",
			pname, caldb.codename, caldb.nfound);
	  if(DEBUG) fprintf(stderr,"gsoght=%s, %d found\n",
			    caldb.filename, caldb.nfound);
	}
	strcpy(com.hxd_gsoght_fname , caldb.filename);
      } else{
	strcpy(com.hxd_gsoght_fname , com.o_hxd_gsoght_fname);
      }

      /*-- Put filename --*/
      string_size = strlen(com.hxd_gsoght_fname);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning hxd_gsoght_fname is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }

      BnkPut("HXD:PIL:hxd_gsoght_fname", string_size, 
	     com.hxd_gsoght_fname);
      BnkPut("HXD:PIL:CALDB_TYPE:gsoght",sizeof(int),&com.caldb_type_gsoght);
      if(DEBUG) fprintf(stderr,"COM:gsoght=%s (size=%d)\n",
			com.hxd_gsoght_fname,string_size);
    }

    /**************** hxd_gsolin_fname ******************/
    *status = PILGetFname("hxd_gsolin_fname", com.o_hxd_gsolin_fname);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname GSO Calibration error (%d)\n", pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      /*-- caldb access --*/
      if (0==CLstricmp("CALDB", com.o_hxd_gsolin_fname) ){
	com.caldb_type_gsolin = 1;
	caldb.telescop = "SUZAKU";   /* TELESCOP */
	caldb.instrume = "HXD";      /* INSTRUME */
	caldb.detnam   = "WELL_GSO"; /* DETNAM   */
	caldb.codename = "ADCDNL";   /* CCNM0001 */
	aste_caldb_get(&caldb);
	if (caldb.status != 0 || caldb.nfound == 0){
	  anl_msg_error("%s: no CALDB entry for '%s-%s' (status=%d)\n",
			pname, caldb.detnam, caldb.codename, caldb.status);
	  *status = ANL_QUIT;
	  return;
	}
	if (caldb.nfound != 1){
	  anl_msg_warning("%s: multiple CALDB entry for '%s' (nfound=%d)\n",
			pname, caldb.codename, caldb.nfound);
	}
	strcpy(com.hxd_gsolin_fname,caldb.filename);
      } else{
	strcpy(com.hxd_gsolin_fname,com.o_hxd_gsolin_fname);
      }

      /*-- Put filename --*/
      string_size = strlen(com.hxd_gsolin_fname);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning hxd_gsolin_fname is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }

      BnkPut("HXD:PIL:hxd_gsolin_fname", string_size, com.hxd_gsolin_fname);
      BnkPut("HXD:PIL:CALDB_TYPE:gsolin", sizeof(int), &com.caldb_type_gsolin);
      if(DEBUG) fprintf(stderr,"COM:gsolin=%s (size=%d, %d, %d)\n",
			com.hxd_gsolin_fname, sizeof(com.hxd_gsolin_fname),
			sizeof(com.o_hxd_gsolin_fname),string_size);
    }

    /**************** hxd_pinlin_fname ******************/
    *status = PILGetFname("hxd_pinlin_fname", com.o_hxd_pinlin_fname);
    if ( *status ) {
      fprintf(stderr, "%s: PILGetFname PIN Calibration error (%d)\n", pname, *status);
      *status = ANL_QUIT;
      exit(-1);
    } else {
      /*-- caldb access --*/
      if (0==CLstricmp("CALDB", com.o_hxd_pinlin_fname) ){
	com.caldb_type_pinlin = 1;
	caldb.telescop = "SUZAKU";   /* TELESCOP */
	caldb.instrume = "HXD";      /* INSTRUME */
	caldb.detnam   = "WELL_PIN"; /* DETNAM   */
	caldb.codename = "ADCINL";   /* CCNM0001 */
	aste_caldb_get(&caldb);
	if (caldb.status != 0 || caldb.nfound == 0){
	  anl_msg_error("%s: no CALDB entry for '%s-%s' (status=%d)\n",
			pname, caldb.detnam, caldb.codename, caldb.status);
	  *status = ANL_QUIT;
	  return;
	}
	if (caldb.nfound != 1){
	  anl_msg_warning("%s: multiple CALDB entry for '%s' (nfound=%d)\n",
			pname, caldb.codename, caldb.nfound);
	}
	strcpy(com.hxd_pinlin_fname , caldb.filename);
      } else{
	strcpy(com.hxd_pinlin_fname , com.o_hxd_pinlin_fname);
      }

      /*-- Put filename --*/
      string_size = strlen(com.hxd_pinlin_fname);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning hxd_pinlin_fname is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }

      BnkPut("HXD:PIL:hxd_pinlin_fname", string_size, com.hxd_pinlin_fname);
      BnkPut("HXD:PIL:CALDB_TYPE:pinlin", sizeof(int), &com.caldb_type_pinlin);
      if(DEBUG) fprintf(stderr,"COM:pinlin=%s (size=%d)\n",
			com.hxd_pinlin_fname,string_size);
    }

    *status = ANL_OK;
    return;
  }

  /* ANL */
  com.hxd_pinghf_fname[0]='\0';
  com.hxd_gsoght_fname[0]='\0';
	
  CLtxtrd("HXD pin gain history file name",
          com.hxd_pinghf_fname, sizeof(com.hxd_pinghf_fname) );

  CLtxtrd("HXD gso gain history table name",
          com.hxd_gsoght_fname, sizeof(com.hxd_gsoght_fname) );

  com.hxd_gsolin_fname[0]='\0';
  com.hxd_pinlin_fname[0]='\0';

  CLtxtrd("HXD GSO Calibration file name?",
          com.hxd_gsolin_fname, sizeof(com.hxd_gsolin_fname) );

  CLtxtrd("HXD PIN Calibration file name?",
          com.hxd_pinlin_fname, sizeof(com.hxd_pinlin_fname) );

  *status = ANL_OK;
}

void
HXDpiFITS_old_init(int *status)
{
  BnkDef("HXDpi:GSO_GHT_NAME", sizeof(char)*FILELIST_MAX_LENGTH);
  BnkDef("HXDpi:GSO_LIN_NAME", sizeof(char)*FILELIST_MAX_LENGTH);
  BnkDef("HXDpi:PIN_GHF_NAME", sizeof(char)*FILELIST_MAX_LENGTH);
  BnkDef("HXDpi:PIN_LIN_NAME", sizeof(char)*FILELIST_MAX_LENGTH);
  BnkDef("HXDpi:HKDATA",   sizeof(hxdpiUtil_old_HK));
  BnkDef("HXDpi:EHKDATA",  sizeof(hxdpiUtil_old_EHK));

  BnkfPutM("HXDpi:GSO_GHT_NAME", sizeof(char)*FILELIST_MAX_LENGTH,
	   com.hxd_gsoght_fname);
  BnkfPutM("HXDpi:PIN_GHF_NAME", sizeof(char)*FILELIST_MAX_LENGTH,
	   com.hxd_pinghf_fname);
  BnkfPutM("HXDpi:GSO_LIN_NAME", sizeof(char)*FILELIST_MAX_LENGTH, 
	   com.hxd_gsolin_fname);
  BnkfPutM("HXDpi:PIN_LIN_NAME", sizeof(char)*FILELIST_MAX_LENGTH,
	   com.hxd_pinlin_fname);

  *status = ANL_OK;
}

void
HXDpiFITS_old_his(int *status)
{
    *status = ANL_OK;
}

void
HXDpiFITS_old_bgnrun(int *status)
{
  int istat=0;
  int size;

  /*com.aste_hk = aste_gethk_init ( hk_filelist );*/
  BnkfGetM("HXDgethkInit:ASTE_HK", sizeof(ASTE_HK *), &size, &com.aste_hk);
  if( com.aste_hk == NULL ){
    fprintf(stderr, "%s: invalid aste_hk\n", pname);
    *status = ANL_QUIT;
    return;
  } 

  com.hk_id[HXD_TEMP_W10_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W10_CAL");
  com.hk_id[HXD_TEMP_W11_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W11_CAL");
  com.hk_id[HXD_TEMP_W12_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W12_CAL");
  com.hk_id[HXD_TEMP_W13_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W13_CAL");
  com.hk_id[HXD_TEMP_W20_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W20_CAL");
  com.hk_id[HXD_TEMP_W21_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W21_CAL");
  com.hk_id[HXD_TEMP_W22_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W22_CAL");
  com.hk_id[HXD_TEMP_W23_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W23_CAL");
  com.hk_id[HXD_TEMP_W00_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W00_CAL");
  com.hk_id[HXD_TEMP_W01_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W01_CAL");
  com.hk_id[HXD_TEMP_W02_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W02_CAL");
  com.hk_id[HXD_TEMP_W03_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W03_CAL");
  com.hk_id[HXD_TEMP_W30_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W30_CAL");
  com.hk_id[HXD_TEMP_W31_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W31_CAL");
  com.hk_id[HXD_TEMP_W32_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W32_CAL");
  com.hk_id[HXD_TEMP_W33_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_W33_CAL");
  com.hk_id[HXD_TEMP_T10_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T10_CAL");
  com.hk_id[HXD_TEMP_T12_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T12_CAL");
  com.hk_id[HXD_TEMP_T14_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T14_CAL");
  com.hk_id[HXD_TEMP_T21_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T21_CAL");
  com.hk_id[HXD_TEMP_T23_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T23_CAL");
  com.hk_id[HXD_TEMP_HV_W2_CAL] 
                            = aste_gethk_id(com.aste_hk, "HXD_TEMP_HV_W2_CAL");
  com.hk_id[HXD_TEMP_HV_P1_CAL] 
                            = aste_gethk_id(com.aste_hk, "HXD_TEMP_HV_P1_CAL");
  com.hk_id[HXD_TEMP_HV_T1_CAL] 
                            = aste_gethk_id(com.aste_hk, "HXD_TEMP_HV_T1_CAL");
  com.hk_id[HXD_TEMP_T00_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T00_CAL");
  com.hk_id[HXD_TEMP_T02_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T02_CAL");
  com.hk_id[HXD_TEMP_T04_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T04_CAL");
  com.hk_id[HXD_TEMP_T31_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T31_CAL");
  com.hk_id[HXD_TEMP_T33_CAL] = aste_gethk_id(com.aste_hk, "HXD_TEMP_T33_CAL");
  com.hk_id[HXD_TEMP_HV_W0_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_HV_W0_CAL");
  com.hk_id[HXD_TEMP_HV_P0_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_HV_P0_CAL");
  com.hk_id[HXD_TEMP_HV_T3_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_HV_T3_CAL");
  com.hk_id[HXD_TEMP_CAP4_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_CAP4_CAL");
  com.hk_id[HXD_TEMP_CAP3_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_CAP3_CAL");
  com.hk_id[HXD_TEMP_BODY4_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BODY4_CAL");
  com.hk_id[HXD_TEMP_BODY3_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BODY3_CAL");
  com.hk_id[HXD_TEMP_BTM3_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BTM3_CAL");
  com.hk_id[HXD_TEMP_BTM4_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BTM4_CAL");
  com.hk_id[HXD_TEMP_BAR3_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BAR3_CAL");
  com.hk_id[HXD_TEMP_CENTER_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_CENTER_CAL");
  com.hk_id[HXD_TEMP_CAP2_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_CAP2_CAL");
  com.hk_id[HXD_TEMP_CAP1_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_CAP1_CAL");
  com.hk_id[HXD_TEMP_BODY2_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BODY2_CAL");
  com.hk_id[HXD_TEMP_BODY1_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BODY1_CAL");
  com.hk_id[HXD_TEMP_BTM1_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BTM1_CAL");
  com.hk_id[HXD_TEMP_BTM2_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BTM2_CAL");
  com.hk_id[HXD_TEMP_BAR1_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BAR1_CAL");
  com.hk_id[HXD_TEMP_BAR2_CAL] 
    = aste_gethk_id(com.aste_hk, "HXD_TEMP_BAR2_CAL");

  com.ehk_id[T_SAA_HXD] = aste_gethk_id(com.aste_hk, "T_SAA_HXD");

  *status = ANL_OK;       
  
}

void
HXDpiFITS_old_ana(int nevent, int eventid, int *status)
{
  int istat = 0;

  int size;
  double time;
  static double gethk_time_info[3] = {0.0, 0.0, 0.0};
  static double getehk_time_info[3] = {0.0, 0.0, 0.0};
/* 0:latch time 1:pre time (not suport) 2:next time */

  HxdEventFits02 event_data;
  static hxdpiUtil_old_HK  hk_data;
  static hxdpiUtil_old_EHK ehk_data;

  int update;
  
  BnkfGetM( "HXD:ALL:UPDATE", sizeof(int), &size, &update );
  if( !(update & HXD_UPDATE_WEL) ){
    *status = ANL_OK;
    return;
  }

  BnkfGetM("HXD:WEL:EVENT", sizeof(HxdEventFits02), &size,&event_data);
  if(event_data.time == HXD_TIME_INVALID){
    *status = ANL_OK;
    return;
  }



  BnkfGetM( "HXD:WEL:PACKET_S_TIME", sizeof(double), &size, &time);

  if ( (gethk_time_info[1]<time) && (time<gethk_time_info[2]) 
       && (getehk_time_info[1]<time) && (time<getehk_time_info[2]) ){
    /** no need to update **/
    *status = ANL_OK;
    return;
  }

  istat = aste_gethk(com.hk_id[HXD_TEMP_W10_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w10_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W11_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w11_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W12_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w12_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W13_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w13_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W20_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w20_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W21_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w21_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W22_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w22_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W23_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w23_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W00_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w00_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W01_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w01_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W02_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w02_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W03_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w03_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W30_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w30_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W31_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w31_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W32_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w32_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_W33_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_w33_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T10_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t10_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T12_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t12_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T14_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t14_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T21_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t21_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T23_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t23_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_HV_W2_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_hv_w2_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_HV_P1_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_hv_p1_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_HV_T1_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_hv_t1_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T00_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t00_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T02_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t02_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T04_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t04_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T31_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t31_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_T33_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_t33_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_HV_W0_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_hv_w0_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_HV_P0_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_hv_p0_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_HV_T3_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_hv_t3_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_CAP4_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_cap4_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_CAP3_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_cap3_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BODY4_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_body4_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BODY3_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_body3_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BTM3_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_btm3_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BTM4_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_btm4_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BAR3_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_bar3_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_CENTER_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_center_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_CAP2_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_cap2_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_CAP1_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_cap1_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BODY2_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_body2_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BODY1_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_body1_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BTM1_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_btm1_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BTM2_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_btm2_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BAR1_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_bar1_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  istat = aste_gethk(com.hk_id[HXD_TEMP_BAR2_CAL], time, TDOUBLE, 1, 
		     &hk_data.hxd_temp_bar2_cal, gethk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  BnkfPutM("HXDpi:HKDATA",  sizeof(hxdpiUtil_old_HK), &hk_data);
 

  istat = aste_gethk(com.ehk_id[T_SAA_HXD], time, TFLOAT, 1, 
		     &ehk_data.t_saa_hxd, getehk_time_info);
  if (istat == ANL_NG){     *status = ANL_NG;    return;  }  

  BnkfPutM("HXDpi:EHKDATA",  sizeof(hxdpiUtil_old_EHK), &ehk_data);

  *status = ANL_OK;

}

void
HXDpiFITS_old_endrun(int *status)
{    
    *status = ANL_OK;
}

void
HXDpiFITS_old_exit(int *status)
{
    *status = ANL_OK;
}
