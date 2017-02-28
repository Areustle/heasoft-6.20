/*
 * TYPE: ASTE_ANL module
 * NAME: HXDtrnpi
 *
 * file: HXDtrnpi.c
 *
 * PURPOSE:
 *  TRANSIENT PH DATA, convert PHA to PI. 
 *
 * HISTORY:
 *       version 0.0.1          99/10/09  Y.Terada  
 *         only unpack PH binning by HXD-DE. (func:hxdtrnpiUtil v0.0.1)
 *       version 0.0.2          99/10/14  Y.Terada
 *         support Gain History Table (NULL FUNCTION)
 *         HXDtrnpi does not support TPU_LINEARITY, ANTI_LINEARITY,
 *         ANTI_GAIN_EACH, because these effects are considered in 
 *         RESPONCE GENERATOR. [14 Oct 1999 : HXD team]
 *       version 0.0.3          99/10/31  Y.Terada
 *         Change #include
 *       version 0.0.4          99/12/24  Y. Terada
 *             Change param name for the third release of HXD-FTOOLS.
 *       version 0.1.0          03/05/03  Y. Terada
 *             HXD-II version, NULL version.
 *       version 0.2.0          03/07/23  Y.Terada  
 *         for HEADAS, PIL
 *       version 0.2.2          2005/11/05 Y.Terada
 *         put PIL parameters in FITS header
 *       version 0.2.3          2005/11/08 Y.Terada
 *         debug
 *       version 0.2.4          2006/02/13 Y.Terada
 *         check uninitialized value
 *       version 2.0.0          2007/05/10 Y.Terada
 *         support CALDB access
 *       version 2.0.1          2013/10/23 Y.Terada
 *         debug init
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "atFunctions.h"
#include "fitsio.h"
#include "cfortran.h"
#include "hbook.h"
#include "hxd/HXD.h"

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

#include "hxdtrnpiUtil.h"
#include "HXDtrnpi.h"
#include "hxdtrnFitsUtil.h"
#include "aste_gethk.h"
#include "hxdFitsHeaderUtil.h"
#include "aste_caldb.h"

char HXDtrnpi_version[] = "version 2.0.0";
static char pname[] = "HXDtrnpi";

#define TRN_PH_CH 54
#define FILELIST_MAX_LENGTH PIL_LINESIZE

#define DEBUG 0

static char rebin_file_list[FILELIST_MAX_LENGTH];
static char o_rebin_file_list[FILELIST_MAX_LENGTH];
static ASTE_HK *rebin_file;
static char gain_history_list[FILELIST_MAX_LENGTH];
static ASTE_HK *gain_history_file;
static int caldb_type_wampht;

void
HXDtrnpi_startup(int *status){
  int used = 1;
  BnkPut( "HXD:ftools:hxdwampi_yn",         sizeof(int), &used);

  BnkDef( "HXD:PIL:trn_bintbl_name",    sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef( "HXD:PIL:trn_gainhist_name",  sizeof(char)*HXDFITSHEADER_LINESIZE);

  BnkDef("HXD:PIL:CALDB_TYPE:wampht", sizeof(int));
  caldb_type_wampht = 0;

  *status = ANL_OK;
}

void
HXDtrnpi_com(int *status) {
  CALDB_INFO caldb;
  int string_size;
  char *k;
        
    if ( *status ) { /* ftools */
        
        *status = 0;

        aste_caldb_init(&caldb);

	/*
	UCLGST("trn_bintbl_name", rebin_file_list, *status);
	*/
	*status = PILGetFname("trn_bintbl_name", o_rebin_file_list);
	if ( *status ) {
	    fprintf(stderr,"%s: PILGetFname trn_bintbl_name error (%d)\n",
		    pname, *status);
	    *status = ANL_QUIT;
	    exit(-1);
	} else {
	  if (0==CLstricmp("CALDB", o_rebin_file_list) ){
	    caldb_type_wampht = 1;
	    caldb.telescop = "SUZAKU";        /* TELESCOP */
	    caldb.instrume = "HXD";           /* INSTRUME */
	    caldb.detnam   = "WAM_ANTI";      /* DETNAM   */
	    caldb.codename = "TRNPHTBL";      /* CCNM0001 */
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
	    strcpy(rebin_file_list, caldb.filename);
	  } else{
	    strcpy(rebin_file_list, o_rebin_file_list);
	  }

	  /*-- Put filename --*/
	  string_size = strlen(rebin_file_list);
	  if  (string_size>HXDFITSHEADER_LINESIZE) {
	    fprintf(stderr, "%s: warning trn_bintbl_name is long\n", pname);
	    string_size = HXDFITSHEADER_LINESIZE;
	  }
	  BnkPut("HXD:PIL:trn_bintbl_name", string_size,
		 rebin_file_list);
	  BnkPut("HXD:PIL:CALDB_TYPE:wampht",sizeof(int),
		 &caldb_type_wampht);
	}

	/*
	UCLGST("trn_gainhist_name", gain_history_list, *status);
	*/
	*status = PILGetFname("trn_gainhist_name", gain_history_list);
	if ( *status ) {
	  fprintf(stderr,"%s: PILGetFname trn_gainhist_name error (%d)\n",
		  pname, *status);
	    *status = ANL_QUIT;
	    exit(-1);
	} else {
	  string_size = strlen(gain_history_list);
	  if  (string_size>HXDFITSHEADER_LINESIZE) {
	    fprintf(stderr, "%s: warning trn_gainhist_name is long\n", pname);
	    string_size = HXDFITSHEADER_LINESIZE;
	  }
	  BnkPut("HXD:PIL:trn_gainhist_name", string_size,
		 gain_history_list);
	}
	
        *status = ANL_OK;
        return;
    }
    
    /**** HXD-QL ****/
    rebin_file_list[0] = '\0';
    CLtxtrd("hxd_trnpi_rebin_table_file_name",
	    rebin_file_list, sizeof(rebin_file_list) );
  
    gain_history_list[0] = '\0';
    CLtxtrd("hxd_trnpi_gain_table_file_name",
	    gain_history_list, sizeof(gain_history_list) );

    *status = ANL_OK;
}

void
HXDtrnpi_init(int *status) {
    int istat = ANL_OK;

    if(DEBUG) fprintf(stderr,"%s:<<DEBUG>> DE rebin init\n",pname);
/*    hxdtrnpi_DE_correct_Init(rebin_file_list, &rebin_file, &istat); */
    if(istat == ANL_NG){
	fprintf(stderr,"can not init rebin_file(%d)\n",istat);
	*status = ANL_QUIT;
	return;
    }
    
    if(DEBUG) fprintf(stderr,"%s:<<DEBUG>> Gain init\n",pname);
/*    hxdtrnpi_Gain_correct_Init(gain_history_list, &gain_history_file, &istat);*/
    if(istat == ANL_NG){
	fprintf(stderr,"can not init gain_table_file(%d)\n",istat);
	*status = ANL_QUIT;
	return;
    }
    
    *status = ANL_OK;
}

void
HXDtrnpi_his(int *status){
    *status = ANL_OK;
}

void
HXDtrnpi_bgnrun(int *status){
    
    *status = ANL_OK;
    
}

void
HXDtrnpi_ana(int nevent, int eventid, int *status){
    int size, istat;
    int rdbin, board, ch;
    double trntime; /* must be time assigned by hxdtrntime */
    
    int pha[TRN_PH_CH];
    double pi_ae[TRN_PI_NUM], pi[TRN_PI_NUM];
    int pi_int[TRN_PI_NUM];
    
    int    rebin_table[TRN_PH_CH];     /* HXD-DE binning format */
    double gain_table[GAIN_TABLE_BIN]; /* gain correction table */
    istat = ANL_OK;

    BnkfGetM( "HXD:TRN:EV_TIME", sizeof(double), &size, &trntime);
    BnkfGetM( "HXD:TRB:PH"     , sizeof(pha), &size , pha );
    BnkfGetM( "HXD:TRH:BOARD"  , sizeof(int), &size , &board );
    BnkfGetM( "HXD:TRN:RDBIN"  , sizeof(int), &size , &rdbin );

    if(DEBUG) fprintf(stderr,"%s:<<DEBUG>> rdbin=%d\n",pname,rdbin);
    /* ========== (1) HXD-DE binning correction ================ */
    if( rdbin == 0 ){
	 hxdtrnpi_DE_no_correct(pha, pi_ae);
    } else {
/*	hxdtrnpi_rebintable_read(rebin_file_list, trntime,
                                 rebin_table, &istat); */
	if(istat==ANL_NG){
	    fprintf(stderr,"HXDtrnpi:cannot read rebintable.\n");
	    *status = ANL_QUIT;
	    return;
	}
/*	hxdtrnpi_DE_correct( board, pha, pi_ae, rebin_table); */
    }
    if(DEBUG) fprintf(stderr,"%s:<<DEBUG>> DE done..\n",pname);

    /* ========== (2) HXD-AE binning correction ================ */
    /****** We does not need to correct this binning. ************/
/*  hxdtrnpi_AEDE_correct( pha_ae, pha_adc ); */ /* 54ch -> 64ch */

    /* ========== (3) HXD-TPU linearity correction ============= */
    /****** Included in RESPONCE BUILDER.              ***********/
    /****** HXDTRNPI does not correct AE non-linearity ***********/
/*  hxdtrnpi_AE_correct( board,... );*/

    /* ========== (4) HXD-Anti linearity correction ============ */
    /****** Included in RESPONCE BUILDER.              ***********/
    /****** HXDTRNPI does not correct AE non-linearity ***********/
/*  hxdtrnpi_S_correct( board, );*/

    /* =========== (5) Anti gain_total correction ============== */
    /**** gain_unit correction is included in RESPONCE BUILDER ***/
/*  hxdtrnpi_gaintable_read(gain_history_list, trntime, board,
			    gain_table, &istat);*/

    if(istat==ANL_NG){
	fprintf(stderr,"HXDtrnpi:cannot read gain history table.\n");
	*status = ANL_QUIT;
	return;
    }
    if(DEBUG) fprintf(stderr,"%s:<<DEBUG>> Gain Tbl read done..\n",pname);
    
/*  hxdtrnpi_Gain_correction( pi_ae, pi, gain_table, &istat ); */
    if(istat==ANL_NG){
	fprintf(stderr,"HXDtrnpi: Gain correction err.\n");
 	*status = ANL_QUIT;
	return;
    }
    if(DEBUG) fprintf(stderr,"%s:<<DEBUG>> Gain done..\n",pname);
    
    /* =========== (6) reformat  ============== */
    for(ch=0;ch<TRN_PI_NUM;ch++) {
/*    pi_int[ch] = (int) pi[ch]; */
      pi_int[ch] = 0;
    }

    /*************** DEBUG ***************/
    if(DEBUG) {
	int k;
	fprintf(stderr,"==============================\n");
    	for(k=0;k<54;k++) {
	    fprintf(stderr,"PHA0[%02d]=%d ",k,pha[k]);
	    if(k-(k/5)*5==3) fprintf(stderr,"\n");
	} fprintf(stderr,"\n");
	fprintf(stderr,"------------------------------\n");
    	for(k=0;k<54;k++) {
	    fprintf(stderr,"PHA1[%02d]=%f ",k,pi_ae[k]);
	    if(k-(k/5)*5==3) fprintf(stderr,"\n");
	} fprintf(stderr,"\n");
	fprintf(stderr,"------------------------------\n");
    	for(k=0;k<54;k++) {
	    fprintf(stderr,"PI[%02d]=%f ",k,pi[k]);
	    if(k-(k/5)*5==3) fprintf(stderr,"\n");
	} fprintf(stderr,"\n");
	fprintf(stderr,"------------------------------\n");
    	for(k=0;k<54;k++) {
	    fprintf(stderr,"PI[%02d]=%d ",k,pi_int[k]);
	    if(k-(k/5)*5==3) fprintf(stderr,"\n");
	} fprintf(stderr,"\n");
    }

    /* ========== (?) OUTPUT ================================ */
    BnkfPutM("HXD:TRB:PI",  sizeof(pi_int), pi_int);
/*  BnkfPutM("HXD:TRB:PI",  sizeof(int)*HXD_TRN_MAX_ENE_BIN, pi_int);*/
    
    *status = ANL_OK;
}

void
HXDtrnpi_endrun(int *status){ *status = ANL_OK; }

void
HXDtrnpi_exit(int *status){ *status = ANL_OK; }
/********************** END OF HXDtrnpi.c ************************/




