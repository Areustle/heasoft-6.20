/*
   HXDHKFitsRead
     v0.0.9 test version created by M.Sugiho
     v0.1.3 fits ver 0.4.3
     v0.1.4 fits ver 0.4.4
     v0.1.4 fits ver 0.4.5 support float 
     v0.1.6 support analog value : HXDHKFitsWrite 0.5.7 fits ver 0.4.5
     v0.1.7 support HISTGRAM
     v0.1.8 support no sorting : hxdhkFitsUtil 0.0.7
     v0.1.9 support event : hxdhkFitsUtil 0.0.9
     v0.2.0 for fits 0.4.7
     v0.2.1 for fits 0.4.7 (change #include)
     v0.3.0 for HEADAS 
     v0.4.0 the current HK file structure (M.K)
     v0.5.0 HXD_WxEVTLMBFSH -> HXD_WxTLMBFSH (M.K) (fitsin-0.8.2)
     v2.0.0 2006-09-10, new FFF format v2 by T.Kitaguchi
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "anl.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "fitsio.h"
#include "cfortran.h"
/*#include "pfile.h"*/
/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"


#include "HXD.h"
#include "hxdhkFitsUtil.h"
#include "hxdtrnFitsUtil.h"
#include "hxdtrnFitsToBnkUtil.h"
#include "hxdeventFitsUtil.h"
#include "hxdeventFitsToBnkUtil.h"

#define FILENAME_MAX_LENGTH 256

char HXDHKFitsRead_version[] = "version 0.5.0";

static char pname[] = "HXDHKFitsRead";

static struct {
  char filelistname[FILENAME_MAX_LENGTH];
  int colnum[HXD_TRN_FITS_KEY_NUM];
  int event_colnum[HXD_EVENT_FITS_KEY_NUM];
  int sort_flag;
}com;

static HxdHkFits fits;


void
HXDHKFitsRead_startup(int *status)
{
  *status = ANL_OK;
}


void
HXDHKFitsRead_com(int *status)
{

  char sort_flag_strings[32];
  int sort_flag;

  if ( *status ) { /* ftools */

    *status = 0;
    /*
    UCLGST("hxd_hk_fits_file_list_name", com.filelistname, *status);
    */
    *status = PILGetFname("hxd_hk_fits_file_list_name", com.filelistname);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }
    /*
    UCLGST("hxd_hk_fits_sort_flag", sort_flag_strings,*status);
    */
    *status = PILGetBool("hxd_hk_fits_sort_flag", &sort_flag);

    if( sort_flag == 1){
      com.sort_flag = HXD_HK_FITS_SORT;
    } else if( sort_flag == 0){
      com.sort_flag = HXD_HK_FITS_NO_SORT;
    } else {
      fprintf(stderr, "%s : no such answer : yes or no\n",
	      sort_flag_strings );
      *status = ANL_QUIT;
      exit(-1);
    }


    *status = ANL_OK;
    return;
  }

  com.filelistname[0]='\0';
  CLtxtrd("HXD HK fits file list name",
	  com.filelistname, sizeof(com.filelistname) );


  sort_flag_strings[0]='\0';

  CLtxtrd("HXD HK fits sorting with TIME",
	  sort_flag_strings, sizeof(sort_flag_strings) );

  if(!strcmp("yes", sort_flag_strings)){
    com.sort_flag = HXD_HK_FITS_SORT;
  } else if(!strcmp("no", sort_flag_strings)){
    com.sort_flag = HXD_HK_FITS_NO_SORT;
  } else {
    fprintf(stderr, "%s : no such answer : yes or no\n",
	    sort_flag_strings );
    *status = ANL_QUIT;
    exit(-1);
  }

  *status = ANL_OK;

}


void
HXDHKFitsRead_his(int *status)
{
  *status = ANL_OK;
}


void
HXDHKFitsRead_init(int *status)
{

  BnkDef( "HXD:ALL:EVENTTYPE", sizeof(int) );
  BnkDef( "HXD:ALL:PACKET_AETIME", sizeof(double) );

  HXDHKFitsReadHK_init();
  HXDHKFitsReadSYS_init();
  HXDHKFitsReadACU_init();
  HXDHKFitsReadSCL_init();
  HXDHKFitsReadRHK_init();
  HXDHKFitsReadSTM_init();
  HXDHKFitsReadPPR_init();
  HXDHKFitsReadPST_init();
  HXDHKFitsReadAET_HC_init();
  HXDHKFitsReadAET_SC_init();
  HXDHKFitsReadSFC_init();
  HXDHKFitsReadSFF1_init();
  HXDHKFitsReadSFF2_init();
  HXDHKFitsReadDLT_init();
  HXDHKFitsReadSP_PMT_init();
  HXDHKFitsReadSP_PIN_init();
  HXDHKFitsReadMEM_DMP_init();
  HXDHKFitsReadIO_DMP_init();
  HXDHKFitsReadECC_DMP_init();
  HXDHKFitsReadRECC_DMP_init();
  HXDHKFitsReadRIO_DMP_init();

  hxdtrnFitsToBnk_init();
  hxdeventFitsToBnk_init();

  *status = ANL_OK;
}


void
HXDHKFitsRead_bgnrun(int *status)
{

  int i;

  int istat = 0;

  hxdhkFits_init( com.filelistname, com.sort_flag, &fits, &istat );

  if( istat ){
    *status = ANL_QUIT;
    return;
  }

  for(i=0;i<fits.filenum;i++){

    /*printf("============%d===========\n",fits.filetype[i]);*/

    switch(fits.filetype[i]){
    case HXD_HK_FITS_FILETYPE_HK:
      if(HXDHKFitsReadHK_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadSYS_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadACU_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadSCL_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadRHK_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      break;
    case HXD_HK_FITS_FILETYPE_TBL:
      if(HXDHKFitsReadSTM_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadPPR_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadPST_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadAET_HC_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadAET_SC_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      break;
    case HXD_HK_FITS_FILETYPE_DMP:
      if(HXDHKFitsReadMEM_DMP_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadECC_DMP_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadIO_DMP_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadRECC_DMP_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadRIO_DMP_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      break;
    case HXD_HK_FITS_FILETYPE_HST:
      if(HXDHKFitsReadSFC_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadSFF1_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadSFF2_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadDLT_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadSP_PMT_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      if(HXDHKFitsReadSP_PIN_bgnrun(fits.fptr[i])){
	*status = ANL_QUIT; return;}
      break;
    case HXD_HK_FITS_FILETYPE_TRN:
      hxdtrnFits_col_num( fits.fptr[i], com.colnum, &istat );
      if( istat ){ *status = ANL_QUIT; return; }
      break;
    case HXD_HK_FITS_FILETYPE_WEL:
      hxdeventFits_col_num( fits.fptr[i], com.event_colnum, &istat );
      if( istat ){ *status = ANL_QUIT; return; }
      break;
    default:
      *status = ANL_QUIT;
      return;
    }
  }

  *status = ANL_OK;

}


void
HXDHKFitsRead_ana(int nevent, int eventid, int *status)
{

  int istat = 0;

  int filenum;
  int eventtype;
  int irow;
  double time;

  hxdhkFits_read( &filenum, &eventtype, &irow, &istat );

  if(istat){
    *status = ANL_QUIT;
    return;
  }

  /*printf("%d %x %d\n", filenum, eventtype, irow, istat );*/

  switch (eventtype){
  case HXD_EVTYP_HK:
    if(HXDHKFitsReadHK_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_SYS:
    if(HXDHKFitsReadSYS_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_ACU:
    if(HXDHKFitsReadACU_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_SCL:
    if(HXDHKFitsReadSCL_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_RHK:
    if(HXDHKFitsReadRHK_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_STM:
    if(HXDHKFitsReadSTM_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_PPR:
    if(HXDHKFitsReadPPR_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_PST:
    if(HXDHKFitsReadPST_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_AET_HC:
    if(HXDHKFitsReadAET_HC_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_AET_SC:
    if(HXDHKFitsReadAET_SC_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_MEM_DMP:
    if(HXDHKFitsReadMEM_DMP_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_ECC_DMP:
    if(HXDHKFitsReadECC_DMP_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_IO_DMP:
    if(HXDHKFitsReadIO_DMP_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_RECC_DMP:
    if(HXDHKFitsReadRECC_DMP_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_RIO_DMP:
    if(HXDHKFitsReadRIO_DMP_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_SFC:
    if(HXDHKFitsReadSFC_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_SFF1:
    if(HXDHKFitsReadSFF1_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_SFF2:
    if(HXDHKFitsReadSFF2_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_DLT:
    if(HXDHKFitsReadDLT_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_SP_PMT:
    if(HXDHKFitsReadSP_PMT_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_SP_PIN:
    if(HXDHKFitsReadSP_PIN_ana(fits.fptr[filenum], irow)){
      *status = ANL_QUIT; return; }break;
  case HXD_EVTYP_TRN:{
    HxdTrnFits data;
    hxdtrnFits_col_read( fits.fptr[filenum], irow, com.colnum, &data, &istat );
    if( istat ){ *status = ANL_QUIT; return; }
    hxdtrnFitsToBnk_put( &data );
    break;
  }
  case HXD_EVTYP_WEL:{
    HxdEventFits02 data;
    hxdeventFits_col_read( fits.fptr[filenum], irow, com.event_colnum,
			  &data, &istat );
    if( istat ){ *status = ANL_QUIT; return; }
    hxdeventFitsToBnk_put( &data );
    break;
  }
  default:
    *status = ANL_QUIT; return;
  }

  BnkfPutM( "HXD:ALL:EVENTTYPE", sizeof(int), &eventtype );

  *status = ANL_OK;

}


void
HXDHKFitsRead_endrun(int *status)
{
  *status = ANL_OK;
}


void
HXDHKFitsRead_exit(int *status)
{
  *status = ANL_OK;
}
