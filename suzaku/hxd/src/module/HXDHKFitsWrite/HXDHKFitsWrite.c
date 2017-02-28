/*     HXDHKFitsWrite                                          */
/*                        for HXD HK first Fits                */
/*     v 0.1.0  first version                                  */
/*     v 0.1.1  debug                                          */
/*     v 0.1.2  IBLOCK support                                 */
/*              dmp_flag support                               */
/*     v 0.1.3  comment support                                */
/*     v 0.1.4  use UINT, fits_copy_hdu debug!!                */
/*     v 0.1.5  use xord_flag num_position                     */
/*     v 0.2.3  uint -> int                                    */
/*     v 0.3.0  bst file name change, IBLOCK -> SEC_HEADER     */
/*     v 0.3.1  bst file name change, SEC_HEADER -> IBLOCK     */
/*     v 0.3.2  devide bst file                                */
/*     v 0.3.3  CCSDS_HEADER ETI                               */
/*     v 0.3.4  delete #define comment_flag                    */
/*     v 0.3.5  comment (each keyward) support                 */
/*     v 0.3.6  BenkPut "HXD:ALL:YYYYMMDD" "HXD:ALL:HHMMSS"    */
/*     v 0.3.7  long comment support                           */
/*     v 0.3.8  exchange flag support                          */
/*     v 0.3.9  comment change                                 */
/*                                                             */
/*     v 0.5.6  support float format for fits 0.4.5            */
/*     v 0.5.7                                                 */
/*     v 0.5.8  suppor TLMAX : fits 0.4.6                      */
/*     v 0.5.9  SCL add TIME TIME -> SCL_AETIME                */
/*     v 0.6.0  YYYYMMDD HHMMSS                                */
/*     v 0.6.1  separate _scl.hk file from .hk file.           */
/*     v 0.6.2  add CHECKSUM to _hxd_scl.hl                    */
/*     v 0.6.3  change SCL_AETIME -> HXD_SCL_AETIME            */
/*     v 0.6.4  add GSFC-request, based on v 0.6.1gsfc         */
/*     v 0.6.5  ask mk1stfits_dir, hxd_fits_key_dir            */
/*              not to get MK1STFITS_ENV, HXD_FITS_KEY_ENV     */
/*     v 0.7.0  for HXD-II format. add TI extension for well   */
/*     v 0.7.1  include SCL in hk, exclude RHK from hk         */
/*     v 0.7.2  add TI (HK,SYS,ACU)                            */
/*     v 0.7.3  change .fits --> .fitsin,  by Y.Tearda         */
/*     v 0.8.0  ASTE_ANL v1.40, add credit by Y.Terada         */
/*     v 0.8.1  atFunctions v2.2, astetool v1.26  by Y.Terada  */
/*     v 0.8.2  debug TSTART, TSTOP (PWH)  by Y.Terada         */
/*     v 0.8.3  debug PWH extension        by Y.Terada         */
/*     v 0.8.4  debug tlmax type           by Y.Terada  040502 */
/*     v 0.8.5  fill TIME of SCL/PWH by M.Suzu,Y.Terada 040915 */
/*     v 0.8.6  change file name, AEpacketFRFread:FILE_NAME    */
/*              to AEpacketRPTread:FILE_NAME.                  */
/*              by Y.Terada, R.Miyawaki 050426                 */
/*     v 0.9.0  delete ETI                 by Y.Terada 050517  */
/*     v 0.9.1  add DETNAM for SCL/PWH ext by Y.Terada 050519  */
/*              debug writeFITS            by Y.Terada 050520  */
/*              add S_TIME                 by Y.Terada 050520  */
/*     v 0.9.2  don't use malloc/free      by Y.Terada 050520  */
/*     v 0.9.3  change TIME meaning in PWH by Y.Terada 050524  */
/*     v 0.9.4  new FITS Header keywords   by Y.Terada 050626  */
/*     v 0.9.5  read TIM_FILE name         by Y.Terada 050627  */
/*     v 0.9.6  merge all HK files         by Y.Terada 051014  */
/*     v 0.9.7  avoid memory leakage       by Y.Terada 051018  */
/*     v 0.9.8  read leapsec file name     by Y.Terada 051019  */
/*     v 0.9.9  avoid memory leakage       by Y.Terada 051019  */
/*     v 1.0.0  version 1 release          by Y.Terada 051024  */
/*     v 1.0.1  reserve HEADER space       by Y.Terada 051104  */
/*     v 1.0.2  reserve HEADER space,debug by Y.Terada 051109  */
/*     v 2.0.0  avoid buffer overflow      by Y.Terada 060910  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "atFunctions.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "fitsio.h"
#include "aste_time.h"
#include "cfortran.h"

#include "HXD.h"

#include "hxdFitsHeaderUtil.h"
#include "hxdgtiFitsUtil.h"

#include "aeFitsHeaderUtil.h"

#define APID_MAX_NUMBER 22
#define KEYWORD_MAX_NUMBER 70

#define UNIT_MAX_LENGTH 16
#define FORMAT_MAX_LENGTH 8
#define BNK_MAX_LENGTH 32
#define KEYWORD_MAX_LENGTH 32
#define COMMENT_MAX_LENGTH 256
#define FITSNAME_MAX_LENGTH 16
#define FILENAME_MAX_LENGTH 256
/* #define HKFITS_MAX_TFIELD 600 */
#define KEY_MAX_LENGTH 64
#define SAMPLING_MAX_NUM 2

#define FLOAT_FLAG  0x80
#define DOUBLE_FLAG 0x40
#define INT_FLAG    0x20
#define UINT_FLAG   0x10
#define SHORT_FLAG  0x08
#define USHORT_FLAG 0x04
#define BYTE_FLAG   0x02
#define BIT_FLAG    0x01

#define DUMP_FLAG 1
#define NO_DUMP_FLAG 0
#define SAMPLING_FLAG -1

#ifdef BnkGet
#undef BnkGet
#endif

#define MK1STFITS_ENV        "MK1STFITS"
#define HXD_FITS_KEY_ENV     "HXD_KEY_DIR"

char HXDHKFitsWrite_version[] = "version 2.0.0";

static char *pname = "HXDHKFitsWrite";

#define MAX_DATA_LENGTH 4096

#define DEBUG 0
/** for DEBUG **/
static int callnum =0;
static int mallocnum =0;

enum {
  HK,SYS,RHK,ACU,SCL,PPR,PST,STM,
  IO_DMP,ECC_DMP,AET_HC,AET_SC,RIO_DMP,RECC_DMP,MEM_DMP,
  SFC,SFF1,SFF2,DLT,SP_PMT,SP_PIN, PWH,
  HXD_FITS_FILE_NUM
};

static  int morekeys[HXD_FITS_FILE_NUM] = {
  HXD_FITS_HEADER_RESERVE_HK_HK,
  HXD_FITS_HEADER_RESERVE_HK_SYS,
  HXD_FITS_HEADER_RESERVE_HK_RHK,
  HXD_FITS_HEADER_RESERVE_HK_ACU,
  HXD_FITS_HEADER_RESERVE_HK_SCL,
  HXD_FITS_HEADER_RESERVE_HK_PPR,
  HXD_FITS_HEADER_RESERVE_HK_PST,
  HXD_FITS_HEADER_RESERVE_HK_STM,
  HXD_FITS_HEADER_RESERVE_HK_IO_DMP,
  HXD_FITS_HEADER_RESERVE_HK_ECC_DMP,
  HXD_FITS_HEADER_RESERVE_HK_AET_HC,
  HXD_FITS_HEADER_RESERVE_HK_AET_SC,
  HXD_FITS_HEADER_RESERVE_HK_RIO_DMP,
  HXD_FITS_HEADER_RESERVE_HK_RECC_DMP,
  HXD_FITS_HEADER_RESERVE_HK_MEM_DMP,
  HXD_FITS_HEADER_RESERVE_HK_SFC,
  HXD_FITS_HEADER_RESERVE_HK_SFF1,
  HXD_FITS_HEADER_RESERVE_HK_SFF2,
  HXD_FITS_HEADER_RESERVE_HK_DLT,
  HXD_FITS_HEADER_RESERVE_HK_SP_PMT,
  HXD_FITS_HEADER_RESERVE_HK_SP_PIN,
  HXD_FITS_HEADER_RESERVE_HK_PWH
};

typedef struct {
  char bnkword[BNK_MAX_LENGTH];
  int bnk_arraynum;
  char format[FORMAT_MAX_LENGTH];
  unsigned char format_id;
  signed char dmp_flag;
  int keywordnum;
  char unit[UNIT_MAX_LENGTH];
  char sampling_num;
  char sampling_id[SAMPLING_MAX_NUM];  
}FITS;

typedef struct {
  char keyword[KEYWORD_MAX_LENGTH];
  char comment[COMMENT_MAX_LENGTH];
  unsigned int tlmax;
}KEY;

/** for Histogram FITS **/
static FITS fixFITS1[] ={
  { "HXD:ALL:PACKET_AETIME", 1, "1D", DOUBLE_FLAG, NO_DUMP_FLAG, 1,
      "s" }
  ,{ "HXD:ALL:YYYYMMDD", 1, "1J", INT_FLAG, NO_DUMP_FLAG, 1,
       "" }
  ,{ "HXD:ALL:HHMMSS", 1, "1J", INT_FLAG, NO_DUMP_FLAG, 1,
       "" }
  ,{ "HXD:ALL:PACKET_SEC_HEADER", 1, "1V", UINT_FLAG, NO_DUMP_FLAG, 1,
       "1/4096 s" } /*
  ,{ "HXD:ALL:ETI", 2, "2V", UINT_FLAG, DUMP_FLAG, 1,
  "" } */
  ,{ "ASTE:PACKET_S_TIME", 1, "1D",  DOUBLE_FLAG, NO_DUMP_FLAG, 1,
       "s" }
};

static KEY fixKEY1[] ={
  { "TIME", "Packet edit time" }
  ,{ "YYYYMMDD", "Year, Month, Day" }
  ,{ "HHMMSS", "Hour, Minute, Second" }
  ,{ "TI", "Secondary header time" } /*
  ,{ "ETI", "Extended secondary header time" } */
  ,{ "S_TIME", "Packet edit time assigned by SIRIUS." }
};

/** for HK FITS **/
static FITS fixFITS2[] ={
  { "HXD:ALL:PACKET_AETIME", 1, "1D", DOUBLE_FLAG, NO_DUMP_FLAG, 1,
      "s" }
  ,{ "HXD:ALL:YYYYMMDD", 1, "1J", INT_FLAG, NO_DUMP_FLAG, 1,
       "" }
  ,{ "HXD:ALL:HHMMSS", 1, "1J", INT_FLAG, NO_DUMP_FLAG, 1,
       "" }
  ,{ "HXD:ALL:CCSDS_HEADER", 6, "6B", BYTE_FLAG, DUMP_FLAG, 1, 
       "" }
  ,{ "HXD:ALL:PACKET_SEC_HEADER", 1, "1V", UINT_FLAG, NO_DUMP_FLAG, 1,
       "1/4096 s" } /*
  ,{ "HXD:ALL:ETI", 2, "2V", UINT_FLAG, DUMP_FLAG, 1, 
       "" }*/
  /* ,{ "ASTE:PACKET_ETI", 2, "2V", UINT_FLAG, DUMP_FLAG, 1,
     "" } */
  ,{ "ASTE:PACKET_S_TIME", 1, "1D",  DOUBLE_FLAG, NO_DUMP_FLAG, 1,
       "s" }
};

static KEY fixKEY2[] ={
  { "TIME", "Packet edit time" }
  ,{ "YYYYMMDD", "Year, Month, Day" }
  ,{ "HHMMSS", "Hour, Minute, Second" }
  ,{ "CCSDS_HEADER", "6 byte CCSDS header" }
  ,{ "TI", "secondary header time" } /*
  ,{ "ETI", "Extended secondary header time" } */
  ,{ "S_TIME", "Packet edit time assigned by SIRIUS" }
};


/** for SCL extension HK FITS **/
static FITS fixFITS3[] ={
/* { "HXDHKFitsWrite:SCL:TIME", 1, "1D", DOUBLE_FLAG, NO_DUMP_FLAG, 1,
      "s" }*/
   { "HXD:ALL:PACKET_AETIME", 1, "1D", DOUBLE_FLAG, NO_DUMP_FLAG, 1,
      "s" }
  ,{ "HXD:ALL:YYYYMMDD", 1, "1J", INT_FLAG, NO_DUMP_FLAG, 1,
       "" }
  ,{ "HXD:ALL:HHMMSS", 1, "1J", INT_FLAG, NO_DUMP_FLAG, 1,
       "" }
  ,{ "HXD:ALL:CCSDS_HEADER", 6, "6B", BYTE_FLAG, DUMP_FLAG, 1, 
       "" }
  ,{ "HXD:ALL:PACKET_SEC_HEADER", 1, "1V", UINT_FLAG, NO_DUMP_FLAG, 1,
       "1/4096 s" } /*
  ,{ "HXD:ALL:ETI", 2, "2V", UINT_FLAG, DUMP_FLAG, 1,
       "" } */
  ,{ "HXD:ALL:PACKET_AETIME", 1, "1D", DOUBLE_FLAG, NO_DUMP_FLAG, 1,
       "s" }  
  ,{ "ASTE:PACKET_S_TIME", 1, "1D",  DOUBLE_FLAG, NO_DUMP_FLAG, 1,
       "s" }
};

static KEY fixKEY3[] ={
  { "TIME", "time"}
  ,{ "YYYYMMDD", "Year, Month, Day" }
  ,{ "HHMMSS", "Hour, Minute, Second" }
  ,{ "CCSDS_HEADER", "6 byte CCSDS header" }
  ,{ "TI", "secondary header time" } /*
  ,{ "ETI", "Extended secondary header time" } */
  ,{ "HXD_SCL_AETIME", "Packet edit time (not an s_time)" }
  ,{ "HXD_SCL_S_TIME", "Packet edit time, s_time." }
};


/** for PWH extension HK FITS **/
static FITS fixFITS4[] ={
/*  { "HXDHKFitsWrite:PWH:TIME", 1, "1D", DOUBLE_FLAG, NO_DUMP_FLAG, 1,
    "s" } */
   { "ASTE:PACKET_S_TIME", 1, "1D", DOUBLE_FLAG, NO_DUMP_FLAG, 1,
    "s" } 
  ,{ "HXD:ALL:YYYYMMDD", 1, "1J", INT_FLAG, NO_DUMP_FLAG, 1,
       "" }
  ,{ "HXD:ALL:HHMMSS", 1, "1J", INT_FLAG, NO_DUMP_FLAG, 1,
       "" }
  ,{ "HXD:ALL:CCSDS_HEADER", 6, "6B", BYTE_FLAG, DUMP_FLAG, 1, 
       "" }
  ,{ "HXD:ALL:PACKET_SEC_HEADER", 1, "1V", UINT_FLAG, NO_DUMP_FLAG, 1,
       "1/4096 s" }  /*
  ,{ "HXD:ALL:ETI", 2, "2V", UINT_FLAG, DUMP_FLAG, 1,
       "" } */
  ,{ "HXD:ALL:PACKET_AETIME", 1, "1D", DOUBLE_FLAG, NO_DUMP_FLAG, 1,
       "s" }  
  ,{ "HXDHKFitsWrite:PWH:WPU_ID", 1, "1I", SHORT_FLAG, DUMP_FLAG, 1, 
  "" }
};

static KEY fixKEY4[] ={
  { "TIME", "time, same as S_TIME in Well event FITS."}
  ,{ "YYYYMMDD", "Year, Month, Day" }
  ,{ "HHMMSS", "Hour, Minute, Second" }
  ,{ "CCSDS_HEADER", "6 byte CCSDS header" }
  ,{ "TI", "secondary header time" } /*
  ,{ "ETI", "Extended secondary header time" } */
  ,{ "HXD_WEL_AETIME", "packet edit time" }
  ,{ "HXD_WEL_WPUID", "WPU ID" }
};


static struct {
  fitsfile *fp[APID_MAX_NUMBER];
  char fitsname[APID_MAX_NUMBER][FITSNAME_MAX_LENGTH];
  FITS *FITS[APID_MAX_NUMBER];
  KEY *KEY[APID_MAX_NUMBER];
  int nbnknum[APID_MAX_NUMBER];
  int ncolnum[APID_MAX_NUMBER];
  long irow[APID_MAX_NUMBER];
  char filename[FILENAME_MAX_LENGTH];
  char mk1stfits_dir[FILENAME_MAX_LENGTH];
  char hxd_fits_key_dir[FILENAME_MAX_LENGTH];
} com;


static HXD_STD_KEYS hkstdkeys;
static HXD_GTI gti;

static char *
basenam(char *p)
{
  char *pp;
  for (pp = p; *p; p++) {
    if ( *p == '/' ) {
      pp = p + 1;
    }
  }
  return(pp);
}


static void 
byte2bits(int b, char barray[8])
{
  int i;
  unsigned char mask=1;
  for (i=0; i<8; i++) {
    barray[i] = mask & b;
    b=b>>1;
  }
}

static int
reallocFITS(int index, int *ibnknum){
  
  (*ibnknum)++;
  
  com.FITS[index] = realloc(com.FITS[index],sizeof(FITS)*(*ibnknum));
  
  if( NULL == com.FITS[index]+(*ibnknum)){
    fprintf(stderr, "%s: realloc com.FITS[%s][%d] failed \n", pname ,
	    com.fitsname[index], *ibnknum);
    return ANL_NG;
  }
  
  return ANL_OK;
  
}

static void
fix_keyword(char *p)
{
  while ( *p ) {
    if ( *p == '.' ) *p = '_';
    p++;
  }
}

static int
reallocKEY(int index, int *icolnum, int keywordnum){
  
  int i;
  
  for(i=0;i<keywordnum;i++){
    
    (*icolnum)++;
    
    com.KEY[index] = realloc(com.KEY[index],sizeof(KEY)*(*icolnum));
    
    if( NULL == com.KEY[index]+(*icolnum)){
      fprintf(stderr, "%s: realloc com.KEY[%s][%d] failed \n", pname ,
	      com.fitsname[index], *icolnum);
      return ANL_NG;
    }
  }
  
  return ANL_OK;
  
}


static int
readFITS(int index, char *fits){
  FILE *fp;
  int ch;
  int ibnknum = 0;
  int icolnum = 0;
  FITS fitsbuf;
  KEY keybuf[KEYWORD_MAX_NUMBER];
  
  int i;
  
  fp = fopen(fits, "r");
  if ( NULL == fp ) {
    fprintf(stderr, "%s:  FITS '%s' open failed\n",pname,fits);
    return ANL_NG;
  }
  
  if(index == SFF1 || index == SFF2 || index == SFC ||
       index == DLT || index == SP_PMT || index == SP_PIN){
    for (i=0;i<sizeof(fixFITS1)/sizeof(*fixFITS1);i++){
      if(reallocFITS(index, &ibnknum)){
	return ANL_NG;
      }
      if(reallocKEY(index, &icolnum, fixFITS1[i].keywordnum)){
	return ANL_NG;
      }
      com.FITS[index][i] = fixFITS1[i];
      com.KEY[index][i] = fixKEY1[i];
    }
  } else if(index == SCL ){
    for (i=0;i<sizeof(fixFITS3)/sizeof(*fixFITS3);i++){
      if(reallocFITS(index, &ibnknum)){
	return ANL_NG;
      }
      if(reallocKEY(index, &icolnum, fixFITS3[i].keywordnum)){
	return ANL_NG;
      }
      com.FITS[index][i] = fixFITS3[i];
      com.KEY[index][i] = fixKEY3[i];
    }    
  } else if(index == PWH ){
    for (i=0;i<sizeof(fixFITS4)/sizeof(*fixFITS4);i++){
      if(reallocFITS(index, &ibnknum)){
	return ANL_NG;
      }
      if(reallocKEY(index, &icolnum, fixFITS4[i].keywordnum)){
	return ANL_NG;
      }
      com.FITS[index][i] = fixFITS4[i];
      com.KEY[index][i] = fixKEY4[i];
    }    
  } else {
    for (i=0;i<sizeof(fixFITS2)/sizeof(*fixFITS2);i++){
      if(reallocFITS(index, &ibnknum)){
	return ANL_NG;
      }
      if(reallocKEY(index, &icolnum, fixFITS2[i].keywordnum)){
	return ANL_NG;
      }
      com.FITS[index][i] = fixFITS2[i];
      com.KEY[index][i] = fixKEY2[i];
    }
  }
  
  
  
  ch = ' ';
  while ( EOF != ch ) {
    int ic, iw;
    char line[65536];
    char *word[256];
    
    /* read one line */
    ic = iw = 0;
    word[iw++] = line;
    for (;;) {
      ch = fgetc(fp);
      if ( EOF == ch || '\n' == ch ) {
	line[ic] = '\0';
	break;
      }
      if ( ic < sizeof(line)-1 ) {
	line[ic] = ch;
	if ( '\t' == ch ) {
	  line[ic] = '\0';
	  if ( iw < sizeof(word)/sizeof(*word) ) {
	    word[iw] = line + ic + 1;
	    iw++;
	  }
		}
	ic++;
      }
    }
    
    while ( iw < sizeof(word)/sizeof(*word) ) {
      word[iw] = "";
      iw++;
    }
    
    if ( word[0][0] ) {
      strcpy(fitsbuf.bnkword, word[0]);
    } else {
      continue;
    }
    
    if(word[1][0]){
      fitsbuf.bnk_arraynum = atoi(word[1]);
    } else {
      continue;
    }
    
    if ( word[2][0] ) {
      if(strchr(word[2],'D')!=NULL){
	fitsbuf.format_id = DOUBLE_FLAG;
      }
      else if(strchr(word[2],'E')!=NULL){
	fitsbuf.format_id = FLOAT_FLAG;	
      }
      else if (strchr(word[2],'X')!=NULL){
	fitsbuf.format_id = BIT_FLAG;
      }
      else if (strchr(word[2],'B')!=NULL){
	fitsbuf.format_id = BYTE_FLAG;		
      }
      else if (strchr(word[2],'I')!=NULL){
	fitsbuf.format_id = SHORT_FLAG;		
      }
      else if (strchr(word[2],'J')!=NULL){
	fitsbuf.format_id = INT_FLAG;		
      }
      else if (strchr(word[2],'U')!=NULL){
	fitsbuf.format_id = USHORT_FLAG;
      }
      else if (strchr(word[2],'V')!=NULL){
	fitsbuf.format_id = UINT_FLAG;
      }
      strcpy(fitsbuf.format, word[2]);
    } else {
      continue;
    }
    
    if ( word[3][0] ) {
      fitsbuf.dmp_flag = atoi(word[3]);
      fitsbuf.sampling_num = 0;
      
      if( fitsbuf.dmp_flag != DUMP_FLAG ){
	fitsbuf.keywordnum = fitsbuf.bnk_arraynum;
	for(i=0;i<fitsbuf.keywordnum;i++){
	  keybuf[i].tlmax = 0;
	}
	
	if( fitsbuf.dmp_flag == SAMPLING_FLAG ){
	  if(strchr(fitsbuf.bnkword,'0')!=NULL){
	    fitsbuf.sampling_num ++;
	    fitsbuf.sampling_id[fitsbuf.sampling_num-1]=0;
	  }
	  if(strchr(fitsbuf.bnkword,'1')!=NULL){
	    fitsbuf.sampling_num ++;
	    fitsbuf.sampling_id[fitsbuf.sampling_num-1]=1;
	  }
	  if(strchr(fitsbuf.bnkword,'2')!=NULL){
	    fitsbuf.sampling_num ++;
	    fitsbuf.sampling_id[fitsbuf.sampling_num-1]=2;
	  }
	  if(strchr(fitsbuf.bnkword,'3')!=NULL){
	    fitsbuf.sampling_num ++;
	    fitsbuf.sampling_id[fitsbuf.sampling_num-1]=3;
	  }
	  if(strchr(fitsbuf.bnkword,'4')!=NULL){
	    fitsbuf.sampling_num ++;
	    fitsbuf.sampling_id[fitsbuf.sampling_num-1]=4;
	  }
	  if(strchr(fitsbuf.bnkword,'5')!=NULL){
	    fitsbuf.sampling_num ++;
	    fitsbuf.sampling_id[fitsbuf.sampling_num-1]=5;
	  }
	  if(strchr(fitsbuf.bnkword,'6')!=NULL){
	    fitsbuf.sampling_num ++;
	    fitsbuf.sampling_id[fitsbuf.sampling_num-1]=6;
	  }
	  if(strchr(fitsbuf.bnkword,'7')!=NULL){
	    fitsbuf.sampling_num ++;
	    fitsbuf.sampling_id[fitsbuf.sampling_num-1]=7;
	  }
	} else if ( fitsbuf.dmp_flag != NO_DUMP_FLAG ){
	  fitsbuf.dmp_flag = NO_DUMP_FLAG;
	  for(i=0;i<fitsbuf.keywordnum;i++){
	    keybuf[i].tlmax = (0x01 << (atoi(word[3]))) -1;
	  }
	}
	
      } else {
	fitsbuf.keywordnum = 1;
	keybuf[0].tlmax = 0;
      }
    } else {
      continue;
    }
	
    if ( word[4][0] ) {
      strcpy(fitsbuf.unit, word[4]);
    } else {
      strcpy(fitsbuf.unit, "");
    }
    
    for(i=0;i<fitsbuf.keywordnum;i++){
      if(word[5+i][0]){
	strcpy(keybuf[i].keyword,word[5+i]);
      } else {
	strcpy(keybuf[i].keyword,"");
      }
      /* debug */
      fix_keyword(keybuf[i].keyword);
    }
    
    for(i=0;i<fitsbuf.keywordnum;i++){
      if(word[5+fitsbuf.keywordnum+i][0]){
	strcpy(keybuf[i].comment,word[5+fitsbuf.keywordnum+i]);
      } else {
	strcpy(keybuf[i].comment,"");
      }
    }
    
    reallocFITS(index, &ibnknum);
    
    reallocKEY(index, &icolnum, fitsbuf.keywordnum);
    
    com.FITS[index][ibnknum-1] = fitsbuf;
    
    for(i=0;i<fitsbuf.keywordnum;i++){
      com.KEY[index][icolnum-fitsbuf.keywordnum+i] = keybuf[i];
    }
    
  }
  
  com.ncolnum[index]=icolnum;
  com.nbnknum[index]=ibnknum;
  /*
     printf("DEBUG:READFITS-COLNAME:%s (bnkcolnum=%d)\n",
     fits,com.nbnknum[index]);
     icolnum = 0;
     
     for(ibnknum = 0;ibnknum<com.nbnknum[index];ibnknum++){
     printf("DEBUG:READFITS-COLNAME:%d:%d:|%s|%s|%s|",index,i,
	       com.FITS[index][ibnknum].bnkword,
	       com.FITS[index][ibnknum].format,
	       com.FITS[index][ibnknum].unit
	       );
	       
	       for (i=0;i<com.FITS[index][ibnknum].keywordnum;i++){
	       printf("%s|",com.KEY[index][icolnum].keyword);
	       printf("%s|",com.KEY[index][icolnum].comment);
	       icolnum++;
	       }
	       
	       printf("\n");
	       }
*/    
  fclose(fp);
  return ANL_OK;
}


static int
createFITS(int index,char *extname)
{
  
  int istat = 0;
  
  int icolnum = 0;
  int ibnknum;
  int ikeywordnum;
  
  char **ttype;
  char **tform;
  char **tunit;
  char **comment;
  int  *tlmax;
  
  int tbltype = BINARY_TBL;
  long naxis2 = 0;
  int tfield = com.ncolnum[index];
  
  int i,j;

  int hdunum; 

  ttype   = malloc(sizeof(char *)*tfield);
  tform   = malloc(sizeof(char *)*tfield);
  tunit   = malloc(sizeof(char *)*tfield);
  comment = malloc(sizeof(char *)*tfield);
  tlmax   = malloc(sizeof(int)   *tfield);

  if ( ttype == NULL || tform == NULL || tunit == NULL || comment == NULL || tlmax == NULL ){
    fprintf(stderr, "%s: malloc failed (tfield=%d)\n", pname, tfield);
    return ANL_NG;
  }
  
  for(ibnknum = 0;ibnknum < com.nbnknum[index];ibnknum++){
    
    for(ikeywordnum = 0;ikeywordnum< com.FITS[index][ibnknum].keywordnum;
	ikeywordnum++){
      char **ttp = ttype   + icolnum;
      char **tfp = tform   + icolnum;
      char **tup = tunit   + icolnum;
      char **cmp = comment + icolnum;
      int  *tlp  = tlmax   + icolnum;
      
      *tfp = com.FITS[index][ibnknum].format;
      *tup = com.FITS[index][ibnknum].unit;
      *ttp = com.KEY[index][icolnum].keyword;
      *cmp = com.KEY[index][icolnum].comment;
      tlp  = &com.KEY[index][icolnum].tlmax;

      icolnum++;
    }
  }
  /*    
     printf("DEBUG: FITS TABLE EXT: %s tfield=%d\n",com.fitsname[index],tfield);
     for (i = 0; i< tfield; i++) {
     printf("DEBUG:COLUMN:%d:%d:|%s|%s|%s|%s|\n",index,i,
     ttype[i], tform[i], tunit[i],comment[i]);
     }
     */    
  
  fits_create_tbl(com.fp[index], tbltype, naxis2, tfield,
		  ttype, tform, tunit, extname, &istat);
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_create_tbl %s failed (%d)\n", pname,
	    com.fitsname[index], istat);
    return ANL_NG;
  }

  fits_set_hdrsize(com.fp[index], morekeys[index], &istat);
  if ( istat ) {
    fprintf(stderr, "%s: Error in fits_set_hdrsize(%d) (status=%d)\n", 
            pname, morekeys[index], istat);
    return ANL_NG;
  }
  
  fits_write_comment(com.fp[index], "\
Number of possible values of the parameters and meanings of these \
values are indicated in the square brackets on the comment field. \
For example, if a parameter has [2 1:ON 0:OFF] on the comment field, \
this parameter takes the value either 1 (for ON) or 0 (for OFF).", &istat);
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_write_comment failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  if(DEBUG) fprintf(stderr, "come here NUM=2 index=%d\n", index);
  /** uninitialized when index=5 PPR**/
  hdunum = 2;  
  if (hxdFitsHeader_writeHXDStdKeys(com.fp[index], hkstdkeys, hdunum, &istat)
      != ANL_OK) {
    fprintf(stderr,"%s: Error in writeHXDStdKey() 2 (status=%d)\n",pname,
	    istat);
    return ANL_NG;
  } else {
    int i,j;
    char **ttypename; 
/*  char ttypename[KEY_MAX_LENGTH][HKFITS_MAX_TFIELD]; */

    hkstdkeys.hduclass_ev_hk = HXD_FITS_HEADER_UTIL_HDUCLASS_HK;

    ttypename = malloc (sizeof(char *)*tfield); 
    
    for(i=0;i<tfield;i++){
      ttypename[i] = malloc( sizeof(char)*(KEY_MAX_LENGTH + 1)); 
      sprintf(ttypename[i], "TTYPE%-3d",i+1);      
    }
    
    hxdFitsComment_write( com.fp[index], tfield, ttypename, comment, &istat );
    
    for(i=0;i<tfield;i++){
      
      char tmp_card[81];
      char min_card[81];
      char max_card[81];
      
      if(com.KEY[index][i].tlmax){
	sprintf(min_card, "TLMIN%-3d= %20u / minimum legal value",i+1, 0);
	sprintf(max_card, "TLMAX%-3d= %20u / maximum legal value",i+1,
		com.KEY[index][i].tlmax);
	
	for(j=1;;j++){
	  if( fits_read_record(com.fp[index], j, tmp_card, &istat) ) {
	    istat = 0;
	    break;
	  }
	  if(0 == strncmp(ttypename[i], tmp_card, KEY_MAX_LENGTH)){
	    fits_insert_record(com.fp[index], j+1, min_card, &istat);
	    fits_insert_record(com.fp[index], j+2, max_card, &istat);	    
	    if ( istat ) {
	      fprintf(stderr, "%s:fits_insert_record failed (%d)\n",
		      pname, istat);
	      break;
	    }
	    
	    if (fits_flush_file(com.fp[index], &istat)){
	      fprintf(stderr,
		      "%s:fits_flush_file failed(%d)\n", istat);
	    }    
	    
	  }
	}	
      }
      
    }
    /** --- memory free ---**/
    for(i=0;i<tfield;i++){ free(ttypename[i]); }
    free(ttypename);
    /** ------ free ------ **/

    if(istat){
      return ANL_NG;
    }
  }
  
  free(ttype);
  free(tform);
  free(tunit);
  free(comment);
  free(tlmax);
  
  return ANL_OK;
  
}

static int
createFITSFILE(char *fitsname, int index, char *filename){
  
  char fitsfilename[FILENAME_MAX_LENGTH];
  char *fits;
  char extname[32];
  
  int istat = 0;
  
  int i;
  
  int bitpix = 8;
  int naxis = 0;
  long *naxes = { 0 };
  int hdunum; 
  int morekeys_primary;

/*    
  char * MK1STFITS_DIR    = getenv(MK1STFITS_ENV); 
  char * HXD_FITS_KEY_DIR = getenv(HXD_FITS_KEY_ENV);
*/      
/*
  if ( mk1stfits_dir == NULL ) {
    fprintf(stderr,"%s: ERROR: %snot defined\n", pname, MK1STFITS_ENV);
    exit (1);
  }
  if ( HXD_FITS_KEY_DIR  == NULL ) {
    fprintf(stderr,"%s: ERROR: %s not defined\n", pname, HXD_FITS_KEY_ENV);
    exit (1);
  }
*/

  fits = (char*) malloc(strlen(com.mk1stfits_dir) 
			+ strlen(com.hxd_fits_key_dir)
			+ strlen(fitsname) 
			+ strlen("//.fitsin_PADDING") );
  sprintf(fits,"%s/%s/%s.fitsin", com.mk1stfits_dir, com.hxd_fits_key_dir, 
	  fitsname);
  /*
   sprintf(fits,"%s/%s/%s.fits", com.mk1stfits_dir, com.hxd_fits_key_dir, fitsname);*/

  strcpy(com.fitsname[index],fitsname);
  strcpy(fitsfilename, com.filename);
  strcat(fitsfilename, "_hxd");
  strcat(fitsfilename, filename);
  
  strcat(fitsfilename, ".hk");
  unlink(fitsfilename);
  
  strcpy(extname,"HXD_");
  strcat(extname, fitsname);
  
  fits_create_file(&com.fp[index], fitsfilename, &istat);
  
  if ( istat ) {
    fprintf(stderr, "%s: fits_create_file %s failed (%d)\n", pname,
	    fitsname, istat);
    return ANL_NG;
  }
  
  fits_create_img(com.fp[index], bitpix, naxis, naxes, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: fits_create_img %s failed (%d)\n", pname,
	    fitsname, istat);
    return ANL_NG;
  }

  morekeys_primary = HXD_FITS_HEADER_RESERVE_HK_PRI;
  fits_set_hdrsize(com.fp[index], morekeys_primary, &istat);
  if ( istat ) {
    fprintf(stderr, "%s: Error in fits_set_hdrsize(%d) (status=%d)\n", 
	    pname,  morekeys_primary, istat);
    return ANL_NG;
  }


  if ( index == SCL || index == PWH) {
    hkstdkeys.use_detnam=1;
    /* hkstdkeys.detnam=strdup("WELL"); */
    sprintf(hkstdkeys.detnam, "WELL");
  } else {
    hkstdkeys.use_detnam=0;
  }

  hdunum = 1;
  if (hxdFitsHeader_writeHXDStdKeys(com.fp[index], hkstdkeys, hdunum, &istat)
      != ANL_OK) {
    fprintf(stderr,"%s: Error in writeHXDStdKey() 1 (status=%d)\n",pname,
	    istat);
    return ANL_NG;
  }

  com.irow[index] = 0;
  
  if( readFITS(index,fits) ){return ANL_NG;}
  
  if( createFITS(index,extname) ){return ANL_NG;}
  
  free(fits);

  return ANL_OK;
}


static int
writeFITS(int index){
  
  int used;
  int ibnknum = 0;
  int icolnum = 1;
  int istat = 0;
  int ifail = 0;
  int ikeywordnum;

#if 0
  static int total_write_num = 0;
#define FLUSH_FITS_CALL_NUM 20000

  total_write_num ++;
  if (total_write_num > FLUSH_FITS_CALL_NUM){
    int files;
    if (DEBUG) fprintf(stdout, "HK FITS flushed.\n");
    total_write_num = 0;
    for(files=0;files<HXD_FITS_FILE_NUM;files++){
      if (fits_flush_file(com.fp[files], &istat)){
	fprintf(stderr,
		"Error doing fits_flush_file() (status=%d)\n", istat);
	return ANL_NG;
      }
    }
  }
#endif 

  if (DEBUG) {
    callnum ++;
    if (callnum>70440)
      fprintf(stderr, "HK write FITS No.%d come here (%d)\n", 
	      index, callnum);
  }
  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS start..\n");

  com.irow[index]++;
    
  while( ibnknum<com.nbnknum[index] ){

    if (DEBUG && (callnum>70440 || index==19) )
      fprintf(stderr, "ibnknum=%d -----> com.nbnknum[index=%d]=%d)\n", 
	      ibnknum, index, com.nbnknum[index]);

    /**====== (1) Dump ====== **/
    if(com.FITS[index][ibnknum].dmp_flag == DUMP_FLAG){
      if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP\n");

      /**------(1-1) Dump, double ------- **/
      if(com.FITS[index][ibnknum].format_id == DOUBLE_FLAG){
	/* 	double *double_data; */
	double double_data[MAX_DATA_LENGTH];
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP Double\n");
	if (DEBUG) mallocnum ++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (double) %d , ", mallocnum);
	/*
	double_data = malloc(sizeof(double)*
	com.FITS[index][ibnknum].bnk_arraynum); */
	BnkGet(com.FITS[index][ibnknum].bnkword,
	       sizeof(double)*com.FITS[index][ibnknum].bnk_arraynum,
	       &used, double_data );
	
	fits_write_col_dbl(com.fp[index], icolnum, com.irow[index],
			   1, com.FITS[index][ibnknum].bnk_arraynum,
			   double_data, &istat);
	if(istat){
	  fprintf(stderr,
		  "%s: fits_write_col %s : %s failed (%d)\n",
		  pname, com.fitsname[index],
		  com.KEY[index][icolnum].keyword, istat);
	  istat = 0;
	  ifail++;
	}
	
	icolnum++;
	ibnknum++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(double)\n");
	/* free(double_data); */
      /**------(1-2) Dump, float ------- **/
      } else  if(com.FITS[index][ibnknum].format_id == FLOAT_FLAG){
	int i;
	/*
	double *double_data;
	float *float_data;
	*/
	double double_data[MAX_DATA_LENGTH];
	float  float_data[MAX_DATA_LENGTH];

	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP Flt\n");
	if (DEBUG) mallocnum ++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (double), %d  ", mallocnum);
	/*
	double_data = malloc(sizeof(double)*
	com.FITS[index][ibnknum].bnk_arraynum); */
	BnkGet(com.FITS[index][ibnknum].bnkword,
	       sizeof(double)*com.FITS[index][ibnknum].bnk_arraynum,
	       &used, double_data );
	
	if (DEBUG) mallocnum ++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (float), %d ", mallocnum);
	/*
	float_data =
	malloc(sizeof(float)*com.FITS[index][ibnknum].bnk_arraynum); */
	
	for(i=0;i<com.FITS[index][ibnknum].bnk_arraynum;i++){
	  float_data[i] = (float)double_data[i];
	}
	
	fits_write_col_flt(com.fp[index], icolnum, com.irow[index],
			   1, com.FITS[index][ibnknum].bnk_arraynum,
			   float_data, &istat);
	if(istat){
	  fprintf(stderr,
		  "%s: fits_write_col %s : %s failed (%d)\n",
		  pname, com.fitsname[index],
		  com.KEY[index][icolnum].keyword, istat);
	  istat = 0;
	  ifail++;
	}
	
	icolnum++;
	ibnknum++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(double), ");
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(float)\n");
	/*
	free(double_data);
	free(float_data); 
	*/
      /**------(1-3) Dump, others ------- **/
      } else {
	/* unsigned int *data; */
	unsigned int data[MAX_DATA_LENGTH];

	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP oth\n");
	if (DEBUG) mallocnum ++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "malloc size = %d\n",
				     com.FITS[index][ibnknum].bnk_arraynum);
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (data), %d  ", mallocnum);
	/*
	  data = malloc(sizeof(int)*com.FITS[index][ibnknum].bnk_arraynum); */
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "come here (1) \n");
	
	BnkGet(com.FITS[index][ibnknum].bnkword,
	       sizeof(int)*com.FITS[index][ibnknum].bnk_arraynum,
	       &used,data );
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "come here (2) \n");
	
	/**------(1-3a) Dump, byte ------- **/
	if( com.FITS[index][ibnknum].format_id == BYTE_FLAG ){
	  /* unsigned char *char_data; */
	  unsigned char  char_data[MAX_DATA_LENGTH];

	  int i;
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP BYTE\n");
	if (DEBUG) mallocnum ++;
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (char), %d ", mallocnum);
	  /*
	  char_data = malloc(sizeof(char)*
	  com.FITS[index][ibnknum].bnk_arraynum); */
	  
	  for(i=0;i<com.FITS[index][ibnknum].bnk_arraynum;i++){
	    if( data[i] & 0xffffff00 ){
	      printf("%s: data size overflow ( >char ) %s : %s ",
		     pname,com.fitsname[index],
		     com.KEY[index][icolnum].keyword);
	    }
	    char_data[i] = (unsigned char)data[i];
	  }
	  
	  fits_write_col_byt(com.fp[index], icolnum, com.irow[index],
			     1,com.FITS[index][ibnknum].bnk_arraynum,
			     char_data, &istat);
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(char)\n");
	  /*	  free(char_data); */
	/**------(1-3b) Dump, short ------- **/
	} else if ( com.FITS[index][ibnknum].format_id == SHORT_FLAG ){
	  /* short *short_data; */
	  short short_data[MAX_DATA_LENGTH];
	  int i;
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP SHORT\n");
	if (DEBUG) mallocnum ++;
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (short), %d ", mallocnum);
	  /*
	  short_data = malloc(sizeof(short)*
			      com.FITS[index][ibnknum].bnk_arraynum);
	  */
	  for(i=0;i<com.FITS[index][ibnknum].bnk_arraynum;i++){
	    if( data[i] & 0xffff0000 ){
	      printf("%s: data size overflow ( >short ) %s : %s "
		     ,pname,com.fitsname[index],
		     com.KEY[index][icolnum].keyword);
	    }
	    short_data[i] = (short)data[i];
	  }
	  
	  fits_write_col_sht(com.fp[index], icolnum, com.irow[index],
			     1,com.FITS[index][ibnknum].bnk_arraynum,
			     short_data, &istat);
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(short)\n");
	  /* free(short_data); */
	/**------(1-3c) Dump, ushort ------- **/
	} else if ( com.FITS[index][ibnknum].format_id == USHORT_FLAG ){
	  /* unsigned short *ushort_data; */
	  unsigned short ushort_data[MAX_DATA_LENGTH];
	  int i;
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP U SHORT\n");
	  if (DEBUG) mallocnum ++;
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (ushort), %d ", mallocnum);
	  /*
	  ushort_data = malloc(sizeof(short)*
	  com.FITS[index][ibnknum].bnk_arraynum);*/
	  
	  for(i=0;i<com.FITS[index][ibnknum].bnk_arraynum;i++){
	    if( data[i] & 0xffff0000 ){
	      printf("%s: data size overflow ( >short ) %s : %s "
		     ,pname,com.fitsname[index],
		     com.KEY[index][icolnum].keyword);
	    }
	    ushort_data[i] = (unsigned short)data[i];
	  }
	  
	  fits_write_col_usht(com.fp[index], icolnum, com.irow[index], 1,
			      com.FITS[index][ibnknum].bnk_arraynum,
			      ushort_data, &istat);
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(ushort)\n");
	  /*	  free(ushort_data);		     */
	/**------(1-3d) Dump, int ------- **/
	} else if ( com.FITS[index][ibnknum].format_id == INT_FLAG ){
	  /* int *int_data; */
	  int int_data[MAX_DATA_LENGTH];
	  int i;
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP INT\n");
	  if (DEBUG) mallocnum ++;
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (int), %d ", mallocnum);
	  /*
	  int_data = malloc(sizeof(int)*
	  com.FITS[index][ibnknum].bnk_arraynum);*/
	  
	  for(i=0;i<com.FITS[index][ibnknum].bnk_arraynum;i++){
	    int_data[i] = (int) data[i];
	  }
	  
	  fits_write_col_int(com.fp[index], icolnum, com.irow[index], 1,
			     com.FITS[index][ibnknum].bnk_arraynum,
			     int_data, &istat);
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(int)\n");
	  /* free(int_data); */

	/**------(1-3e) Dump, bit ------- **/
	} else if ( com.FITS[index][ibnknum].format_id == BIT_FLAG ){

	  int i, j, num, nbits;
	  char bits[8];
	  /*  char * bit_data; */
	  char bit_data[MAX_DATA_LENGTH];

	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP BIT\n");

	  num = com.FITS[index][ibnknum].bnk_arraynum;
	  nbits = num * 8;
	  if (DEBUG) mallocnum ++;
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (bit), %d ", mallocnum);
	  /* 	  bit_data = malloc(sizeof(char)*nbits); */
	  
	  for(i=0;i< num; i++){
	    byte2bits(data[i],bits);
	    for (j=0;j<8;j++) bit_data[i*8+j] = bits[j];
	  }
	  
	  fits_write_col_bit(com.fp[index], icolnum, com.irow[index], 1,
			     nbits, bit_data, &istat);

	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(bit)\n");
	  /* free(bit_data); */

	/**------(1-3f) Dump, uint ------- **/
	} else {
	  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS DMP UINT\n");
	  fits_write_col_uint(com.fp[index], icolnum, com.irow[index], 1,
			      com.FITS[index][ibnknum].bnk_arraynum,
			      data, &istat);
	}
	
	if(istat){
	  fprintf(stderr, "%s: fits_write_col %s : %s failed (%d)\n",
		  pname, com.fitsname[index],
		  com.KEY[index][icolnum].keyword, istat);
	  istat = 0;
	  ifail++;
	}
	
	icolnum++;
	ibnknum++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(data)\n");
	/* free(data); */
      }

    /**====== (2) Sampling ====== **/
    } else if (com.FITS[index][ibnknum].dmp_flag == SAMPLING_FLAG){


      int data_id;
      
      if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS SMP\n");
      BnkGet( "HXD:HKA:DATA_ID", sizeof(int), &used, &data_id);
      
      /**------(2-1) Sampling, float ------- **/
      if(com.FITS[index][ibnknum].format_id == FLOAT_FLAG){
	
	int i;
	int valid_flag=0;
	/*
	double *double_data;
	*/
	double double_data[MAX_DATA_LENGTH];

	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS SMP FLOAT\n");
	for(i=0;i<com.FITS[index][ibnknum].sampling_num;i++){
	  /*fprintf(stderr,"%d %d\n",com.FITS[index][ibnknum].sampling_id[i],
		  data_id);*/
	  if( com.FITS[index][ibnknum].sampling_id[i] == data_id ){	    
	    valid_flag++;
	  }
	}
	
	if (DEBUG) mallocnum ++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (double), %d ", mallocnum);
	/* 
	double_data = malloc(sizeof(double)*
	com.FITS[index][ibnknum].bnk_arraynum); */
	BnkGet(com.FITS[index][ibnknum].bnkword,
	       sizeof(double)*com.FITS[index][ibnknum].bnk_arraynum,
	       &used, double_data );
	ikeywordnum = 0;
	
	while(ikeywordnum<com.FITS[index][ibnknum].keywordnum){
	  
	  float float_data;
	  
	  float_data = (float)double_data[ikeywordnum];
	  
	  if(valid_flag){
	    fits_write_col_flt(com.fp[index], icolnum, com.irow[index],
			       1, 1, &float_data,&istat);    
	  } else {
	    /*fprintf(stderr,"====%d=====\n",com.irow[index]);*/
	    fits_write_col_null(com.fp[index], icolnum, com.irow[index],
				1, 1, &istat);
	  }
	  if(istat){
	    fprintf(stderr,
		    "%s: fits_write_col %s : %s failed (%d)\n",
		    pname, com.fitsname[index],
		    com.KEY[index][icolnum].keyword, istat);
	    istat = 0;
	    ifail++;
	  }
	  ikeywordnum++;
	  icolnum++;
	}
	ibnknum++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(double)\n");
	/* free(double_data);	 */
	
      }

    /**====== (3) Others ====== **/
    } else {

      if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS OTH\n");

      /**------(3-1) Others, double ------- **/
      if(com.FITS[index][ibnknum].format_id == DOUBLE_FLAG){
	/*
	double *double_data;
	*/
	double double_data[MAX_DATA_LENGTH];

	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS OTH DOUBLE\n");
	if (DEBUG) mallocnum ++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (double), %d ", mallocnum);
	/*
	double_data = malloc(sizeof(double)*
	com.FITS[index][ibnknum].bnk_arraynum); */
	BnkGet(com.FITS[index][ibnknum].bnkword,
	       sizeof(double)*com.FITS[index][ibnknum].bnk_arraynum,
	       &used, double_data );
	ikeywordnum = 0;
	
	while(ikeywordnum<com.FITS[index][ibnknum].keywordnum){
	  fits_write_col_dbl(com.fp[index], icolnum, com.irow[index],
			     1, 1, double_data + ikeywordnum,&istat);
	  if(istat){
	    fprintf(stderr,
		    "%s: fits_write_col %s : %s failed (%d)\n",
		    pname, com.fitsname[index],
		    com.KEY[index][icolnum].keyword, istat);
	    istat = 0;
	    ifail++;
	  }
	  ikeywordnum++;
	  icolnum++;
	}
	ibnknum++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(double)\n");
	/*
	  free(double_data); */
	
      /**------(3-2) Others, float ------- **/
      } else if(com.FITS[index][ibnknum].format_id == FLOAT_FLAG){
	/*
	  double *double_data;*/
	double double_data[MAX_DATA_LENGTH];

	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS OTH FLOAT\n");
	if (DEBUG) mallocnum ++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (double), %d ", mallocnum);
	/*
	double_data = malloc(sizeof(double)*
	com.FITS[index][ibnknum].bnk_arraynum); */
	BnkGet(com.FITS[index][ibnknum].bnkword,
	       sizeof(double)*com.FITS[index][ibnknum].bnk_arraynum,
	       &used, double_data );
	ikeywordnum = 0;
	
	while(ikeywordnum<com.FITS[index][ibnknum].keywordnum){
	  
	  float float_data;
	  
	  float_data = (float)double_data[ikeywordnum];
	  
	  fits_write_col_flt(com.fp[index], icolnum, com.irow[index],
			     1, 1, &float_data,&istat);
	  if(istat){
	    fprintf(stderr,
		    "%s: fits_write_col %s : %s failed (%d)\n",
		    pname, com.fitsname[index],
		    com.KEY[index][icolnum].keyword, istat);
	    istat = 0;
	    ifail++;
	  }
	  ikeywordnum++;
	  icolnum++;
	}
	ibnknum++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(double)\n");
	/*
	  free(double_data); */
	
      /**------(3-3) Others, others ------- **/
      } else {
	/*
	unsigned int *data;
	*/
	unsigned int data[MAX_DATA_LENGTH];

	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS OTH oth\n");
	if (DEBUG) mallocnum ++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " malloc (data), %d ", mallocnum);
	/*
	data = malloc(sizeof(int)*
	com.FITS[index][ibnknum].bnk_arraynum); */
	BnkGet(com.FITS[index][ibnknum].bnkword, 
		   sizeof(int) * com.FITS[index][ibnknum].bnk_arraynum,
	       &used, data);
	ikeywordnum = 0;
	
	while(ikeywordnum<com.FITS[index][ibnknum].keywordnum){
	  
	  /**------(3-3a) Others, ushort------- **/
	  if( com.FITS[index][ibnknum].format_id == USHORT_FLAG ){
	    unsigned short ushort_data;
	    if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS OTH U SHORT\n");
	    ushort_data = (unsigned short) data[ikeywordnum];
	    
	    /*
	       printf("%d %d %s %d\n", icolnum, com.irow[index],
	       com.FITS[index][ibnknum].bnkword,
	       ushort_data);
	       */
	    
	    fits_write_col_usht(com.fp[index], icolnum, com.irow[index],
				1, 1, &ushort_data, &istat);
	    if(istat){
	      fprintf(stderr,
		      "%s: fits_write_col %s : %s failed (%d)\n",
		      pname, com.fitsname[index],
		      com.KEY[index][icolnum].keyword, istat);
	      istat = 0;
	  }
	    
	  /**------(3-3b) Others, uint ------ **/
	  } else if ( com.FITS[index][ibnknum].format_id == UINT_FLAG ){
	    if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS OTH U INT\n");
	    fits_write_col_uint(com.fp[index], icolnum, com.irow[index],
				1, 1, data+ikeywordnum, &istat);

	  /**------(3-3c) Others, bit ------ **/
	  } else if ( com.FITS[index][ibnknum].format_id == BIT_FLAG ){
	    char bit_data = data[ikeywordnum];

	    if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS OTH BIT\n");
            fits_write_col_bit(com.fp[index], icolnum, com.irow[index],
			       1, 1, &bit_data, &istat);

	  /**------(3-3d) Others, int ------ **/
	  } else {
	    int int_data;
	    if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "writeFITS OTH INT\n");
	    int_data = (int) data[ikeywordnum];
	    fits_write_col_int(com.fp[index], icolnum, com.irow[index],
			       1, 1, &int_data, &istat);
	  }
	  
	  if(istat){
	    fprintf(stderr,
		    "%s: fits_write_col %s : %s failed (%d)\n",
		    pname, com.fitsname[index],
		    com.KEY[index][icolnum].keyword, istat);
	    istat = 0;
	    ifail++;
	  }
	  ikeywordnum++;
	  icolnum++;
	}
	ibnknum++;
	if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, " free(data)\n");
	/*
	  free(data); */
      }
    }

    if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "while end (%d)\n", ibnknum);
  }
  
  if (DEBUG && (callnum>70440 || index==19) )fprintf(stderr, "end of writeFITS\n");

  if (ifail){
    return ANL_NG;
  } else {
    return ANL_OK;
  }
 
}


static int
copyFITSFILE(int index1, int index2){

  int istat = 0;
  int hdunum;

  fits_modify_key_lng(com.fp[index1], "NAXIS2", com.irow[index1], "&", &istat);  
  if ( istat ) {
    fprintf(stderr, "%s: fits_modify_key NAXIS2 %s failed (%d)\n", pname,
	    com.fitsname[index1], istat);
    return ANL_NG;
  }
  
  hkstdkeys.nevents=com.irow[index1];
  
  if (fits_update_key_lng(com.fp[index1], "NEVENTS", hkstdkeys.nevents,
			  "Number of events", &istat)) {
    fprintf(stderr, "%s: fits_update_key NEVENTS %s failed (%d)\n", pname,
	    com.fitsname[index1], istat);
    return ANL_NG;
  }
  
  hdunum=0;
  if (hxdFitsHeader_updateStdTimeKeys(com.fp[index1], hkstdkeys, hdunum,
				      &istat) != ANL_OK){
    fprintf(stderr,"%s: Error in updateStdTimeKeys (%d)\n",pname, istat);
    return ANL_NG;
  }
  
  if ( fits_flush_file(com.fp[index1], &istat) ){
    fprintf(stderr, "Error doing fits_flush_file (%d)\n", istat);
    return ANL_NG;
  }

  if(index1 != index2){
    
    fits_copy_hdu(com.fp[index1], com.fp[index2], morekeys[index1], &istat);
  
    if(istat){
      fprintf(stderr, "%s: fits_copy_hdu failed (%d)\n", pname, istat);
      return ANL_NG;
    }

    fits_delete_file(com.fp[index1],&istat);

    if(istat){
      fprintf(stderr, "%s: fits_delete_file failed (%d)\n",
	      com.fitsname[index1], pname, istat);
      return ANL_NG;
    }
    
  }
  
  if (fits_write_date(com.fp[index2], &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", pname, istat);
    return ANL_NG;
  }
  
  if (fits_write_chksum(com.fp[index2], &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  }
    
  return ANL_OK;
  
}

static int
closeFITSFILE(int index)
{
  int istat = 0;
  /*  int hdutype; */
  int hdunum;
  hdunum = 1;
  if (hxdFitsHeader_updateStdTimeKeys(com.fp[index], hkstdkeys, hdunum, 
				      &istat)   != ANL_OK){
    fprintf(stderr,"%s: Error in updateStdTimeKeys (%d)\n",pname, istat);
    return ANL_NG;
  }
  
  if (fits_write_date(com.fp[index], &istat)) {
    fprintf(stderr,"%s: fits_write_date failed (%d)\n", pname, istat);
    return ANL_NG;
  }
  
  if (fits_write_chksum(com.fp[index], &istat)) {
    fprintf(stderr, "%s:  fits_write_chksum failed (%d)\n", pname, istat);
    return ANL_NG;
  }

  fits_close_file(com.fp[index],&istat);
  
  if(istat){
    fprintf(stderr, "%s: fits_close_file %s failed (%d)\n", pname,
	    com.fitsname[index], istat);
    return ANL_NG;
  }
  
  return ANL_OK;
  
}

void
HXDHKFitsWrite_bgnrun(int *status)
{
  int index, size;
  int i;
  int istat = 0;
  
  char buff[FITS_KEY_MAX_LENGTH];
  char *tim_file;
  char leapfile[1024];

  /* HXD HK STD KEYWORDS */
  hxdFitsHeader_mallocSTDKEYS(&hkstdkeys);
  hxdFitsHeader_setDefaultKeywordValues(&hkstdkeys);
  /* hkstdkeys.datamode=strdup("HOUSEKEEPING"); */
  /* sprintf (hkstdkeys.datamode, "HOUSEKEEPING"); */
  sprintf(buff, "ANL: %s: %s", pname, HXDHKFitsWrite_version);
  /* hkstdkeys.creator=strdup(buff);*/
  sprintf(hkstdkeys.creator, buff);

  /* Credits, from ASTE_ANL (later than version 1.40)*/
/* hkstdkeys.credits     = strdup(anl_task_credits());
  hkstdkeys.task_name   = strdup(anl_task_name()   );
  hkstdkeys.task_version= strdup(anl_task_version());*/
  sprintf(hkstdkeys.credits,     anl_task_credits());
  sprintf(hkstdkeys.task_name,   anl_task_name()   );
  sprintf(hkstdkeys.task_version,anl_task_version());

  if ( ANL_OK == BnkKey("AEpacketRPTread:FILE_NAME", &index) ) {
    int len;
    memset(com.filename, 0, sizeof(com.filename));
    /* fill filename with '\0' */
    BnkfGetM("AEpacketRPTread:FILE_NAME", sizeof(com.filename),
	     &size, com.filename);
    strcpy(com.filename, basenam(com.filename));
    
    sprintf(hkstdkeys.tlmfile, com.filename);
    
    len = strlen(com.filename);
    if ( 4 < len && 0 == strcmp(".rpt", &com.filename[len-4]) ) {
      com.filename[len-4] = '\0';
    }

    /*** get tim file name **/
    BnkGet("ASTE:TIM_FILE:PTR", sizeof(tim_file), &size, &tim_file);
    if ( size == sizeof(tim_file) ) {
      tim_file = aefits_basename(tim_file);
    } else {
      tim_file = "none";
    }
    sprintf(hkstdkeys.timfile, tim_file);

    /*** get leapsec file name **/
    size = -1;
    BnkGet("ASTE:LEAPSEC_FILE", sizeof(leapfile), &size, &leapfile);
    if(size<0) size=0;
    leapfile[size] = '\0';
    sprintf(hkstdkeys.leapfile, leapfile);

  }else{
    *status = ANL_QUIT;
    return;
  }
    
  if( createFITSFILE("HK",HK,"") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("SYS",SYS,"_sys") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("RHK",RHK,"_rhk") ){
    *status = ANL_QUIT;	return;}
  if( createFITSFILE("ACU",ACU,"_acu") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("SCL",SCL,"_scl") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("PPR",PPR,"_ppr") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("PST",PST,"_pst") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("STM",STM,"_tbl") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("IO_DMP",IO_DMP,"_io_dmp") ){
    *status = ANL_QUIT;	return;}
  if( createFITSFILE("ECC_DMP",ECC_DMP,"_ecc_dmp")){
    *status = ANL_QUIT;	return;}
  if( createFITSFILE("AET_HC",AET_HC,"_aet_hc") ){
    *status = ANL_QUIT;	return;}
  if( createFITSFILE("AET_SC",AET_SC,"_aet_sc") ){
    *status = ANL_QUIT;	return;}
  if( createFITSFILE("RIO_DMP",RIO_DMP,"_rio_dmp")){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("RECC_DMP",RECC_DMP,"_recc_dmp") ){
    *status = ANL_QUIT;	return;}
  if( createFITSFILE("MEM_DMP",MEM_DMP,"_dmp")){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("SFC",SFC,"_hst") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("SFF1",SFF1,"_sff1") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("SFF2",SFF2,"_sff2") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("DLT",DLT,"_dlt") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("SP_PMT",SP_PMT,"_sp_pmt") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("SP_PIN",SP_PIN,"_sp_pin") ){
    *status = ANL_QUIT; return;}
  if( createFITSFILE("PWH",PWH,"_pwh") ){
    *status = ANL_QUIT; return;}
  
  *status = ANL_OK;
  
}


void
HXDHKFitsWrite_ana(int *nevent, int *eventid, int *status)
{
  double aetime;
  int evtyp;
  int used;

  int wpu_id;
  static int prev_pwh_ti[4] = {0, 0, 0, 0};
  int pwh_ti;

  int yyyymmdd;
  int hhmmss;	
  /* AtTime attime; */
  AtTimeD attime;
  
  if (DEBUG && callnum>70440) fprintf(stderr, "start HXDHKFitsWrite_ana\n");

  BnkfGetM("ASTE:PACKET_AETIME", sizeof(double), &used, &aetime);    
  /* aste2attime(aetime, &attime); */
  aste2attimeD(aetime, &attime);
  yyyymmdd = attime.yr*10000 + attime.mo*100 + attime.dy;
  hhmmss = attime.hr*10000 + attime.mn*100 + attime.sc;
  BnkfPutM("HXD:ALL:YYYYMMDD", sizeof(int), &yyyymmdd);
  BnkfPutM("HXD:ALL:HHMMSS", sizeof(int), &hhmmss);
  
  BnkfGetM("HXD:ALL:EVENTTYPE", sizeof(int), &used, &evtyp);
  switch ( evtyp ) {
  case HXD_EVTYP_HK:
    if( writeFITS(HK) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_SYS:
    if( writeFITS(SYS) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_RHK:
    if( writeFITS(RHK) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_ACU:
    if( writeFITS(ACU) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_SCL:
    if( writeFITS(SCL) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_PPR:
    if( writeFITS(PPR) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_PST:
    if( writeFITS(PST) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_STM:
    if( writeFITS(STM) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_IO_DMP:
    if( writeFITS(IO_DMP) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_ECC_DMP:
    if(writeFITS(ECC_DMP)){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_AET_HC:
    if( writeFITS(AET_HC) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_AET_SC:
    if( writeFITS(AET_SC) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_RIO_DMP:
    if(writeFITS(RIO_DMP)){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_RECC_DMP:
    if(writeFITS(RECC_DMP)){*status = ANL_QUIT;return;}break;
  case HXD_EVTYP_MEM_DMP:
    if(writeFITS(MEM_DMP)){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_SFC:
    if( writeFITS(SFC) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_SFF1:
    if( writeFITS(SFF1) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_SFF2:
    if( writeFITS(SFF2) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_DLT:
    if( writeFITS(DLT) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_SP_PMT:
    if( writeFITS(SP_PMT) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_SP_PIN:
    if( writeFITS(SP_PIN) ){*status = ANL_QUIT; return;}break;
  case HXD_EVTYP_WE0:
    wpu_id = 0;
    BnkfGetM( "HXD:ALL:PACKET_SEC_HEADER", sizeof(int),  &used, &pwh_ti);
    if (pwh_ti != prev_pwh_ti[wpu_id] ) {
      BnkfPutM( "HXDHKFitsWrite:PWH:WPU_ID", sizeof(int),  &wpu_id );
      if( writeFITS(PWH) ){*status = ANL_QUIT; return;}
      prev_pwh_ti[wpu_id] = pwh_ti;
    } 
    break;
  case HXD_EVTYP_WE1:
    wpu_id = 1;
    BnkfGetM( "HXD:ALL:PACKET_SEC_HEADER", sizeof(int),  &used, &pwh_ti);
    if (pwh_ti != prev_pwh_ti[wpu_id] ) {
      BnkfPutM( "HXDHKFitsWrite:PWH:WPU_ID", sizeof(int),  &wpu_id );
      if( writeFITS(PWH) ){*status = ANL_QUIT; return;}
      prev_pwh_ti[wpu_id] = pwh_ti;
    } 
    break;
  case HXD_EVTYP_WE2:
    wpu_id = 2;
    BnkfGetM( "HXD:ALL:PACKET_SEC_HEADER", sizeof(int),  &used, &pwh_ti);
    if (pwh_ti != prev_pwh_ti[wpu_id] ) {
      BnkfPutM( "HXDHKFitsWrite:PWH:WPU_ID", sizeof(int),  &wpu_id );
      if( writeFITS(PWH) ){*status = ANL_QUIT; return;}
      prev_pwh_ti[wpu_id] = pwh_ti;
    } 
    break;
  case HXD_EVTYP_WE3:
    wpu_id = 3;
    BnkfGetM( "HXD:ALL:PACKET_SEC_HEADER", sizeof(int),  &used, &pwh_ti);
    if (pwh_ti != prev_pwh_ti[wpu_id] ) {
      BnkfPutM( "HXDHKFitsWrite:PWH:WPU_ID", sizeof(int),  &wpu_id );
      if( writeFITS(PWH) ){*status = ANL_QUIT; return;}
      prev_pwh_ti[wpu_id] = pwh_ti;
    } 
    break;
  default:
    return;
  }
  
  if (DEBUG && callnum>70440) 
    fprintf(stderr, "HXDHKFitsWrite_ana timeUpdate (%d)\n", aetime);
  hxdFitsHeader_timeUpdate ( &hkstdkeys, aetime );

  if ( (int) aetime == 0 ) 
    fprintf(stderr, 
	    "%s, hxdFitsHeader_timeUpdate: aetime is 0.0 (evtype=0x%x)\n",
	    pname, evtyp);

  if (DEBUG && callnum>70440) fprintf(stderr, "end of HXDHKFitsWrite_ana\n");
  *status = ANL_OK;
}

void
HXDHKFitsWrite_endrun(int *status)
{
  
  int istat=0;

  hxdgtiFits_finalizeGTI ( &gti, hkstdkeys.tstart, hkstdkeys.tstop );
  
  hkstdkeys.ontime=gti.ontime;
  
  if( copyFITSFILE(HK,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(SYS,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(ACU,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(SCL,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(PWH,HK) ){*status = ANL_QUIT; return;}
/*if( closeFITSFILE(HK) ){*status = ANL_QUIT; return;} */

  if( copyFITSFILE(RHK,HK) ){*status = ANL_QUIT; return;}
/*if( closeFITSFILE(RHK) ){*status = ANL_QUIT; return;} */

  if( copyFITSFILE(STM,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(PPR,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(PST,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(AET_HC,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(AET_SC,HK) ){*status = ANL_QUIT; return;}  
/*if( closeFITSFILE(STM) ){*status = ANL_QUIT; return;}*/
  
  if( copyFITSFILE(MEM_DMP,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(ECC_DMP,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(IO_DMP,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(RECC_DMP,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(RIO_DMP,HK) ){*status = ANL_QUIT; return;}
/*if( closeFITSFILE(MEM_DMP) ){*status = ANL_QUIT; return;}*/

  if( copyFITSFILE(SFC,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(SFF1,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(SFF2,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(DLT,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(SP_PMT,HK) ){*status = ANL_QUIT; return;}
  if( copyFITSFILE(SP_PIN,HK) ){*status = ANL_QUIT; return;}


  if( closeFITSFILE(HK) ){*status = ANL_QUIT; return;}
  
  hxdFitsHeader_freeSTDKEYS(&hkstdkeys);
  *status = ANL_OK;
  
}


void
HXDHKFitsWrite_init(int *status)
{

  double scl_time = 0.0;
  double wel_time = 0.0;
  int wpu_id = 0;
  
  BnkDef("HXD:ALL:YYYYMMDD", sizeof(int));
  BnkDef("HXD:ALL:HHMMSS", sizeof(int));

  BnkDef( "HXDHKFitsWrite:SCL:TIME", sizeof(double) );
  BnkDef( "HXDHKFitsWrite:PWH:TIME", sizeof(double) );
  BnkDef( "HXDHKFitsWrite:PWH:WPU_ID", sizeof(int) );
  
  BnkfPutM( "HXDHKFitsWrite:SCL:TIME", sizeof(double), &scl_time );
  BnkfPutM( "HXDHKFitsWrite:PWH:TIME", sizeof(double), &wel_time );
  BnkfPutM( "HXDHKFitsWrite:PWH:WPU_ID", sizeof(int),  &wpu_id );

  *status = ANL_OK;
}



void
HXDHKFitsWrite_startup(int *status)
{
    *status = ANL_OK;
}


void
HXDHKFitsWrite_com(int *status){

  com.mk1stfits_dir[0] = '\0';
  CLtxtrd("HXDHKFitsWrite: Input mk1stfits_dir>",
	  com.mk1stfits_dir, sizeof(com.mk1stfits_dir) );
  
  com.hxd_fits_key_dir[0] = '\0';
  CLtxtrd("HXDHKFitsWrite: Input hxd_fits_key_dir>",
	  com.hxd_fits_key_dir, sizeof(com.hxd_fits_key_dir) );
  
  *status = ANL_OK;
}


void
HXDHKFitsWrite_his(int *status)
{
	*status = ANL_OK;
}


void
HXDHKFitsWrite_exit(int *status)
{
    *status = ANL_OK;
}

