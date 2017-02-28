
/*
 *  HXDmkgainhistWritePHA
 *  v0.3.0 2005-06-28, 06-29 created by Y.Terada
 *  v0.3.1 2005-06-29, by Y.Terada, debug 
 *  v0.4.3 2005-11-08, by Y.Terada, debug 
 *  v0.4.4 2006-08-18, by Y.Terada, save memory 
 *  v2.0.0 2006-09-10, by Y.Terada, for v2.0 format
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
#include "fitsio.h"
#include "HXD.h"

#include "hxdFitsHeaderUtil.h"
#include "hxdgtiFitsUtil.h"
#include "hxdphaUtil.h"
#include "hxdeventFitsUtil.h"

/* #include "uclpario.h" */
#include "pil.h"
#include "headas.h"

char HXDmkgainhistWritePHA_version[] = "version 2.0.0";
static char pname[] = "HXDmkgainhistWritePHA";

#define HXDMKGAINHIST_NG 99
#define HXDMKGAINHIST_OK  1

#define MAX_LENGTH PIL_LINESIZE
#define MAX_GTI_NUM  512
#define HXDMKGAINHIST_EVENT_NOT_GSO 999
#define HXDMKGAINHIST_EVENT_NOT_PIN 999
#define HXDMKGAINHIST_PMT_PHA_CH   4096
#define HXDMKGAINHIST_PIN_PHA_CH    256

#define HXDMKGAINHIST_OUT_OF_GTIS   999

#define HXDMKGAINHIST_TRIG_ANODE 0x00000020
#define HXDMKGAINHIST_FLAG_PSD   0x00000001
#define HXDMKGAINHIST_FLAG_PINLD 0x00000040

static struct {
  int pha_mode;
  int extract_gso;
  int extract_pin;
  char gtilist_fname[MAX_LENGTH];
  char pha_base_name[MAX_LENGTH];
  char origin[256];
  /*
  HxdPha slowpha[MAX_GTI_NUM][16];
  HxdPha fastpha[MAX_GTI_NUM][16];
  HxdPha pinpha [MAX_GTI_NUM][64];
  */
  HxdPha *slowpha[16];
  HxdPha *fastpha[16];
  HxdPha *pinpha [64];
  int pmt_detchan;
  int pin_detchan;
}com;

static struct {
  HXD_GTI hxdgti[MAX_GTI_NUM];
  int num;
} gti;


void
HXDmkgainhistWritePHA_startup(int *status){ 
  int ch;
  com.pha_mode = 0;
  com.extract_gso = 0;
  com.extract_pin = 0;
  com.pmt_detchan = HXDMKGAINHIST_PMT_PHA_CH;
  com.pin_detchan = HXDMKGAINHIST_PIN_PHA_CH;

/*fprintf(stdout, "%s: %d+%d+%d bytes\n", pname,
	  sizeof(HxdPha) * MAX_GTI_NUM * 16,
	  sizeof(HxdPha) * MAX_GTI_NUM * 16,
	  sizeof(HxdPha) * MAX_GTI_NUM * 64);*/
  for(ch=0;ch<16;ch++){
    if (com.slowpha[ch] == NULL){
      com.slowpha[ch] = (HxdPha*) malloc( sizeof(HxdPha) * MAX_GTI_NUM);
    } else {
      fprintf(stderr, "%s: Momory allocation error, com.slowpha[%d]\n",
	      pname,ch);
      *status = ANL_NG;
      return;
    }
    if (com.fastpha[ch] == NULL){
      com.fastpha[ch] = (HxdPha*) malloc( sizeof(HxdPha) * MAX_GTI_NUM);
    } else {
      fprintf(stderr, "%s: Momory allocation error, com.fastpha[%d]\n",
	      pname,ch);
      *status = ANL_NG;
      return;
    }
  }

  for(ch=0;ch<64;ch++){
    if (com.pinpha[ch] == NULL){
      com.pinpha[ch] = (HxdPha*) malloc( sizeof(HxdPha) * MAX_GTI_NUM);
    } else {
      fprintf(stderr, "%s: Momory allocation error, com.pinpha[%d]\n",
	      pname,ch);
      *status = ANL_NG;
      return;
    }
  }

  *status = ANL_OK; 
}

/*** local functions ***/
static int  hxdmkgainhistWritePHA_read_gti( void );
static int  hxdmkgainhistWritePHA_gti_id( double aetime );
static int  hxdmkgainhistWritePHA_event_gso   ( HxdEventFits02 *event );
static int  hxdmkgainhistWritePHA_event_pin   ( HxdEventFits02 *event );
static int  hxdmkgainhistWritePHA_pmt_channel ( int pha );
static int  hxdmkgainhistWritePHA_pin_channel ( HxdEventFits02 *event,
						int pinid);

void
HXDmkgainhistWritePHA_com(int *status) {

  char mod_string[MAX_LENGTH];
  static char flist[MAX_LENGTH];

  if ( *status ) { /* ftools */
    
    *status = 0;

    *status = PILGetBool("phaextractor_mode", &com.pha_mode);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }

    if (! com.pha_mode){
      *status = ANL_OK;
      return;
    }

   *status = PILGetString("hxdmkgainhist_origin", com.origin);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }

   *status = PILGetString("phaextractor_base_name", com.pha_base_name);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }

    *status = PILGetFname("phaextractor_gti_list", flist);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }
    if (flist[0] == '@'){
      sprintf(com.gtilist_fname, "%s", flist+1);
    } else {
      fprintf(stderr, "%s: get phaextractor_gti_list error. Please start @.",
	      pname);
      *status = ANL_QUIT;
      exit(-1);
    }

    *status = PILGetBool("phaextractor_extract_gso", &com.extract_gso);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }

    *status = PILGetBool("phaextractor_extract_pin", &com.extract_pin);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }

    *status = PILGetInt("phaextractor_gso_detchan", &com.pmt_detchan);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }

    *status = PILGetInt("phaextractor_pin_detchan", &com.pin_detchan);
    if ( *status ) {
      *status = ANL_QUIT;
      exit(-1);
    }
    
    *status = ANL_OK;
    return;
  }
  /* ANL */

  mod_string[0] = '\0';
  
  CLtxtrd("GHF Write Mode? (yes or no)",
	  mod_string, sizeof(mod_string) );
  if(!strcmp("yes", mod_string)){
    com.pha_mode=1;
  } else if(!strcmp("no", mod_string)){
    com.pha_mode=0;
  } else {
    fprintf(stderr, "%s : no such answer : yes or no\n",
	    mod_string);
    *status = ANL_QUIT;
    exit(-1);
  }

  if (! com.pha_mode){
    *status = ANL_OK;
    return;
  }

  *status = ANL_OK;
}

void
HXDmkgainhistWritePHA_init(int *status) {
  if (! com.pha_mode){
    *status = ANL_OK;
    return;
  }

  *status = ANL_OK;
}

void
HXDmkgainhistWritePHA_his(int *status){
  *status = ANL_OK; 
}

void
HXDmkgainhistWritePHA_bgnrun(int *status){
  int stat;
  int gtiid, ch, unit;

  if (! com.pha_mode){
    *status = ANL_OK;
    return;
  }

  /**** Read GTI *****/
  stat = hxdmkgainhistWritePHA_read_gti();
  if ( stat != HXDMKGAINHIST_OK ){
    fprintf(stderr, "%s: read gti failed\n", pname);
    *status = ANL_QUIT;
    return;
  }

  /** clear HxdPha for SLOW/FAST **/
  if(com.extract_gso){
    for(gtiid =0; gtiid < gti.num; gtiid ++){
      for (unit =0; unit < 16; unit++){
	for (ch=0; ch< com.pmt_detchan; ch++){
	  com.slowpha[gtiid][unit].counts[ch] = 0;
	  com.fastpha[gtiid][unit].counts[ch] = 0;
	}
	com.slowpha[gtiid][unit].detchans = com.pmt_detchan;
	com.fastpha[gtiid][unit].detchans = com.pmt_detchan;
      }
    }
  }

  /** clear HxdPha for PIN **/
  if(com.extract_pin){
    for(gtiid =0; gtiid < gti.num; gtiid ++){
      for (unit =0; unit < 64; unit++){
	for (ch=0; ch< com.pin_detchan; ch++){
	  com.pinpha[gtiid][unit].counts[ch] = 0;
	}
	com.pinpha[gtiid][unit].detchans = com.pin_detchan;
      }
    }
  }

  *status = ANL_OK;
}

void
HXDmkgainhistWritePHA_ana(int nevent, int eventid, int *status){
  int stat;
  int size;
  HxdEventFits02 event;
  int gtiid;
  int detid;
  int channel;

  if (! com.pha_mode){
    *status = ANL_OK;
    return;
  }

  /** Read event **/
  BnkfGetM( "HXD:WEL:EVENT", sizeof(HxdEventFits02), &size, &event );

  /** get GTI ID **/
  gtiid = hxdmkgainhistWritePHA_gti_id( event.time ); /** need hxdtime **/
  if (gtiid == HXDMKGAINHIST_OUT_OF_GTIS){
    *status = ANL_OK;
    return;
  }

  /** calc Event Type (GSO) **/
  if(com.extract_gso){
    detid =  hxdmkgainhistWritePHA_event_gso(&event);
    if (detid != HXDMKGAINHIST_EVENT_NOT_GSO) {
      /** FAST histogram **/
      channel = hxdmkgainhistWritePHA_pmt_channel( event.pha_fast);
      com.slowpha[gtiid][detid].counts[channel] ++;
      /** SLOW histogram **/
      channel = hxdmkgainhistWritePHA_pmt_channel( event.pha_slow);
      com.fastpha[gtiid][detid].counts[channel] ++;
    }
  }

  /** calc Event Type (PIN) **/
  if(com.extract_pin){
    detid =  hxdmkgainhistWritePHA_event_pin(&event);
    if (detid != HXDMKGAINHIST_EVENT_NOT_PIN) {
      /** PIN histogram **/
      channel = hxdmkgainhistWritePHA_pin_channel( &event, detid);
      com.pinpha[gtiid][detid].counts[channel] ++;
    }
  }
  
  *status = ANL_OK;
}

void
HXDmkgainhistWritePHA_endrun(int *status){
  int gtiid, unit;
  static HXD_STD_KEYS header;
  int stat;

  if (! com.pha_mode){
    *status = ANL_OK;
    return;
  }

  BnkDef("ASTE:FFF_ORIGIN", sizeof(com.origin)-1);
  BnkPut("ASTE:FFF_ORIGIN", strlen(com.origin), com.origin);

  /**** write SLOW PHA files ****/
  if(com.extract_gso){
    for(gtiid =0; gtiid < gti.num; gtiid ++){
      for (unit =0; unit < 16; unit++){
	  char phaname [256];
	  char chantype[256] = "PHA_SLOW";
	  /** prepare header **/
	  hxdFitsHeader_setDefaultKeywordValues( &header );
	  header.tstart = gti.hxdgti[gtiid].start[0];
	  header.tstop  = gti.hxdgti[gtiid].stop[gti.hxdgti[gtiid].row-1];
	  header.ontime = gti.hxdgti[gtiid].ontime;
	  header.telapse= gti.hxdgti[gtiid].ontime;
	  header.use_detnam = 1;
	  header.detnam =  (char *)strdup("WELL_GSO");
	  header.task_name=(char *)strdup(pname);
	  header.task_version=(char *)strdup(HXDmkgainhistWritePHA_version);

	  /** create FITS **/
	  sprintf(phaname, "%s_slow_w%01d%01d_gti%02d.pha",
		  com.pha_base_name, unit>>2, unit&0x03, gtiid);
	  stat = hxdphaUtil_create_FITS( phaname, header, 
					 gti.hxdgti[gtiid].ontime, 
					 chantype, 
					 com.slowpha[gtiid][unit].detchans);
	  if (stat != HXDPHAUTIL_OK){
	    fprintf(stderr, "err in creation");
	    *status = ANL_QUIT;
	    return;
	  }

	  stat = hxdphaUtil_write_FITS ( &com.slowpha[gtiid][unit],
					 header, &gti.hxdgti[gtiid]);
	  if (stat != HXDPHAUTIL_OK){
	    fprintf(stderr, "err in writting");
	    *status = ANL_QUIT;
	    return;
	  }

	  stat = hxdphaUtil_close_FITS ();
	  if (stat != HXDPHAUTIL_OK){
	    fprintf(stderr, "err in closing");
	    *status = ANL_QUIT;
	    return;
	  }

      } /** end of unit **/
    } /** end of GTI **/
  } /** end of GSO / SLOW **/

  /**** write FAST PHA files ****/
  if(com.extract_gso){
    for(gtiid =0; gtiid < gti.num; gtiid ++){
      for (unit =0; unit < 16; unit++){
	  char phaname [256];
	  char chantype[256] = "PHA_FAST";
	  /** prepare header **/
	  hxdFitsHeader_setDefaultKeywordValues( &header );
	  header.tstart = gti.hxdgti[gtiid].start[0];
	  header.tstop  = gti.hxdgti[gtiid].stop[gti.hxdgti[gtiid].row-1];
	  header.ontime = gti.hxdgti[gtiid].ontime;
	  header.telapse= gti.hxdgti[gtiid].ontime;
	  header.use_detnam = 1;
	  header.detnam = (char *)strdup("WELL_GSO");
	  header.task_name=(char *)strdup(pname);
	  header.task_version=(char *)strdup(HXDmkgainhistWritePHA_version);

	  /** create FITS **/
	  sprintf(phaname, "%s_fast_w%01d%01d_gti%02d.pha",
		  com.pha_base_name, unit>>2, unit&0x03, gtiid);
	  stat = hxdphaUtil_create_FITS( phaname, header, 
					 gti.hxdgti[gtiid].ontime, 
					 chantype, 
					 com.fastpha[gtiid][unit].detchans);
	  if (stat != HXDPHAUTIL_OK){
	    fprintf(stderr, "err in creation");
	    *status = ANL_QUIT;
	    return;
	  }

	  stat = hxdphaUtil_write_FITS ( &com.fastpha[gtiid][unit],
					 header, &gti.hxdgti[gtiid]);
	  if (stat != HXDPHAUTIL_OK){
	    fprintf(stderr, "err in writting");
	    *status = ANL_QUIT;
	    return;
	  }

	  stat = hxdphaUtil_close_FITS ();
	  if (stat != HXDPHAUTIL_OK){
	    fprintf(stderr, "err in closing");
	    *status = ANL_QUIT;
	    return;
	  }

      } /** end of unit **/
    } /** end of GTI **/
  } /** end of GSO / FAST **/


  /**** write PIN PHA files ****/
  if(com.extract_pin){
    for(gtiid =0; gtiid < gti.num; gtiid ++){
      for (unit =0; unit < 64; unit++){
	  char phaname [256];
	  char chantype[256] = "PHA_PIN";
	  /** prepare header **/
	  hxdFitsHeader_setDefaultKeywordValues( &header );
	  header.tstart = gti.hxdgti[gtiid].start[0];
	  header.tstop  = gti.hxdgti[gtiid].stop[gti.hxdgti[gtiid].row-1];
	  header.ontime = gti.hxdgti[gtiid].ontime;
	  header.telapse= gti.hxdgti[gtiid].ontime;
	  header.use_detnam = 1;
	  header.detnam = (char *)strdup("WELL_PIN");
	  header.task_name=(char *)strdup(pname);
	  header.task_version=(char *)strdup(HXDmkgainhistWritePHA_version);

	  /** create FITS **/
	  sprintf(phaname, "%s_pin_w%01d%01dp%01d_gti%02d.pha",
		  com.pha_base_name, unit>>4, (unit>>2)&0x03, unit&0x03,gtiid);
	  stat = hxdphaUtil_create_FITS( phaname, header, 
					 gti.hxdgti[gtiid].ontime, 
					 chantype, 
					 com.pinpha[gtiid][unit].detchans);
	  if (stat != HXDPHAUTIL_OK){
	    fprintf(stderr, "err in creation");
	    *status = ANL_QUIT;
	    return;
	  }

	  stat = hxdphaUtil_write_FITS ( &com.pinpha[gtiid][unit],
					 header, &gti.hxdgti[gtiid]);
	  if (stat != HXDPHAUTIL_OK){
	    fprintf(stderr, "err in writting");
	    *status = ANL_QUIT;
	    return;
	  }

	  stat = hxdphaUtil_close_FITS ();
	  if (stat != HXDPHAUTIL_OK){
	    fprintf(stderr, "err in closing");
	    *status = ANL_QUIT;
	    return;
	  }

      } /** end of unit **/
    } /** end of GTI **/
  } /** end of PIN **/

  *status = ANL_OK;
}

void
HXDmkgainhistWritePHA_exit(int *status){
  *status = ANL_OK;
}

/*****************************************************************************
 ***  local functions ********************************************************
 ****************************************************************************/

/* ---------------------------------------------------------------------------
 **  Read GTI File lests 
 * --------------------------------------------------------------------------*/
static int
hxdmkgainhistWritePHA_read_gti( void ){
  FILE *hxdmkgainhistpha_gtilist_fp;
  FILE *hxdmkgainhistpha_gti_fp   [MAX_GTI_NUM];
  char hxdmkgainhistpha_gti_fname[MAX_GTI_NUM][MAX_LENGTH];
  int gti_id;
  int intval;
  int status = HXDMKGAINHIST_OK;
  double tstart[MAX_GTI_NUM], tstop[MAX_GTI_NUM];
  int i;

  /*** (1) read gti list files ***/
  /** (1-1) open file **/
  hxdmkgainhistpha_gtilist_fp = fopen(com.gtilist_fname, "r");
  if (hxdmkgainhistpha_gtilist_fp == NULL){
      fprintf(stderr, "%s: file %s open error\n", pname, com.gtilist_fname);
      status = HXDMKGAINHIST_NG;
      return status;
  }

  /** (1-2) read file **/
  gti.num = 0;
  while (1) {
    if (gti.num > MAX_GTI_NUM){
      fprintf(stderr, "%s: too many GTI files in %s\n",
	      pname, com.gtilist_fname);
      status = HXDMKGAINHIST_NG;
      return status;
    }

    if (fscanf(hxdmkgainhistpha_gtilist_fp, "%s\n", 
	       hxdmkgainhistpha_gti_fname[gti.num] ) == EOF) break;
    gti.num ++;
  }

  /** (1-3) close file **/
  if (fclose (hxdmkgainhistpha_gtilist_fp)){
    fprintf(stderr, "%s: file %s close error.\n",
	    pname, com.gtilist_fname);
    status = HXDMKGAINHIST_NG;
    return status;
  }

  /*** (2) read GTI files ***/
  for (gti_id = 0; gti_id < gti.num; gti_id ++){
    /** (2-1) open file **/
    hxdmkgainhistpha_gti_fp[gti_id] 
      = fopen(hxdmkgainhistpha_gti_fname[gti_id], "r");
    if (hxdmkgainhistpha_gti_fp[gti_id] == NULL){
      fprintf(stderr, "%s: file %s open error\n", pname, 
	      hxdmkgainhistpha_gti_fname[gti_id]);
      status = HXDMKGAINHIST_NG;
      return status;
    }

    /** (2-2) read file **/
    intval=0;
    while (1) {
      if( fscanf (hxdmkgainhistpha_gti_fp[gti_id], "%lf %lf\n",
		  &tstart[intval], &tstop[intval]) == EOF ){break;}
      intval ++;
    }

    gti.hxdgti[gti_id].row = intval;
    gti.hxdgti[gti_id].start = malloc( sizeof(double)* intval );
    gti.hxdgti[gti_id].stop  = malloc( sizeof(double)* intval );
    for (i=0; i< intval; i++){
      gti.hxdgti[gti_id].start[i] = tstart[i] ;
      gti.hxdgti[gti_id].stop [i] = tstop [i] ;
    }
    hxdgtiFits_UpdateOntime( &gti.hxdgti[gti_id]);

    /** (2-3) close file **/
    if (fclose (hxdmkgainhistpha_gti_fp[gti_id])){
      fprintf(stderr, "%s: file %s close error.\n",
	      pname, hxdmkgainhistpha_gti_fname[gti_id]);
      status = HXDMKGAINHIST_NG;
      return status;
    }
  } /** end of gti_id **/

  return status;
}

/* ---------------------------------------------------------------------------
 **  Check GTI ID 
 * --------------------------------------------------------------------------*/
static int
hxdmkgainhistWritePHA_gti_id( double aetime ){
  int gti_id;
  int intval;

  for (gti_id = 0; gti_id < gti.num; gti_id ++){
    for (intval = 0; intval < gti.hxdgti[gti_id].row; intval ++){
      if (     (gti.hxdgti[gti_id].start[intval] <= aetime)
	    && (aetime <= gti.hxdgti[gti_id].stop[intval])  ){
	/** found **/
	return gti_id;
      }
    }
  }  

  /** not found **/
  gti_id = HXDMKGAINHIST_OUT_OF_GTIS;
  return gti_id;
}


/* ---------------------------------------------------------------------------
 **  calc Event Type, return detector id
 * --------------------------------------------------------------------------*/
static int 
hxdmkgainhistWritePHA_event_gso( HxdEventFits02 *event ){
  /** fselect expr 
      (UNITID==1)&&(TRIG==b0100000)&&(QUALITY_FLAGS==bxxxxxx1) **/
  int unitid = (int) event->unitid;
  int trig   = (int) event->trig;
  int flags  = (int) event->quality_flags;

  if( (trig==HXDMKGAINHIST_TRIG_ANODE) && (flags & HXDMKGAINHIST_FLAG_PSD) ){
    return unitid;
  } else {
    return HXDMKGAINHIST_EVENT_NOT_GSO;
  }
}

static int 
hxdmkgainhistWritePHA_event_pin( HxdEventFits02 *event ){
  int unitid = (int) event->unitid;
  int trig   = (int) event->trig;
  int pintrg = (trig&0x0000001D) >> 1;
  int flags  = (int) event->quality_flags;

  int chan;
  int pinid;

  switch ( pintrg ) {
  case 0x01:
    chan = 3; break;
  case 0x02:
    chan = 2; break;
  case 0x04:
    chan = 1; break;
  case 0x08:
    chan = 0; break;
  default:
    return HXDMKGAINHIST_EVENT_NOT_PIN;
  }

  if (flags & HXDMKGAINHIST_FLAG_PINLD){
    pinid = (unitid*4+chan);
    return pinid;
  } else {
    return HXDMKGAINHIST_EVENT_NOT_PIN;
  }

}

/* ---------------------------------------------------------------------------
 **  calc CHANNEL
 * --------------------------------------------------------------------------*/
static int 
hxdmkgainhistWritePHA_pmt_channel( int pha ){
  double factor;
  int chan;

  factor = (double) HXDMKGAINHIST_PMT_PHA_CH / (double) com.pmt_detchan;
  chan  = (int) (   ((double)pha) / factor   );

  return chan;
}

static int 
hxdmkgainhistWritePHA_pin_channel( HxdEventFits02 *event, int pinid ){
  double factor;
  int chan;
  int pha;

  switch (pinid % 4 ){
  case 0:
    pha = (int) event->pha_pin0;    break;
  case 1:
    pha = (int) event->pha_pin1;    break;
  case 2:
    pha = (int) event->pha_pin2;    break;
  case 3:
    pha = (int) event->pha_pin3;    break;
  default:
    fprintf(stderr, "logic error in calc pin_channel\n");
    return com.pin_detchan;
  }

  factor = (double) HXDMKGAINHIST_PIN_PHA_CH / (double) com.pin_detchan;
  chan  = (int) (   ((double)pha) / factor   );

  return chan;
}
/****** EOF *****/
