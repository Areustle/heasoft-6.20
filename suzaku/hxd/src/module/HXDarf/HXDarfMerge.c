/*
 *   HXDarf created by Y. Matsumoto, Y.Terada
 *         v0.1.0 mod Y.Terada 1999-10-27
 *         v0.2.1 mod Y.Terada 2000-01-25
 *         v0.3.0 for HEADAS
 *
 *         v0.4.0 HXD-II version, created by K.Tamura, Y.Terada  2005-05-23
 *         v0.4.8 mod Y.Terada 2006-08-25
 *                for headas_body v1.71, for ver1.2.2.3 process 
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "anl.h"
#include "HXD.h"

#include "hxdarfUtil.h"
#include "hxdrspUtil.h"
#include "hxdcaldbUtil.h"

#include "HXDarf.h"

char HXDarfMerge_version[] = "version 2.0.0";
static char pname[] = "HXDarfInit";

static HxdArfData arf_buff[HXDARF_MAX_POSNUM][HXDARF_MAX_DETNUM];

#define DEBUG 0

void
HXDarfMerge_startup(int *status){ *status = ANL_OK; }

void
HXDarfMerge_com(int *status){
  *status = ANL_OK;
  return;
}

void
HXDarfMerge_init(int *status){
  BnkDef("HXDarfMerge:ARF", sizeof(HxdArfData));
  *status = ANL_OK;
  return;
}

void
HXDarfMerge_his(int *status){
  *status = ANL_OK; 
  return;
}

void
HXDarfMerge_bgnrun(int *status){
  *status = ANL_OK;
  return;
}

void
HXDarfMerge_ana(int nevent, int eventid, int *status){
  HxdArfData arf;
  int    calc_detid;
  int    calc_posid;
  int    size;

  BnkGet("HXDarfInit:POS_ID",   sizeof(int), &size, &calc_posid);
  BnkGet("HXDarfInit:DET_ID",   sizeof(int), &size, &calc_detid);
  BnkGet("HXDarf:AN_ARF", sizeof(HxdArfData), &size, &arf);

  hxdarfUtil_copy_HxdArfData(&arf, &arf_buff[calc_posid][calc_detid]);

  if (DEBUG){
    fprintf(stderr, 
	    "ANA: arf_buff[posid=%d][detid=%d].energy_low[irow=10]=%f, %f\n",
	    calc_posid, calc_detid, 
	    arf_buff[calc_posid][calc_detid].energy_low[10],
	    arf.energy_low[10]);
  }

  *status = ANL_OK;
  return;
}

void
HXDarfMerge_endrun(int *status){ 
  HxdArfData arf_data;
  int nrow = arf_buff[0][0].irow;
  int pinid, gsoid, posid;
  int dettype, detid;
  int size;
  int merge;

  BnkGet("HXDarfInit:DETTYPE", sizeof(int), &size, &dettype);
  BnkGet("HXDarfInit:DET_ID", sizeof(int),  &size, &detid);
  BnkGet("HXDarfInit:POS_ID", sizeof(int),  &size, &posid);
  BnkGet("HXDarfInit:MERGE_FLG", sizeof(int), &size, &merge);

  /**======= PIN ====== **/
  if (dettype == HXDARF_DETTYPE_PIN) {
    if (! merge){
      if (DEBUG){
	fprintf(stderr, "arf_buff[posid=%d][detid=%d].energy_low[irow=10]=%f\n",
		posid, detid, arf_buff[posid][detid].energy_low[10]);
      }
      hxdarfUtil_copy_HxdArfData(&arf_buff[posid][detid], &arf_data);
    } else {
      double wmap_exposure_ratio[64];
      hxdarfUtil_get_Exp_Ratio(HXDARFUTIL_PIFILE_TYPE_PIN, 
			       wmap_exposure_ratio);
      hxdarfUtil_clear_HxdArfData(&arf_data, nrow);
      for (pinid=0;pinid<64;pinid++){
	double ratio_norm = 1.0;
	hxdarfUtil_add_HxdArfData( &arf_buff[posid][pinid], &arf_data, 
				   &arf_data,
				   wmap_exposure_ratio[pinid], ratio_norm);
      }
    }

  /**======= GSO ====== **/
  } else if (dettype == HXDARF_DETTYPE_GSO){
    if (! merge){
      hxdarfUtil_copy_HxdArfData(&arf_buff[posid][detid], &arf_data);
    } else {
      double wmap_exposure_ratio[16];
      hxdarfUtil_get_Exp_Ratio(HXDARFUTIL_PIFILE_TYPE_GSO, 
			       wmap_exposure_ratio);
      hxdarfUtil_clear_HxdArfData(&arf_data, nrow);  
      for (gsoid=0;gsoid<16;gsoid++){
	double ratio_norm = 1.0;
	hxdarfUtil_add_HxdArfData( &arf_buff[posid][gsoid], &arf_data, 
				   &arf_data,
				   wmap_exposure_ratio[gsoid], ratio_norm);
      }
    }

  }

  BnkPut("HXDarfMerge:ARF", sizeof(HxdArfData), &arf_data);

  *status = ANL_OK; 
  return;
}

void
HXDarfMerge_exit(int *status){
  *status = ANL_OK; 
  return;
}

/******** EOF *********/
