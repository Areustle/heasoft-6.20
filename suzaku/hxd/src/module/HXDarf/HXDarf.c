/*
 *   HXDarf created by Y. Matsumoto, Y.Terada
 *         v0.1.0 mod Y.Terada 1999-10-27
 *         v0.2.1 mod Y.Terada 2000-01-25
 *         v0.3.0 for HEADAS
 *
 *         v0.4.0 HXD-II version, created by K.Tamura, Y.Terada  2005-05-23
 *         v0.4.8 mod Y.Terada 2006-08-25
 *                for headas_body v1.71, for ver1.2.2.3 process 
 *         v0.5.0 = v1.0.0
 *         v2.0.0 support CALDB access function, Y.Terada 2007-05-01
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

char HXDarf_version[] = "version 2.0.0";
static char pname[] = "HXDarf";

#define DEBUG 0

void
HXDarf_startup(int *status){
  *status = ANL_OK; 
  return;
}

void
HXDarf_com(int *status){
  *status = ANL_OK;
  return;
}

void
HXDarf_init(int *status){
  BnkDef("HXDarf:AN_ARF", sizeof(HxdArfData) );
  *status = ANL_OK;
  return;
}

void
HXDarf_his(int *status){
  *status = ANL_OK; 
  return;
}

void
HXDarf_bgnrun(int *status){
  *status = ANL_OK;
  return;
}

void
HXDarf_ana(int nevent, int eventid, int *status){
  int stat, size;

  int    dettype;
  int    calc_detid;
  double calc_ra, calc_dec;
  HxdArfData arf;

  BnkGet("HXDarfInit:DETTYPE",  sizeof(int),    &size, &dettype);
  BnkGet("HXDarfInit:CALC_RA",  sizeof(double), &size, &calc_ra);
  BnkGet("HXDarfInit:CALC_DEC", sizeof(double), &size, &calc_dec);
  BnkGet("HXDarfInit:DET_ID",   sizeof(int),    &size, &calc_detid);

  if (DEBUG)
  fprintf(stdout, "HXDarf body: type=%d, det_id=%d RA/DEC=%f, %f\n",
	  dettype, calc_detid, calc_ra, calc_dec);

  if (dettype == HXDARF_DETTYPE_PIN){
    stat = hxdarfUtil_PIN(calc_detid, calc_ra, calc_dec, &arf);
  } else if (dettype == HXDARF_DETTYPE_GSO){
    stat = hxdarfUtil_GSO(calc_detid, calc_ra, calc_dec, &arf);
  } 
  if (stat != HXDARFUTIL_STATUS_OK) {
    fprintf(stderr,"%s: read error\n", pname);
    *status = ANL_NG;
    return;
  }

  BnkPut("HXDarf:AN_ARF", sizeof(HxdArfData), &arf);

  *status = ANL_OK;
  return;
}

void
HXDarf_endrun(int *status){
  *status = ANL_OK; 
  return;
}

void
HXDarf_exit(int *status){
  *status = ANL_OK; 
  return;
}

/******** EOF *********/
