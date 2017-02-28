/*
 *   HXDarf created by Y. Matsumoto, Y.Terada
 *         v0.1.0 mod Y.Terada 1999-10-27
 *         v0.2.1 mod Y.Terada 2000-01-25
 *         v0.3.0 for HEADAS
 *
 *         v0.4.0 HXD-II version, created by K.Tamura, Y.Terada  2005-05-23
 *         v0.4.7 by Y.Terada, 2005-11-05
 *         v0.4.8 mod Y.Terada 2006-08-25
 *                for headas_body v1.71, for ver1.2.2.3 process 
 *         v2.0.0 mod Y.Terada 2007-05-01
 *                add create_name for output.
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
#include "hxdFitsHeaderUtil.h"
#include "fitsio.h"

#include "HXDarf.h"
#define DEBUG 0

char HXDarfout_version[] = "version 2.0.0";
static char pname[] = "HXDarfInit";

void
HXDarfout_startup(int *status){ *status = ANL_OK; }

void
HXDarfout_com(int *status){
  *status = ANL_OK;
  return;
}

void
HXDarfout_init(int *status){
  *status = ANL_OK;
  return;
}

void
HXDarfout_his(int *status){
  *status = ANL_OK; 
  return;
}

void
HXDarfout_bgnrun(int *status){
  *status = ANL_OK;
  return;
}

void
HXDarfout_ana(int nevent, int eventid, int *status){
  *status = ANL_OK;
  return;
}

void
HXDarfout_endrun(int *status){ 
  HxdArfData arf_data;
  int size;
  char arf_fname   [HXDARF_MAX_FNAME_LENGTH];
  char create_name [HXDARF_MAX_FNAME_LENGTH];
  int  create_size;
  int dettype, detid;
  HxdArfInfo arf_com;
  int stat;
  int merge;
  char comments[HXDARF_MAX_COMMENT_LINE][HXDARF_MAX_COMMENT_LEN_PLINE];
  int  comment_lines;
  int iline;
  int extension_id;
  fitsfile *fp;

  BnkGet("HXDarfInit:DETTYPE", sizeof(int), &size, &dettype);
  BnkGet("HXDarfInit:DET_ID", sizeof(int),  &size, &detid);
  BnkGet("HXDarfInit:MERGE_FLG", sizeof(int), &size, &merge);

  BnkGet("HXDarf:PIL:create_name", sizeof(char)*HXDFITSHEADER_LINESIZE, 
	 &size, create_name);
  BnkGet("HXDarf:PIL:create_size", sizeof(int), &size, &create_size);
  create_name[create_size] = '\0';

  /**======= PIN ====== **/
  if (dettype == HXDARF_DETTYPE_PIN) {
    sprintf(arf_com.detnam, "WELL_PIN");
    if (! merge) {
      sprintf(arf_fname, "%s_pin%02d.arf", create_name, detid);
    } else {
      sprintf(arf_fname, "%s_pin.arf", create_name);
    }
  /**======= GSO ====== **/
  } else if (dettype == HXDARF_DETTYPE_GSO){
    sprintf(arf_com.detnam, "WELL_GSO");
    if (! merge) {
      sprintf(arf_fname, "%s_gso%02d.arf", create_name, detid);
    } else {
      sprintf(arf_fname, "%s_gso.arf",create_name);
    }
  }
  BnkGet("HXDarfInit:COMMENT", sizeof(comments), &size, comments);
  BnkGet("HXDarfInit:COMMENT:NUM", sizeof(int),  &size, &comment_lines);

  sprintf(comments[comment_lines], "  hxdarfgen: out fname = %s", arf_fname);
  comment_lines++;

  BnkGet("HXDarfMerge:ARF", sizeof(HxdArfData), &size, &arf_data);
  sprintf(arf_com.telescop, "SUZAKU");
  sprintf(arf_com.instrume, "HXD");

  stat = hxdrspUtil_arf_create_fits( arf_fname, &arf_com);
  if(stat == HXDRSPUTIL_NG) {
    fprintf(stderr, "Error In creating arf file\n");
    *status = ANL_NG;
    return;
  }

  for(iline=0;iline<comment_lines; iline++){
    if (DEBUG) printf("%d: %s\n", iline, comments[iline]);
    stat = hxdrspUtil_arf_fits_add_comment(comments[iline]);
    if(stat == HXDRSPUTIL_NG) {
      fprintf(stderr, "Error In writing comment(linx=%d)\n",iline);
      *status = ANL_NG;
      return;
    }
  }

  stat = hxdrspUtil_arf_write_fits ( &arf_data);
  if(stat == HXDRSPUTIL_NG) {
    fprintf(stderr, "Error In writing arf file\n");
    *status = ANL_NG;
    return;
  }
  /** paremeters are written as comments **/
  /*
  fp = hxdrspUtil_arf_Get_FITS_fp();
  for(extension_id=1;extension_id<3; extension_id++){
    stat = hxdFitsHeader_writeParamer_arf(fp, extension_id);
    if(stat == HXD_FITS_HEADER_UTIL_NG) {
      fprintf(stderr, "Error In updating arf header\n");
      *status = ANL_NG;
      return;
    }
  }
  */

  stat = hxdrspUtil_arf_close_fits ();
  if(stat == HXDRSPUTIL_NG) {
    fprintf(stderr, "Error In closing arf file\n");
    *status = ANL_NG;
    return;
  }

  *status = ANL_OK; 
  return;
}

void
HXDarfout_exit(int *status){
  *status = ANL_OK; 
  return;
}
/******** EOF *********/
