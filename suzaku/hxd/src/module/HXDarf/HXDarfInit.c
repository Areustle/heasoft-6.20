/*
 *   HXDarf created by Y. Matsumoto, Y.Terada
 *         v0.1.0 mod Y.Terada 1999-10-27
 *         v0.2.1 mod Y.Terada 2000-01-25
 *         v0.3.0 for HEADAS
 *
 *         v0.4.0 HXD-II version, created by K.Tamura, Y.Terada  2005-05-23
 *         v0.4.1 debug ANL loop, by Y.Terada 2005-06-06
 *         v0.4.2 add comments of Inputs by Y.Terada 2005-06-06
 *         v0.4.4 modify calc dettype part by K.Tamura 2005-06-15
 *         v0.4.5 debug by Y.Terada 2005-06-22
 *         v0.4.6 by Y.Terada 2005-07-04
 *         v0.4.7 by Y.Terada 2005-07-04, write PIL parameters
 *         v0.4.8 mod Y.Terada 2006-08-25
 *                for headas_body v1.71, for ver1.2.2.3 process 
 *         v0.5.0 debug by T.Kitaguchi, K.Tamura 2007-03-12
 *         ---> v1.0.0
 *         v2.0.0 support CALDB access function, Y.Terada 2007-05-01
 *                add create_name for output.
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

#include "HXDarf.h"
#include "aste_caldb.h"

#include "pil.h"
#include "headas.h"

char HXDarfInit_version[] = "version 2.0.0";
static char pname[] = "HXDarfInit";

#define DEBUG 1

static struct {
  /** Input value, in par file **/
  int pinid, gsoid;
  char create_name   [HXDARF_MAX_FNAME_LENGTH];
  char teldef_fname  [HXDARF_MAX_FNAME_LENGTH];
  char attitude_fname[HXDARF_MAX_FNAME_LENGTH];
  char arfdb_fname   [HXDARF_MAX_FNAME_LENGTH];
  int point_yes;
  double point_ra, point_dec;
  char image_fname   [HXDARF_MAX_FNAME_LENGTH];
  char pi_fname      [HXDARF_MAX_FNAME_LENGTH];

  int dettype; /** PIN or GSO **/
  int merge;

  char o_teldef_fname  [HXDARF_MAX_FNAME_LENGTH];
  char o_arfdb_fname   [HXDARF_MAX_FNAME_LENGTH];
  int  caldb_type_teldef; /* 0:explicit, 1:CALDB */
  int  caldb_type_arfdb ; /* 0:explicit, 1:CALDB */

  char comments[HXDARF_MAX_COMMENT_LINE][HXDARF_MAX_COMMENT_LEN_PLINE];
} hxdarfinit_com;

void
HXDarfInit_startup(int *status){ 
  BnkDef("HXDarf:PIL:create_name",     sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXDarf:PIL:create_size",     sizeof(int));
  BnkDef("HXDarf:PIL:hxd_arf_pinid",   sizeof(int));
  BnkDef("HXDarf:PIL:hxd_arf_gsoid",   sizeof(int));
  BnkDef("HXDarf:PIL:hxd_teldef",      sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXDarf:PIL:attitude",        sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXDarf:PIL:hxd_arfdb_name",  sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXDarf:PIL:hxdarf_point_yn", sizeof(int)   );
  BnkDef("HXDarf:PIL:point_ra",        sizeof(double));
  BnkDef("HXDarf:PIL:point_dec",       sizeof(double));
  BnkDef("HXDarf:PIL:image_fname",
          sizeof(char)*HXDFITSHEADER_LINESIZE);
  BnkDef("HXDarf:PIL:input_pi_name",
          sizeof(char)*HXDFITSHEADER_LINESIZE);

  *status = ANL_OK; 
  return;
}

void
HXDarfInit_com(int *status){
  CALDB_INFO caldb;
  char *k;
  int string_size ;

  /**** ftools ****/
  if ( *status ) {
    
    *status = 0;

    /** ================== output name =======================**/
    *status = PILGetFname(k="create_name", hxdarfinit_com.create_name);
    if ( *status ) {
      fprintf(stderr, "%s: get create_name error\n", pname);
      *status = ANL_QUIT;
      return;      
    } else {
      string_size = strlen(hxdarfinit_com.create_name);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning create_name is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXDarf:PIL:create_name",   string_size,
	     hxdarfinit_com.create_name);
      BnkPut("HXDarf:PIL:create_size",   sizeof(int), &string_size);
    }

    /** ================== pinid ============================**/
    *status = PILGetInt("hxd_arf_pinid", &hxdarfinit_com.pinid);
    if ( *status ) {
      fprintf(stderr, "%s: get hxd_arf_pinid error\n", pname);
      *status = ANL_QUIT;
      return;      
    } else {
      BnkPut("HXDarf:PIL:hxd_arf_pinid",   sizeof(int), 
	     &hxdarfinit_com.pinid );
    }
    
    /** ================== gsoid ============================**/
    *status = PILGetInt("hxd_arf_gsoid", &hxdarfinit_com.gsoid);
    if ( *status ) {
      fprintf(stderr, "%s: get hxd_arf_gsoid error\n", pname);
      *status = ANL_QUIT;
      return;      
    } else {
      BnkPut("HXDarf:PIL:hxd_arf_gsoid",   sizeof(int),
	     &hxdarfinit_com.gsoid);
    }
    
    aste_caldb_init(&caldb);

    /** ================== CALDB: teldef =====================**/
    *status = PILGetFname(k="hxd_teldef", hxdarfinit_com.o_teldef_fname);
    if ( *status ) {
      fprintf(stderr, "%s: get teldef_name error\n", pname);
      *status = ANL_QUIT;
      return;      
    } else {
      /*-- caldb access --*/
      if (0==CLstricmp("CALDB", hxdarfinit_com.o_teldef_fname) ){
        hxdarfinit_com.caldb_type_teldef = 1;
        caldb.telescop = "SUZAKU";        /* TELESCOP */
        caldb.instrume = "HXD";           /* INSTRUME */
	caldb.detnam   = "NONE";          /* DETNAME */
        caldb.codename = "TELDEF";        /* CCNM0001 */
        aste_caldb_get(&caldb);
        if (caldb.status != 0 || caldb.nfound == 0){
          anl_msg_error("%s: no CALDB entry for '%s-%s' (status=%d)\n",
                        pname, caldb.instrume, caldb.codename, caldb.status);
          *status = ANL_QUIT;
          return;
        }
        if (caldb.nfound != 1){
          anl_msg_warning("%s: multiple CALDB entry for '%s' (nfound=%d)\n",
                        pname, caldb.codename, caldb.nfound);
        }
        strcpy(hxdarfinit_com.teldef_fname, caldb.filename);
      } else{
        hxdarfinit_com.caldb_type_teldef = 0; /* given fname*/
        strcpy(hxdarfinit_com.teldef_fname, hxdarfinit_com.o_teldef_fname);
      }
      /*-- Put filename --*/
      string_size = strlen(hxdarfinit_com.teldef_fname);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning hxd_teldef is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXDarf:PIL:hxd_teldef", string_size,
             hxdarfinit_com.teldef_fname);
      if(DEBUG) fprintf(stderr,"teldef=%s\n", hxdarfinit_com.teldef_fname);
    }

    /** ===================== attitude =======================**/
    *status = PILGetFname("attitude", hxdarfinit_com.attitude_fname);
    if ( *status ) {
      fprintf(stderr, "%s: get attitude_name error\n", pname);
      *status = ANL_QUIT;
      return;      
    } else {
      string_size = strlen(hxdarfinit_com.attitude_fname);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning attitude is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXDarf:PIL:attitude", string_size,
             hxdarfinit_com.attitude_fname);
    }

    /** ================== CALDB: arf database ================**/
    *status = PILGetFname("hxd_arfdb_name", hxdarfinit_com.o_arfdb_fname);
    if ( *status ) {
      fprintf(stderr, "%s: get hxd_arfdb_name error\n", pname);
      *status = ANL_QUIT;
      return;      
    } else {
      /*-- caldb access --*/
      if (0==CLstricmp("CALDB", hxdarfinit_com.o_arfdb_fname) ){
        hxdarfinit_com.caldb_type_arfdb = 1;
        caldb.telescop = "SUZAKU";        /* TELESCOP */
        caldb.instrume = "HXD";           /* INSTRUME */
        caldb.codename = "ARFMATRIX";     /* CCNM0001 */

	if (0<= hxdarfinit_com.pinid && hxdarfinit_com.pinid <= 64){
	  caldb.detnam   = "WELL_PIN";       /* DETNAME */
	} else if (0 <= hxdarfinit_com.gsoid && hxdarfinit_com.gsoid <=16){
	  caldb.detnam   = "WELL_GSO";       /* DETNAME */
	} else {
          anl_msg_error("%s: Un defined DETNAM (CALDB %s)\n", 
			pname, caldb.codename);
	  *status = ANL_QUIT;  return;
	}
	  
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
        strcpy(hxdarfinit_com.arfdb_fname, caldb.filename);
      } else{
        hxdarfinit_com.caldb_type_arfdb = 0; /* given fname*/
        strcpy(hxdarfinit_com.arfdb_fname, hxdarfinit_com.o_arfdb_fname);
      }
      /*-- Put filename --*/
      string_size = strlen(hxdarfinit_com.arfdb_fname);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
        fprintf(stderr, "%s: warning hxd_arfdb_name is long\n", pname);
        string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXDarf:PIL:hxd_arfdb_name", string_size,
             hxdarfinit_com.arfdb_fname);
      if(DEBUG) fprintf(stderr,"arfdb-%s=%s\n", caldb.detnam, 
			hxdarfinit_com.arfdb_fname);
    }

    /** ================== point source y/n ==================**/
    *status = PILGetBool("hxdarf_point_yn", &hxdarfinit_com.point_yes);
    if ( *status ) {
      fprintf(stderr, "%s: get hxdarf_point_yn error\n", pname);
      *status = ANL_QUIT;
      return;      
    } else {
      BnkPut("HXDarf:PIL:hxdarf_point_yn", sizeof(int), 
	     &hxdarfinit_com.point_yes  );
    }

    if (hxdarfinit_com.point_yes) {
      *status = PILGetReal("point_ra", &hxdarfinit_com.point_ra);
      if ( *status ) {
	fprintf(stderr, "%s: get point_ra error\n", pname);
	*status = ANL_QUIT;
	return;      
      } else {
	  BnkPut("HXDarf:PIL:point_ra",        sizeof(double),
		 &hxdarfinit_com.point_ra);
      }

      *status = PILGetReal("point_dec", &hxdarfinit_com.point_dec);
      if ( *status ) {
	fprintf(stderr, "%s: get point_dec error\n", pname);
	*status = ANL_QUIT;
	return;      
      } else {
	BnkPut("HXDarf:PIL:point_dec",       sizeof(double),
	       &hxdarfinit_com.point_dec);
      }

    } else {
      fprintf(stderr, "%s: Warning, No support Image input\n",pname);
      *status = PILGetFname("image_fname", hxdarfinit_com.image_fname);
      if ( *status ) {
	fprintf(stderr, "%s: get image_fname error\n", pname);
	*status = ANL_QUIT;
	return;      
      } else {
	string_size = strlen(hxdarfinit_com.image_fname);
	if  (string_size>HXDFITSHEADER_LINESIZE) {
	  fprintf(stderr, "%s: warning image_fname is long\n", pname);
	  string_size = HXDFITSHEADER_LINESIZE;
	}
	BnkPut("HXDarf:PIL:image_fname", string_size,
	       hxdarfinit_com.image_fname);
      }
    }

    /** ==================== PHA file ========================**/
    *status = PILGetFname("input_pi_name", hxdarfinit_com.pi_fname);
    if ( *status ) {
      fprintf(stderr, "%s: get input_pi_name error\n", pname);
      *status = ANL_QUIT;
      return;      
    } else {
      string_size = strlen(hxdarfinit_com.pi_fname);
      if  (string_size>HXDFITSHEADER_LINESIZE) {
	fprintf(stderr, "%s: warning input_pi_name is long\n", pname);
	string_size = HXDFITSHEADER_LINESIZE;
      }
      BnkPut("HXDarf:PIL:input_pi_name", string_size,
	     hxdarfinit_com.pi_fname);
    }
    
    *status = ANL_OK;
    return;
  }
  /****** ANL ******/

  /* not used */
  *status = ANL_OK;
  
}

void
HXDarfInit_init(int *status){
  int arfdbutil_dettype;
  int stat;

  BnkDef("HXDarfInit:DETTYPE", sizeof(int));
  BnkDef("HXDarfInit:DET_ID",  sizeof(int));
  BnkDef("HXDarfInit:POS_ID",  sizeof(int));
  BnkDef("HXDarfInit:CALC_RA",  sizeof(double) );
  BnkDef("HXDarfInit:CALC_DEC", sizeof(double) );
  BnkDef("HXDarfInit:MERGE_FLG", sizeof(int) );
  BnkDef("HXDarfInit:COMMENT", sizeof(hxdarfinit_com.comments) );
  BnkDef("HXDarfInit:COMMENT:NUM", sizeof(int) );

  /** calc dettype **/
  if (0<= hxdarfinit_com.pinid && hxdarfinit_com.pinid <= 64){
    hxdarfinit_com.dettype = HXDARF_DETTYPE_PIN;
    BnkPut("HXDarfInit:DETTYPE", sizeof(int), &hxdarfinit_com.dettype);
    if (hxdarfinit_com.pinid == 64){
      hxdarfinit_com.merge = HXDARF_MERGE_YES;
    } else {
      hxdarfinit_com.merge = HXDARF_MERGE_NO;
    }
    BnkPut("HXDarfInit:MERGE_FLG", sizeof(int), &hxdarfinit_com.merge );


  } else if (0 <= hxdarfinit_com.gsoid && hxdarfinit_com.gsoid <=16){
    hxdarfinit_com.dettype = HXDARF_DETTYPE_GSO;
    BnkPut("HXDarfInit:DETTYPE", sizeof(int), &hxdarfinit_com.dettype);
    if (hxdarfinit_com.gsoid == 16){
      hxdarfinit_com.merge = HXDARF_MERGE_YES;
    } else {
      hxdarfinit_com.merge = HXDARF_MERGE_NO;
    }
    BnkPut("HXDarfInit:MERGE_FLG", sizeof(int), &hxdarfinit_com.merge );


  } else {
    fprintf(stderr, "%s: undefined PIN/GSO ID\n", pname);
    *status = ANL_QUIT;
    return;
  }


  /** Initialize **/
  if (hxdarfinit_com.dettype == HXDARF_DETTYPE_PIN) {
    arfdbutil_dettype = HXDARFUTIL_ARFDB_TYPE_PIN;
  } else if (hxdarfinit_com.dettype == HXDARF_DETTYPE_GSO) {
    arfdbutil_dettype = HXDARFUTIL_ARFDB_TYPE_GSO;
  }
  stat = hxdarfUtil_init(arfdbutil_dettype, 
			 hxdarfinit_com.arfdb_fname,
			 hxdarfinit_com.teldef_fname,
			 hxdarfinit_com.attitude_fname,
			 hxdarfinit_com.pi_fname);
  if (stat != HXDARFUTIL_STATUS_OK) {
    fprintf(stderr,"%s: arfUtil Initialize Error \n", pname);
    *status = ANL_QUIT; 
    return;
  }

}

void
HXDarfInit_his(int *status){ *status = ANL_OK; }

void
HXDarfInit_bgnrun(int *status){
  *status = ANL_OK; 
}

void
HXDarfInit_ana(int nevent, int eventid, int *status){
  double calc_ra, calc_dec;
  static int calc_detid = 0;
  static int calc_posid = 0;
  static int first = 1;

  if (! hxdarfinit_com.point_yes){
    fprintf(stderr, "%s: Support only POINT-arf\n",pname);
    *status = ANL_QUIT;
    return;
  }

  BnkPut("HXDarfInit:POS_ID", sizeof(int), &calc_posid);
  BnkPut("HXDarfInit:CALC_RA",  sizeof(double), &hxdarfinit_com.point_ra );
  BnkPut("HXDarfInit:CALC_DEC", sizeof(double), &hxdarfinit_com.point_dec);

  /******* PIN *********/
  if (hxdarfinit_com.dettype == HXDARF_DETTYPE_PIN) {
    if (hxdarfinit_com.merge){
      /** scan PIN ID **/
      BnkPut("HXDarfInit:DET_ID", sizeof(int), &calc_detid);
      calc_detid ++;
      if (calc_detid > 64){
	*status = ANL_QUIT;
	return;
      }
      *status = ANL_NEWROOT;
      return;

    } else {
      if (first) {
	BnkPut("HXDarfInit:DET_ID", sizeof(int), &hxdarfinit_com.pinid);
	first = 0;
	*status = ANL_OK;
	return;
      } else {
	*status = ANL_QUIT;
	return;
      }
    }
  /******* GSO *********/
  } else if (hxdarfinit_com.dettype == HXDARF_DETTYPE_GSO) {
    if (hxdarfinit_com.merge){
      /** scan GSO ID **/
      BnkPut("HXDarfInit:DET_ID", sizeof(int), &calc_detid);
      calc_detid ++;
      if (calc_detid > 16){
	*status = ANL_QUIT;
	return;
      }
      *status = ANL_NEWROOT;
      return;

    } else {
      if (first) {
	BnkPut("HXDarfInit:DET_ID", sizeof(int), &hxdarfinit_com.gsoid);
	first = 0;
	*status = ANL_OK;
	return;
      } else {
	*status = ANL_QUIT;
	return;
      }
    }
  }

  /** no reach **/
  return;
}

void
HXDarfInit_endrun(int *status){ 
  int stat; 
  char temp_card[HXDARF_MAX_COMMENT_LINE][HXDARF_MAX_COMMENT_LEN_PLINE];
  int  line=0;
  int iline;

  stat = hxdarfUtil_end();
  if (stat != HXDARFUTIL_STATUS_OK) {
    fprintf(stderr,"%s: close error\n", pname);
    *status = ANL_NG; 
    return;
  }

  /** make comments **/
  sprintf(hxdarfinit_com.comments[line],     "  hxdarfgen: teldef   = %s", 
	  hxdarfinit_com.teldef_fname); line++;
  sprintf(hxdarfinit_com.comments[line],     "  hxdarfgen: attitude = %s",
	 hxdarfinit_com.attitude_fname);line++;
  sprintf(hxdarfinit_com.comments[line],     "  hxdarfgen: arf db   = %s",
	 hxdarfinit_com.arfdb_fname);line++;
  if (hxdarfinit_com.point_yes){
    sprintf(hxdarfinit_com.comments[line],   "  hxdarfgen: Object Point (RA, DEC) = (%04.1f, %04.1f)",
	 hxdarfinit_com.point_ra, hxdarfinit_com.point_dec);line++;
  } else {
    sprintf(hxdarfinit_com.comments[line],   "  hxdarfgen: Object Image = %s",
	 hxdarfinit_com.image_fname);line++;
  }

  if (hxdarfinit_com.dettype == HXDARF_DETTYPE_PIN){
    char tmp_card[256];
    sprintf(tmp_card,                        "  hxdarfgen: calculation for PIN");
    if (hxdarfinit_com.merge) {
      sprintf(hxdarfinit_com.comments[line], "%s(0 -- 63, merged)",
	      tmp_card);
      line++;
    } else {
      sprintf(hxdarfinit_com.comments[line], "%s%d", tmp_card,
	      hxdarfinit_com.pinid);
      line++;
    }
  } else if (hxdarfinit_com.dettype == HXDARF_DETTYPE_GSO){
    char tmp_card[256];
    sprintf(tmp_card,                        "  hxdarfgen: calculation for GSO");
    if (hxdarfinit_com.merge) {
      sprintf(hxdarfinit_com.comments[line], "%s (W00 -- W33, merged)",
	      tmp_card);
      line++;
    } else {
      sprintf(hxdarfinit_com.comments[line], "%s W%01d%01d", tmp_card, 
	     hxdarfinit_com.gsoid >> 2, hxdarfinit_com.gsoid &0x03);
      line++;
    }
  }

  sprintf(hxdarfinit_com.comments[line],     "  hxdarfgen: PI file  = %s",
	 hxdarfinit_com.pi_fname);line++;


  BnkPut("HXDarfInit:COMMENT", sizeof(hxdarfinit_com.comments),
	 hxdarfinit_com.comments);
  BnkPut("HXDarfInit:COMMENT:NUM", sizeof(int), &line);

  *status = ANL_OK; 
  return;
}

void
HXDarfInit_exit(int *status){
  *status = ANL_OK; 
  return;
}

/******** EOF *********/
