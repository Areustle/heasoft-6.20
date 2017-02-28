#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../include/xmtcl.h"
#include "cfortran.h"
#include "fitsio.h"

#include "../include/xcommon.h"
#include "../include/maxvals.h"
#include "../include/cmddef.h"
#include "../include/map.h"
#include "../include/null.h"

/*
 * chheader --
 *
 * Command implemented in C, so header values don't have to go
 *  from text to numbers and back
 */
int
chheader(ClientData cdata,Tcl_Interp* interp,int objc, Tcl_Obj* CONST objv[] )
{
   cmddef *curcmd;
   pardef *parlist;
   int status;

   char mapid[MAX_IDSTR];
   char key[FLEN_KEYWORD];
   char val[FLEN_VALUE];
   int show;
   char template[MAX_FILELEN];

   int di;
   float dr;
   double dd;
   char type[1], msg[100];
   void *valptr;
   char *string;
   Tcl_Obj *objPtr;

   status = 0;
   *mapid = '\0';
   *key = '\0';
   *val = '\a';     /* Use ASCII bell as flag character to allow for
                       NULL as entry to value */
   show = 0;
   *template = '\0';

   curcmd = cdata;
   if ( !CParseParm(interp, objc, objv, curcmd) ) {
      return TCL_ERROR;
   }
   if ( curcmd->cmdargc !=0 ) {
      cwrongargs(curcmd->cmdargs);
      return TCL_ERROR;
   }
   parlist = curcmd->parlist;
   cgpars(parlist,"mapid",mapid,MAX_IDSTR,&status);
   cgpars(parlist,"key",key,FLEN_KEYWORD,&status);
   cgpars(parlist,"value",val,FLEN_VALUE,&status);
   cgparl(parlist,"show",&show,&status);
   cgpars(parlist,"template",template,FLEN_FILENAME,&status);
   if ( status != 0 ) return TCL_ERROR;

   if ( *mapid == '?' ) {
      LSMAPID();
      return TCL_OK;
   }
   if ( !*mapid ) strcpy(mapid, "CUR");
   if ( !*key ) show = 1;
   if ( *template ) show = 1;
   if ( show ) {
      if ( !*template ) strcpy(template, "chheader");
      PRHEAD(mapid, template, status);
      return TCL_OK;
   }

   status = 0;
   GHEADTYPE(key, type, status);
   if ( status != 0 ) {
      sprintf(msg, " Keyword not found: %s", key);
      cxwrite(msg, 10);
      return TCL_ERROR;
   }
   if ( *val != '\a' ) {  /* If value given, set, otherwise, get */
      switch (*type) {
         case 'i':
            valptr = allocval('i', val);
            if ( !valptr ) return TCL_ERROR;
            GHEADI(mapid, key, * (int *) valptr, 1, status);
            if ( status == 0 ) {
               sprintf(msg, " Map: %s Keyword: %s = %d",
                              mapid, key, * (int *) valptr);
               cxwrite(msg, 10);
            }
            free(valptr);
            break;
         case 'd':
            valptr = allocval('r', val);
            if ( !valptr ) return TCL_ERROR;
            dr = * (double *) valptr;
            if ( ISRNULL(dr) ) * (double *) valptr = DNULL();
            GHEADD(mapid, key, * (double *) valptr, 1, status);
            if ( status == 0 ) {
               if ( ISDNULL(* (double *) valptr) ) {
                  sprintf(msg, " Map: %s Keyword: %s = NULL", 
                                mapid, key);
               } else {
                  sprintf(msg, " Map: %s Keyword: %s = %f", 
                                mapid, key, * (double *) valptr);
               }
               cxwrite(msg, 10);
            }
            free(valptr);
            break;
         case 's':
            valptr = allocval('s', val);
            if ( !valptr ) return TCL_ERROR;
            string = (char *) valptr;
            GHEADS(mapid, key, string, 1, status);
            if ( status == 0 ) {
               sprintf(msg, " Map: %s Keyword: %s = %s", 
                       mapid, key, (char *) valptr);
               cxwrite(msg, 10);
            }
            free(valptr);
            break;
         default: 
            cxwrite(" Invalid type to set in CHHEADER", 10);
            return TCL_ERROR;
      } 
   } else {              /* Return value as object*/
      switch (*type) {
         case 'i':
            GHEADI(mapid, key, di, 0, status);
            objPtr = Tcl_NewIntObj(di);
            break;
         case 'd':
            GHEADD(mapid, key, dd, 0, status);
            if ( ISDNULL(dd) ) dd = RNULL();
            objPtr = Tcl_NewDoubleObj(dd);
            break;
         case 's':
            GHEADS(mapid, key, val, 0, status);
            objPtr = Tcl_NewStringObj(val, -1);
            break;
         default: 
            cxwrite(" Invalid type to get in CHHEADER", 10);
            return TCL_ERROR;
      }
      if ( status == 0 ) Tcl_SetObjResult(interp, objPtr);
   }

   if ( status != 0 ) return TCL_ERROR;
   return TCL_OK;
}
