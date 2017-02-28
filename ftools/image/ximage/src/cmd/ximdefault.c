/*
 *  ximdefs.c
 *  Manage default values using a parameter file
 *
 *  Routines: getdefault, setdefault
 */

#include <stddef.h>
#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "../include/xmtcl.h"
#include "pil.h"
#include "cfortran.h"

#include "ape/ape_trad.h"
#include "ape/ape_util.h"

void getdefault (int *status) {
/*
 *  Get default settings from ximage parameter file
 *
 *  O  status       (i) Error flag (0 = OK)
 */
   char **pardata = 0;
   char **parname = 0;
   char *pfilename;
   static char pfilebuf[] = "ximage";

   pfilename = pfilebuf;

   *status = 0;

   if ( PILInit(1, &pfilename) ) {
      cxwrite("Failed to initialize PIL", 10);
      goto failed;
   }

   if ( PILOverrideQueryMode(PIL_QUERY_OVERRIDE) ) {
      cxwrite("Failed to set PIL to non-query mode", 10);
      goto failed;
   }

   if ( ape_trad_get_par_names(&pardata) ) {
      cxwrite("Failed to get names of parameters", 10);
      goto failed;
   }
   for ( parname = pardata; 0 != *parname; ++parname ) {
      char *parvalue = 0;

      /* Ignore mode parameter */
      if ( strcmp(*parname,"mode") == 0 ) continue;

      /* Get parameter value. */
      if ( ape_trad_get_string(*parname, &parvalue) ) {
         cxwrite("Failed to get parameter", 10);
         goto failed;
      }

      Tcl_SetVar2(xm_interp, "default", *parname,
                  parvalue, TCL_GLOBAL_ONLY);

      free(parvalue);
   }
   ape_util_free_string_array(pardata);
   return;
   
failed:
   *status = -1;
}
FCALLSCSUB1(getdefault,GETDEFAULT,getdefault,PINT)

void setdefault (int *status) {
/*
 *  Set default settings in ximage parameter file
 *
 *  O  status       (i) Error flag (0 = OK)
 */
   char **pardata = 0;
   char **parname = 0;
   char *parvalue = 0;
   char partype[APE_PAR_TYPE_CODE_LEN] = "";

   Tcl_Obj *obj, *defobj;
   int dbool, dint;
   char *dstr;
   double ddbl;

   *status = 0;

   defobj = Tcl_NewStringObj("default", -1);

   if ( ape_trad_get_par_names(&pardata) ) {
      cxwrite("Failed to get names of parameters", 10);
      goto failed;
   }
   for ( parname = pardata; 0 != *parname; ++parname ) {
  
      /* Get value of parameter from 'default' array */
      obj = Tcl_ObjGetVar2(xm_interp, defobj,
                           Tcl_NewStringObj(*parname, -1),
                           TCL_GLOBAL_ONLY);
      if ( !obj ) continue;  /* If missing, go to next */

      if ( ape_trad_get_type(*parname, partype) ) {
         cxwrite("Failed to get type of parameter", 10);
         goto failed;
      }
      
      /* Put parameter back in file based on type */
      switch (partype[0]) {
         case 'b':
            Tcl_GetBooleanFromObj(xm_interp, obj, &dbool);
            PILPutBool(*parname, dbool);
            break;
         case 'i':
            Tcl_GetIntFromObj(xm_interp, obj, &dint);
            PILPutInt(*parname, dint);
            break;
         case 's':
            dstr = Tcl_GetStringFromObj(obj, &dint);
            PILPutString(*parname, dstr);
            break;
         case 'f':
            dstr = Tcl_GetStringFromObj(obj, &dint);
            PILPutFname(*parname, dstr);
            break;
         case 'd':
         case 'r':
            Tcl_GetDoubleFromObj(xm_interp, obj, &ddbl);
            PILPutReal(*parname, ddbl);
            break;
         default: 
            cxwrite(" Unknown parameter type", 10);
            goto failed;
      }
   }

   PILClose(*status);
   return;

failed:
   *status = -1;

}
FCALLSCSUB1(setdefault,SETDEFAULT,setdefault,PINT)
