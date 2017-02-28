#ifdef __WIN32__
#include <windows.h>
#endif
#include "powdata.h"

int Powclient_Init(Tcl_Interp *interp_instance);
/* globals linked with tcl variables */


int tty = 0;
int Pow_Done = 0;  /* current method for event handling, will change */

int pixelSizes[5] = {1,2,4,4,8};
Tcl_HashTable PowDataTable;


Tcl_Interp *interp = NULL;		/* Interpreter for application. */

int Powclient_Init(Tcl_Interp *interp_instance) {
  Tcl_DString pow_env;
  char *charptr;
  char temp[1000];

  Tcl_DStringInit(&pow_env);

  interp = interp_instance;


  charptr = Tcl_GetVar2(interp,"env", "POW_LIBRARY", TCL_GLOBAL_ONLY);
  if( charptr == NULL ) {
      puts("Could not find defaults.");
      puts("  Set your POW_LIBRARY environment variable.");
      return TCL_ERROR;
  } else {
    Tcl_DStringAppend(&pow_env,charptr, -1);
  }

  /*  Brackets needed around %s to prevent Windows-style paths  */
  /*  ... eg, c:\fv... from getting converted to control chars  */
  /*  when doing the GlobalEval.                                */
  sprintf(temp,"lappend auto_path {%s}; powXPA::init",charptr);
  if( Tcl_GlobalEval(interp,temp) == TCL_ERROR )
     return TCL_ERROR;

  
  Tcl_InitHashTable(&PowDataTable, TCL_STRING_KEYS);


  Tcl_CreateCommand(interp,"powCreateDataFromList",PowCreateDataFromList,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);

  Tcl_CreateObjCommand(interp,"powCreateDataFromChannel",
                       PowCreateDataFromChannel,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powCreateDataFromPtr",
                       PowCreateDataFromPtr,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);
  Tcl_CreateObjCommand(interp,"powCreateStrFromPtr",
                       PowCreateStrFromPtr,
                       (ClientData) NULL, 
                       (Tcl_CmdDeleteProc *) NULL);

  return TCL_OK;
}


