#include <tk.h>
#include <tix.h>
#include <itcl.h>
#include <itk.h>
#include "pow.h"
#include "fitsTcl.h"
#include "fvexec.h"

int Et_AppInit(Tcl_Interp *interp)
{
  char str[80];

  sprintf(str, "TIX_VERSION=%s", TIX_RELEASE);
  putenv(str);

  if (Itcl_Init(interp) == TCL_ERROR) {
     return TCL_ERROR;
  }

  /*
   *  This is itclsh, so import all [incr Tcl] commands by
   *  default into the global namespace.  Fix up the autoloader
   *  to do the same.
  */
  if (Tcl_Import(interp, Tcl_GetGlobalNamespace(interp),
          "::itcl::*", /* allowOverwrite */ 1) != TCL_OK) {
      return TCL_ERROR;
  }

  if (Tcl_Eval(interp, "auto_mkindex_parser::slavehook { _%@namespace import -force ::itcl::* }") != TCL_OK) {
      return TCL_ERROR;
  }

  if (Itk_Init(interp) == TCL_ERROR) {
     return TCL_ERROR;
  }

  /*
   *  This is itclsh, so import all [incr Tcl] commands by
   *  default into the global namespace.  Fix up the autoloader
   *  to do the same.
  */
  if (Tcl_Import(interp, Tcl_GetGlobalNamespace(interp),
          "::itk::*", /* allowOverwrite */ 1) != TCL_OK) {
      return TCL_ERROR;
  }

  if (Tcl_Eval(interp, "auto_mkindex_parser::slavehook { _%@namespace import -force ::itk::* }") != TCL_OK) {
      return TCL_ERROR;
  }

  if (Fits_Init(interp) == TCL_ERROR) {
     return TCL_ERROR;
  }

  if (Pow_InitExec(interp) == TCL_ERROR) {
     return TCL_ERROR;
  }

  if (Tix_Init(interp) == TCL_ERROR) {
     fprintf(stderr, "Error initializing TIX\n");
     return TCL_ERROR;
  }

  Tcl_StaticPackage(interp, "Tix", Tix_Init, (Tcl_PackageInitProc *) NULL);

  /* Pan Chai: add XPA shared library */

  if (Tclxpa_Init(interp) == TCL_ERROR) {
     fprintf(stderr, "Error initializing XPA\n");
     return TCL_ERROR;
  }

  Tcl_StaticPackage(interp, "tclxpa", Tclxpa_Init, NULL); 

  return TCL_OK;
}

