/* 
 * Visu_Init.c
 *
 *	This file contains interpreter initialization
 *	functions. After the tkUnixInit.c file in the Tk4.1 distribution.
 *
 * Copyright (c) 1995-1996 Sun Microsystems, Inc.
 * Copyright (c) 1995-1996 The Regents of the University of California
 * Copyright (c) 1996      Pierre-Louis Bossart 
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */




#include "tkpict.h"

/* globals linked with tcl variables */
int Private_Colormap = 0;
int slice_nb = 0;
int nb_slices = 1;

/*
 * Default directory in which to look for libraries:
 */

#if !(defined(__WIN32__) || defined(macintosh))
static char defaultLibraryDir[200] = VISU_LIBRARY;
#endif

/*
 * The following string is the startup script executed in new
 * interpreters.  It looks on disk in several different directories
 * for a script "visu.tcl" that is compatible with this version
 * of Visu.  The visu.tcl script does all of the real work of
 * initialization.
 */

static char *initScript = "" ;


int Visu_Init(interp)
     Tcl_Interp *interp;		/* Interpreter for application. */
{
#if !(defined(__WIN32__) || defined(macintosh))
  const char *libDir;
#endif

#if !(defined(__WIN32__) || defined(macintosh))
  /* create pict image format */
  Tk_CreateImageType(&tkPictImageType);
  
  /* least current files first */
  
  Tcl_LinkVar(interp,"has_plb_segment",(char*)&has_plb_segment,TCL_LINK_INT);
#endif

  Tcl_LinkVar(interp,"slice_nb",(char*)&slice_nb,TCL_LINK_INT);
  Tcl_LinkVar(interp,"nb_slices",(char*)&nb_slices,TCL_LINK_INT);
  Tcl_LinkVar(interp,"Private_Colormap",(char*)&Private_Colormap,TCL_LINK_INT);

#if !(defined(__WIN32__) || defined(macintosh))
  libDir = Tcl_GetVar(interp, "visu_library", TCL_GLOBAL_ONLY);
  if (libDir == NULL) {
    Tcl_SetVar(interp, "visu_library", defaultLibraryDir, TCL_GLOBAL_ONLY);
  }
#endif
  return Tcl_Eval(interp, initScript);
}
