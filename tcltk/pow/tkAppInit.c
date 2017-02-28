/* 
 * visu.c --
 *
 *     custom version of the Tcl_AppInit procedure for
 *	use in visu
 *
 * Copyright (c) 1995 The Regents of the University of California.
 *
 * Author: Pierre-Louis Bossart
 * Date: November 17, 1995
 *
 * derived from Tcl_AppInit.c in the  tk4.0b2 distribution copyrighted
 * as follows :
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#include "pow.h"



int readpha(ClientData, Tcl_Interp *, int, char **); /* This is a sample
							user function that
							"creates" data
							and allows use
							of powCreateData */




/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

extern int matherr();
int *tclDummyMathPtr = (int *) matherr;


/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tk_Main never returns here, so this procedure never
 *	returns either.
 *
 * Side effects:
 *	Whatever the application does.
 *
 *----------------------------------------------------------------------
 */

int
main(argc, argv)
    int argc;			/* Number of command-line arguments. */
    char **argv;		/* Values of command-line arguments. */
{
  Tk_Main(argc, argv, Tcl_AppInit);
  return 0;			/* Needed only to prevent compiler warning. */
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *	Most applications, especially those that incorporate additional
 *	packages, will have their own version of this procedure.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 * Side effects:
 *	Depends on the startup script.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(interp)
    Tcl_Interp *interp;		/* Interpreter for application. */
{
#if (TK_MINOR_VERSION == 0)
  Tk_Window main;
  main = Tk_MainWindow(interp);
#endif  

  if (Tcl_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  if (Tk_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  if (Visu_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  if (Pow_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
    

  Tcl_CreateCommand(interp,"readpha",readpha,
		    (ClientData) NULL, 
		    (Tcl_CmdDeleteProc *) NULL);

  return TCL_OK;
}
