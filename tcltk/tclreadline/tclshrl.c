
 /* ==================================================================

    FILE: "/home/joze/src/tclreadline/tclshrl.c"
    LAST MODIFICATION: "Sat, 25 Mar 2000 20:51:39 +0100 (joze)"
    (C) 1998 - 2000 by Johannes Zellner, <johannes@zellner.org>
    $Id: tclshrl.c,v 1.1.1.1 2001/02/27 20:11:49 irby Exp $
    ---

    tclreadline -- gnu readline for tcl
    http://www.zellner.org/tclreadline/
    Copyright (c) 1998 - 2000, Johannes Zellner <johannes@zellner.org>

    This software is copyright under the BSD license.

    ================================================================== */  

#ifdef HAVE_CONFIG_H
#   include "config.h"
#endif

#include <tcl.h>
#include <tclreadline.h>

#if 0
#include <assert.h>
#endif

extern int Tclreadline_Init(Tcl_Interp *interp);
extern int Tclreadline_SafeInit(Tcl_Interp *interp);

int
TclreadlineAppInit(Tcl_Interp* interp)
{
    char file[0xff];
    int status;
#if 0
    assert(Tcl_InitStubs(interp, TCL_VERSION, 0));
#endif
    if (TCL_ERROR == Tcl_Init(interp)) {
	return TCL_ERROR;
    }
    if (TCL_ERROR == Tclreadline_Init(interp)) {
	return TCL_ERROR;
    }
    Tcl_StaticPackage(interp, "tclreadline",
	Tclreadline_Init, Tclreadline_SafeInit);
#if (TCL_MAJOR_VERSION == 7) && (TCL_MINOR_VERSION == 4)
    tcl_RcFileName = "~/.tclshrc";
#else
    Tcl_SetVar(interp, "tcl_rcFileName", "~/.tclshrc", TCL_GLOBAL_ONLY);
#endif
    sprintf(file, "%s/tclreadlineInit.tcl", TCLRL_LIBRARY);
    if ((status = Tcl_EvalFile(interp, file))) {
	fprintf(stderr, "(TclreadlineAppInit) unable to eval %s\n", file);
	exit (1);
    }
    return TCL_OK;
}

int
main(int argc, char *argv[])
{
    Tcl_Main(argc, argv, TclreadlineAppInit);
    return 0;
}
