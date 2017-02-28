/* 
 * tkAppInit.c --
 *
 *	Provides a default version of the Tcl_AppInit procedure for
 *	use in wish and similar Tk-based applications.
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * Copyright (c) 1994 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

#ifndef lint
static char sccsid[] = "@(#) tkAppInit.c 1.15 95/06/28 13:14:28";
#endif /* not lint */

#include "tk.h"
#include "xdf.h"
EXTERN int		Itcl_Init _ANSI_ARGS_((Tcl_Interp *interp));
EXTERN int		Itk_Init _ANSI_ARGS_((Tcl_Interp *interp));

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

extern int matherr();
static int (*dummyMathPtr)() = matherr;


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
 * tcl_bin_copy
 */
int
xdf_tcl_bin_copy( cdata, interp, argc, argv )
  ClientData *cdata;
Tcl_Interp *interp;
int argc;
char **argv;
{
  char *buf;
  int bsize, mode, read;
  Tcl_Channel inchan, ouchan;
  
  if( argc != 4 ) return TCL_ERROR;
  
  if( sscanf(argv[3],"%d",&bsize) != 1 ) return TCL_ERROR;
  buf = (char *)malloc(bsize*sizeof(char));
  
  inchan = Tcl_GetChannel(interp, argv[1], &mode);
  if( ! ( mode & TCL_READABLE ) ) return TCL_ERROR;
  
  ouchan = Tcl_GetChannel(interp, argv[2], &mode);
  if( ! ( mode & TCL_WRITABLE ) ) return TCL_ERROR;
  
  while( ! Tcl_Eof(inchan) ){
    if( (read = Tcl_Read(inchan, buf, bsize)) == -1 ) return TCL_ERROR;
    if( Tcl_Write(ouchan, buf, read) != read ) return TCL_ERROR;
  }
  
  free(buf);
  
  return TCL_OK;
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
  if (Tcl_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  if (Tk_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  
  /*
   * Call the init procedures for included packages.  Each call should
   * look like this:
   *
   * if (Mod_Init(interp) == TCL_ERROR) {
   *     return TCL_ERROR;
   * }
   *
   * where "Mod" is the name of the module.
   */
  if (Itcl_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  if (Itk_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }
  if (Tcl_PkgRequire(interp, "Iwidgets", (char*)NULL, 0) == NULL) {
    return TCL_ERROR;
  }
  
  /*
   * Call Tcl_CreateCommand for application-specific commands, if
   * they weren't already created by the init procedures called above.
   */
  
  /*
   * Specify a user-specific startup file to invoke if the application
   * is run interactively.  Typically the startup file is "~/.apprc"
   * where "app" is the name of the application.  If this line is deleted
   * then no user-specific startup file will be run under any conditions.
   */
  
  Tcl_CreateCommand (interp, "SelectObs", SelectObs,
		     (ClientData)Tk_MainWindow(interp), 
		     (Tcl_CmdDeleteProc *)NULL);
  
  Tcl_CreateCommand (interp, "SelectAppIds", SelectAppIds,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);
  
  Tcl_CreateCommand (interp, "SelectFiles", SelectFiles,
		     (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);
  
  /*
   * Special command for copying binary files.
   */
  Tcl_CreateCommand(interp, "xdfbcopy", xdf_tcl_bin_copy,
		    (ClientData *)NULL, (Tcl_CmdDeleteProc *)NULL);
  
  Tcl_SetVar(interp, "tcl_rcFileName", "~/.itkwishrc", TCL_GLOBAL_ONLY);
  return TCL_OK;
}

