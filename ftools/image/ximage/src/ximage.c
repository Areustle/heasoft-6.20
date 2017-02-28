#include <stdlib.h>

#include "include/xmtcl.h"
#include <tk.h>
#include <tclreadline.h>
#include "tkpgplot.h"

#include "include/maxvals.h"
#include "include/xcommon.h"
#include "include/cmddef.h"

#include "cmd/fcmd.h"
#include "cmd/ccmd.h"
#include "cfortran.h"

Tcl_Interp *xm_interp;
char *XM_PROMPT = "[XIMAGE> ";
char *cmdname;
Tcl_ObjCmdProc *cmdfnc;

#define XIMINIT(A) CCALLSFSUB1(XIMINIT,ximinit,LOGICAL, A)

int Ximage_Init(Tcl_Interp* interp);
Tcl_ObjCmdProc ximunk, xm_log, xm_script;
Tcl_ObjCmdProc TxwriteCmd, TxreadCmd, ParseParmCmd, XsourceCmd;

void load_tclimplot(Tcl_Interp *interp);

extern int Tclreadline_Init(Tcl_Interp *interp);
extern int Tclreadline_SafeInit(Tcl_Interp *interp);

extern int xm_echo_script;

static int standalone;

#define LINE_LEN 81

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tcl_Main never returns here, so this procedure never
 *	returns either.
 *
 *----------------------------------------------------------------------
 */

int
main(int argc,char ** argv)		/* Values of command-line arguments. */
{
    static char dash[] = "-";
    char **tmpargv;
    int i;

    standalone = argc>1 ? 0 : 1;

    /*
     *  Allocate new argv to contain '-' so arguments are treated
     *  as commands, rather than as a script file
     */

    tmpargv = (char **) malloc((argc+1)*sizeof(char *));
    if ( !tmpargv ) {
       cxwrite(" Failed to allocate space for arguments", 5);
       return 1;
    } 
    tmpargv[0] = argv[0];
    tmpargv[1] = dash;
    for ( i = 1; i < argc; i++ ) {
       tmpargv[i+1] = argv[i];
    }
    argc++;
    
    Tk_Main(argc, tmpargv, Tcl_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}

/*
 *----------------------------------------------------------------------
 *
 * Tcl_AppInit --
 *
 *	This procedure performs application-specific initialization.
 *
 * Results:
 *	Returns a standard Tcl completion code, and leaves an error
 *	message in interp->result if an error occurs.
 *
 *----------------------------------------------------------------------
 */

int
Tcl_AppInit(Tcl_Interp *interp)	/* Interpreter for application. */
{
    CONST char *msg;
    char *init_script, *tclreadline;
    char *tclrl_vers, *tclrl_library, *tclrl_init;
    int result;
    int status;

    if (Tcl_Init(interp) == TCL_ERROR) 
    {
        return TCL_ERROR;
    }

/* TK */
    if (Tk_Init(interp) == TCL_ERROR) 
    {
       fprintf(stdout, "Tk startup failed: /xtk device unavailable\n");
    } 
    else if (Tkpgplot_Init(interp) == TCL_ERROR)
    {
          return TCL_ERROR;
    }

    Tcl_StaticPackage(interp, "tclreadline",
        Tclreadline_Init, Tclreadline_SafeInit);
    Tcl_SetVar(interp, "tcl_rcFileName", "~/.wishrc", TCL_GLOBAL_ONLY);
    tclrl_vers = strcatalloc("/lib/tclreadline", TCLRL_VERSION_STR);
    tclrl_library = strcatalloc(getenv("LHEASOFT"), tclrl_vers);
    tclrl_init = strcatalloc(tclrl_library, "/tclreadlineInit.tcl");
    free(tclrl_vers);
    free(tclrl_library);
    if ((status = Tcl_EvalFile(interp, tclrl_init))) {
        fprintf(stderr, "(TclreadlineAppInit) unable to eval %s\n",
tclrl_init);
    }

/* TK */
/*
#ifndef WIN32
*/
    /* Set up handler for Ctrl-C or SIGINT  */
/*
    
    Sig_Setup(interp);
#endif
*/


    xm_interp = interp;
    if (Ximage_Init(interp) == TCL_ERROR) 
    {
	return TCL_ERROR;
    }

    /*
     * Source ximage standard initialization script
     */
    init_script = strcatalloc(getenv("XANADU"), 
                             "/image/ximage/manager/ximage.tcl");
    if ( init_script ) {
       result = Tcl_EvalFile(interp, init_script);
       if ( result != TCL_OK ) {
          msg = Tcl_GetStringResult(interp);
          cxwrite(msg, 5);
          cxwrite("Failed to execute init script:", 5);
          cxwrite(init_script, 5);
       }
       free(init_script);
    } else {
       cxwrite("Failed to construct path to init script:", 5);
    }

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  
     */

/* Start up tclreadline when Tk looping starts... */
    tclreadline = strcatalloc(getenv("XANADU"), 
                             "/image/ximage/manager/tclreadline.tcl");
    Tcl_SetVar(interp, "tcl_rcFileName", tclreadline, TCL_GLOBAL_ONLY);
    free(tclreadline);

    return TCL_OK;
}

/*
 * Ximage_Init -- main XIMAGE initialization routine.
 */
int
Ximage_Init(Tcl_Interp *interp)             /* Interpreter for application. */
{
  int i = 0; 
  int result = 0;
  Tcl_CmdInfo *infoPtr;
  char renStrg[LINE_LEN];
  cmddef *curcmd;
  
  /* code for renaming tcl/tk commands which clash with 
     ximage command abbreviations */
  
  const char tclPref[] = "tcl";
  const char tkPref[] = "tk::";
   
  char* renTclCommands[] = {"exit","read","unknown","close",NULL};
  char* renTkCommands[] = {"bell","bind","bindtags","bitmap","clipboard",
                           "cursors","destroy","event","focus","font",
                           "grab","grid","image","keysyms","loadTk","lower",
                           "menu","option","options","pack","photo","place",
                           "raise","selection","send","winfo","wm",NULL};
  
  infoPtr = (Tcl_CmdInfo *)malloc(sizeof(Tcl_CmdInfo));

  /* prefix "tcl" to the names of tcl commands that we wish to supersede */
  
  i = 0;
  
  while ( renTclCommands[i] != NULL )
  {
        strcpy(renStrg,"");
        if ( Tcl_GetCommandInfo(interp, renTclCommands[i], infoPtr) ) 
        {
                sprintf(renStrg,"rename %s  %s%s",renTclCommands[i],
                        tclPref,renTclCommands[i]);
                result = Tcl_VarEval(interp,renStrg,(char *)NULL);
                if ( result != TCL_OK) 
                {
                        sprintf(renStrg,"\nError in command rename - %s", 
                                renTclCommands[i]);
                        Tcl_AppendResult(interp,renStrg,(char *)NULL);               
                        return TCL_ERROR;
                }
        }
        i++;
  }
   
  i = 0;
  while ( renTkCommands[i] != NULL )
  {
        strcpy(renStrg,"");
        if ( Tcl_GetCommandInfo(interp, renTkCommands[i], infoPtr) ) 
        {
                sprintf(renStrg,"rename %s  %s%s",renTkCommands[i],
                        tkPref,renTkCommands[i]);
                result = Tcl_VarEval(interp,renStrg,(char *)NULL);
                if ( result != TCL_OK) 
                {
                        sprintf(renStrg,"\nError in command rename - %s", 
                                renTkCommands[i]);
                        Tcl_AppendResult(interp,renStrg,(char *)NULL);               
                        return TCL_ERROR;
                }
        }
        i++;
  }
 
  free(infoPtr);
/*
 *  Load C-based commands (w/qualifiers)
 */
  for ( i = 0; i < NCCMD; i++ ) {
     cmdname = ccmd[i];
     cmdfnc = ccmdfnc[i];
     curcmd = cmdload(cmdname);
     Tcl_CreateObjCommand(interp,cmdname,cmdfnc,(ClientData)curcmd,
                          (Tcl_CmdDeleteProc *)NULL);
  }
/*
 *  Load FORTRAN-based commands (w/qualifiers)
 */
  for ( i = 0; i < NFCMD; i++ ) {
     cmdname = fcmd[i];
     curcmd = cmdload(cmdname);
     Tcl_CreateObjCommand(interp,cmdname,procfcmd,(ClientData)curcmd,
                          (Tcl_CmdDeleteProc *)NULL);
  }
/*
 *  Direct access to routines such as PGPLOT from Tcl
 */
  load_tclimplot(interp);
/*
 *  Load other commands
 */
  Tcl_CreateObjCommand(interp,"unknown",ximunk,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"txwrite",TxwriteCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"txread",TxreadCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"parseparm",ParseParmCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp,"xsource",XsourceCmd,(ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp, "log", xm_log, (ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  Tcl_CreateObjCommand(interp, "script", xm_script, (ClientData)NULL,
                       (Tcl_CmdDeleteProc *)NULL);

  xm_echo_script = 1;  /* Echo commands executed by script if 1 */
  Tcl_LinkVar(interp, "xm_echo_script", (char *) &xm_echo_script, 
              TCL_LINK_INT);

  XIMINIT(standalone);

  return TCL_OK;
}
