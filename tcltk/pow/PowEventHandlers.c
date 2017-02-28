#include "pow.h"


/*
 * Forward declarations for procedures defined later in this file:
 */

static void		Prompt _ANSI_ARGS_((Tcl_Interp *interp, int partial));
static void		StdinProc _ANSI_ARGS_((ClientData clientData,
			    int mask));
static Tcl_DString command;



void PowHandleEvents() {
  Pow_Done = 0;
  while (!(Pow_Done)) {
    Tk_DoOneEvent(0);
  }
}

void PowWishHandleEvents() {
  Pow_Done = 0;
  fflush(stdout);

  Tcl_DStringInit(&command);
  Tk_CreateFileHandler(0,TK_READABLE,StdinProc, (ClientData) NULL);
  if (tty) {
    Prompt(interp, 0);
  } 
   
  


  while (!(Pow_Done)) {
    Tk_DoOneEvent(0);
  }

  Tk_DeleteFileHandler(0);

  Tcl_DStringFree(&command);
}

/*Below here.... Nicked from tkMain.c with slight modifications*/



/*
 *----------------------------------------------------------------------
 *
 * StdinProc --
 *
 *	This procedure is invoked by the event dispatcher whenever
 *	standard input becomes readable.  It grabs the next line of
 *	input characters, adds them to a command being assembled, and
 *	executes the command if it's complete. It refuses to execute
 *      exit or quit.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Could be almost arbitrary, depending on the command that's
 *	typed.
 *
 *----------------------------------------------------------------------
 */

    /* ARGSUSED */
static void
StdinProc(clientData, mask)
    ClientData clientData;		/* Not used. */
    int mask;				/* Not used. */
{
#define BUFFER_SIZE 4000
    char input[BUFFER_SIZE+1];
    static int gotPartial = 0;
    char *cmd;
    int code, count;

    count = read(fileno(stdin), input, BUFFER_SIZE);
    if (count <= 0) {
	if (!gotPartial) {
	    if (tty) {
		Tcl_Eval(interp, "exit");
		exit(1);
	    } else {
		Tk_DeleteFileHandler(0);
	    }
	    return;
	} else {
	    count = 0;
	}
    }
    cmd = Tcl_DStringAppend(&command, input, count);
    if (count != 0) {
	if ((input[count-1] != '\n') && (input[count-1] != ';')) {
	    gotPartial = 1;
	    goto prompt;
	}
	if (!Tcl_CommandComplete(cmd)) {
	    gotPartial = 1;
	    goto prompt;
	}
    }
    gotPartial = 0;

    /*
     * Disable the stdin file handler while evaluating the command;
     * otherwise if the command re-enters the event loop we might
     * process commands from stdin before the current command is
     * finished.  Among other things, this will trash the text of the
     * command being evaluated.
     */

    Tk_CreateFileHandler(0, 0, StdinProc, (ClientData) 0);
    /*    if (memcmp(cmd,"quit",4) || (memcmp(cmd,"exit",4))) { */
    code = Tcl_RecordAndEval(interp, cmd, TCL_EVAL_GLOBAL);
    /* } else { 
       printf("Please use the GUI to return control to main program\n");
       } */
    Tk_CreateFileHandler(0, TK_READABLE, StdinProc, (ClientData) 0);
    Tcl_DStringFree(&command);
    if (Tcl_GetStringResult(interp) != 0) {
      if ((code != TCL_OK) || (tty)) {
	    /*
	     * The statement below used to call "printf", but that resulted
	     * in core dumps under Solaris 2.3 if the result was very long.
	     */

	    puts(Tcl_GetStringResult(interp));
	}
    }

    /*
     * Output a prompt.
     */

    prompt:
    if (tty) {
	Prompt(interp, gotPartial);
    }
    Tcl_ResetResult(interp);
}


/*
 *----------------------------------------------------------------------
 *
 * Prompt --
 *
 *	Issue a prompt on standard output, or invoke a script
 *	to issue the prompt.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	A prompt gets output, and a Tcl script may be evaluated
 *	in interp.
 *
 *----------------------------------------------------------------------
 */

static void
Prompt(interp, partial)
    Tcl_Interp *interp;			/* Interpreter to use for prompting. */
    int partial;			/* Non-zero means there already
					 * exists a partial command, so use
					 * the secondary prompt. */
{
    const char *promptCmd;
    int code;

    promptCmd = Tcl_GetVar(interp,
	partial ? "tcl_prompt2" : "tcl_prompt1", TCL_GLOBAL_ONLY);
    if (promptCmd == NULL) {
	defaultPrompt:
	if (!partial) {
	    fputs("% ", stdout);
	}
    } else {
	code = Tcl_Eval(interp, promptCmd);
	if (code != TCL_OK) {
	    Tcl_AddErrorInfo(interp,
		    "\n    (script that generates prompt)");
	    fprintf(stderr, "%s\n", Tcl_GetStringResult(interp));
	    goto defaultPrompt;
	}
    }
    fflush(stdout);
}
