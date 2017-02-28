// fv.cpp : Defines the entry point for the application.
//

#include "stdafx.h"
#include <tk.h>
#include "tkInt.h"
#include <malloc.h>
#include <locale.h>
#include <string.h>

int Tk_AppInit(Tcl_Interp *interp);
void setargv_fv (int *argcPtr, char *** argvPtr);

int APIENTRY WinMain(HINSTANCE hInstance,
                     HINSTANCE hPrevInstance,
                     LPSTR     lpCmdLine,
                     int       nCmdShow)
{

	char **argv;
	int argc;     
	char buffer[MAX_PATH+1];




	
	Tcl_Interp *interp;
	char *p;

    setlocale(LC_ALL, "C");
    setargv_fv(&argc, &argv);


	interp = Tcl_CreateInterp();

    GetModuleFileName(NULL, buffer, sizeof(buffer));
    argv[0] = buffer;
    for (p = buffer; *p != '\0'; p++) {
	if (*p == '\\') {
	    *p = '/';
	}
    }

#ifdef TK_LOCAL_MAIN_HOOK
    TK_LOCAL_MAIN_HOOK(&argc, &argv);
#endif


	// don't pass arguments to Tk, only fv needs them. 
    Tk_Main(0, argv,Tk_AppInit);

	return 0;
}


int Tk_AppInit(Tcl_Interp *interp) {
	char **argv;
	int argc;   
	char cmd[512];
	int i;
	char *p;
	int hasquote;

	if(Tcl_Init(interp) == TCL_ERROR) {
		return TCL_ERROR;
	} 

	if (Tk_Init(interp) == TCL_ERROR) { 
		return TCL_ERROR;
	}

    Tcl_StaticPackage(interp, "Tk", Tk_Init, Tk_SafeInit);
//    if {Tcl_SetVar(interp, "tcl_rcFilename", "C:/fvtest/lib/fv/fvinit.tcl",
//		TCL_GLOBAL_ONLY);
    
//  Get the fv.tcl path
	Tcl_Eval(interp, "info nameofexecutable");
	strcpy(cmd,interp->result);
	i = strlen(cmd);
	p = cmd + (i-3);
	strcpy(p,"tcl");
    for (p = cmd; *p != '\0'; p++) {
	         if (*p == '\\') {
	            *p = '/';
			 }
	}		 	
		

	
	if (Tcl_EvalFile(interp, cmd) == TCL_ERROR) {
		return TCL_ERROR;
	}

	// pass arguments with quotes to fvInit 
    setlocale(LC_ALL, "C");
    setargv_fv(&argc, &argv);
	sprintf(cmd," fvInit ");
	for (i = 1; i < argc; i++) {
		 p = argv[i];
		 if (*p == '\"') {
			  hasquote = 1;
		 } else {
			  hasquote = 0;
		 }
         for (p = argv[i]; *p != '\0'; p++) {
	         if (*p == '\\') {
	            *p = '/';
			 }
		 }		 
		 strcat(cmd, " ");
		 if (hasquote == 0) strcat (cmd,"\"");
		 strcat (cmd, argv[i]);
		 if (hasquote == 0) strcat (cmd,"\"");

	}

	if (Tcl_Eval(interp, cmd) == TCL_ERROR) {
		return TCL_ERROR;
	}	

	return TCL_OK;
}



void setargv_fv (int *argcPtr, char *** argvPtr)
{
    char *cmdLine, *p, *arg, *argSpace;
    char **argv;
    int argc, size, inquote, copy, slashes;
    
    cmdLine = GetCommandLine();	/* INTL: BUG */

    /*
     * Precompute an overly pessimistic guess at the number of arguments
     * in the command line by counting non-space spans.
     */

    size = 2;
    for (p = cmdLine; *p != '\0'; p++) {
	if ((*p == ' ') || (*p == '\t')) {	/* INTL: ISO space. */
	    size++;
	    while ((*p == ' ') || (*p == '\t')) { /* INTL: ISO space. */
		p++;
	    }
	    if (*p == '\0') {
		break;
	    }
	}
    }
    argSpace = (char *) Tcl_Alloc(
	    (unsigned) (size * sizeof(char *) + strlen(cmdLine) + 1));
    argv = (char **) argSpace;
    argSpace += size * sizeof(char *);
    size--;

    p = cmdLine;
    for (argc = 0; argc < size; argc++) {
	argv[argc] = arg = argSpace;
	while ((*p == ' ') || (*p == '\t')) {	/* INTL: ISO space. */
	    p++;
	}
	if (*p == '\0') {
	    break;
	}

	inquote = 0;
	slashes = 0;
	while (1) {
	    copy = 1;
	    while (*p == '\\') {
		slashes++;
		p++;
	    }
	    if (*p == '"') {
		if ((slashes & 1) == 0) {
		    copy = 0;
		    if ((inquote) && (p[1] == '"')) {
			p++;
			copy = 1;
		    } else {
			inquote = !inquote;
		    }
                }
                slashes >>= 1;
            }

            while (slashes) {
		*arg = '\\';
		arg++;
		slashes--;
	    }

	    if ((*p == '\0')
		    || (!inquote && ((*p == ' ') || (*p == '\t')))) { /* INTL: ISO space. */
		break;
	    }
	    if (copy != 0) {
		*arg = *p;
		arg++;
	    }
	    p++;
        }
	*arg = '\0';
	argSpace = arg + 1;
    }
    argv[argc] = NULL;

    *argcPtr = argc;
    *argvPtr = argv;
}
