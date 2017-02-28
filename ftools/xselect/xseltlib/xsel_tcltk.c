#include <signal.h>
#include <tcl.h>
#ifdef _TCL
#undef VOID
#endif
#include "xselect.h" 
#include "xsel_tcl.h"

char xsl_pfix[256]; /* prefix of the prompt */
char *othercmd[] = {"lparm","xlog","script","lcmd","term"};
int nothercmd = 5;
int xsl_interrupt = 0;
int ftout_buffer = 0; 

/* The common initializtion procedure */  
int Common_AppInit()  
{
    int i,j; 
    char prompt[255]; 
    int lpfix = 0; 
    int status = 0; 
    char** xselcmd;
    int nxselcmd = 0;
    char* tclvar; 
    char* tclvar2; 
    char* tclvar3;
    int ifv;

    char snull[] = " ";


    if((tclvar = Tcl_GetVar(xsl_interp,"ftool_out_buffer",
                             TCL_LEAVE_ERR_MSG)) == NULL)  { 
         ftout_buffer = 0; 
    }
    else 
         sscanf(tclvar,"%d",&ftout_buffer);

    setterm(6);			       /* set the term output */

    if((tclvar = Tcl_GetVar(xsl_interp,"fplot_window",
                             TCL_LEAVE_ERR_MSG)) == NULL)  { 
        tclvar = snull;
    }
    if((tclvar2 = Tcl_GetVar(xsl_interp,"fplot_isbkg",
                             TCL_LEAVE_ERR_MSG)) == NULL)  { 
        tclvar2 = snull;
    }
    if((tclvar3 = Tcl_GetVar(xsl_interp,"using_fv",
                             TCL_LEAVE_ERR_MSG)) == NULL)  { 
        tclvar3 = snull;
    }
    ifv = 0;
    if( ! strncmp(tclvar3,"yes",3) )  ifv = 1 ;
    XsetFlotWin(tclvar,tclvar2,ifv);

    setterm(6);			       /* set the term output */
    /* initilize the xselect */
    XselInit(xsl_pfix,lpfix);
    xsl_pfix[lpfix] = '\0';
    

    /* set the prompt */
    sprintf (prompt,"set tcl_prompt1 \{ puts -nonewline \"%s\" \}",xsl_pfix);
    Tcl_Eval(xsl_interp,prompt); 
    Tcl_SetVar(xsl_interp,"xsl_prompt",xsl_pfix, TCL_GLOBAL_ONLY);

    /* read in the commands */ 
    xselcmd = (char **)malloc(MAXNCMDS*sizeof(char *)); 
    for (i = 0; i < MAXNCMDS; i++)
        xselcmd[i] = (char *)malloc(17 * sizeof(char));

    xsel_read_cmd(xselcmd,&nxselcmd,&status);
    if(status) {
        fprintf(stderr,"Error in read in the available xselect commands \n");
        return TCL_OK;
    }
 
    /* register the command */
    for (i = 0, j = 0; i < nxselcmd; i++) { 
        if(strchr(xselcmd[i],'*') != NULL) continue; 
        Tcl_CreateCommand(xsl_interp,xselcmd[i],XSLCmd, (ClientData)NULL, 
                         (Tcl_CmdDeleteProc *)NULL ); 
        j++;
        free(xselcmd[i]);
    }  
    free(xselcmd);
    /* other commands */  
        Tcl_CreateCommand(xsl_interp,"xlog",XSLXlog, (ClientData)NULL, 
                         (Tcl_CmdDeleteProc *)NULL ); 
        Tcl_CreateCommand(xsl_interp,"lparm",XSLLparm, (ClientData)NULL, 
                         (Tcl_CmdDeleteProc *)NULL ); 
        Tcl_CreateCommand(xsl_interp,"lcmd",XSLLcmd, (ClientData)NULL, 
                         (Tcl_CmdDeleteProc *)NULL ); 
        Tcl_CreateCommand(xsl_interp,"script",XSLScript, (ClientData)NULL, 
                         (Tcl_CmdDeleteProc *)NULL ); 
        Tcl_CreateCommand(xsl_interp,"term",XSLTerm, (ClientData)NULL, 
                         (Tcl_CmdDeleteProc *)NULL ); 
    return TCL_OK;
}

/* interrupt handler */
void handler(sig) 
{
     xsl_interrupt = 1;
     return ;
} 

