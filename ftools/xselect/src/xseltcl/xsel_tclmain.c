#include <signal.h>
#include <tcl.h>
#ifdef _TCL
#undef VOID
#endif
#include "xselect.h" 
#include "xsel_tcl.h"

Tcl_Interp *xsl_interp;  /* global xselect tcl pointer */

int main (int argc, char *argv[]) { 
     
     signal(SIGINT, handler);
     Tcl_Main(argc,argv,Tcl_AppInit);
     exit(0);
}
 

/* The initializtion procedure */  
int Tcl_AppInit(Tcl_Interp *interp)  
{
    int mode;
    char* chan_name; 
    int status;
    FILE* fp;
    char tmp[250];
    char *p;

    if(Tcl_Init(interp) == TCL_ERROR) { 
        return TCL_ERROR;
    } 

    /* define global interp */
    xsl_interp = interp;

    /* execute the initialize tcl script (before xselect init) */
    if ( (fp = fopen("xsel_init.tcl","r")) != NULL ) { 
       close(fp);
       status = Tcl_EvalFile(xsl_interp, "xsel_init.tcl");
       if (status != TCL_OK) { 
           return TCL_ERROR;
       }
    }
    else {
       p = getenv("FTOOLS");
       strcpy(tmp,p);
       strcat(tmp,"/lib/txselect/xsel_init.tcl");
       status = Tcl_EvalFile(xsl_interp, tmp);
       if (status != TCL_OK) { 
             return TCL_ERROR;
       }
    }

    /* get the log channel */ 
    chan_name = Tcl_GetVar(xsl_interp,"xsl_log_channame",
                             TCL_LEAVE_ERR_MSG);
    if(chan_name != NULL) { 
        Xsl_Log_Chan = Tcl_GetChannel(xsl_interp, chan_name,
                 &mode); 
    } 
    else { 
        if(openlog("xselect.log")) exit(1);  
    }
    
    /* get the stdout channel */ 
    chan_name = Tcl_GetVar(xsl_interp,"xsl_std_channame",
                             TCL_LEAVE_ERR_MSG);
    if(chan_name != NULL) { 
          Xsl_Std_Chan = Tcl_GetChannel(xsl_interp, chan_name,
                  &mode);  
    } 
    else {
           Tcl_SetVar(xsl_interp,"xsl_std_channame","stdout",
                             TCL_LEAVE_ERR_MSG);
           Xsl_Std_Chan = Tcl_GetChannel(xsl_interp, "stdout",
                 &mode); 
    } 

    /* get the prompt channel */
    chan_name = Tcl_GetVar(xsl_interp,"xsl_prompt_channame",
                             TCL_LEAVE_ERR_MSG);
    if(chan_name != NULL) { 
        Xsl_Prompt_Chan = Tcl_GetChannel(xsl_interp, chan_name,
                 &mode); 
    } 
    else {
           Tcl_SetVar(xsl_interp,"xsl_prompt_channame","stdout",
                             TCL_LEAVE_ERR_MSG);
           Xsl_Prompt_Chan = Tcl_GetChannel(xsl_interp, "stdout",
                 &mode); 
    }
 
    /* get the input command  channel */
    chan_name = Tcl_GetVar(xsl_interp,"xsl_cmd_channame",
                             TCL_LEAVE_ERR_MSG);
    if(chan_name != NULL) { 
        Xsl_Cmd_Chan = Tcl_GetChannel(xsl_interp, chan_name,
                 &mode); 
    } 
    else {
           Tcl_SetVar(xsl_interp,"xsl_cmd_channame","stdin",
                             TCL_LEAVE_ERR_MSG);
           Xsl_Cmd_Chan = Tcl_GetChannel(xsl_interp, "stdin",
                 &mode); 
    }

    if ( Common_AppInit() == TCL_ERROR) { 
         return TCL_ERROR;
    }
    return TCL_OK;
}

/* The termination procedure, called by xsl_exit */  
void Xsl_tcl_end() 
{  
    int status;
    FILE* fp;
    char tmp[250];
    char *p;

    /* execute the terminatation script */
    if ( (fp = fopen("xsel_end.tcl","r")) != NULL ) { 
       close(fp);
       status = Tcl_EvalFile(xsl_interp, "xsel_end.tcl");
       if (status != TCL_OK) { 
             return ;
       }
    } 
    else {
       p = getenv("FTOOLS");
       strcpy(tmp,p);
       strcat(tmp,"/lib/txselect/xsel_end.tcl");
       status = Tcl_EvalFile(xsl_interp, tmp);
       if (status != TCL_OK) { 
             return ;
       }
    }
    return ;
}                   
