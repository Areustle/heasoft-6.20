#include <tcl.h>
#ifdef _TCL
#undef VOID
#endif
#include "xselect.h" 
#include "xsel_tcl.h" 
Tcl_Channel Xsl_Prompt_Chan = NULL;     /* tcl command prompt output channel */
Tcl_Channel Xsl_Cmd_Chan = NULL;        /* tcl command input channel */
Tcl_Channel Xsl_Std_Chan = NULL;        /* tcl message 
                                           stdout channel */

/* command prompt */
    void prompt_cmd(char *desc,char* def) 
{ 
    Tcl_SetVar(xsl_interp,"xsl_cmdprmpt",desc, TCL_GLOBAL_ONLY);
    Tcl_SetVar(xsl_interp,"xsl_defval",def, TCL_GLOBAL_ONLY);
    return;
} 

/* command input */
   int receive_ans(char *type, char *ans)
{ 
    char *p; 
    char query[20];
    strcpy(query, "query_parm ");
    strcat(query,type);
    if(Tcl_Eval(xsl_interp,query) != TCL_OK) { 
        printf("read error \n");
        return 1; 
    }
    p = Tcl_GetVar(xsl_interp,"xsl_parval_cancel",TCL_GLOBAL_ONLY);
    if (!strcmp(p,"cancel")) { 
        return 1;
    }
    p = Tcl_GetVar(xsl_interp,"xsl_parval",TCL_GLOBAL_ONLY);
    strcpy(ans,p);
    return 0;
} 

/* log to terminal */
   void log_terminal(char *mess)
{
    char tmp[1024];
    int slen;
    char *p;
    slen = strlen(mess); 
    p = mess;
    if(mess[slen-1] != '\n') { 
        strcpy(tmp,mess);  
        tmp[slen] = '\n';
        tmp[slen+1] = '\0'; 
        p = tmp;
    }  
    Tcl_SetVar(xsl_interp,"log_mess",p,
       TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG|TCL_APPEND_VALUE);  
/*    if(Tcl_Eval(xsl_interp,"xsltermout",NULL)!=TCL_OK) { 
        printf("log error \n"); 
    } */
    return;
}

