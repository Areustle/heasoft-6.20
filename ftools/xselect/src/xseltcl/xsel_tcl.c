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
    int n; 
    strcat(desc,"[");
    strcat(desc,def);
    strcat(desc,"]");
    if(Xsl_Prompt_Chan != NULL) { 
        n = Tcl_Write(Xsl_Prompt_Chan,desc,-1);
        Tcl_Flush(Xsl_Prompt_Chan);
    }
    else { 
        fprintf(stdout,"%s",desc); 
    }
    return;
} 

/* command input */
   int receive_ans(char *type, char *ans)
{ 
    Tcl_DString parval;

    if(Xsl_Cmd_Chan != NULL) { 
        Tcl_DStringInit(&parval);
        Tcl_Gets(Xsl_Cmd_Chan, &parval);
        strcpy(ans, parval.string); 
        Tcl_DStringFree(&parval);
    }
    else {      
        fgets(ans,MAXPVALUE,stdin); 
    }
    return 0;
} 

/* log to terminal */
   void log_terminal(char *mess)
{
    char tmp[1024];
    int slen;
    int n; 
    char *p;
    slen = strlen(mess); 
    if(Xsl_Std_Chan == NULL) return;
    p = mess;
    if(mess[slen-1] != '\n') { 
        strcpy(tmp,mess);  
        tmp[slen] = '\n';
        tmp[slen+1] = '\0'; 
        p = tmp;
    }  
    n = Tcl_Write(Xsl_Std_Chan,p,-1);
    return;
}

