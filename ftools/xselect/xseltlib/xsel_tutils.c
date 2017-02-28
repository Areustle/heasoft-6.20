#include <tcl.h>
#ifdef _TCL
#undef VOID
#endif
#include "xselect.h" 
#include "xsel_tcl.h" 

static char logfile[80];
Tcl_Channel Xsl_Log_Chan = NULL;        /* tcl message log out
                                           channel */

/* use the tcl interpreter to do the variable substitution for the 
   xpi parameters */
int  expand_pval(char *pval, int *status)
{
    char *p;
    char *pi;
    char *pf;
    char temp[200];
    char varnam[64];
    int n;

    p = pval + 1; 
    if(*p == '{') p++;
    pi = p;
    while((isalnum(*p)|| *p =='_') && *p!='\0')p++;
    pf = p;
    n = pf - pi;
    strncpy(varnam,pi,n);
    if(n> 64)
       varnam[63] = '\0';
    else
       varnam[n] = '\0';
    temp[0]= '\0';
    p = Tcl_GetVar(xsl_interp,varnam,TCL_LEAVE_ERR_MSG); 
    if(p == NULL) {  
         *status = TCL_ERROR;
         return TCL_ERROR;
    }
    strcat(temp,p);
    if(*pf == '}') pf++;
    strcat(temp,pf);
    strcpy(pval,temp);
    status = TCL_OK;
    return TCL_OK;
} 

/* routines to deal with the log files */
int xselopenlog(char *filename)
{
     char mess[80];
     if(Xsl_Log_Chan !=NULL) Tcl_Close(xsl_interp,Xsl_Log_Chan);
     Xsl_Log_Chan =
     Tcl_OpenFileChannel(xsl_interp,filename,"w",0666);
     if(Xsl_Log_Chan == NULL) {
         fprintf(stderr,"can not open the log file %s.\n",filename);
        return 1;
     }
     strcpy(logfile,filename);
     sprintf(mess,"The log file is set to be %s.",logfile);
     writelog(mess,5);
     return 0;
}

void xselcloselog()
{
     if(Xsl_Log_Chan ==NULL) return;
     Tcl_Close(xsl_interp,Xsl_Log_Chan);
     logfile[0] = '\0';
     return;
}

   void log_file(char *mess)
{
    char tmp[512];
    int slen;
    int n;
    char *p;
    slen = strlen(mess);
    if(Xsl_Log_Chan == NULL) return;
    p = mess;
    if(mess[slen-1] != '\n') {
        strcpy(tmp,mess);
        tmp[slen] = '\n';
        tmp[slen+1] = '\0';
        p = tmp;
    }
    n = Tcl_Write(Xsl_Log_Chan,p,-1);
    Tcl_Flush(Xsl_Log_Chan);
    return;
}


/* pass an tcl array element */
   void set_xsel_tarray(char *array_name, char *index, char *val) 
{ 
    Tcl_SetVar2(xsl_interp,array_name, index ,val,
                    TCL_GLOBAL_ONLY);
   return;
}

