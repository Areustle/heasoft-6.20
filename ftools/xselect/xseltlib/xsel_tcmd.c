#include <tcl.h>
#ifdef _TCL
#undef VOID
#endif
#include "xselect.h" 
#include "xsel_tcl.h"


/*  Decalrations for the xselect command parser */ 
int XSLCmd(ClientData clientdata, Tcl_Interp *interp, int argc,
                char *argv[])  
{ 
    char cmd[MAXCMD];       /* command */
    char ucmd[MAXCMD];
    char option[MAXPVALUE];
    char temp[256];
    
    int i; 
    long ipos;  
 
    strcpy(cmd,argv[0]);
    cmd[16] = '\0';
    ucmd[16] = '\0';
    option[MAXPVALUE-1] = '\0';

    
    /* write the command string to the log file */ 
    strcpy(temp,xsl_pfix);
    for (i = 0; i< argc; i++) { 
	strcat(temp," "); 
	strcat(temp,argv[i]);
    } 
    writelog(temp, -2);

    /* convert the command  to upper case if it is applied */
    for (i=0; i < strlen(cmd); i++)
       ucmd[i] = toupper(cmd[i]);  
    ucmd[strlen(cmd)] = '\0';

    /* load the position  parameters */ 
    LdPosPar(ucmd);

    /* parse and load the command line options */
    ipos = 1;
    for (i = 1; i < argc; i++) { 
         LdCmdPar(ipos,argv[i]); 
         ipos++;
    } 
 
    /* execute the command */
    XselCmd(ucmd,argc-1);

    /* reset the command string */
    memset(cmd,' ',16);
    memset(ucmd,' ',16);
    
    return TCL_OK;
} 

/* hadling the log file */
int XSLXlog(ClientData clientdata, Tcl_Interp *interp, int argc,
                char *argv[])  
{  
    char tmp[512];
    if(argc == 1){ 
        xselopenlog("xselect.log");
        return TCL_OK; 
    }  
    if(!strcmp(argv[1],"none") || !strcmp(argv[1],"close")||  
       !strcmp(argv[1],"off")) { 
        xselcloselog();
        return TCL_OK; 
    }
    strcpy(tmp,argv[1]);
    strcat(tmp,".log");
    xselopenlog(tmp);
    return TCL_OK;
}
   
/* list the parameters */
int XSLLparm(ClientData clientdata, Tcl_Interp *interp, int argc,
                char *argv[])  
{ 
    char cmd[MAXCMD];
    if(argc == 1) { 
        writelog("Usage: lparm <cmd>",5);     
        return TCL_OK;
    }
    strcpy(cmd,argv[1]);
    XselLparm(cmd);
    return TCL_OK;
} 

/* list the commands */
int XSLLcmd(ClientData clientdata, Tcl_Interp *interp, int argc,
                char *argv[])  
{ 
    char buf[1024];
    int i;
    int status = 0;
    char** xselcmd;
    int nxselcmd = 0;

    buf[0]='\0';
    for (i=0; i<nothercmd; i++) { 
        strcat(buf,othercmd[i]); 
        strcat(buf,",");
    } 
 
    /* read in the commands */ 
    xselcmd = (char **)malloc(MAXNCMDS*sizeof(char *)); 
    for (i = 0; i < MAXNCMDS; i++)
        xselcmd[i] = (char *)malloc(17 * sizeof(char));
    xsel_read_cmd(xselcmd,&nxselcmd,&status);

    for (i=0; i<nxselcmd; i++) { 
        if(strchr(xselcmd[i],'*') != NULL) continue; 
        strcat(buf,xselcmd[i]); 
        strcat(buf,",");
        free(xselcmd[i]);
    } 
    free(xselcmd);  
    buf[strlen(buf)-1] = '\0';  /* take out the last , */ 
    writelog(buf,5);
    return TCL_OK;
} 

int XSLScript(ClientData clientdata, Tcl_Interp *interp, int argc,
                 char *argv[])  
{ 
    int code;
    if(argc == 1) { 
        writelog("Usage: script <script file>",5);     
        return TCL_OK;
    }
    code = Tcl_EvalFile(interp, argv[1]);
    if(code == TCL_OK) writelog(interp->result,5);
    if(code == TCL_ERROR) writelog(interp->result,5);
    return code;
} 

int XSLTerm(ClientData clientdata, Tcl_Interp *interp, int argc,
                 char *argv[])  
{ 
    if(argc == 1) { 
        setterm(6); 	/* turn on the terminal */
        return TCL_OK;
    }
    if(!strcmp(argv[1],"off")) setterm(3);  
    if(!strcmp(argv[1],"on")) setterm(6);  
        return TCL_OK;
} 

