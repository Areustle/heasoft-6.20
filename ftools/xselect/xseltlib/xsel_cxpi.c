#include "xselect.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>


int logchat = 10;
int termchat = 6;

/* routine for reading the available commands */
int xsel_read_cmd(char** xselcmds, int *ncmds,int *status) 
{ 
    FILE *fp; 
    char line[80];
    int i,j;
    char *p;
    char cmdfile[128];
    char errmes[80];

    *status = 0;
    
    /* open the .cmd file */ 
    p = getenv("FTOOLS"); 
    if(p == NULL) { 
        c_fcerr("The Enviroment variable of FTOOLS is not defined. ");
        *status = 1;
        return 1; 
    } 
    strcpy(cmdfile, p); 
    strcat(cmdfile,"/bin/txselect.cmd");
    fp = fopen(cmdfile,"r");
    if(fp == NULL) { 
        sprintf(errmes,"Can not find the file: %s. ",cmdfile);
        c_fcerr(errmes);
        *status = 1;
        return 1;
    } 
    
    /* read in the commands */
    *ncmds = 0;
    while (fgets(line,80,fp)!= NULL && *ncmds < MAXNCMDS ) { 
        p = line;
        i = 0;
        while(*p != ' ' && *p != '\t' && *p != '\0' && i < 15) { 
            if(*p != '*' ) {  /* take out the star */
                *(xselcmds[(*ncmds)]+i) = *p; 
                i++;
            } 
            p++;
        }
        xselcmds[*ncmds][i] = '\0';
        j = i-1;
        while( (xselcmds[*ncmds][j] == ' ' ||
                xselcmds[*ncmds][j] == '@' ||
                xselcmds[*ncmds][j] == '\0') &&
                j > 0 ) {
           xselcmds[*ncmds][j] = '\0';
           j--;
        }
        (*ncmds)++;
    }  
    fclose(fp);
    return 0; 
}        
   
/* get the parameter value from the command line */
int cget_val(char* desc, char *type, char* parval)

{
    int i;
    int n; 
    char *p;
    char temp[128];
    char tmpval[MAXPVALUE]; 
    char *q; 
    int status = 0;

    i = 79;
    n = strlen(desc);
    if(n <= i) i = n;
    while(desc[i] == ' '&& i >= 0 ){desc[i]='\0';i--;}
    i = 199;
    n = strlen(parval);
    if(n<=i) i = n; 
    while(parval[i] == ' '&& i >= 0 ){parval[i]='\0';i--;}

    prompt_cmd(desc,parval);
    if(receive_ans(type,tmpval)) return 1;
    i = strlen(tmpval)-1; 
    while(isspace(tmpval[i]) && i >=0) { 
         tmpval[i] = '\0'; i--;
    }
    if(strlen(tmpval)==0) { 
        writelog(desc,-1);
        return 0;    /* take the default value */      
    }
    q = tmpval;
    while((p = strchr(q,'$'))!= NULL) { 
	if(p == tmpval) { 
            expand_pval(q, &status); /* substitute the variable */ 
            continue;
        } 
	p--;
	if(*p == '\\'){ 	/* check escape */
	   while(*p != '\0') { 
	       *p = *(p+1);
	       p++;
           } 
           q = p+1; 
        }
        else { 
            q = p + 1;
            expand_pval(q, &status); /* substitute the variable */
        }
    }
    strcpy(parval, tmpval);
    strcpy(temp,desc);
    strcat(temp,parval);
    writelog(temp,-1);
    return 0;
}     
FCALLSCFUN3(INT,cget_val,GET_VAL,get_val,STRING,STRING,PSTRING);
    

void writelog(char *str, int chatty) 
{ 
     if(str == NULL) return;
     if(chatty < -1.0*logchat && chatty < -1.0*termchat) return;
     
     if(chatty > 0 && chatty <= termchat)
        log_terminal(str);

     if(abs(chatty) <= logchat) 
	 log_file(str); 
     return;
}
FCALLSCSUB2(writelog, XSELWRITE,xselwrite,STRING,INT);

void copylog(char *infile, int toterminal)
{ 
     /* toterminal = 1 : controled by chatter parameter 
        toterminal = 0 : never write on the terminal */
     FILE *tmpfile; 
     char line[81];
     char *p; 
 
     tmpfile = fopen(infile,"r"); 
     if(tmpfile == NULL) { 
         fprintf(stderr, "%s does not exist \n",infile);
         return; 
     }
     while(fgets(line,81,tmpfile)!= NULL) { 
        if((p = strchr(line,'\n')) !=NULL) *p = '\0';
        if(toterminal) 
            writelog(line, 5); 
        else 
            writelog(line,-2);
     }
     fclose(tmpfile);
     return;
} 
     
     
void setterm(int termset)
{ 
     termchat = termset;
     return;
}   
FCALLSCSUB1(setterm, XSELSETTERM,xselsetterm,INT);

int toterm() 
{ 
    if(termchat < 5) return 0; 
    return 1; 
} 
int  qinter()    
{ 
   extern int xsl_interrupt;
   if(xsl_interrupt) { 
       xsl_interrupt = 0;   
       return -1; 
   } 
   else 
       return 0; 
} 
FCALLSCFUN0(INT,qinter,XSELQINTER,xselqinter);


/* to send the logical to tcl/tk */
void cset_xsel_ctrl(char* varname, int val) { 
   char strval[20];
   if(val !=0) val = 1;
   sprintf(strval,"%d",val);
   set_xsel_tarray("XSLCTRLFLAG", varname, strval);
   return;
}
FCALLSCSUB2(cset_xsel_ctrl, SET_XSEL_CTRL,set_xsel_ctrl,STRING,LOGICAL);



/*  Ning Gan:   April, 1999:
    Took out from xsel_unix_c.c and modified for tcl-tk input/output. 
*/

/********************** xsl_runcc_ ********************/



#if defined sun && ! defined __STDC__
void xsl_runcc(comfil,status)
int *status;
char *comfil;
#else
void xsl_runcc(char *comfil,int *status)
#endif

{


  mode_t mode;
  FILE *tmpfile; 
  char line[1024];
  char *tmpout = "xselcmd.tmp";
 
  mode = S_IRWXU | S_IRGRP | S_IROTH;
  

  /* status carries the echo flag */
  /* copy the comfil to the log, the echo will control whether write them to
     terminal and log file */
  if(*status ) copylog(comfil,1);   
  
  strcpy(line,comfil);
  if(ftout_buffer)
     strcat(line," |tee xselcmd.tmp >/dev/null");
  else
     strcat(line," | tee xselcmd.tmp ");

  /* if the tmpout file exists, remove it! */
  if((tmpfile = fopen(tmpout,"r"))!= NULL) { 
      fclose(tmpfile);  
      remove(tmpout);
  } 

  /* execute the command file */
  chmod(comfil,mode);
  *status = system(line);
 
  /* if the command generate some outputs..., copy them to the 
     log file */
  if((tmpfile = fopen(tmpout,"r"))!= NULL) { 
      fclose(tmpfile);
      /* copy the command log file only and delete it */
      copylog(tmpout, ftout_buffer);
      /* copylog(tmpout,0); */
  }
  
  return;
}

FCALLSCSUB2(xsl_runcc,XSL_RUNCC,xsl_runcc,STRING,PINT);


