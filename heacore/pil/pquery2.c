#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <float.h> /* for DBL_DIG */
#include "pil.h"
/* SCREW 1145: For Windows port, building without readline must be supported. */
#ifdef HAVE_READLINE_LIBS
#include "readline/readline.h"
#endif

int main(int argc, char *argv[]){
  int ierr, parcnt, j, minmaxflag, partype, parfound=0;
  char *newargv[999], *reqparname, parname[99], buf[PIL_LINESIZE];
  PIL_VALUE vmin, vmax;
  PIL_PARAM pp;

  int pilparget(char *, int);
  int pq2_err_logger(char *);

  /* 
  *  We want all prompts to go to terminal since pquery2
  *  is typically called via backticks from Perl scripts
  */
/* SCREW 1145: For Windows port, building without readline must be supported. */
#ifdef HAVE_READLINE_LIBS
  rl_outstream = fopen("/dev/tty","w");
#endif
  
  if (argc < 3){
      fprintf(stderr,"Usage: %s tool_name parameter_name [par1=value par2=value ...]\n",argv[0]);
      exit(1);
  }

  newargv[0] = argv[1];
  for (j=1;j<(argc-2);j++)
      newargv[j] = argv[j+2];
  newargv[argc-2] = NULL;
  
  reqparname = argv[2];

#ifdef DEBUG
  printf("PILInit arg 1 is %d\n",argc-2);
  for (j=0;j<(argc-1);j++)
      printf("newargv[%d] is %s\n",j,newargv[j]);
#endif
  
  if (PIL_OK != (ierr = PILInit(argc-2, newargv))){
      printf("PILInit failed : %s\n", PIL_err_handler(ierr));
      exit(1);
  }
  PILSetLoggerFunction(pq2_err_logger);

/* enable 'batch mode' via HEADASNOQUERY environment variable */
  if (getenv("HEADASNOQUERY")) PILOverrideQueryMode(PIL_QUERY_OVERRIDE);

  if (PIL_OK != (ierr = PILGetNumParameters(&parcnt))){
      PIL_log_error("PILGetNumParameters failed", ierr);
      exit(1);
  }

  for (j=0;j<parcnt;j++){
      if (PIL_OK != (ierr = PILGetParameter(j, &pp, &minmaxflag, &vmin, &vmax))){
	  PIL_log_error("PILGetParameter failed", ierr);
	  exit(1);
      }
      if (PIL_FORMAT_OK != pp.format) continue;   /* comment, empty or bad format */

      if (!strcmp(pp.strname, reqparname)) {
	  parfound=1;
	  strcpy(parname, pp.strname);
	  partype = pp.type;
	  if (PIL_OK != (ierr = pilparget(parname, partype))) exit(ierr);
	  break;
      }
  }
    
  if (!parfound){
      sprintf(buf, "Parameter %s", reqparname);
      PIL_log_error(buf, PIL_NOT_FOUND);
      exit(1);
  }
  PILClose(PIL_OK);
  return(PIL_OK);
}

int pilparget(char *name, int type){
    int ierr, intval;
    double realval;
    char strval[PIL_LINESIZE-1];
    char buf[PIL_LINESIZE];

    switch (type){
    case PIL_TYPE_BOOL:
	if (PIL_OK != (ierr = PILGetBool(name, &intval))){
	    sprintf(buf, "PILGetBool error for %s", name);
	    PIL_log_error(buf, ierr);
	    return(ierr);
	}
	printf("%s\n", intval==0 ? "no" : "yes");
	break;
    case PIL_TYPE_INT4: 
	if (PIL_OK != (ierr = PILGetInt(name, &intval))){
	    sprintf(buf, "PILGetInt error for %s", name);
	    PIL_log_error(buf, ierr);
	    return(ierr);
	}
	printf("%d\n",intval);
	break;
    case PIL_TYPE_REAL8: 
	if (PIL_OK != (ierr = PILGetReal(name, &realval))){
	    sprintf(buf, "PILGetReal error for %s\n", name);
	    PIL_log_error(buf, ierr);
	    return(ierr);
	}
	/* use maximum sensible precision */
	printf("%.*g\n", DBL_DIG, realval);
	break;
    case PIL_TYPE_STRING: 
	if (PIL_OK != (ierr = PILGetString(name, strval))){
	    sprintf(buf, "PILGetString error for %s", name);
	    PIL_log_error(buf, ierr);
	    return(ierr);
	}
	PIL_trim_spaces(strval);
	printf("%s\n",strval);
	break;
    case PIL_TYPE_FNAME: 
	if (PIL_OK != (ierr = PILGetFname(name, strval))){
	    sprintf(buf, "PILGetFname error for %s",name);
	    PIL_log_error(buf, ierr);
	    return(ierr);
	}
	printf("%s\n",strval);
	break;
    default:
	fprintf(stderr,"%s: UNKNOWN TYPE\n", name);
	return(1);
	break;
    }
  
    return(PIL_OK);
}

int pq2_err_logger(char *s)
{
    fprintf(stderr, "\n%s\n", s ? s : "NULLstring");
    return(0);
}
