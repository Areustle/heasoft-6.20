/* unlearn a par file */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "pil.h"

#ifdef WIN32
static const char sPfilesDelim = '|';
#else
static const char sPfilesDelim = ';';
#endif

int main (int argc,char *argv[]){
  int ierr, j;
  char pfname[PIL_LINESIZE-1], filename[PIL_LINESIZE-1];
  char pfiles[PIL_LINESIZE-1];
  char *parpath;
  
  int pun_err_logger(char *);

  if (argc < 2){
      fprintf(stderr,"No parameter files to unlearn\n");
      exit(1);
  }
  
  if (NULL == getenv("PFCLOBBER")){
      fprintf(stderr,"PFCLOBBER not set; punlearn blocked\n");  
      exit(1);
  }

  if (NULL == getenv("PFILES")){
      fprintf(stderr,"PFILES not set; punlearn inappropriate\n");
      exit(1);
  }
  strcpy(pfiles,getenv("PFILES"));
  if (NULL == strchr(pfiles, sPfilesDelim)){
      fprintf(stderr,"PFILES is single-element (\"%s\"); punlearn inappropriate\n",pfiles);
      exit(1);
  }

  PILSpecialMode |= PIL_NO_POSITIONAL | PIL_OPEN_RDONLY;

  for (j=1;j<argc;j++){
      strcpy(pfname,argv[j]);
      if (NULL == strstr(pfname,".par")) strcat(pfname,".par");
#ifdef DEBUG
      fprintf(stderr,"Loading %s\n",pfname);
      fflush(stderr);
#endif

/* need to set module name since the relevant file is NOT punlearn.par! */
      if (PIL_OK != (ierr = PILSetModuleName(pfname))){
	  fprintf(stderr,"PILSetModuleName failed: %s\n", PIL_err_handler(ierr));
	  exit(1);
      }

      if (PIL_OK != (ierr = PILInit(1, argv))){
	  fprintf(stderr,"PILInit failed : %s\n", PIL_err_handler(ierr));
	  exit(1);
      }
      PILSetLoggerFunction(pun_err_logger);
    
      if (PIL_OK != (ierr = PILGetParFilename(&parpath))){
	  PIL_log_error("PILGetParFilename failed", ierr);
	  exit(1);
      } 
      strcpy(filename,parpath);
    
      PILClose(PIL_OK);

/* Nuke the par file */
      if (remove(filename))
	  fprintf (stderr,"Couldn't remove %s\n",filename);
    /*if (NULL != (infile = fopen(filename,"w"))){
      fprintf(infile," ");
      fclose(infile);
      } else fprintf (stderr,"Couldn't open %s\n",filename);*/
    
      if (PIL_OK != (ierr = PILSetModuleName(pfname))){
	  PIL_log_error("PILSetModuleName failed", ierr);
	  exit(1);
      }
      if (PIL_OK != (ierr = PILInit(1, argv))){
	  PIL_log_error("PILInit(2) failed", ierr);
	  exit(1);
      }
    
      PILClose(PIL_OK);
  }
  
  exit(0);
}

int pun_err_logger(char *s)
{
    fprintf(stderr, "\n%s\n", s ? s : "NULLstring");
    return(0);
}
