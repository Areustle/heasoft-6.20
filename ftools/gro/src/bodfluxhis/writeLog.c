/*       ----------------------------------------------
       filename: writeLog.c
       purpose:  save flux array in a ascii data file
       author/date: C. Pan, Feb., 2002
       ---------------------------------------------- */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"
#include <sys/types.h>

void  writeLog(char *fitsFname, char *outfil_dir)
{
   FILE  *fp; 
  int  status = 0, BufLen_2 = 20,dirLen=0 , count,i, slen;
  char  s[100], viewlog[21], string[]="mkdir ./";
  struct direct **files;
  char  logFname[100], msg[100]="";

  for (i =0; i<100;i++)logFname[i]='\0';
 
    /* get name of log director */
     strcpy(msg,"outfil_dir");
     Uclgst(msg, outfil_dir, &status);
     if (status != 0) 
	{
          printf ("Problem getting name of outfile director!");
          exit (1);
	  }  
  
  dirLen = strlen(outfil_dir); 
  /* make new directory to hold output file */

  strcat(string, outfil_dir);
  system(string);
  strcat(logFname,outfil_dir );   
  strcat(logFname,"/flux.log");
 
  /*count = scandir(outfil_dir, &files, file_select, alphasort);*/
    if ((fp = fopen (logFname,"r+")) == NULL)
     { if ((fp = fopen (logFname,"w+")) == NULL)
         {
           printf("cannot creat log files\n");
           exit (1);
         }
     }
  
   else
  {          
  if ((fp = fopen (logFname,"r+")) == NULL)
     {
      printf("cannot open log files\n");
     exit (1);
     }
  }
   status = 0;
   strcpy(msg,"viewlog");
   Uclgst(msg, viewlog, &status);
     if (status != 0) 
	{
          printf("Problem getting variable viewlog!");
          exit (1);
	  }
      
     for (i = 0; i<100; i++) s[i]='\0';
    /* check log file */
    printf("Checking file name in log file\n");
    while (1)
    {
     fgets(s,100,fp);
     s[strlen(s)-1]='\0';
     if (strncmp(viewlog,"yes",3) == 0) printf("%s\n", s);
     slen = strlen(fitsFname);
     if ( strncmp(s,fitsFname,slen) == 0) 
     {
      printf(" %s is already in log file, no writting\n",fitsFname);
      break;
     }
     else if (feof(fp)) 
     {
      fseek(fp, 0L, SEEK_END);
      printf(" writting %s in log file\n", fitsFname);
      fprintf(fp,"%s \n", fitsFname); 
      break;
     }
    }
   fclose(fp);

}

