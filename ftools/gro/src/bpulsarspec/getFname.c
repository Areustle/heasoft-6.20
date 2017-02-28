#include <bpulsarspec.h>

void getFname(char *ss, char *data_dir)
{
   int     BufLen_2 = 50, status = 0;
   char    FitsFname[51], msg[50]="";

    /* get name of FITs file */
     strcpy(msg, "FitsFname");
     Uclgst(msg, FitsFname, &status);
     if (status != 0)
        {
          printf ("Problem getting name of fits file to be created!\n");
          exit (1);
          }
    
    /* make the name of data file */      
    strcat(ss,data_dir);
    strcat(ss,"/");
    strcat(ss,FitsFname);
  
}
