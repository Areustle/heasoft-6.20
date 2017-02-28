#include <bcont.h>

void getFname(char *ss, char *data_dir)
{
   int     BufLen_2 = 50, status = 0;
   char    outFname[51], msg[50]="";

    /* get name of FITs file */
     strcpy(msg, "outFname");
     Uclgst(msg, outFname, &status);
     if (status != 0)
        {
          printf ("Problem getting name of out file!\n");
          exit (1);
          }
    
    /* make the name of data file */  
    strcpy(ss,data_dir);
    strcat(ss,"/");
    strcat(ss,outFname);
    
}
