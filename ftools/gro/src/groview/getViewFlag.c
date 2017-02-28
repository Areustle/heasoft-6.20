#include "groview.h"
void getViewFlag(char *show_OSSE,char *show_EGRET,char *show_COMPTEL,
     char *show_BATSE)
{
  int status = 0, BufLen_2 = 15;
  char  msg[15]="";

 step1:
   strcpy(msg,"show_OSSE");
        Uclgst(msg, show_OSSE, &status);
        if (status != 0)
          {
           printf ("Problem getting show_OSSE!\n");
           exit (1);
          }

   if (strncmp(show_OSSE,"n",1)!=0 &&
            strncmp(show_OSSE,"y",1)!=0 )
     { 
	   printf(" Please enter n/y!\n");
           goto step1;
     }

 step2:
  strcpy(msg,"show_EGRET");
        Uclgst(msg, show_EGRET, &status);
        if (status != 0)
          {
           printf ("Problem getting show_EGRET!\n");
           exit (1);
          }
  
  if (strncmp(show_EGRET,"n",1)!=0 &&
            strncmp(show_EGRET,"y",1)!=0 )
     { 
	   printf(" Please enter n/y!\n");
           goto step2;
     }

 step3:
  strcpy(msg,"show_COMPTEL");
        Uclgst(msg, show_COMPTEL, &status);
        if (status != 0)
          {
           printf ("Problem getting show_COMPTEL!\n");
           exit (1);
          }

  if (strncmp(show_COMPTEL,"n",1)!=0 &&
            strncmp(show_COMPTEL,"y",1)!=0 )
     { 
	   printf(" Please enter n/y!\n");
           goto step3;
     }

 step4:
 strcpy(msg,"show_BATSE");
        Uclgst(msg, show_BATSE, &status);
        if (status != 0)
          {
           printf ("Problem getting show_BATSE!\n");
           exit (1);
          }
  
  if (strncmp(show_BATSE,"n",1)!=0 &&
            strncmp(show_BATSE,"y",1)!=0 )
     { 
	   printf(" Please enter n/y!\n");
           goto step4;
     }
}
