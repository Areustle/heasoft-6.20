
#include "groview.h"

void getTime(float *hour, float *minu, float *sec)
{
  int status = 0, BufLen_2 = 50,i=0,j=0;
  int pos[2];
  char  msg[50]="", tra[51], shour[5],smin[5],ssec[5],ctmp[1]="";
  
  for (i=0;i<5;i++)
    {
      shour[i] = '\0';
      smin[i] = '\0';
      ssec[i] = '\0';
    }

   /* get H, M, S */
  status = 0;
  strcpy(msg,"tra");
  Uclgst(msg, tra, &status);
      if (status != 0 )
        {
         printf ("Problem getting Z-axis Right Ascension!\n");
         exit (1);
	}

      j = 0;
      for (i=0;i<strlen(tra);i++)
	{  
	    if (strncmp(tra+i,"-",1)==0) 
	   {
            pos[j] = i; 
            j++;
	   }
	}

     for (i=0;i<pos[0];i++) shour[i] = tra[i];
    
     for (i=pos[0]+1;i<pos[1];i++) smin[i-pos[0]-1] = tra[i];

     for (i=pos[1]+1;i<strlen(tra);i++) ssec[i-pos[1]-1] = tra[i];
 
     *hour = atof(shour);
     *minu= atof(smin);
     *sec =  atof(ssec);
   
}
   
