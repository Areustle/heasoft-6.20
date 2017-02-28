
#include "groview.h"

void getSource(float *raSource, float *decSource)
{
  int status = 0;
  double ra, dec;
  char  msg[50]="";

  status = 0;
  strcpy(msg,"raSource");
  Uclgsd(msg, &ra, &status);
      if (status != 0 )
        {
         printf ("Problem getting raSource!\n");
         exit (1);
	}
 
   strcpy(msg,"decSource");
   Uclgsd(msg, &dec, &status);
      if (status != 0 )
        {
         printf ("Problem getting decSource!\n");
         exit (1);
	}

      *raSource = ra*1.0;
      *decSource = dec*1.0;
}
   
