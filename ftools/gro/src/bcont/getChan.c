#include <bcont.h>

 void getChan(int *chansta, int *chanstp, int *nchan)
 {
  int   status=0;
  char msg[50]="";

     strcpy(msg,"chansta");
     Uclgsi(msg, chansta, &status);
   if (status != 0)
   {
      printf("Error while reading chansta!\n");
      exit (1);
   }

   if ( (*chansta > *nchan) || (*chansta <1) )
        {
         printf("start channel must be in the range of 1-%d\n", *nchan);
         exit (1);
	}

    strcpy(msg,"chanstp");
     Uclgsi(msg, chanstp, &status);
   if (status != 0)
   {
      printf("Error while reading chanstp!\n");
      exit (1);
   }

   if ((*chanstp > *nchan) || (*chanstp <1) || (*chanstp < *chansta))
        {
         printf("stop channel must be in the range of 1-%d for DISCLA and greater than start channel\n", *nchan);
         exit (1);
	}
 }

