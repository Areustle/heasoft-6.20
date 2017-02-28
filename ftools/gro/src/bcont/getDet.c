
#include <bcont.h>
 void getDet(int *detsta, int *detstp)
 {
  int   status=0;
  char msg[50]="";

     strcpy(msg,"detsta");
     Uclgsi(msg, detsta, &status);
   if (status != 0)
   {
      printf("Error while reading detsta!\n");
      exit (1);
   }

   if ( *detsta > 8 || *detsta <1 )
        {
         printf("detsta must be in the range 1-%d\n",8);
         exit (1);
	}

    strcpy(msg,"detstp");
     Uclgsi(msg, detstp, &status);
   if (status != 0)
   {
      printf("Error while reading detstp!\n");
      exit (1);
   }

   if ( *detstp > 8 || *detstp <1 || *detstp < *detsta)
        {
         printf("detstp must be in the range 1-%d and greater than detsta\n",8);
         exit (1);
	}
 }

