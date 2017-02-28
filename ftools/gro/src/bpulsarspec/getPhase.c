#include <bpulsarspec.h>

 void getPhase( int *phsta, int *phstp)
 {
  int   status=0;
  char msg[50]="";

   strcpy(msg,"phsta");
   Uclgsi(msg, phsta, &status);
   if (status != 0)
   {
      printf("Error while reading phsta!\n");
      exit (1);
   }
 
   /* Check if tjdstp is within the file range */   
      if ( *phsta > 64 || *phsta <1)
        {
         printf("phsta must be in the range 1:64!\n");
         exit (1);
	}

       strcpy(msg,"phstp");
      Uclgsi(msg, phstp, &status);
      if (status != 0)
      {
         printf("Error while reading phstp!\n");
         exit (1);
      }
  
     if ( *phstp > 64 || *phstp <1 || *phstp < *phsta )
        {
         printf("phstp must be in the range 1:64 and greater than phsta!\n");
         exit (1);
	}
} 

