#include <bpulsarspec.h>

 void getChan(int *chani, int *chanj, int *chanm, int *chann, int *chansta,
              int *chanstp)
 {
  int   status=0;
  char msg[50]="";

   strcpy(msg,"chani");
   Uclgsi(msg, chani, &status);
   if (status != 0)
   {
      printf("Error while reading chani!\n");
      exit (1);
   }
 
   /* Check if chani is within the file range */   
      if ( *chani <1 || *chani > 16)
        {
         printf("chani must be in the range 1:16!\n");
         exit (1);
	}

       strcpy(msg,"chanj");
      Uclgsi(msg, chanj, &status);
      if (status != 0)
      {
         printf("Error while reading chanj!\n");
         exit (1);
      }
  
     if ( *chanj > 16 || *chanj <1 || *chanj < *chani )
        {
         printf("chanj must be in the range 1:16 and greater than chani!\n");
         exit (1);
	}

    strcpy(msg,"chanm");
   Uclgsi(msg, chanm, &status);
   if (status != 0)
   {
      printf("Error while reading chanm!\n");
      exit (1);
   }
 
   /* Check if chanm is within the file range */   
      if ( *chanm <1 || *chanm > 16)
        {
         printf("chanm must be in the range 1:16!\n");
         exit (1);
	}

       strcpy(msg,"chann");
      Uclgsi(msg, chann, &status);
      if (status != 0)
      {
         printf("Error while reading chann!\n");
         exit (1);
      }
  
     if ( *chann > 16 || *chann <1 || *chann < *chanm )
        {
         printf("chann must be in the range 1:16 and greater than chanm!\n");
         exit (1);
	}


     strcpy(msg,"chansta");
     Uclgsi(msg, chansta, &status);
   if (status != 0)
   {
      printf("Error while reading chansta!\n");
      exit (1);
   }

   if ( *chansta > 16 || *chansta <1 )
        {
         printf("chansta must be in the range 1-16\n");
         exit (1);
	}

    strcpy(msg,"chanstp");
     Uclgsi(msg, chanstp, &status);
   if (status != 0)
   {
      printf("Error while reading chanstp!\n");
      exit (1);
   }

   if ( *chanstp > 16 || *chanstp <1 || *chanstp < *chansta)
        {
         printf("chanstp must be in the range 1-16 and greater than chansta\n");
         exit (1);
	}
 }

