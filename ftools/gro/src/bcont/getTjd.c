#include <bcont.h>

 void getTjd( double *tjdsta, double *tjdstp, double *tjdFitsta, 
              double *tjdFitstp)
 {
  int   status=0;
  char msg[50]="";
  double diff1=0.0, diff2=0.0;

   strcpy(msg,"tjdsta");
   Uclgsd(msg, tjdsta, &status);
   if (status != 0)
   {
      printf("Error while reading tjdsta\n");
      exit (1);
   }
   
   /* Check if tjdsta is within the file range */
   diff1 = (*tjdsta) - (*tjdFitsta);
   diff2 = (*tjdFitstp) - (*tjdsta);
   
   if ( (diff1<=-0.000000001) || (diff2 <= -0.0000000001))
        {
         printf("\nStart time should be between %15.9f - %15.9f\n",
         *tjdFitsta ,*tjdFitstp);
         exit (1);
	 }
  
      strcpy(msg,"tjdstp");
      Uclgsd(msg, tjdstp, &status);
      if (status != 0)
      {
         printf("Error while reading tjdstp\n");
           exit (1);
      }
   
  /* Check if tjdstp is within the file range */ 
     diff1 = (*tjdFitstp) - (*tjdstp) ;
     diff2 = (*tjdstp) - (*tjdsta); 
     
      if ((diff1 <= -0.000000000) || (diff2 <= -0.000000001))
        {
         printf("\nStop time should be between %15.9f - %15.9f\n",
         *tjdsta, *tjdFitstp );
         exit (1);
	}

}

