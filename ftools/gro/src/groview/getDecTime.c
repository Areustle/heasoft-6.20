
#include"groview.h"

void getDecTime(float *Ddeg, float *Dmin, float *Dsec)
{
  int status = 0, BufLen_2 = 50;
  int pos[2];
  char  msg[51]=" ";
  double deg,min,sec;

  status = 0;
  strcpy(msg,"Ddeg");
  Uclgsd(msg, &deg, &status);
      if (status != 0 )
        {
         printf ("Problem getting Ddeg!\n");
         exit (1);
	}
 
   strcpy(msg,"Dmin");
   Uclgsd(msg, &min, &status);
      if (status != 0 )
        {
         printf ("Problem getting Dmin!\n");
         exit (1);
	}

  strcpy(msg,"Dsec");
  Uclgsd(msg, &sec, &status);
      if (status != 0 )
        {
         printf ("Problem getting Dsec!\n");
         exit (1);
	}
      *Ddeg = deg*1.0;
      *Dmin = min*1.0;
      *Dsec = sec*1.0; 

     
}
   
