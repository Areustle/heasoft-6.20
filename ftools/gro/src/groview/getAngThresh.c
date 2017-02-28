#include"groview.h"

void getAngThresh(float *incThresh)
{
  int status = 0, BufLen_2 = 50;
  char  msg[51]=" ";
  double thresh;

  status = 0;
  strcpy(msg,"incThresh");
  Uclgsd(msg, &thresh, &status);
      if (status != 0 )
        {
         printf ("Problem getting incThresh!\n");
         exit (1);
        }
      *incThresh = thresh;
}
