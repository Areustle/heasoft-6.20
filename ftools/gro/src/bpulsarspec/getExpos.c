#include "bpulsarspec.h"

void getExpos(long *nrows, double *tjdsta, double *tjdstp, 
              double **startTime, double **stopTime, double *texpos)
{
  double sum=0, tmpdt1=0, tmpdt2=0;
  int i;
 
  for (i=0; i<*nrows; i++)
    {
     tmpdt1 =  (*startTime)[i] - (*tjdsta);
     tmpdt2 =  (*tjdstp) - (*stopTime)[i];
     if ( (tmpdt1 >= -0.000000001) && (tmpdt2 >= -0.000000001))   
      {
      sum =sum + (*stopTime)[i] - (*startTime)[i];
      }
    }
   *texpos = sum * 86400.0;
}
