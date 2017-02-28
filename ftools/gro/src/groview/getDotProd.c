/* return dot product of two
   vectors, v1 & v2, dmension ndim
*/
#include "groview.h"
float getDotProd( float **v1, float *v2, long *ndim )
{
  float sum = 0.0, xsum=0., ysum=0.;
  int i;
 
  for(i=0;i<*ndim;i++)
    {
      sum = sum +  (*v1)[i] * v2[i];
      xsum = xsum + (*v1)[i]*(*v1)[i];
      ysum = ysum + v2[i] * v2[i];
    }
  
  sum = sum; /*/(xsum*ysum);*/

  return (sum);
}
