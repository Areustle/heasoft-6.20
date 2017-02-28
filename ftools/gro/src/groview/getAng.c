/*COMPUTE  AUNGULAR SEPERATIONS OF POINTS ON A SPHERE*/

#include "groview.h"

void getAng(float *raSource, float *decSource, float **raZ, float **decZ, 
     float **ang, long *npt)
{
  int i;
  float *zx=NULL, *zy=NULL, *zz=NULL, sx, sy, sz;
  double tmp1,tmp,a,d;
  long npts;

  npts = *npt;
  *ang = (float *) malloc((long)*npt * sizeof(float));

  getAdxyz(raZ, decZ, &zx, &zy, &zz, &npts);
 
  a =  *raSource * RAD;
  d =  *decSource * RAD ;    /* dec BETWEEN +/- 90 DEGREES */
  sx = cos(a) * cos(d);
  sy = sin(a) * cos(d);
  sz = sin(d);

  for (i=0; i<(*npt); i++)
    {
      tmp = zz[i] * sz + zy[i] * sy + zx[i] * sx;
      tmp1 = sqrt(zz[i]*zz[i] + zy[i]*zy[i] + zx[i]*zx[i])
	*sqrt(sz*sz + sy*sy + sx*sx);
      (*ang)[i] = acos(tmp/tmp1);
    }
}

