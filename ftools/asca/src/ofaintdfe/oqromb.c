/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/oqromb.c,v 3.9 2000/09/14 14:34:57 peachey Exp $   */

/* Romberg integration of func(sensor,ccdmode) from a to b  */
/* Just the Numerical Recipes qromb() with sensor and ccdmode args added */

#include <math.h>
#define EPS 1.0e-8
#define JMAX 25
#define JMAXP (JMAX+1)
#define K 5

double qromb(func,  sensor, ccdmode, a,  b)
double (*func)();
int sensor, ccdmode;
double a,  b;
{
  void polint();
  double trapzd();
  void nrerror();
  double ss,dss;
  double s[JMAXP+1],h[JMAXP+1];
  int j;

  h[1]=1.0;
  for (j=1;j<=JMAX;j++) {
    s[j]=trapzd(func,sensor,ccdmode,a,b,j);
    if (j >= K) {
      polint(&h[j-K],&s[j-K],K,0.0,&ss,&dss);
      if (fabs(dss) < EPS*fabs(ss)) return ss;
    }
    s[j+1]=s[j];
    h[j+1]=0.25*h[j];
  }
  nrerror("Too many steps in routine qromb");
  return 0.0;
}
#undef EPS
#undef JMAX
#undef JMAXP
#undef K
