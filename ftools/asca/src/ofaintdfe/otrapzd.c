/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/ofaintdfe/otrapzd.c,v 3.8 2000/09/14 14:34:57 peachey Exp $   */
/*                   */
#define FUNC(i,j,x) ((*func)(i,j,x))
/*
double trapzd(double (*func)(int,int,double), int sensor, int ccdmode,
	      double a, double b, int n)
	      */
double trapzd(func,  sensor,  ccdmode, a,  b,  n)
double (*func)(); 
int sensor,  ccdmode, n;
double a, b;
{
  double x,tnm,sum,del;
  static double s;
  int it,j;

  if (n == 1) {
    return (s=0.5*(b-a)*(FUNC(sensor,ccdmode,a)+FUNC(sensor,ccdmode,b)));
  } else {
    for (it=1,j=1;j<n-1;j++) it <<= 1;
    tnm=it;
    del=(b-a)/tnm;
    x=a+0.5*del;
    for (sum=0.0,j=1;j<=it;j++,x+=del) sum += FUNC(sensor,ccdmode,x);
    s=0.5*(s+(b-a)*sum/tnm);
    return s;
  }
}
#undef FUNC
