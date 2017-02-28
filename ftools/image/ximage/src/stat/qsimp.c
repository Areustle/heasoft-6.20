#include <math.h>
#include "../include/xcommon.h"

/* Simple integration */

#define EPS 1.0e-5
#define JMAX 20

float trapzd(float (*func)(float),float a, float b, int n);

float qsimp(float (*func)(float), float a, float b, int *status)
{
   /* NR 4.2
    * Returns the integral of the function func from a to b
    */
	int j;
	float s,st,ost=0.0,os=0.0;


	for (j=1;j<=JMAX;j++){
		st=trapzd(func,a,b,j);
            
		s=(4.0*st-ost)/3.0;
		
		if (j > 5)
			if (fabs(s-os) < EPS*fabs(os) ||
				(s == 0.0 && os == 0.0)) return s;
		os=s;
		ost=st;
	}
	cxwrite("Too many steps in routine qsimp", 10);
	*status = -1;
	return 0.0;
}


float trapzd(float (*func)(float), float a, float b, int n) 
   /*
    * Computes the nth stage of refinment of an extended trapezoidal
    * rule
    */
{
	float x,tnm,sum,del;
	static float s;
	int it,j;

	if (n == 1) {
	  return (s=0.5*(b-a)*((*func)(a)+(*func)(b)));
	} else {

	  it =pow(2,n-2);

	  tnm=it;
	  del=(b-a)/tnm;
	  x=a+0.5*del;
	  sum=0.0;
	  for (j=1;j<=it;j++){

	    sum += (*func)(x);
	    x+=del;
	  }
	  s=0.5*(s+(b-a)*sum/tnm);
	  return s;
	}
       
}
