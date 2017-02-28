/******
C
C	comptel_view:	input arguments are source position in SC body
C			carteasian; outputs approximate angular response
C			function value for several cases; 1.27 Mev and 
c			6 Mev, both assuming no phi-bar restriction; 
c			then same energies for phi-bar <= 30 deg; recall
c			phi-bar == compton scatering < (as opposed to 
c			geometric scattering < )
c
c		input/output args:	S == unit source position vector
c					     in SC carteasian
c					R == array of response values for
c					     for various cases
c
c
c		internal variables:	R_nnn   == Response @energy n.nn, MeV
c					inc_nnn == Corresponding incident <
c
c
c	C.R. Shrader	GSFC Code 668.1     9/90
c       Chunhui Pan     GSFC Code 668.1     8/02 written in C
*/
#include "groview.h"
void  CmpView(float **SpaceCor, float **Resp)
{	
  double inc;
  double tmp;
  int i;
/*response values from calibration data, currently pre-launch; note that
c  once calibrations stabilize, the polynomial fit should be replaced w/a
c  lookup table*/
 static double a[] = {1.034749, -0.011170696, 0.00013061724,-2.4751263e-06};
 /* static double b[] = {0.633999, -0.00961830};*/
 static double b[] ={0.61556959,0.00050953134,-0.00056970371,6.9665390e-06};
/*get direction cosine then incident angle (former is trivial for z-axis)*/
 tmp = (*SpaceCor)[2];
 inc  = acos(tmp)*DEGREE;
 
  *Resp = (float *) malloc((long)(2) * sizeof(float));
  
  if( (fabs(inc)- 60.0) > 0.0 )     /*for inc angles > 45 deg, R=0*/
    {
      for(i=0;i<2;i++) (*Resp)[i] = 0.0;
      return;
     }
  else
     {
   /* 1275 KEV Case, total no phi-bar restric*/ 
      (*Resp)[0] = a[0] + a[1] * inc + a[2]*inc*inc + a[3]*pow(inc,3);
      if (fabs((*Resp)[0]) > 1.0) (*Resp)[0] = 1.0;
      if ((*Resp)[0] < 0.0) (*Resp)[0] = 0.;

   /* 1275 KEV Case, std phi */
      (*Resp)[1] = b[0] + b[1]*inc;
      if (fabs((*Resp)[1]) > 1.0) (*Resp)[1] = 1.0;
      if ((*Resp)[1] < 0.0) (*Resp)[1] = 0.;
     }
}

