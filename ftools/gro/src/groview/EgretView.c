/*
C
C	egret_view:	input arguments are source position in SC body
C			carteasian; outputs approximate angular response
c			for several cases
c
c
c		input/output args:	S == unit source position vector
c					     in SC carteasian
c					R == array of response values for
c					     for various cases
c
c
c		internal variables:	R_nnn   == Response @energy n.nn, GeV
c					inc_nnn == Corresponding incident <
c
c
c	C.R. Shrader	GSFC Code 668.1     9/90
c       Chunhui Pan     GSFC Code 668.1     8/02  written in C
*/

#include "groview.h"
void EgretView(float **SpaceCor,float **Resp)
{
  float inc; 
  double tmp;
  int i;
  /* response values from Figure IV-2, Phase-2 NRA, Appendix G (p. G-43).*/
  static float a[] = {714.00118, -41.292101, 0.70354288, -0.0029371043};
  static float b[] = {1607.6282, -132.35794, 3.6438965, -0.0331018011};
  static float c[] = {996.20801, -0.25075045, -1.7395012, 0.028781577};
  static float d[] = {1585.7071, -16.906675, -2.2796584, 0.04348440};

  *Resp= (float *) malloc((long)(4) * sizeof(float));
  /* get direction cosine then incident angle (former is trivial for z-axis)*/
   tmp = (*SpaceCor)[2];
   inc  =  acos(tmp)*DEGREE;
 
   if( (fabs(inc)- 40.0) > 0.0 )     /*for inc angles > 45 deg, R=0*/
    {
      for(i=0;i<4;i++) (*Resp)[i] = 0.0;
      return;
     }
  else
     {
       /* 100Mev case with only the central telescopes active */
          (*Resp)[0] = a[0] + a[1] * inc + a[2]*inc*inc + a[3]*pow(inc,3); 
       
       /* 1 Gev case with only the central telescopes active */
         (*Resp)[1] = b[0] + b[1] * inc + b[2]*inc*inc + b[3]*pow(inc,3);
      
       /* 100Mev case, Std. */
          (*Resp)[2] = c[0] + c[1] * inc + c[2]*inc*inc + c[3]*pow(inc,3);
      
       /* 1 Gev case, Std. */
          (*Resp)[3] = d[0] + d[1] * inc + d[2]*inc*inc + d[3]*pow(inc,3);    
          
     }
   
      for(i=0;i<4;i++) 
         {
           if ((*Resp)[i] <0.0) (*Resp)[i] = 0.0;
	 }
      
}
    
