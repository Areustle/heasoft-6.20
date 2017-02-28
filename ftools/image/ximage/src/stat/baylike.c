#include <math.h>
#include "../include/xcommon.h"
#include "cfortran.h"

/*
 *  Bayesian approach with likelihood probability for Poission
 *  Barnett et al particle data group
 *  physc rev D 54 (1996) 166.
 *
 *  See baylike routine at bottom
 */

static float Cnttotal = 0.0;
static float Cntbg = 0.0;
static float Conflev = 0.0;

int zbrac(float(*func)(float), float *x1, float *x2, int *status);

float rtbis(float (*func)(float), float x1, float x2,
                  float xacc, int *status);

float rtsafe(void (*funcd)(float, float *, float *), float x1, float x2, 
                  float xacc, int *status);

void like_init(float ctot, float cbg, float cl) {
 /* 
  * Assign static variables (constants) before calculation 
  */
    Cnttotal = ctot;
    Cntbg = cbg;
    Conflev = cl;
}

double like_fact(int j) {
  /* 
   * Simple factorial function 
   */
     double f=1;     
     if (j<0) return 0;
     while (j>1)
     f*=j--;

     return f;
 }

float like_func (float x)
   /*
    *  f(x) form
    */
{

    double srcbg, expsrcbg, srcbgn, expbg, bgn, cntsrc;
    double ff01, ff02, ff03, ff04;   
    float fx;
   
    int n, m;
    
    cntsrc=x;
    
    srcbg     = cntsrc+Cntbg;
    expsrcbg  = exp(-srcbg);
    expbg     = exp(-Cntbg);

    srcbgn    = 0.;
    bgn       = 0.;    
	   
    for(n=0;n<=Cnttotal;n++) {
	ff01=1.0*like_fact(n);
	srcbgn+=pow(srcbg,n)/ff01;
    
    }

    for(m=0;m<=Cnttotal;m++) {
	ff02=1.0*like_fact(m);
	bgn+=pow(Cntbg,m)/ff02;
    
    }   
    
    if(Cntbg<0.000001) {
        
	bgn=1.0;
	
    }
    
    ff03 = expsrcbg/expbg;
    ff04 = srcbgn/bgn;
    
    
    fx=ff03*ff04  -1. + Conflev;
    
    
    return fx;
    
}


float like_deriv (float x)
   /*
    *  f'(x) form
    */
{

    double srcbg, expsrcbg, cntsrc, bgn2;
    double ff0, ff1, ff2, ff3, ff4;
    
    float dfx;

    int m;
     
    bgn2 = 0.;
    cntsrc = x;
    
    srcbg     = cntsrc+Cntbg;
    
    expsrcbg  = exp(-srcbg);
   
    ff0=expsrcbg;
    
    ff1=pow(srcbg,Cnttotal);
    
    ff2=1.0*like_fact(Cnttotal);
    
    
    for(m=0;m<=Cnttotal;m++) {
	ff3=1.0*like_fact(m);
	bgn2+= pow(Cntbg,m)/ff3;    
    }  
    
    if(Cntbg<0.000001) {
        
	bgn2=1.0;
	
    }
    
    ff4=exp(-Cntbg)*bgn2;
    
  
    
    dfx=-ff0*ff1/(ff2*ff4);
   
    
    return dfx;
        
}

void like_funcd (float x, float *fx, float *dfx) {
       
    *fx  = like_func(x);
    *dfx = like_deriv(x);
    
    return;
}

float baylike(int mode, float ctot, float cbg, float cl, int *status) {
   /*
    *  Bayesian approach with likelihood probability function for Poission
    *  Barnett et al particle data group
    *  physc rev D 54 (1996) 166.
    *
    * mode   (i) 0=rtbis, 1=rtsafe
    * ctot   (f) Total counts
    * cbg    (f) Background counts
    * cl     (f) Confidence level
    * status (i) Error flag 0=OK
    */

   float xlo, xhi, result;
   int zbout;

   like_init(ctot, cbg, cl);
  
   xlo = 5 + ctot - cbg;
   xhi = 10 + ctot - cbg;

   *status = 0;
   zbout = zbrac(like_func, &xlo, &xhi, status);
   if ( *status ) return 0.0; 
   if ( zbout != 1 ) {
      cxwrite(" Unacceptably large bracket in zbrac", 15);
      *status = -1;
      return 0.0;
   }

   if ( mode ) {
      result = rtsafe(like_funcd, xlo, xhi, 0.0001, status);
   } else {
      result = rtbis(like_func, xlo, xhi, 0.0001, status);
   }
   return result;
}
FCALLSCFUN5(FLOAT,baylike,BAYLIKE,baylike,INT,FLOAT,FLOAT,FLOAT,PINT)
