#include <math.h>
#include <stdlib.h>
#include "../include/xcommon.h"
#include "cfortran.h"

#define BUFCHUNK 5000
#define INTSTEP 0.005
#define INTMINAREA 1.e-20

/*
 *  Bayesian method using prior probability distribution
 *  ApJ 374,344-355 (1991)
 *
 *  See baypri routine at bottom
 */

static float Cnttotal = 0.0;
static float Cntbg = 0.0;
static float Conflev = 0.0;

float qsimp(float (*func)(float), float a, float b, int *status);

void prip_init(float ctot, float cbg, float cl) {
 /* 
  * Assign static variables (constants) before calculation 
  */
    Cnttotal = ctot;
    Cntbg = cbg;
    Conflev = cl;
}

double prip_fact(int j) {
  /* 
   * Simple factorial function 
   */
     double f=1;     
     if (j<0) return 0.0;
     while (j>1)
     f*=j--;

     return f;
}

float prip_func (float x)
   /*
    *  f(x) form
    */
{
    int m;	
    double xbg, expxbg, pxbg, expbg, bgn;
    double ff0, ff1, ff2, fx;
    
    xbg     = x+Cntbg;
    expxbg  = exp(-xbg);
    pxbg    = pow(xbg,Cnttotal);
    expbg     = exp(-Cntbg);
    bgn       = 0.;    
	   
    ff0=prip_fact(Cnttotal);
    ff1=expxbg*pxbg/ff0;

    for(m=0;m<=Cnttotal;m++) {
	ff2=prip_fact(m);
	bgn+=pow(Cntbg,m)/ff2;
    
    }   
    if( Cntbg == 0.0) {
        bgn=1;
    }
   
    fx=ff1/(expbg*bgn);
    
    if(fx<0.0) fx=0.0;
    
    return fx;
    
    
}

float baypri(float ctot, float cbg, float cl, int *status) {
   /*
    *  Bayesian method using prior probablility distribution
    *  ApJ 374,344-355 (1991)
    *
    * ctot   (f) Total counts
    * cbg    (f) Background counts
    * cl     (f) Confidence level
    * status (i) Error flag 0=OK
    */

   float xs, result;
   int buf1size, buf2size;
   float *S1, *S2, *xini1, *xini2, *xmax1, *xmax2;
   float lastS1, lastS2;
   double sum;
   int i, j, imax, jmax;

   *status = 0;

   prip_init(ctot, cbg, cl);
  
   xs = Cnttotal - Cntbg;
   if ( xs < 0. ) xs = 0.;

   buf1size = BUFCHUNK;
   buf2size = BUFCHUNK;
   i = 0;
   /*
    * Allocate first chunk buffer
    */
   S1 = malloc(buf1size*sizeof(float));
   xini1 = malloc(buf1size*sizeof(float));
   xmax1 = malloc(buf1size*sizeof(float));
   S2 = malloc(buf2size*sizeof(float));
   xini2 = malloc(buf2size*sizeof(float));
   xmax2 = malloc(buf2size*sizeof(float));
   if ( !( S1 && S2 && xini1 && xini2 && xmax1 && xmax2 ) ) {
      cxwrite(" priorprob: Memory allocation failed", 10);
      *status = -1;
      return 0.0;
   }
   
   S2[0] = 0.;
   xini2[0] = 0.;
   xmax2[0] = xs;
   lastS2 = 2.;
   i = 0;

   while ( lastS2 > INTMINAREA ) {

      i++;
      if ( i >= buf2size ) {
         buf2size += BUFCHUNK;
         S2 = realloc(S2, buf2size*sizeof(float));
         xini2 = realloc(xini2, buf2size*sizeof(float));
         xmax2 = realloc(xmax2, buf2size*sizeof(float));
         if ( !( S2 && xini2 && xmax2 ) ) {
            cxwrite(" priorprob: S2 Memory reallocation failed", 10);
            *status = -1;
            return 0.0;
         }
      }
      xmax2[i] = xmax2[i-1] + INTSTEP;
      xini2[i] = xmax2[i-1];
      if ( xmax2[i] < 0.0 || xini2[i] < 0.0 ) {
         S2[i] = 0.;
      } else {
         S2[i] = qsimp(prip_func, xini2[i], xmax2[i], status);
         if ( *status != 0 ) return 0.0;
      }
      lastS2 = S2[i];
   }
   imax = i;

   S1[0] = 0.;
   xini1[0] = xs;
   xmax1[0] = xs;
   lastS1 = 2.;
   j = 0;

   while ( lastS1 > INTMINAREA ) {
      j++;
      if ( j >= buf1size ) {
         buf1size += BUFCHUNK;
         S1 = realloc(S1, buf1size*sizeof(float));
         xini1 = realloc(xini1, buf1size*sizeof(float));
         xmax1 = realloc(xmax1, buf1size*sizeof(float));
         if ( !( S1 && xini1 && xmax1 ) ) {
            cxwrite(" priorprob: S1 Memory reallocation failed", 10);
            *status = -1;
            return 0.0;
         }
      }

      xmax1[j] = xini1[j-1];
      xini1[j] = xini1[j-1] - INTSTEP;
      if ( xini1[j] < 0.0 || xmax1[j] < 0.0 ) {
         S1[j] = 0.;
      } else {
         S1[j] = qsimp(prip_func, xini1[j], xmax1[j], status);
         if ( *status != 0 ) return 0.0;
      }
      lastS1 = S1[j];
   }
   jmax = j;

   sum = 0.;
   i = 1;
   j = 1;
   while ( sum <= Conflev && i <= imax && j <= jmax ) {
      if ( S1[j] >= S2[i] ) {
         sum += S1[j];
         j++;
      } else {
         sum += S2[i];
         i++;
      }
   }
   result = 0.5*(xmax2[i-1] + xmax2[i]);

   free(S1);
   free(xini1);
   free(xmax1);
   free(S2);
   free(xini2);
   free(xmax2);
   
   return result;
}
FCALLSCFUN4(FLOAT,baypri,BAYPRI,baypri,FLOAT,FLOAT,FLOAT,PINT)
