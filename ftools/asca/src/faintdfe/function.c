#include <math.h>
#include <stdio.h>
#include "function.h"


/*****************************************************************************
******************************************************************************
* normalize a distribution
*****************************************************************************/
void normalize_distribution(double* dist, int n) {

int i;
double ave=0.;
double sig=0.;
double nn;

for(i=0; i<n; i++){
    ave += dist[i];
    sig += dist[i]*dist[i];
}

nn=(double)n;

ave /= nn;
sig /= nn;
sig = sqrt((sig-ave*ave)*(nn/(nn-1.0)));

for(i=0; i<n; i++){
    dist[i]=(dist[i]-ave )/sig;
}

} /* end of normalize_distribution function */

/****************************************************************************
*****************************************************************************
* Calculate the cross correlation function between two distributions
* note that ccr must have dimension= n1+n2-1
****************************************************************************/
void cross_correlate(double* ccr, double* dist1, int n1, 
                                  double* dist2, int n2) {

int i,j,k;
int n3;
int nn;

n3=n1+n2-1;


/*************
* initialize *
*************/
for(k=0;k<n3;++k) {
    ccr[k]=0.0;
}

/******************
* cross-correlate *
******************/
for(i=0;i<n1;++i) {
    for(j=0;j<n2;++j) {

        k=j-i+n1-1;
        if(k>=n3) fprintf(stderr,"i=%d j=%d k=%d n3=%d\n",i,j,k,n3);
        ccr[k] += dist1[i]*dist2[j];
    }
}

/************
* normalize *
************/
for(k=0;k<=n1-1;++k) {

    nn=k+n2-n1+1;
    if(nn<=n1) ccr[k] /= (double)nn;
    else       ccr[k] /= (double)n1;
}

for(k=n1;k<n3;++k) {

    nn=2*n1-k-1;
    if(nn<=n2) ccr[k] /= (double)nn;
    else       ccr[k] /= (double)n2;
}


} /* end of cross_correlation function */



/****************************************************************************
*****************************************************************************
* find the peak of a distribution - this is done by first finding the
* largest array value and then finding a maximum of the quadratic
* fit to the three points surrounding this point.
* Note: if largest array value is in the first or last position in the array
* the interpolation will read outside the array. This is OK though since the
* intent is that this function will only be passed the center part of
* a larger array.
****************************************************************************/
double find_peak(double* dist, int n) {

int i;

double max;
int npeaks;
int sumi;

int i0;
double a,b;

/*******************************
* find the largest array value *
*******************************/
max=dist[0];

for(i=1; i<n; ++i) {

/*
printf("%d %g\n",i,dist[i]);
*/

    if(dist[i]>max) max=dist[i];
}

/************************************
* locate the position of this value *
************************************/
npeaks=0;
sumi=0;
for(i=0; i<n; ++i) {
    if(dist[i]==max) {
        sumi+=i;
        ++npeaks;
    }
}

i0=(int)((double)sumi/(double)npeaks + 0.5 );

/****************
* quadratic fit *
****************/
a = (dist[i0-1]+dist[i0+1])*.5 - dist[i0];
b = (dist[i0]-dist[i0-1]) - a*(double)(2*i0-1);


if(a!=0.) return( -0.5*b/a );
else      return((double)i0);

} /* end of find_peak function */


