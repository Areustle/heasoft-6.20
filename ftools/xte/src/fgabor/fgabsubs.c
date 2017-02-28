#include <stdlib.h>
#include <math.h>
#include "cftools.h"
#include "xpi.h"

int fgrpar(char *infile,float *fmin,float *fmax,int *nfreq,float *window,
	   int *time_units,char *ratecol,char *extnam, char *outfile,float *maxgap,
	   float *winfrac, float *sigma, int *normalize, int *quiet, int *clobber) 
{
  int BufLen_2 = FLEN_FILENAME;
  int parstat = 0;
  char text[FLEN_STATUS];
  float rnfreq = 0.0;
  
  Uclgst("infile", infile, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error with input filename parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgsr("fmin", fmin, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error with minimum frequency parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsr("fmax", fmax, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error with maximum frequency parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  /* read it as a real so using decimal won't cause error */
  /* parameter will be truncated to integer               */
  Uclgsr("nfreq", &rnfreq, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error with number of frequencies parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  *nfreq = (int) rnfreq;

  Uclgsr("window", window, &parstat);
  if(parstat != 0){
      if(parstat == 3){
	  Uclpst("window","INDEF",&parstat);
	  parstat=0;
	  *window=-6.66;
      }else{
	  c_fcerr(" ");
	  c_fcerr("Error reading window size parameter.");
	  fits_get_errstatus(parstat, text);
	  exit(1);
      }  
  }

  Uclgsb("timeunits",time_units, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error with time units parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  BufLen_2=FLEN_VALUE;
  Uclgst("ratecol", ratecol, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error with rate column name parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgst("extname", extnam, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error with extname parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  BufLen_2=FLEN_FILENAME;
  Uclgst("outfile", outfile, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error with output filename parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgsr("maxgap",maxgap, &parstat);
  if(parstat != 0){
      c_fcerr(" ");
      c_fcerr("Error reading maxgap parameter.");
      fits_get_errstatus(parstat, text);
      exit(1);
  }
  
  Uclgsr("winfrac",winfrac, &parstat);
  if(parstat != 0){
      c_fcerr(" ");
      c_fcerr("Error reading winfrac parameter.");
      fits_get_errstatus(parstat, text);
      exit(1);
  }

  Uclgsr("sigma", sigma, &parstat);
  if(parstat != 0){
      if(parstat == 3){
	  Uclpst("sigma","INDEF",&parstat);
	  parstat=0;
	  *sigma=-6.66;
      }else{
	  c_fcerr(" ");
	  c_fcerr("Error reading sigma parameter.");
	  fits_get_errstatus(parstat, text);
	  exit(1);
      }  
  }
  
  Uclgsb("normalize", normalize, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading normalize parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }

  Uclgsb("quiet",quiet, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading quiet parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  Uclgsb("clobber",clobber, &parstat);
  if(parstat != 0){
    c_fcerr(" ");
    c_fcerr("Error reading clobber parameter.");
    fits_get_errstatus(parstat, text);
    exit(1);
  }
  
  return parstat;
}

float *gabor_transpowr(float winsize, float *freqs, int nfreqs, float *data, int ndata, int odata, int norm)
/*
Based on IDL routine of the same name by Pat Carter (NSWC) via Padi Boyd (GSFC)
  winsize: +/-3 sigma, measured in timesteps
  freqs:   array of frequencies, measured in cycles per dataset
  nfreqs:  number of frequencies
  data:    data array (counts/s) (padded up to next factor of 2)
  ndata:   number of points in data array
  odata:   number of actual data points (prior to padding)
*/
{
    float sigma, *cdata, *pwr, *ftdata, *cwave, *fcwave, *finv, *func;
    float pmin=1.0E+9, pmax=0.0E+0, tempr, tempi, absv, absv2;
    int i, j;
    float *idlfft(float *,int,int); /* remember to decrement pointer by 1 per NR p507 */
    float *newmorlet(int,float,float);
    float *idlcshift(float *,int,int,int);

    sigma = (winsize/6.)/ndata;

    cdata = (float *) malloc(2*ndata*sizeof(float));
    for(i=0;i<ndata;i++) {
	*(cdata+2*i) = *(data+i); /* put data into real part of complex array */
	*(cdata+2*i+1) = 0.0;     /* and set imaginary part to zero           */
    }
    ftdata = idlfft(cdata-1,ndata,-1);
    free(cdata);

    pwr = (float *) malloc(nfreqs*odata*sizeof(float));
    func = (float *) malloc(2*ndata*sizeof(float));
    for(i=0;i<nfreqs;i++){
	cwave = newmorlet(ndata,*(freqs+i),sigma); /* create Morlet wavelet  */
	cwave = idlcshift(cwave,ndata,-1*ndata/2,1); /* shift by one-half Tmax */
	fcwave = idlfft(cwave-1,ndata,-1);
	free(cwave);
	for(j=0;j<ndata;j++) { /* multiply transformed data by transformed wavelet */
	    /* with complex multiplication done correctly this time... */
	    *(func+2*j) = (*(ftdata+2*j) * *(fcwave+2*j)) - (*(ftdata+2*j+1) * *(fcwave+2*j+1));
	    *(func+2*j+1) = (*(ftdata+2*j+1) * *(fcwave+2*j)) + (*(ftdata+2*j) * *(fcwave+2*j+1));
	}
	free(fcwave);
	finv = idlfft(func-1,ndata,1); /* inverse transform of result */
	for(j=0;j<odata;j++) {
	    /* Using Numerical Recipes scheme for complex abs() */
	    tempr = fabs(*(finv+2*j));
	    tempi = fabs(*(finv+2*j+1));
	    if (tempr == 0.0)
		absv = tempi;
	    else if (tempi == 0.0)
		absv = tempr;
	    else if (tempr > tempi)
		absv = tempr * sqrt(1.0+(tempi*tempi)/(tempr*tempr));
	    else
		absv = tempi * sqrt(1.0+(tempr*tempr)/(tempi*tempi));
	    absv2 = absv*absv;
	    if (absv2 < pmin) pmin=absv2;
	    if (absv2 > pmax) pmax=absv2;
	    *(pwr+(odata*i)+j) = absv2;
	}
	free(finv);
    }
    free(func);
    free(ftdata);

    if (norm){
	for(i=0;i<nfreqs;i++){
	    for(j=0;j<odata;j++) {
		*(pwr+(odata*i)+j) = (*(pwr+(odata*i)+j)-pmin)/(pmax-pmin);
	    }
	}
    }
    return pwr;
}

float *newmorlet(int npoints, float k, float sigma)
/*
   Generate Morlet wavelets
     npoints: number of complex data points
     k: frequency
     sigma: fraction of npoints
 
   Complex numbers represented in single array in
   usual fashion (real0,imaginary0,real1,imaginary1,...)
*/
{
    float x, *mw;
    double gwindow, arg, tmp, sigsq;
    int i;
    sigsq=(double) sigma*sigma;
    mw=(float *)malloc(2*npoints*sizeof(float));
    for (i=0;i<npoints;i++) {
	x = (i*1.0)/((float) npoints) - 0.5;
	tmp = (double) x*x/sigsq;
	arg = (double) -1.0*(tmp < 25.0 ? tmp : 25.0);
	gwindow = exp(arg)/sigma/sqrt(M_PI);
	mw[2*i] = cos((double) k*2*M_PI*x) * gwindow;
	mw[2*i+1] = sin((double) k*2*M_PI*x) * gwindow;
    }
    return mw;
}

float *idlshift(float *data, int n, int nshift, int freeorig)
/* 
   Like the IDL "shift" function for floating point arrays
   arg "freeorig" specifies whether to free original data after use
*/
{
    float *temp;
    int i;
    if (nshift == 0) return data;

    temp = (float *) malloc(n*sizeof(float));
    for(i=0;i<n;i++){
	if (nshift < 0){
	    temp[i]=data[(i-nshift)%n];
	}else{
	    temp[i]=data[(i+n-nshift)%n];
	}
    }
    if(freeorig) free(data);
    return temp;
}

float *idlcshift(float *data, int n, int nshift, int freeorig)
/* 
   Like the IDL "shift" function for complex arrays
   (which are represented in C as 2*ndata float arrays)
*/
{
    float *temp, *tempr, *tempi;
    int i;
    float *idlshift(float *,int,int,int);

    if (nshift == 0) return data;

    temp  = (float *) malloc(2*n*sizeof(float));
    tempr = (float *) malloc(n*sizeof(float));
    tempi = (float *) malloc(n*sizeof(float));
    for(i=0;i<n;i++){
	*(tempr+i) = *(data+2*i);
	*(tempi+i) = *(data+2*i+1);
    }
    tempr = idlshift(tempr,n,nshift,1);
    tempi = idlshift(tempi,n,nshift,1);
    for(i=0;i<n;i++){
	*(temp+2*i) = *(tempr+i);
	*(temp+2*i+1) = *(tempi+i);
    }

    free(tempr);
    free(tempi);
    if(freeorig) free(data);
    return temp;
}

#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

float *idlfft(float *indata,int nn,int isign)
/*
Adapted from Numerical Recipes four1.c

Note that NR and IDL amusingly reverse the sense of the forward 
and inverse transform so that the isign parameter is consistent
even though the identification of forward/inverse isn't!

I've included the factor of 1/N where appropriate and made the
function return the transformed data instead of transforming it
in place.
*/
{
    int n,mmax,m,j,istep,i;
    double wtemp,wr,wpr,wpi,wi,theta;
    float tempr,tempi;
    float *outdata;

    /* 21Aug2001 ouch! needed one extra element here!*/
    outdata=(float *)malloc((2*nn+1)*sizeof(float));
    for(i=1;i<=2*nn;i++) *(outdata+i) = *(indata+i);
    
    n=nn << 1;
    j=1;
    for (i=1;i<n;i+=2) {
	if (j > i) {
	    SWAP(outdata[j],outdata[i]);
	    SWAP(outdata[j+1],outdata[i+1]);
	}
	m=n >> 1;
	while (m >= 2 && j > m) {
	    j -= m;
	    m >>= 1;
	}
	j += m;
    }
    mmax=2;
    while (n > mmax) {
	/*istep=2*mmax;
	  theta=6.28318530717959/(isign*mmax);*/
	istep=mmax << 1;
	theta=isign*(6.28318530717959/mmax);
	wtemp=sin(0.5*theta);
	wpr = -2.0*wtemp*wtemp;
	wpi=sin(theta);
	wr=1.0;
	wi=0.0;
	for (m=1;m<mmax;m+=2) {
	    for (i=m;i<=n;i+=istep) {
		j=i+mmax;
		tempr=wr*outdata[j]-wi*outdata[j+1];
		tempi=wr*outdata[j+1]+wi*outdata[j];
		outdata[j]=outdata[i]-tempr;
		outdata[j+1]=outdata[i+1]-tempi;
		outdata[i] += tempr;
		outdata[i+1] += tempi;
	    }
	    wr=(wtemp=wr)*wpr-wi*wpi+wr;
	    wi=wi*wpr+wtemp*wpi+wi;
	}
	mmax=istep;
    }

    if (isign < 0) {
	for(m=0;m<2*nn;m++) outdata[m]=outdata[m+1]/nn;
    }else{
	for(m=0;m<2*nn;m++) outdata[m]=outdata[m+1];
    }

    return outdata;
}

#undef SWAP

float gasdev(idum)
int *idum;
{
	static int iset=0;
	static float gset;
	float fac,r,v1,v2;
	float ran1();

	if  (iset == 0) {
		do {
			v1=2.0*ran1(idum)-1.0;
			v2=2.0*ran1(idum)-1.0;
			r=v1*v1+v2*v2;
		} while (r >= 1.0);
		fac=sqrt(-2.0*log(r)/r);
		gset=v1*fac;
		iset=1;
		return v2*fac;
	} else {
		iset=0;
		return gset;
	}
}

#define M1 259200
#define IA1 7141
#define IC1 54773
#define RM1 (1.0/M1)
#define M2 134456
#define IA2 8121
#define IC2 28411
#define RM2 (1.0/M2)
#define M3 243000
#define IA3 4561
#define IC3 51349

float ran1(idum)
int *idum;
{
	static long ix1,ix2,ix3;
	static float r[98];
	float temp;
	static int iff=0;
	int j;
	/*void nrerror();*/

	if (*idum < 0 || iff == 0) {
		iff=1;
		ix1=(IC1-(*idum)) % M1;
		ix1=(IA1*ix1+IC1) % M1;
		ix2=ix1 % M2;
		ix1=(IA1*ix1+IC1) % M1;
		ix3=ix1 % M3;
		for (j=1;j<=97;j++) {
			ix1=(IA1*ix1+IC1) % M1;
			ix2=(IA2*ix2+IC2) % M2;
			r[j]=(ix1+ix2*RM2)*RM1;
		}
		*idum=1;
	}
	ix1=(IA1*ix1+IC1) % M1;
	ix2=(IA2*ix2+IC2) % M2;
	ix3=(IA3*ix3+IC3) % M3;
	j=1 + ((97*ix3)/M3);
	if (j > 97 || j < 1) c_fcerr("RAN1: This cannot happen.");
	temp=r[j];
	r[j]=(ix1+ix2*RM2)*RM1;
	return temp;
}

#undef M1
#undef IA1
#undef IC1
#undef RM1
#undef M2
#undef IA2
#undef IC2
#undef RM2
#undef M3
#undef IA3
#undef IC3

