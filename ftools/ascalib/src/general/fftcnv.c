/**************************************************************************
 * void fftcnv(double **data1, double **data2, int n1, int n2, double **m)
 *
 * Purpose:   This is a general purpose 2d fft convolution function in
 *            double precision. 
 *
 * Input:     data1, data2 with dimentional size: n1 X n2
 *
 * Output:    m[1...n1][1...n2]
 *
 * Note:      1) these functions are based on Numerical Recipes Routines
 *            2) all matrices are 1-offset indexed (NR convention)
 *               (0-offset index C standard can be converted using
 *                NR utility routines )
 *            3) input data arrarys are not modified in calling functions
 *
 * Routines:  fft2d(), fourn_dbl(), 
 * used       NR utility routines: nrutil.c (nrutil.h)
 *
 * History:
 * 06/xx/97 ZH  created
 * 06/16/00 ZH  made two changes to make this code more general
 *              and easy to maintain
 *                (1) added two working arrarys, so the input data
 *                    will not be modified in calling functions
 *                (2) changed the output array to double **m;
 *
 ***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define NRANSI        
#include "nrutil.h"        /* this file and nrutil.c were modified */

void fftcnv(double **data1, double **data2, int n1, int n2, double **m)
{

  void fft2d(double **, double *, unsigned long, unsigned long, int);

  int i, j, si, sj;                       /* loop counters */

  double **w1, **w2;                      /* two working arrays */
  double *speq1, *speq2, *sp1, *sp2;      /* worling variables */
  double fac,dr,di;                       /* working variables */

  /* first check the input array sizes, must be powers of 2 for fft */
  if (((int) fmod(n1,2.0) != 0) ||
      ((int) fmod(n2,2.0) != 0) ) {
    printf("fftcnv(): input image sizes must be powers of 2 !\n");
    exit(1);
  }

  /* w1,w2 to store data from data1, data2, 
   * so that data1, data2 will not be modified in calling function */
  w1=dmatrix(1,n1,1,n2); 
  w2=dmatrix(1,n1,1,n2); 
  for (i=1;i<=n1;i++)
    for (j=1;j<=n2;j++) {
      w1[i][j]=data1[i][j];
      w2[i][j]=data2[i][j];
    }

  /* allocate memory using modified NR routines*/
  speq1=dvector(1,2*n1);
  speq2=dvector(1,2*n1);

  /* now do the fft convolution: */
  fft2d(w1,speq1,n1,n2,1);
  fft2d(w2,speq2,n1,n2,1);

  fac=2.0/(n1*n2);
  sp1 = &w1[1][1];
  sp2 = &w2[1][1];

  for (j=1;j<=n1*n2/2;j++) {
    dr = sp1[0]*sp2[0] - sp1[1]*sp2[1];
    di = sp1[0]*sp2[1] + sp1[1]*sp2[0];
    sp1[0] = fac*dr;
    sp1[1] = fac*di;
    sp1 += 2;
    sp2 += 2;
  }
  sp1 = &speq1[1];
  sp2 = &speq2[1];
  for (j=1;j<=n1;j++) {
    dr = sp1[0]*sp2[0] - sp1[1]*sp2[1];
    di = sp1[0]*sp2[1] + sp1[1]*sp2[0];
    sp1[0] = fac*dr;
    sp1[1] = fac*di;
    sp1 += 2;
    sp2 += 2;
  }

  /* inverse fft, output stored in w1 */
  fft2d(w1,speq1,n1,n2,-1);

  /* re-arrange output array:
   *   (1) shift the output array to center
   *   (2) m[1...n1][1...n2]
   */
  for(i=1;i<=n1;i++) {
    for(j=1;j<=n2;j++) {
      si=i+n1/2-1;
      sj=j+n2/2-1;
      si=(si>n1) ? (si-n1) : si;
      sj=(sj>n2) ? (sj-n2) : sj;
      m[i][j]=w1[si][sj];
    }
  }
   
  /* free up memory */
  free_dvector(speq1,1,2*n1);
  free_dvector(speq2,1,2*n1);
  free_dmatrix(w1,1,n1,1,n2);
  free_dmatrix(w2,1,n1,1,n2);
}


/* 2-d double precision fft routines to be called by giscnv.c
 * based on NR routines */

void fft2d(double **data, double *speq, 
	       unsigned long nn2, unsigned long nn3, 
	       int isign)
{

  void fourn_dbl(double data[], unsigned long nn[], int ndim, int isign);
  unsigned long i1,i2,i3,j1,j2,j3,nn[4],ii3;
  double theta,wi,wpi,wpr,wr,wtemp;
  double c1,c2,h1r,h1i,h2r,h2i;

  unsigned long nn1=1;

  c1=0.5;
  c2 = -0.5*isign;
  theta=isign*(6.28318530717959/nn3);
  wtemp=sin(0.5*theta);
  wpr = -2.0*wtemp*wtemp;
  wpi=sin(theta);

  nn[1]=nn2;
  nn[2]=nn3 >> 1;
  if (isign == 1) {

    fourn_dbl(&data[1][1]-1,nn,2,isign);

    for (i2=1,j2=0;i2<=nn2;i2++) {
      speq[++j2]=data[i2][1];
      speq[++j2]=data[i2][2];
    }
  }

  i1=nn1;

  j1=(i1 != 1 ? nn1-i1+2 : 1);
  wr=1.0;
  wi=0.0;
  for (ii3=1,i3=1;i3<=(nn3>>2)+1;i3++,ii3+=2) {
    for (i2=1;i2<=nn2;i2++) {
      if (i3 == 1) {
	j2=(i2 != 1 ? ((nn2-i2)<<1)+3 : 1);
	h1r=c1*(data[i2][1]+speq[j2]);
	h1i=c1*(data[i2][2]-speq[j2+1]);
	h2i=c2*(data[i2][1]-speq[j2]);
	h2r= -c2*(data[i2][2]+speq[j2+1]);
	data[i2][1]=h1r+h2r;
	data[i2][2]=h1i+h2i;
	speq[j2]=h1r-h2r;
	speq[j2+1]=h2i-h1i;
      } else {
	j2=(i2 != 1 ? nn2-i2+2 : 1);
	j3=nn3+3-(i3<<1);
	h1r=c1*(data[i2][ii3]+data[j2][j3]);
	h1i=c1*(data[i2][ii3+1]-data[j2][j3+1]);
	h2i=c2*(data[i2][ii3]-data[j2][j3]);
	h2r= -c2*(data[i2][ii3+1]+data[j2][j3+1]);
	data[i2][ii3]=h1r+wr*h2r-wi*h2i;
	data[i2][ii3+1]=h1i+wr*h2i+wi*h2r;
	data[j2][j3]=h1r-wr*h2r+wi*h2i;
	data[j2][j3+1]= -h1i+wr*h2i+wi*h2r;
      }
    }
    wr=(wtemp=wr)*wpr-wi*wpi+wr;
    wi=wi*wpr+wtemp*wpi+wi;
  }

  if (isign == -1)
    fourn_dbl(&data[1][1]-1,nn,2,isign);

}

/* modified general fft routine from NR */
#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

void fourn_dbl(double data[], unsigned long nn[], int ndim, int isign)
{
  int idim;
  unsigned long i1,i2,i3,i2rev,i3rev,ip1,ip2,ip3,ifp1,ifp2;
  unsigned long ibit,k1,k2,n,nprev,nrem,ntot;
  double tempi,tempr;
  double theta,wi,wpi,wpr,wr,wtemp;

  for (ntot=1,idim=1;idim<=ndim;idim++)
    ntot *= nn[idim];
  nprev=1;
  for (idim=ndim;idim>=1;idim--) {
    n=nn[idim];
    nrem=ntot/(n*nprev);
    ip1=nprev << 1;
    ip2=ip1*n;
    ip3=ip2*nrem;
    i2rev=1;
    for (i2=1;i2<=ip2;i2+=ip1) {
      if (i2 < i2rev) {
	for (i1=i2;i1<=i2+ip1-2;i1+=2) {
	  for (i3=i1;i3<=ip3;i3+=ip2) {
	    i3rev=i2rev+i3-i2;
	    SWAP(data[i3],data[i3rev]);
	    SWAP(data[i3+1],data[i3rev+1]);
	  }
	}
      }
      ibit=ip2 >> 1;
      while (ibit >= ip1 && i2rev > ibit) {
	i2rev -= ibit;
	ibit >>= 1;
      }
      i2rev += ibit;
    }
    ifp1=ip1;
    while (ifp1 < ip2) {
      ifp2=ifp1 << 1;
      theta=isign*6.28318530717959/(ifp2/ip1);
      wtemp=sin(0.5*theta);
      wpr = -2.0*wtemp*wtemp;
      wpi=sin(theta);
      wr=1.0;
      wi=0.0;
      for (i3=1;i3<=ifp1;i3+=ip1) {
	for (i1=i3;i1<=i3+ip1-2;i1+=2) {
	  for (i2=i1;i2<=ip3;i2+=ifp2) {
	    k1=i2;
	    k2=k1+ifp1;
	    tempr=(float)wr*data[k2]-(float)wi*data[k2+1];
	    tempi=(float)wr*data[k2+1]+(float)wi*data[k2];
	    data[k2]=data[k1]-tempr;
	    data[k2+1]=data[k1+1]-tempi;
	    data[k1] += tempr;
	    data[k1+1] += tempi;
	  }
	}
	wr=(wtemp=wr)*wpr-wi*wpi+wr;
	wi=wi*wpr+wtemp*wpi+wi;
      }
      ifp1=ifp2;
    }
    nprev *= n;
  }
}
#undef SWAP







