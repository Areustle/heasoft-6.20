#include <fitsio.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include "imageutils.h"
#include "pil.h"
#include "headas.h"
#include "batcelldetect.h"
#include "mpfit.h"
#include "bat_gswdev.h"

/* 
 * batcelldetect
 * Point spread fitting routines 
 * 
 *
 *  22 May 2003 - split out from batcelldetect.c
 *
 * C. Markwardt 
 *
 * $Id: psffit.c,v 1.64 2011/04/29 17:15:44 craigm Exp $
 */

int svdsol(int n, int dof, double *sbdt, double *x, double *y);

typedef int (*psf_func_type)(int npoints, int npar, double *par, 
		  double *resid, double **dvec, 
		  double *imx, double *imy, int nsrc);

/* Extended information about the covariance matrix. */
struct covar_struct {
  int n;        /* SVD: number of data points */
  int dof;      /* SVD: number of degrees of freedom */
  double bmax;  /* SVD: maximum singular value */
  double *dptr; /* SVD: pointer to all data (free this vector!) */
  double *a, *b, *c, *d, *sbdt, *temp;
  double *cholesky;  /* Old Cholesky method (factored covariance matrix) */
};


/* Structure to pass private data through mpfit() */
struct private_struct {
  double *imx;    /* Array of IMX values */
  double *imy;    /* Array of IMY values */
  double *flux;   /* Array of flux values (counts, rate, etc) */
  double *eflux;  /* Array of error-in-flux values */
  struct covar_struct covar; /* Covariance matrix information */
  /* PSF function: computes the PSF function and partial derivatives */
  psf_func_type psf_func;
  /* Likelihood function: computes likelihood function given the function */
  int (*lik_func)(int npoints, int npar, double *resid, double **dvec, 
		  double *flux, double *eflux, struct covar_struct *covar);
  int nsrc;
};


/* Main residual function.  This function does two things:
   1. Call the model function to compute the sum of PSFs and the
      partial derivatives of the model with respect to the parameters
   2. Call the likelihood function to compute the residuals, and
      the derivatives of the residuals with respect to the parameters
*/
    
int psf_resid(int npoints, int npar, double *par, double *resid,
	      double **dvec, void *pr)
{
  struct private_struct *private = (struct private_struct *) pr;
  double *imx, *imy, *flux, *eflux;
  struct covar_struct *covar;
  int nsrc;
  int status = 0;
  
  imx = private->imx;
  imy = private->imy;
  flux = private->flux;
  eflux = private->eflux;
  nsrc = private->nsrc;
  covar = &(private->covar);

  status = (private->psf_func)(npoints, npar, par, resid, dvec, 
			       imx, imy, nsrc);
  if (status < 0) return status;

  status = (private->lik_func)(npoints, npar, resid, dvec, flux, eflux, covar);

  return status;
}

/*
 * Gaussian likelihood function.
 *   It does the basic (data - model)/sigma calculation for
 *   the residuals, and (-df/dp)/(sigma_flux) for the partials.
 *
 */
int gauss_lik(int npoints, int npar, double *resid, double **dvec, 
	      double *flux, double *eflux,
	      struct covar_struct *covar)
{
  int i, j;

  /* Compute residual */
  for (i=0; i<npoints; i++) {
    resid[i] = (flux[i] - resid[i]);
  }
  /* Negate the Jacobian, since we compute data *MINUS* model */
  if (dvec) {
    for (j=0; j<npar; j++) if (dvec[j]) {
      for (i=0; i<npoints; i++) dvec[j][i] = -dvec[j][i];
    }
  }
	
  if (covar->cholesky) {
    /* If factored covariance matrix was supplied then use it to filter
       the residuals.  Otherwise, just use normal "sigma" weighting. */
    mcholsol(npoints, covar->cholesky, covar->cholesky+npoints*npoints,
	     resid, resid);
    if (dvec) for (j=0; j<npar; j++) if (dvec[j]) {
      mcholsol(npoints, covar->cholesky, covar->cholesky+npoints*npoints,
	       dvec[j], dvec[j]);
    }

  } else if (covar->dptr) {
    /* SVD solution is required */
    svdsol(npoints, covar->dof, covar->sbdt, resid, covar->temp);
    if (dvec) for (j=0; j<npar; j++) if (dvec[j]) {
      svdsol(npoints, covar->dof, covar->sbdt, dvec[j], covar->temp);
    }
  } else {    
    /* DEFAULT: Scale by weights */
    for (i=0; i<npoints; i++) resid[i] /= eflux[i];
    if (dvec) for (j=0; j<npar; j++) if (dvec[j]) {
      for (i=0; i<npoints; i++) dvec[j][i] /= eflux[i];
    }
  }    


  return 0;
}


#define NSRCPAR 6
/* =================================================================== */
/* 
 * Gaussian model:
 *    f_j = Sum_i { p2_i * exp( - u_ij / 2) + p4_i }
 *          j is the pixel number
 *          i is a sum over sources
 *        
 *               (x_j - p0_i)^2 + (y_j - p1_i)^2
 *        u_ij = -------------------------------
 *                           p3_i^2
 *
 *      p0_i = IMX centroid for source i
 *      p1_i = IMY centroid for source i
 *      p2_i = Amplitude (in image units) for source i
 *      p3_i = 1-sigma radius of gaussian for source i
 *      p4_i = local constant background level near source i
 *
 * Parameters are stored as [p0_0, p1_0, p2_0, p3_0, p4_0, p0_1, p1_1, p2_1, p3_1, ...]
 *
 */

int gausspeaks(int npoints, int npar, double *par, double *fval, 
	       double **fjac, double *imx, double *imy, int nsrc)
{
  double *pcur = 0, **fjcur = 0;
  int i, j, icur;
  int ns = NSRCPAR;
  /* double norm = 0; */
  /* static double oldpar[1024]; */

  for (i=0; i<npoints; i++) fval[i] = 0.0;

  /*
     DEBUG PURPOSES ONLY 
  printf("===============\n");
  for (i=0; i<npar; i++) {
    if (par[i] != oldpar[i]) {
      int ii = i/ns;
      printf("CHANGED: %d %f %f %f %f\n", ii, 
	     par[ii*ns+0], par[ii*ns+1], par[ii*ns+2], par[ii*ns+3]);
      printf("         %d %f %f %f %f\n", ii, 
	     par[ii*ns+0]-oldpar[ii*ns+0], 
	     par[ii*ns+1]-oldpar[ii*ns+1],
	     par[ii*ns+2]-oldpar[ii*ns+2],
	     par[ii*ns+3]-oldpar[ii*ns+3],
	     par[ii*ns+3]-oldpar[ii*ns+4]);
    }
    oldpar[i] = par[i];
  }
  */

  /* For each point, compute the gaussian function due to each source */
  for (i=0; i<npoints; i++) {
    double imxi = imx[i], imyi = imy[i];
    int foundbkg = 0;

    /* Loop through sources */
    icur = 0;
    for (j=0; j<nsrc; j++, icur+=ns) {
      double xc, yc, u, sig, sig2;
      double f, f0;

      pcur = par+icur;
      if (fjac) fjcur = fjac+icur;

      xc = (imxi-pcur[0]);                 /* X distance from centroid */
      yc = (imyi-pcur[1]);                 /* Y distance from centroid */
      sig = pcur[3]/2.35; sig2 = sig*sig;  /* Sigma value */
      u = (xc*xc + yc*yc)/sig2;            /* Argument of the exponent */

      f0 = exp(-0.5*u);  /* Function value normalized to unity */
      f = pcur[2]*f0;    /* Function value */
      fval[i] += f;     /* Add to total function */

      /* Include a background term if we haven't yet. */
      if ((u < 9) && (foundbkg == 0)) {
	fval[i] += pcur[4];
	foundbkg = 1;
	if (fjac && fjcur[4])      fjcur[4][i] = 1.0;
      }

      if (fjac && (f0 > 1e-6)) { 
	if (fjcur[0])      fjcur[0][i] = f*xc/sig2;
	if (fjcur[1])      fjcur[1][i] = f*yc/sig2;
	if (fjcur[2] && f) fjcur[2][i] = f/pcur[2]; else fjcur[2][i] = 0;
	if (fjcur[3])      fjcur[3][i] = f*u/sig/2.35;

	/* 
	 * FOR DEBUGGING PURPOSES 
	 *
	printf(" %2d %5d   %f   %f %f %f %f\n",
	       j, i, f0,
	       fjcur[0] ? fjcur[0][i] : 999,
	       fjcur[1] ? fjcur[1][i] : 999,
	       fjcur[2] ? fjcur[2][i] : 999,
	       fjcur[3] ? fjcur[3][i] : 999);
	*/
      }

    }

    /* norm += fval[i]*fval[i]; */
  }

  /* printf("   NORM = %f\n", norm); */
  return 0;
}

/* =================================================================== */
/* 
 * Truncated paraboloid:
 *    f_j = Sum_i { p2_j * fx_ij * fy_ij   +   p4_j }
 * 
 *    fx_ij = (0.5*(w1+|dx_ij|)-w2)/(w1-w2)
 *    fy_ij = (0.5*(w1+|dy_ij|)-w2)/(w1-w2)
 * 
 *    dx_ij = (x_j - p0_i)
 *    dy_ij = (y_j - p1_i)
 *
 *    w2 = p3_i
 *    w1 = p5_i (tangent plane units)
 *          j is the pixel number
 *          i is a sum over sources
 *        
 *      p0_i = IMX centroid for source i
 *      p1_i = IMY centroid for source i
 *      p2_i = Amplitude (in image units) for source i
 *      p3_i = HWHM radius of frustum for source i
 *      p4_i = local constant background level near source i
 *      p5_i = truncated top radius
 *
 * Parameters are stored as [p0_0, p1_0, p2_0, p3_0, p4_0, p0_1, p1_1, p2_1, p3_1, ...]
 *
 */

int pyrpeaks(int npoints, int npar, double *par, double *fval, 
	       double **fjac, double *imx, double *imy, int nsrc)
{
  double *pcur = 0, **fjcur = 0;
  int i, j, icur;
  int ns = NSRCPAR;
  /* double norm = 0; */

  for (i=0; i<npoints; i++) fval[i] = 0.0;

  /* For each point, compute the gaussian function due to each source */
  for (i=0; i<npoints; i++) {
    double imxi = imx[i], imyi = imy[i];
    int foundbkg = 0;

    /* Loop through sources */
    icur = 0;
    for (j=0; j<nsrc; j++, icur+=ns) {
      double xc, yc, u, w1, w2;
      double f, fx, fy, fxy, dw;
      double absx, absy;

      pcur = par+icur;
      if (fjac) fjcur = fjac+icur;

      xc = (imxi-pcur[0]);       /* X distance from centroid */
      yc = (imyi-pcur[1]);	 /* Y distance from centroid */
      w2 = pcur[3]/2;            /* HWHM */
      w1 = pcur[5]/2;            /* Half-width of plateau at top */
      u = (xc*xc + yc*yc)/w2/w2; /* Distance from centroid in PSF units (squared) */
      dw = (w2 - w1);            
      absx = fabs(xc);
      absy = fabs(yc);
      
      fx = (w2 - 0.5*(w1+absx))/dw; /* X component of PSF function */
      if (fx > 1) fx = 1;           /* Clip value to [0..1] range */
      if (fx < 0) fx = 0;
      fy = (w2 - 0.5*(w1+absy))/dw; /* Y component of PSF function */
      if (fy > 1) fy = 1;
      if (fy < 0) fy = 0;

      fxy = fx*fy;      /* PSF function, normalized to unity */
      f = pcur[2]*fxy;  /*    ... and normalized to source flux */
      /* 
      printf("  (%f,%f)-(%f,%f) = (%f,%f)    %f %f     %f\n",
	     imxi,imyi,pcur[0],pcur[1],xc, yc, fx, fy, fxy);
      */
      fval[i] += f;

      /* Include a background term if we haven't yet. */
      if ((u < 9) && (foundbkg == 0)) {
	fval[i] += pcur[4];
	foundbkg = 1;
	if (fjac && fjcur[4])  fjcur[4][i] = 1.0;
      }

      if (fjac && f) {
	double K   = (pcur[2]);                /* Scale factor for fvalual deriv */
	double Kxy = 0.5 / dw;                 /* Scale factor for d/d{x,y} */  
	double Kw  = Kxy / dw;                 /* Scale factor for d/dw1 */ 
	int gfx = (fx > 0) && (fx < 1);        /* Whether fx is not pegged */
	int gfy = (fy > 0) && (fy < 1);        /* Whether fy is not pegged */
	double xsign = (xc > 0)?(1.0):(-1.0);  /* Sign of X */
	double ysign = (yc > 0)?(1.0):(-1.0);  /* Sign of Y */

	double dfx_dx0 = -Kxy*xsign*gfx;
	double dfy_dy0 = -Kxy*ysign*gfy;
	double dfx_dw2 = (absx - w1)*Kw*gfx;
	double dfy_dw2 = (absy - w1)*Kw*gfy;
	double dfx_dw1 = (w2 - absx)*Kw*gfx;
	double dfy_dw1 = (w2 - absy)*Kw*gfy;
	
	if (fjcur[0])  fjcur[0][i] = -K*fy*dfx_dx0;
	if (fjcur[1])  fjcur[1][i] = -K*fx*dfy_dy0;
	if (fjcur[2])  fjcur[2][i] =  fxy;
	if (fjcur[3])  fjcur[3][i] = K*(fx*dfy_dw2 + dfx_dw2*fy)/2;
	if (fjcur[5])  fjcur[5][i] = K*(fx*dfy_dw1 + dfx_dw1*fy)/2;
      }


    }

    /* norm += fval[i]*fval[i]; */
  }

  /* printf("   NORM = %f\n", norm); */
  return 0;
}


/* =================================================================== */
/*   XXXXXXXXXXXXXXXXXX NOTE: NOT USED XXXXXXXXXXXXXXXXXXXXXXXX */
/* 
 * Flat-sided pyramidal frustum
 *    f_j = Sum_i { p2_j * (fx_ij * fy_ij - 1)   +   p4_j }
 * 
 *    fx_ij = (0.5*(w1+|dx_ij|)-w2)/(w1-w2)
 *    fy_ij = (0.5*(w1+|dy_ij|)-w2)/(w1-w2)
 * 
 *    dx_ij = (x_j - p0_i)
 *    dy_ij = (y_j - p1_i)
 *
 *    w2 = p3_i
 *    w1 = p5_i (tangent plane units)
 *          j is the pixel number
 *          i is a sum over sources
 *        
 *      p0_i = IMX centroid for source i
 *      p1_i = IMY centroid for source i
 *      p2_i = Amplitude (in image units) for source i
 *      p3_i = HWHM radius of frustum for source i
 *      p4_i = local constant background level near source i
 *
 * Parameters are stored as [p0_0, p1_0, p2_0, p3_0, p4_0, p0_1, p1_1, p2_1, p3_1, ...]
 *
 */

int flatpyrpeaks(int npoints, int npar, double *par, double *fval, 
	       double **fjac, double *imx, double *imy, int nsrc)
{
  /*   XXXXXXXXXXXXXXXXXX NOTE: NOT USED XXXXXXXXXXXXXXXXXXXXXXXX */
  double *pcur = 0, **fjcur = 0;
  int i, j, icur;
  int ns = NSRCPAR;
  /* double norm = 0; */

  for (i=0; i<npoints; i++) fval[i] = 0.0;

  /* For each point, compute the gaussian function due to each source */
  for (i=0; i<npoints; i++) {
    double imxi = imx[i], imyi = imy[i];
    int foundbkg = 0;

    /* Loop through sources */
    icur = 0;
    for (j=0; j<nsrc; j++, icur+=ns) {
      double xc, yc, u, wid, topwid;
      double f, fx, fy, fxy, dw;
      double absx, absy;

      pcur = par+icur;
      if (fjac) fjcur = fjac+icur;

      xc = (imxi-pcur[0]);  /* X distance from centroid */
      yc = (imyi-pcur[1]);  /* Y distance from centroid */
      wid    = pcur[3]/2;        /* HWHM */
      topwid = pcur[5]/2;     /* HWHM at top */
      u = (xc*xc + yc*yc)/wid/wid; /* Distance from centroid in PSF units (squared) */
      dw = (topwid - wid);
      absx = fabs(xc);
      absy = fabs(yc);
      
      fx = (0.5*(topwid+absx)-wid)/(topwid-wid); /* X component of PSF function */
      if (fx > 1) fx = 1;			 /* Clip value to [0..1] range */
      if (fx < 0) fx = 0;
      fy = (0.5*(topwid+absy)-wid)/(topwid-wid); /* Y component of PSF function */
      if (fy > 1) fy = 1;
      if (fy < 0) fy = 0;

      fxy = fx+fy-1;        /* PSF function, normalized to unity */	  
      if (fxy < 0) fxy = 0; /*    ... clipped ... */
      f = pcur[2]*fxy;      /*    ... and normalized to source flux */

      /* 
      printf("  (%f,%f)-(%f,%f) = (%f,%f)    %f %f     %f\n",
	     imxi,imyi,pcur[0],pcur[1],xc, yc, fx, fy, f);
      */
      fval[i] += f;

      /* Include a background term if we haven't yet. */
      if ((u < 9) && (foundbkg == 0)) {
	fval[i] += pcur[4];
	foundbkg = 1;
      }

      if (fjac && f) {
	double de = 1.0;                       /* Error in flux */
	double dw1 = pcur[2]/(2*dw*de);        /* Scale factor for d/d{x,y} */
	double dw2 = pcur[2]/(2*dw*dw*de);     /* Scale factor for d/dwidth */
	int gfx = (fx > 0) && (fx < 1);        /* Whether fx is not pegged */
	int gfy = (fx > 0) && (fx < 1);        /* Whether fy is not pegged */
	double xsign = (xc > 0)?(1.0):(-1.0);  /* Sign of X */
	double ysign = (yc > 0)?(1.0):(-1.0);  /* Sign of Y */
	
	if (fjcur[0] && gfx) fjcur[0][i] = -xsign*dw1;
	if (fjcur[1] && gfy) fjcur[1][i] = -ysign*dw1;
	if (fjcur[2])        fjcur[2][i] = fxy;
	if (fjcur[3])        fjcur[3][i] = dw2*((wid-absx)*gfx+(wid-absy)*gfy);
	/* FITTING top width 
	if (fjcur[X])        fjcur[X][i] = -dw2*((topwid-absx)*gfx+(topwid-absy)*gfy);
	*/
	if (fjcur[4])      fjcur[4][i] = 1.0;
      }

    }

    /* norm += fval[i]*fval[i]; */
  }

  /* printf("   NORM = %f\n", norm); */
  return 0;
}

/* =================================================================== */
/* 
 * Truncated cone
 *    f_j = Sum_i { 0.5*p2_j * (2*w2 - w1 - rij) / (w2 - w1) + p4_j }
 * 
 *    r = sqrt(dx_ij^2 + dy_ij^2)
 * 
 *    dx_ij = (x_j - p0_i)
 *    dy_ij = (y_j - p1_i)
 *
 *    w2 = p3_i
 *    w1 = p5_i (tangent plane units)
 *          j is the pixel number
 *          i is a sum over sources
 *        
 *      p0_i = IMX centroid for source i
 *      p1_i = IMY centroid for source i
 *      p2_i = Amplitude (in image units) for source i
 *      p3_i = HWHM radius of frustum for source i
 *      p4_i = local constant background level near source i
 *      p5_i = truncated top radius
 *
 * Parameters are stored as [p0_0, p1_0, p2_0, p3_0, p4_0, p5_0, p0_1, p1_1, p2_1, p3_1, ...]
 *
 */

int conepeaks(int npoints, int npar, double *par, double *fval, 
	       double **fjac, double *imx, double *imy, int nsrc)
{
  double *pcur = 0, **fjcur = 0;
  int i, j, icur;
  int ns = NSRCPAR;
  /* double norm = 0; */

  for (i=0; i<npoints; i++) fval[i] = 0.0;

  /* For each point, compute the gaussian function due to each source */
  for (i=0; i<npoints; i++) {
    double imxi = imx[i], imyi = imy[i];
    int foundbkg = 0;

    /* Loop through sources */
    icur = 0;
    for (j=0; j<nsrc; j++, icur+=ns) {
      double xc, yc, u, r, w1, w2;
      double f, fp, dw;

      pcur = par+icur;
      if (fjac) fjcur = fjac+icur;

      xc = (imxi-pcur[0]);      /* X distance from centroid */	   
      yc = (imyi-pcur[1]);	/* Y distance from centroid */	   
      w2 = pcur[3]/2;		/* HWHM */			   
      w1 = pcur[5]/2;		/* Half-width of plateau at top */
      u = (xc*xc + yc*yc);      /* Distance from centroid (squared) */
      dw = (w2 - w1);
      r = sqrt(u);
      
      fp = 0.5*(2*w2 - w1 - r)/dw;  /* Cone function ... */
      if (fp > 1) fp = 1;           /*   ... clipped and normalized to unity ... */
      if (fp < 0) fp = 0;
      f = fp*pcur[2];               /* And normalized to flux */


      /* 
      printf("  (%f,%f)-(%f,%f) = (%f,%f)    %f %f     %f\n",
	     imxi,imyi,pcur[0],pcur[1],xc, yc, fx, fy, f);
      */
      fval[i] += f;

      /* Include a background term if we haven't yet. */
      if ((r < 2*w2) && (foundbkg == 0)) {
	fval[i] += pcur[4];
	foundbkg = 1;
	if (fjac && fjcur[4])  fjcur[4][i] = 1.0;
      }

      if (fjac && f) {
	double de = 1.0;                       /* Error in flux */
	double dw1 = pcur[2]/(2*dw*de);        /* Scale factor for d/d{x,y} */
	double dw2 = pcur[2]/(2*dw*dw*de);     /* Scale factor for d/dwidth */
	int pv = (f > 0 && f < 1);             /* f is varying (betw pegged range) */
	
	if (fjcur[0]&&pv) fjcur[0][i] = dw1*xc/r;
	if (fjcur[1]&&pv) fjcur[1][i] = dw1*yc/r;
	if (fjcur[2])     fjcur[2][i] = fp;
	if (fjcur[3]&&pv) fjcur[3][i] = -0.6*(3*w1-r)*dw2;
	if (fjcur[5]&&pv) fjcur[5][i] = -0.5*(r-w2)*dw2;
      }

    }

    /* norm += fval[i]*fval[i]; */
  }

  /* printf("   NORM = %f\n", norm); */
  return 0;
}

/* =================================================================== */
/* Utility routine to copy initial source struct values to fit parameters */
int psf_init_par(double par[],
		 struct source_struct *source, 
		 /* struct detect_struct *detect, */
		 int bkgfit, int nxpix, int nypix,
		 double psffwhm, double psftopwidth,
		 struct image_struct *image, 
		 struct image_struct *bkgmap, 
		 struct image_struct *bkgvarmap
		 )
{ 
  int iysum, ixsum;
  int inrange;

  /* Convert from FITS 1-based indices to C 0-based indices */
  ixsum = rint(source->xsum - 1);
  iysum = rint(source->ysum - 1);
  /* Be sure the pixel value is in range */
  inrange = ((ixsum >= 0) && (ixsum < nxpix) &&
	     (iysum >= 0) && (iysum < nypix));

  par[0] = source->imx;       /* IMX */
  par[1] = source->imy;       /* IMY */
  par[2] = source->bestflux;  /* FLUX - already background subtracted */
  par[3] = psffwhm;              /* FWHM */
  par[5] = psftopwidth;          /* Pyramid top width */
  
  /* Background level - depends on whether we are fitting it or not */
  if (bkgfit && inrange) {
    par[4] = bkgmap->datap[iysum][ixsum]; /* BKG - constant background */
    /* headas_chat(5, "   (bkg[%d,%d]=%f)\n", ixsum, iysum, par[4]); */
  } else {
    par[4] = 0;
  }

  /* Handle case where the initial flux is zero.  For the fitting
     it's better to have *something* in there which is non-zero. */
  if (par[2] == 0 && inrange)    par[2] = image->datap[iysum][ixsum];
  if (par[2] == 0 && inrange)    par[2] = bkgvarmap->datap[iysum][ixsum];
  if (par[2] == 0)    par[2] = 1;
  if (par[4] == 0 && bkgfit && inrange) 
    par[4] = bkgvarmap->datap[iysum][ixsum];
  
  return 0;
}

/* Utility routine to add a small angle to a large one, accouting roughly for
   the map projection type */
double coor_delta(double x, double dx, int altwcs[])
{
  /* Tangent plane projection */
  if (altwcs['T'-'@'] >= 0) return tan(atan(x)+dx*DTOR);
  
  /* Otherwise, treat things as a pure cartesian projection */
  return x+dx*DTOR;
}
  
/* =================================================================== */
/* Utility routine to make fitting constraints */
int psf_constraints(double par[], mp_par *constraints,
		    struct source_struct *source, 
		    struct detect_struct *detect, 
		    int bkgfit, int nxpix, int nypix, 
		    double posfitwindow,
		    double psffwhm, double psftopwidth,
		    int fit_fwhm, int fit_topwidth,
		    double srcwindowrad, 
		    struct image_struct *bkgvarmap,
		    int posfluxfit
		 )
{
  double imxmax, imxmin;
  double imymax, imymin;
  double poserr, bkgvar;
  double pxmin, pxmax, pymin, pymax;
  double phxmin, phxmax, phymin, phymax;
  int ixpix, iypix;
  int j;
  int status = 0;

  /* Find the bounds of the image */
  /* Pixel position to tangent plane position */
  coco(detect->wcs, detect->altwcs, '.', '+', 0, 0, &imxmin, &imymin, &status);
  coco(detect->wcs, detect->altwcs, '.', '+', nxpix, nypix, &imxmax, &imymax, &status);
  if (imxmin > imxmax) {
    double temp = imxmin; imxmin = imxmax; imxmax = temp; 
  }
  if (imymin > imymax) {
    double temp = imymin; imymin = imymax; imymax = temp; 
  }

  poserr = source->err_rad;   /* Position error [deg] */
  posfitwindow = posfitwindow; /* Position fit window [deg] */

  /* Convert from FITS 1-based indices to C 0-based indices */
  ixpix = rint(source->xsum - 1);
  iypix = rint(source->ysum - 1);
  bkgvar = bkgvarmap->datap[iypix][ixpix];

  /* If we are going to hold the PSF width fixed */
  if (!fit_fwhm) source->constraintflags |= FIX_WIDTH;
  
  if (source->constraintflags & FIX_IMX  ) constraints[0].fixed = 1; /* IMX */
  if (source->constraintflags & FIX_IMY  ) constraints[1].fixed = 1; /* IMY */
  if (source->constraintflags & FIX_FLUX ) constraints[2].fixed = 1; /* FLUX */
  if (source->constraintflags & FIX_WIDTH) constraints[3].fixed = 1; /* SIGMA */
  constraints[5].fixed = 1 - fit_topwidth;

  /* If the background is fixed, above, then we don't allow it to vary */
  if (!bkgfit) {
    constraints[4].fixed = 1;
    par[4] = 0;
  }

  /* Set up for derivatives - ALL YES */
  for (j=0; j<NSRCPAR; j++) constraints[j].side = 3;
  
      /* Enable only for testing the derivative calculations */
#ifdef GLOBAL_DEBUG
  for (j=0; j<NSRCPAR; j++) {
    constraints[j].side = 0;             /* numerical derivatives */
    constraints[j].deriv_debug = 1;      /* use derivative debug checking */
    constraints[j].deriv_reltol = 1e-10; /* tolerance for debug check logging */
  }
#endif


  /* Set finite different step sizes */

  /* Make a position step size which is ~0.05 pix step */
  /* FITS 1-based pixel position to tangent plane position */
  coco(detect->wcs, detect->altwcs, '.', '+', 
       source->xsum+0.05, source->ysum+0.05, 
       &(constraints[0].step), &(constraints[1].step), &status);
  constraints[0].step = fabs(par[0] - constraints[0].step);  /* IMX */
  constraints[1].step = fabs(par[1] - constraints[1].step);  /* IMY */
  constraints[2].step = 0.01*bkgvar;                         /* Amplitude */
  constraints[3].step = par[3]/100;                          /* Radius */
  constraints[4].step = 0.01*bkgvar;                         /* Background */
  constraints[5].step = constraints[3].step;                 /* Top radius */
  
  /* Set parameter limits for sanity */

  /* The position fitting logic is as follows:
   *   * if the ERR_RAD column is missing, source->err_rad = 0;
   *        ---> position fit window is posfitwindow;
   *   * if the ERR_RAD column is null,    source->err_rad = -1;
   *        ---> position fit window is srcwindowrad;
   *   * otherwise the source->err_rad = ERR_RAD;
   *        ---> position fit window is max(3*err_rad,posfitwindow)
   */
  

  /* Convert window box to tangent plane coordinates */
  /* FITS 1-based pixel position to tangent plane position */
  coco(detect->wcs, detect->altwcs, '.', '+',
       source->xsum-srcwindowrad*0.9, source->ysum-srcwindowrad*0.9,
       &pxmin, &pymin, &status);
  coco(detect->wcs, detect->altwcs, '.', '+',
       source->xsum+srcwindowrad*0.9, source->ysum+srcwindowrad*0.9,
       &pxmax, &pymax, &status);

  if (poserr == 0) {
    /* If the position error is zero, then fix it */
    constraints[0].fixed = 1;  
    constraints[1].fixed = 1;  
  } else if (poserr > 0) {  
    /* Use known position error if possible == 3 SIGMA radius */
    pxmin = coor_delta(source->imx,-3*poserr,detect->altwcs);
    pxmax = coor_delta(source->imx,+3*poserr,detect->altwcs);
    pymin = coor_delta(source->imy,-3*poserr,detect->altwcs);
    pymax = coor_delta(source->imy,+3*poserr,detect->altwcs);
  }
  if (posfitwindow > 0) {  
    /* Force the position to be fit if there is a minimum fit window */
    /* IMX Remove position fitting constraints */
    constraints[0].fixed = 0;  
    constraints[1].fixed = 0;  
    
    if (posfitwindow > poserr) {
      pxmin = coor_delta(source->imx,-posfitwindow,detect->altwcs);
      pxmax = coor_delta(source->imx,+posfitwindow,detect->altwcs);
      pymin = coor_delta(source->imy,-posfitwindow,detect->altwcs);
      pymax = coor_delta(source->imy,+posfitwindow,detect->altwcs);
    }
  }

  /* Now convert back the other way and see if the box is too big;
     i.e. if the centroid would fall outside the window we
     chose. */
  /* Tangent plane position to pixel position */
  coco(detect->wcs, detect->altwcs, '+', '.', pxmin, pymin,
       &phxmin, &phymin, &status);
  coco(detect->wcs, detect->altwcs, '+', '.', pxmax, pymax,
       &phxmax, &phymax, &status);

  /* Finally, check that the position error will fit in with the pixel
     values that we have available.  The error is only triggered if:
        1) At least one of the positions is allowed to vary and 
	2) The error box is bigger than the pixel window
  */
  /* All of {x,y}sum and ph{x,y}{min,max} use FITS 1-based indices */
  if ( (constraints[0].fixed == 0 || constraints[1].fixed == 0) &&
       ((fabs(phxmin-source->xsum) > srcwindowrad) ||
	(fabs(phxmax-source->xsum) > srcwindowrad) ||
	(fabs(phymin-source->ysum) > srcwindowrad) ||
	(fabs(phymax-source->ysum) > srcwindowrad)) ) {
    fprintf(stderr, "WARNING: The error radius for source %s at (IMX,IMY) = (%f,%f)\n",
	    source->name, source->imx, source->imy);
    fprintf(stderr, "         is larger than the source window radius.  This may lead to\n");
    fprintf(stderr, "         erroneous PSF fits.  See the ERR_RAD column in incatalog and\n");
    fprintf(stderr, "         the posfitwindow task parameter.\n");
    
    /* Set the error status */
    if (source->status == 0) {
      source->status = ERR_ERRTOOBIG;
    }
  }


  if (pxmin < imxmin) pxmin = imxmin;
  if (pxmax > imxmax) pxmax = imxmax;
  if (pxmin < pxmax) {
    constraints[0].limits[0] = pxmin;  /* IMX */
    constraints[0].limits[1] = pxmax;
  } else {
    constraints[0].limits[0] = pxmax;  /* IMX */
    constraints[0].limits[1] = pxmin;
  }

  if (pymin < imymin) pymin = imymin;
  if (pymax > imymax) pymax = imymax;
  if (pymin < pymax) {
    constraints[1].limits[0] = pymin; /* IMY */
    constraints[1].limits[1] = pymax;
  } else {
    constraints[1].limits[0] = pymax; /* IMY */
    constraints[1].limits[1] = pymin;
  }

  constraints[0].limited[0] = 1; /* IMX */
  constraints[0].limited[1] = 1;
  constraints[1].limited[0] = 1; /* IMY */
  constraints[1].limited[1] = 1;
  
  constraints[3].limits[0] = 0.60*psffwhm;  /* SIGMA hard limit */
  constraints[3].limits[1] = 1.40*psffwhm;  /* SIGMA hard limit */
  constraints[3].limited[0] = 1;
  constraints[3].limited[1] = 1;

  /* Constrained to be positive-only flux */
  if (posfluxfit) {
    /* FLUX */
    constraints[2].limited[0] = 1;
    constraints[2].limits[0]  = 0;
    /* Make sure flux initial condition does not violate the new constraint */
    if (par[2] <= 0) {
      par[2] = 0.5*bkgvar;
    }
  }
  
  return 0;
}


/* =================================================================== */
/* Utility routine to copy fit parameters to the output source structure */
int psf_update_par(double par[], double epar[], 
		   struct source_struct *source, 
		   struct detect_struct *detect,
		   int bkgfit, int nxpix, int nypix,
		   double bestnorm, int nfunc, int nfree,
		   double oversampfact,
		   double contamflux)
{
  int ixpix, iypix, inrange;
  int status = 0;

    /* Position information */
    source->imx = par[0];
    source->imy = par[1];
    source->imx_err = epar[0] * oversampfact;
    source->imy_err = epar[1] * oversampfact;
    if (source->imx_err > 1) source->imx_err = 1;  /* Sanity checks */
    if (source->imy_err > 1) source->imy_err = 1;
    /* Pixel position information: from tangent plane position to pixel position */
    /* Note: xsum and ysum are in 1-based FITS pixel coordinates */
    coco(detect->wcs, detect->altwcs, '+', '.', par[0], par[1], 
	 &(source->xsum), &(source->ysum), &status);
    /* ixpix/iypix are C 0-based indices */
    ixpix = rint(source->xsum - 1);
    iypix = rint(source->ysum - 1);
    inrange = ((ixpix >= 0) && (ixpix < nxpix) && 
	       (iypix >= 0) && (iypix < nypix));

    /* Flux information */
    source->bestflux     = par[2];  /* Background subtracted! */
    source->bestflux_err = epar[2] * oversampfact;
    source->contamflux = contamflux;  /* NOTE: possibly modified below */
    /* If we didn't fit the background level, then a constant level
       was subtracted during the pixel collection stage.  Here we add
       the background level back in. Thus, the CONTAM_RATE measures
       the total contamination due to sources plus diffuse
       background. */
    if (! bkgfit && inrange) { 
      source->contamflux += detect->bkgmap->datap[iypix][ixpix];
    }
    
    /* Width information */
    source->wimx = fabs(par[3]);
    source->wimy = fabs(par[3]);
    source->wimx_err = epar[3] * oversampfact;
    source->wimy_err = epar[3] * oversampfact;

    /* Background information */
    source->bkgvar  = 0;
    source->bkgflux = 0;
    source->ebkgflux = 0;
    if (inrange) {
      source->bkgvar   = detect->bkgvarmap->datap[iypix][ixpix];
      source->bkgflux  = detect->bkgmap   ->datap[iypix][ixpix];
      source->ebkgflux = detect->bkgvarmap->datap[iypix][ixpix];
      if (source->bkgflux_cell == NULL_POS) {
	source->bkgflux_cell  = detect->bkgmap   ->datap[iypix][ixpix];
      }
    }
    /* Note that in either of the two cases, bkgfit=YES or bkgfit=NO,
       the bestflux value represents the background-subtracted flux.
       For bkgfit=YES, the background term was included as a separate
       parameter in the fitting model, so it was not part of par[2].
       For bkgfit=NO, the background was subtracted from the pixel
       values during the preparatory phase.  Therefore, the SNR should be
       computed without subtracting any more background.
    */
    if (source->bkgvar > 0) {
      source->snr = source->bestflux/source->bkgvar;
    }

    /* If we were fitting the background level, then store those values */
    source->bkgflux_fit = NULL_POS;
    if (bkgfit) {
      source->bkgflux  =  par[4];
      source->ebkgflux = epar[4] * oversampfact;
      source->bkgflux_fit =  par[4];
    }

    /* Status information */
    source->chi2 = bestnorm;
    source->dof  = (nfunc - nfree);
    /* NOTE: even though the image may be oversampled, the reduced
       chi-square value will still come out near unity for a good fit,
       so it does not need to be rescaled, or rather, it is being
       rescaled by the number of (oversampled) pixels, which is
       correct.  

       The parameter errors, on the other hand, *do* need to be
       rescaled, as already done above. 
    */

    source->method |= METH_PSF;  /* PSF fit flag added */

    return 0;
}

/* =================================================================== */
int psf_cluster_search(struct source_struct *sources, int nsrc,
		       int isrc, int pixradius)
{
  int i = isrc, j;
  double dx, dy;

  for (j=0; j<nsrc; j++) {
    if (i == j || sources[j].psfcluster >= 0) continue;
    /* Both are FITS 1-based indices, so difference is still pixels */
    dx = fabs(sources[i].xsum - sources[j].xsum);
    dy = fabs(sources[i].ysum - sources[j].ysum);
    if ((dx < 2*pixradius) && (dy < 2*pixradius)) {
      /* Found a nearby match */
      sources[j].psfcluster = sources[i].psfcluster;
      /* Now extend this new source as far as it can go */
      /*   WWW Note recursion */
      psf_cluster_search(sources, nsrc, j, pixradius);
    }
  }

  return 0;
}

/* =================================================================== */
int psf_cluster_sort(struct source_struct *sources, int nsrc,
		     int pixradius, int *nclusters,
		     struct image_struct *found,
		     struct image_struct *mask)
{
  int i, j;
  int icluster = 0;
  int nxpix, nypix, ntotpix;
  int ixpix, iypix;
  int nmatched = 0;

  nxpix = found->axes[0];
  nypix = found->axes[1];
  ntotpix = nxpix*nypix;

  for (i=0; i<nsrc; i++) {
    sources[i].psfcluster = -1;
  }

  for (i=0; i<nsrc; i++) {
    /* Already assigned to a PSF cluster */
    if (sources[i].status < 0) continue;
    if (sources[i].psfcluster >= 0) continue;
    /* Assign a new cluster */
    sources[i].psfcluster = icluster ++;
    
    psf_cluster_search(sources, nsrc, i, pixradius);
  }

  *nclusters = icluster;


  /* Re-assign the "found" array to be the cluster number.  We do it
     at the pixel level because when we extract pixels we don't want
     to double count if there are multiple sources. */
  for (i=0; i<ntotpix; i++) {
    if (found->data[i] <  0)  continue;
    if (mask->data[i]  <= 0)  continue;
    /* Pixel position in X and Y as FITS 1-based indices */
    ixpix = (i % nxpix) + 1;
    iypix = (i / nxpix) + 1;

    found->data[i] = -1;
    for (j=0; j<nsrc; j++) {
      if (fabs(ixpix-sources[j].xsum) < pixradius &&
	  fabs(iypix-sources[j].ysum) < pixradius) {
	found->data[i] = sources[j].psfcluster;
	nmatched ++;
      }
    } /* Loop over sources */

#if 0
    if (found->data[i] == -1) {
      headas_chat(5, "  ... LOST PIXEL (%d,%d)..\n", ixpix, iypix);
    }
#endif

  } /* Loop over pixels */

  headas_chat(5,"  ... cluster stats: %d clusters, %d pixels ...\n",
	      *nclusters, nmatched);

  return 0;
}



/*
 * psffit_covar_cholesky
 * Compute covariance matrix and immediately factor by Cholesky decomposition
 *
 * NOTE: The correlation function is based on the current PSF,
 * including the default input widths, so fitting your PSF may cause
 * problems.
 *
 * int n - number of data points
 * double *imx, *imy, *eflux - data point positions and measured noise
 * int type - type of correlation
 *               1 = use PSF directly
 *               2 = use GAUSSIAN (sigma=parms[0])
 * double *parms - parameters for correlation function
 * double width_base, width_top - PSF widths
 * psf_func_type psf_func - PSF function
 *
 * RETURNS: nxn matrix of factored covariance matrix upon success
 *          0 upon failure
 */
double *psffit_covarmat(int n, double *imx, double *imy,
				     double *eflux,
				     int type, double *parms,
				     double width_base, double width_top,
				     psf_func_type psf_func)
				     
{
  double *a;
  double *ai;
  int i, j;
  double *temp;
  double p[6];

  temp = (double *)malloc(sizeof(double)*n);
  if (temp == 0) return 0;
  a = (double *)malloc(sizeof(double)*n*(n+1));
  if (a == 0) return 0;

  /* ------- */
  /* Initialize PSF parameters: unit height, with default widths, no bkg */
  if (type == 1) {
    p[0] = 0;
    p[1] = 0;
    p[2] = 1;
    p[3] = width_base;
    p[4] = 0;
    p[5] = width_top;
  }

  /* ------- */
  /* Only fill the "upper" triangle since the matrix is symmetric, and
     mcholdc() only needs that triangle to work. */
  for (i=0; i<n; i++) {

    if (type == 1) {
      /* --------------- */
      /* Compute PSF function for every pixel in the dataset, assuming
	 the "ith" pixel is the center of the PSF. */
      p[0] = imx[i];
      p[1] = imy[i];
      psf_func(n, 6, p, temp, 0, imx, imy, 1);
      
      /* Compute covariance matrix as (psf)*noise[1]*noise[2] */
      ai = a + i*n;
      for (j=i; j<n; j++) {
	ai[j] = temp[j]*eflux[i]*eflux[j];
      }
    } else {
      /* --------------- */
      /* Gaussian correlation function */
      double dist2, pcor, inv_sigma2;

      ai = a + i*n;
      inv_sigma2 = 1.0/(parms[0]*parms[0]);
      
      for (j=i; j<n; j++) {
	dist2 = ((imx[i]-imx[j])*(imx[i]-imx[j]) + 
		 (imy[i]-imy[j])*(imy[i]-imy[j]));
	pcor = exp(-0.5*dist2*inv_sigma2);
	ai[j] = pcor*eflux[i]*eflux[j];
	/*
	printf("COVAR[%3d][%3d] = %15.9g %f %f %f\n", i, j, ai[j],
	       sqrt(dist2*inv_sigma2), eflux[i], eflux[j]);
	*/
      }
    }
  }


  free(temp);

  /* ------ */
  /* Perform Cholesky factorization.  If it fails, return a null image */
  /* Note that the factorization occurs "in-place," or more specifically,
     the lower triangle of the matrix is used. */
  if (mcholdc(n, a, a+n*n) < 0) {
    fprintf(stderr, 
	    "ERROR: Cholesky factorization of covariance matrix failed.\n"
	    "       Falling back to standard 'sigma' weighting.\n");
    return 0;
  }

  return a;
}

/*
 * psffit_covar_svd
 * Compute covariance matrix and immediately factor it by SVD
 *
 * NOTE: The correlation function is based on the current PSF,
 * including the default input widths, so fitting your PSF may cause
 * problems.
 *
 * int n - number of data points
 * double *imx, *imy, *eflux - data point positions and measured noise
 * int type - type of correlation
 *               1 = use PSF directly
 *               2 = use GAUSSIAN (sigma=parms[0])
 * double *parms - parameters for correlation function
 * double width_base, width_top - PSF widths
 * psf_func_type psf_func - PSF function
 * double eps - SVD iteration threshold
 * double thresh - threshold for singular values, as a fraction of max value
 * struct covar_struct *covar - structure containing results
 *    ->dptr = main data pointer.  Free this to free all data.
 *    ->a    = covariance matrix
 *    ->b    = n-vector of singular values (after thresholding)
 *    ->c    = nxn matrix of left singular vectors
 *    ->d    = nxn matrix of right singular vectors (untransposed)
 *    ->sbdt = nxn matrix found by computing B^(-1/2) D^T
 *    ->temp = n-vector for temporary work
 *    ->n    = n
 *    ->dof  = number of singular values above threshold
 *    ->bmax = maximum singular value
 *
 * RETURNS: 0 upon failure
 *          1 upon success
 */
int psffit_covarmat_svd(int n, double *imx, double *imy,
			double *eflux,
			int type, double *parms,
			double width_base, double width_top,
			psf_func_type psf_func, 
			double eps, double thresh,
			struct covar_struct *covar)
				     
{
  int i, j;
  int dof;
  double *temp;
  double p[6];
  double *a, *b, *c, *d, *sbdt;
  double *dptr = 0;
  double *ai;
  double bmax;

  covar->dptr = 0;
  /*                                         a b c d sbdt temp */
  dptr = (double *) malloc(sizeof(double)*n*(n+1+n+n+n    +1));
  if (dptr == 0) return 0;

  /* Assign each of the matrix elements */
  a = dptr;        /* A = Covariance matrix */
  b = a+(n*n);     /* B = diagonal matrix in SVD (D B C^T) */
  c = b+n;         /* C = lower resultant in SVD */
  d = c+(n*n);     /* D = upper resultant in SVD */
  sbdt = d+(n*n);  /* B^(-1/2) D^T */
  temp = d+(n*n);  /* temp = temporary vector */

  /* ------- */
  /* Initialize PSF parameters: unit height, with default widths, no bkg */
  if (type == 1) {
    p[0] = 0;
    p[1] = 0;
    p[2] = 1;
    p[3] = width_base;
    p[4] = 0;
    p[5] = width_top;
  }

  /* Compute covariance matrix */
  /* ------- */
  for (i=0; i<n; i++) {

    if (type == 1) {
      /* --------------- */
      /* Compute PSF function for every pixel in the dataset, assuming
	 the "ith" pixel is the center of the PSF. */
      p[0] = imx[i];
      p[1] = imy[i];
      psf_func(n, 6, p, temp, 0, imx, imy, 1);
      
      /* Compute covariance matrix as psf*noise[1]*noise[2] */
      ai = a + n*i;
      for (j=i; j<n; j++) {
	ai[j] = temp[j]*eflux[i]*eflux[j];
      }
    } else {
      /* --------------- */
      /* Gaussian correlation function */
      double dist2, pcor, inv_sigma2;

      ai = a + n*i;
      inv_sigma2 = 1.0/(parms[0]*parms[0]);
      
      for (j=i; j<n; j++) {
	dist2 = ((imx[i]-imx[j])*(imx[i]-imx[j]) + 
		 (imy[i]-imy[j])*(imy[i]-imy[j]));
	pcor = exp(-0.5*dist2*inv_sigma2);
	ai[j] = pcor*eflux[i]*eflux[j];
	/*
	printf("COVAR[%3d][%3d] = %15.9g %f %f %f\n", i, j, ai[j],
	       sqrt(dist2*inv_sigma2), eflux[i], eflux[j]);
	*/
      }
    }
  }

  /* Make it symmetric if needed */
  for (i=0; i<n; i++) {
    ai = a + n*i;
    for (j=i+1; j<n; j++) {
      a[j*n+i] = ai[j];
    }
  }
      

  /* ------ */
  /* Perform singular value decomposition. */
  svd(a, n, n, d, b, c, eps, 3, temp);

  /* Find the maximum singular value */
  bmax = 0;
  for (i=0; i<n; i++) if (b[i] > bmax) bmax = b[i];

  /* Remove values smaller than thresh*bmax */
  for (i=0; i<n; i++) if (b[i] < thresh*bmax) b[i] = 0;
  
  /* Find number of degrees of freedom */
  for (i=n-1; i>=0; i--) if (b[i] != 0) break;
  dof = i+1;

  /* Compute B^(-1/2) D^T, at least the first dof rows of it. */
  for (i=0; i<n; i++) {
    if (b[i] > 0) {
      for (j=0; j<n; j++) sbdt[i*n+j] = d[j*n+i]/sqrt(b[i]);
    } else {
      for (j=0; j<n; j++) sbdt[i*n+j] = 0;
    }
  }

#if 0
  {
    static int done = 0;
    FILE *tempfile = 0;

    if (done == 0) {
      tempfile = fopen("/tmp/covarmap.dat","w");
      for (i=0; i<n; i++) {
	for (j=0; j<n; j++) {
	  fprintf(tempfile, "%d %d %g %g %g %g %g\n", 
		  i, j, a[i*n+j], c[i*n+j], d[i*n+j], b[i], sbdt[i*n+j]);
	}
      }
      fclose(tempfile);
      done = 1;
    }
  }
#endif


  /* Save the results for later */
  covar->n = n;
  covar->dof = dof;
  covar->dptr = dptr;
  covar->bmax = bmax;
  covar->a = a;
  covar->b = b;
  covar->c = c;
  covar->d = d;
  covar->sbdt = sbdt;
  covar->temp = temp;

  return 1;
}

/* Compute: temp = B^(-1/2) D^T x, then temp -> x */
int svdsol(int n, int dof, double *sbdt, double *x, double *temp)
{
  int i, j;
  double *xp;
  double ytot;

  for (i=0; i<n; i++) {
    ytot = 0;
    xp = x;
    for (j=0; j<n; j++) ytot += (*sbdt++)*(*xp++);
    temp[i] = ytot;
  }
  for (i=0; i<n; i++) x[i] = temp[i];
  return 0;
}



/* =================================================================== */
/* =================================================================== */
/* =================================================================== */
/* Main point source fitting routine */
/* =================================================================== */
/* =================================================================== */
/* =================================================================== */
int psffit(struct parm_struct *parms, struct detect_struct *detect)
{
  int npix, ngoodpix;
  int nsrc, ngoodsrc;
  int icluster, nclusters;
  int ngoodpar, npar;
  int status = 0;
  int nxpix, nypix, ntotpix;
  int i, igood, j, ixpix, iypix;
  int ns = NSRCPAR;  /* Number of parameters per source */
  /* Data pointers */
  double *ptr = 0, *imx = 0, *imy = 0, *flux = 0, *eflux = 0;
  /* Parameter pointers */
  double *par = 0, *epar = 0, *parx = 0;
  int *iptr = 0, *ixpixlist = 0, *iypixlist = 0;
  int *icxmin = 0, *icxmax = 0, *icymin = 0, *icymax = 0, *ncpix = 0;
  FILE *outfile = 0;

  double oversampfact = 1.0;
  double psffwhm, psftopwidth;

  /* Source detection information */
  struct source_struct *sources = 0;

  /* Image information */
  struct image_struct *found = 0;
  struct image_struct *image = 0;
  struct image_struct *bkgmap = 0;
  struct image_struct *bkgvarmap = 0;
  struct image_struct *mask = 0;

  /* Fitting variables */
  struct private_struct private;
  mp_result fitresult;
  mp_par    *constraints = 0;
  mp_config mpconfig;

  if ((parms == 0) || (detect == 0) 
      || (detect->npixfound == 0) || (detect->nsrcfound == 0))
    return NULL_INPUT_PTR;

  private.covar.cholesky = 0;
  private.covar.dptr = 0;

  sources = detect->sources;
  nsrc = detect->nsrcfound;

  ngoodsrc = 0;
  for (i=0; i<nsrc; i++) {
    if (sources[i].status >= 0) ngoodsrc++;
  }
  /* No sources to fit --- ignore */
  if (ngoodsrc == 0) return 0;

  npar = ngoodsrc*ns;

  /* This is the factor by which each parameter fit error must be
     scaled (multiplied by) in order to account for the oversampling.
     Since oversampling increases the number of data points without
     increasing the number of *independent* data-points, the parameter
     errors will be too small by SQRT(OVERSAMPLING).  In this case,
     the image is oversampled in X and Y, hence the product within the
     sqrt(). */
  if (parms->pt_correl == 0) {
    oversampfact = sqrt(parms->oversampx*parms->oversampy);
  } else {
    /* Oversampling (pt-pt correlation) is built into the covariance method */
    oversampfact = 1.0;
  }
  
  /* Collect image information */
  found = detect->found;
  image = detect->image;
  bkgmap = detect->bkgmap;
  bkgvarmap = detect->bkgvarmap;
  mask  = detect->mask;
  nxpix = image->axes[0];
  nypix = image->axes[1];
  ntotpix = nxpix*nypix;
  psffwhm = parms->psffwhm;
  psftopwidth = parms->psftopwidth;

  /* --------------------- */
  /* Sort the sources into clusters of nearby pixels.  Each cluster
     will be fit independently of the other clusters */
  psf_cluster_sort(sources, nsrc, parms->srcwindowrad*5/4, &nclusters, 
		   found, mask);

  /* Count number of "found" pixels */
  npix = 0;
  for (i=0; i<ntotpix; i++) {
    if ((found->data[i] >= 0)&&(mask->data[i] > 0)) {
      npix++;
    }
  }

  headas_chat(5, "  ...psffit nsrc=%d ngoodsrc=%d npar=%d npix=%d...\n", 
	      nsrc, ngoodsrc, npar, npix);
#if 0
 {
   fitsfile *clusterfile = 0;
   int mystatus = 0;
   fprintf(stderr, "WARNING: debugging clustermap code is on!\n");
   fits_create_file(&clusterfile, "/tmp/clustermap.fits", &mystatus);
   mystatus = 0;
   image_write(clusterfile, found, &mystatus);
   mystatus = 0;
   fits_close_file(clusterfile, &mystatus);
 }
#endif


  /* Determine the fitting function */
  if (parms->psfshape == GAUSSIAN) {
    private.psf_func = gausspeaks;
  } else if (parms->psfshape == PYRAMID) {
    private.psf_func = pyrpeaks;
  } else if (parms->psfshape == TRUNCONE) {
    private.psf_func = conepeaks;
  } else {
    return -1;  /* Unknown PSF type */
  }

  /* --------------------- */
  /*
   * Image data:
   *    IMX - position of pixel in tanx [npix]
   *    IMY - position of pixel in tany [npix]
   *    FLUX - image flux at pixel position [npix]
   *    EFLUX - image flux variance at pixel position [npix]
   * Parameter data
   *    IMX - position of source in tanx [nsrc]
   *    IMY - position of source in tany [nsrc]
   *    FLUX - flux of source at image peak [nsrc]
   *    SIGMA - width of peak in gaussian sigmas
   * Parameter uncertainty
   *    [ same as parameter data ]
   *    CLUSTER BOUNDARIES
   */
  ptr = (double *) malloc(sizeof(double)*(npix*ns + npar*2 + npar));
  iptr = (int *) malloc(sizeof(int)*(npix*2 + nclusters*5));
  constraints = malloc(sizeof(mp_par)*npar);
  if (ptr == 0 || iptr == 0 || constraints == 0) {
    status = MEMORY_ALLOCATION;
    goto DONE;
  }

  /* --------------------- */
  /* Partition the allocated memory into storage for location,
     image flux, and fit parameters */
  imx = ptr; imy = ptr+npix; flux = imy+npix; eflux = flux+npix;
  par = eflux+npix; parx = par+npar; epar = parx+npar; 
  ixpixlist = iptr; iypixlist = iptr+npix;
  icxmin = iypixlist + npix;   icxmax = icxmin + nclusters;
  icymin = icxmax + nclusters; icymax = icymin + nclusters;
  ncpix = icymax + nclusters;

  /* --------------------- */
  /* Optimization: pass through the image once and collect the min/max
     extents of each cluster */
  for (i=0; i<nclusters; i++) {
    icxmin[i] = -1; icxmax[i] = -1;
    icymin[i] = -1; icymax[i] = -1;
    ncpix[i] = 0; /* Number of pixels */
  }
  for (i=0; i<ntotpix; i++) {
    if ((found->data[i] >= 0) && (mask->data[i] > 0)) {
      ixpix = i % nxpix;
      iypix = i / nxpix;
      icluster = found->data[i];
      ncpix[icluster] ++;
      if (icxmin[icluster] == -1 || ixpix < icxmin[icluster]) icxmin[icluster] = ixpix;
      if (icxmax[icluster] == -1 || ixpix > icxmax[icluster]) icxmax[icluster] = ixpix;
      if (icymin[icluster] == -1 || iypix < icymin[icluster]) icymin[icluster] = iypix;
      if (icymax[icluster] == -1 || iypix > icymax[icluster]) icymax[icluster] = iypix;
    }
  }
  
  

  /* --------------------- */
  /* --------------------- */
  /* Start of the per-cluster loop */
  for (icluster = 0; icluster < nclusters; icluster++) {

  /* Cluster was not present in-bounds */
  if (icxmin[icluster] == -1 || icxmax[icluster] == -1 ||
      icymin[icluster] == -1 || icymax[icluster] == -1) {
    headas_chat(5, "WARNING: PSF cluster %d is out of bounds\n", icluster);
    goto NEXT_SOURCE;
  }

  /* If fewer than half of the pixels for one source are covered,
     then skip this source */
  if (ncpix[icluster] < 14*parms->srcwindowrad*parms->srcwindowrad/10) {
    headas_chat(5, "WARNING: PSF cluster %d has too few pixels (%d < %d)\n", 
		icluster, ncpix[icluster], 
		14*parms->srcwindowrad*parms->srcwindowrad/10);
    goto NEXT_SOURCE;
  }

  /* --------------------- */
  /* Locate the "detected" pixels, and copy into the fitting data arrays */
  j = 0;
  for (iypix=icymin[icluster]; iypix <= icymax[icluster]; iypix++) {
    for (ixpix=icxmin[icluster]; ixpix <= icxmax[icluster]; ixpix++) {
      if (found->datap[iypix][ixpix] != icluster) continue;
      if (mask ->datap[iypix][ixpix] <= 0)        continue;

      ixpixlist[j] = ixpix;
      iypixlist[j] = iypix;
      
      /* Compute tanxy position of pixel from pixel coordinates */
      /* Note that ixpix and iypix are in zero-based C array indices, and
	 the "+ 1" expression converts to one-based FITS pixel indices */
      status = 0;
      coco(detect->wcs, detect->altwcs, '.', '+', ixpix+1, iypix+1, 
	   &imx[j], &imy[j], &status);
      if (status) continue;
      
      /* Use background-subtracted flux, and map variance */
      flux[j]  = image    ->datap[iypix][ixpix];
      eflux[j] = bkgvarmap->datap[iypix][ixpix];
      if (eflux[j] == 0) eflux[j] = 1.0e10;
      
      /* Fixed background, if we are not fitting */
      if (! parms->bkgfit) flux[j] -= bkgmap->datap[iypix][ixpix];
      
      /* 
	 Debugging output
	 headas_chat(10,"%d %d %d %f %f %f %f\n",
	 j, ixpix, iypix, imx[j], imy[j], flux[j], eflux[j]);
      */
      
      j++;
      if (j > npix) {
	fprintf(stderr, "ERROR: too many pixels in psffit()?\n");
	break;
      }

    } /* End of ixpix loop */
  }   /* End of iypix loop */
  ngoodpix = j;
  
  /* --------------------- */
  /* Compute pixel-to-pixel covariance matrix, and factor it. */
  if (private.covar.cholesky) free(private.covar.cholesky);
  if (private.covar.dptr)     free(private.covar.dptr);
  private.covar.cholesky = 0;
  private.covar.dptr = 0;
  if (parms->pt_correl != 0) {
    private.covar.cholesky = psffit_covarmat(ngoodpix, imx, imy, eflux,
			       parms->pt_correl, parms->pt_correl_parms,
			       psffwhm, psftopwidth, private.psf_func);
#if 0
    headas_chat(5, "  ... Computing SVD with %d points...\n", ngoodpix);
    psffit_covarmat_svd(ngoodpix, imx, imy, eflux, 
			parms->pt_correl, parms->pt_correl_parms,
			psffwhm, psftopwidth, private.psf_func,
			1e-4, 1e-2, &(private.covar));
    headas_chat(5, "               (done)\n");
#endif
  }


  /* --------------------- */
  /* Fill private data structure */
  private.imx = imx;
  private.imy = imy;
  private.flux = flux;
  private.eflux = eflux;
  private.nsrc = ngoodsrc;
  /* private.psf_func = func; Set above */
  private.lik_func = gauss_lik;

  /* Initialize result structure from MPFIT */
  memset(&fitresult, 0, sizeof(fitresult));
  fitresult.xerror = epar;  /* Request parameter errors */
  memset(constraints, 0, sizeof(mp_par)*npar);


  /* --------------------- */
  /* Initialize the parameters */
  /* Variables accessed:
     sources[] (input)
     image bkgmap, bkgvarmap (input)
     psffwhm, psftopwidth (input)

     par[] (output)
   */
  headas_chat(5, "   (setting initial parameter values)\n");
  igood = 0;
  for (i=0; i<nsrc; i++) {
    if (sources[i].status >= 0 && sources[i].psfcluster == icluster) {

      /* Parameter values */
      psf_init_par(&par[igood*ns], &sources[i],
		   parms->bkgfit, nxpix, nypix, 
		   psffwhm, psftopwidth,
		   image, bkgmap, bkgvarmap);

      /* Parameter constraints */
      psf_constraints(&par[igood*ns], &constraints[igood*ns], &sources[i], 
		      detect, parms->bkgfit, nxpix, nypix, 
		      parms->posfitwindow, psffwhm, psftopwidth, 
		      parms->fit_fwhm, parms->fit_topwidth,
		      parms->srcwindowrad, bkgvarmap, 
		      parms->posfluxfit);

      headas_chat(5, 
		  "  Source %d (%s) (cluster=%d)\n"
		  "      IMX = %f : (min,max) = (%f,%f) %s;\n"
		  "      IMY = %f : (min,max) = (%f,%f) %s;\n"
		  "          MAX = %f ; BKG = %f %s;\n"
		  "          FWHM = %f %s; TOPWIDTH = %f %s;\n",
		  i, sources[i].name, sources[i].psfcluster, 
		  par[igood*ns+0],  /* IMX */
		  constraints[igood*ns+0].limits[0], constraints[igood*ns+0].limits[1],
		  constraints[igood*ns+0].fixed ? "fixed" : "fitted",
		  par[igood*ns+1],  /* IMY */
		  constraints[igood*ns+1].limits[0], constraints[igood*ns+1].limits[1],
		  constraints[igood*ns+1].fixed ? "fixed" : "fitted",
		  par[igood*ns+2],  /* MAX */
		  par[igood*ns+4],  /* BKG */
		  constraints[igood*ns+4].fixed ? "fixed" : "fitted",
		  par[igood*ns+3],  /* FWHM */
		  constraints[igood*ns+3].fixed ? "fixed" : "fitted",
		  par[igood*ns+5],  /* TOPWIDTH */
		  constraints[igood*ns+5].fixed ? "fixed" : "fitted");


      igood++;
    }
  }
  private.nsrc = igood;
  headas_chat(5,"          ==> found %d sources in cluster %d\n", 
	      private.nsrc, icluster);
  ngoodpar = igood*ns;
  if (ngoodpar == 0) goto NEXT_SOURCE;

  /* --------------------- */
  /* Actually fit the gaussian PSF model to the data 
   * - use default MPFIT configurations
   */
  memset(&mpconfig, 0, sizeof(mpconfig));
  mpconfig.xtol = parms->psf_partol;
  mpconfig.ftol = parms->psf_chitol;

  headas_chat(5, "   (executing mpfit())\n");
  status = mpfit(psf_resid, ngoodpix, ngoodpar, par, constraints, 
		 &mpconfig, (void *)&private, &fitresult);

  headas_chat(5, "  ... fit status = %d   niter = %d...\n", status,
	      fitresult.niter);
  headas_chat(5, "      chi-square = %f   dof = %d...\n",
	      fitresult.bestnorm, fitresult.nfunc-fitresult.nfree);

  if (status <= 0) {
    fprintf(stderr, "ERROR: PSF fit procedure failed with status code %d\n",
	    status);
    status = 0;
    goto NEXT_SOURCE;
  }
  status = 0;

  /* --------------------- */
  /* Write pixels to output diagnostic file */
  if (parms->psfdebugfile[0]) {
   double *resid = 0;
 
   resid = (double *) malloc(sizeof(double)*ngoodpix);
   if (resid == 0) {
     fprintf(stderr, "ERROR: DEBUG MEMORY ALLOCATION FAILED\n");
     status = MEMORY_ALLOCATION;
     goto DONE;
   }
   psf_resid(ngoodpix, ngoodpar, par, resid, 0, (void*)&private);

   if (outfile == 0) {
     outfile = fopen(parms->psfdebugfile,"w");
     fprintf(outfile, "# %s v%s - PSF fit debug output\n",
	     parms->taskname, parms->taskver);
     fprintf(outfile, "# Num XPIX YPIX IMX IMY FLUX EFLUX MODEL RESID\n");
   }
   if (outfile) {
     fprintf(outfile, "# BEGIN PSF cluster %d\n", icluster);
     for (i=0; i<ngoodpix; i++) {
       fprintf(outfile, "%d %d %d %f %f %f %f %f %f\n",
	       i, ixpixlist[i], iypixlist[i],
	       private.imx[i], private.imy[i],
	       private.flux[i], private.eflux[i],
	       private.flux[i] - resid[i]*private.eflux[i], resid[i]);
     }
     fprintf(outfile, "# END PSF cluster %d\n", icluster);
   }
   
   free(resid);
  }

  /* --------------------- */
  /* store the values away, and change the "technique" flag to psffit */
  /* variables accessed: 
     sources[]  (output)
     par[] (input)
     epar[] (input)
     detect (input)
     parms->bkgfit (input)
     fitresult.{bestnorm,nfunc,nfree} (input)
  */
     
  /* We are next going to be mucking with the parameter values while
     computing the CONTAM_XXX model values.  Just to be save, we will
     work on a copy of the parameters called parx[] rather than the
     original data called par[]. */
  for (i=0; i<ngoodpar; i++) { parx[i] = par[i]; }

  headas_chat(5, "         %6s  %30s | %12s | %12s | %12s\n",
	      "", "", "FLUX", "BKG", "CONTAM");
  igood = 0;
  for (i=0; i<nsrc; i++) {
    double model_flux = 0;
    double *psrc = parx + igood*ns;
    double opari;
    int is_good;

    /* The "good" test is the same here as above, except we need to
       be particularly careful working around sources that were flagged
       during constraint setup.
    */
    is_good = ((sources[i].psfcluster == icluster) && 
	       (sources[i].status >= 0 || sources[i].status == ERR_ERRTOOBIG));
    if (! is_good ) continue;

    /* Calculate model without the current source */
    opari = psrc[2];  /* Save flux value */
    psrc[2] = 0;
    model_flux = NULL_POS;
    if ( (private.psf_func)(/* npoints */ 1, ngoodpar, parx, &model_flux, 
			    /* fjac */ 0, &(psrc[0]), &(psrc[1]), private.nsrc) ) {
      fprintf(stderr, "FAIL\n");
      model_flux = NULL_POS;
    }
    psrc[2] = opari;  /* Restore original flux value */

    psf_update_par(par+igood*ns, epar+igood*ns, &(sources[i]),
		   detect, parms->bkgfit, nxpix, nypix, 
		   fitresult.bestnorm, fitresult.nfunc, 
		   fitresult.nfree, oversampfact, 
		   model_flux);

    headas_chat(5, "  Source %6d |%30s | %12.6f | %12.6f | %12.6f\n",
		i, sources[i].name, psrc[2], psrc[4], model_flux);

    /* If we are using the covariance matrix approach, the number 
       of degrees of freedom is too large.  This is a fudge! */
    if (private.covar.cholesky) {
      /* Cholesky factorization */
      sources[i].dof /= (parms->oversampx*parms->oversampy);
    } else if (private.covar.dptr) {
      sources[i].dof = private.covar.dof;
    }

    igood ++;
  }

  /* Flag sources with errors if necessary */
  if (status < 0) {
  NEXT_SOURCE:
    headas_chat(5, "WARNING: failed to fit PSF cluster %d\n", icluster);
    for (i=0; i<nsrc; i++) {
      if (sources[i].status >= 0 && sources[i].psfcluster == icluster) {
	sources[i].status = -5;
	headas_chat(5, "     -- Source: %s [%d]\n",
		    sources[i].name, i);
      }
    }
  }

  } /* End of cluster loop */

 DONE:
  
  /* Free temporary resources */
  if (private.covar.cholesky) free(private.covar.cholesky);
  if (private.covar.dptr)     free(private.covar.dptr);
  if (outfile) fclose(outfile);
  if (ptr) free(ptr);
  if (iptr) free(iptr);
  if (constraints) free(constraints);
  return status;
}
