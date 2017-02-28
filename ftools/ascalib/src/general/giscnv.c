/************************************************************************
 * convolve XRT PSF (input) with GIS PSF (calculated here) using fft.
 * This is based on old fortran routine giscnv.f
 * 
 * History:
 * 9/26/97: ZH A bug fixed in calculating gis psf;
 *             gis_psf_eff_x[y] were added to make the boundary clearer
 * 5/16/00: ZH order of data in 1-d array changed to satisfy c-f interface
 **************************************************************************/

#include <stdlib.h>
#include <math.h>

#define NRANSI        
#include "nrutil.h"                     /* new routines were added */
#include "cfortran.h"

#ifdef vms
#  define giscnv giscnv_
#endif

#define GIS_PSF_RADIUS_MIN_LIMIT 1      /* minimum GIS psf radius */
#define GIS_PSF_CUTOFF_VALUE 1.0e-16    /* cutoff value for GIS PSF */

void giscnv(double Ekev, int wmsiz1, int wmsiz2, double *psf,
	    double binsze, double abnsze)
{

  void fftcnv(double **, double **, int, int, double **);
  double gis_psf(double, double, double, double, double);

  int i, j;                           /* loop counters */

  double xoff, yoff, add, sum;        /* working variables */
  double **gispsf, **xrtpsf;          /* psf arrays for xrt and gis */
  double **cnvpsf;                    /* fft CONvolved image */

  int gis_psf_eff_radius;             /* effective gis psf radius */
  int gis_psf_eff_x,gis_psf_eff_y;    /* effective gis psf radius in x,y */
  int gis_psf_row_min;                /* these 4 paras set boundaries */
  int gis_psf_row_max;                /* for the gis psf matrix */ 
  int gis_psf_col_min;
  int gis_psf_col_max;
  int col_center,row_center;          /* central pixels of the gis psf */


  /* center of gispsf */
  row_center=wmsiz1/2.0;
  col_center=wmsiz2/2.0;

  /* Calculate GIS PSF, which will be normalized
   * to make sure that no flux is lost. */

  /* First, determine how big the 2-D GIS PSF will have to be;
   * the variable "gis_psf_eff_radius" indicates the effective radius of the  
   * significant PSF. The limits are stored in gis_psf_eff_x[y]
   */

  gis_psf_eff_radius = (row_center > col_center) ? row_center : col_center;
  gis_psf_eff_radius -= 1;

  for (i=0;i<=gis_psf_eff_radius;i++) {
    yoff = i * binsze;
    if (gis_psf(0.0,0.0,Ekev,0.0,yoff) < GIS_PSF_CUTOFF_VALUE) {
      gis_psf_eff_radius = i;
      break;   /* do not bother with further calculation */
    }
  }

  /* set a minimum limit for effective radius */
  if (gis_psf_eff_radius < GIS_PSF_RADIUS_MIN_LIMIT) 
    gis_psf_eff_radius = GIS_PSF_RADIUS_MIN_LIMIT;

  /* set the upper limits: row[col]_center - 1 */
  gis_psf_eff_x = (gis_psf_eff_radius < row_center-1) ? 
                   gis_psf_eff_radius : row_center-1;
  gis_psf_eff_y = (gis_psf_eff_radius < col_center-1) ?
                   gis_psf_eff_radius : col_center-1;


  /* allocate memory using modified NR routines - initialized*/
  gispsf=dmatrix(1,wmsiz1,1,wmsiz2);

  /* do gis-psf calculations:
   * we calculate first quardran, then map it to other quardrans */

  /* normalization factor */
  sum = 0.0;  

  /* non-axis: gis_psf_eff_x[y] starts from the center of gispsf[][] */
  for(i=1;i<=gis_psf_eff_x;i++) {
    xoff = i * binsze;
    for(j=1;j<=gis_psf_eff_y;j++) {
      yoff = j * binsze;
      add = gis_psf(0.0, 0.0, Ekev, xoff, yoff);
      gispsf[row_center+i][col_center+j] = add;
      gispsf[row_center-i][col_center+j] = add;
      gispsf[row_center-i][col_center-j] = add;
      gispsf[row_center+i][col_center-j] = add;
      sum += 4.0*add;
    }
  }

  /* on x-axis: j=0 */
  for(i=1;i<=gis_psf_eff_x;i++) {
    xoff = i * binsze;
    add = gis_psf(0.0, 0.0, Ekev, xoff, 0.0);
    gispsf[row_center+i][col_center]=add;
    gispsf[row_center-i][col_center]=add;
    sum += 2.0*add;
  }

  /* on y-axis: i=0 */
  for(j=1;j<=gis_psf_eff_y;j++) {
    yoff = j * binsze;
    add = gis_psf(0.0, 0.0, Ekev, 0.0, yoff);
    gispsf[row_center][col_center+j]=add;
    gispsf[row_center][col_center-j]=add;
    sum += 2.0*add;
  }

  /* central pixel */
  add = gis_psf(0.0, 0.0, Ekev, 0.0, 0.0);
  gispsf[row_center][col_center]=add;
  sum += add;

  /* normalize the array to conserve the flux */
  for (i=1;i<=wmsiz1;i++)
    for (j=1;j<=wmsiz2;j++)
      gispsf[i][j] = gispsf[i][j] / sum;  

  /* convert 0-indexed xrt psf to a 2-d arrray in NR convention */
  xrtpsf=dmatrix(1,wmsiz1,1,wmsiz2); 
  for (j=1;j<=wmsiz2;j++) 
    for (i=1;i<=wmsiz1;i++)
      xrtpsf[i][j]=psf[(j-1)*wmsiz1+(i-1)];

  /* allocate mem for cnspsf -- to store result from fftcnv */
  cnvpsf=dmatrix(1,wmsiz1,1,wmsiz2);

  /* do convolution code */
  fftcnv(gispsf,xrtpsf,wmsiz1,wmsiz2,cnvpsf);

  /* store data in conventional 1-d C array */
  for (j=1;j<=wmsiz2;j++) 
    for (i=1;i<=wmsiz1;i++)
      psf[(j-1)*wmsiz1+(i-1)]=cnvpsf[i][j];

  /* free up memory */
  free_dmatrix(gispsf,1,wmsiz1,1,wmsiz2);
  free_dmatrix(cnvpsf,1,wmsiz1,1,wmsiz2);
  free_dmatrix(xrtpsf,1,wmsiz1,1,wmsiz2);

}

/* define c-f interface */
FCALLSCSUB6(giscnv,GISCNV,giscnv,DOUBLE,INT,INT,PDOUBLE,DOUBLE,DOUBLE)
