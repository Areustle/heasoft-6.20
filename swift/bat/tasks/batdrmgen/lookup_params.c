/* --- ----lookup_params.c - depth distribution generator----------- */
/* ----------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "headas_utils.h"
#include "bat_gswdev.h"
#include "batdrmgen.h"


int expand_dist(int n_coeffs, int n_vectors, int n_x,
    float *coeffs, float *x, float *dist)

/* function: expand_dist
 *
 * Returns a set of depth distributions, given a set
 * of chebyshev coefficients that approximate the distributions
 *
 * Based upon "chebev.c" from
 *      "Numerical Recipes in C++: The Art of Scientific
 *      Computing", Second Edition, by
 *      W. Press, S. Teukolsky, W. Vetterling, and B. Flannery
 *      Cambridge University Press, 2002, p. 199
 *
 * The distribution values are given by:
 *
 *                     / n_coeffs-1               \
 *      dist(x) = exp |     Sum  coeffs[j]*T_j(x)  |
 *                     \    j=0                   /
 *
 *      (Note: this is a slightly different definition than
 *      that used in "chebev.c")
 *
 * Inputs:
 *
 *   n_coeffs:    number of chebyshev coefficients in each vector
 *   n_vectors:   number of vectors or distributions 
 *                  (for instance, 3: one for each peak)
 *   n_x:         number of detector slices
 *                  i.e. the number of elements in the expanded distributions)
 *   coeffs:      a one-dimensional array of size n_coeffs*n_vectors
 *                  gives the chebyshev coefficients for each vector
 *                  the order of the coefficients in the array is from 
 *                  lowest-order to highest order
 *   x:           a one-dimensional array of size n_x*n_vectors
 *                  gives the depths of the centers of the detector slices
 *                  (in cm)
 *
 * Ouput:
 *   dist:        a one-dimensional array of size n_x*n_vectors
 *                  gives the distribution values for each vector
 */

{
  double d, dd, sv, y, y2;
  int ind, vector, j;
  float *cur_coeffs;
  
  for (vector=0;vector<n_vectors;vector++) {
    cur_coeffs=coeffs+vector*n_coeffs;
    for (ind=0;ind<n_x;ind++) {
      
      y=((2.0*x[ind]-x[0]-x[n_x-1])/
	 (x[n_x-1]-x[0]));
      y2=2.0*y;
      d=0.0;
      dd=0.0;
      for (j=n_coeffs-1;j>=1;j--) {
	sv=d;
	d=y2*d-dd+cur_coeffs[j];
	dd=sv;
      }
      dist[vector*n_x+ind]=exp(y*d-dd+cur_coeffs[0]);
    }
  }
  return 0;
}


int get_distinterp(float energy, float tanx, float tany, 
    fitsfile *fits_file, float *eninterp, float *xinterp, float *yinterp,
    float *distinterp) {

/*
 * function: get_distinterp
 *
 * Returns arrays of energy, tanx, tany, and distribution values
 * to interpolate between to get an array of distribution values
 * for the energy, tanx, and tany of incident photons
 *
 * Inputs:
 *
 *   energy:     incident photon energy
 *   tanx:       tan(theta_x) for incident photons
 *   tany:       tan(theta_y) for incident photons
 *   fits_file:  pointer to depth distribution "fitsfile" structure
 *
 * Outputs:
 *
 *   eninterp:	a 3-element array:
 *                [0]: table energy just lower than incident photon energy
 *                [1]: incident photon energy
 *                [2]: table energy just higher than incident photon energy
 *              (allocated prior to function call)
 *
 *   xinterp:	a 3-element array:
 *                [0]: table tanx just lower than tanx for incident photons
 *                [1]: tanx for incident photons
 *                [2]: table tanx just higher than tanx for incident photons
 *              (allocated prior to function call)
 *
 *   yinterp:	a 3-element array:
 *                [0]: table tany just lower than tany for incident photons
 *                [1]: tany for incident photons
 *                [2]: table tany just higher than tany for incident photons
 *              (allocated prior to function call)
 *
 *   distinterp: an 8 x 3 x 1000-element array (1 dimensional):
 *                 1000: number of depths sampled
 *                    3: number of peaks (photo + 2 escapes)   
 *                    8: number of different depth distributions needed for 
 *                       bilinear interpolation
 *               last index (depth) varies most rapidly
 *               first index (bilinear index) varies least rapidly
 *               (allocated prior to function call)
 */

  int status=0;
  int hdutype;
  int nulval=0;
  int anynul;

  long n_rows;
  int n_cols;
  long vector_widths[N_PEAKS];
  long n_vectors;
  int n_coeffs;

  int i;
  int j;
  long temp_long;
  long column_bytes;
  int typecode;

  float *encol;
  float *xcol;
  float *ycol;

  int n_energies;
  float *xycol;
  int xy_uniq;

  int small_en_ind;
  int small_x_ind;
  int small_y_ind;

  int interp_rownums[8];
  int en_ind;
  int x_ind;
  int y_ind;

  int bigind;
  int smallind;
  int xdiff;
  int ydiff;

  int row_ind;
  int col_ind;

  float coeff_temp[N_COEFFS];
  float coeff_interp[8*N_COEFFS*N_PEAKS];
  float *coeff_pointer;
  float *dist_pointer;
  int status2;
  float depths[N_DEPTHS];

  int ignore_vectors=0;

  double temp_sum, temp_sum2;
  
  headas_chat(5,"...Inside get_distinterp...\n");
  headas_chat(4,"energy of interest: %f\n",energy);
  headas_chat(4,"tanx of interest: %f\n",tanx);
  headas_chat(4,"tany of interest: %f\n",tany);

  /* see if the energy is below the Cd K edge or Te K edge
   *
   * if it is, we will ignore 1 or 2 of the vectors in the 
   * distribution file */

  if (energy<TE_EDGE) ignore_vectors=1;
  if (energy<CD_EDGE) ignore_vectors=2;
  headas_chat(5,"number of vectors to ignore: %d\n",ignore_vectors);

  /* move to extension 1 of depth distribution fits file */

  if (fits_movabs_hdu(fits_file, 2, &hdutype, &status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* get n_rows */
 
  if (fits_get_num_rows(fits_file, &n_rows, &status)) {
    fits_report_error(stderr,status);
    exit(status);
  }
  headas_chat(4,"number of rows in DEPTH_DIST table: %ld\n",n_rows);

  /* get n_vectors, and make sure it is N_PEAKS */

  if (fits_get_num_cols(fits_file, &n_cols, &status)) {
    fits_report_error(stderr,status);
    exit(status);
  }
  n_vectors=n_cols-3;
  if (n_vectors!=N_PEAKS) {
    fprintf(stderr,
	"ERROR:  number of vectors in distfile is %ld, not N_PEAKS\n",
	n_vectors); 
    return 1;
  }

  /* get n_coeffs (make sure it is N_COEFFS)
   * and vector_widths (make sure they are N_COEFFS) */

  if (fits_get_coltype(fits_file, 4, &typecode,
	&temp_long, &column_bytes, &status)) {
    fits_report_error(stderr,status);
    exit(status);
  }
  n_coeffs=(int)temp_long;
  vector_widths[0]=(int)temp_long;
  if (n_coeffs!=N_COEFFS) {
    fprintf(stderr,
	"ERROR:  number of coefficients in distfile is %d, not N_COEFFS\n",
	n_coeffs); 
    return 1;
  }
  for (i=1; i<n_vectors; i++) {
    if (fits_get_coltype(fits_file, i+4, &typecode,
	  &temp_long, &column_bytes, &status)) {
      fits_report_error(stderr,status);
      exit(status);
    }
    vector_widths[i]=(int)temp_long;
    if (vector_widths[i]!=N_COEFFS) {
      fprintf(stderr,
  	  "ERROR:  width of vector %d in distfile is %ld, not N_COEFFS\n",i,
	  vector_widths[i]); 
      return 1;
    }
  }

  /* allocate memory for encol, xcol, ycol */

  encol=(float *)malloc(n_rows*sizeof(float));
  xcol=(float *)malloc(n_rows*sizeof(float));
  ycol=(float *)malloc(n_rows*sizeof(float));

  /* read in encol, xcol, ycol */
  
  if (fits_read_col(fits_file,TFLOAT,1,1,1,n_rows,&nulval,
	encol,&anynul,&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }
  if (fits_read_col(fits_file,TFLOAT,2,1,1,n_rows,&nulval,
	xcol,&anynul,&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }
  if (fits_read_col(fits_file,TFLOAT,3,1,1,n_rows,&nulval,
	ycol,&anynul,&status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* find the total number of energies in encol */

  n_energies=1;
  while (encol[n_energies-1]<encol[n_energies]) n_energies++;

  headas_chat(4,"number of energies in DEPTH_DIST table: %d\n",n_energies);

  /* error checking: energy in the proper range */

  if ((energy<encol[0])||(energy>encol[n_energies-1])) {
    fprintf(stderr,
      "ERROR:  incident photon energy outside range of distfile\n");
    return 3;
  }

  /* find eninterp, xinterp, yinterp, coeff_interp */

  /* NOTE: ONLY CENTER DISTRIBUTIONS ARE USED */

      /* find number of unique values in xcol */

      xy_uniq=1;
      for (i=1;i<n_rows;i++) {
        if (xcol[i-1]!=xcol[i]) xy_uniq++;
      }

      /* allocate memory for xycol */

      xycol=(float *)malloc(xy_uniq*sizeof(float));

      /* fill xycol */

      xycol[0]=xcol[0];
      j=1;
      for (i=1;i<n_rows-1;i++) {
        if (xcol[i-1]!=xcol[i]) {
	  xycol[j]=xcol[i];
	  j++;
        }
      }

      /* for the center depth distribution,
       * tanx and tany in the table are positive */

      if (tanx<0) tanx*=-1.;
      if (tany<0) tany*=-1.;

      /* error checking: tanx and tany in the proper range */

      if (((tanx<xycol[0])||(tanx>xycol[xy_uniq-1]))||
	((tany<xycol[0])||(tany>xycol[xy_uniq-1]))) {
        fprintf(stderr,
          "ERROR:  source position is outside range of distfile\n");
	return 4;
      }

      /* find index of value in encol that is just smaller than energy */
  
      small_en_ind=0;
      while (encol[small_en_ind+1]<energy) small_en_ind++;
  
      /* find index of value in xycol that is just smaller than tanx */
  
      small_x_ind=0;
      while (xycol[small_x_ind+1]<tanx) small_x_ind++;
  
      /* find index of value in xycol that is just smaller than tany */
  
      small_y_ind=0;
      while (xycol[small_y_ind+1]<tany) small_y_ind++;
  
      /*
       * find interp_rownums 
       *
       *   [0]: lower en, lower x, lower y
       *   [1]: upper en,    "   ,    "   
       *   [2]: lower en, upper x,    "
       *   [3]: upper en,    "   ,    "
       *   [4]: lower en, lower x, upper y
       *   [5]: upper en,    "   ,    "
       *   [6]: lower en, upper x,    "
       *   [7]: upper en,    "   ,    "
       */
  
      for (en_ind=small_en_ind;en_ind<small_en_ind+2;en_ind++) {
        for (x_ind=small_x_ind;x_ind<small_x_ind+2;x_ind++) {
          for (y_ind=small_y_ind;y_ind<small_y_ind+2;y_ind++) {
	    if (x_ind<y_ind) {
	      bigind=y_ind;
	      smallind=x_ind;
	    }
	    else {
	      bigind=x_ind;
	      smallind=y_ind;
	    }
	    xdiff=x_ind-small_x_ind;
	    ydiff=y_ind-small_y_ind;
	    interp_rownums[en_ind-small_en_ind+2*xdiff+4*ydiff]=
	      n_energies*(bigind*(bigind+1.)/2+smallind)+en_ind+1;
	  }
        }
      }

      headas_chat(5,"row numbers of depthfile to interpolate between:\n");
      headas_chat(5,"    %d: lower en, lower x, lower y\n",interp_rownums[0]);
      headas_chat(5,"    %d: upper en, lower x, lower y\n",interp_rownums[1]);
      headas_chat(5,"    %d: lower en, upper x, lower y\n",interp_rownums[2]);
      headas_chat(5,"    %d: upper en, upper x, lower y\n",interp_rownums[3]);
      headas_chat(5,"    %d: lower en, lower x, upper y\n",interp_rownums[4]);
      headas_chat(5,"    %d: upper en, lower x, upper y\n",interp_rownums[5]);
      headas_chat(5,"    %d: lower en, upper x, upper y\n",interp_rownums[6]);
      headas_chat(5,"    %d: upper en, upper x, upper y\n",interp_rownums[7]);

  /* find eninterp */

  eninterp[1]=energy;
  if (small_en_ind<n_energies-1) {
    eninterp[0]=encol[small_en_ind];
    eninterp[2]=encol[small_en_ind+1];
  }
  else {
    eninterp[0]=encol[small_en_ind-1];
    eninterp[2]=encol[small_en_ind];
  }

  headas_chat(5,"Three energies to interpolate between:\n");
  headas_chat(5,"    lower: %f\n",eninterp[0]);
  headas_chat(5,"    middle: %f\n",eninterp[1]);
  headas_chat(5,"    upper: %f\n",eninterp[2]);

  /* find xinterp */

  xinterp[1]=tanx;
  if (small_x_ind<xy_uniq-1) {
    xinterp[0]=xycol[small_x_ind];
    xinterp[2]=xycol[small_x_ind+1];
  }
  else {
    xinterp[0]=xycol[small_x_ind-1];
    xinterp[2]=xycol[small_x_ind];
  }

  headas_chat(5,"Three tanx values to interpolate between:\n");
  headas_chat(5,"    lower: %f\n",xinterp[0]);
  headas_chat(5,"    middle: %f\n",xinterp[1]);
  headas_chat(5,"    upper: %f\n",xinterp[2]);
  
  /* find yinterp */

  yinterp[1]=tany;
  if (small_y_ind<xy_uniq-1) {
    yinterp[0]=xycol[small_y_ind];
    yinterp[2]=xycol[small_y_ind+1];
  }
  else {
    yinterp[0]=xycol[small_y_ind-1];
    yinterp[2]=xycol[small_y_ind];
  }

  headas_chat(5,"Three tany values to interpolate between:\n");
  headas_chat(5,"    lower: %f\n",yinterp[0]);
  headas_chat(5,"    middle: %f\n",yinterp[1]);
  headas_chat(5,"    upper: %f\n",yinterp[2]);

  /* read in chebyshev coefficents for all 8 rows (coeff_interp) */
 
  for (row_ind=0;row_ind<8;row_ind++) {
    for (col_ind=4;col_ind<n_vectors+4;col_ind++) {
      if (fits_read_col(fits_file,TFLOAT,col_ind,interp_rownums[row_ind],1,
	    vector_widths[col_ind-4],&nulval,coeff_temp,&anynul,&status)) {
	fits_report_error(stderr,status);
	exit(status);
      }
      for (i=0;i<n_coeffs;i++)
	coeff_interp[row_ind*n_vectors*n_coeffs+(col_ind-4)*
	  n_coeffs+i]=coeff_temp[i];
    }
  }

  /* expand chebyshev coefficients (coeff_interp) */

  for (i=0;i<N_DEPTHS;i++) depths[i]=0.0001+i*0.0002;
  for (i=0;i<8;i++) {
    coeff_pointer=coeff_interp+i*n_vectors*n_coeffs;
    dist_pointer=distinterp+i*n_vectors*N_DEPTHS;
    status2=expand_dist(n_coeffs,n_vectors-ignore_vectors,N_DEPTHS,
        coeff_pointer,depths,dist_pointer);
  }

  /* report totals */

  for (row_ind=0;row_ind<8;row_ind++) {   /* each set of distributions */
    temp_sum2=0;
    for (col_ind=0;col_ind<n_vectors;col_ind++) { /* each vector */
      temp_sum=0;
      for (i=0;i<N_DEPTHS;i++)                    /* each depth */
        temp_sum+=distinterp[(row_ind*n_vectors+col_ind)*N_DEPTHS+i];
      headas_chat(5," dist total (row %d, vector %d): %f\n",
	  row_ind,col_ind,temp_sum); 
      headas_chat(5,"   effective area (dist total * dx): %f\n",
	  temp_sum*DET_THICKNESS/N_DEPTHS); 
      temp_sum2+=temp_sum;
    }
    headas_chat(5," dist total (row %d, all vectors):%f\n",
        row_ind,temp_sum2); 
    headas_chat(5,"   effective area (dist total * dx): %f\n",
	temp_sum2*DET_THICKNESS/N_DEPTHS); 
  }

  /* free memory */

  free (encol);
  free (xcol);
  free (ycol);
  free (xycol);

  /* return */

  headas_chat(5,"...Leaving get_distinterp...\n");
  return 0;

}

      
int interp_params(float *eninterp, float *xinterp, float *yinterp,
    float *parinterp, int n_elements, int n_vectors, 
    float *param_vector) {

  int i,j;
  float interp_weights[8];
  double temp_sum,temp_sum2;
 
  headas_chat(5,"...Inside interp_params...\n");

  /* initialize param_vector */

  for (i=0;i< n_elements*n_vectors;i++) param_vector[i]=0;

  /* create weights used for interpolation */

  interp_weights[0]=
    (eninterp[2]-eninterp[1])*
    (xinterp[2]-xinterp[1])*
    (yinterp[2]-yinterp[1])/
    ((eninterp[2]-eninterp[0])*
     (xinterp[2]-xinterp[0])*
     (yinterp[2]-yinterp[0]));
  interp_weights[1]=
    (eninterp[1]-eninterp[0])*
    (xinterp[2]-xinterp[1])*
    (yinterp[2]-yinterp[1])/
    ((eninterp[2]-eninterp[0])*
     (xinterp[2]-xinterp[0])*
     (yinterp[2]-yinterp[0]));
  interp_weights[2]=
    (eninterp[2]-eninterp[1])*
    (xinterp[1]-xinterp[0])*
    (yinterp[2]-yinterp[1])/
    ((eninterp[2]-eninterp[0])*
     (xinterp[2]-xinterp[0])*
     (yinterp[2]-yinterp[0]));
  interp_weights[3]=
    (eninterp[1]-eninterp[0])*
    (xinterp[1]-xinterp[0])*
    (yinterp[2]-yinterp[1])/
    ((eninterp[2]-eninterp[0])*
     (xinterp[2]-xinterp[0])*
     (yinterp[2]-yinterp[0]));
  interp_weights[4]=
    (eninterp[2]-eninterp[1])*
    (xinterp[2]-xinterp[1])*
    (yinterp[1]-yinterp[0])/
    ((eninterp[2]-eninterp[0])*
     (xinterp[2]-xinterp[0])*
     (yinterp[2]-yinterp[0]));
  interp_weights[5]=
    (eninterp[1]-eninterp[0])*
    (xinterp[2]-xinterp[1])*
    (yinterp[1]-yinterp[0])/
    ((eninterp[2]-eninterp[0])*
     (xinterp[2]-xinterp[0])*
     (yinterp[2]-yinterp[0]));
  interp_weights[6]=
    (eninterp[2]-eninterp[1])*
    (xinterp[1]-xinterp[0])*
    (yinterp[1]-yinterp[0])/
    ((eninterp[2]-eninterp[0])*
     (xinterp[2]-xinterp[0])*
     (yinterp[2]-yinterp[0]));
  interp_weights[7]=
    (eninterp[1]-eninterp[0])*
    (xinterp[1]-xinterp[0])*
    (yinterp[1]-yinterp[0])/
    ((eninterp[2]-eninterp[0])*
     (xinterp[2]-xinterp[0])*
     (yinterp[2]-yinterp[0]));

  for (i=0;i<8;i++)
    headas_chat(5,"interp_weights[%d]: %f\n",i,interp_weights[i]);

  /* interpolate to find the new parameter vector (param_vector) */

  for (i=0;i<8;i++) {
    for (j=0;j<  n_elements * n_vectors;j++)
      param_vector[j]+=interp_weights[i]*parinterp[i*n_elements*n_vectors+j];
  }

  /* report totals */

  temp_sum2=0;
  for (i=0;i<n_vectors;i++) {
    temp_sum=0;
    for (j=0;j<n_elements;j++)
      temp_sum+=param_vector[i*n_elements+j];
    headas_chat(4," dist total (vector %d):%f\n",
        i,temp_sum); 
    headas_chat(4,"   effective area (dist total * dx): %f\n",
	temp_sum*DET_THICKNESS/N_DEPTHS); 
    temp_sum2+=temp_sum;
  }
  headas_chat(4," dist total (all vectors):%f\n",
      temp_sum2); 
  headas_chat(4,"   effective area (dist total * dx): %f\n",
      temp_sum2*DET_THICKNESS/N_DEPTHS); 

  headas_chat(5,"...Leaving interp_params...\n");
  return 0;

}


int lookup_params(float energy, float tanx, float tany, 
    fitsfile *fits_file, float **param_vector, int *n_elements, 
    int *n_vectors) {

/* function: lookup_params
 *
 * Returns a vector of parameters or depth distribution values
 * (depending on the type of fits file passed to it)
 *
 * Inputs:
 *
 *   energy:        incident photon energy (keV)
 *   tanx:          tan(theta_x) for incident photons 
 *   tany:          tan(theta_y) for incident photons 
 *   fits_file:     pointer to a fitsfile structure
 *                    (either a parameter fits file or a depth distribution
 *                    fits file)
 *
 * Ouput:
 *   param_vector:  a one-dimensional array of size n_vectors*n_elements
 *                  giving the parameters of depth distribution values
 *                  for the givin energy, tanx, and tany
 *                    element number is the most rapidly varying index
 *                    vector number is the least rapidly varying index
 *                  (Memory for this array is allocated inside this function)
 *   n_elements:    the number of elements in each vector
 *   n_vectors:     the number of vectors in param_vector
 *
 * FOR NOW, THIS FUNCTION ONLY WORKS WITH DEPTH DISTRIBUTION FITS FILES.
 * IF A PARAMETER FITS FILE IS PASSED TO IT, IT RETURNS WITH A STATUS OF 1
 */

  float eninterp[3];
  float xinterp[3];
  float yinterp[3];
  float distinterp[8*N_PEAKS*N_DEPTHS];
  char file_type_string[20];
  char comment[100];
  int status=0;
  int hdutype;
  float *dist_vector;
  int i;

  headas_chat(5,"...Inside lookup_params...\n");

  /* Safety Measure:
   *   If the incident photon energy lies very near the
   *   Cd or Te K edge, it must be rounded up or down to the values used
   *   in the depth distribution table */

  if ((energy>26.71)&&(energy<=26.711)) energy=26.71;
  if ((energy>26.711)&&(energy<26.72)) energy=26.72;
  if ((energy>31.81)&&(energy<=31.813)) energy=31.81;
  if ((energy>31.813)&&(energy<31.82)) energy=31.82;

  /* initialize distinterp to zero */

  for (i=0;i<8*N_PEAKS*N_DEPTHS;i++) distinterp[i]=0;

  /* move to extension 1 of depth distribution fits file */

  if (fits_movabs_hdu(fits_file, 2, &hdutype, &status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* get the file type */

  if (fits_read_key(fits_file, TSTRING, "CCNM0001", file_type_string, 
	comment, &status)) {
    fits_report_error(stderr,status);
    exit(status);
  }

  /* if the file is a depth distribution fits file */

  if (strcmp(file_type_string,"DEPTH_DIST")==0) {

    /* get eninterp, xinterp, yinterp, and distinterp */

    if (get_distinterp(energy,tanx,tany,fits_file,eninterp,xinterp,
	  yinterp,distinterp)) {
      fprintf(stderr,"ERROR:  get_distinterp failed\n");
      return 1;
    }
 
    /* allocate memory for dist_vector */

    dist_vector=(float *)malloc(N_DEPTHS*N_PEAKS*sizeof(float));

    /* get dist_vector */

    if (interp_params(eninterp,xinterp,yinterp,distinterp,N_DEPTHS,
	  N_PEAKS,dist_vector)) {
      fprintf(stderr,"ERROR:  interp_params failed\n"); 
      return 2;
    }

    /* assign the address of dist_vector to param_vector 
     * and fill n_elements and n_vectors */

    *param_vector=&dist_vector[0];
    *n_vectors=N_PEAKS;
    *n_elements=N_DEPTHS;

  }
  else {
    fprintf(stderr,"ERROR:  depthfile CCNM0001 keyword not DEPTH_DIST\n"); 
    return 1;
  }

  headas_chat(5,"...Leaving lookup_params...\n");
  return 0;

}


