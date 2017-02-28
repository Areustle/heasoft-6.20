#include "xrs_cornercoord.h"
#include "math.h"

/******************************************************************
* Xrsimage assumes that xrscoord has been applied to the input event
* file; X (RA), Y (Dec), ROLL are read from fits table, which should be
* calculated by xrscoord.  You should re-run xrscoord when you need to
* change the center of image, pointing, etc.  Note that xrsimage just
* make image fits according to the information in the input event file.
* In the current version of xrsimage, calibration pixel or dead pixel
* is not treated appropreately.  This problem should be fixed in future
* version.
******************************************************************/

/* enum { DET = 0, FOC = 1, SKY = 2 }; */

/******************************************************************
* xrs_cornercoord is used when iamge_type is DET or FOC.
* This routine calculates the corner pixels with teldef file,
******************************************************************/

int xrs_cornercoord (TELDEF *teldef, int pixel, int type, 
			  double x[], double y[]) {
  int i;
  double tmpx, tmpy;
  /* calculate DETX, DETY from pixel */
  for (i=0; i<4; i++) {
    xrs_pixel2det(teldef, pixel, i+1, &x[i], &y[i]);
  }  
  if(0 == type) { /* det */
    x[4]=x[0];  y[4]=y[0];
    return 0;
  } 

  /* calculate FOCX, FOCY from DETX, DETY */
  for (i=0; i<4; i++) {
    tmpx = x[i];  tmpy = y[i];
    aste_det2foc(teldef, tmpx, tmpy, &x[i], &y[i]);
  } 
  if(1 == type) {  /* foc */
    x[4]=x[0];  y[4]=y[0];
    return 0;
  }
}

  
/******************************************************************
* xrs_skycornercoord is used for image_type is SKY.
* In this routine, corner pixels are calculated
* with X, Y and ROLL parameters written in the input event file:
* 
* The output image in SKY coordinate may NOT be accurate.  One reason is
* xrsimage uses X, Y and ROLL values, which were truncated to INTEGER.
* Xrsimage assumes image of each XRS pixel is square (or rectangle)
* similar to FOC pixel, which is not correct if detector is located far
* from the center of a whole image.
******************************************************************/

int xrs_skycornercoord (TELDEF *teldef, int pixel, double centx, 
		      double centy, double roll, double x[], double y[]) {
  int i;
  double tmpx, tmpy;
  double focx[5], focy[5], dfocx[4], dfocy[4];
  double radroll = roll * PI / 180.;

  /* calculate FOCX, FOCY of each corner */
  for (i=0; i<5; i++) {
    xrs_pixel2det(teldef, pixel, i, &tmpx, &tmpy);
    aste_det2foc(teldef, tmpx, tmpy, &focx[i], &focy[i]);
  }  

  /* calculate vectors of FOC between each corner and the center */ 
  for (i=0; i<4; i++) {
    dfocx[i] = focx[i+1] - focx[0];
    dfocy[i] = focy[i+1] - focy[0];
  }  


  /* calculate X, Y of corner from X, Y, ROLL and FOCX, FOCY */

  /*     ( cos -ROLL  -sin -ROLL )    ( dfocx[i] )   ( X[0] )   ( X[i] )  */
  /*     (                       ) x  (          ) + (      ) = (      )  */
  /*     ( sin -ROLL   cos -ROLL )    ( dfocy[i] )   ( Y[0] )   ( X[i] )  */
/*  where i=1..4 indicates each corner, (dfocx[i], dfocy[i]) is a vector */
/*  from a corner pixel to the center pixel in FOC coordinate, (X[0], */
/*  Y[0]) is a position of pixel center, (X[i], Y[i]) is a position of a */
/*  corner. */

  /* Note ROLL is defined as an angle of the FOCY, CCW from north */

  for (i=0; i<4; i++) {
    x[i] =  (cos(-radroll) * dfocx[i] - sin(-radroll) * dfocx[i] )
      + centx;
    y[i] =  (sin(-radroll) * dfocy[i] + cos(-radroll) * dfocy[i] )
      + centy;

/*      centx = (int)(focx[0] + 0.5); */
/*      centy = (int)(focy[0] + 0.5); */
/*      x[i] =  dfocx[i] + centx; */
/*      y[i] =  dfocy[i] + centy; */
  }  
  x[4] = x[0]; y[4] = y[0];
  return 0;
}

