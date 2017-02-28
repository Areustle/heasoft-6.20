/*
 *  suzaku_psf.c
 *  Calculate psf for Suzaku XRT
 *  Used by npsf.f as REAL FUNCTION
 */

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "cfortran.h"

#define MAX_INSTRUME 4
#define MAX_CONSTS 6
/*
 * Empirical analytic model for PSF
 * From MCG-6-30-15 data taken in very early phase of Suzaku
 *
 * MODEL = EXP1 + GAUSS1 + EXP1
 * 
 * EXP1   = EN1*EXP(-X/EW1)
 * GAUSS1 = GN1*EXP(-Z*Z/2.),
 *  where Z=X/GW1
 * EXP2   = EN2*EXP(-X/EW2)
 * 
 */
   static float c[MAX_INSTRUME][MAX_CONSTS] = {
   /* EW1  | EN1  | GW1  | GN1   | EW2  | EN2    */
      0.221, 1.101, 0.970, 0.0919, 1.937, 0.00539, /* XIS0 */
      0.176, 1.006, 1.047, 0.0929, 1.937, 0.00539, /* XIS1 */
      0.172, 1.082, 0.927, 0.1175, 1.937, 0.00539, /* XIS2 */
      0.218, 0.957, 1.014, 0.0891, 1.937, 0.00539  /* XIS3 */
   };

float suzaku_norm(char *instrume, int *status) {
/*
 *  Returns PSF normalization factor
 *
 *  I  instrume (s) Instrument name (XIS0, XIS1, XIS2, XIS3)
 *  O  status   (i) Error flag (0 = OK)
 */
   int i;
   float norm;

   /* Instrument index */
   i = atoi(&instrume[strlen(instrume)-1]);
   if ( i < 0 || i >= MAX_INSTRUME ) {
      *status = -1;
      return 1.0;
   }

   /* 2pi(EN1*EW1^2 + GN1*GW1^2 + EN2*EW2^2) */
   /*norm = 2.0*myPI*(c[i][0]*c[i][1]*c[i][1] + c[i][2]*c[i][3]*c[i][3] + 
                    c[i][4]*c[i][5]*c[i][5]);*/
   norm = 2.0*myPI*(c[i][0]*c[i][0]*c[i][1] + c[i][2]*c[i][2]*c[i][3] + 
                    c[i][4]*c[i][4]*c[i][5]);
   /*
    *  Convert to arcsec for integration in NPSF routine
    */
   norm = norm*3600.0;

   return norm;
}
FCALLSCFUN2(FLOAT,suzaku_norm,SUZAKU_NORM,suzaku_norm,STRING,PINT)

float suzaku_psf(char *instrume, float offaxis, float radius, int *status) {
/*
 *  Returns value of PSF at a particular radius
 *
 *  I  instrume (s) Instrument name (XIS0, XIS1, XIS2, XIS3)
 *  I  offaxis  (r) Off-axis angle in arcmin
 *  I  radius   (r) Distance from target position in arcsec
 *  O  status   (i) Error flag (0 = OK)
 */
   float retval = 1.0;
   double r, z;
   int i;

   *status = 0;
   
   /* Instrument index */
   i = atoi(&instrume[strlen(instrume)-1]);
   if ( i < 0 || i >= MAX_INSTRUME ) {
      *status = -1;
      return retval;
   }
   /*
    *  Routine expects radius in arcmin, not arcsec as npsf passes
    */
   r = radius/60.;

   z = r/c[i][2];
   retval = c[i][1]*exp(-r/c[i][0]) + c[i][3]*exp(-z*z/2.) +
	    c[i][5]*exp(-r/c[i][4]);

   return retval;

   *status = -1;
   return 1.0;
}
FCALLSCFUN4(FLOAT,suzaku_psf,SUZAKU_PSF,suzaku_psf,STRING,FLOAT,FLOAT,PINT)
