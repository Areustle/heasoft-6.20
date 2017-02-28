/*
 *  swift_psf.c
 *  Calculate psf for Swift XRT
 *  Used by npsf.f as REAL FUNCTION
 *  
 *  HISTORY:  AP Fixed incorrect interpolation of coefficients
 *               Should have had E/offaxis * 10.0
 */

#include <math.h>
#include "fitsio.h"
#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "cfortran.h"

PROTOCCALLSFFUN3(INT,DETIDX,detidx,STRING,STRING,STRING)
#define DETIDX(A,B,C) CCALLSFFUN3(DETIDX,detidx,STRING,STRING,STRING,A,B,C)

#define LKUPCAL(A,B,C,D) CCALLSFSUB4(LKUPCAL,lkupcal,INT,STRING,PSTRING,PINT,A,B,C,D)

#define NCOEFF 4
static int loaded = 0;
static float par[NCOEFF][NCOEFF];

float swift_psf(float energy, float offaxis, float radius, int *status) {
/*
 *  I  energy  (r) Energy in keV
 *  I  offaxis (r) Off-axis angle in arcmin
 *  I  radius  (r) Distance from target position in arcsec
 *  O  status  (i) Error flag (0 = OK)
 */
   float retval = 1.0;
   float rad, c[NCOEFF], totflux, fg, fk, scl_ene, scl_off;
   static float old_energy = 0.0;
   static float old_offaxis = 0.0;
   int i, icol, fstat, hdutype, print_coeffs;
   char constfile[MAX_FILELEN];
   char buf[100];
   fitsfile *fptr = NULL;

   *status = 0;

   /*
   ** Only print the coefficients if the energy or offset has changed
   */
   if ( energy != old_energy || offaxis != old_offaxis ) {
       print_coeffs = 1;
       old_energy = energy;
       old_offaxis = offaxis;
   } else {
       print_coeffs = 0;
   }
   /* 
   ** coefficients in CALDB are defined with energy and offset * 10 
   ** for some reason
   */
   scl_ene = 10.0;
   scl_off = 10.0;
   energy *= scl_ene;
   offaxis *= scl_off;
   
   /*
    *  Cache calibration values to avoid repeated reads
    */
   if ( !loaded ) {
      strcpy(constfile, "");
      fstat = 0;
      LKUPCAL(DETIDX("SWIFT","XRT",""), "psfconst_*.fits", constfile, fstat);
      *status = fstat;
      if ( *status != 0 ) goto psfbad;

      /*
       *  Open calibration file
       */
      fits_open_file(&fptr, constfile, READONLY, status);
      fits_movrel_hdu(fptr, 1, &hdutype, status);
      if ( *status != 0 ) {
         cxwrite("swift_psf: No extension found in calibration file", 5);
         cxwrite(constfile, 10);
         goto psfbad;
      }
      /* Read columns */
      for ( i = 0; i < NCOEFF; i++ ) {
         icol = i;
         icol++; /* Column numbers start at 1 */
         icol++; /* Skip column 1 as it contains only P# names */
         fits_read_col(fptr, TFLOAT, icol, 1, 1, NCOEFF, NULL,
                       &par[i][0], NULL, status);
      }
      if ( *status != 0 ) {
         cxwrite("swift_psf: Failed to read columns from calibration file", 5);
         goto psfbad;
      }
      fits_close_file(fptr, status);
      *status = 0;
      loaded = 1;

   }
   /*
    *  Routine expects radius in original pixels, not arcsec as npsf
    *    passes
    */
   rad = radius/2.36;

   for ( i = 0; i < NCOEFF; i++ ) {
      c[i] = par[0][i] + offaxis*par[1][i] + energy*par[2][i]
             + offaxis*energy*par[3][i];
      if ( print_coeffs == 1 ) {
          sprintf( &buf[0], 
                  "swift_psf: For (energy,offaxis)=(%f,%f), Param. %d = %1.6e",
                  energy / scl_ene, offaxis / scl_off, i, c[i] );
      
          cxwrite( buf, 15 );
      }
   }

   if ( (c[3]-1.) == 0. ) {
      cxwrite("Trapped division by zero (1) in swiftxrt_psf", 10);
      goto psfbad;
   }
      
   totflux=2.*myPI*c[0]*c[1]*c[1]+myPI*c[2]*c[2]*(1.-c[0])/(c[3]-1.);

   if ( c[2] == 0. || c[1] == 0. ) {
      cxwrite("Trapped division by zero (2) in swiftxrt_psf", 10);
      goto psfbad;
   }
   fg=2.*myPI*c[0]*c[1]*c[1]*
             (1.-exp(-1.*rad*rad/2./(c[1]*c[1])));

   fk=myPI*c[2]*c[2]*(1.-c[0])/(1.-c[3])*
          (pow((1.+(rad/c[2])*(rad/c[2])),(1.-c[3]))-1.);

   if ( totflux == 0. ) {
      cxwrite("Trapped division by zero (3) in swiftxrt_psf", 10);
      goto psfbad;
   }
   retval = (fg + fk)/totflux;

   return retval;

psfbad:
   *status = -1;
   return 1.0;
}
FCALLSCFUN4(FLOAT,swift_psf,SWIFT_PSF,swift_psf,FLOAT,FLOAT,FLOAT,PINT)
