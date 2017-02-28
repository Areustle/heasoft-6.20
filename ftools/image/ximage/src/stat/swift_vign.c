/*
 *  swift_vign.c
 *  Calculate vignetting for Swift XRT
 *  Used by get_vign.f
 */

#include <math.h>
#include "fitsio.h"
#include "../include/maxvals.h"
#include "../include/xcommon.h"
#include "cfortran.h"

PROTOCCALLSFFUN3(INT,DETIDX,detidx,STRING,STRING,STRING)
#define DETIDX(A,B,C) CCALLSFFUN3(DETIDX,detidx,STRING,STRING,STRING,A,B,C)

#define LKUPCAL(A,B,C,D) CCALLSFSUB4(LKUPCAL,lkupcal,INT,STRING,PSTRING,PINT,A,B,C,D)

#define NCOEFF 3
static int loaded = 0;
static float par[NCOEFF];

void swift_vign(float offaxis, float energy, float *vcor, int *status) {
/*
 *  I  offaxis (r) Off-axis angle in arcmin
 *  I  energy  (r) Energy in keV
 *  O  vcor    (r) Vignetting correction
 *  O  status  (i) Error flag (0 = OK)
 */
   float vcoeff;
   int i, icol, fstat, hdutype;
   char constfile[MAX_FILELEN];
   fitsfile *fptr;

   *status = 0;

   /*
    *  Cache calibration values to avoid repeated reads
    */
   if ( !loaded ) {
      strcpy(constfile, "");
      fstat = 0;
      LKUPCAL(DETIDX("SWIFT","XRT",""), "vignconst_*.fits", constfile, fstat);
      *status = fstat;
      if ( *status != 0 ) goto vignbad;

      /*
       *  Open calibration file
       */
      fits_open_file(&fptr, constfile, READONLY, status);
      fits_movrel_hdu(fptr, 1, &hdutype, status);
      if ( *status != 0 ) {
         cxwrite("swift_vign: No extension found in calibration file", 5);
         cxwrite(constfile, 10);
         goto vignbad;
      }
      /* Read columns */
      for ( i = 0; i < NCOEFF; i++ ) {
         icol = i;
         icol++; /* Column numbers start at 1 */
         fits_read_col(fptr, TFLOAT, icol, 1, 1, 1, NULL, &par[i],
                       NULL, status);
      }
      if ( *status != 0 ) {
         cxwrite("swift_vign: Failed to read columns from calibration file", 5);
         goto vignbad;
      }
      fits_close_file(fptr, status);
      *status = 0;
      loaded = 1;
   }

   vcoeff = par[0]*pow(par[1],energy) + par[2];
   *vcor = 1. - vcoeff*offaxis*offaxis;
   if ( *vcor < 0.0 ) *vcor = 0.0;
   return;

vignbad:
   *status = -1;
   return;
}
FCALLSCSUB4(swift_vign,SWIFT_VIGN,swift_vign,FLOAT,FLOAT,PFLOAT,PINT)
