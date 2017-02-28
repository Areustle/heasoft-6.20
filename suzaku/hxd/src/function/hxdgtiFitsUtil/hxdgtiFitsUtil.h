#ifndef _HXD_GTI_FITS_UTIL_H_
#define _HXD_GTI_FITS_UTIL_H_

#include "hxdFitsHeaderUtil.h"

#define GTI_HDU_NFIELDS 2 /* gti HDU column number */

/* is parameter has to be tested  in the future */
#define GTI_GAP_TOLERANCE 24

typedef struct {
  double *start;
  double *stop;
  double ontime;
  int row;
} HXD_GTI;

int hxdgtiFits_createGTIxtention( fitsfile *fp, int hdunum, HXD_GTI,
				 HXD_STD_KEYS, int *status);

int hxdgtiFits_updateGTI( HXD_GTI *, double stop, double start );

int hxdgtiFits_finalizeGTI ( HXD_GTI *, double start, double stop );

int hxdgtiFits_readGTI ( fitsfile *fp, int hdunum, HXD_GTI *, int *status );


/* not used */
int hxdgtiFits_checkGTI ( HXD_GTI *, double aetime );


/**** add ****/

int hxdgtiFits_UpdateOntime ( HXD_GTI * );

#endif
