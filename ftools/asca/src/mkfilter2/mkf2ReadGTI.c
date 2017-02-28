/* FTOOLS info $Header: /headas/headas/ftools/asca/src/mkfilter2/mkf2ReadGTI.c,v 1.1 1998/03/10 04:56:12 guerber Exp $ */

/* mkf2ReadGTI: Read ALLGTI list.
 * Jeff Guerber, RSTX/GSFC, Feb. 1998
 */

#include <stdio.h>
#include <string.h>
#include "mkfilter.h"
#include "cfitsio.h"
#include "mkf2_recd.h"

void mkf2ReadGTI ( int unit, GtiList *gti, int *status )

{
  int hdutype, ngti, anyflag;
  char extkey[] = "EXTNAME";     /* extension name keyword */
  char naxis2key[] = "NAXIS2";   /* keywd for number of rows */
  char extname[FITS_CLEN_STRVAL], comment[FITS_CLEN_COMMENT];

  /* initialize the elements of the gti structure */
  gti->ngti = 0;
  gti->start = NULL;
  gti->stop = NULL;

  /* Position to 2nd extension (HDU 3), assume this is where ALLGTI
   * will be.  If there's no such extension, assume that it's all OK.
   * Better would be to loop over the extensions. */
  FCMAHD( unit, 3, &hdutype, status);
  if (*status == 107) {      /* EOF */
    c_fcecho("No ALLGTI found, assuming all times good");
    *status = 0;
    return;
  }

  /* check that this is an ALLGTI extension */
  FCGKYS( unit, extkey, extname, comment, status );
  if (strcmp(extname, "ALLGTI") != 0) {
    c_fcerr("mkf2ReadGTI: expected ALLGTI extension isn't one");
    *status = 1;
    return;
  }

  /* read the number of rows (NAXIS2) */
  FCGKYJ( unit, naxis2key, &ngti, comment, status );
  if (ngti != 0 && *status == 0) {  /* not an error if ngti==0, fall through */

    /* allocate the start and stop arrays */
    gti->ngti = ngti;
    gti->start = malloc( gti->ngti * sizeof (double) );
    gti->stop  = malloc( gti->ngti * sizeof (double) );

    if (gti->start == NULL || gti->stop == NULL) {
      c_fcerr("mkf2ReadGTI: could not malloc array space!");
      *status = 2;
      return;
    }

    /* read the arrays; assume standard GTI structure: double precision
     * start times in column 1, stop times in column 2 */

    FCGCVD( unit, 1, 1, 1, ngti, 0.0e0, gti->start, &anyflag, status );
    FCGCVD( unit, 2, 1, 1, ngti, 0.0e0, gti->stop, &anyflag, status );

    /* run through GTICLEAN to be sure arrays are ordered and don't
     * overlap; put cleaned arrays in same space */

    GTICLEAN( gti->start, gti->stop, &gti->ngti, gti->ngti,
	      gti->start, gti->stop, gti->ngti, status );

  }
  return;
}
