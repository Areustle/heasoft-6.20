#ifndef WCS_INCLUDED

#define WCS_DECIMALS 12

#include "coordfits.h"

/********************************************************************
* this structure handles a set of FITS image WCS keywords ********************************************************************/

typedef struct {

double crpix[2];
double crval[2];
double cdelt[2];

char ctype[2][FLEN_VALUE];

char cunit[FLEN_VALUE];

} WCS;


/*****************************************************************************
*
*****************************************************************************/
WCS* allocateWCS();

/*****************************************************************************
*
*****************************************************************************/
WCS * createWCS(COORDDEF* coord, double crval1, double crval2,
            double binx, double biny);

/*****************************************************************************
* write the WCS values to a FITS file
*****************************************************************************/
int writeWCS(WCS* wcs, fitsfile* fp);


#define WCS_INCLUDED
#endif /* WCS_INCLUDED */
