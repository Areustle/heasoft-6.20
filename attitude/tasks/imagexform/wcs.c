#include <string.h>

#include "wcs.h"
#include "headas.h"

/*****************************************************************************
*
*****************************************************************************/
WCS* allocateWCS() {

WCS* wcs;

wcs = (WCS*)malloc(sizeof(WCS));

return wcs;

} /* end of allocateWCS function */

/*****************************************************************************
*
*****************************************************************************/
WCS * createWCS(COORDDEF* coord, double crval1, double crval2, 
            double binx, double biny) {

WCS * wcs;

wcs = allocateWCS();

/*****************************************************************************
* Note: we bin so that the left edge of pixel 1 is aligned in the
* pre and post binning coordinate systems.
* This is what the extractor does.
*****************************************************************************/
wcs->crpix[0] = coord->center_x;
wcs->crpix[1] = coord->center_y;

wcs->crval[0] = crval1;
wcs->crval[1] = crval2;

wcs->cdelt[0] = coord->scale_x * binx;
wcs->cdelt[1] = coord->scale_y * biny;

strcpy(wcs->cunit, coord->scale_unit);

if(!strcmp(coord->name, "SKY") ) {
    /*************************************************
    * flip the X axis for sky coordinates to follow 
    * the convention that RA increases to the left
    *************************************************/
    wcs->cdelt[0] = -wcs->cdelt[0];

    strncpy(wcs->ctype[0],"RA---TAN", FLEN_VALUE);
    strncpy(wcs->ctype[1],"DEC--TAN", FLEN_VALUE);
} else {

    strncpy(wcs->ctype[0], coord->name_x, FLEN_VALUE);
    strncpy(wcs->ctype[1], coord->name_y, FLEN_VALUE);
}

return wcs;
} /* end of setWCS function */


/*****************************************************************************
* write the WCS values to a FITS file
*****************************************************************************/
int writeWCS(WCS* wcs, fitsfile* fp) {

int status=0;

/***************************************************************
* don't write anything if the wcs pointer is null 
* This is kind of a cheat to handle the stupid case of the
* identity transform without having to both reading the
* WCS keywords form the original file. Just lazy I guess.
* We write a message to stdout, since this could mask a bug.
**************************************************************/
if(wcs == NULL) {
    headas_chat(1, "Not updating WCS keywords");
    return 0;
}

/********
* CTYPE *
********/
fits_update_key_str(fp, "CTYPE1", wcs->ctype[0], "X axis coordinate name", &status);
fits_update_key_str(fp, "CTYPE2", wcs->ctype[1], "Y axis coordinate name", &status);
/********
* CRPIX *
********/
fits_update_key_dbl(fp, "CRPIX1", wcs->crpix[0], WCS_DECIMALS,
                    "X axis reference pixel", &status);

fits_update_key_dbl(fp, "CRPIX2", wcs->crpix[1], WCS_DECIMALS,
                    "Y axis reference pixel", &status);

/********
* CRVAL *
********/
fits_update_key_dbl(fp, "CRVAL1", wcs->crval[0], WCS_DECIMALS,
                    "Coord of X ref pixel", &status);

fits_update_key_dbl(fp, "CRVAL2", wcs->crval[1], WCS_DECIMALS,
                    "Coord of Y ref pixel", &status);

/********
* CDELT *
********/
fits_update_key_dbl(fp, "CDELT1", wcs->cdelt[0], WCS_DECIMALS,
                    "X axis increment", &status);

fits_update_key_dbl(fp, "CDELT2", wcs->cdelt[1], WCS_DECIMALS,
                    "Y axis increment", &status);

/********
* CUNIT *
********/
fits_update_key_str(fp, "CUNIT1", wcs->cunit, "X axis unit", &status);

fits_update_key_str(fp, "CUNIT2", wcs->cunit, "Y axis unit", &status);

return status;

} /* end of writeWCS method */
