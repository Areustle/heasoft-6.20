#ifndef WCS_INCLUDED

#include "coord.h"
#include "fitsio.h"

typedef struct {

XFORM2D* trans;
double xref;
double yref;

char xunit[FLEN_VALUE];
char yunit[FLEN_VALUE];

char xtype[FLEN_VALUE];
char ytype[FLEN_VALUE];


} WCS;

/****************************************************************************
* create a set of WCS keywords with default values. The defaults are
* an identity transform with the reference pixel at (0,0).
****************************************************************************/
WCS* allocateWCS();

/**************************************************************************
*
**************************************************************************/
void destroyWCS(WCS* wcs);

/**************************************************************************
* Create a new WCS structure which describes the world coordinates for
* a given set of coordinates. Note this  function leaves the reference
* pixel set to (0,0).
**************************************************************************/
WCS* createWCSForCoordDef(COORDDEF* coord, double center_x, double center_y);

/******************************************************************************
* Modify the WCS in order to account for a transform applied to the pixels.
* This transforms the reference point as well as the transform. The units
* and coordinate types remain unchanged
******************************************************************************/
void transformWCSPixels(WCS* wcs, XFORM2D* trans);

/******************************************************************************
* Read a two-dimensional set of WCS keywords. This method can read
* wither the PC or CD matrix conventions, or the plain old
* CDELT. This function ignores the CROTA keywords.
******************************************************************************/
WCS* readWCS(fitsfile* fp, char* suffix);

/************************************************************************
* Write a set of WCS keywords. This function uses the PC matrix convention.
* However, it will not write a matrix value if it is more than
* WCS_MATRIX_TOLERANCE different from the default identity matrix.
************************************************************************/
int writeWCS(WCS* wcs, fitsfile* fp, char* suffix);

#define WCS_INCLUDED
#endif /* WCS_INCLUDED */
