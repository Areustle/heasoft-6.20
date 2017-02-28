#include "fitsio.h"
#include "param.h"
#include "image.h"

/***************************************************************************
* a few misc "global" things
****************************************************************************/
#define VERSION_ID "0.1"
#define VERSION "xrsimage " VERSION_ID

#define TELDEF_NOT_EXIST 100
/*********************************
* high level function prototypes *
*********************************/
IMAGE* xrs_det_image(PARAM* param, fitsfile* fp, int* iserror);
IMAGE* xrs_sky_image(PARAM* param, fitsfile* fp, int* iserror);
