#ifndef _HXD_TRN_FITS_TO_BNK_UTIL_H_
#define _HXD_TRN_FITS_TO_BNK_UTIL_H_

#include <fitsio.h>
#include "hxdtrnFitsUtil.h"

void hxdtrnFitsToBnk_init();

void hxdtrnFitsToBnk_put( HxdTrnFits *fits );

void hxdtrnFitsToBnk_get( HxdTrnFits *fits );

#endif
