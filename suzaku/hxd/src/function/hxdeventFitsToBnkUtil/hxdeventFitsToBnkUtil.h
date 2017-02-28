#ifndef _HXD_EVENT_FITS_TO_BNK_UTIL_H_
#define _HXD_EVENT_FITS_TO_BNK_UTIL_H_
#include "hxdeventFitsUtil.h"

void hxdeventFitsToBnk_init();

void hxdeventFitsToBnk_put( HxdEventFits02 *fits );
void hxdeventFitsToBnk_get( int board, HxdEventFits02 *fits );
#define HXD_WEL_LENGTH 16
#endif
