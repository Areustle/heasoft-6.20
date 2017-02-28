#ifndef _HXD_BST_FITS_TO_BNK_UTIL_H_
#define _HXD_BST_FITS_TO_BNK_UTIL_H_

#include <fitsio.h>
#include "hxdbstFitsUtil.h"

#define HXDBSTFITS_WPU_N_BOARD 4
#define HXDBSTFITS_TPU_N_BOARD 4
#define HXDBSTFITS_N_TANT 5

void hxdbstFitsToBnk_init();

void hxdbstFitsToBnk_put( HxdBstFits *fits );

void hxdbstFitsToBnk_get( HxdBstFits *fits );

#endif
