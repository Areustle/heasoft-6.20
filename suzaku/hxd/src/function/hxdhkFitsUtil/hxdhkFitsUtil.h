#ifndef _HXD_HK_FITS_UTIL_H_
#define _HXD_HK_FITS_UTIL_H_

#include <fitsio.h>

#define HXD_HK_FITS_FILELIST_MAXNUM 10

#define HXD_HK_FITS_FILETYPE_HK 0
#define HXD_HK_FITS_FILETYPE_TBL 1
#define HXD_HK_FITS_FILETYPE_HST 2
#define HXD_HK_FITS_FILETYPE_DMP 3
#define HXD_HK_FITS_FILETYPE_TRN 4
#define HXD_HK_FITS_FILETYPE_WEL 5

#define HXD_HK_FITS_SORT 1
#define HXD_HK_FITS_NO_SORT 0

typedef struct {
  fitsfile *fptr[HXD_HK_FITS_FILELIST_MAXNUM];
  int filetype[HXD_HK_FITS_FILELIST_MAXNUM];
  int filenum;
} HxdHkFits;

void hxdtableFits_init( char *filelistname, int sort_flag, HxdHkFits *fits,
		       int *istat );

void hxdtableFits_read( int *filenum, int *eventtype, int *irow, int *istat );

#endif
