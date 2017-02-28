#ifndef _HXD_TABLE_FITS_UTIL_H_
#define _HXD_TABLE_FITS_UTIL_H_

#include <fitsio.h>

#define HXD_TABLE_FITS_FILELIST_MAXNUM 16

typedef struct {
  fitsfile *fptr[HXD_TABLE_FITS_FILELIST_MAXNUM];
} HxdTableFits;

void hxdtableFits_Init( char *filelist, HxdTableFits *fits, int *istat);

void hxdtableFits_col_register( char *colname, HxdTableFits *fits,
			       int *index, int *istat);

void hxdtableFits_get_col_int( HxdTableFits *fits, char *colname, int index,
			      int irow, int *value, int *istat );

void hxdtableFits_get_col_dbl( HxdTableFits *fits, char *colname, int index,
			      int irow, double *value, int *istat );

void hxdtableFits_get_key_str( HxdTableFits *fits, char *keyname, char *value,
			      int *istat  );

void hxdtableFits_get_key_lng( HxdTableFits *fits, char *keyname, long *value,
			      int *istat );

void hxdtableFits_get_key_dbl( HxdTableFits *fits, char *keyname,
			      double *value, int *istat );

#endif
