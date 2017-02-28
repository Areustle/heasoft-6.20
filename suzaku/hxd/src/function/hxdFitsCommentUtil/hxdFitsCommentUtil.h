#ifndef _HXD_FITS_COMMENT_UTIL_H_
#define _HXD_FITS_COMMENT_UTIL_H_

#include <fitsio.h>

void hxdFitsComment_write ( fitsfile *fp, int comment_num, char **keyword,
			   char **comment, int *istat);

void hxdFitsComment_write_single ( fitsfile *fp, char *keyword,
				  char *comment, int *istat);

#endif
