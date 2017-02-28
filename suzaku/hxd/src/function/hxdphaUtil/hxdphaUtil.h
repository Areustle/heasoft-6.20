#ifndef _HXD_PHA_UTIL_H_
#define _HXD_PHA_UTIL_H_
/*
 * hxdphaUtil
 *      to make PHA and PI spectrum FITS.
 *      v0.0.1, created by Y.Terada
 *
 */
#include "hxdFitsHeaderUtil.h"
#include "hxdgtiFitsUtil.h"

#define HXDPHAUTIL_OK 1
#define HXDPHAUTIL_NG 0

#define HXDPHAUTIL_MAX_CHANNEL 4096

typedef struct {
  /** spectrum extension **/
  int counts[HXDPHAUTIL_MAX_CHANNEL];
  int detchans;
} HxdPha;

#define HXDPHAUTIL_PRIMARY_HDU 1
#define HXDPHAUTIL_SPECTRU_HDU 2
#define HXDPHAUTIL_GTI_HDU     3

int hxdphaUtil_create_FITS( char * fitsname, HXD_STD_KEYS header,
			    double exposure, char* chantype,
			    int detchans);
int hxdphaUtil_write_FITS ( HxdPha *hxdpha, HXD_STD_KEYS header, 
			    HXD_GTI *gti);
int hxdphaUtil_close_FITS ( void );

#endif

