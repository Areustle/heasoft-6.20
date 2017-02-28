#ifndef _HXD_GET_HK_UTIL_
#define _HXD_GET_HK_UTIL_

#include <fitsio.h>

#define HXD_GET_HK_ERR -1           /* quit */
#define HXD_GET_HK_OK 0             /* *val ac_time[0] ac_time[1] */
#define HXD_GET_HK_NO_UPDATE 1      /* */
#define HXD_GET_HK_NOT_YET_EXIST 2  /* ac_time[1] */
#define HXD_GET_HK_NEXT_NOT_EXIST 3 /* *val ac_time[0] */

int hxd_getHK_int(double time, int index, unsigned int *val, double *ac_time);

int hxd_getHK_double(double time, int index, double *val, double *ac_time);

int hxd_getHK_Init(fitsfile *fp, char *filename);
/* HXD_GET_HK_OK HXD_GET_HK_ERR */

int hxd_getHK_Register(fitsfile *fp, char *key, int *index);
/* HXD_GET_HK_OK HXD_GET_HK_ERR */

#endif
