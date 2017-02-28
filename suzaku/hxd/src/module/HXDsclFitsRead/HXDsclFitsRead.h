#ifndef _HXD_SCL_FITS_READ_H_
#define _HXD_SCL_FITS_READ_H_

#define HXD_SCL_FITS_PRIMARY_HDU     1
#define HXD_SCL_FITS_EVENT_EXTENTION 5

#define HXD_SCL_FITS_HK_EXTENTION   2
#define HXD_SCL_FITS_SYS_EXTENTION  3

#define HXD_SCL_FITS_NUM_EXTENTION  22

#define HXD_SCL_FITS_KEY_READ_NUM    6

static char *hxd_scl_fits_keyword[HXD_SCL_FITS_KEY_READ_NUM] = {
	"TIME",           /* assigned time     */
	"TI",             /* secoundary header */
	"HXD_SCL_AETIME", /* packet aetime     */
	"HXD_SCL_S_TIME", /* packet s_time     */
	"HXD_SCL_TIME",   /* 24-bit counter    */
	"HXD_SCL_PI_WPU_ID"/* WPU board ID      */
/*	"HXD_SCL_MODULE" */ /* WPU board ID      */
};

enum{
	TIME,
	TI,
	HXD_SCL_AETIME,
	HXD_SCL_S_TIME,
	HXD_SCL_TIME,
	HXD_SCL_PI_WPU_ID
};


#endif
