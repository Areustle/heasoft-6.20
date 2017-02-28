/* $Id: xrs_tick2ti.h,v 1.4 2005/08/08 14:20:22 irby Exp $ */
/*
  xrs_tick2ti
    TICK [s] -> TI [1/4096 s] conversion for XRS

	2005-02-16	Y.ISHISAKI	version 1.0

	2005/05/03 Y.ISHISAKI	version 1.4
		rename all RECV_TIME -> S_TIME

	2005/07/26 Y.ISHISAKI	version 1.8
		add prev_s_time, prev_pkt_ti, prev_evt_ti, prev_tick for each pixel

	2005/07/26 Y.ISHISAKI	version 1.9
		add prev_tick2ti for each pixel
*/

#ifndef _XRS_TICK2TI_H_
#define _XRS_TICK2TI_H_

typedef struct {

	char *tick_hk_file;			/* xrs_tick.hk FITS file name */

	int ntk;					/* number of thk items */
	struct xrs_tick_hk_struct {

		double S_TIME;			/* reference Astro-E time */
		unsigned int TI;		/* reference DP-TI, 1/4096 s */
		unsigned int TICK;		/* reference XRS TICK, 1 s */
		int TICK2TI;			/* TI/4096 = TICK + TICK2TI */
		unsigned char PIXEL;	/* 0-31, or 100:side-A, 200:side-B, 255:all */
		unsigned char TIME_QUALITY;	/* how ref-TI & TICK are determined */
						/* 0:SET_TICK, 2:reboot, 4:tick-jump, 6:pre-tick-jump,
						  +1:time-lost, +10:copied from init file */
	} *tk;

/* remember previous conversion for each pixel */
	double prev_s_time[32];
	unsigned int prev_pkt_ti[32];
	unsigned int prev_evt_ti[32];
	int prev_tick[32];
	int prev_tick2ti[32];

/* debug information for last time correction */
	double s_time;
	unsigned int pkt_ti;
	int pixel;
	unsigned int tick;
	int tick_bits;
	double margin;

} XRS_TICK2TI;

#ifdef __cplusplus
extern "C"
{
#endif

/************************************************************************
int xrs_tick2ti_init()	: initialize XRS_TICK2TI information

Input:
	char *tick_hk_file	: xrs_tick.hk FITS file name

Output:
	XRS_TICK2TI **xtpp	: pointer to XRS_TICK2TI pointer after initialization

Return_Values:
	0					: success
	-1					: malloc() error
	others				: CFITSIO error
************************************************************************/
int xrs_tick2ti_init(XRS_TICK2TI **xtpp, char *tick_hk_file);


/************************************************************************
int xrs_tick2ti_free()	: free memory of XRS_TICK2TI

Input:
	XRS_TICK2TI *xtp	: XRS_TICK2TI pointer to free

Return_Values:
	0					: success
	-1					: XRS_TICK2TI *xtp is not allocated
************************************************************************/
int xrs_tick2ti_free(XRS_TICK2TI *xtp);


/************************************************************************
int xrs_tick2ti()		: convert XRS-TICK [s] -> TI [1/4096 s]

Input:
	XRS_TICK2TI *xtp	: XRS_TICK2TI pointer used for the time correction
	unsigned int pkt_ti	: TI [1/4096 s] of the packet
	double s_time	    : Astro-E time [s] of the packet (S_TIME column)
	int pixel			: XRS PIXEL number (PIXEL column, 0-31)
	unsigned int tick	: XRS TICK [s] (TICK_COUNTER column), usually 0-15
	int tick_bits		: usable number of bit of tick, usually 4
	double margin_sec	: time margin [s] to judge TI continuity, usually 60

Output:
	unsigned int *ti_return	: calculated TI [1/4096 s]
	int *time_quality_return: quality of time assignment (0-8)

		0: time is determined by SET_TICK
		1: sole errTimeLost happened after time_quality=0
		2: by CDP-HK preceded by errRebooting
		3: sole errTimeLost happened after time_quality=2
		4: by CDP-HK not preceded by errRebooting
		5: sole errTimeLost happened after time_quality=4
		6: next is time_quality=4, and determined by previous CDP-HK
		7: next is time_quality=4, and determined by next CDP-HK
		8: TICK2TI is not known, so that pkt_ti itself was used

Return_Values:
	0					: success
	-1					: no valid TICK2TI information in XRS_TICK2TI *xtp
************************************************************************/
int
xrs_tick2ti(XRS_TICK2TI *xtp, unsigned int pkt_ti, double s_time,
		int pixel, unsigned int tick, int tick_bits, double margin_sec,
		unsigned int *ti_return, int *time_quality_return);

#ifdef __cplusplus
}
#endif

#endif	/* _XRS_TICK2TI_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
