/*
	xrs_gain.c		XRS gain releated functions

	2005/05/09 Y.ISHISAKI	version 1.6
		created for XRSgainhistMake-1.6
*/

#ifndef _XRS_GAIN_H_
#define _XRS_GAIN_H_

#define XRS_GAIN_POLY_MAX	9	/* up to 8th order of polynomial */

typedef struct {

	double start;			/* START column */
	double stop;			/* STOP column */
	int pixel;				/* PIXEL column */
	int method;				/* METHOD column */
	int np;					/* NP column */
	double *p;				/* Pn columns */
	double pbuf[XRS_GAIN_POLY_MAX];

} XRS_GAIN_PARAMS;


typedef struct {

	char *filename;			/* FITS file name */
	double pi_escal;		/* PI_ESCAL keyword */
	int ng;					/* number of gains items */

	XRS_GAIN_PARAMS *g;

} XRS_GAINS;

#ifdef __cplusplus
extern "C"
{
#endif

XRS_GAINS *xrs_gain_file_read(char *filename, int *status);
XRS_GAIN_PARAMS * xrs_gain_param_get(XRS_GAINS *gs, int pixel, double t, int *status);

#ifdef __cplusplus
}
#endif

#endif	/* _XRS_GAIN_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
