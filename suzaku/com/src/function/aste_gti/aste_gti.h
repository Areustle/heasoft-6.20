/*
  aste_gti: GTI support routines for SUZAKU

	2006-07-25 Y.ISHISAKI	version 1.0
		aste_gti_shrink(), aste_gti_and(), aste_gti_or() declared,
		but not implemented
*/

#ifndef _ASTE_GTI_H_
#define _ASTE_GTI_H_

typedef struct {
	double tstart;	/* TSTART keyword:	time start */
	double tstop;	/* TSTOP keyword:	time stop */
	double telapse;	/* TELAPSE keyword:	elapsed time = TSTOP - TSTART */
	double ontime;	/* ONTIME keyword:	on time = sum of all GTIs */
	int ngti;
	double *start;
	double *stop;
} GTI_DATA;

#ifdef __cplusplus
extern "C"
{
#endif

/************************************************************************
int aste_gti_zero()		: initialize GTI as zero

Input/Output:
	GTI_DATA *gp		: pointer to GTI

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_zero(GTI_DATA *gp);

/************************************************************************
int aste_gti_read()		: read GTI_DATA from FITS or text file

Input:
	char *gtifile		: file name to read

Input/Output:
	GTI_DATA *gp		: pointer to GTI

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_read(GTI_DATA *gp, char *gtifile);

/************************************************************************
int aste_gti_shrink()	: shrink redundant GTI

Input:
	GTI_DATA *gp		: pointer to GTI

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_shrink(GTI_DATA *gp);

/************************************************************************
int aste_gti_and()		: make AND of two GTIs

Input:
	GTI_DATA *g1p		: pointer to 1st GTI
	GTI_DATA *g2p		: pointer to 2nd GTI
	GTI_DATA *g3p		: resultant GTI of (1st GTI .AND. 2nd GTI)

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_and(GTI_DATA *g1p, GTI_DATA *g2p, GTI_DATA *g3p);

/************************************************************************
int aste_gti_or()		: make OR of two GTIs

Input:
	GTI_DATA *gti1		: pointer to 1st GTI
	GTI_DATA *gti2		: pointer to 2nd GTI
	GTI_DATA *gti_new	: resultant GTI of (1st GTI .OR. 2nd GTI)

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_or(GTI_DATA *g1p, GTI_DATA *g2p, GTI_DATA *g3p);

/************************************************************************
int aste_gti_free()		: free allocated memory for GTI

Input:
	GTI_DATA *gp		: pointer to GTI

Return_Values:
	0					: success
	others  			: CFITSIO errors (defined in fitsio.h)
************************************************************************/
int
aste_gti_free(GTI_DATA *gp);

#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_GTI_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
