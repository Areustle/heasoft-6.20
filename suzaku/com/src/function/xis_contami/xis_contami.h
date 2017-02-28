/*

  xis_contami: calculate transmission of the Suzaku XIS OBF contamination

	2006-05-24	Y.ISHISAKI	version 1.0

	2006-08-26	Y.ISHISAKI	version 1.1
		char *contami_file -> contamifile in XIS_CONTAMI structure
		add last_irow to cache irow in contamifile

	2006-08-26	Y.ISHISAKI	version 1.2
		add coratio in XIS_CONTAMI
		return carbon, oxygen column density in xis_contami()

	2009-12-01	Y.ISHISAKI	version 1.3
		modified for FORMAT_VERSION=2, variable elemental ratios
*/

#ifndef _XIS_CONTAMI_H_
#define _XIS_CONTAMI_H_

typedef struct {
	double t;			/* mission time (s) */
	double A, B, C;		/* coefficients in CONTAMI_GROWTH extension */
	double O, H, X;		/* added in FORMAT_VERSION 2 */
} XIS_CONTAMI_GROWTH;

typedef struct {
	double norm, offs;
	int nbody;		/* in fact, sizeof(body)-1 */
	int *body;		/* 0-nbody */
} XIS_CONTAMI_TRANS_INDEX;

typedef struct {

	int version;		/* 1: FORMAT_VERSION=1, 2: FORMAT_VERSION=2 */

	char *contamifile;	/* XIS contami FITS file name */

	int ngp;			/* number of XIS_CONTAMI_GROWTH */
	XIS_CONTAMI_GROWTH *gp;

	double coratio;		/* CORATIO keyword, only FORMAT_VERSION=1 */

	int nen;
	double *en;
	double *vc, *vo, *vh, *vx;
	XIS_CONTAMI_TRANS_INDEX index;

	int last_irow;			/* cache irow in contamifile */
	double last_radius;		/* cache last radius in arcmin */
	double last_energy;		/* cache last energy */
	double last_trans[4];	/* cache last transmission */

} XIS_CONTAMI;

#ifdef __cplusplus
extern "C"
{
#endif

/************************************************************************
int xis_contami_init()	: initialize XIS contamination information

Input:
	char *contamifile	: XIS contami FITS file name

Output:
	XIS_CONTAMI **xcpp	: pointer to XIS_CONTAMI pointer after initialization

Return_Values:
	0					: success
	-1					: malloc() error
	others				: CFITSIO error
************************************************************************/
int xis_contami_init(XIS_CONTAMI **xcpp, char *contamifile);


/************************************************************************
int xis_contami_free()	: free memory of XIS_CONTAMI

Input:
	XIS_CONTAMI *xcp	: XIS_CONTAMI pointer to free

Return_Values:
	0					: success
	-1					: XIS_CONTAMI *xcp is not allocated
************************************************************************/
int xis_contami_free(XIS_CONTAMI *xcp);


/************************************************************************
double xis_contami()	: get transmission of XIS contamination

Input:
	XIS_CONTAMI *xcp	: XIS_CONTAMI pointer used for the calculation
	double energy		: X-ray photon energy in keV
	double aetime		: Astro-E time when X-ray photon was arrived
	double detx, dety	: DETX, DETY coordinates of the X-ray incident

Output:
	double *carbon		: contamination in 1e18 cm-2 of carbon column density
	double *oxygen		: contamination in 1e18 cm-2 of oxygen column density

Return_Values:
	0.0 -- 1.0			: calculated transmission
	-1.0				: error in XIS_CONTAMI *xcp
************************************************************************/
double xis_contami(XIS_CONTAMI *xcp,
	double energy, double aetime, double detx, double dety,
	double *carbon, double *oxygen);


#ifdef __cplusplus
}
#endif

#endif	/* _XIS_CONTAMI_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; c-basic-offset:4  ***
;;; End: ***
*/
