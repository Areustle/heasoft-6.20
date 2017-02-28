/*

  aste_orbit: calculate satellite position for Astro-E2

	2005-08-30	Y.ISHISAKI	version 1.0

	2005-10-24	Y.ISHISAKI	version 1.2 (to match with AEorbitFitsWrite 1.2)
		add int version; in ORBIT
*/

#ifndef _ASTE_ORBIT_H_
#define _ASTE_ORBIT_H_

typedef struct {
	double t;						/* mission time (s) */
	double A, E, I, AN, AP, MA;		/* Keplerian orbital elements */
} KEPLERIAN;

typedef struct {

	int version;		/* 1: atSetElement2(), 2: aste_orbit */

	char *orbit_file;	/* orbit FITS file name */

	double tstart;
	double tstop;

	int nkp;			/* number of Keplerian */
	KEPLERIAN *kp;

	int prev_ipos;		/* cache previous pointer */

} ORBIT;

#ifdef __cplusplus
extern "C"
{
#endif

/************************************************************************
int aste_orbit_init()	: initialize ORBIT information

Input:
	char *orbit_file	: orbit FITS file name

Output:
	ORBIT **obpp		: pointer to ORBIT pointer after initialization

Return_Values:
	0					: success
	-1					: malloc() error
	others				: CFITSIO error
************************************************************************/
int aste_orbit_init(ORBIT **obpp, char *orbit_file);


/************************************************************************
int aste_orbit_free()	: free memory of ORBIT

Input:
	ORBIT *obp		: ORBIT pointer to free

Return_Values:
	0					: success
	-1					: ORBIT *ttp is not allocated
************************************************************************/
int aste_orbit_free(ORBIT *obp);


/************************************************************************
int aste_orbit()		: get sidereal vector to the satellite

Input:
	ORBIT *obp			: ORBIT pointer used for the calculation
	double aetime		: Astro-E time when the packet was received

Output:
	AtVect xyz			: sidereal vector to the satellite (km)
	AtVect vxyz			: sidereal vector of the satellite velocity (km/s)

Return_Values:
	0					: success
	-1					: no valid time interval in ORBIT *obp
************************************************************************/
int aste_orbit(ORBIT *obp, double aetime, AtVect xyz, AtVect vxyz);


#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_ORBIT_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
