/*
 *	calculus.h: --- matrix operations, transformation, etc. ---
 *
 *	DESCRIPTION:
 *
 *	EXPORTED FUNCTIONS
 *
 *	AUTHOR:
 *
 *	UL, Wed Jun 21 10:05:07 METDST 1995
 */
#ifndef CALCULUS_H
#define CALCULUS_H

						/********************************/
						/*           includes           */
						/********************************/

#include "basic.h"
#include <fitsio2.h>
						/********************************/
						/*      defines / typedefs      */
						/********************************/

#ifndef M_PI			/* Avoid compiler warnings on OSF */
#define M_PI			3.14159265358979323846
#endif
#define M_HALF_PI		(.5*M_PI)
#define M_TWO_PI		(2.*M_PI)
#define M_SQRT_TWO_PI	2.5066282746310002		/* sqrt(2.*Pi)			*/
#define M_2SQ2LN2		2.3548200450309493		/* 2.*sqrt(2.*ln(2))	*/
#define EPSILON			1.e-12
#define DEG_TO_RAD		(M_PI/180.)
#define RAD_TO_DEG		(180./M_PI)

typedef double Vect_t[3];		/* cartesian vector							*/
typedef double RaDec_t[3][2];	/* right ascension/declination of S/C axes	*/
typedef double RotMat_t[3][3];	/* rotation matrix							*/
typedef struct {
	double	Phi,		/* rotation angle around z-axis	in decimal degrees	*/
			Theta,		/* rotation angle around x-axis in decimal degrees	*/
			Psi;		/* rotation angle around z-axis	in decimal degrees	*/
} EulerAng_t;
typedef struct {
	double	AvgVal,		/* average value		*/
			StdDev;		/* standard deviation	*/
} VecStat_t;

typedef enum {
	CALC_OK = 0,			/* everything fine					*/
	CALC_ILLEGAL_ROTMAT,	/* matrix is not a rotation matrix	*/
	CALC_ERROR				/* unspecified error				*/
} CalcError_t;

						/********************************/
						/*      function prototypes     */
						/********************************/

/*
 *	SYNOPSIS:
 *		CalcError_t Equat2Cart(const double Ra, const double Dec, Vect_t Vec);
 *
 *	DESCRIPTION:
 *		Transform geocentric equatorial direction vector into geocentric
 *		cartesian unit vector.
 *
 *	PARAMETERS:
 *	 I	:	Ra		: geocentric right ascension
 *			Dec		: geocentric declination
 *	 O	:	Vec		: calculated cartesian unit vector
 *
 *	RETURNS:
 *		always 0
 */
extern CalcError_t Equat2Cart(const double Ra, const double Dec, Vect_t Vec);



/*
 *	SYNOPSIS:
 *		CalcError_t RaDec2DirCosMat(RaDec_t RaDec, RotMat_t RotMat)
 *
 *	DESCRIPTION:
 *
 *	Transform right ascension/declination values for the three S/C axes
 *	into a direction cosine matrix.
 *
 *	PARAMETERS:
 *
 *	 I	:	RaDec	: Ra/Dec values of S/C X/Y/Z axes
 *	 O	:	RotMat	: the calculated direction cosine matrix
 *	 I/O:	<none>
 *
 *	RETURNS:
 *
 *	0/<0: success/failure (calculated matrix is not a valid rotation matrix,
 *						   e.g. non-orthogonal)
 */
extern CalcError_t RaDec2DirCosMat(RaDec_t RaDec, RotMat_t RotMat);



/*
 *	SYNOPSIS:
 *		CalcError_t DirCosMat2RaDec(RotMat_t RotMat, RaDec_t RaDec);
 *
 *	DESCRIPTION:
 *
 *	Transform a direction cosine matrix into geocentric equatorial coordinates
 *	(right ascension/declination) for the three S/C axes.
 *
 *	PARAMETERS:
 *
 *	 I	:	RotMat	: the direction cosine matrix
 *	 O	:	RaDec	: the computed Ra/Dec values of S/C's X/Y/Z-axis
 *	 I/O:	<none>
 *
 *	RETURNS:
 *
 *	always 0
 */
extern CalcError_t DirCosMat2RaDec(RotMat_t RotMat, RaDec_t RaDec);



/*
 *	SYNOPSIS:
 *		CalcError_t DirCosMat2EulerAng(RotMat_t RotMat, EulerAng_t *EulerAng)
 *
 *	DESCRIPTION:
 *
 *	Calculate the three Euler angles corresponding to a particular direction
 *	cosine matrix.
 *
 *	PARAMETERS:
 *
 *	 I	:	RotMat	: the direction cosine matrix
 *	 O	:	EulerAng: pointer to structure that, on successful return, will
 *					  contain the calculated Euler angles
 *	 I/O:	<none>
 *
 *	RETURNS:
 *
 *	0/<0: success/failure
 */
extern CalcError_t DirCosMat2EulerAng(RotMat_t RotMat,
									  EulerAng_t *EulerAng);



/*
 *	SYNOPSIS:
 *		CalcError_t EulerAng2DirCosMat(EulerAng_t EulerAng, RotMat_t RotMat);
 *
 *	DESCRIPTION:
 *
 *	Calculate the direction cosine matrix corresponding to a set of 3-1-3
 *	Euler angles.
 *
 *	PARAMETERS:
 *
 *	 I	:	EulerAng	: the Euler angles
 *	 O	:	RotMat		: will contain the computed attitude matrix
 *	 I/O:	<none>
 *
 *	RETURNS:
 *
 *	always 0
 */
extern CalcError_t EulerAng2DirCosMat(EulerAng_t EulerAng, RotMat_t RotMat);



/*
 *	SYNOPSIS:
 *		CalcError_t RotMatXRotMat(RotMat_t R2, RotMat_t R1, RotMat_t RSum);
 *
 *	DESCRIPTION:
 *
 *	Calculate the product of two successive rotations: RSum = R2 * R1
 *
 *	PARAMETERS:
 *
 *	 I	:	R1		: first rotation matrix
 *			R2		: second rotation matrix
 *	 O	:	RSum	: rotation matrix corresponding to product R2 * R1
 *	 I/O:	<none>
 *
 *	RETURNS:
 *
 *	always 0
 */
extern CalcError_t RotMatXRotMat(RotMat_t R1,
								 RotMat_t R2,
								 RotMat_t RSum);



/*
 *	SYNOPSIS:
 *		CalcError_t RotMatXVector(RotMat_t R, Vect_t V, Vect_t VRes)
 *
 *	DESCRIPTION:
 *
 *	Apply 3x3 matrix to vector.
 *
 *	PARAMETERS:
 *
 *	 I	:	R	: the 3x3 matrix
 *			V	: the 3-component vector
 *	 O	:	VRes: the resulting vector VRes = R * V
 *	 I/O:	<none>
 *
 *	RETURNS:
 *
 *	always 0
 */
extern CalcError_t RotMatXVector(RotMat_t R,
								 Vect_t V,
								 Vect_t VRes);



/*
 *	SYNOPSIS:
 *		CalcError_t PolarDetFrame(const double Ra, const double Dec,
 *								  RaDec_t ActAtt,
 *								  RotMat_t BS_Misalign,
 *								  double *Theta, double *Phi, double *Roll);	
 *
 *	DESCRIPTION:
 *
 *	This function does two things:
 *	  1. Calculate polar angles Theta, Phi of given direction in momentary
 *		 detector reference frame
 *	  2. Calculate current S/C roll angle
 *
 *	PARAMETERS:
 *
 *	 I	:	Ra			: geocentric right ascension of direction vector in
 *						  radians
 *			Dec			: geocentric declination of direction vector radians
 *			ActAttitude	: current attitude of S/C as Ra/Dec values of X/Y/Z axes
 *			BS_Misalign	: boresight misalignment matrix
 *	 O	:	*Theta		: colatitude of direction vector in S/C reference frame
 *						  in radians
 *			*Phi		: azimuth of direction vector in S/C reference frame in
 *						  radians
 *			*Roll		: roll angle corresponding to <ActAtt> in radians
 *	 I/O:	<none>
 *
 *	RETURNS:
 *
 *	0/<0 on success/failure
 */
extern CalcError_t PolarDetFrame(const double Ra,
								 const double Dec,
								 double *OptRa,
								 double *OptDec,
								 RaDec_t ActAtt,
								 RotMat_t BS_Misalign,
								 double *Theta,
								 double *Phi,
								 double *Roll);


/*
 *	SYNOPSIS:
 *       CalcError_t ChangeRefSystem(const double Ra, const double Dec, 
 *			   			             Vect_t *VecCoord, RotMat_t RotMat, 
 *						             double *Theta, double *Phi);
 *
 *	DESCRIPTION:
 *
 *	Transform coordinates into a new reference system using a rotational 
 *  matrix. Input coordinates can be supplied as (Ra,Dec) or in vectorial
 *  form. New coordinates are given as colatitude (Theta) and azimuth (Phi).
 *
 *	PARAMETERS:
 *
 *	 I	:  Ra		   : geocentric right ascension of direction vector in
 *		    			 radians
 *		   Dec		   : geocentric declination of direction vector radians
 *         VecCoord    : direction vector
 *		   Rot         : rotational matrix
 *	 O	:  *Theta	   : colatitude of direction vector in new reference frame
 *						 in radians
 *		   *Phi		   : azimuth of direction vector in new reference frame in
 *						 radians
 *	 I/O:	<none>
 *
 *	RETURNS:
 *
 *	always 0
 */
extern CalcError_t ChangeRefSystem(const double Ra, const double Dec, 
							Vect_t *VecCoord, RotMat_t RotMat, 
							double *Theta, double *Phi);


/*
 *	SYNOPSIS:
 *		CalcError_t VectorStatistics_[D|F](const [double|float|int] Vec[],
 *										   unsigned Size,
 *										   VecStat_t *VStat,
 *										   BOOL BoundsPresent,
 *										   ...);
 *
 *	DESCRIPTION:
 *		Functions to compute the average value and standard deviation of a
 *		vector containing double, float, or int values.
 *
 *	PARAMETERS:
 *		I	:	DVec		  : data vector
 *			:	Size		  : size of <DVec>: DVec[0] ... DVec[Size-1]
 *			:	BoundsPresent : boolean value determining whether two numeric
 *								values end the function's argument list.
 *								Those numbers (of the right type) would
 *								be taken as an interval specification: only
 *								elements in <DVec> lying in this interval
 *								are considered for the computation
 *		O	:	*VStat		  : structure filled computed average value and
 *							    standard deviation
 *		I/O :	<none>
 *
 *	RETURNS:
 *		CALC_OK/CALC_ERROR  on Size>0/0
 */
extern CalcError_t VectorStatistics_D(const double DVec[],
									  unsigned Size,
									  VecStat_t *VStat,
									  BOOL BoundSpecsPresent,
									  /* MinVal, MaxVal */ ...);

extern CalcError_t VectorStatistics_F(const float FVec[],
									  unsigned Size,
									  VecStat_t *VStat,
									  BOOL BoundSpecsPresent,
									  /* MinVal, MaxVal */ ...);

#define VectorStatistics(a, b, c, d)	VectorStatistics_D(a, b, c, d)

#endif
