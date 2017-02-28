/*
 *	calculus.c: --- matrix operations, transformation, etc. ---
 *
 *	DESCRIPTION:
 *		Functions which perform certain types of calculations.
 *
 *	CHANGE HISTORY:
 *		UL, 21/06/95: 1.0.0: - first version
 *		UL, 04/09/95: 1.0.1: - bug fix in 'DirCosMat2RaDec'
 *		AM, 30/10/96: 1.0.2: - a specific task previously performed in
 *                             'PolarDetFrame' has been moved to 
 *                             'ChangeRefSystem'
 *      AM, 26/09/97: 1.0.3: - change in prototype of function 'PolarDetFrame'
 *
 *	AUTHOR:
 *		UL, Wed Jun 21 10:05:07 METDST 1995
 */

#define CALCULUS_C
#define CALCULUS_VERSION	1.0.3

						/********************************/
						/*        header files          */
						/********************************/

#include <math.h>
#include <stdarg.h>
#include "basic.h"
#include "highfits.h"	/* for definition of NULL values */
#include "calculus.h"

						/********************************/
						/*      external functions      */
						/********************************/

CalcError_t Equat2Cart(const double Ra, const double Dec, Vect_t Vec)
{
	double	CosDec;

	CosDec = cos(Dec);
	Vec[0] = cos(Ra) * CosDec;
	Vec[1] = sin(Ra) * CosDec;
	Vec[2] = sin(Dec);

	return CALC_OK;
} /* Equat2Cart */



CalcError_t RaDec2DirCosMat(RaDec_t RaDec, RotMat_t RotMat)
{
	unsigned	i;
	double		alpha, delta, cos_delta;

	for (i=0; i<3; ++i) {
		alpha		= DEG_TO_RAD * RaDec[i][0];
		delta		= DEG_TO_RAD * RaDec[i][1];
		cos_delta	= cos(delta);
		RotMat[i][0] = cos(alpha) * cos_delta;
		RotMat[i][1] = sin(alpha) * cos_delta;
		RotMat[i][2] = sin(delta);
	}

	return CALC_OK;
} /* RaDec2DirCosMat */



CalcError_t DirCosMat2RaDec(RotMat_t RotMat, RaDec_t RaDec)
{
	unsigned	i;
	double		phi;

	for (i=0; i<3; ++i) {
		phi = atan2(RotMat[i][1], RotMat[i][0]);
		if (phi < 0.)
			phi = M_TWO_PI + phi;
		RaDec[i][0] = phi * RAD_TO_DEG;
		RaDec[i][1] = (M_HALF_PI - acos(RotMat[i][2])) * RAD_TO_DEG;
	}

	return CALC_OK;
} /* DirCosMat2RaDec */



CalcError_t DirCosMat2EulerAng(RotMat_t RotMat, EulerAng_t *EulerAng)
{
	if (RotMat[2][2] > 1.-EPSILON) {
		/*
		 *	only rotation around z
	 	 */
		EulerAng->Theta = 0;
		EulerAng->Phi = atan2(RotMat[0][1], RotMat[0][0]);
		EulerAng->Psi = 0;
	} else if (RotMat[2][2] < -1.+EPSILON) {
		/*
		 *	flip w/ y and rot around z
		 */
		EulerAng->Theta = M_PI;
		EulerAng->Phi = atan2(-RotMat[0][1], -RotMat[0][0]);
		EulerAng->Psi = 0;
	} else {
		EulerAng->Theta = acos(RotMat[2][2]);

		if (fabs(RotMat[2][0]) > EPSILON || fabs(RotMat[2][1]) > EPSILON)
			EulerAng->Phi = atan2(RotMat[2][0], -RotMat[2][1]);
		else {
			EulerAng->Phi = 0;

			return CALC_ILLEGAL_ROTMAT;
		}

		if (fabs(RotMat[0][2]) > EPSILON || fabs(RotMat[1][2]) > EPSILON)
			EulerAng->Psi = atan2(RotMat[0][2], RotMat[1][2]);
		else { 
			EulerAng->Psi = 0;
			return CALC_ILLEGAL_ROTMAT;
		}
	}

	if (EulerAng->Phi < 0.)
		EulerAng->Phi += M_TWO_PI;
	if (EulerAng->Psi < 0.)
		EulerAng->Psi += M_TWO_PI;

	return CALC_OK;
} /* DirCosMat2EulerAng */



CalcError_t EulerAng2DirCosMat(EulerAng_t EulerAng, RotMat_t RotMat)
{
	double	SinPhi, CosPhi, SinTheta, CosTheta, SinPsi, CosPsi;

	SinPhi		= sin(EulerAng.Phi * DEG_TO_RAD);
	CosPhi		= cos(EulerAng.Phi * DEG_TO_RAD);
	SinTheta	= sin(EulerAng.Theta * DEG_TO_RAD);
	CosTheta	= cos(EulerAng.Theta * DEG_TO_RAD);
	SinPsi		= sin(EulerAng.Psi * DEG_TO_RAD);
	CosPsi		= cos(EulerAng.Psi * DEG_TO_RAD);

	RotMat[0][0] = 	CosPsi*CosPhi - CosTheta*SinPsi*SinPhi;
	RotMat[0][1] =	CosPsi*SinPhi + CosTheta*SinPsi*CosPhi;
	RotMat[0][2] =	SinTheta*SinPsi;
	RotMat[1][0] = -SinPsi*CosPhi - CosTheta*CosPsi*SinPhi;
	RotMat[1][1] = -SinPsi*SinPhi + CosTheta*CosPsi*CosPhi;
	RotMat[1][2] =	SinTheta*CosPsi;
	RotMat[2][0] =	SinTheta*SinPhi;
	RotMat[2][1] = -SinTheta*CosPhi;
	RotMat[2][2] =	CosTheta;

	return CALC_OK;
} /* EulerAng2DirCosMat */



CalcError_t RotMatXRotMat(RotMat_t R2, RotMat_t R1, RotMat_t RSum)
{
	RSum[0][0] = R2[0][0]*R1[0][0] + R2[0][1]*R1[1][0] + R2[0][2]*R1[2][0];
	RSum[0][1] = R2[0][0]*R1[0][1] + R2[0][1]*R1[1][1] + R2[0][2]*R1[2][1];
	RSum[0][2] = R2[0][0]*R1[0][2] + R2[0][1]*R1[1][2] + R2[0][2]*R1[2][2];
	RSum[1][0] = R2[1][0]*R1[0][0] + R2[1][1]*R1[1][0] + R2[1][2]*R1[2][0];
	RSum[1][1] = R2[1][0]*R1[0][1] + R2[1][1]*R1[1][1] + R2[1][2]*R1[2][1];
	RSum[1][2] = R2[1][0]*R1[0][2] + R2[1][1]*R1[1][2] + R2[1][2]*R1[2][2];
	RSum[2][0] = R2[2][0]*R1[0][0] + R2[2][1]*R1[1][0] + R2[2][2]*R1[2][0];
	RSum[2][1] = R2[2][0]*R1[0][1] + R2[2][1]*R1[1][1] + R2[2][2]*R1[2][1];
	RSum[2][2] = R2[2][0]*R1[0][2] + R2[2][1]*R1[1][2] + R2[2][2]*R1[2][2];

	return CALC_OK;
} /* RotMatXRotMat */



CalcError_t RotMatXVector(RotMat_t R, Vect_t V, Vect_t VRes)
{
	VRes[0] = R[0][0]*V[0] + R[0][1]*V[1] + R[0][2]*V[2];
	VRes[1] = R[1][0]*V[0] + R[1][1]*V[1] + R[1][2]*V[2];
	VRes[2] = R[2][0]*V[0] + R[2][1]*V[1] + R[2][2]*V[2];

	return CALC_OK;
}



CalcError_t ChangeRefSystem(const double Ra, const double Dec, 
							Vect_t *VecCoord, RotMat_t RotMat, 
							double *Theta, double *Phi)
{
	Vect_t SC_Vec, SkyVec;

	#define XU	SC_Vec[0]
	#define YU	SC_Vec[1]
	#define ZU	SC_Vec[2]

	if(!VecCoord) 
		/*
		 *  coordinates are supplied as (Ra,Dec)
		 */
		Equat2Cart(Ra, Dec, SkyVec);
	else {
		/*
		 *  coordinates are already in cartesian form
		 */
		SkyVec[0] = (*VecCoord)[0];
		SkyVec[1] = (*VecCoord)[1];
		SkyVec[2] = (*VecCoord)[2];
	}

	RotMatXVector(RotMat, SkyVec, SC_Vec);
	/*
	 *	*Theta is 0/180 along +/- Z axis
	 *	*Phi   is 0 along +X axis
	 */
	if (fabs(ZU) + EPSILON <= 1.) {
		*Theta = acos(ZU);
		if (fabs(XU) > EPSILON || fabs(YU) > EPSILON)
			*Phi = atan2(YU, XU);
		else {
			*Phi = 0.;

			return CALC_ERROR;
		}
	} else {
		*Phi	= 0.;
		*Theta	= ZU <= -.5 ? M_PI : 0.;
	}

	return CALC_OK;
} /* ChangeRefSystem */



CalcError_t PolarDetFrame(const double Ra, const double Dec,
						  double *OptRa, double *OptDec,
						  RaDec_t ActAttitude,
 						  RotMat_t BS_MisalignMatrix,
 						  double *Theta, double *Phi, double *Roll)
{
	RotMat_t	AttitudeMatrix, BS_AttitudeMatrix;
	EulerAng_t	EulerAng;
	RaDec_t ActOptAxisPnt;

	/*
	 *	get S/C attitude as direction cosine matrix
	 */
	RaDec2DirCosMat(ActAttitude, AttitudeMatrix);

	/*
	 *	consider boresight misalignment  
	 */
	RotMatXRotMat(BS_MisalignMatrix, AttitudeMatrix, BS_AttitudeMatrix);

	/*
	 *   get optical axis pointing coordinates
	 */
	DirCosMat2RaDec(BS_AttitudeMatrix, ActOptAxisPnt);
	*OptRa = ActOptAxisPnt[2][0] * DEG_TO_RAD;
	*OptDec = ActOptAxisPnt[2][1] * DEG_TO_RAD;
	  
	/*
	 *	calculate Euler angles to obtain roll angle
	 */
	if (!DirCosMat2EulerAng(BS_AttitudeMatrix, &EulerAng)) {
		*Roll = -(M_HALF_PI - EulerAng.Psi);
		if (*Roll < -M_PI)
			*Roll += M_TWO_PI;
	} else
		return CALC_ILLEGAL_ROTMAT;

	/*
	 *	calculate polar coordinates of pointing direction in S/C reference
	 *	frame
	 */
	ChangeRefSystem(Ra, Dec, (Vect_t *)NULL, BS_AttitudeMatrix, 
					Theta, Phi);

	return CALC_OK;
} /* PolarDetFrame */



#define VEC_STAT_BODY(Vec, NULLTEST)   						\
	register unsigned	n, nVals;							\
	double				avg, vari, MinVal, MaxVal;			\
															\
	VStat->AvgVal = VStat->StdDev = 0;						\
															\
	/*														\
	 *	compute average										\
	 */														\
	avg		= 0.;											\
	vari	= 0.;											\
	nVals	= 0;											\
															\
	if (!BoundsPresent)										\
		for (n=0; n<Size; ++n)								\
			if (!NULLTEST(Vec[n])) {						\
				avg += Vec[n];								\
				vari += Vec[n] * Vec[n];					\
				++nVals;									\
			}												\
	else {													\
		va_list ap;											\
															\
		va_start(ap, BoundsPresent);						\
		MinVal = va_arg(ap, double);						\
		MaxVal = va_arg(ap, double);						\
		va_end(ap);											\
															\
		for (n=0; n<Size; ++n)								\
			if (!NULLTEST(Vec[n]) && MinVal <= Vec[n] && MaxVal >= Vec[n]){ \
				avg += Vec[n];								\
				vari += Vec[n] * Vec[n];					\
				++nVals;									\
			}												\
	}														\
															\
	if (nVals) {											\
		avg /= (double)nVals;								\
		VStat->AvgVal = avg;								\
		VStat->StdDev = sqrt(vari/(double)nVals - avg*avg);	\
															\
		return CALC_OK;										\
	} else													\
		return CALC_ERROR;


CalcError_t VectorStatistics_F(const float FVec[], unsigned Size,
							 VecStat_t *VStat, BOOL BoundsPresent, ...)
{
	VEC_STAT_BODY(FVec, IS_E_NULL)
} /* VectorStatistics_F */

CalcError_t VectorStatistics_D(const double DVec[], unsigned Size,
							 VecStat_t *VStat, BOOL BoundsPresent, ...)
{
	VEC_STAT_BODY(DVec, IS_D_NULL)
} /* VectorStatistics_D */
