/* $Id: aste_coord_core.c,v 1.12 2007/05/07 16:45:32 ishisaki Exp $ */
/************************************************************************
  aste_coord_core.c

  aste_det2foc		convert DETX/Y[ch] -> FOCX/Y[ch]
  aste_foc2det		convert FOCX/Y[ch] -> DETX/Y[ch]
  aste_foc2ecs		convert FOCX/Y[ch] -> (RA,DEC)[deg]
  aste_ecs2foc		convert (RA,DEC)[deg] -> FOCX/Y[ch]
  aste_ecs2sky		convert (RA,DEC)[deg] -> SKYX/Y[ch]
  aste_sky2ecs		convert SKYX/Y[ch] -> (RA,DEC)[deg]
  aste_euler2skyref	calculate SKYREF [deg] from Euler angles [radian]
  aste_skyref2euler	determin Euler angles [radian] from SKYREF [deg]
  aste_foc2sky		convert FOCX/Y[ch] -> SKYX/Y[ch]
  aste_sky2foc		convert SKYX/Y[ch] -> FOCX/Y[ch]
  aste_det2sky		convert DETX/Y[ch] -> SKYX/Y[ch]
  aste_sky2det		convert SKYX/Y[ch] -> DETX/Y[ch]
  aste_det2ecs		convert DETX/Y[ch] -> (RA,DEC)[deg]
  aste_ecs2det		convert (RA,DEC)[deg] -> DETX/Y[ch]
  aste_det2det		convert DETX/Y[ch] between sensors, sensor0 -> sensor1

  aste_cor_aberration	aberration correction  (mission independent)
  aste_inv_aberration	inverse aberration correction (mission independent)

  1999-12-19	Y.ISHISAKI

  2005-06-16	Y.ISHISAKI	version 1.51
	return ASTE_COORD_OVER_90DEG if opposite direction in aste_ecs2foc/sky()

  2005-10-27	Y.ISHISAKI	version 1.72
	add 360.0 to skyref->roll if negative in aste_euler2skyref()

  2006-08-01	Y.ISHISAKI	version 1.80
	add aste_cor_aberration(), aste_inv_aberration() [mission independent]

************************************************************************/

#include <stdio.h>
#include <math.h>
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_time.h"

/************************************************************************
  aste_det2foc		convert DETX/Y[ch] -> FOCX/Y[ch]
************************************************************************/
int
aste_det2foc(
	TELDEF *teldef, double detx_ch, double dety_ch,		/* input */
	double *focx_ch, double *focy_ch					/* output*/
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;
	double tmpx_ch = detx_ch - p->det.xcen - p->foc_xoff;
	double tmpy_ch = dety_ch - p->det.ycen - p->foc_yoff;
	double cos_foc_rotd = p->cos_foc_rotd;
	double sin_foc_rotd = p->sin_foc_rotd;

	*focx_ch = p->foc.xcen + cos_foc_rotd * tmpx_ch - sin_foc_rotd * tmpy_ch;
	*focy_ch = p->foc.ycen + sin_foc_rotd * tmpx_ch + cos_foc_rotd * tmpy_ch;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_foc2det		convert FOCX/Y[ch] -> DETX/Y[ch]
************************************************************************/
int
aste_foc2det(
	TELDEF *teldef, double focx_ch, double focy_ch,		/* input */
	double *detx_ch, double *dety_ch					/* output*/
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;
	double x = focx_ch - p->foc.xcen;
	double y = focy_ch - p->foc.ycen;
	double cos_foc_rotd = p->cos_foc_rotd;
	double sin_foc_rotd = p->sin_foc_rotd;

	*detx_ch = p->det.xcen + p->foc_xoff + cos_foc_rotd * x + sin_foc_rotd * y;
	*dety_ch = p->det.ycen + p->foc_yoff - sin_foc_rotd * x + cos_foc_rotd * y;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_foc2ecs		convert FOCX/Y[ch] -> (RA,DEC)[deg]
	Unit of the Eulder angles is radian.
************************************************************************/
int
aste_foc2ecs(
	TELDEF *teldef, AtEulerAng *ea, double focx_ch, double focy_ch,	/* input */
	double *alpha, double *delta									/* output*/
)
{
	double norm;
	AtVect vec_ecs;		/* pointing vector in celestial coordinates */
	AtVect vec_sat;		/* pointing vector in satellite coordinates */
	AtVect vec_foc;		/* pointing vector in FOC coordinates */
	AtRotMat Aij, invAij;	/* rotation matrix to describe satellite aspect */

	TELDEF_ASTROE *p = teldef->mission.aste;

/* convert Euler angles to a rotation matrix, then obtain its inverse matrix */
	atEulerToRM(ea, Aij);
	atInvRotMat(Aij, invAij);

/* convert to a pointing vector in FOC coordinate,
   flipping y-axis to make it look-down */
	vec_foc[0] = - (focx_ch - p->foc.xcen) * p->foc.xscl;
	vec_foc[1] =   (focy_ch - p->foc.ycen) * p->foc.yscl;
	vec_foc[2] =    p->focallen;

/* convert to the pointing vector in SAT coordinate,
   taking account of instrument offsets */
	atRotVect(p->invMij, vec_foc, vec_sat);

/* convert to the pointing vector in celestial coordinates,
   taking account of satellite attitudes */
	atRotVect(invAij, vec_sat, vec_ecs);

/* convert to the RA and DEC */
	atVectToPolDeg(vec_ecs, &norm, alpha, delta);

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_ecs2foc		convert (RA,DEC)[deg] -> FOCX/Y[ch]
	Unit of the Eulder angles is radian.

  RETURN
	ASTE_COORD_DIVIDE_BY_ZERO: division by zero, probably invalid Euler angles
************************************************************************/
int
aste_ecs2foc(
	TELDEF *teldef, AtEulerAng *ea, double alpha, double delta,	/* input */
	double *focx_ch, double *focy_ch							/* output*/
)
{
	double norm;
	AtVect vec_ecs;		/* pointing vector in celestial coordinates */
	AtVect vec_sat;		/* pointing vector in satellite coordinates */
	AtVect vec_foc;		/* pointing vector in FOC coordinates */
	AtRotMat Aij;		/* rotation matrix to describe satellite aspect */

	TELDEF_ASTROE *p = teldef->mission.aste;

/* convert Euler angles to a rotation matrix */
	atEulerToRM(ea, Aij);

/* convert (alpha, delta) to a vector in celestial coordinates */
	atPolDegToVect(1.0, alpha, delta, vec_ecs);

/* convert celestical coordinates to satellite coordinates]*/
	atRotVect(Aij, vec_ecs, vec_sat);

/* correct instrumental offsets */
	atRotVect(p->Mij, vec_sat, vec_foc);

/* convert to FOC coordinates, flipping y-axis to make it look-up */
	if ( 0.0 == vec_foc[2] ) {
		return ASTE_COORD_DIVIDE_BY_ZERO;
	}
	norm = p->focallen / vec_foc[2];
	*focx_ch = p->foc.xcen - norm * vec_foc[0] / p->foc.xscl;
	*focy_ch = p->foc.ycen + norm * vec_foc[1] / p->foc.yscl;

/* check if opposite direction */
	if ( vec_foc[2] < 0 ) {
		return ASTE_COORD_OVER_90DEG;	/* added in v1.51 */
	}

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_ecs2sky		convert (RA,DEC)[deg] -> SKYX/Y[ch]

  RETURN
	ASTE_COORD_DIVIDE_BY_ZERO: division by zero, probably invalid SKYREF
************************************************************************/
int
aste_ecs2sky(
	TELDEF *teldef, SKYREF *skyref, double alpha, double delta,	/* input */
	double *skyx_ch, double *skyy_ch							/* output*/
)
{
	double norm;
	AtEulerAng ea;
	AtRotMat Bij;		/* rotation matrix to describe SKYREF */
	AtVect vec_ecs;		/* pointing vector in celestial coordinates */
	AtVect vec_sky;		/* pointing vector in sky coordinates */

	TELDEF_ASTROE *p = teldef->mission.aste;

/* convert alpha, delta and roll to Euler angles */
	ea.phi = skyref->alpha * DEG2RAD;
	ea.theta = M_PI_2 - skyref->delta * DEG2RAD;
	ea.psi = M_PI_2 - skyref->roll * DEG2RAD;

/* convert Euler angles to a rotation matrix */
	atEulerToRM(&ea, Bij);

/* convert alpha and delta to a pointing vector in celestial coordinates */
	atPolDegToVect(1.0, alpha, delta, vec_ecs);

/* convert celestial coordinates to sky coordinates */
	atRotVect(Bij, vec_ecs, vec_sky);

/* convert to the SKY coordinates, flipping y-axis to make it look-up */
	if ( 0.0 == vec_sky[2] ) {
		return ASTE_COORD_DIVIDE_BY_ZERO;
	}
	norm = p->focallen / vec_sky[2];
	*skyx_ch = p->sky.xcen - norm * vec_sky[0] / p->sky.xscl;
	*skyy_ch = p->sky.ycen + norm * vec_sky[1] / p->sky.yscl;

/* check if opposite direction */
	if ( vec_sky[2] < 0 ) {
		return ASTE_COORD_OVER_90DEG;	/* added in v1.51 */
	}

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_sky2ecs		convert SKYX/Y[ch] -> (RA,DEC)[deg]
	If aberration correection required, give aberration corrected RA and DEC.
************************************************************************/
int
aste_sky2ecs(
	TELDEF *teldef, SKYREF *skyref, double skyx_ch, double skyy_ch,
	double *alpha, double *delta
)
{
	double norm;
	AtEulerAng ea;
	AtRotMat Bij, invBij;	/* rotation matrix to describe SKYREF */
	AtVect vec_ecs;		/* pointing vector in celestial coordinates */
	AtVect vec_sky;		/* pointing vector in sky coordinates */

	TELDEF_ASTROE *p = teldef->mission.aste;

/* convert alpha, delta and roll to Euler angles */
	ea.phi = skyref->alpha * DEG2RAD;
	ea.theta = M_PI_2 - skyref->delta * DEG2RAD;
	ea.psi = M_PI_2 - skyref->roll * DEG2RAD;

/* convert Euler angles to a rotation matrix, then obtain an inverse matrix */
	atEulerToRM(&ea, Bij);
	atInvRotMat(Bij, invBij);

/* convert to a pointing vector in sky coordinates,
   flipping y-axis to make it look-down */
	vec_sky[0] = - (skyx_ch - p->sky.xcen) * p->sky.xscl;
	vec_sky[1] =   (skyy_ch - p->sky.ycen) * p->sky.yscl;
	vec_sky[2] =    p->focallen;

/* convert to a pointing vector in celestial coordinates, using skyref */
	atRotVect(invBij, vec_sky, vec_ecs);

/* convert to the RA and DEC */
	atVectToPolDeg(vec_ecs, &norm, alpha, delta);

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_euler2skyref	calculate SKYREF [deg] from Euler angles [radian],
	which makes SKY coordinates identical to FOC coordinates
************************************************************************/
int
aste_euler2skyref(
	TELDEF *teldef, AtEulerAng *ea,	/* input */
	SKYREF *skyref					/* output*/
)
{
	AtRotMat Aij;	/* rotation matrix to describe satellite aspect */
	AtRotMat Fij;
	AtEulerAng fov;

	TELDEF_ASTROE *p = teldef->mission.aste;

/* convert Euler angles to a rotation matrix */
	atEulerToRM(ea, Aij);

/* multiply aligment matrix, and convert to FOC rotation matrix */
	atRMProd(Aij, p->Mij, Fij);

/* determine instrument Euler angles = FOV */
	atRMToEuler(Fij, &fov);

	skyref->alpha = fov.phi * RAD2DEG;				/* 0 <= phi < 2 pi */
	skyref->delta = (M_PI_2 - fov.theta) * RAD2DEG;	/* 0 <= theta < pi */
	skyref->roll  = (M_PI_2 - fov.psi) * RAD2DEG;	/* 0 <= psi < 2 pi */

/* positive definition of roll angle */
	if ( skyref->roll < 0.0 ) {
		skyref->roll += 360.0;
	}

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_skyref2euler	determin Euler angles [radian] from SKYREF [deg],
	which makes SKY coordinates identical to FOC coordinates
************************************************************************/
int
aste_skyref2euler(
	TELDEF *teldef, SKYREF *skyref,		/* input */
	AtEulerAng *ea						/* output*/
)
{
	AtEulerAng fov;
	AtRotMat Fij;
	AtRotMat Aij;

	TELDEF_ASTROE *p = teldef->mission.aste;

/* convert SKYREF to instrument Euler angles */
	fov.phi = skyref->alpha * DEG2RAD;
	fov.theta = M_PI_2 - skyref->delta * DEG2RAD;
	fov.psi = M_PI_2 - skyref->roll * DEG2RAD;

/* convert instrument Euler Angles to a rotation matrix */
	atEulerToRM(&fov, Fij);

/* multiply the inverse matrix of the alignment matrix,
   and convert to the satellite rotation matrix */
	atRMProd(Fij, p->invMij, Aij);

/* convert rotation matrix to Euler angles */
	atRMToEuler(Aij, ea);

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_foc2sky		convert FOCX/Y[ch] -> SKYX/Y[ch]
************************************************************************/
int
aste_foc2sky(
	TELDEF *teldef, AtEulerAng *ea, SKYREF *skyref,
	double focx_ch, double focy_ch,						/* input */
	double *skyx_ch, double *skyy_ch					/* output*/
)
{
	int istat;
	double alpha, delta;

	istat = aste_foc2ecs(teldef, ea, focx_ch, focy_ch, &alpha, &delta);
	if ( istat ) return istat;
	istat = aste_ecs2sky(teldef, skyref, alpha, delta, skyx_ch, skyy_ch);

	return istat;
}

/************************************************************************
  aste_sky2foc		convert SKYX/Y[ch] -> FOCX/Y[ch]
************************************************************************/
int
aste_sky2foc(
	TELDEF *teldef, AtEulerAng *ea, SKYREF *skyref,
	double skyx_ch, double skyy_ch,						/* input */
	double *focx_ch, double *focy_ch					/* output*/
)
{
	int istat;
	double alpha, delta;

	istat = aste_sky2ecs(teldef, skyref, skyx_ch, skyy_ch, &alpha, &delta);
	if ( istat ) return istat;
	istat = aste_ecs2foc(teldef, ea, alpha, delta, focx_ch, focy_ch);

	return istat;
}

/************************************************************************
  aste_det2sky		convert DETX/Y[ch] -> SKYX/Y[ch]
************************************************************************/
int
aste_det2sky(
	TELDEF *teldef, AtEulerAng *ea, SKYREF *skyref,
	double detx_ch, double dety_ch,						/* input */
	double *skyx_ch, double *skyy_ch					/* output*/
)
{
	int istat;
	double focx_ch, focy_ch, alpha, delta;

	istat = aste_det2foc(teldef, detx_ch, dety_ch, &focx_ch, &focy_ch);
	if ( istat ) return istat;
	istat = aste_foc2ecs(teldef, ea, focx_ch, focy_ch, &alpha, &delta);
	if ( istat ) return istat;
	istat = aste_ecs2sky(teldef, skyref, alpha, delta, skyx_ch, skyy_ch);

	return istat;
}

/************************************************************************
  aste_sky2det		convert SKYX/Y[ch] -> DETX/Y[ch]
************************************************************************/
int
aste_sky2det(
	TELDEF *teldef, AtEulerAng *ea, SKYREF *skyref,
	double skyx_ch, double skyy_ch,						/* input */
	double *detx_ch, double *dety_ch					/* output*/
)
{
	int istat;
	double alpha, delta, focx_ch, focy_ch;

	istat = aste_sky2ecs(teldef, skyref, skyx_ch, skyy_ch, &alpha, &delta);
	if ( istat ) return istat;
	istat = aste_ecs2foc(teldef, ea, alpha, delta, &focx_ch, &focy_ch);
	if ( istat ) return istat;
	istat = aste_foc2det(teldef, focx_ch, focy_ch, detx_ch, dety_ch);

	return istat;
}

/************************************************************************
  aste_det2ecs		convert DETX/Y[ch] -> (RA,DEC)[deg]
************************************************************************/
int
aste_det2ecs(
	TELDEF *teldef, AtEulerAng *ea, double detx_ch, double dety_ch,	/* input */
	double *alpha, double *delta									/* output*/
)
{
	int istat;
	double focx_ch, focy_ch;

	istat = aste_det2foc(teldef, detx_ch, dety_ch, &focx_ch, &focy_ch);
	if ( istat ) return istat;
	istat = aste_foc2ecs(teldef, ea, focx_ch, focy_ch, alpha, delta);

	return istat;
}

/************************************************************************
  aste_ecs2det		convert (RA,DEC)[deg] -> DETX/Y[ch]
************************************************************************/
int
aste_ecs2det(
	TELDEF *teldef, AtEulerAng *ea, double alpha, double delta,	/* input */
	double *detx_ch, double *dety_ch							/* output*/
)
{
	int istat;
	double focx_ch, focy_ch;

	istat = aste_ecs2foc(teldef, ea, alpha, delta, &focx_ch, &focy_ch);
	if ( istat ) return istat;
	istat = aste_foc2det(teldef, focx_ch, focy_ch, detx_ch, dety_ch);

	return istat;
}

/************************************************************************
  aste_det2det		convert DETX/Y[ch] between sensors, sensor0 -> sensor1
************************************************************************/
int
aste_det2det(
	TELDEF *teldef0, double  detx0_ch, double  dety0_ch,	/* source */
	TELDEF *teldef1, double *detx1_ch, double *dety1_ch		/* destination */
)
{
	int istat;
	double focx_ch, focy_ch;

	if ( teldef0->id == teldef1->id ) {
		*detx1_ch = detx0_ch;
		*dety1_ch = dety0_ch;
		return ASTE_COORD_NORMAL_END;
	}

	istat = aste_det2foc(teldef0, detx0_ch, dety0_ch, &focx_ch, &focy_ch);
	if ( istat ) return istat;
	istat = aste_foc2det(teldef1, focx_ch, focy_ch, detx1_ch, dety1_ch);

	return istat;
}

/************************************************************************
  aste_cor_aberration	aberration correction  (mission independent)
************************************************************************/
int
aste_cor_aberration(
	double astetime, int mjdrefi, double mjdreff,	/* input */
	double *alphaInOut, double *deltaInOut			/* input/output */
)
{
	double mjd_tt;
	AtVect v0, v1;
	double r;

	mjd_tt = aste2mjdtt(astetime, mjdrefi, mjdreff);
	atPolDegToVect(1.0, *alphaInOut, *deltaInOut, v0);
	atAberration(mjd_tt, v0, v1);
	atVectToPolDeg(v1, &r, alphaInOut, deltaInOut);

	return 0;
}

/************************************************************************
  aste_inv_aberration	inverse aberration correction (mission independent)
************************************************************************/
int
aste_inv_aberration(
	double astetime, int mjdrefi, double mjdreff,	/* input */
	double *alphaInOut, double *deltaInOut			/* input/output */
)
{
	double mjd_tt;
	AtVect v0, v1;
	double r;

	mjd_tt = aste2mjdtt(astetime, mjdrefi, mjdreff);
	atPolDegToVect(1.0, *alphaInOut, *deltaInOut, v0);
	atInvAberration(mjd_tt, v0, v1);
	atVectToPolDeg(v1, &r, alphaInOut, deltaInOut);

	return 0;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
