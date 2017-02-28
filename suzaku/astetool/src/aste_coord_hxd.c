/* $Id: aste_coord_hxd.c,v 1.14 2007/05/07 16:45:32 ishisaki Exp $ */
/************************************************************************
  aste_coord_hxd.c

  hxd_pin2det		get DETX/Y[ch] of PIN 0-63
  hxd_gso2det		get DETX/Y[ch] of GSO 0-15
  hxd_pin_ecs2pol	convert (RA,DEC)[deg] -> theta[arcmin]/phi[deg] (look-down)
  hxd_pin_pol2ecs	convert theta[arcmin]/phi[deg] (look-down) -> (RA,DEC)[deg]
  hxd_gso_ecs2pol	convert (RA,DEC)[deg] -> theta[arcmin]/phi[deg] (look-down)
  hxd_gso_pol2ecs	convert theta[arcmin]/phi[deg] (look-down) -> (RA,DEC)[deg]

  1999-12-19	Y.ISHISAKI

  2000-01-28	Y.ISHISAKI
	fix comments on hxd_pin/gso_ecs2pol/pol2ecs
************************************************************************/

#include <stdio.h>
#include <math.h>
#include "atFunctions.h"
#include "aste_coord.h"

/************************************************************************
  hxd_pin2det		get DETX/Y[ch] of PIN 0-63
************************************************************************/
int
hxd_pin2det(
	TELDEF *teldef,		/* input: teldef file of HXD */
	int pin_id,			/* input: ID number of PIN [0-63] */
	double *detx_ch,	/* output: DETX [ch] */
	double *dety_ch		/* output: DETY [ch] */
)
{
	static char pname[] = "hxd_pin2det";
	TELDEF_ASTROE *p = teldef->mission.aste;
	struct pin_alignment *pin = &p->pin[pin_id];

	if ( ASTE_HXD_SENSOR != teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not HXD (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	if ( pin_id < 0 || 64 <= pin_id ) {
		fprintf(stderr, "\
%s: invalid PIN ID of %d\n", pname, pin_id);
		return ASTE_COORD_INVALID_PIXEL;
	}

	*detx_ch = p->det.xcen
		+ p->detxflip * (pin->intx - p->int_xcen - p->det_xoff) / p->det_scal;
	*dety_ch = p->det.ycen
		+ p->detyflip * (pin->inty - p->int_ycen - p->det_yoff) / p->det_scal;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  hxd_gso2det		get DETX/Y[ch] of GSO 0-15
************************************************************************/
int
hxd_gso2det(
	TELDEF *teldef,		/* input: teldef file of HXD */
	int gso_id,			/* input: ID number of GSO [0-15] */
	double *detx_ch,	/* output: DETX [ch] */
	double *dety_ch		/* output: DETY [ch] */
)
{
	static char pname[] = "hxd_gso2det";
	TELDEF_ASTROE *p = teldef->mission.aste;
	struct gso_alignment *gso = &p->gso[gso_id];

	if ( ASTE_HXD_SENSOR != teldef->id ) {
		fprintf(stderr, "\
%s: TELDEF is not HXD (%d)\n", pname, teldef->id);
		return ASTE_COORD_INVALID_SENSOR;
	}

	if ( gso_id < 0 || 16 <= gso_id ) {
		fprintf(stderr, "\
%s: invalid GSO ID of %d\n", pname, gso_id);
		return ASTE_COORD_INVALID_PIXEL;
	}

	*detx_ch = p->det.xcen
		+ p->detxflip * (gso->intx - p->int_xcen - p->det_xoff) / p->det_scal;
	*dety_ch = p->det.ycen
		+ p->detyflip * (gso->inty - p->int_ycen - p->det_yoff) / p->det_scal;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  hxd_pin_ecs2pol	convert (RA,DEC)[deg] -> theta[arcmin]/phi[deg] (look-down)
************************************************************************/
int
hxd_pin_ecs2pol(
	TELDEF *teldef,		/* input: teldef file of HXD */
	AtEulerAng *ea,		/* input: Euler angles [radian] of satellite */
	int pin_id,			/* input: ID number of PIN [0-63] */
	double alpha,		/* input: RA [deg] */
	double delta,		/* input: DEC [deg] */
	double *theta_min,	/* output: theta [arcmin], defined in look-down */
	double *phi_deg		/* output: phi [deg], defined in look-down  */
)
{
	int istat;
	double detx_pin, dety_pin, detx_ch, dety_ch, dx_mm, dy_mm, dr_mm;

	TELDEF_ASTROE *p = teldef->mission.aste;

	istat = hxd_pin2det(teldef, pin_id, &detx_pin, &dety_pin);
	if ( istat ) return istat;

	istat = aste_ecs2det(teldef, ea, alpha, delta, &detx_ch, &dety_ch);
	if ( istat ) return istat;

	dx_mm =   (detx_ch - detx_pin) * p->det.xscl;
	dy_mm = - (dety_ch - dety_pin) * p->det.yscl;	/* look-up -> look-down */

	if ( 0.0 == dx_mm && 0.0 == dy_mm ) {
		*theta_min = 0.0;
		*phi_deg = 0.0;
	} else {
		dr_mm = sqrt(dx_mm * dx_mm + dy_mm * dy_mm);
		*theta_min = atan2(dr_mm, p->focallen) * RAD2ARCMIN;
		*phi_deg   = atan2(dy_mm, dx_mm) * RAD2DEG;
	}

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  hxd_pin_pol2ecs	convert theta[arcmin]/phi[deg] (look-down) -> (RA,DEC)[deg]
************************************************************************/
int
hxd_pin_pol2ecs(
	TELDEF *teldef,		/* input: teldef file of HXD */
	AtEulerAng *ea,		/* input: Euler angles [radian] of satellite */
	int pin_id,			/* input: ID number of PIN [0-63] */
	double theta_min,	/* input: theta [arcmin], defined in look-down */
	double phi_deg,		/* input: phi [deg], defined in look-down  */
	double *alpha,		/* output: RA [deg] */
	double *delta		/* output: DEC [deg] */
)
{
	int istat;
	double detx_pin, dety_pin, detx_ch, dety_ch, dx_mm, dy_mm;

	TELDEF_ASTROE *p = teldef->mission.aste;
	double focallen = p->focallen;
	double tan_theta = tan(theta_min * ARCMIN2RAD);
	double phi = phi_deg * DEG2RAD;

	istat = hxd_pin2det(teldef, pin_id, &detx_pin, &dety_pin);
	if ( istat ) return istat;

	dx_mm = focallen * tan_theta * cos(phi);
	dy_mm = focallen * tan_theta * sin(phi);

	detx_ch = detx_pin + dx_mm / p->det.xscl;
	dety_ch = dety_pin - dy_mm / p->det.yscl;	/* look-down -> look-up */

	istat = aste_det2ecs(teldef, ea, detx_ch, dety_ch, alpha, delta);

	return istat;
}

/************************************************************************
  hxd_gso_ecs2pol	convert (RA,DEC)[deg] -> theta[arcmin]/phi[deg] (look-down)
************************************************************************/
int
hxd_gso_ecs2pol(
	TELDEF *teldef,		/* input: teldef file of HXD */
	AtEulerAng *ea,		/* input: Euler angles [radian] of satellite */
	int gso_id,			/* input: ID number of GSO [0-15] */
	double alpha,		/* input: RA [deg] */
	double delta,		/* input: DEC [deg] */
	double *theta_min,	/* output: theta [arcmin], defined in look-down */
	double *phi_deg		/* output: phi [deg], defined in look-down  */
)
{
	int istat;
	double detx_gso, dety_gso, detx_ch, dety_ch, dx_mm, dy_mm, dr_mm;

	TELDEF_ASTROE *p = teldef->mission.aste;

	istat = hxd_gso2det(teldef, gso_id, &detx_gso, &dety_gso);
	if ( istat ) return istat;

	istat = aste_ecs2det(teldef, ea, alpha, delta, &detx_ch, &dety_ch);
	if ( istat ) return istat;

	dx_mm =   (detx_ch - detx_gso) * p->det.xscl;
	dy_mm = - (dety_ch - dety_gso) * p->det.yscl;	/* look-up -> look-down */

	if ( 0.0 == dx_mm && 0.0 == dy_mm ) {
		*theta_min = 0.0;
		*phi_deg = 0.0;
	} else {
		dr_mm = sqrt(dx_mm * dx_mm + dy_mm * dy_mm);
		*theta_min = atan2(dr_mm, p->focallen) * RAD2ARCMIN;
		*phi_deg   = atan2(dy_mm, dx_mm) * RAD2DEG;
	}

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  hxd_gso_pol2ecs	convert theta[arcmin]/phi[deg] (look-down) -> (RA,DEC)[deg]
************************************************************************/
int
hxd_gso_pol2ecs(
	TELDEF *teldef,		/* input: teldef file of HXD */
	AtEulerAng *ea,		/* input: Euler angles [radian] of satellite */
	int gso_id,			/* input: ID number of GSO [0-15] */
	double theta_min,	/* input: theta [arcmin], defined in look-down */
	double phi_deg,		/* input: phi [deg], defined in look-down  */
	double *alpha,		/* output: RA [deg] */
	double *delta		/* output: DEC [deg] */
)
{
	int istat;
	double detx_gso, dety_gso, detx_ch, dety_ch, dx_mm, dy_mm;

	TELDEF_ASTROE *p = teldef->mission.aste;
	double focallen = p->focallen;
	double tan_theta = tan(theta_min * ARCMIN2RAD);
	double phi = phi_deg * DEG2RAD;

	istat = hxd_gso2det(teldef, gso_id, &detx_gso, &dety_gso);
	if ( istat ) return istat;

	dx_mm = focallen * tan_theta * cos(phi);
	dy_mm = focallen * tan_theta * sin(phi);

	detx_ch = detx_gso + dx_mm / p->det.xscl;
	dety_ch = dety_gso - dy_mm / p->det.yscl;	/* look-down -> look-up */

	istat = aste_det2ecs(teldef, ea, detx_ch, dety_ch, alpha, delta);

	return istat;
}
