/* $Id: aste_coord_xrt.c,v 1.12 2007/05/07 16:45:32 ishisaki Exp $ */
/************************************************************************
  aste_coord_xrt.c

  aste_xrt_pol2rec	convert XRT theta[arcmin]/phi[deg] -> XRTX/Y[mm]
  aste_xrt_rec2pol	convert XRTX/Y[mm] -> XRT theta[arcmin]/phi[deg]
  aste_xrt2det		convert XRTX/Y[mm] (look-down) -> DETX/Y[ch]
  aste_det2xrt		convert DETX/Y[ch] -> XRTX/Y[mm] (look-down)

  1999-12-19	Y.ISHISAKI

  2003-09-12	Y.ISHISAKI
	bug fix in aste_det2xrt() for xrty_mm

  2006-08-01	Y.ISHISAKI	version 1.81
	rename aste_pol2rec() -> aste_xrt_pol2rec()
	define aste_pol2rec, aste_rec2pol for backward compatibility

************************************************************************/

#include <stdio.h>
#include <math.h>
#include "atFunctions.h"
#include "aste_coord.h"

/************************************************************************
  aste_xrt_pol2rec	convert XRT theta[arcmin]/phi[deg] -> XRTX/Y[mm]
************************************************************************/
int
aste_xrt_pol2rec(
	TELDEF *teldef, double theta_min, double phi_deg,	/* input */
	double *xrtx_mm, double *xrty_mm					/* output*/
)
{
	double focallen = teldef->mission.aste->focallen;
	double tan_theta = tan(theta_min * ARCMIN2RAD);
	double phi = phi_deg * DEG2RAD;

	*xrtx_mm = focallen * tan_theta * cos(phi);
	*xrty_mm = focallen * tan_theta * sin(phi);

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_xrt_rec2pol	convert XRTX/Y[mm] -> XRT theta[arcmin]/phi[deg]
************************************************************************/
int
aste_xrt_rec2pol(
	TELDEF *teldef, double xrtx_mm, double xrty_mm,		/* input */
	double *theta_min, double *phi_deg					/* output*/
)
{
	double focallen = teldef->mission.aste->focallen;
	double r_mm = sqrt(xrtx_mm * xrtx_mm + xrty_mm * xrty_mm);

	*theta_min = atan2(r_mm, focallen) * RAD2ARCMIN;
	*phi_deg   = atan2(xrty_mm, xrtx_mm) * RAD2DEG;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_xrt2det		convert XRTX/Y[mm] (look-down) -> DETX/Y[ch]
************************************************************************/
int
aste_xrt2det(
	TELDEF *teldef, double xrtx_mm, double xrty_mm,		/* input */
	double *detx_ch, double *dety_ch					/* output*/
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;

	*detx_ch = p->optaxisx + xrtx_mm / p->det.xscl;
	*dety_ch = p->optaxisy - xrty_mm / p->det.yscl;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_det2xrt		convert DETX/Y[ch] -> XRTX/Y[mm] (look-down)
************************************************************************/
int
aste_det2xrt(
	TELDEF *teldef, double detx_ch, double dety_ch,		/* input */
	double *xrtx_mm, double *xrty_mm					/* output*/
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;

	*xrtx_mm =   p->det.xscl * (detx_ch - p->optaxisx);
	*xrty_mm = - p->det.yscl * (dety_ch - p->optaxisy);

	return ASTE_COORD_NORMAL_END;
}
