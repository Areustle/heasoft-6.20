/* $Id: aste_coord_ch2mm.c,v 1.12 2007/05/07 16:45:32 ishisaki Exp $ */
/************************************************************************
  aste_coord_ch2mm.c

  aste_det_ch2mm	convert DETX/Y[ch] -> DETX/Y[mm]
  aste_det_mm2ch	convert DETX/Y[mm] -> DETX/Y[ch]
  aste_foc_ch2mm	convert FOCX/Y[ch] -> FOCX/Y[mm]
  aste_foc_mm2ch	convert FOCX/Y[mm] -> FOCX/Y[ch]
  aste_sky_ch2mm	convert SKYX/Y[ch] -> SKYX/Y[mm]
  aste_sky_mm2ch	convert SKYX/Y[mm] -> SKYX/Y[ch]

  1999-12-19	Y.ISHISAKI
************************************************************************/

#include <stdio.h>
#include <math.h>
#include "atFunctions.h"
#include "aste_coord.h"

/************************************************************************
  aste_det_ch2mm	convert DETX/Y[ch] -> DETX/Y[mm]
************************************************************************/
int
aste_det_ch2mm(
	TELDEF *teldef, double detx_ch, double dety_ch,
	double *detx_mm, double *dety_mm
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;

	*detx_mm = p->det.xscl * (detx_ch -  p->det.xcen);
	*dety_mm = p->det.yscl * (dety_ch -  p->det.ycen);

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_det_mm2ch	convert DETX/Y[mm] -> DETX/Y[ch]
************************************************************************/
int
aste_det_mm2ch(
	TELDEF *teldef, double detx_mm, double dety_mm,
	double *detx_ch, double *dety_ch
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;

	*detx_ch = p->det.xcen + detx_mm / p->det.xscl;
	*dety_ch = p->det.ycen + dety_mm / p->det.yscl;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_foc_ch2mm	convert FOCX/Y[ch] -> FOCX/Y[mm]
************************************************************************/
int
aste_foc_ch2mm(
	TELDEF *teldef, double focx_ch, double focy_ch,
	double *focx_mm, double *focy_mm
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;

	*focx_mm = p->foc.xscl * (focx_ch -  p->foc.xcen);
	*focy_mm = p->foc.yscl * (focy_ch -  p->foc.ycen);

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_foc_mm2ch	convert FOCX/Y[mm] -> FOCX/Y[ch]
************************************************************************/
int
aste_foc_mm2ch(
	TELDEF *teldef, double focx_mm, double focy_mm,
	double *focx_ch, double *focy_ch
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;

	*focx_ch = p->foc.xcen + focx_mm / p->foc.xscl;
	*focy_ch = p->foc.ycen + focy_mm / p->foc.yscl;

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_sky_ch2mm	convert SKYX/Y[ch] -> SKYX/Y[mm]
************************************************************************/
int
aste_sky_ch2mm(
	TELDEF *teldef, double skyx_ch, double skyy_ch,
	double *skyx_mm, double *skyy_mm
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;

	*skyx_mm = p->sky.xscl * (skyx_ch -  p->sky.xcen);
	*skyy_mm = p->sky.yscl * (skyy_ch -  p->sky.ycen);

	return ASTE_COORD_NORMAL_END;
}

/************************************************************************
  aste_sky_mm2ch	convert SKYX/Y[mm] -> SKYX/Y[ch]
************************************************************************/
int
aste_sky_mm2ch(
	TELDEF *teldef, double skyx_mm, double skyy_mm,
	double *skyx_ch, double *skyy_ch
)
{
	TELDEF_ASTROE *p = teldef->mission.aste;

	*skyx_ch = p->sky.xcen + skyx_mm / p->sky.xscl;
	*skyy_ch = p->sky.ycen + skyy_mm / p->sky.yscl;

	return ASTE_COORD_NORMAL_END;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
