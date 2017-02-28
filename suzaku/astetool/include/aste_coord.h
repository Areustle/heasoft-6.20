/* $Id: aste_coord.h,v 1.12 2006/08/12 15:49:46 ishisaki Exp $ */
/************************************************************************
  aste_coord_init	aste_coord initialization function
  aste_coord_free	free allocated memory for TELDEF structure
  aste_coord_teldef	return already allocated TELDEF structure

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

  aste_det_ch2mm	convert DETX/Y[ch] -> DETX/Y[mm]
  aste_det_mm2ch	convert DETX/Y[mm] -> DETX/Y[ch]
  aste_foc_ch2mm	convert FOCX/Y[ch] -> FOCX/Y[mm]
  aste_foc_mm2ch	convert FOCX/Y[mm] -> FOCX/Y[ch]
  aste_sky_ch2mm	convert SKYX/Y[ch] -> SKYX/Y[mm]
  aste_sky_mm2ch	convert SKYX/Y[mm] -> SKYX/Y[ch]

  aste_xrt_pol2rec	convert XRT theta[arcmin]/phi[deg] -> XRTX/Y[mm]
  aste_xrt_rec2pol	convert XRTX/Y[mm] -> XRT theta[arcmin]/phi[deg]
  aste_xrt2det		convert XRTX/Y[mm] (look-down) -> DETX/Y[ch]
  aste_det2xrt		convert DETX/Y[ch] -> XRTX/Y[mm] (look-down)

  xrs_pixel2det		convert XRS PIXEL to DETX/Y[ch]
  xrs_det2pixel		convert XRS DETX/Y[ch] to PIXEL

  xis_ppu2raw		convert XIS PPUX/Y[ch] -> RAWX/Y[ch]
  xis_raw2ppu		convert XIS RAWX/Y[ch] -> PPUX/Y[ch]
  xis_raw2act		convert XIS RAWX/Y[ch] -> ACTX/Y[ch]
  xis_act2raw		convert XIS ACTX/Y[ch] -> RAWX/Y[ch]
  xis_act2det		convert XIS ACTX/Y[ch] -> DETX/Y[ch]
  xis_det2act		convert XIS DETX/Y[ch] -> ACTX/Y[ch]
  xis_ppu2raw		convert XIS PPUX/Y[ch] -> RAWX/Y[ch]
  xis_raw2ppu		convert XIS RAWX/Y[ch] -> PPUX/Y[ch]

  hxd_pin2det		get DETX/Y[ch] of PIN 0-63
  hxd_gso2det		get DETX/Y[ch] of GSO 0-15
  hxd_pin_ecs2pol	convert (RA,DEC)[deg] -> theta[arcmin]/phi[deg] (look-down)
  hxd_pin_pol2ecs	convert theta[arcmin]/phi[deg] (look-down) -> (RA,DEC)[deg]
  hxd_gso_ecs2pol	convert (RA,DEC)[deg] -> theta[arcmin]/phi[deg] (look-down)
  hxd_gso_pol2ecs	convert theta[arcmin]/phi[deg] (look-down) -> (RA,DEC)[deg]

	1999-12-19	Y.ISHISAKI	version 1.0

	2005-05-27	Y.ISHISAKI	version 1.50
		change AstEsensorID 29xx -> 38xx for Astro-E2

	2005-06-16	Y.ISHISAKI	version 1.51
		add ASTE_COORD_OVER_90DEG (-12) for aste_ecs2sky() & aste_ecs2foc()

	2005-06-16	Y.ISHISAKI	version 1.52
		define ASTE_TELESCOP_ID, ASTE_xxx_ID
		add declarations of aste_telescop_id/aste_instrume/aste_instrume_id()

	2006-08-01	Y.ISHISAKI	version 1.80
		add aste_cor_aberration(), aste_inv_aberration() [mission independent]

	2006-08-01	Y.ISHISAKI	version 1.81
		rename aste_pol2rec() -> aste_xrt_pol2rec()
		define aste_pol2rec, aste_rec2pol for backward compatibility

************************************************************************/

#ifndef _ASTE_COORD_H_
#define _ASTE_COORD_H_

#include "teldef.h"	/* TELDEF definition */

#define ASTE_TELESCOP_ID		38

typedef enum {
	ASTE_XIS0_SENSOR=3800,	/* 38: satellite number of Astro-E2 */
	ASTE_XIS1_SENSOR=3801,
	ASTE_XIS2_SENSOR=3802,
	ASTE_XIS3_SENSOR=3803,
	ASTE_XRS_SENSOR= 3804,
	ASTE_HXD_SENSOR= 3805,
	ASTE_XIS0_ID=3800,	/* INSTRUME = 'XIS0' or 'XIS-0' */
	ASTE_XIS1_ID=3801,	/* INSTRUME = 'XIS1' or 'XIS-1' */
	ASTE_XIS2_ID=3802,	/* INSTRUME = 'XIS2' or 'XIS-2' */
	ASTE_XIS3_ID=3803,	/* INSTRUME = 'XIS3' or 'XIS-3' */
	ASTE_XRS_ID= 3804,	/* INSTRUME = 'XRS' */
	ASTE_HXD_ID= 3805,	/* INSTRUME = 'HXD' */
	ASTE_XIS_ID= 3806,	/* INSTRUME = 'XIS' */
	ASTE_XRT_ID= 3807,	/* INSTRUME = 'XRT' */
	ASTE_XRTI_ID=3808,	/* INSTRUME = 'XRT-I'  */
	ASTE_XRT0_ID=3810,	/* INSTRUME = 'XRT-I0' */
	ASTE_XRT1_ID=3811,	/* INSTRUME = 'XRT-I1' */
	ASTE_XRT2_ID=3812,	/* INSTRUME = 'XRT-I2' */
	ASTE_XRT3_ID=3813,	/* INSTRUME = 'XRT-I3' */
	ASTE_XRTS_ID=3814	/* INSTRUME = 'XRT-S'  */
} AstEsensorID;

typedef struct {
	double alpha;	/* RA  (deg) of the center of SKY coordinates */
	double delta;	/* DEC (deg) of the center of SKY coordinates */
	double roll;	/* angle (deg) between SKY y-axis and north,
			   		   measured counter clockwise from north */
} SKYREF;

#define ARCMIN2RAD	(DEG2RAD/60.0)
#define RAD2ARCMIN	(RAD2DEG*60.0)

#define ASTE_COORD_NORMAL_END		0
#define ASTE_COORD_DIVIDE_BY_ZERO	-1
#define ASTE_COORD_ALLOC_ERR		-2
#define ASTE_COORD_NOT_INITIALIZED	-3
#define ASTE_COORD_INVALID_SENSOR	-4
#define ASTE_COORD_NOT_SIS			-5
#define ASTE_COORD_INVALID_PIXEL	-6
#define ASTE_COORD_INVALID_SEGMENT	-7
#define ASTE_COORD_INVALID_WIN_OPT	-8
#define ASTE_COORD_MATRIX_ERR		-10
#define ASTE_COORD_SISMAT_ERR		-11
#define ASTE_COORD_OVER_90DEG		-12

#ifdef __cplusplus
extern "C"
{
#endif

/* aste_telescop.c */

/************************************************************************
  aste_telescop		Return TELESCOP keyword for FITS header.
					Currently, "Astro-E2", but will be changed after launch.
************************************************************************/
char *
aste_telescop(void)
;

/************************************************************************
  aste_telescop_id	Return ID number corresponding to the TELESCOP keyword,
					-1 for unknown
************************************************************************/
int
aste_telescop_id(char *telescop)
;

/* aste_instrume.c */

/************************************************************************
  aste_instrume		Return INSTRUME keyword for FITS header,
					NULL for unkonwn
************************************************************************/
char *
aste_instrume(int id)
;

/************************************************************************
  aste_instrume_id	Return ID number corresponding to the INSTRUME keyword,
					-1 for unknown
************************************************************************/
int
aste_instrume_id(char *instrume)
;

/* aste_coord_teldef.c */

/************************************************************************
  aste_coord_init	aste_coord initialization function

  INPUT
	char *telescop
		telescope name, which must match TELESCOP header keyword
		in the specified teldef file.
		If NULL or "" or "*", first occurence is used.
	char *instrume
		instrument name, which must match INSTRUME header keyword
		in the specified teldef file.
		If NULL or "" or "*", first occurence is used.
	char *filename
		teldef file name to read

  OUTPUT
	TELDEF *aste_coord_init
		allocated TELDEF structure.
		If this function fails, it returns NULL pointer and set errno
		(basically CFITSIO error number) to indicate the error
************************************************************************/
TELDEF *
aste_coord_init(char *telescop, char *instrume, char *filename)
;

/************************************************************************
  aste_coord_free	free allocated memory for TELDEF structure
************************************************************************/
int
aste_coord_free(TELDEF *teldef)
;

/************************************************************************
  aste_coord_teldef	return already allocated TELDEF structure

  INPUT
	char *telescop
		telescope name, which must match TELESCOP header keyword in TELDEF.
		If NULL or "" or "*", first occurence is used.
	char *instrume
		instrument name, which must match INSTRUME header keyword in TELDEF.
		If NULL or "" or "*", first occurence is used.

  OUTPUT
	TELDEF *aste_coord_teldef
		already allocated TELDEF structure.
		If no TELDEF is found, it returns NULL.
************************************************************************/
TELDEF *
aste_coord_teldef(char *telescop, char *instrume)
;

/* aste_coord_core.c */

/************************************************************************
  aste_det2foc		convert DETX/Y[ch] -> FOCX/Y[ch]
************************************************************************/
int
aste_det2foc(
	TELDEF *teldef, double detx_ch, double dety_ch,		/* input */
	double *focx_ch, double *focy_ch					/* output*/
);

/************************************************************************
  aste_foc2det		convert FOCX/Y[ch] -> DETX/Y[ch]
************************************************************************/
int
aste_foc2det(
	TELDEF *teldef, double focx_ch, double focy_ch,		/* input */
	double *detx_ch, double *dety_ch					/* output*/
);

/************************************************************************
  aste_foc2ecs		convert FOCX/Y[ch] -> (RA,DEC)[deg]
	Unit of the Eulder angles is radian.
************************************************************************/
int
aste_foc2ecs(
	TELDEF *teldef, AtEulerAng *ea, double focx_ch, double focy_ch,	/* input */
	double *alpha, double *delta									/* output*/
);

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
);

/************************************************************************
  aste_ecs2sky		convert (RA,DEC)[deg] -> SKYX/Y[ch]

  RETURN
	ASTE_COORD_DIVIDE_BY_ZERO: division by zero, probably invalid SKYREF
************************************************************************/
int
aste_ecs2sky(
	TELDEF *teldef, SKYREF *skyref, double alpha, double delta,	/* input */
	double *skyx_ch, double *skyy_ch							/* output*/
);

/************************************************************************
  aste_sky2ecs		convert SKYX/Y[ch] -> (RA,DEC)[deg]
	If aberration correection required, give aberration corrected RA and DEC.
************************************************************************/
int
aste_sky2ecs(
	TELDEF *teldef, SKYREF *skyref, double skyx_ch, double skyy_ch,
	double *alpha, double *delta
);

/************************************************************************
  aste_euler2skyref	calculate SKYREF [deg] from Euler angles [radian],
	which makes SKY coordinates identical to FOC coordinates
************************************************************************/
int
aste_euler2skyref(
	TELDEF *teldef, AtEulerAng *ea,	/* input */
	SKYREF *skyref					/* output*/
);

/************************************************************************
  aste_skyref2euler	determin Euler angles [radian] from SKYREF [deg],
	which makes SKY coordinates identical to FOC coordinates
************************************************************************/
int
aste_skyref2euler(
	TELDEF *teldef, SKYREF *skyref,		/* input */
	AtEulerAng *ea						/* output*/
);

/************************************************************************
  aste_foc2sky		convert FOCX/Y[ch] -> SKYX/Y[ch]
************************************************************************/
int
aste_foc2sky(
	TELDEF *teldef, AtEulerAng *ea, SKYREF *skyref,
	double focx_ch, double focy_ch,						/* input */
	double *skyx_ch, double *skyy_ch					/* output*/
);

/************************************************************************
  aste_sky2foc		convert SKYX/Y[ch] -> FOCX/Y[ch]
************************************************************************/
int
aste_sky2foc(
	TELDEF *teldef, AtEulerAng *ea, SKYREF *skyref,
	double skyx_ch, double skyy_ch,						/* input */
	double *focx_ch, double *focy_ch					/* output*/
);

/************************************************************************
  aste_det2sky		convert DETX/Y[ch] -> SKYX/Y[ch]
************************************************************************/
int
aste_det2sky(
	TELDEF *teldef, AtEulerAng *ea, SKYREF *skyref,
	double detx_ch, double dety_ch,						/* input */
	double *skyx_ch, double *skyy_ch					/* output*/
);

/************************************************************************
  aste_sky2det		convert SKYX/Y[ch] -> DETX/Y[ch]
************************************************************************/
int
aste_sky2det(
	TELDEF *teldef, AtEulerAng *ea, SKYREF *skyref,
	double skyx_ch, double skyy_ch,						/* input */
	double *detx_ch, double *dety_ch					/* output*/
);

/************************************************************************
  aste_det2ecs		convert DETX/Y[ch] -> (RA,DEC)[deg]
************************************************************************/
int
aste_det2ecs(
	TELDEF *teldef, AtEulerAng *ea, double detx_ch, double dety_ch,	/* input */
	double *alpha, double *delta									/* output*/
);

/************************************************************************
  aste_ecs2det		convert (RA,DEC)[deg] -> DETX/Y[ch]
************************************************************************/
int
aste_ecs2det(
	TELDEF *teldef, AtEulerAng *ea, double alpha, double delta,	/* input */
	double *detx_ch, double *dety_ch							/* output*/
);

/************************************************************************
  aste_det2det		convert DETX/Y[ch] between sensors, sensor0 -> sensor1
************************************************************************/
int
aste_det2det(
	TELDEF *teldef0, double  detx0_ch, double  dety0_ch,	/* source */
	TELDEF *teldef1, double *detx1_ch, double *dety1_ch		/* destination */
);

/************************************************************************
  aste_cor_aberration	aberration correction  (mission independent)
************************************************************************/
int
aste_cor_aberration(
	double astetime, int mjdrefi, double mjdreff,	/* input */
	double *alphaInOut, double *deltaInOut			/* input/output */
);

/************************************************************************
  aste_inv_aberration	inverse aberration correction (mission independent)
************************************************************************/
int
aste_inv_aberration(
	double astetime, int mjdrefi, double mjdreff,	/* input */
	double *alphaInOut, double *deltaInOut			/* input/output */
);

/* aste_coord_ch2mm.c */

/************************************************************************
  aste_det_ch2mm	convert DETX/Y[ch] -> DETX/Y[mm]
************************************************************************/
int
aste_det_ch2mm(
	TELDEF *teldef, double detx_ch, double dety_ch,		/* input */
	double *detx_mm, double *dety_mm					/* output*/
);

/************************************************************************
  aste_det_mm2ch	convert DETX/Y[mm] -> DETX/Y[ch]
************************************************************************/
int
aste_det_mm2ch(
	TELDEF *teldef, double detx_mm, double dety_mm,		/* input */
	double *detx_ch, double *dety_ch					/* output*/
);

/************************************************************************
  aste_foc_ch2mm	convert FOCX/Y[ch] -> FOCX/Y[mm]
************************************************************************/
int
aste_foc_ch2mm(
	TELDEF *teldef, double focx_ch, double focy_ch,		/* input */
	double *focx_mm, double *focy_mm					/* output*/
);

/************************************************************************
  aste_foc_mm2ch	convert FOCX/Y[mm] -> FOCX/Y[ch]
************************************************************************/
int
aste_foc_mm2ch(
	TELDEF *teldef, double focx_mm, double focy_mm,		/* input */
	double *focx_ch, double *focy_ch					/* output*/
);

/************************************************************************
  aste_sky_ch2mm	convert SKYX/Y[ch] -> SKYX/Y[mm]
************************************************************************/
int
aste_sky_ch2mm(
	TELDEF *teldef, double skyx_ch, double skyy_ch,		/* input */
	double *skyx_mm, double *skyy_mm					/* output*/
);

/************************************************************************
  aste_sky_mm2ch	convert SKYX/Y[mm] -> SKYX/Y[ch]
************************************************************************/
int
aste_sky_mm2ch(
	TELDEF *teldef, double skyx_mm, double skyy_mm,		/* input */
	double *skyx_ch, double *skyy_ch					/* output*/
);

/* aste_coord_xrt.c */

/************************************************************************
  aste_xrt_pol2rec	convert XRT theta[arcmin]/phi[deg] -> XRTX/Y[mm]
************************************************************************/
int
aste_xrt_pol2rec(
	TELDEF *teldef, double theta_min, double phi_deg,	/* input */
	double *xrtx_mm, double *xrty_mm					/* output*/
);
#define aste_pol2rec	aste_xrt_pol2rec

/************************************************************************
  aste_xrt_rec2pol	convert XRTX/Y[mm] -> XRT theta[arcmin]/phi[deg]
************************************************************************/
int
aste_xrt_rec2pol(
	TELDEF *teldef, double xrtx_mm, double xrty_mm,		/* input */
	double *theta_min, double *phi_deg					/* output*/
);
#define aste_rec2pol	aste_xrt_rec2pol

/************************************************************************
  aste_xrt2det		convert XRTX/Y[mm] (look-down) -> DETX/Y[ch]
************************************************************************/
int
aste_xrt2det(
	TELDEF *teldef, double xrtx_mm, double xrty_mm,		/* input */
	double *detx_ch, double *dety_ch					/* output*/
);

/************************************************************************
  aste_det2xrt		convert DETX/Y[ch] -> XRTX/Y[mm] (look-down)
************************************************************************/
int
aste_det2xrt(
	TELDEF *teldef, double detx_ch, double dety_ch,		/* input */
	double *xrtx_mm, double *xrty_mm					/* output*/
);

/* aste_coord_xis.c */

/************************************************************************
  xrs_pixel2det		convert XRS PIXEL to DETX/Y[ch]

  INPUT
	corner 0:center, 1:left-low, 2:right-low, 3:right-up, 4:left-up

  RETURN
	ASTE_COORD_INVALID_PIXEL: invalid pixel number
************************************************************************/
int
xrs_pixel2det(
	TELDEF *teldef,		/* input: teldef file of XRS */
	int pixel,			/* input: pixel number [0-31] */
	int corner,			/* input: position in pixel 0:center, 1-4:corner */
	double *detx_ch,	/* output: DETX [ch] */
	double *dety_ch		/* output: DETY [ch] */
);

/************************************************************************
  xrs_det2pixel		convert XRS DETX/Y[ch] to PIXEL

  OUTPUT
	int *pixel: XRS pixel number, but -1 outside of the pixels
************************************************************************/
int
xrs_det2pixel(
	TELDEF *teldef,		/* input: teldef file of XRS */
	double detx_ch,		/* input: DETX [ch] */
	double dety_ch,		/* input: DETY [ch] */
	int *pixel			/* output: pixel number [0-31], or -1: out of pixel */
);

/* aste_coord_xis.c */

/************************************************************************
  xis_ppu2raw	convert XIS PPUX/Y[ch] -> RAWX/Y[ch]
************************************************************************/
int
xis_ppu2raw(
	TELDEF *teldef,	/* input: teldef file */
	int ppux,		/* input: PPUX [0-259] */
	int ppuy,		/* input: PPUY [0-1023] */
	int *rawx,		/* output: RAWX [-2-257] */
	int *rawy		/* output: RAWY [0-1023] */
);

/************************************************************************
  xis_raw2ppu	convert XIS RAWX/Y[ch] -> PPUX/Y[ch]
************************************************************************/
int
xis_raw2ppu(
	TELDEF *teldef,	/* input: teldef file */
	int rawx,		/* input: RAWX [-2-257] */
	int rawy,		/* input: RAWY [0-1023] */
	int *ppux,		/* output: PPUX [0-259] */
	int *ppuy		/* output: PPUY [0-1023] */
);

/************************************************************************
  xis_raw2act	convert XIS RAWX/Y[ch] -> ACTX/Y[ch]

  RETURN
	ASTE_COORD_INVALID_PIXEL: invalid pixel range
	ASTE_COORD_INVALID_SEGMENT: invalid segment id
	ASTE_COORD_INVALID_WIN_OPT: invalid window option
************************************************************************/
int
xis_raw2act(
	TELDEF *teldef,	/* input: teldef file */
	int segid,		/* input: Segment ID [0-3] */
	int rawx,		/* input: RAWX [0-255] */
	int rawy,		/* input: RAWY [0-1023] */
	int win_opt,	/* input: window option [0-3] */
	int win_pos,	/* input: window position */
	int *actx,		/* output: ACTX [0-1023] */
	int *acty		/* output: ACTY [0-1023] */
);

/************************************************************************
  xis_act2raw	convert XIS ACTX/Y[ch] -> RAWX/Y[ch]

  RETURN
	ASTE_COORD_INVALID_PIXEL: invalid pixel range
	ASTE_COORD_INVALID_SEGMENT: invalid segment id
	ASTE_COORD_INVALID_WIN_OPT: invalid window option
************************************************************************/
int
xis_act2raw(
	TELDEF *teldef,	/* input: teldef file */
	int actx,		/* input: ACTX [0-1023] */
	int acty,		/* input: ACTY [0-1023] */
	int win_opt,	/* input: window option [0-3] */
	int win_pos,	/* input: window position */
	int *segid_ptr,	/* output: Segment ID [0-3] */
	int *rawx_ptr,	/* output: RAWX [0-255] */
	int *rawy_ptr	/* output: RAWY [0-1023] */
);

/************************************************************************
  xis_act2det	convert XIS ACTX/Y[ch] -> DETX/Y[ch]
************************************************************************/
int
xis_act2det(
	TELDEF *teldef,	/* input: teldef file */
	int actx_ch,	/* input: ACTX [0-1023] */
	int acty_ch,	/* input: ACTY [0-1023] */
	int *detx_ch,	/* input: DETX [1-1024] */
	int *dety_ch	/* input: DETY [1-1024] */
);

/************************************************************************
  xis_det2act	convert XIS DETX/Y[ch] -> ACTX/Y[ch]
************************************************************************/
int
xis_det2act(
	TELDEF *teldef,	/* input: teldef file */
	int detx_ch,	/* input: DETX [1-1024] */
	int dety_ch,	/* input: DETY [1-1024] */
	int *actx_ch,	/* input: ACTX [0-1023] */
	int *acty_ch	/* input: ACTY [0-1023] */
);

/************************************************************************
  hxd_pin2det		get DETX/Y[ch] of PIN 0-63
************************************************************************/
int
hxd_pin2det(
	TELDEF *teldef,		/* input: teldef file of HXD */
	int pin_id,			/* input: ID number of PIN [0-63] */
	double *detx_ch,	/* output: DETX [ch] */
	double *dety_ch		/* output: DETY [ch] */
);

/************************************************************************
  hxd_gso2det		get DETX/Y[ch] of GSO 0-15
************************************************************************/
int
hxd_gso2det(
	TELDEF *teldef,		/* input: teldef file of HXD */
	int gso_id,			/* input: ID number of GSO [0-15] */
	double *detx_ch,	/* output: DETX [ch] */
	double *dety_ch		/* output: DETY [ch] */
);

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
);

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
);

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
);

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
);

#ifdef __cplusplus
}
#endif

#endif	/* _ASTE_COORD_H_ */

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
