/*
 SimASTE_PhotonGen.c
   SimASTE module : Photon Reader

  1998-07-09	version 1.00	Y.Ishisaki
	coded first

  1998-09-10	version 1.10	Y.Ueda, Y.ISHISAKI
	time_mode, limit_mode are available
	calcurate coutrate from flux, exposure
	work with ftools parameter interface

  1999-01-22	version 1.20	Y.ISHISAKI
	BNK put TSTART, TSTOP, EXPOSURE

  2003-11-08	version 1.40	Y.ISHISAKI
	bug fix when energy step of qdp spectrum file is not linear
	change float definitions into double in read_qdpfile()
	increase MAX_SPEC_BUF 10000 -> 300000

  2003-05-18	version 1.50	Y.ISHISAKI
	add image_mode 'uniform_sky'

  2005-12-24	version 1.51	Y.ISHISAKI
	remove redundant comma after IMAGE_MODE_UNIFORM_SKY

  2006-04-09 version 1.6	Y.ISHISAKI
	increase qdpfile[256] -> [1024], imgfile[256] -> [1024]

  2006-07-24 version 2.0	Y.ISHISAKI
	print error in reading parameters
	use anl_msg_xxx() functions

  2006-08-02 version 2.1	Y.ISHISAKI
	parameter right_ascension -> ra, declination -> dec

  2006-08-06 version 2.2	Y.ISHISAKI
	add write_history(), BnkGet/Put SimASTE:WRITE_HISTORY:FUNC in _init()
	BnkPut SimASTE:GEOMAREA, SimASTE:N_PHOTON
	int -> double nphoton, integ_photon to support large number of photons
	add enable_photonge parameter, to work with SimASTE_PhotonRead
	use acos() in generating random photon for image_mode=UNIFORM_SKY

  2006-08-26 version 2.3	Y.ISHISAKI
	float -> double, with Hrndm1D**(), Hrndm2D**() in astetool-1.83

  2008-07-31 version 2.4	Y.ISHISAKI
	check if WCS keywords exists in check_keyword(), and fail if not exists
	add '\n' in printing NxN image read from '...' in _init()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <unistd.h>
#include "fitsio.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"		/* ARCMIN2RAD is defined here */
#include "aste_rand.h"
#include "SimASTE.h"

static char pname[] = "SimASTE_PhotonGen";
char SimASTE_PhotonGen_version[] = "version 2.4";

#define unless(a) if(!(a))

enum spec_mode {
	SPEC_MODE_QDP,
	SPEC_MODE_MONOCHROME
};

enum image_mode {
	IMAGE_MODE_FITS,
	IMAGE_MODE_POINTLIKE,
	IMAGE_MODE_UNIFORM_SKY
};

enum point_type {
	POINT_TYPE_J2000
};

enum time_mode {
	TIME_MODE_CONSTANT,
	TIME_MODE_POISSON
};

enum limit_mode {
	LIMIT_MODE_NPHOTON,
	LIMIT_MODE_EXPOSURE
};

#define MAX_POINT 100

/* followings are examples of input paraeters for astePhotonRead */
static struct {
	int enable_photongen;
	enum spec_mode spec_mode;
	enum image_mode image_mode;
	enum point_type point_type;
	enum time_mode time_mode;
	enum limit_mode limit_mode;
	double energy;
	int np;
	double flux[MAX_POINT], xp[MAX_POINT], yp[MAX_POINT];
	char qdpfile[PIL_LINESIZE];
	double flux_emin, flux_emax;
	char imgfile[PIL_LINESIZE];
	double sky_ra, sky_dec, sky_r_min, sky_r_max;
	double cos_sky_r_min, cos_sky_r_mami;
	AtRotMat sky_rm;
	double start_time, time, integ_time;
	double photon_flux, geomarea, scale_factor, rate;
	double nphoton;
	double integ_photon;
	double exposure;
	struct Hrndm1D *hm1;
	struct Hrndm2D *hm2;
	int (*prev_write_history)(fitsfile *);
} com = {
	ANL_YES,					/* enable_photongen */
	SPEC_MODE_MONOCHROME,		/* spec_mode */
	IMAGE_MODE_POINTLIKE,		/* image_mode */
	POINT_TYPE_J2000,			/* point_type */
	TIME_MODE_CONSTANT,			/* time_mode */
	LIMIT_MODE_NPHOTON,			/* limit_mode */
	2.0,						/* energy */
	1,							/* np */
	{1.0}, {0.0}, {0.0},		/* flux, xp, yp */
	"spec.qdp",					/* qdpfile */
	2.0, 10.0,					/* emin, emax */
	"image.fits",				/* imgfile */
	0.0, 0.0, 0.0, 0.0,			/* sky_ra, sky_dec, sky_r_min, sky_r_max */
	1.0, 0.0,					/* cos_sky_r_min, cos_sky_r_mami */
	{{0,0,0},{0,0,0},{0,0,0}},	/* sky_rm */
	0.0, 0.0, 0.0,				/* start_time, time, integ_time */
	1.0, 1147.3, 1.0, 0.0,		/* photon_flux, geomarea, scale_factor, rate*/
	100000.0,					/* nphoton */
	0.0,						/* integ_photon */
	40000.0,					/* exposure */

	NULL,						/* hm1 */
	NULL,						/* hm2 */
	NULL						/* prev_write_history */
};

static int
write_history(fitsfile *fp)
{
	int istat;
	char history[PIL_LINESIZE + FLEN_VALUE];

	if ( NULL != com.prev_write_history &&
		 write_history != com.prev_write_history ) {
		istat = (*com.prev_write_history)(fp);
		if ( istat ) {
			return istat;
		}
	}

	istat = SimASTE_write_history_pname(fp, pname);

	sprintf(history, "\
  enable_photongen=%s", com.enable_photongen ? "yes" : "no");
	fits_write_history(fp, history, &istat);
	if ( 0 == com.enable_photongen ) goto skip;

	sprintf(history, "\
  photon_flux=%.6e photons/cm2/s", com.photon_flux);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  flux_emin=%.3f keV  flux_emax=%.3f keV", com.flux_emin, com.flux_emax);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  geometrical_area=%.4f cm2  scale_factor=%.6f",
		com.geomarea, com.scale_factor);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  start_time=%.3f", com.start_time);
	fits_write_history(fp, history, &istat);
	if ( SPEC_MODE_QDP == com.spec_mode ) {
		sprintf(history, "\
  spec_mode=%d:QDP-SPEC", com.spec_mode);
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  qdp_spec_file='%s'", com.qdpfile);
		fits_write_history(fp, history, &istat);
	} else if ( SPEC_MODE_MONOCHROME == com.spec_mode ) {
		sprintf(history, "\
  spec_mode=%d:MONOCHROME  energy=%.3f keV", com.spec_mode, com.energy);
		fits_write_history(fp, history, &istat);
	}
	if ( IMAGE_MODE_FITS == com.image_mode ) {
		sprintf(history, "\
  image_mode=%d:FITS-IMAGE", com.image_mode);
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  fits_image_file='%s'", com.imgfile);
		fits_write_history(fp, history, &istat);
	} else if ( IMAGE_MODE_POINTLIKE == com.image_mode ) {
		sprintf(history, "\
  image_mode=%d:POINT-LIKE  ra=%.4f deg  dec=%.4f deg",
			com.image_mode, com.sky_ra, com.sky_dec);
		fits_write_history(fp, history, &istat);
	} else if ( IMAGE_MODE_UNIFORM_SKY == com.image_mode ) {
		sprintf(history, "\
  image_mode=%d:UNIFORM-SKY  ra=%.4f deg  dec=%.4f deg",
			com.image_mode, com.sky_ra, com.sky_dec);
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  sky_r_min=%.3f arcmin  sky_r_max=%.3f arcmin",
			com.sky_r_min, com.sky_r_max);
		fits_write_history(fp, history, &istat);
	}
	if ( TIME_MODE_CONSTANT == com.time_mode ) {
		sprintf(history, "\
  time_mode=%d:CONSTANT", com.time_mode);
	} else if ( TIME_MODE_POISSON == com.time_mode ) {
		sprintf(history, "\
  time_mode=%d:POISSON", com.time_mode);
	}
	if ( LIMIT_MODE_NPHOTON == com.limit_mode ) {
		sprintf(history + strlen(history), "\
  limit_mode=%d:NPHOTON  nphoton=%.0f", com.limit_mode, com.nphoton);
	} else if ( LIMIT_MODE_EXPOSURE == com.limit_mode ) {
		sprintf(history + strlen(history), "\
  limit_mode=%d:EXPOSURE  exposure=%.3f s", com.limit_mode, com.exposure);
	}
	fits_write_history(fp, history, &istat);

 skip:
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

static void
MSG(char *format, ...)
{
	FILE *fp = stdout;
	va_list args;
	va_start(args, format);
	if ( '!' == *format ) {
		vfprintf(fp, format+1, args);
	} else {
		vfprintf(fp, format, args);
		fputc('\n', fp);
	}
	va_end(args);
	if ( isatty(fileno(fp)) ) fflush(fp);
}

static void
show_parameter(char *title)
{
	int i;

	MSG("");
	MSG(title, pname);
	MSG("");

	if ( 0 == com.enable_photongen ) {
		MSG("%4s%-20s%s", "", "ENABLE_PHOTONGEN", "NO");
		return;
	}

	MSG("%4s%-20s%s", "", "ENABLE_PHOTONGEN", "YES");
	MSG("%4s%-20s%.3g (photons/s/cm2) (%.3f - %.3f keV)", "", "PHOTON_FLUX",
		com.photon_flux, com.flux_emin, com.flux_emax);
	MSG("%4s%-20s%.3f (cm2)", "", "GEOMETICAL_AREA", com.geomarea);
	MSG("%4s%-20s%.3f", "", "SCALE_FACTOR", com.scale_factor);
	MSG("%4s%-20s%.4f", "", "START_TIME", com.start_time);

	switch ( com.spec_mode ) {
	case SPEC_MODE_QDP:
		MSG("%4s%-20s%d:%s", "", "SPEC_MODE", com.spec_mode, "QDP-SPEC");
		MSG("%4s%-20s'%s'", "", "  QDP_SPEC_FILE", com.qdpfile);
		break;
	case SPEC_MODE_MONOCHROME:
		MSG("%4s%-20s%d:%s", "", "SPEC_MODE", com.spec_mode, "MONOCHROME");
		MSG("%4s%-20s%.3f (keV)", "", "  ENERGY", com.energy);
		break;
	default:
		MSG("%4s%-20s%d:%s", "", "SPEC_MODE", com.spec_mode, "UNKNOWN");
	}
	switch ( com.image_mode ) {
	case IMAGE_MODE_POINTLIKE:
		MSG("%4s%-20s%d:%s", "", "IMAGE_MODE", com.image_mode, "POINTLIKE");
		switch ( com.point_type ) {
		case POINT_TYPE_J2000:
			MSG("%4s%-20s%s", "", "  POINT_TYPE", "J2000");
			MSG("%4s%-20s%d", "", "  NPOINT", com.np);
			for (i = 0; i < com.np; i++) {
				MSG("%4s%3d%-17s%.4f %9.4f %9.4f", "", i+1, ": flux,RA,DEC",
					com.flux[i], com.xp[i], com.yp[i]);
			}
			break;
		default:
			MSG("%4s%-20s%d:%s", "", "POINT_TYPE", com.image_mode, "UNKNOWN");
		}
		break;
	case IMAGE_MODE_FITS:
		MSG("%4s%-20s%d:%s", "", "IMAGE_MODE", com.image_mode, "FITS-IMAGE");
		MSG("%4s%-20s'%s'", "", "  FITS_IMAGE_FILE", com.imgfile);
		break;
	case IMAGE_MODE_UNIFORM_SKY:
		MSG("%4s%-20s%d:%s", "", "IMAGE_MODE", com.image_mode, "UNIFORM-SKY");
		MSG("%4s%-20s%.4f (deg)", "", "  SKY_RA", com.sky_ra);
		MSG("%4s%-20s%.4f (deg)", "", "  SKY_DEC", com.sky_dec);
		MSG("%4s%-20s%.4f (arcmin)", "", "  SKY_R_MIN", com.sky_r_min);
		MSG("%4s%-20s%.4f (arcmin)", "", "  SKY_R_MAX", com.sky_r_max);
		break;
	default:
		MSG("%4s%-20s%s", "", "IMAGE_MODE", "UNKNOWN");
	}

	switch ( com.time_mode ) {
	case TIME_MODE_CONSTANT:
		MSG("%4s%-20s%d:%s", "", "TIME_MODE", com.time_mode, "CONSTANT");
		break;
	case TIME_MODE_POISSON:
		MSG("%4s%-20s%d:%s", "", "TIME_MODE", com.time_mode, "POISSON");
		break;
	default:
		MSG("%4s%-20s%d:%s", "", "TIME_MODE", com.time_mode, "UNKNOWN");
	}

	switch ( com.limit_mode ) {
	case LIMIT_MODE_NPHOTON:
		MSG("%4s%-20s%d:%s", "", "LIMIT_MODE", com.limit_mode, "NPHOTON");
		MSG("%4s%-20s%.0f (photons)", "", "  NPHOTON", com.nphoton);
		break;
	case LIMIT_MODE_EXPOSURE:
		MSG("%4s%-20s%d:%s", "", "LIMIT_MODE", com.limit_mode, "EXPOSURE");
		MSG("%4s%-20s%.3f (sec)", "", "  EXPOSURE", com.exposure);
		break;
	default:
		MSG("%4s%-20s%d:%s", "", "LIMIT_MODE", com.limit_mode, "UNKNOWN");
	}

}

static int
read_one_line(char *line, int n, FILE *fp)
{
	int len, linelen;
	char buf[80];

	if ( NULL == fgets(line, n, fp) ) return 0;
	len = linelen = strlen(line);
	strcpy(buf, (len+1 < sizeof(buf)) ? line : &line[len+1-sizeof(buf)]);

	for (;;) {
		len = strlen(buf);
		if ( 0 == len ) break;
		/* read until end of line */
		if ( '\n' != buf[len-1] ) {
			strcpy(buf, buf+sizeof(buf)-10);
			len = strlen(buf);
			if ( NULL == fgets(buf+len, sizeof(buf)-len, fp) ) break;
			continue;
		}
		/* remove trailing CR/LF */
		while ( 0 < len && ( '\n' == buf[len-1] || '\r' == buf[len-1] ) ) {
			len--;
		}
		if ( 0 == len ) break;
		/* check continuous line, i.e., line ends with '-' */
		if ( '-' == buf[len-1] ) {
			if ( NULL == fgets(buf, sizeof(buf), fp) ) break;
			continue;
		}
		break;
	}

	return linelen;
}

static int  /* return read size */
read_qdpfile(char *fn, int ne, double ear[/*ne+1*/], double photar[/*ne*/], double *ratio)
{
	FILE *fp;
	char line[80];
	int ie, len;
	double e0, de, val;
	double integ1 = 0.0;
	double integ2 = 0.0;

	fp = fopen(fn, "r");
	if ( NULL == fp ) {
		anl_msg_error("\
%s: QDP-file '%s' open failed\n", pname, fn);
		return 0;
	}

	ie = 0;

	for (;;) {
		len = read_one_line(line, sizeof(line), fp);
		if ( 0 == len && feof(fp) ) break;
		/* just ignore blank line */
		if ( '\0' == *line ) {
			continue;
		}
		/* check if data start */
		if ( '!' == *line ) {
			ie = 0;
			continue;
		}
		/* read data */
		if ( 3 != sscanf(line, "%lf%lf%lf", &e0, &de, &val) ) {
			continue;
		}
		if ( ie < ne ) {
			ear[ie] = e0 - de/2;
			ear[ie+1] = e0 + de/2;
			photar[ie] = val;
			ie++;
		} else {
			anl_msg_warning("\
%s: WARNING: too many energy bins in QDP-file '%s'\n", pname, fn);
			break;
		}
	}

	fclose(fp);
	ne = ie;

	for (ie = 0; ie < ne; ie++) {
		val = photar[ie];
		de = ear[ie+1] - ear[ie];
		/* integrate all photon flux */
		integ1 += val * de;
		/* integrate photon flux from EMIN to EMAX */
		if ( com.flux_emin <= ear[ie] && ear[ie+1] <= com.flux_emax ) {
			integ2 += val * de;
		}
	}

	if ( com.flux_emin < ear[0] ) {
		anl_msg_warning("\
%s: WARNING: FLUX_EMIN=%.1f is lower than that in QDP-file %.1f (keV)\n",
			pname, com.flux_emin, ear[0]
			);
	}
	if ( com.flux_emax > ear[ne] ) {
		anl_msg_warning("\
%s: WARNING: FLUX_EMAX=%.1f is higher than that in QDP-file %.1f (keV)\n",
			pname, com.flux_emax, ear[ne]
			);
	}

	if ( integ2 > 0.0 ) {
		*ratio = integ1 / integ2;
	} else {
		anl_msg_warning("\
%s: ERROR: photon flux from FLUX_EMIN to FLUX_EMAX is zero in QDP-file\n",
			pname);
		*ratio = 0.0;
	}

	return ne;
}

static double equinox = 2000.0;
static double crval1=0, crval2=0;
static double cdelt1=-0.25, cdelt2=0.25;
static double crpix1=128.5, crpix2=128.5, crota2=0;

static void
pixel_to_ecs(double x, double y, double *alpha, double *delta)
{
	double mjd0, mjd1;
	AtPolarVect pv0, pv1;
	AtVect v0, v1;
	AtEulerAng ea;
	AtRotMat rm, rm_inv;
	AtVect vec_ecs;				/* 天球座標系での目標天体の位置ベクトル */
	AtVect vec_sky;				/* SKY 座標系での目標天体の位置ベクトル */
	double dummy;

	/* alpha, delta, roll をオイラー角に変換する */
	ea.phi = crval1 * DEG2RAD;
	ea.theta = M_PI_2 - crval2 * DEG2RAD;
	ea.psi = M_PI_2 + crota2 * DEG2RAD;

	/* オイラー角を回転行列に変換した上でその逆行列を求める */
	atEulerToRM(&ea, rm);
	atInvRotMat(rm, rm_inv);

	/* SKY 座標系での方向ベクトルに変換。*/
	vec_sky[0] = cdelt1 * (x - crpix1) * DEG2RAD;
	vec_sky[1] = cdelt2 * (y - crpix2) * DEG2RAD;
	vec_sky[2] = 1.0;

	/* 衛星の姿勢を使って 天球座標系での方向ベクトルに逆変換 */
	atRotVect(rm_inv, vec_sky, vec_ecs);

	/* 天球座標に変換 */
	atVectToPolDeg(vec_ecs, &dummy, alpha, delta);

	if ( 1950.0 == equinox ) {
		atB1950toJ2000(*alpha, *delta, alpha, delta);
	} else if ( 2000.0 != equinox ) {
		mjd0 = 51544.0 - (2000.0 - equinox) / 0.0027379093;
		mjd1 = MJD_J2000;
		pv0.lon = *alpha * DEG2RAD;
		pv0.lat = *delta * DEG2RAD;
		pv0.r = 1.0;
		atPolToVect(&pv0, v0);
		atPrecession(mjd0, v0, mjd1, v1);
		atVectToPol(v1, &pv1);
		*alpha = pv1.lon * RAD2DEG;
		*delta = pv1.lat * RAD2DEG;
	}
}

static int
check_keyword(fitsfile *fp)
{
	static char *keylist[] = {
		"TELESCOP", "INSTRUME", "OBJECT",
		"EXPOSURE", "EQUINOX",
		"CDELT1", "CDELT2", "CRPIX1", "CRPIX2",
		"CRVAL1", "CRVAL2", "CROTA2"
	};
	static double *varlist[] = {
		NULL, NULL, NULL,
		NULL, &equinox,
		&cdelt1, &cdelt2, &crpix1, &crpix2,
		&crval1, &crval2, &crota2
	};
	static int must_exist[] = {
		-1, -1, -1,
		-1, 1,
		1, 1, 1, 1,
		1, 1, 0
	};
	int i, nfound;

	for (i = nfound = 0; i < sizeof(keylist)/sizeof(*keylist); i++) {
		char card[81];
		int istat = 0;
		char *key = keylist[i];

		fits_read_card(fp, key, card, &istat);
		if ( 0 == istat ) {
			nfound++;
			anl_msg_info("%s\n", card);
			if ( NULL != varlist[i] ) {
				fits_read_key_dbl(fp, key, varlist[i], NULL, &istat);
			}
		}
		if ( istat ) {
			if ( 0 == must_exist[i] ) {
				anl_msg_warning("\
%s: WARNING: WCS keyword not found,\n\
    using default value of %s=%.3f\n", pname, keylist[i], *(varlist[i]));
			} else if ( 1 == must_exist[i] ) {
				anl_msg_error("\
%s: WCS keyword '%s' not found\n", pname, keylist[i]);
				return -1;
			}
		}
	}

	return nfound;
}

static double *
read_fitsimage(char *fn, int *nx, int *ny)
{
	fitsfile *fp;
	double *image;
	int nfound;
	int simple, bitpix, naxis, extd, anynul, status;
	long naxes[2], pc, gc;

	status = 0;
	fits_open_file(&fp, fn, READONLY, &status);
	if ( status ) {
		anl_msg_error("\
%s: FITS-image '%s' open failed\n", pname, fn);
		return NULL;
	}

	ffghpr(fp, 2, &simple, &bitpix, &naxis, naxes, &pc, &gc, &extd, &status);
	*nx = naxes[0];
	*ny = naxes[1];
	if ( naxis < 2 ) {
		*ny = 1;
	}
	unless ( 0 == status && naxis && nx && ny ) return NULL;
	image = malloc(sizeof(*image) * (*nx) * (*ny));
	if ( NULL == image ) return NULL;

	nfound = check_keyword(fp);
	if ( nfound < 0 ) {
		return NULL;	/* error in WCS keywords */
	}
	anl_msg_info("\
%s: %d keywords detected in '%s'\n", pname, nfound, com.imgfile);

	fits_read_img_dbl(fp, 1, 1, (*nx)*(*ny), 0.0, image, &anynul, &status);
	fits_close_file(fp, &status);
	if ( status ) {
		anl_msg_error("\
%s: FITS-image '%s' read error\n", pname, fn);
		return NULL;
	}

	return image;
}

void
SimASTE_PhotonGen_startup(int *status)
{
	;
}

void
SimASTE_PhotonGen_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"PHOTON_FLUX", "GEOMAREA", "SCALE_FACTOR",
		"SPEC_MODE", "QDP_SPEC_FILE", "ENERGY",
		"IMAGE_MODE", "POINT_TYPE", "FITS_IMAGE_FILE", "NPOINT", "RA-DEC",
		"START_TIME",
		"TIME_MODE",
		"LIMIT_MODE", "NPHOTON", "EXPOSURE",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"Flux in photons s-1 cm-2",
		"Geometrical area of mirror in cm2",
		"Scale factor for intensity",
		"Spectral mode (QDP-SPEC/MONOCHROME)",
		"QDP file for spectrum",
		"Photon energy in keV",
		"Image mode (FITS-IMAGE/RA-DEC)",
		"Point position coordinates (J2000/B1950/Galactic)",
		"FITS file for image",
		"Number of point-like sources",
		"RA/DEC in degree",
		"Start time in mission-time",
		"Time mode (CONSTANT/POISSON)",
		"Limit mode (NPHOTON/EXPOSURE)",
		"Number of incident photons",
		"Exposure in sec",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);

	char *k;

	if ( *status ) {			/* ftools */
		if ( PILGetBool(k="enable_photongen", &com.enable_photongen) ) {
			goto error;
		}
		if ( 0 == com.enable_photongen ) {
			goto skip;
		}
		if ( PILGetReal(k="photon_flux", &com.photon_flux) ||
			 PILGetReal(k="flux_emin", &com.flux_emin) ||
			 PILGetReal(k="flux_emax", &com.flux_emax) ||
			 PILGetReal(k="geometrical_area", &com.geomarea) ||
			 PILGetReal(k="scale_factor", &com.scale_factor) ||
			 PILGetReal(k="start_time", &com.start_time) ||
			 PILGetInt( k="spec_mode", (int *)&com.spec_mode) ||
			 0 ) {
			goto error;
		}
		switch (com.spec_mode) {
		case SPEC_MODE_QDP:
			if ( PILGetFname(k="qdp_spec_file", com.qdpfile) ) {
				goto error;
			}
			break;
		case SPEC_MODE_MONOCHROME:
			if ( PILGetReal(k="energy", &com.energy) ) {
				goto error;
			}
			break;
		default:
			;
		}
		if ( PILGetInt(k="image_mode", (int *)&com.image_mode) ) {
			goto error;
		}

		switch (com.image_mode) {
		case IMAGE_MODE_FITS:
			if ( PILGetFname(k="fits_image_file", com.imgfile) ) {
				goto error;
			}
			break;
		case IMAGE_MODE_POINTLIKE:
			com.np = 1;
			com.flux[0] = 1.0;
			if ( PILGetReal(k="ra", &com.sky_ra) ||
				 PILGetReal(k="dec", &com.sky_dec) ) {
				goto error;
			}
			com.xp[0] = com.sky_ra;
			com.yp[0] = com.sky_dec;
			break;
		case IMAGE_MODE_UNIFORM_SKY:
			if ( PILGetReal(k="ra", &com.sky_ra) ||
				 PILGetReal(k="dec", &com.sky_dec) ||
				 PILGetReal(k="sky_r_min", &com.sky_r_min) ||
				 PILGetReal(k="sky_r_max", &com.sky_r_max) ) {
				goto error;
			}
			break;
		default:
			;
		}
		if ( PILGetInt(k="time_mode", (int *)&com.time_mode) ||
			 PILGetInt(k="limit_mode", (int *)&com.limit_mode) ) {
			goto error;
		}
		switch (com.limit_mode) {
		case LIMIT_MODE_NPHOTON:
			if ( PILGetReal(k="nphoton", &com.nphoton) ) {
				goto error;
			}
			break;
		case LIMIT_MODE_EXPOSURE:
			if ( PILGetReal(k="exposure", &com.exposure) ) {
				goto error;
			}
			break;
		default:
			;
		}
	skip:
		*status = ANL_OK;;
		return;

	error:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
		*status = ANL_QUIT;
		return;

	}

	for (;;) {
		char *key;
		int ans[2];

		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("SHOW", key) ) {
			show_parameter("%s:  *** show parameter ***");
		} else if ( 0 == strcmp("SPEC_MODE", key) ) {
			static char *keytbl[] = {
				"QDP-SPEC", "MONOCHROME"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			static int it;
			static char key[32];

			CLkeyrd(-1, "SPEC_MODE", key, keytbl, nkey, &it, sizeof(key));
			if ( 0 == strcmp("QDP-SPEC", key) ) {
				com.spec_mode = SPEC_MODE_QDP;
			} else if ( 0 == strcmp("MONOCHROME", key) ) {
				com.spec_mode = SPEC_MODE_MONOCHROME;
			}
		} else if ( 0 == strcmp("QDP_SPEC_FILE", key) ) {
			CLtxtrd(key, com.qdpfile, sizeof(com.qdpfile));
		} else if ( 0 == strcmp("ENERGY", key) ) {
			CLfdprd(key, &com.energy);
		} else if ( 0 == strcmp("IMAGE_MODE", key) ) {
			static char *keytbl[] = {
				"FITS-IMAGE", "POINTLIKE", "UNIFORM-SKY"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			static int it;
			static char key[32];

			CLkeyrd(-1, "IMAGE_MODE", key, keytbl, nkey, &it, sizeof(key));
			if ( 0 == strcmp("FITS-IMAGE", key) ) {
				com.image_mode = IMAGE_MODE_FITS;
			} else if ( 0 == strcmp("POINTLIKE", key) ) {
				com.image_mode = IMAGE_MODE_POINTLIKE;
			} else if ( 0 == strcmp("UNIFORM-SKY", key) ) {
				com.image_mode = IMAGE_MODE_UNIFORM_SKY;
				CLfdprd("SKY_R_MIN (arcmin)", &com.sky_r_min);
				CLfdprd("SKY_R_MAX (arcmin)", &com.sky_r_max);
			}
		} else if ( 0 == strcmp("POINT_TYPE", key) ) {
			static char *keytbl[] = {
				"J2000"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			static int it;
			static char key[32];

			CLkeyrd(-1, "POINT_TYPE", key, keytbl, nkey, &it, sizeof(key));
			if ( 0 == strcmp("J2000", key) ) {
				com.point_type = POINT_TYPE_J2000;
			}
		} else if ( 0 == strcmp("FITS_IMAGE_FILE", key) ) {
			CLtxtrd(key, com.imgfile, sizeof(com.imgfile));
		} else if ( 0 == strcmp("NPOINT", key) ) {
			CLintrdL(key, &com.np, 1, MAX_POINT);
		} else if ( 0 == strcmp("RA-DEC", key) ) {
			int i;
			static char ra[80], dec[80];
			for (i = 0; i < com.np; i++) {
				CLfdprd("flux", &com.flux[i]);
				CLftoa(com.xp[i], ra, sizeof(ra));
				CLftoa(com.yp[i], dec, sizeof(dec));
				CLtxtrd("Right Ascension", ra, sizeof(ra));
				CLtxtrd("Declination", dec, sizeof(dec));
				com.xp[i] = atParseRAToRadian(ra) * RAD2DEG;
				com.yp[i] = atParseDecToRadian(dec) * RAD2DEG;
			}
		} else if ( 0 == strcmp("START_TIME", key) ) {
			CLfdprd(key, &com.start_time);
		} else if ( 0 == strcmp("TIME_MODE", key) ) {
			static char *keytbl[] = {
				"CONSTANT", "POISSON"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			static int it;
			static char key[32];

			CLkeyrd(-1, "TIME_MODE", key, keytbl, nkey, &it, sizeof(key));
			if ( 0 == strcmp("CONSTANT", key) ) {
				com.time_mode = TIME_MODE_CONSTANT;
			} else if ( 0 == strcmp("POISSON", key) ) {
				com.time_mode = TIME_MODE_POISSON;
			}
		} else if ( 0 == strcmp("PHOTON_FLUX", key) ) {
			CLfdprd("Photon Flux (c/s/cm2)", &com.photon_flux);
			CLfdprd("Flux Emin (keV)", &com.flux_emin);
			CLfdprd("Flux Emax (keV)", &com.flux_emax);
		} else if ( 0 == strcmp("GEOMAREA", key) ) {
			CLfdprd(key, &com.geomarea);
		} else if ( 0 == strcmp("SCALE_FACTOR", key) ) {
			CLfdprd(key, &com.scale_factor);
		} else if ( 0 == strcmp("LIMIT_MODE", key) ) {
			static char *keytbl[] = {
				"NPHOTON", "EXPOSURE"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			static int it;
			static char key[32];

			CLkeyrd(-1, "LIMIT_MODE", key, keytbl, nkey, &it, sizeof(key));
			if ( 0 == strcmp("NPHOTON", key) ) {
				com.limit_mode = LIMIT_MODE_NPHOTON;
				CLfdprd("Number of Photons", &com.nphoton);
			} else if ( 0 == strcmp("EXPOSURE", key) ) {
				com.limit_mode = LIMIT_MODE_EXPOSURE;
				CLfdprd("Exposure (sec)", &com.exposure);
			}
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
SimASTE_PhotonGen_init(int *status)
{
	static int (*func)(fitsfile *fp) = write_history;
	int used;

	EvsDef("SimASTE_PhotonGen:BEGIN");
	EvsDef("SimASTE_PhotonGen:ENTRY");
	EvsDef("SimASTE_PhotonGen:OK");

	com.prev_write_history = NULL;
	BnkGet("SimASTE:WRITE_HISTORY:FUNC", sizeof(com.prev_write_history),
		&used, &com.prev_write_history);
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

	show_parameter("%s:  *** show parameter ***");

	if ( 0 == com.enable_photongen ) {
		goto skip;
	}

	if ( SPEC_MODE_QDP == com.spec_mode ) {
#define MAX_SPEC_BUF  300000
		static double ear[MAX_SPEC_BUF+1];
		static double photar[MAX_SPEC_BUF];
		double ratio;
		int ne;

		ne = read_qdpfile(com.qdpfile, MAX_SPEC_BUF, ear, photar, &ratio);
		if ( ne < 2 ) {
			anl_msg_error("\
%s: too small energy bins (%d) in QDP-file '%s'\n",
				pname, ne, com.qdpfile);
			*status = ANL_QUIT;
			return;
		}

		anl_msg_info("\n\
%s: %d energy bins read from '%s'\n", pname, ne, com.qdpfile);
		/* for debug
		   for (ie = 0; ie < ne; ie++) {
		   printf("%f %f %e\n", ear[ie], ear[ie+1], photar[ie]);
		   }
		   */
		com.hm1 = Hrndm1D_init(ne, ear, photar, 1000);
		if ( NULL == com.hm1 ) {
			anl_msg_error("\
%s: error in Hrndm1D_init()\n", pname);
			*status = ANL_QUIT;
			return;
		}
#undef MAX_SPEC_BUF
		com.rate = com.photon_flux * com.geomarea * com.scale_factor * ratio;
	} else {
		com.rate = com.photon_flux * com.geomarea * com.scale_factor;
	}

	if ( IMAGE_MODE_FITS == com.image_mode ) {
		double *image;
		int nx = 0, ny = 0;

		image = read_fitsimage(com.imgfile, &nx, &ny);

		if ( NULL == image ) {
			*status = ANL_QUIT;
			return;
		}

		if ( nx < 2 || ny < 2 ) {
			anl_msg_error("\
%s: too small energy bins (%dx%d) in FITS-image '%s'\n",
				pname, nx, ny, com.imgfile);
			*status = ANL_QUIT;
			return;
		}

		anl_msg_info("\
%s: %dx%d image read from '%s'\n", pname, nx, ny, com.imgfile);

		com.hm2 = Hrndm2D_init(image, nx, 0.5, nx+0.5, ny, 0.5, ny+0.5);
		free(image);

		if ( NULL == com.hm2 ) {
			anl_msg_error("\
%s: error in Hrndm2D_init()\n", pname);
			*status = ANL_QUIT;
			return;
		}
	} else if ( IMAGE_MODE_POINTLIKE == com.image_mode ) {
		if ( 1 < com.np ) {
			com.hm2 = Hrndm2D_init(com.flux, com.np, 0.0, com.np, 1, 0.0, 1.0);
			if ( NULL == com.hm2 ) {
				anl_msg_error("\
%s: error in Hrndm2D_init()\n", pname);
				*status = ANL_QUIT;
				return;
			}
		}
	} else if ( IMAGE_MODE_UNIFORM_SKY == com.image_mode ) {
		AtEulerAng ea;
		AtRotMat invrm;

		ea.phi = com.sky_ra * DEG2RAD;
		ea.theta = (90.0 - com.sky_dec) * DEG2RAD;
		ea.psi = 0.0;
		atEulerToRM(&ea, invrm);
		atInvRotMat(invrm, com.sky_rm);

		com.cos_sky_r_min = cos(com.sky_r_min*ARCMIN2RAD);
		com.cos_sky_r_mami= cos(com.sky_r_max*ARCMIN2RAD) - com.cos_sky_r_min;
	}

	com.time = com.start_time;
	com.integ_photon = 0.0;
	com.integ_time = 0.0;

	BnkPut("SimASTE:GEOMAREA", sizeof(com.geomarea), &com.geomarea);
	BnkPut("SimASTE:TSTART", sizeof(com.start_time), &com.start_time);
	if ( LIMIT_MODE_EXPOSURE == com.limit_mode ) {
		double tstop = com.start_time + com.exposure;
		BnkPut("SimASTE:TSTOP", sizeof(tstop), &tstop);
		BnkPut("SimASTE:EXPOSURE", sizeof(com.exposure), &com.exposure);
	}

	/*
	   if ( LIMIT_MODE_EXPOSURE == com.limit_mode ) {
	   com.nphoton = com.rate * com.exposure;
	   } else if ( LIMIT_MODE_NPHOTON == com.limit_mode ) {
	   com.exposure = com.nphoton / com.rate;
	   } else {
	   ;
	   }
	   */

 skip:
	*status = ANL_OK;
}

void
SimASTE_PhotonGen_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_PhotonGen_bgnrun(int *status)
{
	EvsSet("SimASTE_PhotonGen:BEGIN");
	*status = ANL_OK;
}

void
SimASTE_PhotonGen_ana(int nevent, int eventid, int *status)
{
	double energy, xy[2], alpha, delta, theta, phi, cos_theta, sin_theta;
	double rnum, dtime;
	AtVect zaxis, vect;
	AtPolarVect pv;

/* check if photongen is enabled */
	if ( 0 == com.enable_photongen ) {
		*status = ANL_OK;
		return;
	}

/* check for photon ends */
	switch (com.limit_mode) {
	case LIMIT_MODE_NPHOTON:
		if ( com.nphoton <= com.integ_photon ) {
			*status = ANL_QUIT;
			return;
		}
		break;
	case LIMIT_MODE_EXPOSURE:
		if ( com.exposure < com.integ_time ) {
			*status = ANL_QUIT;
			return;
		}
		break;
	default:
		;
	}

	EvsfSetM("SimASTE_PhotonGen:ENTRY");

/* generate photon */
	switch (com.spec_mode) {
	case SPEC_MODE_QDP:
		energy = Hrndm1D(aste_drndts(), com.hm1);
		break;
	case SPEC_MODE_MONOCHROME:
		energy = com.energy;
		break;
	default:
		;
	}

	switch (com.image_mode) {
	case IMAGE_MODE_FITS:
		aste_drndtsn(2, xy);
		Hrndm2D(xy, com.hm2);
		pixel_to_ecs(xy[0], xy[1], &alpha, &delta);
		/* for debug
		   printf("%f %f %f %f %f\n", energy, xy[0], xy[1], alpha, delta);
		   */
		break;
	case IMAGE_MODE_POINTLIKE:
		if ( 1 < com.np ) {
			aste_drndtsn(2, xy);
			Hrndm2D(xy, com.hm2);
			alpha = com.xp[(int)xy[0]];
			delta = com.yp[(int)xy[0]];
		} else {
			alpha = com.xp[0];
			delta = com.yp[0];
		}
		break;
	case IMAGE_MODE_UNIFORM_SKY:
/*
   2006-08-06, Y.ISHISAKI
   To generate a photon which follows f(t) = sin(t) (t0 <= t <= t1),
   (cos(t0) - cos(t1)) * y = F(t) = \int_t0^t sin(t') dt' = cos(t0) - cos(t),
   t = F^{-1}(y) = acos( cos(t0) + y (cos(t1) - cos(t0)) )
   We need to consider aperture cosine factor, elsewhere.
*/
		cos_theta = com.cos_sky_r_min + aste_drndts() * com.cos_sky_r_mami;
		theta = acos(cos_theta);
		sin_theta = sin(theta);
		phi = 360.0 * DEG2RAD * aste_drndts();
		zaxis[0] = cos(phi) * sin_theta;
		zaxis[1] = sin(phi) * sin_theta;
		zaxis[2] = cos_theta;
		atRotVect(com.sky_rm, zaxis, vect);
		atVectToPol(vect, &pv);
		alpha = pv.lon * RAD2DEG;
		delta = pv.lat * RAD2DEG;
		break;

	default:
		;
	}

/* calculate time */
	switch (com.time_mode) {
	case TIME_MODE_CONSTANT:
		dtime = 1/com.rate;
		break;
	case TIME_MODE_POISSON:
		do {
			rnum = aste_drndts();
		} while ( rnum <= 0 );
		dtime = -log(rnum)/com.rate;
		break;
	default:
		dtime = 0.0;
	}
	com.time += dtime;
	com.integ_time += dtime;
	com.integ_photon += 1.0;

/* put data to BNK */
	BnkfPutM("SimASTE:N_PHOTON", sizeof(com.integ_photon), &com.integ_photon);
	BnkfPutM("SimASTE:PHOTON_TIME", sizeof(com.time), &com.time);
	BnkfPutM("SimASTE:PHOTON_ENERGY", sizeof(energy), &energy);
	BnkfPutM("SimASTE:RA", sizeof(alpha), &alpha);
	BnkfPutM("SimASTE:DEC", sizeof(delta), &delta);

	EvsfSetM("SimASTE_PhotonGen:OK");
	*status = ANL_OK;
}

void
SimASTE_PhotonGen_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_PhotonGen_exit(int *status)
{
	if ( 0 == com.enable_photongen ) {
		*status = ANL_OK;
		return;
	}

	if ( LIMIT_MODE_EXPOSURE != com.limit_mode ) {
		double tstop = com.start_time + com.integ_time;
		BnkPut("SimASTE:TSTOP", sizeof(tstop), &tstop);
		BnkPut("SimASTE:EXPOSURE", sizeof(com.integ_time), &com.integ_time);
	}

	*status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; c-basic-offset:4  ***
;;; End: ***
*/
