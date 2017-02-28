/*
 XRSpixelToXY.c

	1999/08/06 Y.ISHISAKI	version 1.0

	1999/12/23 Y.ISHISAKI	version 1.2
		changes for new aste_coord
		can read attitude file
		do aberration correction
		update RA_NOM,DEC_NOM,TLMIN,TLMAX,TCRPX,TCRVL,TCDLT,OPTIC of event file

	1999/12/29 Y.ISHISAKI	version 1.21
		change TCDLT precision 8 -> 7 to match value with XIS

	2000/01/26 Y.ISHISAKI	version 1.3
		bug fix on TCRVL for Y
		TCRVL for DET & FOC = 0.0
		TCDLT for DET is measured in mm.

	2004/03/14 Y.ISHISAKI	version 1.4
		include "com.h"
		remove "cfortran.h"
		special treatment of calibration pixel (pixel=2)

	2004/05/14 Y.ISHISAKI	version 1.5
		fix TCDLT for FOCX (should be negative value)

	2005/01/13 T.FURUSHO	version 1.6
		special treatment for  pixel #3, too.

	2005/07/04 Y.ISHISAKI	version 1.7
		parameter names, ea_phi/theta/psi -> ea1/2/3, teldef_xrs -> xrs_teldef
		Put back ea1/2/3, ref_alpha/delta/roll when pointing=KEY
		read MJDREFI, MJDREFF from event file, and use for conversion
		write CALxxx keywords

	2005/07/20 Y.ISHISAKI	version 1.8
		bug fix in setting ROLL for cal pixel reported by E.Winter & K.Ebisawa
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_att.h"
#include "aeFitsHeaderUtil.h"
#include "pil.h"
#include "headas.h"

static char pname[] = "XRSpixelToXY";
char XRSpixelToXY_version[] = "version 1.8";

enum attitude_type {
	ATTITUDE_FILE,		/* specify attitude file */
	ATTITUDE_KEY,		/* read event file MEAN_EA1/2/3 */
	ATTITUDE_USER		/* set manually by user */
};

enum pointing_type {
	POINTING_KEY,
	POINTING_USER
};

static struct {
	TELDEF *teldef;
	ATTFILE *attfile;
	char teldef_file[PIL_LINESIZE];
	char attitude[PIL_LINESIZE];
	char pointing[PIL_LINESIZE];
	enum attitude_type attitude_type;
	enum pointing_type pointing_type;
	int aberration;
	SKYREF sref;
	double ea1, ea2, ea3;	/* deg */
	AtEulerAng ea;			/* radian */
	int    mjdrefi;
	double mjdreff;
} com;

static int
modkeys(fitsfile *fp, char *name, int col,
		int tlmin, int tlmax,
		double tcrpx, double tcrvl, double tcdlt, double optic
)
{
	char key[16], comment[80];
	int istat = 0;

	fflush(NULL);

	sprintf(key, "TLMIN%d", col);
	sprintf(comment, "minimum legal value for %s", name);
	fits_modify_key_lng(fp, key, tlmin, comment, &istat);
	printf("  %-8s = %20d / %s\n", key, tlmin, comment);

	sprintf(key, "TLMAX%d", col);
	sprintf(comment, "maximum legal value for %s", name);
	fits_modify_key_lng(fp, key, tlmax, comment, &istat);
	printf("  %-8s = %20d / %s\n", key, tlmax, comment);

	sprintf(key, "TCRPX%d", col);
	sprintf(comment, "%s reference pixel", name);
	fits_modify_key_fixdbl(fp, key, tcrpx, 1, comment, &istat);
	printf("  %-8s = %20.1f / %s\n", key, tcrpx, comment);

	sprintf(key, "TCRVL%d", col);
	if ( 0 == strncmp("DET", name, 3) ) {
		sprintf(comment, "%s reference pixel value (mm)", name);
	} else {
		sprintf(comment, "%s reference pixel value (deg)", name);
	}
	fits_modify_key_fixdbl(fp, key, tcrvl, 5, comment, &istat);
	printf("  %-8s = %20.5f / %s\n", key, tcrvl, comment);

	sprintf(key, "TCDLT%d", col);
	if ( 0 == strncmp("DET", name, 3) ) {
		sprintf(comment, "%s pixel scale (mm/pixel)", name);
		fits_modify_key_fixdbl(fp, key, tcdlt, 5, comment, &istat);
		printf("  %-8s = %20.5f / %s\n", key, tcdlt, comment);
	} else {
		sprintf(comment, "%s pixel scale (deg/pixel)", name);
		fits_modify_key_fixdbl(fp, key, tcdlt, 7, comment, &istat);
		printf("  %-8s = %20.7f / %s\n", key, tcdlt, comment);
	}

	sprintf(key, "OPTIC%d", col);
	sprintf(comment, "%s of the optical axis (pixel)", name);
	fits_modify_key_fixdbl(fp, key, optic, 2, comment, &istat);
	printf("  %-8s = %20.2f / %s\n", key, optic, comment);

	fflush(NULL);

	if ( istat ) {
		fprintf(stderr, "\
%s: WARNING: updating keyword for %s faild (ignored)\n", pname, name);
	}

	return 0;
}

void
XRSpixelToXY_startup(int *status)
{
	com.attitude_type = ATTITUDE_FILE;
	strcpy(com.pointing, "KEY");
	com.pointing_type = POINTING_KEY;

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "XRS_TELDEF", com.teldef_file);
	if ( ATTITUDE_USER == com.attitude_type ) {
		printf("%20s   %s\n", "ATTITUDE", "USER");
		printf("%20s   (%.4f, %.4f, %.4f)\n", "", com.ea1, com.ea2, com.ea3);
	} else if ( ATTITUDE_KEY == com.attitude_type ) {
		printf("%20s   %s\n", "ATTITUDE", "KEY");
		printf("%20s   (%.4f, %.4f, %.4f)\n", "", com.ea1, com.ea2, com.ea3);
	} else {
		printf("%20s   '%s'\n", "ATTITUDE", com.attitude);
	}
	printf("%20s   %s\n", "POINTING", com.pointing);
	printf("%20s   (%.4f, %.4f, %.1f)\n", "SKYREF",
		   com.sref.alpha, com.sref.delta, com.sref.roll);
	printf("%20s   %s\n", "ABERRATION", com.aberration ? "YES" : "NO");
}

void
XRSpixelToXY_com(int *status)
{
#define NVAL	8
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"XRS_TELDEF",
		"ATTITUDE",
		"POINTING",
		"EULER",
		"SKYREF",
		"ABERRATION",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"teldef file name",
		"attitude file name",
		"pointing type",
		"euler angles in degree",
		"sky reference position",
		"correct aberration",
		"exit from this menu"
	};
	int nreply = 1;
	int answer[2];

	int istat;

	if ( *status ) {	/* ftools */

		*status = ANL_QUIT;

		istat = PILGetFname("xrs_teldef", com.teldef_file);
		if ( istat ) return;
		istat = PILGetFname("attitude", com.attitude);
		if ( istat ) return;
		if ( 0 == CLstricmp("USER", com.attitude) ) {
			com.attitude_type = ATTITUDE_USER;
			if ( PILGetReal("ea1", &com.ea1) ||
				 PILGetReal("ea2", &com.ea2) ||
				 PILGetReal("ea3", &com.ea3) ) {
				return;
			}
			com.ea.phi   = com.ea1 * DEG2RAD;
			com.ea.theta = com.ea2 * DEG2RAD;
			com.ea.psi   = com.ea3 * DEG2RAD;
		} else if ( 0 == CLstricmp("KEY", com.attitude) ) {
			com.attitude_type = ATTITUDE_KEY;
		} else {
			com.attitude_type = ATTITUDE_FILE;
		}
		istat = PILGetString("pointing", com.pointing);
		if ( istat ) return;
		CLstrupc(com.pointing);
		if ( 0 == strcmp("KEY", com.pointing) ) {
			com.pointing_type = POINTING_KEY;
		} else if ( 0 == strcmp("USER", com.pointing) ) {
			com.pointing_type = POINTING_USER;
			if ( PILGetReal("ref_alpha", &com.sref.alpha) ||
				 PILGetReal("ref_delta", &com.sref.delta) ||
				 PILGetReal("ref_roll",  &com.sref.roll) ) {
				return;
			}
		} else {
			fprintf(stderr, "\
%s: POINTING=%s not supported\n", pname, com.pointing);
			return;
		}
		istat = PILGetBool("aberration", &com.aberration);
		if ( istat ) return;

		*status = ANL_OK;;
		return;
	}

	for (;;) {
		char *p;

		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("XRS_TELDEF", p) ) {
			CLtxtrd(p, com.teldef_file, sizeof(com.teldef_file));
		} else if ( 0 == strcmp("ATTITUDE", p) ) {
			com.attitude_type = ATTITUDE_FILE;
			CLtxtrd(p, com.attitude, sizeof(com.attitude));
			if ( 0 == CLstricmp("KEY", com.attitude) ) {
				com.attitude_type = ATTITUDE_KEY;
				CLstrupc(com.attitude);
			} else if ( 0 == CLstricmp("USER", com.attitude) ) {
				com.attitude_type = ATTITUDE_USER;
				CLstrupc(com.attitude);
			}
		} else if ( 0 == strcmp("POINTING", p) ) {
			CLtxtrd(p, com.attitude, sizeof(com.pointing));
			CLstrupc(com.pointing);
			if ( 0 == strcmp("KEY", com.pointing) ) {
				com.pointing_type = POINTING_KEY;
			} else if ( 0 == strcmp("USER", com.pointing) ) {
				com.pointing_type = POINTING_USER;
			} else {
				com.pointing_type = POINTING_KEY;
				fprintf(stderr, "\
%s: POINTING=%s not supported, KEY assumed\n", pname, com.pointing);
			}
		} else if ( 0 == strcmp("EULER", p) ) {
			com.attitude_type = ATTITUDE_USER;
			CLfdprd("EULER  phi  (deg)", &com.ea1);
			CLfdprd("EULER theta (deg)", &com.ea2);
			CLfdprd("EULER  psi  (deg)", &com.ea3);
			com.ea.phi   = com.ea1 * DEG2RAD;
			com.ea.theta = com.ea2 * DEG2RAD;
			com.ea.psi   = com.ea3 * DEG2RAD;
		} else if ( 0 == strcmp("SKYREF", p) ) {
			com.pointing_type = POINTING_USER;
			CLfdprd("SKYREF alpha (deg)", &com.sref.alpha);
			CLfdprd("SKYREF delta (deg)", &com.sref.delta);
			CLfdprd("SKYREF roll  (deg)", &com.sref.roll);
		} else if ( 0 == strcmp("ABERRATION", p) ) {
			CLlogrd(p, &com.aberration);
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
XRSpixelToXY_init(int *status)
{
	int used;
	fitsfile *ifp;
	char *k, *ifn;

	int istat = 0;

/* get event file name & FITS file pointer */
	BnkfGetM("XRS:EVENT:IFILE_NAME:PTR",sizeof(ifn), &used, &ifn);
	if ( sizeof(ifn) != used ) {
		ifn = NULL;
	}

	BnkfGetM("XRS:EVENT:IFP", sizeof(ifp), &used, &ifp);
	if ( 0 == used ) {
		*status = ANL_QUIT;
		return;
	}

/* open teldef file */
	com.teldef = aste_coord_init(NULL, "XRS", com.teldef_file);
	if ( NULL == com.teldef ) {
		fprintf(stderr, "\
%s: aste_coord_init('%s') failed\n", pname, com.teldef_file);
		*status = ANL_QUIT;
		return;
	}

/* open attitude file */
	if ( ATTITUDE_FILE == com.attitude_type ) {
		com.attfile = aste_att_init(com.attitude);
		if ( NULL == com.attfile ) {
			fprintf(stderr, "\
%s: aste_att_init('%s') failed\n", pname, com.attitude);
			*status = ANL_QUIT;
			return;
		}
	} else {
		com.attfile = NULL;
	}

/* get MJDREFI & MJDREFF keywords */
	if (
fits_read_key(ifp, TINT, k="MJDREFI", &com.mjdrefi, NULL, &istat) ||
fits_read_key_dbl(ifp, k="MJDREFF", &com.mjdreff, NULL, &istat) ||
		 0 ) {
		fprintf(stderr, "\
%s: fits_read_key('%s') failed\n", pname, k);
		*status = ANL_QUIT;
		return;
	}

/* get mean Euler angles */
	if ( ATTITUDE_USER != com.attitude_type ) {

		if (
fits_read_key_dbl(ifp, k="MEAN_EA1", &com.ea1, NULL, &istat) ||
fits_read_key_dbl(ifp, k="MEAN_EA2", &com.ea2, NULL, &istat) ||
fits_read_key_dbl(ifp, k="MEAN_EA3", &com.ea3, NULL, &istat) ||
			 0 ) {
			fprintf(stderr, "\
%s: fits_read_key('%s') failed\n", pname, k);
			*status = ANL_QUIT;
			return;
		}

		com.ea.phi   = com.ea1 * DEG2RAD;
		com.ea.theta = com.ea2 * DEG2RAD;
		com.ea.psi   = com.ea3 * DEG2RAD;

		PILPutReal("ea1", com.ea1);
		PILPutReal("ea2", com.ea2);
		PILPutReal("ea3", com.ea3);

	}

/* set sky reference position */
	if ( POINTING_KEY == com.pointing_type ) {
		fflush(NULL); printf("\
%s: reading RA_NOM, DEC_NOM from '%s'\n", pname, ifn);
		fflush(NULL);
		fits_read_key_dbl(ifp, k="RA_NOM", &com.sref.alpha, NULL, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: '%s' not found in '%s'\n", pname, k, ifn);
			*status = ANL_QUIT;
			return;
		}
		fits_read_key_dbl(ifp, k="DEC_NOM", &com.sref.delta, NULL, &istat);
		if ( istat ) {
			fprintf(stderr, "\
%s: '%s' not found in '%s'\n", pname, k, ifn);
			*status = ANL_QUIT;
			return;
		}
		com.sref.roll = 0.0;

		PILPutReal("ref_alpha", com.sref.alpha);
		PILPutReal("ref_delta", com.sref.delta);
		PILPutReal("ref_roll",  com.sref.roll);

	}

	show_parameter();

	*status = ANL_OK;
}

void
XRSpixelToXY_his(int *status)
{
	*status = ANL_OK;
}

void
XRSpixelToXY_bgnrun(int *status)
{
	static TELDEF_ASTROE ta;
	static struct {
		char *key, *comment;
		int *ipos;
		double *dpos;
	} cal[] = {
{ "CALPIXEL", "ID number of the calibration pixel", &ta.calpixel, NULL},
{ "CAL_DETX", "DET X value for the calibration pixel", &ta.cal_detx, NULL },
{ "CAL_DETY", "DET Y value for the calibration pixel", &ta.cal_dety, NULL },
{ "CAL_FOCX", "FOC X value for the calibration pixel", &ta.cal_focx, NULL },
{ "CAL_FOCY", "FOC Y value for the calibration pixel", &ta.cal_focy, NULL },
{ "CAL_X",    "SKY X value for the calibration pixel", &ta.cal_x, NULL },
{ "CAL_Y",    "SKY Y value for the calibration pixel", &ta.cal_y, NULL },
{ "CAL_ROLL", "ROLL  value for the calibration pixel", NULL, &ta.cal_roll },
{ NULL, NULL, NULL }
	};

	fitsfile *fp;
	char *key, *comment, buf[80];
	int i, col, tlmin, tlmax, ival;
	double alpha, delta, tcrpx, tcrvl, tcdlt, optic, optx_ch, opty_ch, dval;

	TELDEF *teldef = com.teldef;
	TELDEF_ASTROE *p = teldef->mission.aste;

	int used = 0;
	int istat = 0;

	*status = ANL_QUIT;

	BnkGet("XRS:EVENT:OFP", sizeof(fp), &used, &fp);
	if ( used != sizeof(fp) || NULL == fp ) {
		return;
	}

	sprintf(buf, "%s %s", pname, XRSpixelToXY_version);
	fits_write_history(fp, buf, &istat);
	sprintf(buf, "  xrs_teldef='%s'", com.teldef_file);
	fits_write_history(fp, buf, &istat);
	if ( ATTITUDE_USER == com.attitude_type ) {
		sprintf(buf, "\
  attitude=(%.4f, %.4f, %.4f)", com.ea1, com.ea2, com.ea3);
	} else {
		sprintf(buf, "\
  attitude='%s'", com.attitude);
		fits_write_history(fp, buf, &istat);
		sprintf(buf, "\
    (mean)=(%.4f, %.4f, %.4f)", com.ea1, com.ea2, com.ea3);
	}
	fits_write_history(fp, buf, &istat);
	sprintf(buf, "\
  pointing=%s, aberration=%s", com.pointing, com.aberration ? "yes" : "no");
	fits_write_history(fp, buf, &istat);
	sprintf(buf, "\
  skyref=(%.4f, %.4f, %.1f)", com.sref.alpha, com.sref.delta, com.sref.roll);
	fits_write_history(fp, buf, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history failed (%d)\n", pname, istat);
		return;
	}

	fflush(NULL); printf("\n\
%s: updating header keywords ...\n\n", pname);

	aste_det2ecs(teldef, &com.ea, p->det.xcen, p->det.ycen, &alpha, &delta);
	if ( fits_get_colnum(fp, CASESEN, "DETX", &col, &istat) ) {
		istat = 0;				/* ignore it */
	} else {
		tlmin = p->det.xpix1;
		tlmax = p->det.xpix1 + p->det.xsiz - 1;
		tcrpx = p->det.xcen;
		tcrvl = 0.0;
		tcdlt = p->det.xscl;
		optic = p->optaxisx;
		modkeys(fp, "DETX", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic);
	}
	if ( fits_get_colnum(fp, CASESEN, "DETY", &col, &istat) ) {
		istat = 0;				/* ignore it */
	} else {
		tlmin = p->det.ypix1;
		tlmax = p->det.ypix1 + p->det.ysiz - 1;
		tcrpx = p->det.ycen;
		tcrvl = 0.0;
		tcdlt = p->det.yscl;
		optic = p->optaxisy;
		modkeys(fp, "DETY", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic);
	}
	aste_foc2ecs(teldef, &com.ea, p->foc.xcen, p->foc.ycen, &alpha, &delta);
	aste_det2foc(teldef, p->optaxisx, p->optaxisy, &optx_ch, &opty_ch);
	if ( fits_get_colnum(fp, CASESEN, "FOCX", &col, &istat) ) {
		istat = 0;				/* ignore it */
	} else {
		tlmin = p->foc.xpix1;
		tlmax = p->foc.xpix1 + p->foc.xsiz - 1;
		tcrpx = p->foc.xcen;
		tcrvl = 0.0;
		tcdlt = - (p->foc.xscl / p->focallen) * RAD2DEG; /* negative */
		optic = optx_ch;
		modkeys(fp, "FOCX", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic);
	}
	if ( fits_get_colnum(fp, CASESEN, "FOCY", &col, &istat) ) {
		istat = 0;				/* ignore it */
	} else {
		tlmin = p->foc.ypix1;
		tlmax = p->foc.ypix1 + p->foc.ysiz - 1;
		tcrpx = p->foc.ycen;
		tcrvl = 0.0;
		tcdlt = (p->foc.yscl / p->focallen) * RAD2DEG;
		optic = opty_ch;
		modkeys(fp, "FOCY", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic);
	}
	aste_det2sky(teldef, &com.ea, &com.sref, p->optaxisx, p->optaxisy,
				 &optx_ch, &opty_ch);
	if ( fits_get_colnum(fp, CASESEN, "X", &col, &istat) ) {
		istat = 0;				/* ignore it */
	} else {
		tlmin = p->sky.xpix1;
		tlmax = p->sky.xpix1 + p->sky.xsiz - 1;
		tcrpx = p->sky.xcen;
		tcrvl = com.sref.alpha;
		tcdlt = - (p->sky.xscl / p->focallen) * RAD2DEG; /* negative */
		optic = optx_ch;
		modkeys(fp, "X", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic);
	}
	if ( fits_get_colnum(fp, CASESEN, "Y", &col, &istat) ) {
		istat = 0;				/* ignore it */
	} else {
		tlmin = p->sky.ypix1;
		tlmax = p->sky.ypix1 + p->sky.ysiz - 1;
		tcrpx = p->sky.ycen;
		tcrvl = com.sref.delta;
		tcdlt = (p->sky.yscl / p->focallen) * RAD2DEG;
		optic = opty_ch;
		modkeys(fp, "Y", col, tlmin, tlmax, tcrpx, tcrvl, tcdlt, optic);
	}

	ta = *p;
	for (i = 0; NULL != cal[i].key; i++) {
		key = cal[i].key;
		comment = cal[i].comment;
		if ( NULL != cal[i].ipos ) {
			ival = *cal[i].ipos;
			aefits_del_write_key(fp, TINT, key, &ival, comment, &istat);
			printf("  %-8s = %20d / %s\n", key, ival, comment);
		} else {
			dval = *cal[i].dpos;
			aefits_del_write_key_fixdbl(fp, key, dval, 1, comment, &istat);
			printf("  %-8s = %20.1f / %s\n", key, dval, comment);
		}
		if ( istat ) {
			fprintf(stderr, "\
%s: aefits_del_write_key('%s') failed (%d)\n", pname, key, istat);
			return;
		}
	}

	printf("\n"); fflush(NULL);

	*status = ANL_OK;
}

void
XRSpixelToXY_ana(int *nevent, int *eventid, int *status)
{
	int used, istat, pixel;
	double aetime, mjd_tt;
	double detx_ch, dety_ch;
	double focx_ch, focy_ch;
	double alpha, delta, r;
	double skyx_ch, skyy_ch;
	int idetx, idety, ifocx, ifocy, iskyx, iskyy;
	SKYREF fov;
	AtEulerAng ea;
	AtVect v0, v1;

	TELDEF *teldef = com.teldef;
	TELDEF_ASTROE *p = teldef->mission.aste;

	BnkfGetM("XRS:TIME", sizeof(aetime), &used, &aetime);
	BnkfGetM("XRS:PIXEL", sizeof(pixel), &used, &pixel);

/* check if calibration pixel */
	if ( p->calpixel  == pixel ) {
		idetx = p->cal_detx;
		idety = p->cal_dety;
		ifocx = p->cal_focx;
		ifocy = p->cal_focy;
		iskyx = p->cal_x;
		iskyy = p->cal_y;
		fov.roll = p->cal_roll;
		goto skip;
	}

	xrs_pixel2det(teldef, pixel, 0, &detx_ch, &dety_ch);
	aste_det2foc(teldef, detx_ch, dety_ch, &focx_ch, &focy_ch);
	if ( ATTITUDE_USER == com.attitude_type ) {
		ea = com.ea;
	} else {
		istat = aste_att_euler(aetime, &ea);
		if ( istat ) {
			fprintf(stderr, "\
%s: t=%.3f out of range of attfile '%s'\n", pname, aetime, com.attitude);
			*status = ANL_QUIT;
			return;
		}
	}

	aste_foc2ecs(teldef, &ea, focx_ch, focy_ch, &alpha, &delta);
	if ( com.aberration ) {
		mjd_tt = aefits_mission2mjd_tt(aetime, com.mjdrefi, com.mjdreff);
		atPolDegToVect(1.0, alpha, delta, v0);
		atAberration(mjd_tt, v0, v1);
		atVectToPolDeg(v1, &r, &alpha, &delta);
	}
	aste_ecs2sky(teldef, &com.sref, alpha, delta, &skyx_ch, &skyy_ch);
	aste_euler2skyref(teldef, &ea, &fov);

	idetx = (int)(detx_ch + 10000.5) - 10000;
	idety = (int)(dety_ch + 10000.5) - 10000;
	ifocx = (int)(focx_ch + 10000.5) - 10000;
	ifocy = (int)(focy_ch + 10000.5) - 10000;
	iskyx = (int)(skyx_ch + 10000.5) - 10000;
	iskyy = (int)(skyy_ch + 10000.5) - 10000;

 skip:

	BnkfPutM("XRS:DETX", sizeof(idetx), &idetx);
	BnkfPutM("XRS:DETY", sizeof(idety), &idety);
	BnkfPutM("XRS:FOCX", sizeof(ifocx), &ifocx);
	BnkfPutM("XRS:FOCY", sizeof(ifocy), &ifocy);
	BnkfPutM("XRS:X", sizeof(iskyx), &iskyx);
	BnkfPutM("XRS:Y", sizeof(iskyy), &iskyy);
	BnkfPutM("XRS:ROLL", sizeof(fov.roll), &fov.roll);

	*status = ANL_OK;
}

void
XRSpixelToXY_endrun(int *status)
{
	*status = ANL_OK;
}

void
XRSpixelToXY_exit(int *status)
{
	*status = ANL_OK;
}
