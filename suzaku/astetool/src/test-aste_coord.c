#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>
#include "atFunctions.h"
#include "aste_coord.h"

double
rikaku(double alpha1, double delta1, double alpha2, double delta2)
{
	double r;
	AtVect v1, v2;

	atPolDegToVect(1, alpha1, delta1, v1);
	atPolDegToVect(1, alpha2, delta2, v2);

	atAngDistance(v1, v2, &r);

	return r * RAD2DEG;
}

int
test_core(TELDEF *teldef, double detx_ch, double dety_ch, AtEulerAng eulang, SKYREF skyref)
{
	static double eps = 1.0e-12;
	double focx_ch, focy_ch, skyx_ch, skyy_ch, alpha, delta;
	double detx2, dety2, focx2, focy2, skyx2, skyy2, alpha2, delta2;
	double detx_mm, dety_mm, focx_mm, focy_mm, skyx_mm, skyy_mm;

	printf("\
SKYREF = (%.4f, %.4f, %.4f), EULER = (%.4f, %.4f, %.4f)\n",
		   skyref.alpha, skyref.delta, skyref.roll,
		   eulang.phi*RAD2DEG, eulang.theta*RAD2DEG, eulang.psi*RAD2DEG);

	aste_det2foc(teldef, detx_ch, dety_ch, &focx_ch, &focy_ch);
	aste_foc2ecs(teldef, &eulang, focx_ch, focy_ch, &alpha, &delta);
	aste_ecs2sky(teldef, &skyref, alpha, delta, &skyx_ch, &skyy_ch);

	printf("\
DET(%.2f,%.2f)-> FOC(%.2f,%.2f)-> ECS(%.3f,%.3f)-> SKY(%.2f,%.2f)\n",
		   detx_ch, dety_ch, focx_ch, focy_ch, alpha, delta, skyx_ch, skyy_ch);

	aste_det_ch2mm(teldef, detx_ch, dety_ch, &detx_mm, &dety_mm);
	aste_foc_ch2mm(teldef, focx_ch, focy_ch, &focx_mm, &focy_mm);
	aste_sky_ch2mm(teldef, skyx_ch, skyy_ch, &skyx_mm, &skyy_mm);

	printf("\
DET (%.5f,%.5f) mm, FOC(%.5f,%.5f) mm,  SKY(%.5f,%.5f) mm\n",
		   detx_mm, dety_mm, focx_mm, focy_mm, skyx_mm, skyy_mm);

	printf("\
checking functions ...");

	printf(" aste_foc2det"); fflush(stdout);
	aste_foc2det(teldef, focx_ch, focy_ch, &detx2, &dety2);
	if ( eps < fabs(detx2 - detx_ch) && eps < fabs(dety2 - dety_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_ecs2foc"); fflush(stdout);
	aste_ecs2foc(teldef, &eulang, alpha, delta, &focx2, &focy2);
	if ( eps < fabs(focx2 - focx_ch) && eps < fabs(focy2 - focy_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_sky2ecs"); fflush(stdout);
	aste_sky2ecs(teldef, &skyref, skyx_ch, skyy_ch, &alpha2, &delta2);
	if ( eps < rikaku(alpha, delta, alpha2, delta2) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_det2ecs"); fflush(stdout);
	aste_det2ecs(teldef, &eulang, detx_ch, dety_ch, &alpha2, &delta2);
	if ( eps < rikaku(alpha, delta, alpha2, delta2) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_ecs2det"); fflush(stdout);
	aste_ecs2det(teldef, &eulang, alpha, delta, &detx2, &dety2);
	if ( eps < fabs(detx2 - detx_ch) && eps < fabs(dety2 - dety_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_det2sky"); fflush(stdout);
	aste_det2sky(teldef, &eulang, &skyref, detx_ch, dety_ch, &skyx2, &skyy2);
	if ( eps < fabs(skyx2 - skyx_ch) && eps < fabs(skyy2 - skyy_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_sky2det"); fflush(stdout);
	aste_sky2det(teldef, &eulang, &skyref, skyx_ch, skyy_ch, &detx2, &dety2);
	if ( eps < fabs(detx2 - detx_ch) && eps < fabs(dety2 - dety_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_foc2sky"); fflush(stdout);
	aste_foc2sky(teldef, &eulang, &skyref, focx_ch, focy_ch, &skyx2, &skyy2);
	if ( eps < fabs(skyx2 - skyx_ch) && eps < fabs(skyy2 - skyy_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_sky2foc"); fflush(stdout);
	aste_sky2foc(teldef, &eulang, &skyref, skyx_ch, skyy_ch, &focx2, &focy2);
	if ( eps < fabs(focx2 - focx_ch) && eps < fabs(focy2 - focy_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_det_mm2ch"); fflush(stdout);
	aste_det_mm2ch(teldef, detx_mm, dety_mm, &detx2, &dety2);
	if ( eps < fabs(detx2 - detx_ch) && eps < fabs(dety2 - dety_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_foc_mm2ch"); fflush(stdout);
	aste_foc_mm2ch(teldef, skyx_mm, skyy_mm, &focx2, &focy2);
	if ( eps < fabs(focx2 - focx_ch) && eps < fabs(focy2 - focy_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" aste_sky_mm2ch"); fflush(stdout);
	aste_sky_mm2ch(teldef, skyx_mm, skyy_mm, &skyx2, &skyy2);
	if ( eps < fabs(skyx2 - skyx_ch) && eps < fabs(skyy2 - skyy_ch) ) {
		printf(" ERROR !!!\n");
		return -1;
	}

	printf(" OK\n");

	return 0;
}

int
test_xrs(TELDEF *teldef)
{
	int pixel, pixel2, i;
	double detx_ch, dety_ch, detx_mm, dety_mm, detx[4], dety[4];

	for (pixel = 0; pixel < 32; pixel++) {
		xrs_pixel2det(teldef, pixel, 0, &detx_ch, &dety_ch);
		aste_det_ch2mm(teldef, detx_ch, dety_ch, &detx_mm, &dety_mm);
		xrs_det2pixel(teldef, detx_ch, dety_ch, &pixel2);
		if ( pixel != pixel2 ) {
			printf("\
PIXEL %d -> %d !!!\n", pixel, pixel2);
			return -1;
		}
		for (i = 0; i < 4; i++) {
			xrs_pixel2det(teldef, pixel, i+1, &detx[i], &dety[i]);
			aste_det_ch2mm(teldef, detx[i], dety[i], &detx[i], &dety[i]);
		}
		printf("\
PIXEL%02d: (%.2f,%.2f) = (%.3f,%.3f) mm, %.3f %.3f %.3f %.3f\n",
			   pixel, detx_ch, dety_ch, detx_mm, dety_mm,
			   detx[1]-detx[0], -(dety[2]-dety[1]),
			   detx[1]-detx[3], -(dety[3]-dety[0]));
	}

	return 0;
}

int
test_xis(TELDEF *teldef)
{
	int segid, ppux, ppuy, rawx, rawy, actx, acty, detx, dety;
	int segid2, rawx2, rawy2, actx2, acty2;

	segid = 0; rawx = 0; rawy = 0;

	for (segid = 0; segid < 4; segid++) {
		xis_raw2ppu(teldef, rawx, rawy, &ppux, &ppuy);
		xis_raw2act(teldef, segid, rawx, rawy, 0, 0, &actx, &acty);
		xis_act2det(teldef, actx, acty, &detx, &dety);

		printf("\
RAW(%d,%d,%d)-> ACT(%d,%d)-> DET(%d,%d)\n",
			   segid, rawx, rawy, actx, acty, detx, dety);

		xis_det2act(teldef, detx, dety, &actx2, &acty2);
		if ( actx != actx2 || acty != acty2 ) {
			printf("xis_det2act ERROR!!!\n");
			return -1;
		}
		xis_act2raw(teldef, actx, acty, 0, 0, &segid2, &rawx2, &rawy2);
		if ( segid != segid2 || rawx != rawx2 || rawy != rawy2 ) {
			printf("xis_act2raw ERROR!!!\n");
			return -1;
		}
		xis_ppu2raw(teldef, ppux, ppuy, &rawx2, &rawy2);
		if ( rawx != rawx2 || rawy != rawy2 ) {
			printf("xis_ppu2raw ERROR!!!\n");
			return -1;
		}
	}

	return 0;
}

int
test_hxd(TELDEF *teldef)
{
	static double eps = 1.0e-12;
	static AtEulerAng eulang = { 0.0, 90.0*DEG2RAD, 90.0*DEG2RAD };
	double alpha, delta, alpha2, delta2;
	int ipin, igso;
	double detx_ch, dety_ch, detx_mm, dety_mm, theta, phi;

	alpha = 1.0;
	delta = 0.0;

	printf("\
EULER = (%.4f, %.4f, %.4f), (RA,DEC) = (%.4f, %.4f)\n",
		   eulang.phi*RAD2DEG, eulang.theta*RAD2DEG, eulang.psi*RAD2DEG,
		   alpha, delta);

	for (ipin = 0; ipin < 64; ipin++) {
		hxd_pin2det(teldef, ipin, &detx_ch, &dety_ch);
		aste_det_ch2mm(teldef, detx_ch, dety_ch, &detx_mm, &dety_mm);
		hxd_pin_ecs2pol(teldef, &eulang, ipin, alpha, delta, &theta, &phi);
		hxd_pin_pol2ecs(teldef, &eulang, ipin, theta, phi, &alpha2, &delta2);
		if ( eps < rikaku(alpha, delta, alpha2, delta2) ) {
			printf(" ERROR !!!\n");
			return -1;
		}
		printf("\
PIN%02d: (%.2f,%.2f) = (%.4f,%.4f) mm, theta=%.4f, phi=%.4f\n",
			   ipin, detx_ch, dety_ch, detx_mm, dety_mm, theta, phi);
	}

	alpha = 0.0;
	delta = 1.0;

	printf("\
EULER = (%.4f, %.4f, %.4f), (RA,DEC) = (%.4f, %.4f)\n",
		   eulang.phi*RAD2DEG, eulang.theta*RAD2DEG, eulang.psi*RAD2DEG,
		   alpha, delta);

	for (igso = 0; igso < 16; igso++) {
		hxd_gso2det(teldef, igso, &detx_ch, &dety_ch);
		aste_det_ch2mm(teldef, detx_ch, dety_ch, &detx_mm, &dety_mm);
		hxd_gso_ecs2pol(teldef, &eulang, igso, alpha, delta, &theta, &phi);
		hxd_gso_pol2ecs(teldef, &eulang, igso, theta, phi, &alpha2, &delta2);
		if ( eps < rikaku(alpha, delta, alpha2, delta2) ) {
			printf(" ERROR !!!\n");
			return -1;
		}
		printf("\
GSO%02d: (%.2f,%.2f) = (%.4f,%.4f) mm, theta=%.4f, phi=%.4f\n",
			   igso, detx_ch, dety_ch, detx_mm, dety_mm, theta, phi);
	}

	return 0;
}

int
test_aste_coord(TELDEF *teldef)
{
	static AtEulerAng eulang = { 0.0, 90.0*DEG2RAD, 90.0*DEG2RAD };
	static SKYREF skyref = { 0.0, 0.0, 0.0 };
	double detx_ch, dety_ch;
	TELDEF_ASTROE *p = teldef->mission.aste;

	printf("\
Testing '%s'\n", teldef->actual_filename);
	printf("\
TELESCOP= '%s'\n\
INSTRUME= '%s'\n\
FILENAME= '%s'\n\
DATE    = '%s'\n\
VERSION = %d\n\
NCOORDS = %d\n",
		   teldef->telescop, teldef->instrume, teldef->filename, teldef->date,
		   teldef->version, teldef->ncoords);

	detx_ch = p->det.xcen;
	dety_ch = p->det.ycen;
	test_core(teldef, detx_ch, dety_ch, eulang, skyref);

	detx_ch = p->det.xcen + 100;
	dety_ch = p->det.ycen;
	test_core(teldef, detx_ch, dety_ch, eulang, skyref);

	detx_ch = p->det.xcen;
	dety_ch = p->det.ycen + 100;
	test_core(teldef, detx_ch, dety_ch, eulang, skyref);

	return 0;
}

int
main(void)
{
	TELDEF *teldef;
/*	static char *xrs_teldef = "/usr/local/astroe/com/src/caldb/xrs_teldef/2005-05-25/xrs_teldef_2005-05-25.fits";*/
	static char *xrs_teldef = "/usr/local/astroe/com/src/caldb/xrs_teldef/2005-06-22/ae_xrs_teldef_20050622.fits";
	static char *xis_teldef[4] = {
/*		"none",
		"none",
		"none",
		"none" */
/*		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-05-25/xis0_teldef_2005-05-25.fits",
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-05-25/xis1_teldef_2005-05-25.fits",
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-05-25/xis2_teldef_2005-05-25.fits",
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-05-25/xis3_teldef_2005-05-25.fits" */
/*		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-06-15/ae_xis0_teldef_20050615.fits",
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-06-15/ae_xis1_teldef_20050615.fits",
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-06-15/ae_xis2_teldef_20050615.fits",
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-06-15/ae_xis3_teldef_20050615.fits"*/
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-09-22/ae_xi0_teldef_20050922.fits",
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-09-22/ae_xi1_teldef_20050922.fits",
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-09-22/ae_xi2_teldef_20050922.fits",
		"/usr/local/astroe/com/src/caldb/xis_teldef/2005-09-22/ae_xi3_teldef_20050922.fits"
	};
	static char *hxd_teldef = "/usr/local/astroe/com/src/caldb/hxd_teldef/1999-12-29/hxd_teldef_1999-12-29.fits";

	teldef = aste_coord_init("Astro-E2", "XRS", xrs_teldef);
	aste_coord_free(teldef);
	teldef = aste_coord_init(NULL, NULL, xrs_teldef);
	if ( aste_coord_teldef("Astro-E2", "XRS") != teldef ) {
		printf("aste_coord_teldef(\"Astro-E2\", \"XRS\") != teldef\n");
	}
	test_aste_coord(teldef);
	test_xrs(teldef);
	aste_coord_free(teldef);

	teldef = aste_coord_init("SUZAKU", "XIS0", xis_teldef[0]);
	if ( 0 != strcmp("none", xis_teldef[0]) ) {
		aste_coord_free(teldef);
		teldef = aste_coord_init(NULL, NULL, xis_teldef[0]);
	}
	if ( aste_coord_teldef("SUZAKU", "XIS0") != teldef ) {
		printf("aste_coord_teldef(\"SUZAKU\", \"XIS0\") != teldef\n");
	}
	test_aste_coord(teldef);
	test_xis(teldef);
	aste_coord_free(teldef);

	teldef = aste_coord_init("SUZAKU", "XIS1", xis_teldef[1]);
	if ( 0 != strcmp("none", xis_teldef[1]) ) {
		aste_coord_free(teldef);
		teldef = aste_coord_init(NULL, NULL, xis_teldef[1]);
	}
	if ( aste_coord_teldef("SUZAKU", "XIS1") != teldef ) {
		printf("aste_coord_teldef(\"SUZAKU\", \"XIS1\") != teldef\n");
	}
	test_aste_coord(teldef);
	test_xis(teldef);
	aste_coord_free(teldef);

	teldef = aste_coord_init("SUZAKU", "XIS2", xis_teldef[2]);
	if ( 0 != strcmp("none", xis_teldef[2]) ) {
		aste_coord_free(teldef);
		teldef = aste_coord_init(NULL, NULL, xis_teldef[2]);
	}
	if ( aste_coord_teldef("SUZAKU", "XIS2") != teldef ) {
		printf("aste_coord_teldef(\"SUZAKU\", \"XIS2\") != teldef\n");
	}
	test_aste_coord(teldef);
	test_xis(teldef);
	aste_coord_free(teldef);

	teldef = aste_coord_init("SUZAKU", "XIS3", xis_teldef[3]);
	if ( 0 != strcmp("none", xis_teldef[3]) ) {
		aste_coord_free(teldef);
		teldef = aste_coord_init(NULL, NULL, xis_teldef[3]);
	}
	if ( aste_coord_teldef("SUZAKU", "XIS3") != teldef ) {
		printf("aste_coord_teldef(\"SUZAKU\", \"XIS3\") != teldef\n");
	}
	test_aste_coord(teldef);
	test_xis(teldef);
	aste_coord_free(teldef);

	teldef = aste_coord_init("ASTRO-E", "HXD", hxd_teldef);
	aste_coord_free(teldef);
	teldef = aste_coord_init(NULL, NULL, hxd_teldef);
	if ( aste_coord_teldef("ASTRO-E", "HXD") != teldef ) {
		printf("aste_coord_teldef(\"ASTRO-E\", \"HXD\") != teldef\n");
	}
	test_aste_coord(teldef);
	test_hxd(teldef);
	aste_coord_free(teldef);

	return 0;
}
