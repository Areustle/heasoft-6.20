/***********************************************************************
c gisspec.f
c     Spectrum Analysis Routines
c  11/10 1993 created by H.Kubo
c  11/19 1993 H.Kubo EVS GISSPEC:BEGIN,ENTRY,OK
c  12/3  1993 H.Kubo BNK_ -> BNK
c  12/6  1993 M.Hirayama add ANL_put_version
c  12/6  1993 M.Hirayama include '/utk/lib/asca_anl_v0.3/include/asca_anl.inc'
c  12/10 1993 M.Hirayama change var. name (recid -> eventid)
c  12/12 1993 M.Hirayama change include file('/utk/lib/...' -> 'Includes.inc')
c   3/30 1994 H.Kubo     update for asca_anl_v0.5
c   4/10 1994 Y.Ishisaki GIS: -> ANL:, use ANL:SENSOR
c   6/30 1994 Y.Ishisaki add RT, RTI, DETX/Y, X/Y, and correct bin cut
c  11/29 2005 Y.Ishisaki check sensor id, before BNKGET
************************************************************************/
#include <stdio.h>
#include <cfortran.h>
#include <hbook.h>
#include <cli.h>
#include <com.h>
#include <bnk.h>
#include <evs.h>
#include <asca_anl.h>
#include <anl_misc.h>

void gisspec_init(int *);
void gisspec_com(int *);
void gisspec_his(int *);
void gisspec_bgnrun(int *);
void gisspec_ana(int, int, int *);
void gisspec_endrun(int *);
void gisspec_exit(int *);

void
gisspec_init(int *status)
{
	EVSDEF("GISSPEC:BEGIN");
	EVSDEF("GISSPEC:ENTRY");
	EVSDEF("GISSPEC:OK");
	ANL_PUT_VERSION("GISSPEC", "version 4.1");

	*status = ASCA_ANL_OK;
}

void
gisspec_com(int *status)
{
	*status = ASCA_ANL_OK;
}

void
gisspec_his(int *status)
{
/* PHA is 0-1023 */
	HBOOK1(200, "GIS2 PHA", 1024, -0.5, 1023.5, 0.0);
	HBOOK1(300, "GIS3 PHA", 1024, -0.5, 1023.5, 0.0);
/* PI is 0-1023 */
	HBOOK1(210, "GIS2 PI", 1024, -0.5, 1023.5, 0.0);
	HBOOK1(310, "GIS3 PI", 1024, -0.5, 1023.5, 0.0);
/* RISE_TIME is 0-255 */
	HBOOK1(220, "GIS2 RISE_TIME", 256, -0.5, 255.5, 0.0);
	HBOOK1(320, "GIS3 RISE_TIME", 256, -0.5, 255.5, 0.0);
/* RTI is 0-255 */
	HBOOK1(230, "GIS2 RTI", 256, -0.5, 255.5, 0.0);
	HBOOK1(330, "GIS3 RTI", 256, -0.5, 255.5, 0.0);
/* RAWX/Y is 0-255 */
	HBOOK2(240, "GIS2 RAWX vs RAWY", 256, -0.5, 255.5, 256, -0.5, 255.5, 0.0);
	HBOOK2(340, "GIS3 RAWX vs RAWY", 256, -0.5, 255.5, 256, -0.5, 255.5, 0.0);
/* DETX/Y is 1-256 */
	HBOOK2(250, "GIS2 DETX vs DETY", 256, 0.5, 256.5, 256, 0.5, 256.5, 0.0);
	HBOOK2(350, "GIS3 DETX vs DETY", 256, 0.5, 256.5, 256, 0.5, 256.5, 0.0);
/* SKYX/Y is 1-256 */
	HBOOK2(260, "GIS2 X vs Y", 256, 0.5, 256.5, 256, 0.5, 256.5, 0.0);
	HBOOK2(360, "GIS3 X vs Y", 256, 0.5, 256.5, 256, 0.5, 256.5, 0.0);

	*status = ASCA_ANL_OK;
}

void
gisspec_bgnrun(int *status)

{
	EVSSET("GISSPEC:BEGIN");

	*status = ASCA_ANL_OK;
}

void
gisspec_ana(int nevent, int eventid, int *status)
{
	int size, offset;
	int sensor, pha, pi, rt, rti;
	int rawx, rawy, detx, dety, skyx, skyy;

	EVSSET("GISSPEC:ENTRY");

/* check if PH mode or not */
	if ( EVS("GIS:PH mode") ) {
		;
	} else {
		puts("DATAMODE is not PH");
		*status = ASCA_ANL_QUIT;
		return;
	}

/* get sensor id */
	BNKGET("ANL:SENSOR", sizeof(sensor), size, &sensor);

/* check sensor id */
	if ( 2 == sensor || 3 == sensor ) {

/* get pha and pi */
		BNKGET("GIS:PHA", sizeof(pha), size, &pha);
		BNKGET("GIS:PI", sizeof(pi), size, &pi);

/* get rt and rti */
		BNKGET("GIS:RISE_TIME", sizeof(rt), size, &rt);
		BNKGET("GIS:RTI", sizeof(rti), size, &rti);

/* get rawx and rawy */
		BNKGET("GIS:RAWX", sizeof(rawx), size, &rawx);
		BNKGET("GIS:RAWY", sizeof(rawy), size, &rawy);

/* get detx and dety */
		BNKGET("GIS:DETX", sizeof(detx), size, &detx);
		BNKGET("GIS:DETY", sizeof(dety), size, &dety);

/* get skyx and skyy */
		BNKGET("GIS:X", sizeof(skyx), size, &skyx);
		BNKGET("GIS:Y", sizeof(skyy), size, &skyy);

/* make spectrum & image */
		offset = sensor * 100;
		HF1(offset+00, (float)pha, 1.0);
		HF1(offset+10, (float)pi, 1.0);
		HF1(offset+20, (float)rt, 1.0);
		HF1(offset+30, (float)rti, 1.0);
		HF2(offset+40, (float)rawx, (float)rawy, 1.0);
		HF2(offset+50, (float)detx, (float)dety, 1.0);
		HF2(offset+60, (float)skyx, (float)skyy, 1.0);

	}

/* set GISSPEC:OK for ASCA_ANL_OK */
	EVSSET("GISSPEC:OK");
	*status = ASCA_ANL_OK;
}

void
gisspec_endrun(int *status)
{
	*status = ASCA_ANL_OK;
}

void
gisspec_exit(int *status)
{
	*status = ASCA_ANL_OK;
}

/*-------------------
   C wrapper for gisspec
-------------------*/

FCALLSCSUB1(gisspec_init,GISSPEC_INIT,gisspec_init,PINT)
FCALLSCSUB1(gisspec_com,GISSPEC_COM,gisspec_com,PINT)
FCALLSCSUB1(gisspec_his,GISSPEC_HIS,gisspec_his,PINT)
FCALLSCSUB1(gisspec_bgnrun,GISSPEC_BGNRUN,gisspec_bgnrun,PINT)
FCALLSCSUB3(gisspec_ana,GISSPEC_ANA,gisspec_ana,INT,INT,PINT)
FCALLSCSUB1(gisspec_endrun,GISSPEC_ENDRUN,gisspec_endrun,PINT)
FCALLSCSUB1(gisspec_exit,GISSPEC_EXIT,gisspec_exit,PINT)
