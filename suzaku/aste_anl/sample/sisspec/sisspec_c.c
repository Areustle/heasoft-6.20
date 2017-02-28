/*******************************************************************
 sisspec.c
     Example Module for spectral analysis of SIS
     H.Kubo
     M.Hirayama
     93/12/10 M.Hirayama change var. name (recid -> eventid)
     93/12/23 M.Hirayama update for asca_anl_v0.4
     94/03/28 M.Hirayama update for asca_anl_v0.5
     94/04/10 Y.Ishisaki GIS: -> ANL:, use ANL:SENSOR
     94/07/01 M.Hirayama change coverage of histograms
                   spectra: 0.0-1280.0 (2048bin) -> -0.5-4095.5 (512bin)
                   images : 0.0-1280.0 (256bin) -> 0.5-1280.5 (256bin)
   2005/11/29 Y.Ishisaki check sensor id, before BNKGET
********************************************************************/
#include <stdio.h>
#include <cfortran.h>
#include <hbook.h>
#include <cli.h>
#include <com.h>
#include <bnk.h>
#include <evs.h>
#include <asca_anl.h>
#include <anl_misc.h>

void SISspec_init(int *);
void SISspec_com(int *);
void SISspec_his(int *);
void SISspec_bgnrun(int *);
void SISspec_ana(int, int, int *);
void SISspec_endrun(int *);
void SISspec_exit(int *);

void
SISspec_init(int *status)
{
	EVSDEF("SISSPEC:BEGIN");
	EVSDEF("SISSPEC:ENTRY");
	EVSDEF("SISSPEC:OK");
	ANL_PUT_VERSION("SISSPEC", "version 3.1");

	*status = ASCA_ANL_OK;
}

void
SISspec_com(int *status)
{
	*status = ASCA_ANL_OK;
}

void
SISspec_his(int *status)
{
	int sensor, chip, grade, id;
	char title[30];

	for (sensor=0;sensor<2;sensor++) {
		for (chip=0;chip<4;chip++) {
			for (grade=0;grade<8;grade++) {
				id = 4000 + sensor*100 + chip*10 + grade;
				sprintf(title,"SIS%1d CHIP%1d GRADE%1d PHA",sensor,chip,grade);
				HBOOK1(id, title, 512, -0.5, 4095.5, 0.0);
			}
		}
	}
	HBOOK2(4500,"SIS0 DETX vs DETY",256,0.5,1280.5,256,0.5,1280.5,0.0);
	HBOOK2(4600,"SIS1 DETX vs DETY",256,0.5,1280.5,256,0.5,1280.5,0.0);

	*status = ASCA_ANL_OK;
}

void
SISspec_bgnrun(int *status)
{
	EVSSET("SISSPEC:BEGIN");

	*status = ASCA_ANL_OK;
}

void
SISspec_ana(int nevent, int eventid, int *status)
{
	int size;
	int sensor, detx, dety;
	int ccdid, grade, pha;

	EVSSET("SISSPEC:ENTRY");

/* check if bright mode or not */
	if (EVS("SIS:BRIGHT mode")) {
		*status = ASCA_ANL_OK;
	} else {
		puts("DATAMODE is not BRIGHT");
		*status = ASCA_ANL_QUIT;
		return;
	}

/* get sensor id */
	BNKGET("ANL:SENSOR",sizeof(sensor),size,&sensor);

/* check sensor id */
	if ( 0 == sensor || 1 == sensor ) {

/* get pha */
		BNKGET("SIS:PHA", sizeof(pha), size, &pha);
		BNKGET("SIS:CCDID", sizeof(ccdid), size, &ccdid);
		BNKGET("SIS:GRADE", sizeof(grade), size, &grade);

/* get detx and dety*/
		BNKGET("SIS:DETX",sizeof(detx),size,&detx);
		BNKGET("SIS:DETY",sizeof(dety),size,&dety);

/* make spectrum & image */
		HF1(4000+sensor*100+ccdid*10+grade, (float)pha,1.0);
		HF2(4500+sensor*100,(float)detx,(float)dety,1.0);

	}

	EVSSET("SISSPEC:OK");
	*status = ASCA_ANL_OK;
}

void
SISspec_endrun(int *status)
{
	*status = ASCA_ANL_OK;
}

void
SISspec_exit(int *status)
{
	*status = ASCA_ANL_OK;
}

/*-------------------
   C wrapper for SISspec
-------------------*/

FCALLSCSUB1(SISspec_init,SISSPEC_INIT,sisspec_init,PINT)
FCALLSCSUB1(SISspec_com,SISSPEC_COM,sisspec_com,PINT)
FCALLSCSUB1(SISspec_his,SISSPEC_HIS,sisspec_his,PINT)
FCALLSCSUB1(SISspec_bgnrun,SISSPEC_BGNRUN,sisspec_bgnrun,PINT)
FCALLSCSUB3(SISspec_ana,SISSPEC_ANA,sisspec_ana,INT,INT,PINT)
FCALLSCSUB1(SISspec_endrun,SISSPEC_ENDRUN,sisspec_endrun,PINT)
FCALLSCSUB1(SISspec_exit,SISSPEC_EXIT,sisspec_exit,PINT)
