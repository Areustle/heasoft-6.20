/*
 SimASTE_XISRMFsim.c
   SimaSTE module : XIS simulation with RMF

  1998-02-25	version 1.00	Y.ISHISAKI

  1998-04-14	version 1.01	Y.ISHISAKI
	bug fix in SimASTE_XISRMFsim_com (com -> key)
	SimASTE_DRNDTS -> aste_drndts

  1998-09-10	version 1.10	Y.ISHISAKI
	work with ftools parameter interface
	add Be filter selection for FW

  1999-01-22	version 1.20	Y.ISHISAKI
	add event flag (HiRes, MidRes, LowRes, Secondary)
	put SimASTE:SKYX/Y/ROLL
	bug fix in show_parameter() for com.pxl_sel

  1999-02-09	version 1.21	Y.ISHISAKI
	check if (total == 0) in pixel statistics
	print all statistics

  2003-09-13	version 1.30	Y.ISHISAKI
	use latest aste_coord in astetool-1.24

  2003-11-07	version 1.40	Y.ISHISAKI
	BnkPut PI:TLMIN/TLMAX, PHA:TLMIN/TLMAX

  2003-11-10	version 1.50	Y.ISHISAKI
	read fw_file & gv_file

  2004-07-06	version 1.60	Y.ISHISAKI
	quick fix for CAL pixel (No.3) & non-connected pixel (No.2)
	count statistics on H, H+Mp, Ms, Lp+Ls
	print statistics with 2D-array map & pixel order

  2005-12-09	version 1.00 [SimASTE_XISRMFsim]	Y.ISHISAKI
	modified for SimASTE_XISRMFsim

  2006-04-09	version 1.1		Y.ISHISAKI
	change version number only

  2006-05-25	version 1.2		Y.ISHISAKI
	add "xis_contamifile" parameter
	add BNK "SimASTE:XIS:RMF_FILE:PTR"
	add BNK "SimASTE:XIS:CONTAMI_FILE:PTR"
	add BNK "SimASTE:XIS:CONTAMI_TRANS"
	add EVS "SimASTE:XIS:OUT_OF_CCD"

  2006-07-24 version 2.0	Y.ISHISAKI
	support for CALDB
	BnkGet SimASTE:TELESCOP:PTR, SimASTE:INSTRUME:PTR for CALDB

  2006-08-02 version 2.1	Y.ISHISAKI
	add aberration parameter

  2006-08-05 version 2.2	Y.ISHISAKI
	add write_history(), BnkGet/Put SimASTE:WRITE_HISTORY:FUNC in _init()

  2006-08-22 version 2.3	Y.ISHISAKI
	check HrndmRMF_init() error status in _init()

  2006-10-17 version 2.4	Y.ISHISAKI
	add NULL argument for xis_contami() version 1.2

  2007-05-28 version 2.5	Y.ISHISAKI
	add 'enable_pixq', 'hotpixfiles', 'badcolumfile', 'calmaskfile' parameters
	accept rmffile='CALDB'
	BnkDef/Put "SimASTE:XIS:PIXQ_INFO:PTR", "SimASTE:XIS:PIXQ_STAT:PTR"
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <math.h>
#include "fitsio.h"
#include "anl.h"
#include "com.h"
#include "cli.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_rand.h"
#include "aste_time.h"
#include "aste_caldb.h"
#include "xis_contami.h"
#include "xisTelemFormat.h"
#include "xisSciUtil.h"
#include "xisPixelQuality.h"
#include "SimASTE.h"

#define TRANSMISSION_INDEX_SEARCH	/* fast index search for transmission */

static char pname[] = "SimASTE_XISRMFsim";
char SimASTE_XISRMFsim_version[] = "version 2.5";

/* XISRMFsim parameters */
static struct {
	char *telescop;
	char *instrume;
	int mode;
	int xis_efficiency;
	int xis_chip_select;
	char *rmffile, o_rmffile[PIL_LINESIZE];
	struct HrndmRMF *hmf;
	char *contamifile, o_contamifile[PIL_LINESIZE];
	XIS_CONTAMI *xcp;
	double xis_ccd_expo;
	int aberration;
	int mjdrefi;
	double mjdreff;
	int (*prev_write_history)(fitsfile *);
	char phafile[PIL_LINESIZE];
	int enable_pixq;
	char hotpixfiles[PIL_LINESIZE];
	char *badcolumfile, o_badcolumfile[PIL_LINESIZE];
	char *calmaskfile, o_calmaskfile[PIL_LINESIZE];
	int dummy;
} com = {
	NULL,
	NULL,
	-1,				/* mode	(DEFAULT) */
	1,				/* xis_efficiency (YES) */
	1,				/* xis_chip_select */
	NULL, "CALDB",	/* rmffile, o_rmffile */
	NULL,			/* hmf */
	NULL, "CALDB",	/* contamifile, o_contamifile */
	NULL,			/* xcp */
	8.0,			/* xis_ccd_expo */
	ANL_YES,		/* aberration */
	0,				/* mjdrefi */
	0.0,			/* mjdreff */
	NULL,			/* prev_write_history */
	"none",			/* phafile */
	ANL_YES,		/* enable_pixq */
	"none",			/* hotpixfiles */
	NULL, "CALDB",	/* badcolumfile, o_badcolumfile */
	NULL, "CALDB",	/* calmaskfile, o_calmaskfile */
	0				/* dummy for previous comma */
};

static PIXQ_INFO pixq;
static PIXQ_STAT statistics;

static int
write_history(fitsfile *fp)
{
	int i, istat;
	char history[PIL_LINESIZE + FLEN_VALUE];
	HOTPIXFILE_INFO *hp;
	PIXQ_INFO *p = &pixq;

	if ( NULL != com.prev_write_history &&
		 write_history != com.prev_write_history ) {
		istat = (*com.prev_write_history)(fp);
		if ( istat ) {
			return istat;
		}
	}

	istat = SimASTE_write_history_pname(fp, pname);

	if ( com.aberration ) {
		sprintf(history, "\
  aberration=yes  mjdrefi=%d  mjdreff=%.17f", com.mjdrefi, com.mjdreff);
	} else {
		sprintf(history, "\
  aberration=no");
	}
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  xis_rmffile='%s'%s", com.rmffile,
		(com.rmffile == com.o_rmffile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  xis_contamifile='%s'%s", com.contamifile,
		(com.contamifile == com.o_contamifile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  xis_efficiency=%s  xis_chip_select=%s  xis_ccd_expo=%.6f s",
		com.xis_efficiency  ? "yes" : "no",
		com.xis_chip_select ? "yes" : "no",
		com.xis_ccd_expo);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  phafile='%s'", com.phafile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  enable_pixq=%s", com.enable_pixq ? "yes" : "no");
	fits_write_history(fp, history, &istat);
	if ( com.enable_pixq ) {
		sprintf(history, "\
  hotpixfiles='%s'", com.hotpixfiles);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < p->num_hotpixfile; i++) {
			hp = &p->hotpixfile_list[i];
			sprintf(history, "\
    %d: '%s'", i+1, hp->filename);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
       num_hotpix=%ld  num_added=%ld  num_dupli=%ld",
				hp->num_hotpix, hp->num_added, hp->num_dupli);
			fits_write_history(fp, history, &istat);
		}
		sprintf(history, "\
  badcolumfile='%s'%s", com.badcolumfile,
			(com.badcolumfile == com.o_badcolumfile) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  calmaskfile='%s'%s", com.calmaskfile,
			(com.calmaskfile == com.o_calmaskfile) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
	}

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
show_yes_no(char *msg, int sw)
{
	MSG("%4s%-20s%s", "", msg, sw ? "YES" : "NO");
}

static void
show_parameter(char *title)
{
	char *p;

	MSG("");
	MSG(title, pname);
	MSG("");

	switch ( com.mode ) {
	case SimASTE_Mode_Discard: p = "Discard_Mode"; break;
	case SimASTE_Mode_Weight: p = "Weighting_Mode"; break;
	default: p = "Default";
	}
	MSG("%4s%-20s'%s'%s", "", "XIS_RMFFILE", com.rmffile,
		(com.rmffile == com.o_rmffile) ? "" : " (CALDB)");
	MSG("%4s%-20s'%s'%s", "", "XIS_CONTAMIFILE", com.contamifile,
		(com.contamifile == com.o_contamifile) ? "" : " (CALDB)");
	MSG("%4s%-20s%s", "", "SIMULATION_MODE", p);
	show_yes_no("ABERRATION", com.aberration);
	show_yes_no("XIS_EFFICIENCY", com.xis_efficiency);
	show_yes_no("XIS_CHIP_SELECT", com.xis_chip_select);
	MSG("%4s%-20s%.3f", "", "XIS_CCD_EXPO", com.xis_ccd_expo);
	MSG("%4s%-20s'%s'", "", "PHAFILE", com.phafile);
	show_yes_no("ENABLE_PIXQ", com.enable_pixq);
	if ( com.enable_pixq ) {
		MSG("%4s%-20s'%s'", "", "HOTPIXFILES", com.hotpixfiles);
		MSG("%4s%-20s'%s'%s", "", "BADCOLUMFILE", com.badcolumfile,
			(com.badcolumfile == com.o_badcolumfile) ? "" : " (CALDB)");
		MSG("%4s%-20s'%s'%s", "", "CALMASKFILE", com.calmaskfile,
			(com.calmaskfile == com.o_calmaskfile) ? "" : " (CALDB)");
	}
	MSG("");
}

void
SimASTE_XISRMFsim_startup(int *status)
{
	com.rmffile = com.o_rmffile;
	com.contamifile = com.o_contamifile;
	com.badcolumfile = com.o_badcolumfile;
	com.calmaskfile = com.o_calmaskfile;
	com.mjdrefi = aste_mjdrefi();
	com.mjdreff = aste_mjdreff();

	*status = ANL_OK;
}

void
SimASTE_XISRMFsim_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"RMF_FILE",
		"CONTAMI_FILE",
		"SIMULATION_MODE",
		"ABERRATION",
		"EFFICIENCY",
		"CHIP_SELECT",
		"CCD_EXPO",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"RMF utilized for XIS simulation",
		"XIS OBF contamination file",
		"Set Simulation Mode (Default/Discard/Weight)",
		"Correct for aberration or not",
		"Multiply XIS effciency or not",
		"Discard events which is fallen outside of CCD chips",
		"Exposure time for CCD",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
	char *k, *key;
	int ans[2];

	if ( *status ) {	/* ftools */
		if (
PILGetBool (k="aberration", &com.aberration) ||
PILGetFname(k="xis_rmffile", com.rmffile) ||
PILGetFname(k="xis_contamifile", com.contamifile) ||
PILGetBool (k="xis_efficiency", &com.xis_efficiency) ||
PILGetBool (k="xis_chip_select", &com.xis_chip_select) ||
PILGetReal (k="xis_ccd_expo", &com.xis_ccd_expo) ||
PILGetFname(k="phafile", com.phafile) ||
PILGetBool (k="enable_pixq", &com.enable_pixq) ||
PILGetFname(k="hotpixfiles", com.hotpixfiles) ||
PILGetFname(k="badcolumfile", com.badcolumfile) ||
PILGetFname(k="calmaskfile", com.calmaskfile) ||
			 0 ) {
			anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
			*status = ANL_QUIT;
			return;
		}
		*status = ANL_OK;
		return;
	}

	for (;;) {
		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("SHOW", key) ) {
			show_parameter("%s:  *** show parameter ***");
		} else if ( 0 == strcmp("RMF_FILE", key) ) {
			CLtxtrd(key, com.rmffile, sizeof(com.rmffile));
		} else if ( 0 == strcmp("CONTAMI_FILE", key) ) {
			CLtxtrd(key, com.contamifile, sizeof(com.contamifile));
		} else if ( 0 == strcmp("SIMULATION_MODE", key) ) {
			static char *keytbl[] = {
				"Discard_Mode", "Weighting_Mode", "Default"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			static char key[32] = "Default";
			int it;

			CLkeyrd(-1, "SimASTE_XISRMFsim Simulation Mode",
					key, keytbl, nkey, &it, sizeof(key));
			if ( 0 == strcmp("Discard_Mode", key) ) {
				com.mode = SimASTE_Mode_Discard;
			} else if ( 0 == strcmp("Weighting_Mode", key) ) {
				com.mode = SimASTE_Mode_Weight;
			} else if ( 0 == strcmp("Default", key) ) {
				com.mode = -1;
			}
		} else if ( 0 == strcmp("ABERRATION", key) ) {
			CLlogrd("\
Correct for aberration", &com.aberration);
		} else if ( 0 == strcmp("EFFICIENCY", key) ) {
			CLlogrd("\
Multiply XIS efficiency", &com.xis_efficiency);
		} else if ( 0 == strcmp("CHIP_SELECT", key) ) {
			CLlogrd("\
Discard events fallen outside of CCD", &com.xis_chip_select);
		} else if ( 0 == strcmp("CCD_EXPO", key) ) {
			CLfdprd("\
Exposure time for CCD", &com.xis_ccd_expo);
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
SimASTE_XISRMFsim_init(int *status)
{
	static int (*func)(fitsfile *fp) = write_history;

	fitsfile *ifp;
	char *filename, *k;
	int ival, used, istat;
	double trans;
	PIXQ_INFO *pixq_info_ptr = NULL;
	PIXQ_STAT *pixq_stat_ptr = NULL;

	EvsDef("SimASTE_XISRMFsim:BEGIN");
	EvsDef("SimASTE_XISRMFsim:ENTRY");
	EvsDef("SimASTE_XISRMFsim:OK");
	EvsDef("SimASTE:XIS:OUT_OF_CCD");

	BnkDef("SimASTE:XIS:STATUS", sizeof(int));
	BnkDef("SimASTE:XIS:GRADE", sizeof(int));
	BnkDef("SimASTE:XIS:SEGMENT", sizeof(int));
	BnkDef("SimASTE:XIS:RAWX", sizeof(int));
	BnkDef("SimASTE:XIS:RAWY", sizeof(int));
	BnkDef("SimASTE:XIS:ACTX", sizeof(int));
	BnkDef("SimASTE:XIS:ACTY", sizeof(int));
	BnkDef("SimASTE:XIS:RMF_FILE:PTR", sizeof(char *));
	BnkDef("SimASTE:XIS:CONTAMI_FILE:PTR", sizeof(char *));
	BnkDef("SimASTE:XIS:CONTAMI_TRANS", sizeof(double));
	BnkDef("SimASTE:XIS:PIXQ_INFO:PTR", sizeof(pixq_info_ptr));
	BnkDef("SimASTE:XIS:PIXQ_STAT:PTR", sizeof(pixq_stat_ptr));

	com.prev_write_history = NULL;
	BnkGet("SimASTE:WRITE_HISTORY:FUNC", sizeof(com.prev_write_history),
		&used, &com.prev_write_history);
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

	BnkGet("SimASTE:TELESCOP:PTR", sizeof(com.telescop), &used, &com.telescop);
	BnkGet("SimASTE:INSTRUME:PTR", sizeof(com.instrume), &used, &com.instrume);

#define get_caldb_file(CODE,O_FILE)	aste_caldb_find(com.instrume, CODE, O_FILE)
	com.contamifile = get_caldb_file("CONTAMI_TRANS", com.o_contamifile);
	if ( NULL == com.contamifile ) goto quit;
	com.rmffile = get_caldb_file("SPECRESP MATRIX", com.o_rmffile);
	if ( NULL == com.rmffile ) goto quit;
	if ( com.enable_pixq ) {
		com.badcolumfile = get_caldb_file("BADPIX", com.o_badcolumfile);
		if ( NULL == com.badcolumfile ) goto quit;
		com.calmaskfile = get_caldb_file("CALMASK", com.o_calmaskfile);
		if ( NULL == com.calmaskfile ) goto quit;
	}
#undef get_caldb_file

	show_parameter("%s:  *** show parameter ***");

	if ( com.mode < 0 ) {
		int size;
		BnkfGetM("SimASTE:MODE", sizeof(com.mode), &size, &com.mode);
	}

	anl_msg_info("\
%s: reading RMF file ... ", pname);
	com.hmf = HrndmRMF_init(com.rmffile);
	if ( NULL == com.hmf ) {
		anl_msg_error("\
%s: HrndmRMF_init() failed\n", pname);
		*status = ANL_QUIT;
		return;
	}
	anl_msg_info("\
finished\n");

	if ( 0 == CLstricmp("NONE", com.contamifile) ) {
		com.xcp = NULL;
	} else {
		istat = xis_contami_init(&com.xcp, com.contamifile);
		if ( istat ) {
			*status = ANL_QUIT;
			return;
		}
	}

	ival = com.hmf->ebounds.channel[0];
	BnkPut("SimASTE:PHA:TLMIN", sizeof(ival), &ival);
	BnkPut("SimASTE:PI:TLMIN", sizeof(ival), &ival);
	ival = com.hmf->ebounds.channel[0] + com.hmf->detchans - 1;
	BnkPut("SimASTE:PHA:TLMAX", sizeof(ival), &ival);
	BnkPut("SimASTE:PI:TLMAX", sizeof(ival), &ival);

	filename = com.rmffile;
	BnkPut("SimASTE:XIS:RMF_FILE:PTR", sizeof(filename), &filename);
	filename = com.contamifile;
	BnkPut("SimASTE:XIS:CONTAMI_FILE:PTR", sizeof(filename), &filename);

	if ( NULL == com.xcp ) {
		trans = 1.0;
		BnkPut("SimASTE:XIS:CONTAMI_TRANS", sizeof(trans), &trans);
	}

/* initialize pixel quality function */
	if ( com.enable_pixq ) {
		pixq.hotpix_filenames = com.hotpixfiles;
		pixq.badcol_filename  = com.badcolumfile;
		pixq.calmask_filename = com.calmaskfile;
		if ( 0 == CLstricmp("none", com.phafile) ) {
			istat = xisSciReadKeys(NULL, &pixq.sci);
			if ( istat ) goto quit;
		} else {
			fits_open_file(&ifp, k=com.phafile, READONLY, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
				goto quit;
			}
			istat = xisSciReadKeys(ifp, &pixq.sci);
			if ( istat ) {
				int istat2 = 0;
				fits_close_file(ifp, &istat2);	/* ignore error */
				goto quit;
			}
			fits_close_file(ifp, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, k, istat);
				goto quit;
			}
		}
		istat = xisPixelQualityInit(com.instrume, &pixq);
		if ( istat ) goto quit;
		xisPixqStatInit(&statistics);

		pixq_info_ptr = &pixq;
		pixq_stat_ptr = &statistics;
	}

	BnkPut("SimASTE:XIS:PIXQ_INFO:PTR", sizeof(pixq_info_ptr), &pixq_info_ptr);
	BnkPut("SimASTE:XIS:PIXQ_STAT:PTR", sizeof(pixq_stat_ptr), &pixq_stat_ptr);

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
SimASTE_XISRMFsim_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISRMFsim_bgnrun(int *status)
{
	EvsSet("SimASTE_XISRMFsim:BEGIN");
	*status = ANL_OK;
}

void
SimASTE_XISRMFsim_ana(int nevent, int eventid, int *status)
{
	static int grade = 0;					/* grade is always 0 */
	static unsigned int pixel_quality = 0;	/* always 0, if enable_pixq=no */

	int size;
	double energy, photon_time, mod_8sec, detx, dety, skyx, skyy, ra, de;
	double pi, eff, weight, contami_trans;
	int segid, rawx_ch, rawy_ch, actx_ch, acty_ch;
	int pi_ch, detx_ch, dety_ch, skyx_ch, skyy_ch;
	AtEulerAng euler;
	SKYREF skyref;
	TELDEF *teldef;

	EvsfSetM("SimASTE_XISRMFsim:ENTRY");
	*status = ANL_OK;

	BnkfGetM("SimASTE:TELDEF", sizeof(teldef), &size, &teldef);

/* get photon information */
	BnkfGetM("SimASTE:PHOTON_ENERGY", sizeof(energy), &size, &energy);
	BnkfGetM("SimASTE:PHOTON_TIME", sizeof(photon_time), &size, &photon_time);
	BnkfGetM("SimASTE:XISX", sizeof(detx), &size, &detx);
	BnkfGetM("SimASTE:XISY", sizeof(dety), &size, &dety);
	BnkfGetM("SimASTE:EULER", sizeof(euler), &size, &euler);
	BnkfGetM("SimASTE:SKYREF", sizeof(skyref), &size, &skyref);

/* set DETX/Y, X/Y */
	detx_ch = (int)(detx + 10000.5) - 10000;
	dety_ch = (int)(dety + 10000.5) - 10000;

	xis_det2act(teldef, detx_ch, dety_ch, &actx_ch, &acty_ch);

	if ( actx_ch < 0 || 1024 <= actx_ch ||
		 acty_ch < 0 || 1024 <= acty_ch ) {
		EvsfSetM("SimASTE:XIS:OUT_OF_CCD");
		if ( com.xis_chip_select ) {
			*status = ANL_SKIP;
			return;
		}
		segid = -1;
		rawx_ch = -999;
		rawy_ch = -999;
	} else {
		xis_act2raw(teldef, actx_ch, acty_ch, 0, 0, &segid,&rawx_ch,&rawy_ch);
	}

	if ( com.aberration ) {
		aste_det2ecs(teldef, &euler, detx, dety, &ra, &de);
		aste_cor_aberration(photon_time, com.mjdrefi, com.mjdreff, &ra, &de);
		aste_ecs2sky(teldef, &skyref, ra, de, &skyx, &skyy);
	} else {
		aste_det2sky(teldef, &euler, &skyref, detx, dety, &skyx, &skyy);
	}
	skyx_ch = (int)(skyx + 10000.5) - 10000;
	skyy_ch = (int)(skyy + 10000.5) - 10000;

/* randomize PI */
	HrndmRMF(energy, aste_drndts(), com.hmf, &pi, &eff);
	pi_ch = (int)pi + com.hmf->ebounds.channel[0];

/* consider XIS OBF contamination */
	if ( NULL != com.xcp ) {
		contami_trans = xis_contami(com.xcp,
			energy, photon_time, detx, dety, NULL, NULL);
		if ( contami_trans < 0.0 ) {
			anl_msg_warning("\
%s: contami_trans=%f, something is wrong\n", pname, contami_trans);
			contami_trans = 1.0;
		} else {
			eff *= contami_trans;
		}
		BnkfPutM("SimASTE:XIS:CONTAMI_TRANS", sizeof(double), &contami_trans);
	}

/* consider XIS efficiency */
	if ( com.xis_efficiency ) {
		if ( SimASTE_Mode_Discard == com.mode ) {
			if ( eff < aste_drndts() ) {
				*status = ANL_SKIP;
				return;
			}
		} else {
			BnkfGetM("SimASTE:WEIGHT", sizeof(weight), &size, &weight);
			weight *= eff;
			BnkfPutM("SimASTE:WEIGHT", sizeof(weight), &weight);
		}
	}

	if ( com.enable_pixq ) {
		if ( actx_ch < 0 || 1024 <= actx_ch ||
			 acty_ch < pixq.sci.win_st ||
			 pixq.sci.win_st + pixq.sci.win_siz <= acty_ch ) {
			segid = -1;
			rawx_ch = -999;
			rawy_ch = -999;
			pixel_quality = (1 << BIT_OUTSIDE_AREADISCRI);
		} else if ( pixq.sci.winopt ) {
			mod_8sec = fmod(photon_time, 8.0);
			rawy_ch = (int)(1024/8 * mod_8sec);	/* 0 <= rawy < 1024 */
			rawy_ch = (rawy_ch / pixq.sci.win_siz) * pixq.sci.win_siz;
			rawy_ch = rawy_ch + acty_ch - pixq.sci.win_st;
			pixel_quality = xisPixelQuality(&pixq,
				photon_time, actx_ch, acty_ch, rawy_ch);
		} else {
			pixel_quality = xisPixelQuality(&pixq,
				photon_time, actx_ch, acty_ch, rawy_ch);
		}
		xisPixqStatAdd(&statistics, pixel_quality);
	}

/* put event information */
	BnkfPutM("SimASTE:TIME", sizeof(photon_time), &photon_time);
	BnkfPutM("SimASTE:PI", sizeof(pi_ch), &pi_ch);
	BnkfPutM("SimASTE:PHA", sizeof(pi_ch), &pi_ch);
	BnkfPutM("SimASTE:DETX", sizeof(detx_ch), &detx_ch);
	BnkfPutM("SimASTE:DETY", sizeof(dety_ch), &dety_ch);
	BnkfPutM("SimASTE:SKYX", sizeof(skyx_ch), &skyx_ch);
	BnkfPutM("SimASTE:SKYY", sizeof(skyy_ch), &skyy_ch);

	BnkfPutM("SimASTE:XIS:STATUS", sizeof(pixel_quality), &pixel_quality);
	BnkfPutM("SimASTE:XIS:GRADE", sizeof(grade), &grade);
	BnkfPutM("SimASTE:XIS:SEGMENT", sizeof(segid), &segid);
	BnkfPutM("SimASTE:XIS:RAWX", sizeof(rawx_ch), &rawx_ch);
	BnkfPutM("SimASTE:XIS:RAWY", sizeof(rawy_ch), &rawy_ch);
	BnkfPutM("SimASTE:XIS:ACTX", sizeof(actx_ch), &actx_ch);
	BnkfPutM("SimASTE:XIS:ACTY", sizeof(acty_ch), &acty_ch);

	EvsfSetM("SimASTE_XISRMFsim:OK");
}

void
SimASTE_XISRMFsim_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISRMFsim_exit(int *status)
{
	if ( com.enable_pixq ) {
		xisPixqStatWrite(&statistics, stdout);
	}

	if ( NULL != com.xcp ) {
		xis_contami_free(com.xcp);
		com.xcp = NULL;
	}

	*status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
