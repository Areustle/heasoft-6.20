/*

 SimASTE_Root.c
   SimASTE module : root module

  1998-04-14	version 1.01	Y.ISHISAKI
	SimASTE_DRNDTS -> aste_drndts

  1998-09-10	version 1.10	Y.ISHISAKI
	work with ftools parameter interface
	add teldef, rand_seed, rand_skip

  1999-01-22	version 1.20	Y.ISHISAKI
	BNKdef SimASTE:TSTART/TSTOP/EXPOSURE/XRSFILTER

  2003-09-13	version 1.30	Y.ISHISAKI
	use latest aste_coord in astetool-1.24

  2003-11-07	version 1.40	Y.ISHISAKI
	add TLMIN, TLMAX for BNK

  2005-12-20	version 1.50	Y.ISHISAKI
	add new parameter 'leapfile'
	add new BNK, "SimASTE:LEAPSEC_FILE"
	atMissionTimeInit() in _init()

  2005-12-24	version 1.51	Y.ISHISAKI
	do not call atMissionTimeInit() when leapfile="none"

  2006-04-09 version 1.6	Y.ISHISAKI
	change version number only

  2006-04-24 version 1.7	Y.ISHISAKI
	change parameter names: teldef_file->teldef, ea_{phi,theta,psi}->ea{1,2,3}

  2006-07-24 version 2.0	Y.ISHISAKI
	add telescop, instrume parameters for CALDB
	support for CALDB
	new BNK, SimASTE:TELESCOP:PTR, SimASTE:INSTRUME:PTR, SimASTE:LEAPFILE:PTR

  2006-07-25 version 2.1	Y.ISHISAKI
	new BNK, SimASTE:GTI_DATA:PTR
	do not read ea1/2/3 from parameter file
	do not BnkPut SimASTE:EULER, SimASTE:SKYREF, which is moved to PhotonGen

  2006-08-10 version 2.2	Y.ISHISAKI
	add write_history(), BnkPut SimASTE:WRITE_HISTORY:FUNC in _init()
	new BNK, SimASTE:WRITE_HISTORY:PTR, SimASTE:GEOMAREA
	new BNK, SimASTE:N_PHOTON, SimASTE:N_DETECT, SimASTE:N_WEISUM
	new BNK, SimASTE:SHIELD_TRANSMIS, SimASTE:SHIELD_TRANSMIS:FUNC
	remove unused readref() function
	remove BNK SimASTE:XRSFILTER, add SimASTE:FILTER:PTR instead

  2006-10-17 version 2.3	Y.ISHISAKI
	use aste_caldb_find_leapfile() for leapfile

  2007-05-28 version 2.4	Y.ISHISAKI
	change unit "sec" -> "s" in SimASTE_write_gti()
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include "fitsio.h"
#include "cli.h"
#include "com.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_rand.h"
#include "aste_time.h"
#include "aste_caldb.h"
#include "SimASTE.h"

static char pname[] = "SimASTE_Root";
char SimASTE_Root_version[] = "version 2.5";

/* SimASCAroot parameters */
static struct {
	char telescop[PIL_LINESIZE];
	char instrume[PIL_LINESIZE];
	struct {
		int seed;
		double skip;
	} rand;
	int mode;
	int sensor;
	char *teldef_file, o_teldef_file[PIL_LINESIZE];
	char *leapfile, o_leapfile[PIL_LINESIZE];
	TELDEF *teldef;
	char filter[16];
} com = {
	"SUZAKU",				/* telescop */
	"XRS",					/* instrume */
	{ 7, 0.0 },				/* rand seed & skip */
	SimASTE_Mode_Discard,	/* mode */
	-1,						/* sensor */
	NULL, "none",			/* teldef_file, o_teldef_file */
	NULL, "none",			/* leapfile, o_leapfile */
	NULL,					/* teldef */
	"none",					/* filter */
};

static int
write_history(fitsfile *fp)
{
	int istat;
	char history[PIL_LINESIZE + FLEN_VALUE];

	istat = SimASTE_write_history_pname(fp, pname);

	sprintf(history, "\
  telescop='%s'  instrume='%s'", com.telescop, com.instrume);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  rand_seed=%d  rand_skip=%.0f  simulation_mode=%d:%s",
		com.rand.seed, com.rand.skip, com.mode,
		( SimASTE_Mode_Discard == com.mode ) ? "DISCARD" : "WEIGHTING");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  teldef='%s'%s", com.teldef_file,
		(com.teldef_file == com.o_teldef_file) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  leapfile='%s'%s", com.leapfile,
		(com.leapfile == com.o_leapfile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);

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

void
SimASTE_Root_startup(int *status)
{
	com.leapfile = com.o_leapfile;
	com.teldef_file = com.o_teldef_file;
}

static void
show_parameter(char *title)
{
	char *p;

	MSG("");
	MSG(title, pname);
	MSG("");

/*	MSG("%4s%-20s'%s'", "", "Parameter_File", SimASCA_pfile_name(NULL));*/
	MSG("%4s%-20s'%s'", "", "TELESCOP", com.telescop);
	MSG("%4s%-20s'%s'", "", "INSTRUME", com.instrume);
	MSG("%4s%-20s%d", "", "RAND_SEED", com.rand.seed);
	MSG("%4s%-20s%.0lf", "", "RAND_SKIP", com.rand.skip);
	if ( SimASTE_Mode_Discard == com.mode ) {
		p = "0:DISCARD";
	} else {
		p = "1:WEIGHTING";
	}
	MSG("%4s%-20s%s", "", "SIMULATION_MODE", p);
	MSG("%4s%-20s'%s'%s", "", "TELDEF", com.teldef_file,
		(com.teldef_file == com.o_teldef_file) ? "" : " (CALDB)");
	MSG("%4s%-20s'%s'%s", "", "LEAPFILE", com.leapfile,
		(com.leapfile == com.o_leapfile) ? "" : " (CALDB)");
}

void
SimASTE_Root_com(int *status)
{
#define NTBL	11
	static char *keytbl[NTBL] = {
		"SHOW",
		"TELESCOP",
		"INSTRUME",
		"RAND_SEED",
		"RAND_SKIP",
		"SIMULATION_MODE",
		"EA",
		"SKYREF",
		"TELDEF",
		"LEAPFILE",
		"EXIT"
	};
	static char *help[NTBL] = {
		"Show current settings",
		"Set TELESCOP",
		"Set INSTRUME",
		"Set RAND_SEED",
		"Set RAND_SKIP",
		"Set SIMULATION_MODE (discard/weight)",
		"Set default EA",
		"Set default SKYREF alpha/delta",
		"set teldef file",
		"set leapsec file",
		"Exit from this menu"
	};
	static int nkey = NTBL;
#undef NTBL

	char *k;

	if ( *status ) {	/* ftools */
		if ( PILGetString(k="telescop", com.telescop) ||
			 PILGetString(k="instrume", com.instrume) ||
			 PILGetInt   (k="rand_seed", &com.rand.seed) ||
			 PILGetReal  (k="rand_skip", &com.rand.skip) ||
			 PILGetInt   (k="simulation_mode", &com.mode) ||
			 PILGetFname (k="teldef", com.teldef_file) ||
			 PILGetFname (k="leapfile", com.leapfile) ) {
			anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
			*status = ANL_QUIT;
			return;
		}
		*status = ANL_OK;;
		return;
	}

	for (;;) {
		char *key;
		int ans[2];

		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("SHOW", key) ) {
			show_parameter("%s:  *** show parameter ***");
		} else if ( 0 == strcmp("TELESCOP", key) ) {
			CLtxtrd(key, com.telescop, sizeof(com.telescop));
		} else if ( 0 == strcmp("INSTRUME", key) ) {
			CLtxtrd(key, com.instrume, sizeof(com.instrume));
		} else if ( 0 == strcmp("RAND_SEED", key) ) {
			CLintrd("Random number seed ", &com.rand.seed);
		} else if ( 0 == strcmp("RAND_SKIP", key) ) {
			CLfdprd("Random number skip ", &com.rand.skip);
		} else if ( 0 == strcmp("SIMULATION_MODE", key) ) {
			static char *keytbl[] = {
				"Discard_Mode", "Weighting_Mode"
			};
			static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
			static int it;
			static char key[32];
			CLkeyrd(-1, "Default Simulation Mode",
					key, keytbl, nkey, &it, sizeof(key));
			if ( 0 == strcmp("Discard_Mode", key) ) {
				com.mode = SimASTE_Mode_Discard;
			} else if ( 0 == strcmp("Weighting_Mode", key) ) {
				com.mode = SimASTE_Mode_Weight;
			}
		} else if ( 0 == strcmp("TELDEF", key) ) {
			CLtxtrd(key, com.teldef_file, sizeof(com.teldef_file));
		} else if ( 0 == strcmp("LEAPFILE", key) ) {
			CLtxtrd(key, com.leapfile, sizeof(com.leapfile));
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
SimASTE_Root_init(int *status)
{
	static int (*func)(fitsfile *) = write_history;
	static struct {
		char *key;
		int size;
	} bnkdata[] = {
		{ "SimASTE:TELESCOP:PTR", sizeof(char *) },
		{ "SimASTE:INSTRUME:PTR", sizeof(char *) },
		{ "SimASTE:FILTER:PTR", sizeof(char *) },
		{ "SimASTE:RANDOM_SEED", sizeof(int) },
		{ "SimASTE:RANDOM_SKIP", sizeof(double) },
		{ "SimASTE:TELDEF", sizeof(com.teldef) },
		{ "SimASTE:LEAPSEC_FILE", sizeof(com.o_leapfile) },
		{ "SimASTE:LEAPFILE:PTR", sizeof(char *) },
		{ "SimASTE:WRITE_HISTORY:FUNC", sizeof(func) },
		{ "SimASTE:XRT:SHIELD_TRANSMIS", sizeof(double) },
		{ "SimASTE:XRT:SHIELD_TRANSMIS:FUNC", sizeof(func) },

		{ "SimASTE:N_PHOTON", sizeof(double) },
		{ "SimASTE:N_DETECT", sizeof(double) },
		{ "SimASTE:N_WEISUM", sizeof(double) },
		{ "SimASTE:PHOTON_TIME", sizeof(double) },	/* sec */
		{ "SimASTE:TIME", sizeof(double) },			/* sec */
		{ "SimASTE:TSTART", sizeof(double) },		/* sec */
		{ "SimASTE:TSTOP", sizeof(double) },		/* sec */
		{ "SimASTE:EXPOSURE", sizeof(double) },		/* sec */
		{ "SimASTE:GTI_DATA:PTR", sizeof(void *) },
		{ "SimASTE:GEOMAREA", sizeof(double) },		/* cm2 */
		{ "SimASTE:PHOTON_ENERGY", sizeof(double) },/* keV */
		{ "SimASTE:SENSOR", sizeof(int) },			/* 0-4 */
		{ "SimASTE:EULER", sizeof(AtEulerAng) },	/* rad */
		{ "SimASTE:SKYREF", sizeof(SKYREF) },		/* deg */
		{ "SimASTE:MODE", sizeof(com.mode) },		/* discard/weight */
		{ "SimASTE:WEIGHT", sizeof(double) },
		{ "SimASTE:RA", sizeof(double) },			/* deg */
		{ "SimASTE:DEC", sizeof(double) },			/* deg */
		{ "SimASTE:XRTINtheta", sizeof(double) },	/* arcmin */
		{ "SimASTE:XRTINphi", sizeof(double) },		/* deg */
		{ "SimASTE:XRTINX", sizeof(double) },		/* mm */
		{ "SimASTE:XRTINY", sizeof(double) },		/* mm */
		{ "SimASTE:XRTOUTtheta", sizeof(double) },	/* arcmin */
		{ "SimASTE:XRTOUTphi", sizeof(double) },	/* deg */
		{ "SimASTE:XRTOUTX", sizeof(double) },		/* mm */
		{ "SimASTE:XRTOUTY", sizeof(double) },		/* mm */
		{ "SimASTE:FOCX", sizeof(double) },			/* ch */
		{ "SimASTE:FOCX:TLMIN", sizeof(int) },
		{ "SimASTE:FOCX:TLMAX", sizeof(int) },
		{ "SimASTE:FOCY", sizeof(double) },			/* ch */
		{ "SimASTE:FOCY:TLMIN", sizeof(int) },
		{ "SimASTE:FOCY:TLMAX", sizeof(int) },
		{ "SimASTE:XISX", sizeof(double) },			/* mm */
		{ "SimASTE:XISY", sizeof(double) },			/* mm */
		{ "SimASTE:XRSX", sizeof(double) },			/* mm */
		{ "SimASTE:XRSY", sizeof(double) },			/* mm */
		{ "SimASTE:PIXEL", sizeof(int) },			/* ch */
		{ "SimASTE:PIXEL:TLMIN", sizeof(int) },
		{ "SimASTE:PIXEL:TLMAX", sizeof(int) },
		{ "SimASTE:RAWX", sizeof(int) },			/* ch */
		{ "SimASTE:RAWX:TLMIN", sizeof(int) },
		{ "SimASTE:RAWX:TLMAX", sizeof(int) },
		{ "SimASTE:RAWY", sizeof(int) },			/* ch */
		{ "SimASTE:RAWY:TLMIN", sizeof(int) },
		{ "SimASTE:RAWY:TLMAX", sizeof(int) },
		{ "SimASTE:DETX", sizeof(int) },			/* ch */
		{ "SimASTE:DETX:TLMIN", sizeof(int) },
		{ "SimASTE:DETX:TLMAX", sizeof(int) },
		{ "SimASTE:DETY", sizeof(int) },			/* ch */
		{ "SimASTE:DETY:TLMIN", sizeof(int) },
		{ "SimASTE:DETY:TLMAX", sizeof(int) },
		{ "SimASTE:ROLL", sizeof(double) },			/* degree */
		{ "SimASTE:SKYX", sizeof(int) },			/* ch */
		{ "SimASTE:SKYX:TLMIN", sizeof(int) },
		{ "SimASTE:SKYX:TLMAX", sizeof(int) },
		{ "SimASTE:SKYY", sizeof(int) },			/* ch */
		{ "SimASTE:SKYY:TLMIN", sizeof(int) },
		{ "SimASTE:SKYY:TLMAX", sizeof(int) },
		{ "SimASTE:PI", sizeof(int) },				/* ch */
		{ "SimASTE:PI:TLMIN", sizeof(int) },
		{ "SimASTE:PI:TLMAX", sizeof(int) },
		{ "SimASTE:PHA", sizeof(int) },				/* ch */
		{ "SimASTE:PHA:TLMIN", sizeof(int) },
		{ "SimASTE:PHA:TLMAX", sizeof(int) },
		{ "SimASTE:PHAS", 25*sizeof(int) },			/* ch */
		{ "SimASTE:GRADE", sizeof(int) },
		{ NULL, 0 }
	};
	int i, ival;
	char *string_ptr;
	TELDEF_ASTROE *aste;
	CALDB_INFO caldb;
	double n_photon, n_detect, n_weisum;

	EvsDef("SimASTE_Root:BEGIN");
	EvsDef("SimASTE_Root:ENTRY");
	EvsDef("SimASTE_Root:OK");

	for (i = 0; bnkdata[i].size; i++) {
		BnkDef(bnkdata[i].key, bnkdata[i].size);
	}

/* BnkPut TELESCOP, INSTRUME, SENSOR */
	string_ptr = com.telescop;
	BnkPut("SimASTE:TELESCOP:PTR", sizeof(string_ptr), &string_ptr);
	string_ptr = com.instrume;
	BnkPut("SimASTE:INSTRUME:PTR", sizeof(string_ptr), &string_ptr);
	com.sensor = aste_instrume_id(com.instrume);
	BnkPut("SimASTE:SENSOR", sizeof(com.sensor), &com.sensor);
	string_ptr = com.filter;
	BnkPut("SimASTE:FILTER:PTR", strlen(string_ptr), &string_ptr);

/* caldb support */
	com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
	if ( NULL == com.leapfile ) {
		*status = ANL_QUIT;
		return;
	}

	if ( 0 == CLstricmp("CALDB", com.o_teldef_file) ) {
		aste_caldb_init(&caldb);
		caldb.telescop = com.telescop;
		caldb.instrume = com.instrume;
		caldb.codename = "TELDEF";
		aste_caldb_get(&caldb);
		if ( 0 != caldb.status || 0 == caldb.nfound ) {
			anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
			  pname, caldb.codename, caldb.status);
			*status = ANL_QUIT;
			return;
		}
		if ( 1 != caldb.nfound ) {
			anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
				pname, caldb.nfound, caldb.codename);
		}
		com.teldef_file = caldb.filename;
	}

	show_parameter("%s:  *** show parameter ***");

/* initialize leapsec */
	if ( 0 != CLstricmp("NONE", com.o_leapfile) &&
		 NULL == atMissionTimeInit(NULL, -1) ) {
		fflush(NULL); printf("\n"); fflush(NULL);
		if ( NULL == atMissionTimeInit(com.leapfile, -1) ) {
			*status = ANL_QUIT;
			return;
		}
	}

/* read teldef file */
	if ( 0 == CLstricmp("NONE", com.o_teldef_file) ) {
		com.teldef = NULL;
	} else {
		anl_msg_info("\n\
%s: reading teldef file '%s'\n", pname, com.teldef_file);
		com.teldef = aste_coord_init(NULL, NULL, com.teldef_file);
		if ( NULL == com.teldef ) {
			anl_msg_error("\
%s: teldef file '%s' open failed\n", pname, com.teldef_file);
			*status = ANL_QUIT;
			return;
		}
	}

	BnkPut("SimASTE:TELDEF", sizeof(com.teldef), &com.teldef);
	BnkPut("SimASTE:LEAPSEC_FILE", strlen(com.leapfile), com.leapfile);
	BnkPut("SimASTE:LEAPFILE:PTR", sizeof(com.leapfile), &com.leapfile);

	if ( NULL != com.teldef ) {

		aste = com.teldef->mission.aste;
		ival = 0;
		BnkPut("SimASTE:PIXEL:TLMIN", sizeof(ival), &ival);
		ival = aste->seg_num - 1;
		BnkPut("SimASTE:PIXEL:TLMAX", sizeof(ival), &ival);
		ival = aste->raw.xpix1;

		BnkPut("SimASTE:RAWX:TLMIN", sizeof(ival), &ival);
		ival = aste->raw.xsiz + aste->raw.xpix1 - 1;
		BnkPut("SimASTE:RAWX:TLMAX", sizeof(ival), &ival);
		ival = aste->det.ypix1;
		BnkPut("SimASTE:RAWY:TLMIN", sizeof(ival), &ival);
		ival = aste->raw.ysiz + aste->raw.ypix1 - 1;
		BnkPut("SimASTE:RAWY:TLMAX", sizeof(ival), &ival);

		ival = aste->det.xpix1;
		BnkPut("SimASTE:DETX:TLMIN", sizeof(ival), &ival);
		ival = aste->det.xsiz + aste->det.xpix1 - 1;
		BnkPut("SimASTE:DETX:TLMAX", sizeof(ival), &ival);
		ival = aste->det.ypix1;
		BnkPut("SimASTE:DETY:TLMIN", sizeof(ival), &ival);
		ival = aste->det.ysiz + aste->det.ypix1 - 1;
		BnkPut("SimASTE:DETY:TLMAX", sizeof(ival), &ival);

		ival = aste->foc.xpix1;
		BnkPut("SimASTE:FOCX:TLMIN", sizeof(ival), &ival);
		ival = aste->foc.xsiz + aste->foc.xpix1 - 1;
		BnkPut("SimASTE:FOCX:TLMAX", sizeof(ival), &ival);
		ival = aste->foc.ypix1;
		BnkPut("SimASTE:FOCY:TLMIN", sizeof(ival), &ival);
		ival = aste->foc.ysiz + aste->foc.ypix1 - 1;
		BnkPut("SimASTE:FOCY:TLMAX", sizeof(ival), &ival);

		ival = aste->sky.xpix1;
		BnkPut("SimASTE:SKYX:TLMIN", sizeof(ival), &ival);
		ival = aste->sky.xsiz + aste->sky.xpix1 - 1;
		BnkPut("SimASTE:SKYX:TLMAX", sizeof(ival), &ival);
		ival = aste->sky.ypix1;
		BnkPut("SimASTE:SKYY:TLMIN", sizeof(ival), &ival);
		ival = aste->sky.ysiz + aste->sky.ypix1 - 1;
		BnkPut("SimASTE:SKYY:TLMAX", sizeof(ival), &ival);

	}

/* initialize random number */
	aste_rndtsini(com.rand.seed);
	BnkPut("SimASTE:RANDOM_SEED", sizeof(com.rand.seed), &com.rand.seed);
	aste_drndtsn_skipd(com.rand.skip);
	BnkPut("SimASTE:RANDOM_SKIP", sizeof(com.rand.skip), &com.rand.skip);

	BnkPut("SimASTE:MODE", sizeof(com.mode), &com.mode);

/* BnkPut WRITE_HISTORY:FUNC */
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

/* initialize N_PHOTON, N_DETECT, N_WEISUM */
	n_photon = n_detect = n_weisum = 0.0;
	BnkPut("SimASTE:N_PHOTON", sizeof(n_photon), &n_photon);
	BnkPut("SimASTE:N_DETECT", sizeof(n_detect), &n_detect);
	BnkPut("SimASTE:N_WEISUM", sizeof(n_weisum), &n_weisum);

	*status = ANL_OK;
}

void
SimASTE_Root_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_Root_bgnrun(int *status)
{
	EvsSet("SimASTE_Root:BEGIN");

	*status = ANL_OK;
}

void
SimASTE_Root_ana(int nevent, int eventid, int *status)
{
	static double weight = 1.0;

	EvsfSetM("SimASTE_Root:ENTRY");

	BnkfPutM("SimASTE:WEIGHT", sizeof(weight), &weight);

	EvsfSetM("SimASTE_Root:OK");

	*status = ANL_OK;
}

void
SimASTE_Root_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_Root_exit(int *status)
{
	int used;
	double n_photon, n_detect, n_weisum;

	com.rand.skip = aste_drndtsn_gen();
	BnkGet("SimASTE:N_PHOTON", sizeof(n_photon), &used, &n_photon);
	BnkGet("SimASTE:N_DETECT", sizeof(n_detect), &used, &n_detect);
	BnkGet("SimASTE:N_WEISUM", sizeof(n_weisum), &used, &n_weisum);

	anl_msg_info("\
%s: N_PHOTON=%.0f  N_DETECT=%.0f  N_WEISUM=%.6f\n\
%s: Number of Random number generated = %.0f\n",
		pname, n_photon, n_detect, n_weisum,
		pname, com.rand.skip);

	*status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
