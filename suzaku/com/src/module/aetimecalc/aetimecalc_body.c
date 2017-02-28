/*
	aetimecalc.c

	2005/06/28 Y.ISHISAKI	version 1.0

	2006/07/02 Y.ISHISAKI	version 1.1
		support of CALDB for leapfile

	2006/07/23 Y.ISHISAKI	version 1.2
		read yday0 parameter from .par file

	2006/08/02 Y.ISHISAKI	version 1.3
		use aefits_attimeD2datestr/datestr2attimeD() in aeFitsHeaderUtil-3.2
		caldb access moved from _com() to _init()
		print error when PILGet() failes
		use aste_mjdrefi(), aste_mjdreff() in  _startup()

	2007/05/13 Y.ISHISAKI	version 1.4
		use aste_caldb_find_leapfile()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "anl.h"
#include "cli.h"
#include "com.h"
#include "pil.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "aetimecalc";
char aetimecalc_version[] = "version 1.4";

static struct {
/* aetimecalc */
	char input_type[PIL_LINESIZE];
	char *leapfile;
	char o_leapfile[PIL_LINESIZE];
	double mission_time;
	double mjd_utc;
	double mjd_tt;
	double yday;
	char date_utc[PIL_LINESIZE];
	char date_tt[PIL_LINESIZE];
	char date0[PIL_LINESIZE];
	char yday0[PIL_LINESIZE];
	int    mjdrefi;
	double mjdreff;
	AtTimeD date0_attime;
	double date0_mjd;
	AtTimeD yday0_attime;
	double yday0_mjd;
	AtTimeD attime_utc;
	AtTimeD attime_tt;
} com;

static int
calc_time(void)
{
	int    mjdrefi = com.mjdrefi;
	double mjdreff = com.mjdreff;

	if ( 0 == strcmp("mission", com.input_type) ) {
		atMissionToAtTimeD(com.date0_mjd, com.mission_time, &com.attime_utc);
		aefits_attimeD2datestr(&com.attime_utc, com.date_utc);
		atMJulianD(&com.attime_utc, &com.mjd_utc);
		com.mjd_tt = aefits_mission2mjd_tt(com.mission_time, mjdrefi, mjdreff);
		atMJDateD(com.mjd_tt, &com.attime_tt);
		aefits_attimeD2datestr(&com.attime_tt, com.date_tt);
		com.yday = com.mjd_utc - com.yday0_mjd;
	} else if ( 0 == strcmp("date", com.input_type ) ) {
		aefits_datestr2attimeD(com.date_utc, &com.attime_utc);
		aefits_attimeD2datestr(&com.attime_utc, com.date_utc);
		atAtTimeDToMission(com.date0_mjd, &com.attime_utc, &com.mission_time);
		atMJulianD(&com.attime_utc, &com.mjd_utc);
		com.mjd_tt = aefits_mission2mjd_tt(com.mission_time, mjdrefi, mjdreff);
		atMJDateD(com.mjd_tt, &com.attime_tt);
		aefits_attimeD2datestr(&com.attime_tt, com.date_tt);
		com.yday = com.mjd_utc - com.yday0_mjd;
	} else if ( 0 == strcmp("date_tt", com.input_type) ) {
		aefits_datestr2attimeD(com.date_tt, &com.attime_tt);
		aefits_attimeD2datestr(&com.attime_tt, com.date_tt);
		atMJulianD(&com.attime_tt, &com.mjd_tt);
		com.mission_time = aefits_mjd_tt2mission(com.mjd_tt, mjdrefi, mjdreff);
		aefits_attimeD2datestr(&com.attime_utc, com.date_utc);
		atMJulianD(&com.attime_utc, &com.mjd_utc);
		com.yday = com.mjd_utc - com.yday0_mjd;
	} else if ( 0 == strcmp("mjd", com.input_type) ) {
		atMJDateD(com.mjd_utc, &com.attime_utc);
		atAtTimeDToMission(com.date0_mjd, &com.attime_utc, &com.mission_time);
		aefits_attimeD2datestr(&com.attime_utc, com.date_utc);
		com.mjd_tt = aefits_mission2mjd_tt(com.mission_time, mjdrefi, mjdreff);
		atMJDateD(com.mjd_tt, &com.attime_tt);
		aefits_attimeD2datestr(&com.attime_tt, com.date_tt);
		com.yday = com.mjd_utc - com.yday0_mjd;
	} else if ( 0 == strcmp("mjd_tt", com.input_type) ) {
		com.mission_time = aefits_mjd_tt2mission(com.mjd_tt, mjdrefi, mjdreff);
		atMissionToAtTimeD(com.date0_mjd, com.mission_time, &com.attime_utc);
		aefits_attimeD2datestr(&com.attime_utc, com.date_utc);
		atMJulianD(&com.attime_utc, &com.mjd_utc);
		atMJDateD(com.mjd_tt, &com.attime_tt);
		aefits_attimeD2datestr(&com.attime_tt, com.date_tt);
		com.yday = com.mjd_utc - com.yday0_mjd;
	} else if ( 0 == strcmp("yday", com.input_type) ) {
		com.mjd_utc = com.yday0_mjd + com.yday;
		atMJDateD(com.mjd_utc, &com.attime_utc);
		atAtTimeDToMission(com.date0_mjd, &com.attime_utc, &com.mission_time);
		aefits_attimeD2datestr(&com.attime_utc, com.date_utc);
		com.mjd_tt = aefits_mission2mjd_tt(com.mission_time, mjdrefi, mjdreff);
		atMJDateD(com.mjd_tt, &com.attime_tt);
		aefits_attimeD2datestr(&com.attime_tt, com.date_tt);
	} else {
		return -1;
	}

	return 0;
}

static int
print_time(void)
{
	char *k;

	printf("Mission Time = %.6f (s)\n", com.mission_time);
	printf("Date in UTC  = %s\n", com.date_utc);
	printf("MJD  in UTC  = %.9f (dy)\n", com.mjd_utc);
	printf("Date in TT   = %s\n", com.date_tt);
	printf("MJD  in TT   = %.9f (dy)\n", com.mjd_tt);
	printf("Y%+.3f (dy)\n", com.yday);

	if ( PILPutReal(k="mission", com.mission_time) ||
		 PILPutString(k="date", com.date_utc) ||
		 PILPutReal(k="mjd", com.mjd_utc) ||
		 PILPutString(k="date_tt", com.date_tt) ||
		 PILPutReal(k="mjd_tt", com.mjd_tt) ||
		 PILPutReal(k="yday", com.yday) ||
		 0 ) {
		fprintf(stderr, "\
%s: PILPut('%s') failed\n", pname, k);
	}

	PILFlushParameters();

	return 0;
}

void
aetimecalc_startup(int *status)
{
	com.leapfile = com.o_leapfile;

	strcpy(com.input_type, "prompt");
	strcpy(com.yday0, "2005-07-10T00:00:00.000");
	strcpy(com.date0, "2000-01-01T00:00:00.000");
	strcpy(com.o_leapfile, "leapsec.fits");
	com.mjdrefi = aste_mjdrefi();
	com.mjdreff = aste_mjdreff();

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%12s   '%s'\n", "INPUT", com.input_type);
	if ( com.leapfile == com.o_leapfile ) {
		printf("%12s   '%s'\n", "LEAPFILE", com.leapfile);
	} else {
		printf("%12s   '%s' (%s)\n", "LEAPFILE", com.leapfile, com.o_leapfile);
	}
	printf("%12s   '%s' (%.3f in MJD-UTC)\n", "DATE0",com.date0,com.date0_mjd);
	printf("%12s   '%s' (%.3f in MJD-UTC)\n", "YDAY0",com.yday0,com.yday0_mjd);
	printf("%12s   %d\n", "MJDREFI", com.mjdrefi);
	printf("%12s   %.17f\n", "MJDREFF", com.mjdreff);
}

void
aetimecalc_com(int *status)
{
#define NVAL	3
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"LEAPFILE",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"leap seconds table file name",
		"exit from this menu"
	};

	char *k;
	int answer[2];
	int nreply = 1;

	if ( *status ) {	/* ftools */
		if ( PILGetString(k="input", com.input_type) ) {
			goto pil_error;
		}
		if ( 0 == CLstricmp("mission", com.input_type) ) {
			if ( PILGetReal(k="mission", &com.mission_time) ) goto pil_error;
		} else if ( 0 == CLstricmp("date", com.input_type) ) {
			if ( PILGetString(k="date", com.date_utc) ) goto pil_error;
		} else if ( 0 == CLstricmp("date_tt", com.input_type) ) {
			if ( PILGetString(k="date_tt", com.date_tt) ) goto pil_error;
		} else if ( 0 == CLstricmp("mjd", com.input_type) ) {
			if ( PILGetReal(k="mjd", &com.mjd_utc) ) goto pil_error;
		} else if ( 0 == CLstricmp("mjd_tt", com.input_type) ) {
			if ( PILGetReal(k="mjd_tt", &com.mjd_tt) ) goto pil_error;
		} else if ( 0 == CLstricmp("yday", com.input_type) ) {
			if ( PILGetReal(k="yday", &com.yday) ) goto pil_error;
		} else if ( 0 != CLstricmp("prompt", com.input_type) ) {
			anl_msg_warning("\
%s: WARNING: unknown input type '%s', assuming as 'prompt'\n",
				pname, com.input_type);
			strcpy(com.input_type, "prompt");
		}
		if (
PILGetFname (k="leapfile", com.o_leapfile) ||
PILGetString(k="date0", com.date0) ||
PILGetString(k="yday0", com.yday0) ||
PILGetInt   (k="mjdrefi", &com.mjdrefi) ||
PILGetReal  (k="mjdreff", &com.mjdreff) ||
			 0 ) {
			goto pil_error;
		}

		*status = ANL_OK;;
		return;

	pil_error:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
		*status = ANL_QUIT;
		return;
	}

	for (;;) {
		char *p;
		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("LEAPFILE", p) ) {
			CLtxtrd(p, com.leapfile, sizeof(com.leapfile));
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
aetimecalc_init(int *status)
{
	int verbose;

	com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
	if ( NULL == com.leapfile ) {
		goto quit;
	}

/* convert date0, yday0 string into MJD-UT */
	aefits_datestr2attimeD(com.date0, &com.date0_attime);
	aefits_datestr2attimeD(com.yday0, &com.yday0_attime);
	atMJulianD(&com.date0_attime, &com.date0_mjd);
	atMJulianD(&com.yday0_attime, &com.yday0_mjd);

/* show current parameters */
	show_parameter();

/* initialize aste_time */
	printf("\n");
	verbose = ( 0 == CLstricmp("none", com.leapfile) ) ? -1 : -2;
	if ( NULL == atMissionTimeInit(com.leapfile, verbose) ) {
		anl_msg_error("\
%s: atMissionTimeInit('%s') failed\n", pname, com.leapfile);
		goto quit;
	}
	if ( -1 == verbose ) printf("\n");

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
aetimecalc_his(int *status)
{
	*status = ANL_OK;
}

void
aetimecalc_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
aetimecalc_ana(int *nevent, int *eventid, int *status)
{
	int istat;

	if ( 0 == strcmp("prompt", com.input_type) ) {
#define NTABLE 7
		static char *table[NTABLE] = {
			"mission", "date", "mjd", "date_tt", "mjd_tt", "yday", "exit"
		};
		static char prompt[] = "input_type";
		static char word[80];
		int ichoice;

		for (;;) {
			CLkeyrd(-1, prompt, word, table, NTABLE, &ichoice, sizeof(word));
			strcpy(com.input_type, table[ichoice-1]);
			if ( 0 == strcmp("exit", com.input_type) ) {
				*status = ANL_QUIT;
				return;
			} else if ( 0 == strcmp("mission", com.input_type) ) {
				CLfdprd(com.input_type, &com.mission_time);
			} else if ( 0 == strcmp("date", com.input_type) ) {
				CLtitrd(com.input_type, com.date_utc, sizeof(com.date_utc));
			} else if ( 0 == strcmp("mjd", com.input_type) ) {
				CLfdprd(com.input_type, &com.mjd_utc);
			} else if ( 0 == strcmp("date_tt", com.input_type) ) {
				CLtitrd(com.input_type, com.date_tt, sizeof(com.date_tt));
			} else if ( 0 == strcmp("mjd_tt", com.input_type) ) {
				CLfdprd(com.input_type, &com.mjd_tt);
			} else if ( 0 == strcmp("yday", com.input_type) ) {
				CLfdprd(com.input_type, &com.yday);
			}

			istat = calc_time();
			if ( istat ) {
				anl_msg_error("\
%s: unknown input type '%s'\n", pname, com.input_type);
				*status = ANL_QUIT;
				return;
			}
			print_time();
		}
	}

	istat = calc_time();
	if ( istat ) {
		anl_msg_error("\
%s: unknown input type '%s'\n", pname, com.input_type);
		*status = ANL_QUIT;
		return;
	}

	print_time();

	*status = ANL_QUIT;
}

void
aetimecalc_endrun(int *status)
{
	*status = ANL_OK;
}

void
aetimecalc_exit(int *status)
{
	*status = ANL_OK;
}
