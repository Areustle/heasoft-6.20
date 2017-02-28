/*
  AEmkEHKtimeGen.c

	1999/12/26 Y.ISHISAKI	version 1.0

	2004/03/14 Y.ISHISAKI	version 1.1
		fix printf formats
		remove unused variables
		include "com.h", remove "cfortran.h"

	2005/06/26 Y.ISHISAKI	version 1.5 (to match with AEmkEHKfitsWrite-1.5)
		use CLstricmp() instead of local STRICMP()
		read MEAN_EA[1-3] from attitude file header
		BnkDef/Put ASTE:MEAN_EA, ASTE:MEAN_FOV

	2005/10/24 Y.ISHISAKI	version 1.6
		use aste_orbit() instead of atFunctions
		new BNK "ASTE:ORBIT:PTR", "ASTE:LEAPFILE:PTR"

	2006/07/02 Y.ISHISAKI	version 1.7
		support of CALDB for leapfile & teldef

	2006/07/24 Y.ISHISAKI	version 2.0
		use anl_msg_xxx() functions
		print error when PILGetXXX() failed

	2007/04/16 Y.ISHISAKI	version 2.2
		use aste_caldb_find_leapfile() for leapfile

	2007/05/07 Y.ISHISAKI	version 2.3
		remove unused include files

	2007/05/14 Y.ISHISAKI	version 2.4
		change LEAPFILE handling in show_parameter()
		PILGet 'outfile', 'rigidity', 'clobber' in this module
		use aste_caldb_find_rigidity()
		change *pname = "AEmkEHKtimeGen" -> "aemkehk"
		use floor() & ceil() to determine t0, t1 in decide_start_stop()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "com.h"
#include "pil.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_orbit.h"
#include "aste_time.h"
#include "aste_att.h"
#include "aste_caldb.h"

static char pname[] = "aemkehk";
char AEmkEHKtimeGen_version[] = "version 2.4";

static struct {
/* AEmkEHKtimeGen */
	char *outfile, o_outfile[PIL_LINESIZE];
	char orbit_file[PIL_LINESIZE];
	char attitude_file[PIL_LINESIZE];
	char reference_file[PIL_LINESIZE];
	char *leapfile, o_leapfile[PIL_LINESIZE];
	char *teldef_file, o_teldef_file[PIL_LINESIZE];
	char *rigidity, o_rigidity[PIL_LINESIZE];
	char time_col_name[PIL_LINESIZE];
	double start_time, stop_time;
	double t0, t1;
	double step_sec;
	double margin_sec;
	int clobber;

	ORBIT *orbit;
	ATTFILE *attfile;
	fitsfile *reffile;
	AtRigData2 *rdp;
	TELDEF *teldef;
	int time_col_num;
	long irow, nrow;
} com;

static int
read_reference_file(char *reference_filename)
{
	int hdunum, hdutype, anynul, col;
	char comment[80];
	fitsfile *fp;
	long naxis2;
	char *key;
	double t0, t1;

	int istat = 0;

/* open time reference file */
	fits_open_file(&fp, reference_filename, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed\n", pname, reference_filename);
		goto quit;
	}

	fits_get_hdu_num(fp, &hdunum);
	if ( 1 == hdunum ) {
		fits_movrel_hdu(fp, 1, &hdutype, &istat);
	}

	if ( '\0' == com.time_col_name ) {
		strcpy(com.time_col_name, "TIME");	/* default column name TIME */
	}
	key = com.time_col_name;
	fits_get_colnum(fp, CASESEN, key, &col, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed in '%s'\n", pname, key, reference_filename);
		goto quit;
	}

	fits_read_key_lng(fp, "NAXIS2", &naxis2, comment, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: NAXIS2 not found in '%s'\n", pname, reference_filename);
		goto quit;
	}

	com.irow = 1;
	com.nrow = naxis2;

	fits_read_col_dbl(fp, col, com.irow, 1, 1, 0.0, &t0, &anynul, &istat);
	fits_read_col_dbl(fp, col, com.nrow, 1, 1, 0.0, &t1, &anynul, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_read_col('%s') faild in '%s'\n", pname, key, reference_filename);
		goto quit;
	}

	anl_msg_info("\
%s: generate TIME from %.3f to %.3f,\n\
\treferring '%s'(%s), %ld rows\n",
		pname, t0, t1, reference_filename, key, com.nrow);

	com.reffile = fp;
	com.time_col_num = col;
	com.t0 = t0;
	com.t1 = t1;

	return 0;

 quit:
	return istat;
}

static int
decide_start_stop(double t0, double t1, double step_sec, double margin_sec)
{
	double t, duration;

/* calculate t0 and t1 from attfile, if t0 or t1 is 0.0 */
	t0 = ( 0.0 == t0 ) ? com.attfile->tstart : t0;
	t1 = ( 0.0 == t1 ) ? com.attfile->tstop  : t1;

	if ( t1 < t0 ) {
		t = t0; t0 = t1; t1 = t;	/* swap t0 <-> t1 */
	}

	if ( step_sec <= 0.0 ) {
		step_sec = 1.0;		/* default 1.0 sec */
	}

	t0 = floor(t0 - margin_sec);
	t1 = ceil(t1 + margin_sec);
	duration = t1 - t0;

	com.irow = 1;
	com.nrow = (long)ceil(duration / step_sec) + 1;

	com.t0 = t0;
	com.t1 = t0 + (com.nrow - 1) * step_sec;

	resetAttFileExtrapolationLimits(com.attfile, margin_sec + step_sec);

	anl_msg_info("\
%s: generate TIME from %.3f to %.3f,\n\
\tin %.1f sec step, %ld rows\n",
		pname, com.t0, com.t1, step_sec, com.nrow);

	return 0;
}

void
AEmkEHKtimeGen_startup(int *status)
{
	com.outfile = com.o_outfile;
	com.leapfile = com.o_leapfile;
	com.teldef_file = com.o_teldef_file;
	com.rigidity = com.o_rigidity;

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "OUTFILE", com.outfile);
	printf("%20s   '%s'\n", "ORBIT", com.orbit_file);
	printf("%20s   '%s'\n", "ATTITUDE", com.attitude_file);
	printf("%20s   '%s'\n", "REFERENCE", com.reference_file);
	printf("%20s   '%s'%s\n", "TELDEF", com.teldef_file,
		(com.teldef_file == com.o_teldef_file) ? "" : " (CALDB)");
	if ( com.leapfile == com.o_leapfile ) {
		printf("%20s   '%s'\n", "LEAPFILE", com.leapfile);
	} else {
		printf("%20s   '%s' (%s)\n", "LEAPFILE", com.leapfile, com.o_leapfile);
	}
	if ( com.rigidity == com.o_rigidity ) {
		printf("%20s   '%s'\n", "RIGIDITY", com.rigidity);
	} else {
		printf("%20s   '%s' (%s)\n", "RIGIDITY", com.rigidity, com.o_rigidity);
	}
	printf("%20s   %.6f\n", "TSTART", com.start_time);
	printf("%20s   %.6f\n", "TSTOP", com.stop_time);
	printf("%20s   '%s'\n", "TIME_COL_NAME", com.time_col_name);
	printf("%20s   %.1f\n", "STEP_SEC", com.step_sec);
	printf("%20s   %.1f\n", "MARGIN_SEC", com.margin_sec);
	printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES" : "NO");
	printf("\n");
}

void
AEmkEHKtimeGen_com(int *status)
{
#define NVAL	14
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"OUTFILE",
		"ORBIT",
		"ATTITUDE",
		"REFERENCE",
		"TELDEF",
		"LEAPFILE",
		"RIGIDITY",
		"TSTART_TSTOP",
		"TIME_COL_NAME",
		"STEP_SEC",
		"MARGIN_SEC",
		"CLOBBER",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"output event file name",
		"input orbit file name",
		"input attitude file name",
		"time reference file name",
		"teldef file name",
		"leap seconds table file name",
		"rigidity data file name",
		"start & stop time, or 0.0 to get from attfile"
		"time column name in the reference file",
		"step time (s)",
		"margin time (s)",
		"overwrite output file if exists",
		"exit from this menu"
	};

	char *p, *k;
	int answer[2];
	int nreply = 1;

	if ( *status ) {	/* ftools */
		*status = ANL_QUIT;
		if (
PILGetFname(k="outfile", com.outfile) ||
PILGetFname(k="orbit", com.orbit_file) ||
PILGetFname(k="attitude", com.attitude_file) ||
PILGetFname(k="reference", com.reference_file) ||
PILGetFname(k="teldef", com.o_teldef_file) ||
PILGetFname(k="leapfile", com.o_leapfile) ||
PILGetFname(k="rigidity", com.o_rigidity) ||
PILGetBool (k="clobber", &com.clobber) ||
			 0 ) {
			goto error;
		}
		if ( 0 == CLstricmp("NONE", com.reference_file) ) {
			if (
PILGetReal(k="start_time", &com.start_time) ||
PILGetReal(k="stop_time", &com.stop_time) ||
PILGetReal(k="step_sec", &com.step_sec) ||
PILGetReal(k="margin_sec", &com.margin_sec) ) {
				goto error;
			}
		} else if (
PILGetString(k="time_col_name", com.time_col_name) ) {
			goto error;
		}

		*status = ANL_OK;;
		return;

	error:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
		*status = ANL_QUIT;;
		return;
	}

	for (;;) {
		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("OUTFILE", p) ) {
			CLtxtrd(p, com.outfile, sizeof(com.outfile));
		} else if ( 0 == strcmp("ORBIT", p) ) {
			CLtxtrd(p, com.orbit_file, sizeof(com.orbit_file));
		} else if ( 0 == strcmp("ATTITUDE", p) ) {
			CLtxtrd(p, com.attitude_file, sizeof(com.attitude_file));
		} else if ( 0 == strcmp("REFERENCE", p) ) {
			CLtxtrd(p, com.reference_file, sizeof(com.reference_file));
		} else if ( 0 == strcmp("TELDEF", p) ) {
			CLtxtrd(p, com.teldef_file, sizeof(com.teldef_file));
		} else if ( 0 == strcmp("LEAPFILE", p) ) {
			CLtxtrd(p, com.leapfile, sizeof(com.leapfile));
		} else if ( 0 == strcmp("RIGIDITY", p) ) {
			CLtxtrd(p, com.o_rigidity, sizeof(com.o_rigidity));
		} else if ( 0 == strcmp("TSTART_TSTOP", p) ) {
			CLfdprd("TSTART, or 0.0 to refer attfile", &com.start_time);
			CLfdprd("TSTOP, or 0.0 to refer attfile", &com.stop_time);
		} else if ( 0 == strcmp("TIME_COL_NAME", p) ) {
			CLtxtrd(p, com.time_col_name, sizeof(com.time_col_name));
		} else if ( 0 == strcmp("STEP_SEC", p) ) {
			CLfdprd(p, &com.step_sec);
		} else if ( 0 == strcmp("MARGIN_SEC", p) ) {
			CLfdprd(p, &com.margin_sec);
		} else if ( 0 == strcmp("CLOBBER", p) ) {
			CLlogrd(p, &com.clobber);
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
AEmkEHKtimeGen_init(int *status)
{
	double aetime;
	AtEulerAng ea;
	SKYREF fov;
	CALDB_INFO caldb;
	int verbose;

	int istat = 0;

/* caldb support */
	com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
	if ( NULL == com.leapfile ) {
		goto quit;
	}

	com.rigidity = aste_caldb_find_rigidity(com.o_rigidity);
	if ( NULL == com.rigidity ) {
		goto quit;
	}

	aste_caldb_init(&caldb);
	if ( 0 == CLstricmp("CALDB", com.o_teldef_file) ) {
		caldb.telescop = aste_telescop();
		caldb.instrume = aste_instrume(ASTE_XIS0_ID);
		caldb.codename = "TELDEF";
		aste_caldb_get(&caldb);
		if ( 0 != caldb.status || 0 == caldb.nfound ) {
			anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
				pname, caldb.codename, caldb.status);
			goto quit;
		}
		if ( 1 != caldb.nfound ) {
			anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
				pname, caldb.nfound, caldb.codename);
		}
		com.teldef_file = caldb.filename;
	}

/* show current parameters */
	show_parameter();

/* BNK definition */
	BnkDef("ASTE:EHK:OFILE_NAME:PTR", sizeof(com.outfile));
	BnkDef("ASTE:EHK:OFP", sizeof(fitsfile *));
	BnkDef("ASTE:ORBIT:PTR", sizeof(com.orbit));
	BnkDef("ASTE:ATTFILE:PTR", sizeof(com.attfile));
	BnkDef("ASTE:LEAPFILE:PTR", sizeof(com.leapfile));
	BnkDef("ASTE:RIGIDITY:PTR", sizeof(com.rigidity));
	BnkDef("ASTE:TELDEF:PTR", sizeof(com.teldef));
	BnkDef("ASTE:MEAN_EA", sizeof(ea));
	BnkDef("ASTE:MEAN_FOV", sizeof(fov));
	BnkDef("ASTE:TIME", sizeof(double));

/* initialize aste_time */
	verbose = ( 0 == CLstricmp("none", com.leapfile) ) ? -1 : -2;
	if ( NULL == atMissionTimeInit(com.leapfile, verbose) ) {
		anl_msg_error("\
%s: atMissionTimeInit('%s') failed\n", pname, com.leapfile);
		goto quit;
	}
	if ( -1 == verbose ) printf("\n");

/* initialize rigidity for COR2 */
	istat = atRigSet2(&com.rdp, com.rigidity);
	if ( istat ) {
		anl_msg_error("\
%s: atRigSet2('%s') failed (%d)\n", pname, com.rigidity, istat);
		goto quit;
	}

/* initialize aste_coord */
	com.teldef = aste_coord_init(NULL, NULL, com.teldef_file);
	if ( NULL == com.teldef ) {
		anl_msg_error("\
%s: aste_coord_init('%s') failed\n", pname, com.teldef_file);
		goto quit;
	}

/* initialize orbit file */
	istat = aste_orbit_init(&com.orbit, com.orbit_file);
	if ( istat ) {
		anl_msg_error("\
%s: error in orbit file '%s'\n", pname, com.orbit_file);
		goto quit;
	}

/* initialize aste_att */
	com.attfile = aste_att_init(com.attitude_file);
	if ( NULL == com.attfile ) {
		anl_msg_error("\
%s: aste_att_init('%s') failed\n", pname, com.attitude_file);
		goto quit;
	}

	com.reffile = NULL;

	if ( 0 == CLstricmp("none", com.reference_file) ) {
		istat = decide_start_stop(com.start_time, com.stop_time,
								  com.step_sec, com.margin_sec);
	} else {
		istat = read_reference_file(com.reference_file);
	}
	if ( istat ) {
		goto quit;
	}

	if ( ! isInExtrapolatedAttFile(com.attfile, com.t0) ) {
		anl_msg_error("\
%s: t=%.3f exceeds attfile range\n", pname, com.t0);
		goto quit;
	} else if ( ! isInExtrapolatedAttFile(com.attfile, com.t1) ) {
		anl_msg_error("\
%s: t=%.3f exceeds attfile range\n", pname, com.t1);
		goto quit;
	}

/* get mean euler angles & fov */

	if ( NULL == com.attfile->fp ||
fits_read_key_dbl(com.attfile->fp, "MEAN_EA1", &ea.phi, NULL, &istat) ||
fits_read_key_dbl(com.attfile->fp, "MEAN_EA2", &ea.theta, NULL, &istat) ||
fits_read_key_dbl(com.attfile->fp, "MEAN_EA3", &ea.psi, NULL, &istat) ||
		 0 ) {
		aetime = (com.t0 + com.t1) / 2.0;
		anl_msg_warning("\
%s: WARNING: reading MEAN_EA[1-3] keywords failed\n\
	using the Euler Angles at t=%.3f\n", pname, aetime);
		istat = aste_att_euler(aetime, &ea);
		if ( istat ) {
			anl_msg_error("\
%s: getting the Euler Angles failed\n", pname);
			goto quit;
		}

	} else {

		ea.phi *= DEG2RAD;
		ea.theta *= DEG2RAD;
		ea.psi *= DEG2RAD;

	}

	aste_euler2skyref(com.teldef, &ea, &fov);

	if ( com.clobber ) {
		unlink(com.outfile);
	}

/* setting BNK */

	BnkPut("ASTE:EHK:OFILE_NAME:PTR", sizeof(com.outfile), &com.outfile);
	BnkPut("ASTE:ORBIT:PTR", sizeof(com.orbit), &com.orbit);
	BnkPut("ASTE:ATTFILE:PTR", sizeof(com.attfile), &com.attfile);
	BnkPut("ASTE:LEAPFILE:PTR", sizeof(com.leapfile), &com.leapfile);
	BnkPut("ASTE:RIGIDITY:PTR", sizeof(com.rdp), &com.rdp);
	BnkPut("ASTE:TELDEF:PTR", sizeof(com.teldef), &com.teldef);
	BnkPut("ASTE:MEAN_EA", sizeof(ea), &ea);
	BnkPut("ASTE:MEAN_FOV", sizeof(fov), &fov);

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
AEmkEHKtimeGen_his(int *status)
{
	*status = ANL_OK;
}

void
AEmkEHKtimeGen_bgnrun(int *status)
{
	fitsfile *fp;
	char history[PIL_LINESIZE+80];
	int used = 0;
	int istat = 0;

	BnkGet("ASTE:EHK:OFP", sizeof(fp), &used, &fp);
	if ( used != sizeof(fp) || NULL == fp ) {
		goto skip;
	}

	sprintf(history, "  outfile='%s'", com.outfile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "  orbit='%s'", com.orbit_file);
	fits_write_history(fp, history, &istat);
	sprintf(history, "  attitude='%s'", com.attitude_file);
	fits_write_history(fp, history, &istat);
	if ( NULL != com.reffile ) {
		sprintf(history, "  reference='%s'", com.reference_file);
		fits_write_history(fp, history, &istat);
		sprintf(history, "  time_col_name='%s'", com.time_col_name);
		fits_write_history(fp, history, &istat);
	} else {
		sprintf(history, "  start_time=%.6f", com.start_time);
		fits_write_history(fp, history, &istat);
		sprintf(history, "  stop_time=%.6f", com.stop_time);
		fits_write_history(fp, history, &istat);
		sprintf(history, "  step_sec=%.6f", com.step_sec);
		fits_write_history(fp, history, &istat);
		sprintf(history, "  margin_sec=%.6f", com.margin_sec);
		fits_write_history(fp, history, &istat);
	}
	sprintf(history, "  teldef='%s'%s", com.teldef_file,
		(com.teldef_file == com.o_teldef_file) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	if ( com.leapfile == com.o_leapfile ) {
		sprintf(history, "  leapfile='%s'", com.leapfile);
	} else {
		sprintf(history, "  leapfile='%s' (%s)", com.leapfile, com.o_leapfile);
	}
	fits_write_history(fp, history, &istat);
	sprintf(history, "  rigidity='%s'", com.rigidity);
	fits_write_history(fp, history, &istat);
	sprintf(history, "  clobber='%s'", com.clobber ? "yes" : "no");
	fits_write_history(fp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
	}

 skip:

	*status = ANL_OK;
}

void
AEmkEHKtimeGen_ana(int *nevent, int *eventid, int *status)
{
	double t;
	int anynul;
	int istat = 0;

/* check if finished */
	if ( com.nrow < com.irow ) {
		*status = ANL_QUIT;
		return;
	}

	if ( NULL != com.reffile ) {
/* read TIME column from reffile */
		fits_read_col_dbl(com.reffile, com.time_col_num, com.irow, 1, 1, 0.0,
						  &t, &anynul, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld in '%s'\n",
				   pname, com.irow, com.reference_file);
			*status = ANL_QUIT;
			return;
		}

		com.irow++;

	} else {
/* calculate t using step_sec */
		t = com.t0 + com.step_sec * (com.irow - 1);
		com.irow++;

	}

	BnkfPutM("ASTE:TIME", sizeof(t), &t);

	*status = ANL_OK;
}

void
AEmkEHKtimeGen_endrun(int *status)
{
	*status = ANL_OK;
}

void
AEmkEHKtimeGen_exit(int *status)
{
	int istat = 0;

	if ( NULL != com.reffile ) {
		fits_close_file(com.reffile, &istat);
		com.reffile = NULL;
	}

	if ( NULL != com.orbit ) {
		aste_orbit_free(com.orbit);
	}

	if ( NULL != com.attfile ) {
		aste_att_close(com.attfile);
	}

	if ( NULL != com.teldef ) {
		aste_coord_free(com.teldef);
	}

	if ( NULL != com.rdp ) {
		atRigFree2(com.rdp);
	}

	*status = ANL_OK;
}
