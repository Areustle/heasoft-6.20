/*
 SimASTE_PhotonFitsWrite.c
   SimASTE module : Photon list FITS writer

  1998-03-03 version 1.00	Y.ISHISAKI
		coded first

  1998-09-10 version 1.10	Y.ISHISAKI
	work with ftools parameter interface
	add/change FITS keywords

  1999-01-22 version 1.20       Y.ISHISAKI
	Write TSTART/TSTOP/EXPOSURE keywords to FITS header

  2006-04-09 version 1.3       Y.ISHISAKI
	rename SimASTE_PhotonFITSwrite -> SimASTE_PhotonFitsWrite
	increase filename[256] -> [1024]

  2006-08-06 version 2.2		Y.ISHISAKI
	add write_history(), BnkGet/Put SimASTE:WRITE_HISTORY:FUNC in _init()
	add write_timestamp(), write_random_number_info()
	rename com.filename -> com.outfile
	write GEOMAREA keyword, BnkGet SimASTE:GEOMAREA
	change extname 'PHOTON LIST' -> 'PHOTON_LIST'

  2006-10-18 version 2.3	Y.ISHISAKI
	fix unit "sec" -> "s" for "PHOTON_TIME" column

  2008-07-31 version 2.4	Y.ISHISAKI
	print '\n' after show_parameter() in _init()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <math.h>
#include "fitsio.h"
#include "anl.h"
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_rand.h"
#include "aeFitsHeaderUtil.h"
#include "SimASTE.h"

static char pname[] = "SimASTE_PhotonFitsWrite";
char SimASTE_PhotonFitsWrite_version[] = "version 2.3";

/* followings are examples of input paraeters for SimASTE_PhotonFitsWrite */
static struct {
	fitsfile *fp;
	char outfile[PIL_LINESIZE];
	int clobber;
	long nrow;
	int (*prev_write_history)(fitsfile *);
} com = {
	NULL,					/* fp */
	"photon_list.fits",		/* outfile */
	0,						/* clobber */
	0L,						/* nrow */
	NULL					/* prev_write_history */
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
  outfile='%s'", com.outfile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  clobber=%s", com.clobber ? "yes" : "no");
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

static void
show_parameter(char *title)
{
	MSG("");
	MSG(title, pname);
	MSG("");

	MSG("%4s%-20s'%s'", "", "OUTFILE", com.outfile);
	MSG("%4s%-20s%s", "", "CLOBBER", com.clobber ? "YES" : "NO");
}

void
SimASTE_PhotonFitsWrite_startup(int *status)
{
	;
}

void
SimASTE_PhotonFitsWrite_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"OUTFILE",
		"CLOBBER",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"Set output file name",
		"When set, overwrite output file if exists",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
	char *k;

	if ( *status ) {	/* ftools */
		if ( PILGetFname(k="outfile", com.outfile) ||
			 PILGetBool (k="clobber", &com.clobber) ) {
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
		} else if ( 0 == strcmp("OUTFILE", key) ) {
			CLtxtrd(key, com.outfile, sizeof(com.outfile));
		} else if ( 0 == strcmp("CLOBBER", key) ) {
			CLlogrd(key, &com.clobber);
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
SimASTE_PhotonFitsWrite_init(int *status)
{
	static int (*func)(fitsfile *fp) = write_history;
	static char extname[] = "PHOTON_LIST";
	static int tbltype = BINARY_TBL;
	static char *ttype[] = {
		"PHOTON_TIME", "PHOTON_ENERGY", "RA", "DEC"
	};
	static char *tform[] = {
		"1D", "1E", "1E", "1E"
	};
	static char *tunit[] = {
		"s", "keV", "deg", "deg"
	};

	fitsfile *fp;
	int used, morekeys;
	char *k;
	double geomarea;

	int istat = 0;

	EvsDef("SimASTE_PhotonFitsWrite:BEGIN");
	EvsDef("SimASTE_PhotonFitsWrite:ENTRY");
	EvsDef("SimASTE_PhotonFitsWrite:OK");

	com.prev_write_history = NULL;
	BnkGet("SimASTE:WRITE_HISTORY:FUNC", sizeof(com.prev_write_history),
		&used, &com.prev_write_history);
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

	if ( com.clobber ) {
		unlink(com.outfile);
	}

	fits_create_file(&fp, com.outfile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: '%s' create failed (%d)\n", pname, com.outfile, istat);
		goto quit;
	}

	fits_create_tbl(fp, tbltype, 0, 4, ttype, tform, tunit, extname, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_tbl() failed (%d)\n", pname, istat);
		goto quit;
	}

	BnkGet("SimASTE:GEOMAREA", sizeof(geomarea), &used, &geomarea);
	if (
fits_write_key_fixdbl(fp, k="GEOMAREA", geomarea, 4,
		"geometrical area of XRT (cm2)", &istat) ||
fits_write_key_null(fp, k="TSTART",
        "mission time of the observation start (s)", &istat) ||
fits_write_key_null(fp, k="TSTOP",
        "mission time of the observation stop (s)", &istat) ||
fits_write_key_null(fp, k="EXPOSURE",
        "exposure time (s)", &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	if ( aefits_write_module_history(fp, pname) ||
		 write_history(fp) ||
		 0 ) {
		goto quit;
	}

	morekeys = 80;
	fits_set_hdrsize(fp, morekeys, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_set_hdrsize(morekeys=%d) failed (%d)\n", pname, morekeys, istat);
		goto quit;
	}

	com.fp = fp;
	com.nrow = 0L;

	show_parameter("%s:  *** show parameter ***");
	anl_msg_info("\n");

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
SimASTE_PhotonFitsWrite_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_PhotonFitsWrite_bgnrun(int *status)
{
	EvsSet("SimASTE_PhotonFitsWrite:BEGIN");
	*status = ANL_OK;
}

void
SimASTE_PhotonFitsWrite_ana(int nevent, int eventid, int *status)
{
	int used, istat;
	double photon_time, energy, ra, dec;

	EvsfSetM("SimASTE_PhotonFitsWrite:ENTRY");

	/* put data to BNK */
	BnkfGetM("SimASTE:PHOTON_TIME", sizeof(photon_time), &used, &photon_time);
	BnkfGetM("SimASTE:PHOTON_ENERGY", sizeof(energy), &used, &energy);
	BnkfGetM("SimASTE:RA", sizeof(ra), &used, &ra);
	BnkfGetM("SimASTE:DEC", sizeof(dec), &used, &dec);

	com.nrow++;
	istat = 0;
	if (
fits_write_col_dbl(com.fp, 1, com.nrow, 1, 1, &photon_time, &istat) ||
fits_write_col_dbl(com.fp, 2, com.nrow, 1, 1, &energy, &istat) ||
fits_write_col_dbl(com.fp, 3, com.nrow, 1, 1, &ra, &istat) ||
fits_write_col_dbl(com.fp, 4, com.nrow, 1, 1, &dec, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_col_dbl() failed at irow=%ld (%d)\n", pname, com.nrow, istat);
		*status = ANL_QUIT;
		return;
	}

	EvsfSetM("SimASTE_PhotonFitsWrite:OK");
	*status = ANL_OK;
}

void
SimASTE_PhotonFitsWrite_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_PhotonFitsWrite_exit(int *status)
{
	char *k;
	int used, istat;
	double tstart, tstop, exposure;
	fitsfile *fp = com.fp;

	BnkGet("SimASTE:TSTART", sizeof(tstart), &used, &tstart);
	BnkGet("SimASTE:TSTOP", sizeof(tstop), &used, &tstop);
	BnkGet("SimASTE:EXPOSURE", sizeof(exposure), &used, &exposure);

	istat = 0;
	if (
fits_update_key_fixdbl(fp, k="TSTART", tstart, 9, NULL, &istat) ||
fits_update_key_fixdbl(fp, k="TSTOP", tstop, 9, NULL, &istat) ||
fits_update_key_fixdbl(fp, k="EXPOSURE", exposure, 9, NULL, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		goto quit;
	}

	if ( SimASTE_write_timestamp(fp) ||
		 SimASTE_write_random_number_info(fp) ) {
		goto quit;
	}

	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
		goto quit;
	}

	anl_msg_info("\
%s: %ld photons write to '%s'\n", pname, com.nrow, com.outfile);

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; c-basic-offset:4  ***
;;; End: ***
*/
