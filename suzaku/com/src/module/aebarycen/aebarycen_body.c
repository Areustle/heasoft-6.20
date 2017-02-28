/*
  aebarycen_body.c

	2006/03/06	Y.Terada(RIKEN), T.Enoto(UT)	version 0.1

	2006/03/08 modified by Y.Terada(RIKEN)		version 0.2
		debug in GTI extension, check TIMESYS=='TT',
        debug in modifing header

	2006/07/10	Y.ISHISAKI	version 0.3
		split axBary to another module
		do not modify DATE-OBS, TIME-OBS

	2006/07/12	Y.ISHISAKI	version 1.0
		use aefits_mission2mjd_tt() instead aste2mjd() in aebarycen_barycorr()
		change parameter names: infile -> filelist
		change parameter names: orbitfiles -> orbit, leapsec_name -> leapfile
		change parameter types (r -> s) : ra, dec
		add parameters: time_col, start_col, stop_col
		remove parameters: outfile, history, clobber
		alway write history
		support for CALDB

	2006/07/23	Y.ISHISAKI	version 1.1
		write task_name & task_version to history
		instead of pname & module_version in _init()
		show progress in aebarycen_modify_column()

	2006/07/24 Y.ISHISAKI	version 1.2
		further bug fix in parseRAstrToDeg() & parseDECstrToDeg()
		static declarations of parseRAstrToDeg() & parseDECstrToDeg()

	2006/07/28 Y.ISHISAKI	version 1.3
		use anl_msg_XXX() functions for error/warning/info messages
		do not change the value read (+/-360 deg) in parseRAstrToDeg()
		do not change the value read (+/-180 deg, etc), parseDECstrToDeg()
		strict range check of ra, dec in _init()
		read FITS key only when ra/dec start with A-Z or a-z in _read_header()
		CALDB access move from _com() -> _init()
		only WARNING in aebarycen_check_time_header(), aebarycen_read_header()
		increase digit .4 -> .6 to print (RA,DEC)
		delete old keys in adding new keys in process_hdu()
		write RA_TDB, DEC_TDB keywords in process_hdu()
		modify TIMESYS comment in aebarycen_modify_header()
		update DATE,DATASUM,CHECKSUM in process_hdu(),
		which has been moved from _modify_header()

	2006/07/31 Y.ISHISAKI	version 1.4
		change order of include file for aeFitsHeaderUtil-3.2
		aefits_mission2mjd_tt() -> aste2mjdtt() in astetool-1.80

	2007/05/14 Y.ISHISAKI	version 1.5
		use aste_caldb_find_leapfile()

	2008/03/03 Y.ISHISAKI	version 1.6
		check if TIME=0.0 and ignore it, before calling aebarycen_barycorr()
*/

#define NEED_LEAPFILE	/* aste_orbit() need leapfile, when old format */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "pil.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_orbit.h"
#ifdef NEED_LEAPFILE
#include "aste_time.h"
#endif
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"

#include "bary.h"

static char pname[] = "aebarycen";
char aebarycen_version[] = "version 1.6";

static struct {
	/** -- I/O -- **/
	char filelist[PIL_LINESIZE];
	char orbit[PIL_LINESIZE];
#ifdef NEED_LEAPFILE
	char *leapfile;			/* initialized as o_leapfile in _startup() */
	char o_leapfile[PIL_LINESIZE];
#endif
	char ra[PIL_LINESIZE];
	char dec[PIL_LINESIZE];
	char time_col[PIL_LINESIZE];
	char start_col[PIL_LINESIZE];
	char stop_col[PIL_LINESIZE];

	double target_ra, target_dec;
	int target_ra_from_key;
	int target_dec_from_key;
	int target_ra_found;	/* initialized as 0 in _init() */
	int target_dec_found;	/* initialized as 0 in _init() */
	double source_dir[3];
	ORBIT *orbitp;

	/** -- system -- **/
	int mjdrefi;
	double mjdreff;
	int time_unit;			/** Second or Day **/
	int file_num;
} com;

#define AEBARYCEN_TIME_UNIT_SEC		1
#define AEBARYCEN_TIME_UNIT_DAY		86400

#define MISSION_SUZAKU		0
#define MISSION_UNKNOWN		0

struct column_info {
	long nrow;
	int t_col;	/* TIME column number */
	int s_col;	/* START column number */
	int e_col;	/* STOP  column number */
};

/*
   parse R.A. string 'NNhNNmNN.Ns' into deg, where 0 <= deg < 360.0
*/
static double
parseRAstrToDeg(char *expression)
{
	char *p, *q;
	AtRightAscension ra;
	double deg;

	ra.hour = ra.min = 0;
	ra.sec = 0.0;
	p = q = expression;

	if ( '.' == *q ) { p = expression; goto skip; }
	while ( *q++ ) {
		if ( '.' == *q ) { p = expression; goto skip; }
		if ( '+' != *q && '-' != *q && ( *q < '0' || '9' < *q ) ) {
			ra.hour = atoi(p);
			p = q + 1;
			break;
		}
	}

	if ( '\0' == *(q-1) || '.' == *q ) { p = expression; goto skip; }
	while ( *q++ ) {
		if ( '.' == *q ) { p = expression; goto skip; }
		if ( '+' != *q && '-' != *q && ( *q < '0' || '9' < *q ) ) {
			ra.min = atoi(p);
			p = q + 1;
			break;
		}
	}

	if ( '\0' == *(q-1) ) { p = "0.0"; }

 skip:
	ra.sec = atof(p);

	if ( p == expression ) {
		deg = ra.sec;
	} else {
		deg = ra.hour*15.0 + 0.25 * (ra.min + ra.sec/60.0);
	}

#if 0
	while ( deg < 0.0 ) {
		deg += 360.0;
	}

	while ( 360.0 <= deg ) {
		deg -= 360.0;
	}
#endif

	return deg;
}

/*
   parse DEC. string 'NNdNNmNN.Ns' into deg, where -90 <= deg <= 90
*/
static double
parseDECstrToDeg(char *expression)
{
	char *p, *q;
	AtDeclination dec;
	double deg;

	dec.sign = 1;
	dec.deg = dec.min = 0;
	dec.sec = 0.0;

	p = q = expression;

	if ( '-' == *p ) {
		dec.sign = -1;
	}

	if ( '.' == *q ) { p = expression; goto skip; }
	while ( *q++ ) {
		if ( '.' == *q ) { p = expression; goto skip; }
		if ( '+' != *q && '-' != *q && ( *q < '0' || '9' < *q ) ) {
			dec.deg = abs(atoi(p));
			p = q + 1;
			break;
		}
	}

	if ( '\0' == *(q-1) || '.' == *q ) { p = expression; goto skip; }
	while ( *q++ ) {
		if ( '.' == *q ) { p = expression; goto skip; }
		if ( '+' != *q && '-' != *q && ( *q < '0' || '9' < *q ) ) {
			dec.min = atoi(p);
			p = q + 1;
			break;
		}
	}

	if ( '\0' == *(q-1) ) { p = "0.0"; }

 skip:
	dec.sec = atof(p);

	if ( p == expression ) {
		deg = dec.sec;
	} else {
		deg = dec.sign * (dec.deg + (dec.min + dec.sec/60) / 60.);
	}

#if 0
	while ( deg < -180.0 ) {
		deg += 180.0;
	}

	while ( +180.0 < deg ) {
		deg -= 180.0;
	}

	if ( deg < -90.0 ) {
		deg = - 180.0 - deg;	/* -91.0 -> -89.0 */
	}

	if ( +90.0 < deg ) {
		deg = 180.0 - deg;		/* 91.0 -> 89.0 */
	}
#endif

	return deg;
}

/*********************************************************************/
/************************ Bary Corerction Main ***********************/
/*********************************************************************/
static int
aebarycen_barycorr(double aetime, double *barytime)
{
	int istat;
	AtVect xyz, vxyz;
	double scposn[3];
	MJDTime mjd_time;
	double mjdtime;

	istat = aste_orbit(com.orbitp, aetime, xyz, vxyz);
	if ( istat ) {
		anl_msg_error("\
%s: aste_orbit(), no valid interval for t=%.0f\n", pname, aetime);
		return istat;
	}

	scposn[0] = xyz[0] * 1000.0; /** km --> m **/
	scposn[1] = xyz[1] * 1000.0;
	scposn[2] = xyz[2] * 1000.0;

	/*- calc TIME -*/
	mjdtime = aste2mjdtt(aetime, com.mjdrefi, com.mjdreff);
	mjd_time.ts = TT;			/** aetime is defined as TT. **/
	mjd_time.MJDint = (long) (mjdtime / 1.0);
	mjd_time.MJDfr  = mjdtime - (double) mjd_time.MJDint;

	*barytime = aetime;
	*barytime += barycorr(&mjd_time, com.source_dir, scposn) / com.time_unit;

	return 0;
}

/************************************************************************/
/*************************  static functions ****************************/
/************************************************************************/
static char *
get_file_name(char *filelist, int *file_num)
{
	static char fn[] = "/tmp/anl.XXXXXX";	/* must contain "." for CLI bug */
	static int orig_lun = 0;

	static char curr_file[1024];

	int fd, curr_lun, istat;
	FILE *fp;

/* check for nothing */

	if ( NULL == filelist || '\0' == *filelist ||
		 0 == CLstricmp("none", filelist) ) {
		return NULL;
	}

/* check direct file name */

	if ( '@' != *filelist ) {
		if ( *file_num ) {
			return NULL;
		}
		*file_num = *file_num + 1;	/* should not be *filenum++ */
		return filelist;
	}

/* @filelist */

	if ( 0 == *file_num ) {
		strcpy(fn+sizeof(fn)-7, "XXXXXX");	/* write back for previouse use */
		fd = mkstemp(fn);
		if ( -1 == fd || NULL == (fp = fdopen(fd, "w")) ) {
			anl_msg_error("\
%s: temporary file '%s' create failed\n", pname, fn);
			return NULL;
		}
		fprintf(fp, "@--\n%s\n\n", filelist);
		fclose(fp);
		istat = CLopnrd(fn);
		unlink(fn);
		if ( istat ) {
			anl_msg_error("\
%s: temporary file '%s' open failed\n", pname, fn);
			return NULL;
		}
		CLilun(&orig_lun);
		goto entry;
	}

	if ( 0 == orig_lun ) {
		return NULL;
	}

 again:

	CLilun(&curr_lun);
	if ( curr_lun == orig_lun ) {	/* end of file */
		orig_lun = 0;
		return NULL;
	}

 entry:
	*curr_file = '\0';
	CLtxtrd("?File", curr_file, sizeof(curr_file));
	if ( '\0' == *curr_file ) {
		goto again;
	}

	*file_num = *file_num + 1;		/* should not be *filenum++ */

	return curr_file;
}

static int
aebarycen_check_extension(fitsfile *fp, struct column_info *cp)
{
	int casesen = FALSE;

	int istat = 0;

/**** search NAXIS2 ****/
	fits_read_key_lng(fp, "NAXIS2", &cp->nrow, NULL, &istat);
	if ( istat ) {
		cp->nrow = 0L;
		cp->t_col = cp->s_col = cp->e_col = 0;
	}
	if ( cp->nrow <= 0L ) {
		return 0;
	}

/**** search TIME ****/
	fits_get_colnum(fp, casesen, com.time_col, &cp->t_col, &istat);
	if ( istat ) {
		cp->t_col = 0;
		istat = 0;
	}

/**** search GTI ****/
	fits_get_colnum(fp, casesen, com.start_col, &cp->s_col, &istat);
	if ( istat ) {
		cp->s_col = 0;
		istat = 0;
	}

	fits_get_colnum(fp, casesen, com.stop_col, &cp->e_col, &istat);
	if ( istat ) {
		cp->e_col = 0;
		istat = 0;
	}

	return 0;
}

static int
aebarycen_check_time_header(fitsfile *fp)
{
	char timeref[PIL_LINESIZE];
	char timesys[PIL_LINESIZE];
	int istat = 0;

	fits_read_key_str(fp, "TIMEREF", timeref, NULL, &istat);
	if ( istat ) {
		anl_msg_warning("\
%s: WARNING: fits_read_key('TIMEREF') failed (%d)\n", pname, istat);
		return istat;
	}

	if ( 0 == CLstricmp(timeref, "SOLARSYSTEM") ) {
		anl_msg_warning("\
%s: WARNING: TIMEREF is already SOLARSYSTEM\n",pname);
		return -1;
	}

	fits_read_key_str(fp, "TIMESYS", timesys, NULL, &istat);
	if ( istat ) {
		anl_msg_warning("\
%s: WARNING: fits_read_key_str('TIMESYS') failed (%d)\n", pname, istat);
		return istat;
	}

	if ( 0 != strcmp(timesys, "TT") &&
	     0 != strcmp(timesys, "TDT") &&
	     0 != strcmp(timesys, "TAI") &&
	     0 != strcmp(timesys, "ET") &&
	     0 != strcmp(timesys, "UTC") ) {
		anl_msg_warning("\
%s: WARNING: TIMESYS is unknown, '%s'\n", pname, timesys);
		return -1;
	}

	if ( 0 == strcmp(timesys, "TDB") ||
		 0 == strcmp(timesys, "TCB") ) {
		anl_msg_warning("\
%s: WARNING: TIMESYS is already %s\n", pname, timesys);
		return -1;
	}

	if ( 0 != strcmp(timesys, "TT") ) {
		anl_msg_warning("\
%s: WARNING: TIMESYS='%s' is not supported\n", pname, timesys);
		return -1;
	}

	return 0;
}

static int
aebarycen_read_header(
	fitsfile *fp, char *telescop, char *timesys,
	int *mjdrefi, double *mjdreff,
	char *timeunit, char *radecsys)
{
	char *k;
	int istat = 0;

	if (
fits_read_key_str(fp, k="TELESCOP", telescop, NULL, &istat) ||
fits_read_key_str(fp, k="TIMESYS", timesys, NULL, &istat) ||
fits_read_key(fp, TINT, k="MJDREFI", mjdrefi, NULL, &istat) ||
fits_read_key_dbl(fp, k="MJDREFF", mjdreff, NULL, &istat) ||
fits_read_key_str(fp, k="TIMEUNIT", timeunit, NULL, &istat) ||
fits_read_key_str(fp, k="RADECSYS", radecsys, NULL, &istat) ||
		 0 ) {
		anl_msg_warning("\
%s: WARNING: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		return -1;
	}

/** check RA,DEC **/
	if ( com.target_ra_from_key ) {
		fits_read_key_dbl(fp, com.ra, &com.target_ra, NULL, &istat);
		if ( istat ) {
			istat = 0;	/* ignore error */
			if ( com.target_ra_found ) {
				anl_msg_warning("\
%s: WARNING: no '%s' key found, using previous one\n", pname, com.ra);
			} else {
				com.target_ra_found = 1;
			}
		}
	}

	if ( com.target_dec_from_key ) {
		fits_read_key_dbl(fp, com.dec, &com.target_dec, NULL, &istat);
		if ( istat ) {
			istat = 0;	/* ignore error */
			if ( com.target_dec_found ) {
				anl_msg_warning("\
%s: WARNING: no '%s' key found, using previous one\n", pname, com.dec);
			} else {
				com.target_dec_found = 1;
			}
		}
	}

	anl_msg_info("\
(RA,DEC)= (%s,%s) = ( %.6f , %.6f )\n",
		com.ra, com.dec, com.target_ra, com.target_dec);

/** calc Source Dir **/
	com.source_dir[0] = cos(com.target_ra/RADEG) * cos(com.target_dec/RADEG) ;
	com.source_dir[1] = sin(com.target_ra/RADEG) * cos(com.target_dec/RADEG) ;
	com.source_dir[2] = sin(com.target_dec/RADEG) ;

	return 0;
}

static int
aebarycen_write_history(fitsfile *fp, int *stat)
{
	char *task_name, *task_version, history[PIL_LINESIZE];
	int istat = 0;

/** task name & version **/
	task_name = anl_task_name();
	task_version = anl_task_version();
	istat = aefits_write_name_vers(fp, task_name, task_version);
	if ( istat ) goto error;

/** filelist **/
	sprintf(history, "  filelist='%s'", com.filelist);
	fits_write_history(fp, history, &istat);
	if ( istat ) goto error;

/** orbit **/
	sprintf(history, "  orbit='%s'", com.orbit);
	fits_write_history(fp, history, &istat);
	if ( istat ) goto error;

#ifdef NEED_LEAPFILE
/** leapfile **/
	if ( com.leapfile == com.o_leapfile ) {
		sprintf(history, "  leapfile='%s'", com.leapfile);
	} else {
		sprintf(history, "  leapfile='%s' (%s)", com.leapfile, com.o_leapfile);
	}
	fits_write_history(fp, history, &istat);
	if ( istat ) goto error;
#endif

/** ra, dec **/
	sprintf(history, "  (RA,DEC) = (%s,%s) = ( %.6f , %.6f )",
		com.ra, com.dec, com.target_ra, com.target_dec);
	fits_write_history(fp, history, &istat);
	if ( istat ) goto error;

/** time_col, start_col, stop_col **/
	sprintf(history, "  time_col='%s', start_col='%s', stop_col='%s'",
		com.time_col, com.start_col, com.stop_col);
	fits_write_history(fp, history, &istat);
	if ( istat ) goto error;

	return istat;

 error:
	anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
	return istat;
}

static int
aebarycen_print_key(fitsfile *fp, char *key)
{
	char card[FLEN_CARD];

	int istat = 0;

	fits_read_card(fp, key, card, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, key, istat);
		return istat;
	}

	anl_msg_info("%s\n", card);
	return 0;
}

static int
aebarycen_modify_header(fitsfile *fp)
{
	char *k, comment[FLEN_CARD];
	double tstart, tstop;
	double barytstart, barytstop;

	int istat = 0;
	int key_exist = 0;

/** modify TIMESYS **/
	fits_update_key_str(fp, k="TIMESYS", "TDB", "\
Time System (TDB: Barycentric Dynamical Time)", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}
	aebarycen_print_key(fp, k);

/** modify TIMEREF **/
	fits_update_key_str(fp, k="TIMEREF", "SOLARSYSTEM", "\
Times are pathlength-corrected to barycenter", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}
	aebarycen_print_key(fp, k);

/** modify TSTART **/
	if ( ! fits_test_keyword(k="TSTART", &key_exist) ) {
		fits_read_key_dbl(fp, k, &tstart, NULL, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
			return istat;
		}

		if ( 0.0 == tstart ) {
			anl_msg_warning("\
%s: WARNING: TSTART=0.0 ignored\n", pname);
		} else {
			istat = aebarycen_barycorr(tstart, &barytstart);
			if ( istat ) {
				anl_msg_error("\
%s: barycentric correction of %s failed (%d)\n", pname, k, istat);
				return istat;
			}

			sprintf(comment, "\
time start   (%+11.6f s barycen corrected)", barytstart - tstart);
			fits_modify_key_fixdbl(fp, k, barytstart, 6, comment, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, k, istat);
				return istat;
			}
			aebarycen_print_key(fp, k);
		}
	} else {
		key_exist = 0;
	}

/** modify TSTOP **/
	if ( ! fits_test_keyword(k="TSTOP", &key_exist) ) {
		fits_read_key_dbl(fp, k, &tstop, NULL, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
			return istat;
		}

		if ( 0.0 == tstop ) {
			anl_msg_warning("\
%s: WARNING: TSTOP=0.0 ignored\n", pname);
		} else {
			istat = aebarycen_barycorr(tstop, &barytstop);
			if ( istat ) {
				anl_msg_error("\
%s: barycentric correction of %s failed (%d)\n", pname, k, istat);
				return istat;
			}

			sprintf(comment, "\
time stop    (%+11.6f s barycen corrected)", barytstop - tstop);
			fits_modify_key_fixdbl(fp, k, barytstop, 6, comment, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, k, istat);
				return istat;
			}
			aebarycen_print_key(fp, k);
		}
	} else {
		key_exist = 0;
	}

	return istat;
}

/*********************************************************************/
/********************* Bary Corerction Functions *********************/
/*********************************************************************/
static int
aebarycen_modify_column(fitsfile *fp, struct column_info *cp)
{
	int i, anul, icol, cols[3];
	double aetime, barytime;
	long irow, nrow, progress, last_progress;

	int istat = 0;

	cols[0] = cp->t_col;
	cols[1] = cp->s_col;
	cols[2] = cp->e_col;

	progress = last_progress = 1;
	nrow = cp->nrow;

	for (irow = 1; irow <= nrow; irow++) {
		if ( 10000 < nrow ) {
			if ( irow == progress * nrow / 10 ) {
				anl_msg_info("...%ld0%%", progress);
				progress++;
			}
		}
		for (i = 0; i < 3; i++) {
			icol = cols[i];
			if ( icol <= 0 ) continue;

/*- read TIME -*/
fits_read_col_dbl(fp, icol, irow, 1, 1, 0.0, &aetime, &anul, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_read_col_dbl('TIME') failed, irow=%ld (%d)\n", pname, irow, istat);
				return istat;
			}

/*- Time Correction -*/
			if ( 0.0 == aetime ) {
				if ( 10000 < nrow && last_progress < progress ) {
					last_progress = progress;
					anl_msg_info("\n");
				}
				anl_msg_warning("\
%s: WARNING: TIME=0.0 at irow=%ld ignored\n", pname, irow);
				barytime = aetime;
			} else {
				istat = aebarycen_barycorr(aetime, &barytime);
				if ( istat ) {
					anl_msg_error("\
%s: TIME correction failed, irow=%ld\n",  pname, irow);
					return istat;
				}

/*- write TIME -*/
fits_write_col_dbl(fp, icol, irow, 1, 1, &barytime, &istat);
				if (istat){
					anl_msg_error("\
%s: fits_write_col('TIME') failed, irow=%ld (%d)\n", pname, irow, istat);
					return istat;
				}
			}
		}
	}

	if ( 10000 < nrow ) {
		anl_msg_info("\n");
	}

	return 0;
}

static int
process_hdu(fitsfile *fp)
{
/*** read Header and Initialize ***/
	char telescop[FLEN_KEYWORD];
	int  mission;
	char timesys[FLEN_KEYWORD];
	char *fromts = "TDB";	/** TDB: Barycentric Dynamical Time,
							    TCB: Barycentric Coordinate Time */
	char timeunit[FLEN_KEYWORD];
	int ephnum;				/** Ephemeris number requested (200,405,0) **/
	char radecsys[FLEN_KEYWORD];	/** FK5 (200) or ICRS (405) **/
	int denum;						/** JPL Planetary Ephemeris DE number **/
	char *k;
	char plephem[FLEN_KEYWORD];

	struct column_info ci;

	int istat = 0;

	istat = aebarycen_check_time_header(fp);
	if ( istat ) {
		return istat;
	}

	istat = aebarycen_check_extension(fp, &ci);
	if ( istat ) {
		return istat;
	}
	anl_msg_info("\
NAXIS2=%ld, %s=%d, %s=%d, %s=%d\n", ci.nrow, com.time_col, ci.t_col,
		com.start_col, ci.s_col, com.stop_col, ci.e_col);

	istat = aebarycen_read_header(fp,
		telescop, timesys, &com.mjdrefi, &com.mjdreff, timeunit, radecsys);
	if ( istat ) {
		return istat;
	}

	if ( 0 == CLstricmp(telescop, "SUZAKU") ) {
		mission = MISSION_SUZAKU;
	} else {
		mission = MISSION_UNKNOWN; /** Suzaku is not defined in bary.h **/
	}

	if ( 0 == CLstricmp(radecsys, "FK5") ) {
		ephnum = 200;	/* PLEPHEM='JPL-DE200' */
	} else if ( 0 == CLstricmp(radecsys, "ICRS") ) {
		ephnum = 405;	/* PLEPHEM='JPL-DE405' */
	} else {
		ephnum = 0;
	}

	if ( 0 == CLstricmp(timeunit, "s") ) {
		com.time_unit = AEBARYCEN_TIME_UNIT_SEC;
	} else if ( 0 == CLstricmp(timeunit,"d") ) {
		com.time_unit = AEBARYCEN_TIME_UNIT_DAY;
	} else {
		anl_msg_error("\
%s: TIMEUNIT is undefined (%s)\n",  pname, timeunit);
		return -1;
	}

/** initialize  barycentric correction function **/
	denum = baryinit(mission, timesys, fromts,
		(double)com.mjdrefi, com.mjdreff, timeunit, ephnum);
	if ( denum ) {
/*      fprintf(stdout, "%s: JPL Planetary Ephemeris DE number %d\n",
					pname, denum); */
	} else {
		anl_msg_error("\
%s: error in baryinit()\n", pname);
		return -1;
	}

/*** modify 'TIME' or 'GTIs' ***/
	istat = aebarycen_modify_column(fp, &ci);
	if ( istat ) {
		return istat;
	}

/** modify existing time keywords **/
	istat = aebarycen_modify_header(fp);
	if( istat ) {
		return istat;
	}

/*** write history ***/
	istat = aebarycen_write_history(fp, &istat);
	if ( istat ) {
		return istat;
	}

/** add new keyword **/
	k = "PLEPHEM";
	fits_delete_key(fp, k, &istat);
	istat = 0;	/* ignore error */
	sprintf(plephem, "JPL-DE%03d", denum);
	fits_write_key_str(fp, k, plephem, "\
Solar System ephemeris used for baryctr corr.", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key_str('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}
	aebarycen_print_key(fp, k);

	k = "RA_TDB";
	fits_delete_key(fp, k, &istat);
	istat = 0;	/* ignore error */
	fits_write_key_fixdbl(fp, k, com.target_ra, 6, "\
Target RA of Barycentric Dynamical Time corr.", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}
	aebarycen_print_key(fp, k);

	k = "DEC_TDB";
	fits_delete_key(fp, k, &istat);
	istat = 0;	/* ignore error */
	fits_write_key_fixdbl(fp, k, com.target_dec, 6, "\
Target DEC of Barycentric Dynamical Time corr.", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}
	aebarycen_print_key(fp, k);

/** update DATE **/
	fits_write_date(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_date() failed (%d)\n", pname, istat);
		return istat;
	}
	aebarycen_print_key(fp, "DATE");

/** update CHKSUM **/
	fits_write_chksum(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n",  pname, istat);
		return istat;
	}
	aebarycen_print_key(fp, "DATASUM");
	aebarycen_print_key(fp, "CHECKSUM");

	return 0;
}

static int
process_file(char *file)
{
	char *msg;
	int hdunum, hdutype;
	fitsfile *fp;

	int istat = 0;

	fits_open_file(&fp, file, READWRITE, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, file, istat);
		return istat;
	}

	fits_get_hdu_num(fp, &hdunum);
	if ( 1 != hdunum ) {
		istat = process_hdu(fp);
		msg = istat ? "SKIP" : "DONE";
		anl_msg_info("\
      [%d] %s\n", hdunum, msg);
	} else {
		for (;;) {
			istat = process_hdu(fp);
			msg = istat ? "SKIP" : "DONE";
			anl_msg_info("\
      [%d] %s\n", hdunum, msg);
			istat = 0;	/* ignore error */
			hdunum++;
			fits_movrel_hdu(fp, 1, &hdutype, &istat);
			if ( istat ) {
				break;
			}
		}
	}

	istat = 0;
	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, file, istat);
		return istat;
	}

	return istat;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "FILELIST", com.filelist);
	printf("%20s   '%s'\n", "ORBIT", com.orbit);
#ifdef NEED_LEAPFILE
	if ( com.leapfile == com.o_leapfile ) {
		printf("%20s   '%s'\n", "LEAPFILE", com.leapfile);
	} else {
		printf("%20s   '%s' (%s)\n", "LEAPFILE", com.leapfile, com.o_leapfile);
	}
#endif
	printf("%20s   '%s'\n", "RA", com.ra);
	printf("%20s   '%s'\n", "DEC", com.dec);
	printf("%20s   '%s'\n", "TIME_COL", com.time_col);
	printf("%20s   '%s'\n", "START_COL", com.start_col);
	printf("%20s   '%s'\n", "STOP_COL", com.stop_col);
}

/* ----------------------------------------------------------
 * start up
 * ----------------------------------------------------------*/
void
aebarycen_startup(int *status)
{
#ifdef NEED_LEAPFILE
	com.leapfile = com.o_leapfile;
#endif

	*status = ANL_OK;
}


/* ----------------------------------------------------------
 *  com
 * ----------------------------------------------------------*/
void
aebarycen_com(int *status)
{
	char *k;

/************ ftools *************/
	if ( *status ) {
		if (
PILGetFname(k="filelist", com.filelist) ||
PILGetFname(k="orbit", com.orbit) ||
#ifdef NEED_LEAPFILE
PILGetFname(k="leapfile", com.o_leapfile) ||
#endif
PILGetString(k="ra", com.ra) ||
PILGetString(k="dec", com.dec) ||
PILGetString(k="time_col", com.time_col) ||
PILGetString(k="start_col", com.start_col) ||
PILGetString(k="stop_col", com.stop_col) ||
			 0 ) {
			anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
			*status = ANL_QUIT;
			return;
		}
	}

	*status = ANL_OK;
}

/* ----------------------------------------------------------
 * init
 * ----------------------------------------------------------*/
void
aebarycen_init(int *status)
{
	int istat;

#ifdef NEED_LEAPFILE
	int verbose;

	com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
	if ( NULL == com.leapfile ) {
		goto quit;
	}
#endif

	show_parameter();
	anl_msg_info("\n");

/** check RA **/
	com.target_ra_from_key = 0;
	com.target_ra_found = 0;

	if ( '\0' == *com.ra ) {
		anl_msg_error("\
%s: ERROR: empty RA parameter\n", pname);
		goto quit;
	} else if ( ('A' <= *com.ra && *com.ra <= 'Z') ||
			    ('a' <= *com.ra && *com.ra <= 'z') ) {
		com.target_ra_from_key = 1;
	} else {
		com.target_ra = parseRAstrToDeg(com.ra);
		if ( com.target_ra < 0.0 || 360.0 <= com.target_ra ) {
			anl_msg_error("\
%s: ERROR: invalid RA range, should be 0.0 <= RA < 360.0\n", pname);
			goto quit;
		}
	}

/** check DEC **/
	com.target_dec_from_key = 0;
	com.target_dec_found = 0;

	if ( '\0' == *com.dec ) {
		anl_msg_error("\
%s: ERROR: empty DEC parameter\n", pname);
		goto quit;
	} else if ( ('A' <= *com.dec && *com.dec <= 'Z') ||
			    ('a' <= *com.dec && *com.dec <= 'z') ) {
		com.target_dec_from_key = 1;
	} else {
		com.target_dec = parseDECstrToDeg(com.dec);
		if ( com.target_dec < -90.0 || 90.0 < com.target_dec ) {
			anl_msg_error("\
%s: ERROR: invalid DEC range, should be -90.0 <= DEC < +90.0\n", pname);
			goto quit;
		}
	}

#ifdef NEED_LEAPFILE
	verbose = ( 0 == CLstricmp("none", com.leapfile) ) ? -1 : -2;
	if ( NULL == atMissionTimeInit(com.leapfile, verbose) ) {
		anl_msg_error("\
%s: atMissionTimeInit('%s') failed\n", pname, com.leapfile);
		goto quit;
	}
	if ( -1 == verbose ) printf("\n");
#endif

/** initialize Suzaku Orbit file fuction **/
	istat = aste_orbit_init(&com.orbitp, com.orbit);
	if(istat){
		anl_msg_error("\
%s: aste_orbit_init() error, (%d)\n", pname, istat);
		goto quit;
	}

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

/* ----------------------------------------------------------
 *  histogram (NOT USED)
 * ----------------------------------------------------------*/
void
aebarycen_his(int *status)
{
	*status = ANL_OK;
}

/* ----------------------------------------------------------
 *  begin run
 * ----------------------------------------------------------*/
void
aebarycen_bgnrun(int *status)
{
	*status = ANL_OK;
}

/* ----------------------------------------------------------
 *  analysis (par Extension)
 * ----------------------------------------------------------*/
void
aebarycen_ana(int *nevent, int *eventid, int *status)
{
	char *file;

	file = get_file_name(com.filelist, &com.file_num);
	if ( NULL == file ) {
		fflush(NULL);
		if ( 0 != com.file_num ) {
			anl_msg_info("\n");
		}
		anl_msg_info("\
Finished.\n");
		fflush(NULL);
		*status = ANL_QUIT;
		return;
	}

	anl_msg_info("\n\
 [%2d] Processing '%s'\n", com.file_num, file);

	process_file(file);

	*status = ANL_OK;
}

/* ----------------------------------------------------------
 *  end run
 * ----------------------------------------------------------*/
void
aebarycen_endrun(int *status)
{
	*status = ANL_OK;
}

/* ----------------------------------------------------------
 *  exit
 * ----------------------------------------------------------*/
void
aebarycen_exit(int *status)
{
	*status = ANL_OK;
}
