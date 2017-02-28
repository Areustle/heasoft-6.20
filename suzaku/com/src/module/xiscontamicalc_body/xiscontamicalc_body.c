/*
	xiscontamicalc.c

	2006/05/24 Y.ISHISAKI	version 1.0

	2006/05/29 Y.ISHISAKI	version 1.1
		renamed xiscontami -> xiscontamicalc

	2006/07/24 Y.ISHISAKI	version 1.2
		add telescop, instrume parameters for CALDB
		support for CALDB

    2006/08/04 Y.ISHISAKI	version 2.0
		add arffile, scale_contaminant, scale_previous, scale_factor parameters
		use aefits_datestr2attimeD/attimeD2datestr() in aeFitsHeaderUtil-3.2
		change radius -> r_offset

    2006/10/17 Y.ISHISAKI	version 2.1
		handle carbon, oxygen contamination separately, using xis_contami-1.2
		add carbon, oxygen parameters, remove contami parameter
		skip initialization, history when scale_previous=yes
		use aste_caldb_find_leapfile() for leapfile
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "fitsio.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_time.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"
#include "xis_contami.h"

static char pname[] = "xiscontamicalc";
char xiscontamicalc_version[] = "version 2.1";

static struct {
/* xiscontami */
	char telescop[PIL_LINESIZE];
	char instrume[PIL_LINESIZE];
	char *contamifile, o_contamifile[PIL_LINESIZE];
	char *leapfile, o_leapfile[PIL_LINESIZE];
	char date_obs[PIL_LINESIZE];
	char date0[PIL_LINESIZE];
	double energy;
	double detx;
	double dety;
	char arffile[PIL_LINESIZE];
	int scale_contaminant;
	int scale_previous;
	double scale_factor;

	double aetime;
	double aetime0;
	AtTimeD attime;
	AtTimeD attime0;
	double r_offset, days, carbon, oxygen, transmis;
	XIS_CONTAMI *xcp;
} com;

int
scale_arffile(char *arffile)
{
	char instrume[FLEN_VALUE], history[PIL_LINESIZE + FLEN_VALUE];
	char *k, *task_name, *task_version, *comment;
	int cl_energ_lo, cl_energ_hi, cl_specresp, cl_contami;
	int i, n, hdunum, anul, flag_contami_prev;
	double contami0, specresp0;
	double energy, carbon, oxygen, transmis;
	double *energ_lo, *energ_hi, *specresp, *contami;
	fitsfile *fp;
	int istat, istat2;

	istat = 0;

	anl_msg_info("\
%s: reading arffile '%s' ...\n", pname, arffile);

	fp = NULL;
	fits_open_file(&fp, arffile, READWRITE, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, arffile, istat);
		goto error;
	}

	fits_get_hdu_num(fp, &hdunum);
	if ( 1 == hdunum ) {
		fits_movnam_hdu(fp, BINARY_TBL, k="SPECRESP", 0, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_movnam_hdu('%s') failed (%d)\n", pname, k, istat);
			goto error;
		}
	}

	fits_read_key_str(fp, k="INSTRUME", instrume, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		goto error;
	}

	if ( 0 != CLstricmp(instrume, com.instrume) ) {
		anl_msg_error("\
%s: instrument name mismatch ('%s' != '%s')\n", pname, instrume, com.instrume);
		istat = -1;
		goto error;
	}

	fits_read_key(fp, TINT, k="NAXIS2", &n, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		goto error;
	}

	if (
fits_get_colnum(fp, CASEINSEN, k="ENERG_LO", &cl_energ_lo, &istat) ||
fits_get_colnum(fp, CASEINSEN, k="ENERG_HI", &cl_energ_hi, &istat) ||
fits_get_colnum(fp, CASEINSEN, k="SPECRESP", &cl_specresp, &istat) ||
fits_get_colnum(fp, CASEINSEN, k="CONTAMI_TRANSMIS", &cl_contami, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		goto error;
	}

	energ_lo = malloc( 4 * n * sizeof(*specresp) );
	energ_hi = &energ_lo[n];
	specresp = &energ_hi[n];
	contami  = &specresp[n];
	if ( NULL == energ_lo ) {
		anl_msg_error("\
%s: malloc( energ_lo[n=%d] ) failed\n", pname, n);
		istat = -1;
		goto error;
	}

	fits_read_col_dbl(fp, cl_energ_lo, 1, 1, n, 0.0, energ_lo, &anul, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_col_dbl('ENERG_LO') failed (%d)\n", pname, istat);
		goto error;
	}
	fits_read_col_dbl(fp, cl_energ_hi, 1, 1, n, 0.0, energ_hi, &anul, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_col_dbl('ENERG_HI') failed (%d)\n", pname, istat);
		goto error;
	}
	fits_read_col_dbl(fp, cl_specresp, 1, 1, n, 0.0, specresp, &anul, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_col_dbl('SPECRESP') failed (%d)\n", pname, istat);
		goto error;
	}
	fits_read_col_dbl(fp, cl_contami,  1, 1, n, 0.0,  contami, &anul, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_read_col_dbl('CONTAMI_TRANSMIS') failed (%d)\n", pname, istat);
		goto error;
	}

	flag_contami_prev = 0;
	for (i = 0; i < n; i++) {
		if ( 1.0 != contami[i] ) {
			flag_contami_prev = 1;
			break;
		}
	}

	if ( 0.0 == com.scale_factor ) {
		if ( ! flag_contami_prev ) {
			anl_msg_warning("\
%s: WARNING: arffile does not include contamination.\n\
%s: WARNING: nothing to do with scale_factor=0.0\n", pname, pname);
			goto skip;
		}
		anl_msg_warning("\
%s: WARNING: scale_factor=0.0\n\
%s: WARNING: resultant arffile include no contamination.\n", pname, pname);
		for (i = 0; i < n; i++) {
			specresp[i] = (0.0 == contami[i]) ? 0.0 : specresp[i] / contami[i];
			contami[i] = 1.0;
		}
	} else if ( com.scale_previous ) {
		if ( ! flag_contami_prev ) {
			anl_msg_warning("\
%s: WARNING: arffile does not include contamination.\n\
%s: WARNING: nothing to do with scale_previous=yes\n", pname, pname);
			goto skip;
		} else if ( 1.0 == com.scale_factor ) {
			anl_msg_warning("\
%s: WARNING: scale_factor=1.0\n\
%s: WARNING: nothing to do with scale_previous=yes\n", pname, pname);
			goto skip;
		}
		for (i = 0; i < n; i++) {
			contami0 = contami[i];
			specresp0 = ( 0.0 == contami0 ) ? 0.0 : specresp[i] / contami0;
			contami[i] = pow(contami0, com.scale_factor);
			specresp[i] = specresp0 * contami[i];
		}
	} else {
		if ( flag_contami_prev ) {
			anl_msg_warning("\
%s: WARNING: arffile already includes contamination.\n\
%s: WARNING: contamination is reset to new value\n", pname, pname);
		}
		for (i = 0; i < n; i++) {
			energy = (energ_lo[i] + energ_hi[i]) / 2.0;
			transmis = xis_contami(com.xcp,
				energy, com.aetime, com.detx, com.dety, &carbon, &oxygen);
			contami0 = contami[i];
			specresp0 = ( 0.0 == contami0 ) ? 0.0 : specresp[i] / contami0;
			contami[i] = pow(transmis, com.scale_factor);
			specresp[i] = specresp0 * contami[i];
		}
	}

	anl_msg_info("\
%s: overwriting arffile ...\n", pname);

	fits_write_col_dbl(fp, cl_specresp, 1, 1, n, specresp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_col_dbl('SPECRESP') failed (%d)\n", pname, istat);
		goto error;
	}
	fits_write_col_dbl(fp, cl_contami,  1, 1, n, contami,  &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_col_dbl('CONTAMI_TRANSMIS') failed (%d)\n", pname, istat);
		goto error;
	}

	task_name = anl_task_name();
	task_version = anl_task_version();

	istat = aefits_write_name_vers(fp, task_name, task_version);
	if ( istat ) {
		return istat;
	}

/* update history */
	if ( com.scale_previous ) {
		goto skip_history;
	}

	sprintf(history, "\
  telescop='%s'  instrume='%s'", com.telescop, com.instrume);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  contamifile='%s'%s", com.contamifile,
		(com.contamifile == com.o_contamifile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  leapfile='%s'%s", com.leapfile,
		(com.leapfile == com.o_leapfile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  date_obs='%s'  time=%.1f", com.date_obs, com.aetime);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  days=%.6f  date0='%s'", com.days, com.date0);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  (detx, dety)=(%.1f, %.1f) pixel  r_offset=%.3f arcmin",
		com.detx, com.dety, com.r_offset);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  carbon=%.6f x 10**18 cm**-2", com.carbon);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  oxygen=%.6f x 10**18 cm**-2", com.oxygen);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  energy=%.3f keV  transmis=%.6e", com.energy, com.transmis);
	fits_write_history(fp, history, &istat);

 skip_history:
	sprintf(history, "\
  arffile='%s'", com.arffile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  scale_contaminant=%s  scale_previous=%s  scale_factor=%.6f",
		com.scale_contaminant ? "yes" : "no",
		com.scale_previous ? "yes" : "no",
		com.scale_factor);
	fits_write_history(fp, history, &istat);
	fits_write_history(fp, " ", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		goto error;
	}

/* update keywords */
	comment = history;
	if ( com.scale_previous ) {
		fits_read_key_dbl(fp, "CARBON", &carbon, NULL, &istat);
		if ( istat ) {
			istat = 0;	/* ignore error */
			anl_msg_warning("\
%s: WARNING: cannot get previous value of CARBON\n", pname);
		} else {
			aefits_del_write_key_fixdbl(fp, k="CARBON",
				carbon * com.scale_factor, 12, "\
carbon column scaled to prev. in 10**18 cm**-2", &istat);
			if ( istat ) goto write_key_error;
		}
		fits_read_key_dbl(fp, "OXYGEN", &oxygen, NULL, &istat);
		if ( istat ) {
			istat = 0;	/* ignore error */
			anl_msg_warning("\
%s: WARNING: cannot get previous value of OXYGEN\n", pname);
		} else {
			aefits_del_write_key_fixdbl(fp, k="OXYGEN",
				oxygen * com.scale_factor, 12, "\
oxygen column scaled to prev. in 10**18 cm**-2", &istat);
			if ( istat ) goto write_key_error;
		}
	} else {
		aefits_del_write_key(fp, TSTRING, k="CONTAMIF",
			aefits_basename(com.contamifile), "\
name of contamination file", &istat);
		if ( istat ) goto write_key_error;
		aefits_del_write_key(fp, TSTRING, k="LEAPFILE",
			aefits_basename(com.leapfile), "\
name of the leap second file", &istat);
		if ( istat ) goto write_key_error;
		sprintf(comment, "\
date of observations (UTC), t=%.1f", com.aetime);
		aefits_del_write_key(fp, TSTRING, k="DATE-OBS",
			com.date_obs, comment, &istat);
		if ( istat ) goto write_key_error;
		sprintf(comment, "\
days after %s", com.date0);
		aefits_del_write_key_fixdbl(fp, k="DAYS",
			com.days, 6, comment, &istat);
		if ( istat ) goto write_key_error;
		aefits_del_write_key_fixdbl(fp, k="DETX", com.detx, 1, "\
DETX location in pixel", &istat);
		if ( istat ) goto write_key_error;
		aefits_del_write_key_fixdbl(fp, k="DETY", com.dety, 1, "\
DETY location in pixel", &istat);
		if ( istat ) goto write_key_error;
		aefits_del_write_key_fixdbl(fp, k="R_OFFSET", com.r_offset, 3, "\
radial offset in arcmin", &istat);
		if ( istat ) goto write_key_error;
		if ( com.scale_previous ) {
			;
		} else {

			if ( 1.0 != com.scale_factor ) {
				anl_msg_always("\
CARBON = %.6f [10**18 cm**-2] (scaled by %.6f)\n",
					carbon * com.scale_factor, com.scale_factor);
				sprintf(comment, "\
carbon column x %.6f in 10**18 cm**-2", com.scale_factor);
			} else {
				sprintf(comment, "\
carbon column in 10**18 cm**-2");
			}
			aefits_del_write_key_fixdbl(fp, k="CARBON",
				carbon * com.scale_factor, 12, comment, &istat);
			if ( istat ) goto write_key_error;

			if ( 1.0 != com.scale_factor ) {
				anl_msg_always("\
OXYGEN = %.6f [10**18 cm**-2] (scaled by %.6f)\n",
					oxygen * com.scale_factor, com.scale_factor);
				sprintf(comment, "\
oxygen column x %.6f in 10**18 cm**-2", com.scale_factor);
			} else {
				sprintf(comment, "\
oxygen column in 10**18 cm**-2");
			}
			aefits_del_write_key_fixdbl(fp, k="OXYGEN",
				oxygen * com.scale_factor, 12, comment, &istat);
			if ( istat ) goto write_key_error;
		}
	}

 skip:

	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed (%d)\n", pname, istat);
		return istat;
	}

	anl_msg_info("\
%s: finished\n", pname);

	return 0;

 write_key_error:
	anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
	goto error;

 error:
	istat2 = 0;
	if ( NULL != fp ) {
		fits_close_file(fp, &istat2);
	}
	return istat;
}

void
xiscontamicalc_startup(int *status)
{
	com.xcp = NULL;
	com.contamifile = com.o_contamifile;
	com.leapfile = com.o_leapfile;

	*status = ANL_OK;
}

void
xiscontamicalc_com(int *status)
{
	char *k;

	if ( *status ) {	/* ftools */
		if ( PILGetString(k="telescop", com.telescop) ||
			 PILGetString(k="instrume", com.instrume) ||
			 PILGetFname (k="contamifile", com.o_contamifile) ||
			 PILGetFname (k="leapfile", com.o_leapfile) ||
			 PILGetString(k="date_obs", com.date_obs) ||
			 PILGetReal  (k="energy", &com.energy) ||
			 PILGetReal  (k="detx", &com.detx) ||
			 PILGetReal  (k="dety", &com.dety) ||
			 PILGetFname (k="arffile", com.arffile) ) {
			goto pil_error;
		}

		com.scale_factor = 1.0;
		com.scale_contaminant = com.scale_previous = ANL_FALSE;
		if ( '\0' != *com.arffile && 0 != CLstricmp("NONE", com.arffile) ) {
			if ( PILGetBool(k="scale_contaminant", &com.scale_contaminant) ) {
				goto pil_error;
			}
			if ( com.scale_contaminant ) {
				if ( PILGetBool(k="scale_previous", &com.scale_previous) ||
					 PILGetReal(k="scale_factor", &com.scale_factor) ) {
					goto pil_error;
				}
			}
		}

		*status = ANL_OK;;
		return;

	pil_error:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
		*status = ANL_QUIT;
		return;
	}

	*status = ANL_OK;
}

void
xiscontamicalc_init(int *status)
{
	int istat;
	CALDB_INFO caldb;

	if ( com.scale_previous ) {		/* no need for initialization */
		*status = ANL_OK;
		return;
	}

/* caldb support */
	com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
	if ( NULL == com.leapfile ) {
		*status = ANL_QUIT;
		return;
	}

	if ( 0 == CLstricmp("CALDB", com.o_contamifile) ) {
		aste_caldb_init(&caldb);
		caldb.telescop = com.telescop;
		caldb.instrume = com.instrume;
		caldb.codename = "CONTAMI_TRANS";
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
		com.contamifile = caldb.filename;
	}

/* initialize aste_time */
	if ( NULL == atMissionTimeInit(com.leapfile, -2) ) {
		*status = ANL_QUIT;
	}

/* initialize xis_contami */
	istat = xis_contami_init(&com.xcp, com.contamifile);
	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}

/* convert date string to aetime */
	aefits_datestr2attimeD(com.date_obs, &com.attime);
	com.aetime = attimeD2aste(&com.attime);
	aefits_attimeD2datestr(&com.attime, com.date_obs);

	com.aetime0 = com.xcp->gp[0].t;
	aste2attimeD(com.aetime0, &com.attime0);
	aefits_attimeD2datestr(&com.attime0, com.date0);

	*status = ANL_OK;
}

void
xiscontamicalc_his(int *status)
{
	*status = ANL_OK;
}

void
xiscontamicalc_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
xiscontamicalc_ana(int *nevent, int *eventid, int *status)
{
	char *k;
	int istat;

	if ( com.scale_previous ) {
		istat = scale_arffile(com.arffile);
		*status = ANL_ENDLOOP;
		return;
	}

	com.transmis = xis_contami(com.xcp,
		com.energy, com.aetime, com.detx, com.dety, &com.carbon, &com.oxygen);
	com.r_offset = com.xcp->last_radius;
	com.days = (com.aetime - com.aetime0) / (24*60*60);

	anl_msg_always("\n");
	anl_msg_always("\
TELESCOP = '%s'\n", com.telescop);
	anl_msg_always("\
INSTRUME = '%s'\n", com.instrume);
	anl_msg_always("\
CONTAMIFILE = '%s'%s\n", com.contamifile,
		(com.contamifile == com.o_contamifile) ? "" : " (CALDB)");
	anl_msg_always("\
LEAPFILE = '%s'%s\n", com.leapfile,
		(com.leapfile == com.o_leapfile) ? "" : " (CALDB)");
	anl_msg_always("\
DATE-OBS = '%s' ( TIME = %.6f )\n", com.date_obs, com.aetime);
	anl_msg_always("\
DAYS = %.3f [dy] after %s\n", com.days, com.date0);
	anl_msg_always("\
ENERGY = %.3f [keV]\n", com.energy);
	anl_msg_always("\
R_OFFSET = %.3f [arcmin] at ( DETX , DETY ) = ( %.1f , %.1f ) [pixel]\n",
		com.r_offset, com.detx, com.dety);
	anl_msg_always("\
CARBON = %.6f [10**18 cm**-2]\n", com.carbon);
	anl_msg_always("\
OXYGEN = %.6f [10**18 cm**-2]\n", com.oxygen);
	anl_msg_always("\
TRANSMIS = %.6e\n", com.transmis);
	anl_msg_always("\n");

	if ( PILPutString(k="date_obs", com.date_obs) ||
		 PILPutString(k="date0", com.date0) ||
		 PILPutReal(k="time", com.aetime) ||
		 PILPutReal(k="days", com.days) ||
		 PILPutReal(k="r_offset", com.r_offset) ||
		 PILPutReal(k="carbon", com.carbon) ||
		 PILPutReal(k="oxygen", com.oxygen) ||
		 PILPutReal(k="transmis", com.transmis) ) {
		anl_msg_error("\
%s: PILPut('%s') failed\n", pname, k);
	}

	if ( '\0' != *com.arffile && 0 != CLstricmp("NONE", com.arffile) ) {
		istat = scale_arffile(com.arffile);
	}

	*status = ANL_ENDLOOP;
}

void
xiscontamicalc_endrun(int *status)
{
	*status = ANL_OK;
}

void
xiscontamicalc_exit(int *status)
{
	if ( NULL != com.xcp ) xis_contami_free(com.xcp);

	*status = ANL_OK;
}
