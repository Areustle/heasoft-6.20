/*
	aeaspect.c

	2005/06/07 Y.ISHISAKI	version 1.2

	2005/06/14 Y.ISHISAKI	version 1.3
		add sample_sec, adopt_median parameters
		update ATT_FILE, TELDEF keywords in FITS header

	2005/06/26 Y.ISHISAKI	version 1.4
		use aste_instrume_id() to judge INSTRUME in process_hdu()
		copy OBSERVER keyword

	2005/10/24 Y.ISHISAKI	version 1.5
		add update_obs parameter
		copy OBS_MODE, OBS_ID, OBS_REM keywords

	2005/10/27 Y.ISHISAKI	version 1.6
		use asp->mean_ea in show_update_keys(), process_hdu()

	2006/04/26 Y.ISHISAKI	version 1.7
		use asp->mean->alpha/delta/roll in process_hdu(), show_update_keys()

	2006/07/02 Y.ISHISAKI	version 1.8
		support of CALDB for leapfile & teldef
		add date_obs, date_end, update_date parameters
		calculate median between date_obs - date_end

	2006/07/12 Y.ISHISAKI	version 1.9
		unlink temporary file in get_file_name()

	2006/07/24 Y.ISHISAKI	version 2.0
		write task_name & task_version to history
		instead of pname & module_version in _init()
		print error when PILGetXXX() failed

	2006/08/08 Y.ISHISAKI	version 2.1
		use aefits_datestr2attimeD() in aeFitsHeaderUtil-3.2
		instead of local datestr2attimeD()
		remove unused aetime, attime in aeaspect_init()

	2007/05/07 Y.ISHISAKI	version 2.2
		copy NOM_PNT if keyword exist in attitude file

	2007/05/14 Y.ISHISAKI	version 2.3
		use aste_caldb_find_leapfile()
		return ANL_ERROR if process_file() failed in _ana()
		call fits_update_chksum() in process_hdu()
		remove ".gz" ".Z" ".z" for ATT_FILE header keyword
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
#include "aste_time.h"
#include "aste_att.h"
#include "aste_aspect.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "aeaspect";
char aeaspect_version[] = "version 2.3";

static struct {
/* aeaspect */
	char attitude_file[PIL_LINESIZE];
	char mod_file_list[PIL_LINESIZE];
	char o_leapfile[PIL_LINESIZE];
	char *leapfile;
	char o_xrs_teldef[PIL_LINESIZE];
	char *xrs_teldef;
	char o_hxd_teldef[PIL_LINESIZE];
	char *hxd_teldef;
	char o_xis_teldef[4][PIL_LINESIZE];
	char *xis_teldef[4];
	double sample_sec;
	double offset_tolerance;
	double roll_tolerance;
	int adopt_median;
	int num_split;
	int update_obs;
	int update_obj;
	int update_nom;
	int update_pnt;
	int update_ea;
	int update_date;
	char date_obs[PIL_LINESIZE];
	char date_end[PIL_LINESIZE];
	double t0;
	double t1;
	char date_obs_new[32];	/* 'YYYY-MM-DDThh:mm:ss' */
	char date_end_new[32];	/* 'YYYY-MM-DDThh:mm:ss' */

	ATTFILE *attitude;
	TELDEF *teldef_xrs;
	TELDEF *teldef_hxd;
	TELDEF *teldef_xis[4];

	ASTE_ASPECT aspect;
	AE_STD_KEYS stdkeys;

	int file_num;
	char **msg;
} com;

static char *
get_attitude_file_nogz(char *attitude_file)
{
	static char attitude_file_nogz[PIL_LINESIZE];
	char *base = aefits_basename(attitude_file);
	int len = strlen(base);

	if ( 3 < len && 0 == strcmp(".gz", &base[len-3]) ) {
		strcpy(attitude_file_nogz, base);
		attitude_file_nogz[len-3] = '\0';
		return attitude_file_nogz;
	} else if ( 2 < len && 0 == strcmp(".Z", &base[len-2]) ) {
		strcpy(attitude_file_nogz, base);
		attitude_file_nogz[len-2] = '\0';
		return attitude_file_nogz;
	} else if ( 2 < len && 0 == strcmp(".z", &base[len-2]) ) {
		strcpy(attitude_file_nogz, base);
		attitude_file_nogz[len-2] = '\0';
		return attitude_file_nogz;
	}

	return base;
}

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
		 0 == strcmp("none", filelist) ) {
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
			fprintf(stderr, "\
%s: temporary file '%s' create failed\n", pname, fn);
			return NULL;
		}
		fprintf(fp, "@--\n%s\n\n", filelist);
		fclose(fp);
		istat = CLopnrd(fn);
		unlink(fn);
		if ( istat ) {
			fprintf(stderr, "\
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
calc_mean_attitude(void)
{
	int i, istat, flag_ok;
	double t0, t1, dt;
	ASTE_ASPECT asp_min;

	ASTE_ASPECT *asp = &com.aspect;

	asp->adopt_median = com.adopt_median;
	asp->verbose = -1;
	asp->teldef = com.teldef_xrs;
	asp->attitude = com.attitude;
	asp->t0 = com.t0;
	asp->t1 = com.t1;
	asp->sample_sec = com.sample_sec;
	asp->offset_tolerance = 0.0;
	asp->roll_tolerance = 0.0;

	istat = aste_aspect_init(asp);
	if ( istat ) {
		return -1;
	}

	istat = aste_aspect_attitude(asp);
	if ( istat ) {
		return -1;
	}

	com.msg = aste_aspect_message(asp);
	fflush(NULL);
	printf("\n");
	for (i = 0; NULL != com.msg[i]; i++) {
		printf("%s\n", com.msg[i]);
	}
	printf("\n");

	if ( com.adopt_median ) {
		return 0;
	}

	flag_ok = ANL_TRUE;
	if ( 0.0 < com.offset_tolerance &&
		 com.offset_tolerance < asp->ma_offset ) {
		printf("\
%s: Maximum offset %7.3f arcmin exceed offset_tolerance=%.3f\n",
			pname, asp->ma_offset, com.offset_tolerance);
		flag_ok = ANL_FALSE;
	}

	if ( 0.0 < com.roll_tolerance &&
		 com.roll_tolerance <  asp->ma.roll - asp->mi.roll ) {
		printf("\
%s: Maximum  roll  %7.3f degree exceed   roll_tolerance=%.3f\n",
			pname, asp->ma.roll - asp->mi.roll, com.roll_tolerance);
		flag_ok = ANL_FALSE;
	}
	fflush(NULL);

	if ( flag_ok ) {
		return 0;
	}

	if ( 0 < com.num_split ) {
		printf("\n\
    search for stable time intervals ...\n");
		fflush(NULL);
		asp_min = com.aspect;
		t0 = asp->attitude->tstart;
		t1 = asp->attitude->tstop;
		dt = t1 - t0;
		for (i = 0; i < com.num_split; i++) {
			printf("\n\
 [%d/%d]\n", i+1, com.num_split);
			asp->t0 = t0 + dt * i / com.num_split;
			asp->t1 = t0 + dt * (i+1) / com.num_split;
			istat = aste_aspect_attitude(asp);
			if ( istat ) {
				continue;
			}
			if ( asp->ma_offset < asp_min.ma_offset ) {
				asp_min = *asp;
			}
			printf("\
  Max offset / roll angle deviation :%12.6f (arcmin)%12.6f (deg)\n",
				asp->ma_offset, asp->ma.roll - asp->mi.roll);
		}

		if ( ( 0.0 < com.offset_tolerance &&
			   com.offset_tolerance < asp_min.ma_offset ) ||
			 ( 0.0 < com.roll_tolerance &&
			   com.roll_tolerance <  asp_min.ma.roll - asp_min.mi.roll ) ) {
			printf("\n\
%s: no stable time intervals are found\n", pname);
			flag_ok = ANL_FALSE;
		} else {
			printf("\n\
%s: stable time interval(s) are found\n", pname);
			flag_ok = ANL_TRUE;
		}

		*asp = asp_min;

		com.msg = aste_aspect_message(asp);
		fflush(NULL);
		printf("\n");
		for (i = 0; NULL != com.msg[i]; i++) {
			printf("%s\n", com.msg[i]);
		}
		printf("\n");
		fflush(NULL);

		if ( flag_ok ) {
			return 0;
		}
	}

	return 1;	/* not perfect, but acceptable */
}

static int
show_update_keys(void)
{
	static struct AE_STD_KEYS_COMMENT *comment = &com.stdkeys.comment;

	int i;
	double ra_pnt, dec_pnt;
	TELDEF *teldef;
	TELDEF_ASTROE *p;
	char string[FLEN_CARD];

	ASTE_ASPECT *asp = &com.aspect;
	AtEulerAng *ea = asp->mean_ea;

	fflush(NULL); printf("\
%s: keywords to be updated are ...\n\n", pname);

	if ( com.update_obs ) {
		printf("\
%s\n", asp->obs_mode_card);
		printf("\
%s\n", asp->obs_id_card);
		printf("\
%s\n", asp->observer_card);
		printf("\
%s\n", asp->obs_rem_card);
		printf("\
%s\n", asp->nom_pnt_card);
	}

	if ( com.update_obj ) {
		printf("\
%s\n", asp->object_card);
		memcpy(asp->ra_obj_card, "RA_OBJ  ", 8);
		printf("\
%s\n", asp->ra_obj_card);
		memcpy(asp->dec_obj_card, "DEC_OBJ ", 8);
		printf("\
%s\n", asp->dec_obj_card);
	}

	if ( com.update_nom ) {
		printf("\
%-8s= %20.4f / %s\n", "RA_NOM", asp->mean->alpha, comment->ra_nom);
		printf("\
%-8s= %20.4f / %s\n", "DEC_NOM", asp->mean->delta, comment->dec_nom);
		printf("\
%-8s= %20.4f / %s\n", "PA_NOM", asp->mean->roll, comment->pa_nom);
	}

	if ( com.update_ea ) {
		printf("\
%-8s= %20.12f / %s\n", "MEAN_EA1", RAD2DEG*ea->phi,   comment->mean_ea1);
		printf("\
%-8s= %20.12f / %s\n", "MEAN_EA2", RAD2DEG*ea->theta, comment->mean_ea2);
		printf("\
%-8s= %20.12f / %s\n", "MEAN_EA3", RAD2DEG*ea->psi,   comment->mean_ea3);
	}

	sprintf(string, "'%s'", get_attitude_file_nogz(com.attitude_file));
	printf("\
%-8s= %20s / %s\n", "ATT_FILE", string, comment->att_file);

	if ( com.update_date ) {
		printf("\
%-8s= '%s'/ %s\n", "DATE-OBS", com.date_obs_new, comment->date_obs);
		printf("\
%-8s= '%s'/ %s\n", "DATE-END", com.date_end_new, comment->date_end);
	}

	if ( com.update_pnt ) {
		printf("\
 [XRS]\n");
		sprintf(string, "'%s'", aefits_basename(com.xrs_teldef));
		printf("\
%-8s= %20s / %s\n", "TELDEF", string, comment->teldef);
		teldef = com.teldef_xrs;
		p = teldef->mission.aste;
		aste_det2ecs(teldef, ea, p->optaxisx, p->optaxisy, &ra_pnt, &dec_pnt);
		printf("\
%-8s= %20.4f / %s\n", "RA_PNT", ra_pnt, comment->ra_pnt);
		printf("\
%-8s= %20.4f / %s\n", "DEC_PNT", dec_pnt, comment->dec_pnt);

		printf("\
 [HXD]\n");
		sprintf(string, "'%s'", aefits_basename(com.hxd_teldef));
		printf("\
%-8s= %20s / %s\n", "TELDEF", string, comment->teldef);
		teldef = com.teldef_hxd;
		p = teldef->mission.aste;
		aste_det2ecs(teldef, ea, p->optaxisx, p->optaxisy, &ra_pnt, &dec_pnt);
		printf("\
%-8s= %20.4f / %s\n", "RA_PNT", ra_pnt, comment->ra_pnt);
		printf("\
%-8s= %20.4f / %s\n", "DEC_PNT", dec_pnt, comment->dec_pnt);

		for (i = 0; i < 4; i++) {
			printf("\
 [XIS%d]\n", i);
			sprintf(string, "'%s'", aefits_basename(com.xis_teldef[i]));
			printf("\
%-8s= %20s / %s\n", "TELDEF", string, comment->teldef);
			teldef = com.teldef_xis[i];
			p = teldef->mission.aste;
			aste_det2ecs(teldef, ea, p->optaxisx, p->optaxisy, &ra_pnt, &dec_pnt);
			printf("\
%-8s= %20.4f / %s\n", "RA_PNT", ra_pnt, comment->ra_pnt);
			printf("\
%-8s= %20.4f / %s\n", "DEC_PNT", dec_pnt, comment->dec_pnt);
		}
	}

	printf("\n");
	fflush(NULL);

	return 0;
}

static int
process_hdu(fitsfile *fp, int hdunum)
{
	static struct AE_STD_KEYS_COMMENT *cm = &com.stdkeys.comment;

	int i, instrume_id;
	char *p, *task_name, *task_version, *fn, history[1024];
	char telescop[FLEN_KEYWORD], instrume[FLEN_KEYWORD];
	double ra_pnt, dec_pnt;

	int istat = 0;
	ASTE_ASPECT *asp = &com.aspect;
	AtEulerAng *ea = asp->mean_ea;

	if ( fits_read_key_str(fp, "TELESCOP", telescop, NULL, &istat) ) {
/* no TELESCOP keywords, just skip it */
		return 0;
	}

	if ( fits_read_key_str(fp, "INSTRUME", instrume, NULL, &istat) ) {
/* no INSTRUME keywords, assume common */
		instrume[0] = '\0';
		instrume_id = -1;
		istat = 0;	/* ignore error */
	} else {
		instrume_id = aste_instrume_id(instrume);
	}

	fflush(NULL); printf("\
%+8d: TELESCOP='%s', INSTRUME='%s'\n", hdunum-1, telescop, instrume);
	fflush(NULL);

	task_name = anl_task_name();
	task_version = anl_task_version();
	istat = aefits_write_name_vers(fp, task_name, task_version);
	if ( istat ) {
		return istat;
	}
	sprintf(history, "\
  attitude='%s'", com.attitude_file);
	fits_write_history(fp, history, &istat);
/*	sprintf(history, "\
  filelist='%s'", com.mod_file_list);
	fits_write_history(fp, history, &istat);*/
	if ( com.leapfile == com.o_leapfile ) {
		sprintf(history, "\
  leapfile='%s'", com.leapfile);
	} else {
		sprintf(history, "\
  leapfile='%s' (%s)", com.leapfile, com.o_leapfile);
	}
	fits_write_history(fp, history, &istat);
	if ( ASTE_XRS_ID == instrume_id ) {
		sprintf(history, "\
  xrs_teldef='%s'%s", com.xrs_teldef,
			(com.xrs_teldef == com.o_xrs_teldef) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
	} else if ( ASTE_HXD_ID == instrume_id ) {
		sprintf(history, "\
  hxd_teldef='%s'%s", com.hxd_teldef,
			(com.hxd_teldef == com.o_hxd_teldef) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
	} else if ( ASTE_XIS0_ID == instrume_id ) {
		sprintf(history, "\
  xis0_teldef='%s'%s", com.xis_teldef[0],
			(com.xis_teldef[0] == com.o_xis_teldef[0]) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
	} else if ( ASTE_XIS1_ID == instrume_id ) {
		sprintf(history, "\
  xis1_teldef='%s'%s", com.xis_teldef[1],
			(com.xis_teldef[1] == com.o_xis_teldef[1]) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
	} else if ( ASTE_XIS2_ID == instrume_id ) {
		sprintf(history, "\
  xis2_teldef='%s'%s", com.xis_teldef[2],
			(com.xis_teldef[2] == com.o_xis_teldef[2]) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
	} else if ( ASTE_XIS3_ID == instrume_id ) {
		sprintf(history, "\
  xis3_teldef='%s'%s", com.xis_teldef[3],
			(com.xis_teldef[3] == com.o_xis_teldef[3]) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
	}
	sprintf(history, "\
  sample_sec=%.3f", com.sample_sec);
	fits_write_history(fp, history, &istat);
	if ( ! com.adopt_median ) {
		sprintf(history, "\
  offset_tolerance=%.3f", com.offset_tolerance);
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  roll_tolerance=%.3f", com.roll_tolerance);
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  num_split=%d", com.num_split);
		fits_write_history(fp, history, &istat);
	}
	sprintf(history, "\
  adopt_median=%s, update_obs=%s, update_date=%s",
		com.adopt_median ? "yes" : "no",
		com.update_obs ? "yes" : "no",
		com.update_date ? "yes" : "no");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  update_obj=%s, update_nom=%s, update_pnt=%s, update_ea=%s",
		com.update_obj ? "yes" : "no",
		com.update_nom ? "yes" : "no",
		com.update_pnt ? "yes" : "no",
		com.update_ea  ? "yes" : "no");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  date_obs='%-8s' (t0=%.1f, DATE-OBS='%s')",
		com.date_obs, com.t0, com.date_obs_new);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  date_end='%-8s' (t1=%.1f, DATE-END='%s')",
		com.date_end, com.t1, com.date_end_new);
	fits_write_history(fp, history, &istat);
	fits_write_history(fp, " ", &istat);

	for (i = 0; NULL != com.msg[i]; i++) {
		p = com.msg[i];
		if ( '\0' == *p ) {
			p = " ";
		} else {
			p += 2;
		}
		fits_write_history(fp, p, &istat);
	}

	fits_write_history(fp, " ", &istat);

	if ( istat ) {
		fprintf(stderr, "\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	if ( com.update_obs ) {
		if ( 0 ==istat ) {
fits_delete_key(fp, "NOM_PNT", &istat);
			istat = 0;		/* ignore error */
		}
fits_update_card(fp, "OBS_MODE",	asp->obs_mode_card,	&istat);
fits_update_card(fp, "OBS_ID",		asp->obs_id_card,	&istat);
fits_update_card(fp, "OBSERVER",	asp->observer_card,	&istat);
fits_update_card(fp, "OBS_REM",		asp->obs_rem_card,	&istat);
fits_insert_card(fp, asp->nom_pnt_card,	&istat);
	}

	if ( com.update_obj ) {
fits_update_card(fp, "OBJECT",  asp->object_card,  &istat);
fits_update_card(fp, "RA_OBJ",  asp->ra_obj_card,  &istat);
fits_update_card(fp, "DEC_OBJ", asp->dec_obj_card, &istat);
	}

	if ( com.update_nom ) {
fits_update_key_fixdbl(fp, "RA_NOM",  asp->mean->alpha, 4,cm->ra_nom,  &istat);
fits_update_key_fixdbl(fp, "DEC_NOM", asp->mean->delta, 4,cm->dec_nom, &istat);
fits_update_key_fixdbl(fp, "PA_NOM",  asp->mean->roll,  4,cm->pa_nom,  &istat);
	}

	if ( com.update_ea ) {
fits_update_key_fixdbl(fp,"MEAN_EA1",RAD2DEG*ea->phi,  12,cm->mean_ea1,&istat);
fits_update_key_fixdbl(fp,"MEAN_EA2",RAD2DEG*ea->theta,12,cm->mean_ea2,&istat);
fits_update_key_fixdbl(fp,"MEAN_EA3",RAD2DEG*ea->psi,  12,cm->mean_ea3,&istat);
	}

	if ( com.update_date ) {
		int istat2;
fits_update_key_str(fp, "DATE-OBS", com.date_obs_new, cm->date_obs, &istat);
fits_update_key_str(fp, "DATE-END", com.date_end_new, cm->date_end, &istat);
		istat2 = 0;
fits_delete_key(fp, "TIME-OBS", &istat2);	/* ignore error */
		istat2 = 0;
fits_delete_key(fp, "TIME-END", &istat2);	/* ignore error */
	}

	fn = get_attitude_file_nogz(com.attitude_file);
fits_update_key_str   (fp, "ATT_FILE", fn, cm->att_file, &istat);

	if ( com.update_pnt ) {
		TELDEF *teldef;
		TELDEF_ASTROE *p;

		if ( ASTE_XRS_ID == instrume_id ) {
			fn = aefits_basename(com.xrs_teldef);
			teldef = com.teldef_xrs;
			p = teldef->mission.aste;
			aste_det2ecs(teldef, ea, p->optaxisx, p->optaxisy, &ra_pnt, &dec_pnt);
			fits_update_key_str   (fp, "TELDEF",  fn,         cm->teldef,  &istat);
			fits_update_key_fixdbl(fp, "RA_PNT",  ra_pnt,  4, cm->ra_pnt,  &istat);
			fits_update_key_fixdbl(fp, "DEC_PNT", dec_pnt, 4, cm->dec_pnt, &istat);
		} else if ( ASTE_HXD_ID == instrume_id ) {
			fn = aefits_basename(com.hxd_teldef);
			teldef = com.teldef_hxd;
			p = teldef->mission.aste;
			aste_det2ecs(teldef, ea, p->optaxisx, p->optaxisy, &ra_pnt, &dec_pnt);
			fits_update_key_str   (fp, "TELDEF",  fn,         cm->teldef,  &istat);
			fits_update_key_fixdbl(fp, "RA_PNT",  ra_pnt,  4, cm->ra_pnt,  &istat);
			fits_update_key_fixdbl(fp, "DEC_PNT", dec_pnt, 4, cm->dec_pnt, &istat);
		} else if ( ASTE_XIS0_ID == instrume_id ) {
			fn = aefits_basename(com.xis_teldef[0]);
			teldef = com.teldef_xis[0];
			p = teldef->mission.aste;
			aste_det2ecs(teldef, ea, p->optaxisx, p->optaxisy, &ra_pnt, &dec_pnt);
			fits_update_key_str   (fp, "TELDEF",  fn,         cm->teldef,  &istat);
			fits_update_key_fixdbl(fp, "RA_PNT",  ra_pnt,  4, cm->ra_pnt,  &istat);
			fits_update_key_fixdbl(fp, "DEC_PNT", dec_pnt, 4, cm->dec_pnt, &istat);
		} else if ( ASTE_XIS1_ID == instrume_id ) {
			fn = aefits_basename(com.xis_teldef[1]);
			teldef = com.teldef_xis[1];
			p = teldef->mission.aste;
			aste_det2ecs(teldef, ea, p->optaxisx, p->optaxisy, &ra_pnt, &dec_pnt);
			fits_update_key_str   (fp, "TELDEF",  fn,         cm->teldef,  &istat);
			fits_update_key_fixdbl(fp, "RA_PNT",  ra_pnt,  4, cm->ra_pnt,  &istat);
			fits_update_key_fixdbl(fp, "DEC_PNT", dec_pnt, 4, cm->dec_pnt, &istat);
		} else if ( ASTE_XIS2_ID == instrume_id ) {
			fn = aefits_basename(com.xis_teldef[2]);
			teldef = com.teldef_xis[2];
			p = teldef->mission.aste;
			aste_det2ecs(teldef, ea, p->optaxisx, p->optaxisy, &ra_pnt, &dec_pnt);
			fits_update_key_str   (fp, "TELDEF",  fn,         cm->teldef,  &istat);
			fits_update_key_fixdbl(fp, "RA_PNT",  ra_pnt,  4, cm->ra_pnt,  &istat);
			fits_update_key_fixdbl(fp, "DEC_PNT", dec_pnt, 4, cm->dec_pnt, &istat);
		} else if ( ASTE_XIS3_ID == instrume_id ) {
			fn = aefits_basename(com.xis_teldef[3]);
			teldef = com.teldef_xis[3];
			p = teldef->mission.aste;
			aste_det2ecs(teldef, ea, p->optaxisx, p->optaxisy, &ra_pnt, &dec_pnt);
			fits_update_key_str   (fp, "TELDEF",  fn,         cm->teldef,  &istat);
			fits_update_key_fixdbl(fp, "RA_PNT",  ra_pnt,  4, cm->ra_pnt,  &istat);
			fits_update_key_fixdbl(fp, "DEC_PNT", dec_pnt, 4, cm->dec_pnt, &istat);
		}

	}

	fits_update_chksum(fp, &istat);

	return istat;
}

static int
process_file(char *file)
{
	fitsfile *fp;
	int hdunum, hdutype, dummy_status;

	int istat = 0;

	fits_open_file(&fp, file, READWRITE, &istat);
	if ( istat ) {
		fprintf(stderr, "\
%s: '%s' open failed\n", pname, file);
		return istat;
	}

	fits_get_hdu_num(fp, &hdunum);

/* not in primary header, process only current HDU */
	if ( 1 != hdunum ) {
		istat = process_hdu(fp, hdunum);
		if ( istat ) {
			goto quit;
		}
		goto skip;
	}

/* in primary header, process all HDUs */
	for (;;) {
		istat = process_hdu(fp, hdunum);
		if ( istat ) {
			goto quit;
		}
		fits_movrel_hdu(fp, 1, &hdutype, &istat);
		if ( END_OF_FILE == istat ) {
			istat = 0;
			break;
		}
		if ( istat ) {
			goto quit;
		}
		hdunum++;
	}

 skip:
	fits_close_file(fp, &istat);
	return istat;

 quit:
	dummy_status = 0;
	fits_close_file(fp, &dummy_status);
	return istat;
}

static double
get_date(char *k, fitsfile *fp, char *date_key)
{
	AtTimeD attime;
	double aetime;

	int istat = 0;

	if ( '\0' == *date_key || 0 == CLstricmp("NONE", date_key) ) {
		return 0.0;
	}

	if ( NULL != fp ) {
		fits_read_key_dbl(fp, date_key, &aetime, NULL, &istat);
		if ( 0 == istat ) {
			anl_msg_info("\
%s: %s='%-8s' found in attitude header, t=%.1f\n", pname, k, date_key, aetime);
			return aetime;
		}
	}

	istat = aefits_datestr2attimeD(date_key, &attime);
	if ( 0 == istat ) {
		aetime = attimeD2aste(&attime);
		anl_msg_info("\
%s: %s='%-8s' converted to t=%.1f\n", pname, k, date_key, aetime);
		return aetime;
	}

	aetime = 0.0;
	return aetime;
}

static int
get_date_new(double aetime, char *date_new, fitsfile *fp, char *time_key)
{
	AtTimeD attime;

	int istat = 0;

	if ( 0.0 == aetime ) {
		fits_read_key_dbl(fp, time_key, &aetime, NULL, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: '%s' keyword not found in attitude header (%d)\n", pname, time_key, istat);
			return istat;
		}
	}

	aste2attimeD(aetime, &attime);
	sprintf(date_new, "%04d-%02d-%02dT%02d:%02d:%02d",
		attime.yr, attime.mo, attime.dy,
		attime.hr, attime.mn, attime.sc);

	return 0;
}

void
aeaspect_startup(int *status)
{
	strcpy(com.mod_file_list, "none");
	strcpy(com.o_leapfile, "leapsec.fits");
	com.sample_sec = 60.0;
	com.offset_tolerance = 2.0;
	com.roll_tolerance = 1.0;
	com.adopt_median = ANL_YES;
	com.update_obs = ANL_YES;
	com.update_obj = ANL_YES;
	com.update_nom = ANL_YES;
	com.update_pnt = ANL_YES;
	com.update_ea  = ANL_YES;
	com.update_date= ANL_YES;

	com.leapfile = com.o_leapfile;
	com.xrs_teldef = com.o_xrs_teldef;
	com.hxd_teldef = com.o_hxd_teldef;
	com.xis_teldef[0] = com.o_xis_teldef[0];
	com.xis_teldef[1] = com.o_xis_teldef[1];
	com.xis_teldef[2] = com.o_xis_teldef[2];
	com.xis_teldef[3] = com.o_xis_teldef[3];

	com.file_num = 0;

	*status = ANL_OK;
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "ATTITUDE", com.attitude_file);
	printf("%20s   '%s'\n", "MOD_FILE_LIST", com.mod_file_list);

#define PRINT_CALDB(name, o_name, key) \
  printf((name==o_name) ? "%20s   '%s'\n" : "%20s   '%s' (CALDB)\n", key, name)

	if ( com.leapfile == com.o_leapfile ) {
		printf("%20s   '%s'\n", "LEAPFILE", com.leapfile);
	} else {
		printf("%20s   '%s' (%s)\n", "LEAPFILE", com.leapfile, com.o_leapfile);
	}
	PRINT_CALDB(com.xrs_teldef, com.o_xrs_teldef, "XRS_TELDEF");
	PRINT_CALDB(com.hxd_teldef, com.o_hxd_teldef, "XRS_TELDEF");
	PRINT_CALDB(com.xis_teldef[0], com.o_xis_teldef[0], "XIS0_TELDEF");
	PRINT_CALDB(com.xis_teldef[1], com.o_xis_teldef[1], "XIS1_TELDEF");
	PRINT_CALDB(com.xis_teldef[2], com.o_xis_teldef[2], "XIS2_TELDEF");
	PRINT_CALDB(com.xis_teldef[3], com.o_xis_teldef[3], "XIS3_TELDEF");

	printf("%20s   %.3f (s)\n", "SAMPLE_SEC", com.sample_sec);
	printf("%20s   %.6f (arcmin)\n", "OFFSET_TOLERANCE", com.offset_tolerance);
	printf("%20s   %.6f (degree)\n", "ROLL_TOLERANCE", com.roll_tolerance);
	printf("%20s   %s\n", "ADOPT_MEDIAN", com.adopt_median ? "YES" : "NO");
	printf("%20s   %s\n", "UPDATE_OBS", com.update_obs ? "YES" : "NO");
	printf("%20s   %s\n", "UPDATE_OBJ", com.update_obj ? "YES" : "NO");
	printf("%20s   %s\n", "UPDATE_NOM", com.update_nom ? "YES" : "NO");
	printf("%20s   %s\n", "UPDATE_PNT", com.update_pnt ? "YES" : "NO");
	printf("%20s   %s\n", "UPDATE_EA",  com.update_ea  ? "YES" : "NO");
	printf("%20s   %s\n", "UPDATE_DATE",com.update_date? "YES" : "NO");
	printf("%20s   '%s'\n", "DATE_OBS", com.date_obs);
	printf("%20s   '%s'\n", "DATE_END", com.date_end);
}

void
aeaspect_com(int *status)
{
#define NVAL	24
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"ATTITUDE",
		"MOD_FILE_LIST",
		"LEAPFILE",
		"XRS_TELDEF",
		"HXD_TELDEF",
		"XIS0_TELDEF",
		"XIS1_TELDEF",
		"XIS2_TELDEF",
		"XIS3_TELDEF",
		"SAMPLE_SEC",
		"OFFSET_TOLERANCE",
		"ROLL_TOLERANCE",
		"NUM_SPLIT",
		"ADOPT_MEDIAN",
		"UPDATE_OBS",
		"UPDATE_OBJ",
		"UPDATE_NOM",
		"UPDATE_PNT",
		"UPDATE_EA",
		"UPDATE_DATE",
		"DATE_OBS",
		"DATE_END",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"input attitude name",
		"file name or @filelist to modify FITS keywords",
		"leap seconds table file name",
		"teldef file name of XRS",
		"teldef file name of HXD",
		"teldef file name of XIS0",
		"teldef file name of XIS1",
		"teldef file name of XIS2",
		"teldef file name of XIS3",
		"sampling time in second (s)",
		"offset angle tolerance (arcmin)",
		"roll angle tolerance (degree)",
		"number of splitting time intervals",
		"adopt median instead of average",
		"update OBS_MODE/OBS_ID/OBSERVER/OBS_REM/NOM_PNT keywords",
		"update OBJECT/RA_OBJ/DEC_OBJ keywords",
		"update RA_NOM/DEC_NOM/PA_NOM keywords",
		"update RA_PNT/DEC_PNT keywords",
		"update MEAN_EA1/MEAN_EA2/MEAN_EA3 keywords",
		"update DATE-OBS/DATE-END keywords",
		"value or keyword name for DATE-OBS",
		"value or keyword name for DATE-END",
		"exit from this menu"
	};
	char *k;
	int answer[2];
	int nreply = 1;

	if ( *status ) {	/* ftools */
		*status = ANL_QUIT;
		if (
PILGetFname(k="attitude", com.attitude_file) ||
PILGetFname(k="filelist", com.mod_file_list) ||
PILGetFname(k="leapfile", com.o_leapfile) ||
PILGetFname(k="xrs_teldef", com.o_xrs_teldef) ||
PILGetFname(k="hxd_teldef", com.o_hxd_teldef) ||
PILGetFname(k="xis0_teldef", com.o_xis_teldef[0]) ||
PILGetFname(k="xis1_teldef", com.o_xis_teldef[1]) ||
PILGetFname(k="xis2_teldef", com.o_xis_teldef[2]) ||
PILGetFname(k="xis3_teldef", com.o_xis_teldef[3]) ||
PILGetReal (k="sample_sec", &com.sample_sec) ||
PILGetReal (k="offset_tolerance", &com.offset_tolerance) ||
PILGetReal (k="roll_tolerance", &com.roll_tolerance) ||
PILGetInt  (k="num_split", &com.num_split) ||
PILGetBool (k="adopt_median", &com.adopt_median) ||
PILGetBool (k="update_obs", &com.update_obs) ||
PILGetBool (k="update_obj", &com.update_obj) ||
PILGetBool (k="update_nom", &com.update_nom) ||
PILGetBool (k="update_pnt", &com.update_pnt) ||
PILGetBool (k="update_ea",  &com.update_ea) ||
PILGetBool (k="update_date",&com.update_date) ||
PILGetString(k="date_obs", com.date_obs) ||
PILGetString(k="date_end", com.date_end) ||
			 0 ) {
			anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
			*status = ANL_QUIT;
			return;
		}

		*status = ANL_OK;;
		return;
	}

	for (;;) {
		char *p;
		CMinquir(pname, NVAL, names, help, nreply, answer);
		p = names[answer[1]-1];
		if ( 0 == strcmp("SHOW_PARAMETER", p) ) {
			show_parameter();
		} else if ( 0 == strcmp("ATTITUDE", p) ) {
			CLtxtrd(p, com.attitude_file, sizeof(com.attitude_file));
		} else if ( 0 == strcmp("LEAPFILE", p) ) {
			CLtxtrd(p, com.o_leapfile, sizeof(com.o_leapfile));
		} else if ( 0 == strcmp("XRS_TELDEF", p) ) {
			CLtxtrd(p, com.o_xrs_teldef, sizeof(com.xrs_teldef));
		} else if ( 0 == strcmp("HXD_TELDEF", p) ) {
			CLtxtrd(p, com.o_hxd_teldef, sizeof(com.hxd_teldef));
		} else if ( 0 == strcmp("XIS0_TELDEF", p) ) {
			CLtxtrd(p, com.o_xis_teldef[0], sizeof(com.xis_teldef[0]));
		} else if ( 0 == strcmp("XIS1_TELDEF", p) ) {
			CLtxtrd(p, com.o_xis_teldef[1], sizeof(com.xis_teldef[1]));
		} else if ( 0 == strcmp("XIS2_TELDEF", p) ) {
			CLtxtrd(p, com.o_xis_teldef[2], sizeof(com.xis_teldef[2]));
		} else if ( 0 == strcmp("XIS3_TELDEF", p) ) {
			CLtxtrd(p, com.o_xis_teldef[3], sizeof(com.xis_teldef[3]));
		} else if ( 0 == strcmp("SAMPLE_SEC", p) ) {
			CLfdprd(p, &com.sample_sec);
		} else if ( 0 == strcmp("OFFSET_TOLERANCE", p) ) {
			CLfdprd(p, &com.offset_tolerance);
		} else if ( 0 == strcmp("ROLL_TOLERANCE", p) ) {
			CLfdprd(p, &com.roll_tolerance);
		} else if ( 0 == strcmp("NUM_SPLIT", p) ) {
			CLintrd(p, &com.num_split);
		} else if ( 0 == strcmp("ADOPT_MEDIAN", p) ) {
			CLlogrd(p, &com.adopt_median);
		} else if ( 0 == strcmp("UPDATE_OBS", p) ) {
			CLlogrd(p, &com.update_obs);
		} else if ( 0 == strcmp("UPDATE_OBJ", p) ) {
			CLlogrd(p, &com.update_obj);
		} else if ( 0 == strcmp("UPDATE_NOM", p) ) {
			CLlogrd(p, &com.update_nom);
		} else if ( 0 == strcmp("UPDATE_PNT", p) ) {
			CLlogrd(p, &com.update_pnt);
		} else if ( 0 == strcmp("UPDATE_EA", p) ) {
			CLlogrd(p, &com.update_ea);
		} else if ( 0 == strcmp("UPDATE_DATE", p) ) {
			CLlogrd(p, &com.update_date);
		} else if ( 0 == strcmp("DATE_OBS", p) ) {
			CLtxtrd(p, com.date_obs, sizeof(com.date_obs));
		} else if ( 0 == strcmp("DATE_END", p) ) {
			CLtxtrd(p, com.date_end, sizeof(com.date_end));
		} else if ( 0 == strcmp("EXIT", p) ) {
			break;
		}
	}
#undef NVAL

	*status = ANL_OK;
}

void
aeaspect_init(int *status)
{
	static struct CALDB_INDEX {
		char *telescop;
		char *instrume;
		char *codename;
		char *origname;
		char **realname_ptr;
	} caldb_index[] = {
		{ NULL, "XRS",  "TELDEF", com.o_xrs_teldef,    &com.xrs_teldef },
		{ NULL, "HXD",  "TELDEF", com.o_hxd_teldef,    &com.hxd_teldef },
		{ NULL, "XIS0", "TELDEF", com.o_xis_teldef[0], &com.xis_teldef[0] },
		{ NULL, "XIS1", "TELDEF", com.o_xis_teldef[1], &com.xis_teldef[1] },
		{ NULL, "XIS2", "TELDEF", com.o_xis_teldef[2], &com.xis_teldef[2] },
		{ NULL, "XIS3", "TELDEF", com.o_xis_teldef[3], &com.xis_teldef[3] },
		{ NULL, NULL, NULL, NULL, NULL }
	};

	CALDB_INFO caldb;
	int i, istat, verbose;

	com.leapfile = aste_caldb_find_leapfile(com.o_leapfile);
	if ( NULL == com.leapfile ) {
		goto quit;
	}

/* find CALDB file */
	for (i = 0; NULL != caldb_index[i].realname_ptr; i++) {
		struct CALDB_INDEX *p = &caldb_index[i];
		aste_caldb_init(&caldb);
		if ( 0 == CLstricmp("CALDB", p->origname) ) {
			caldb.telescop = p->telescop;
			caldb.instrume = p->instrume;
			caldb.codename = p->codename;
			aste_caldb_get(&caldb);
			if ( 0 != caldb.status || 0 == caldb.nfound ) {
				anl_msg_error("\
%s: no CALDB entry for '%s' (status=%d)\n",
					pname, caldb.codename, caldb.status);
				return;
			}
			if ( 1 != caldb.nfound ) {
				anl_msg_warning("\
%s: WARNING: multiple CALDB entry (nfound=%d) for '%s'\n",
					pname, caldb.nfound, caldb.codename);
			}
			*p->realname_ptr = caldb.filename;
		}
	}

	show_parameter();

/* set standard keyword values for comment words */
	aeSetDefaultKeywordValues(&com.stdkeys);

/* initialize aste_time */
	verbose = ( 0 == CLstricmp("none", com.leapfile) ) ? -1 : -2;
	if ( NULL == atMissionTimeInit(com.leapfile, verbose) ) {
		anl_msg_error("\
%s: atMissionTimeInit('%s') failed\n", pname, com.leapfile);
		goto quit;
	}
	if ( -1 == verbose ) printf("\n");

/* initialize aste_att */
	com.attitude = aste_att_init(com.attitude_file);
	if ( NULL == com.attitude ) {
		fprintf(stderr, "\
%s: aste_att_init('%s') failed\n", pname, com.attitude_file);
		*status = ANL_QUIT;
		return;
	}

/* initialize aste_coord */
	com.teldef_xrs = aste_coord_init(NULL, "XRS", com.xrs_teldef);
	if ( NULL == com.teldef_xrs ) {
		fprintf(stderr, "\
%s: aste_coord_init('%s') failed\n", pname, com.xrs_teldef);
		*status = ANL_QUIT;
		return;
	}

	com.teldef_hxd = aste_coord_init(NULL, "HXD", com.hxd_teldef);
	if ( NULL == com.teldef_hxd ) {
		fprintf(stderr, "\
%s: aste_coord_init('%s') failed\n", pname, com.hxd_teldef);
		*status = ANL_QUIT;
		return;
	}

	com.teldef_xis[0] = aste_coord_init(NULL, "XIS0", com.xis_teldef[0]);
	if ( NULL == com.teldef_xis[0] ) {
		fprintf(stderr, "\
%s: aste_coord_init('%s') failed\n", pname, com.xis_teldef[0]);
		*status = ANL_QUIT;
		return;
	}

	com.teldef_xis[1] = aste_coord_init(NULL, "XIS1", com.xis_teldef[1]);
	if ( NULL == com.teldef_xis[1] ) {
		fprintf(stderr, "\
%s: aste_coord_init('%s') failed\n", pname, com.xis_teldef[1]);
		*status = ANL_QUIT;
		return;
	}

	com.teldef_xis[2] = aste_coord_init(NULL, "XIS2", com.xis_teldef[2]);
	if ( NULL == com.teldef_xis[2] ) {
		fprintf(stderr, "\
%s: aste_coord_init('%s') failed\n", pname, com.xis_teldef[2]);
		*status = ANL_QUIT;
		return;
	}

	com.teldef_xis[3] = aste_coord_init(NULL, "XIS3", com.xis_teldef[3]);
	if ( NULL == com.teldef_xis[3] ) {
		fprintf(stderr, "\
%s: aste_coord_init('%s') failed\n", pname, com.xis_teldef[3]);
		*status = ANL_QUIT;
		return;
	}

/* get values for DATE_OBS, DATE_END */
	com.t0 = get_date("date_obs", com.attitude->fp, com.date_obs);
	com.t1 = get_date("date_end", com.attitude->fp, com.date_end);
	if ( 0.0 != com.t0 || 0.0 != com.t1 ) {
		anl_msg_info("\n");
	}

	if ( com.update_date ) {
		if (
get_date_new(com.t0, com.date_obs_new, com.attitude->fp, "TSTART") ||
get_date_new(com.t1, com.date_end_new, com.attitude->fp, "TSTOP") ) {
			*status = ANL_QUIT;
			return;
		}
	}

/* calculate median of the attitude */
	istat = calc_mean_attitude();
	if ( istat < 0 ) {
		*status = ANL_QUIT;
		return;
	}

	aste_att_close(com.attitude);

	show_update_keys();

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
aeaspect_his(int *status)
{
	*status = ANL_OK;
}

void
aeaspect_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
aeaspect_ana(int *nevent, int *eventid, int *status)
{
	int istat;
	char *file;

	file = get_file_name(com.mod_file_list, &com.file_num);
	if ( NULL == file ) {
		fflush(NULL);
		if ( 0 != com.file_num ) {
			printf("\n");
		}
		printf("\
Finished.\n");
		fflush(NULL);
		*status = ANL_QUIT;
		return;
	}

	fflush(NULL); printf("\
 [%2d] Processing '%s'\n", com.file_num, file);
	fflush(NULL);

	istat = process_file(file);
	if ( istat ) {
		*status = ANL_ERROR;
		return;
	}

	*status = ANL_OK;
}

void
aeaspect_endrun(int *status)
{
	*status = ANL_OK;
}

void
aeaspect_exit(int *status)
{
	*status = ANL_OK;
}
