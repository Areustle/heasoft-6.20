/*
 SimASTE_PhotonRead.c
   SimASTE module : Photon Reader

  1998-02-24	version 1.00	Y.ISHISAKI
	coded first

  1998-02-24	version 1.01	Y.ISHISAKI
	include "unistd.h" for SunOS to define SEEK_SET

  1998-09-10	version 1.10	Y.ISHISAKI
	work with ftools parameter interface
	add/change FITS keywords

  1999-01-22	version 1.20	Y.ISHISAKI
   Read TSTART, TSTOP, EXPOSURE keywords from FITS header and put BNK

  2006-04-09 version 1.3		Y.ISHISAKI
	increase name[256] -> [1024]

  2006-05-28 version 1.4		Y.ISHISAKI
	increase name[1024] -> [PIL_LINESIZE]
	add "gtifile", "date_obs", "date_end", "attitude" parameters
	add EVS "SimASTE:XIS:GTI_WRAP", "SimASTE:XIS:ATT_ERROR"
	add read_gtifile(), open_attitude() functions

  2006-07-25 version 2.0		Y.ISHISAKI
	print error in reading parameters
	use aste_gti_xxx()

  2006-08-02 version 2.1		Y.ISHISAKI
	read ea1, ea2, ea3 if attitude=none, which is moved from SimASTE_Root
	read RA_NOM, DEC_NOM from attitude for skyref when pointint=auto

  2006-08-05 version 2.2		Y.ISHISAKI
	use aefits_datestr2attimeD/attimeD2datestr() in aeFitsHeaderUtil-3.2
	fix printf format for RA (%6.3f -> %06.3f), DEC (%5.2f -> %05.2f)
	add write_history(), BnkGet/Put SimASTE:WRITE_HISTORY:FUNC in _init()
	read GEOMAREA keyword from photon file, BnkPut SimASTE:GEOMAREA
	BnkPut SimASTE:N_PHOTON
	add enable_photongen parameter, to work with SimASTE_PhotonGen

  2008-07-31 version 2.3		Y.ISHISAKI
	bug fix in calculating photon_time when GTI wraparound in _ana()
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <unistd.h>		/* to define SEEK_SET for SunOS */
#include "fitsio.h"
#include "anl.h"
#include "cli.h"
#include "com.h"
#include "bnk.h"
#include "evs.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_time.h"
#include "aste_att.h"
#include "aste_gti.h"
#include "aeFitsHeaderUtil.h"
#include "SimASTE.h"

static char pname[] = "SimASTE_PhotonRead";
char SimASTE_PhotonRead_version[] = "version 2.3";

enum skyref_mode {
	SKYREF_MODE_AUTO,
	SKYREF_MODE_USER
};

/* followings are examples of input paraeters for SimASTE_PhotonRead */
static struct {

	int enable_photongen;

	TELDEF *teldef;

	enum skyref_mode pointing;	/* auto/user */
	SKYREF skyref;

	struct photon_file {
		FILE *fp;
		char name[PIL_LINESIZE];
		struct fitsinfo {
			fitsfile *fp;
			long nrow;
			struct {
				int ti, en, ra, de;
			} col;
		} fits;
		long irow;
		struct photon_info {
			double ti, en, ra, de;
		} val;
		double geomarea;
	} p[8];
	int end_of_file;
	double tstart, tstop, exposure, last_event_time;
	double n_photon;
	double geomarea;

	char gtifile[PIL_LINESIZE];
	char date_obs[PIL_LINESIZE];
	char date_end[PIL_LINESIZE];
	GTI_DATA gti;
	int gti_wraparound;

	char attitude[PIL_LINESIZE];
	AtEulerAng ea_deg, ea_default;
	ATTFILE *ap;
	int att_error;

	int (*prev_write_history)(fitsfile *);

} com = {
	ANL_NO,				/* enable_photongen */
	NULL,				/* teldef */
	SKYREF_MODE_AUTO,	/* pointing */
	{ 0.0, 0.0, 0.0 },	/* skyref */
	{
		{ NULL, "photon.list", { NULL, 0L, {1,2,3,4} }, 0L, {0,0,0,0}, 0.0 },
		{ NULL, "none", { NULL, 0L, {1,2,3,4} }, 0L, {0,0,0,0}, 0.0 },
		{ NULL, "none", { NULL, 0L, {1,2,3,4} }, 0L, {0,0,0,0}, 0.0 },
		{ NULL, "none", { NULL, 0L, {1,2,3,4} }, 0L, {0,0,0,0}, 0.0 },
		{ NULL, "none", { NULL, 0L, {1,2,3,4} }, 0L, {0,0,0,0}, 0.0 },
		{ NULL, "none", { NULL, 0L, {1,2,3,4} }, 0L, {0,0,0,0}, 0.0 },
		{ NULL, "none", { NULL, 0L, {1,2,3,4} }, 0L, {0,0,0,0}, 0.0 },
		{ NULL, "none", { NULL, 0L, {1,2,3,4} }, 0L, {0,0,0,0}, 0.0 }
	},
	0,						/* end_of_file */
	0.0, 0.0, 0.0, 0.0,		/* tstart, tstop, exposure, last_event_time */
	0.0,					/* n_photon */
	0.0,					/* geomarea */

	"none",					/* gtifile */
	"2000-01-01T00:00:00",	/* date_obs */
	"2000-01-01T00:00:00",	/* date_end */
	{ 0.0, 0.0, 0.0, 0.0, 0, NULL, NULL },	/* gti */
	0,						/* gti_wraparound */

	"none",					/* attitude */
	{ 0.0, 0.0, 0.0 },		/* ea_deg */
	{ 0.0, 0.0, 0.0 },		/* ea_default */
	NULL,					/* ap */
	0,						/* att_error */

	NULL					/* prev_write_history */
};

static int
read_gtifile(char *gtifile)
{
	int istat;
	AtTimeD attime;

	com.gti.ngti = 0;
	com.gti.ontime = 0.0;
	com.gti_wraparound = 0;

	if ( 0 == CLstricmp("none", gtifile) ) {

		istat = aefits_datestr2attimeD(com.date_obs, &attime);
		if ( istat ) {
			anl_msg_error("\
%s: invalid format of date_obs='%s'\n", pname, com.date_obs);
			return istat;
		}
		com.gti.tstart = attimeD2aste(&attime);
		istat = aefits_datestr2attimeD(com.date_end, &attime);
		if ( istat ) {
			anl_msg_error("\
%s: invalid format of date_end='%s'\n", pname, com.date_end);
			return istat;
		}
		com.gti.tstop = attimeD2aste(&attime);
		com.gti.telapse = com.gti.tstop - com.gti.tstart;
		com.gti.ontime = com.gti.telapse;
		com.gti.ngti = 1;
		com.gti.start = &com.gti.tstart;
		com.gti.stop = &com.gti.tstop;

		return 0;
	}

	anl_msg_info("\
%s: reading gtifile '%s'\n", pname, gtifile);
	istat = aste_gti_read(&com.gti, gtifile);
	if ( istat ) {
		return istat;
	}

	if ( com.gti.start == com.gti.stop ) {	/* TIME column */
		free(com.gti.start);
		com.gti.ontime = com.gti.telapse;
		com.gti.ngti = 1;
		com.gti.start = &com.gti.tstart;
		com.gti.stop = &com.gti.tstop;
	}

	anl_msg_info("\
   tstart=%.1f, tstop=%.1f, ontime=%.1f, ngti=%d\n",
		com.gti.tstart, com.gti.tstop, com.gti.ontime, com.gti.ngti);

	return 0;
}

static int
open_attitude(char *attitude)
{
	com.ap = NULL;
	com.att_error = 0;

	if ( 0 == CLstricmp("none", attitude) ) {
		return 0;
	}

	if ( 0.0 == com.gti.ontime ) {
		anl_msg_warning("\
%s: WARNING: ONTIME=0.0, ignoring attitude\n", pname);
		return 0;
	}

	anl_msg_info("\
%s: reading attitude '%s'\n", pname, attitude);
	com.ap = aste_att_init(attitude);
	if ( NULL == com.ap ) {
		return -1;
	}

	return 0;
}

static double
photon_time_in_GTI(GTI_DATA *gp, double photon_time)
{
	int i;
	double t0, t1;
	int ngti = gp->ngti;
	double *start = gp->start;
	double *stop = gp->stop;

	for (i = 0; i < ngti; i++) {
		t0 = start[i];
		t1 = stop[i];
		if ( t0 + photon_time <= t1 ) {
			photon_time = t0 + photon_time;
			break;
		}
		photon_time = photon_time - (t1 - t0);
	}
	if ( i == ngti ) {
		photon_time = gp->tstop;
	}

	return photon_time;
}

static int
write_history(fitsfile *fp)
{
	int i, istat;
	char history[PIL_LINESIZE + FLEN_VALUE];
	AtRightAscension ra;
	AtDeclination dec;

	if ( NULL != com.prev_write_history &&
		 write_history != com.prev_write_history ) {
		istat = (*com.prev_write_history)(fp);
		if ( istat ) {
			return istat;
		}
	}

	istat = SimASTE_write_history_pname(fp, pname);

	if ( com.enable_photongen ) {
		sprintf(history, "\
  enable_photongen=yes");
		fits_write_history(fp, history, &istat);
	} else {
		sprintf(history, "\
  enable_photongen=no");
		fits_write_history(fp, history, &istat);
		for (i = 0; i < sizeof(com.p)/sizeof(*com.p); i++) {
			struct photon_file *p = &com.p[i];
			if ( NULL == p->fits.fp && NULL == p->fp ) {
				continue;
			}
			sprintf(history, "\
  infile%d='%s'", i+1, p->name);
			fits_write_history(fp, history, &istat);
		}

		sprintf(history, "\
    GEOMAREA=%.4f cm2", com.geomarea);
		fits_write_history(fp, history, &istat);
	}

	sprintf(history, "\
  pointing=%s", (SKYREF_MODE_AUTO == com.pointing) ? "AUTO" : "USER");
	fits_write_history(fp, history, &istat);

	atDegToRA(com.skyref.alpha, &ra);
	sprintf(history, "\
  ref_alpha=%.4f (%02dh%02dm%06.3fs)", com.skyref.alpha,
		ra.hour, ra.min, ra.sec);
	fits_write_history(fp, history, &istat);

	atDegToDec(com.skyref.delta, &dec);
	sprintf(history, "\
  ref_delta=%.4f (%s%02dd%02dm%05.2fs)", com.skyref.delta,
		(dec.sign < 0) ? "-" : "+", dec.deg, dec.min, dec.sec);
	fits_write_history(fp, history, &istat);

	sprintf(history, "\
  ref_roll=%.4f", com.skyref.roll);
	fits_write_history(fp, history, &istat);

	sprintf(history, "\
  gtifile='%s'", com.gtifile);
	fits_write_history(fp, history, &istat);
	if ( 0 == CLstricmp("NONE", com.gtifile) ) {
		sprintf(history, "\
  date_obs='%s' (t=%.1f)", com.date_obs, com.gti.tstart);
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  date_end='%s' (t=%.1f)", com.date_end, com.gti.tstop);
		fits_write_history(fp, history, &istat);
	}
	sprintf(history, "\
  attitude='%s'", com.attitude);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  ea1=%.4f  ea2=%.4f  ea3=%.4f",
		com.ea_deg.phi, com.ea_deg.theta, com.ea_deg.psi);
	fits_write_history(fp, history, &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

static void
show_parameter(char *title)
{
	int i;
	char buf[80];
	AtRightAscension ra;
	AtDeclination dec;

	anl_msg_always("\n");
	anl_msg_always(title, pname);
	anl_msg_always("\n\n");

	if ( com.enable_photongen ) {
		anl_msg_always("\
%4s%-20s'%s'\n", "", "ENABLE_PHOTONGEN", "YES");
	} else {
		anl_msg_always("\
%4s%-20s'%s'\n", "", "ENABLE_PHOTONGEN", "NO");
		for (i = 0; i < sizeof(com.p)/sizeof(*com.p); i++) {
			sprintf(buf, "INFILE%d", i+1);
			anl_msg_always("\
%4s%-20s'%s'\n", "", buf, com.p[i].name);
		}
	}

	switch ( com.pointing ) {
	case SKYREF_MODE_AUTO:
		anl_msg_always("\
%4s%-20s%s\n", "", "POINTING", "AUTO");
		break;
	case SKYREF_MODE_USER:
		anl_msg_always("\
%4s%-20s%s\n", "", "POINTING", "USER");
		break;
	default:
		anl_msg_always("\
%4s%-20s%s\n", "", "POINTING", "UNKNOWN");
	}

	atDegToRA(com.skyref.alpha, &ra);
	sprintf(buf, "%.4f (%02dh%02dm%06.3fs)", com.skyref.alpha,
		ra.hour, ra.min, ra.sec);
	anl_msg_always("\
  %4s%-20s%s\n", "", "REF_ALPHA (J2000)", buf);
	atDegToDec(com.skyref.delta, &dec);
	sprintf(buf, "%.4f (%s%02dd%02dm%05.2fs)", com.skyref.delta,
		(dec.sign < 0) ? "-" : "+", dec.deg, dec.min, dec.sec);
	anl_msg_always("\
  %4s%-20s%s\n", "", "REF_DELTA (J2000)", buf);
	anl_msg_always("\
  %4s%-20s%.4f\n", "", "REF_ROLL (deg)", com.skyref.roll);

	anl_msg_always("\
%4s%-20s'%s'\n", "", "GTIFILE", com.gtifile);
	if ( 0 == CLstricmp("NONE", com.gtifile) ) {
		anl_msg_always("\
%4s%-20s'%s' (t=%.1f)\n", "", "DATE_OBS", com.date_obs, com.gti.tstart);
		anl_msg_always("\
%4s%-20s'%s' (t=%.1f)\n", "", "DATE_END", com.date_end, com.gti.tstop);
	}
	anl_msg_always("\
%4s%-20s'%s'\n", "", "ATTITUDE", com.attitude);
	anl_msg_always("\
%4s%-20s%.4f %.4f %.4f\n", "", "EA1/2/3",
		com.ea_deg.phi, com.ea_deg.theta, com.ea_deg.psi);

	if ( 0 == com.enable_photongen ) {
		anl_msg_always("\
%4s%-20s%.4f (cm2)\n", "", "GEOMAREA", com.geomarea);
	}
}

static int
read_photon_file(struct photon_file *p)
{
	int anul;
	int irow = p->irow + 1;
	int istat = 0;
	struct photon_info *v = &p->val;

	if ( NULL != p->fits.fp ) {
		struct fitsinfo *f = &p->fits;
		if ( f->nrow < irow ) {
			istat = 0;
			fits_close_file(f->fp, &istat);
			f->fp = NULL;
			return -1;
		}
		if (
fits_read_col_dbl(f->fp, f->col.ti, irow, 1, 1, 0, &v->ti, &anul, &istat) ||
fits_read_col_dbl(f->fp, f->col.en, irow, 1, 1, 0, &v->en, &anul, &istat) ||
fits_read_col_dbl(f->fp, f->col.ra, irow, 1, 1, 0, &v->ra, &anul, &istat) ||
fits_read_col_dbl(f->fp, f->col.de, irow, 1, 1, 0, &v->de, &anul, &istat) ||
			 0 ) {
			anl_msg_error("\
%s: '%s' read error (irow=%d)\n", pname, p->name, irow);
			istat = 0;
			fits_close_file(f->fp, &istat);
			f->fp = NULL;
			return -1;
		}
	} else if ( NULL != p->fp ) {
		char line[256];
		do {
			if ( NULL == fgets(line, sizeof(line), p->fp) ) {
				fclose(p->fp);
				p->fp = NULL;
				return -1;
			}
		} while ( '#' == line[0] );
		if (4 != sscanf(line,"%lf%lf%lf%lf", &v->ti, &v->en, &v->ra, &v->de)) {
			fclose(p->fp);
			p->fp = NULL;
			return -1;
		}
	} else {
		return -1;
	}
	p->irow++;

	return 0;
}

void
SimASTE_PhotonRead_startup(int *status)
{
	;
}

void
SimASTE_PhotonRead_com(int *status)
{
	static char *keytbl[] = {
		"SHOW",
		"FILENAME",
		"GTIFILE",
		"DATE_OBS",
		"DATE_END",
		"ATTITUDE",
		"EA_DEFAULT",
		"EXIT"
	};
	static char *help[] = {
		"Show current settings",
		"Set filename(s) to read",
		"Set GTI file",
		"Set observation start date",
		"Set observation end date",
		"Set attitude file",
		"Set default Euler angle",
		"Exit from this menu"
	};
	static int nkey = sizeof(keytbl) / sizeof(keytbl[0]);
	int i;
	char *k, buf[PIL_LINESIZE], prompt[32];

	if ( *status ) {	/* ftools */

		if ( PILGetBool(k="enable_photongen", &com.enable_photongen) ) {
			goto quit;
		} else if ( 0 == com.enable_photongen ) {

			for (i = 0; i < sizeof(com.p)/sizeof(*com.p); i++) {
				char parname[16];
				sprintf(parname, "infile%d", i+1);
				if ( PILGetFname(k=parname, com.p[i].name) ) {
					goto quit;
				}
				if ( 0 == CLstricmp("NONE", com.p[i].name) ) {
					break;
				}
			}

		}

		if ( PILGetString(k="pointing", buf) ) goto quit;
		if ( 0 == CLstricmp("AUTO", buf) ) {
			com.pointing = SKYREF_MODE_AUTO;
		} else if ( 0 == CLstricmp("USER", buf) ) {
			com.pointing = SKYREF_MODE_USER;
			if ( PILGetReal(k="ref_alpha", &com.skyref.alpha) ||
				 PILGetReal(k="ref_delta", &com.skyref.delta) ||
				 PILGetReal(k="ref_roll",  &com.skyref.roll) ) {
				goto quit;
			}
		}

		if ( PILGetFname (k="gtifile", com.gtifile) ||
			 PILGetString(k="date_obs", com.date_obs) ||
			 PILGetString(k="date_end", com.date_end) ||
			 PILGetFname (k="attitude", com.attitude) ||
			 PILGetReal  (k="ea1", &com.ea_deg.phi) ||
			 PILGetReal  (k="ea2", &com.ea_deg.theta) ||
			 PILGetReal  (k="ea3", &com.ea_deg.psi) ||
			 0 ) {
		quit:
			anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
			*status = ANL_QUIT;
			return;
		}
		*status = ANL_OK;
		return;
	}

	for (;;) {
		char *key;
		int ans[2];

		CMinquir(pname, nkey, keytbl, help, 1, ans);
		key = keytbl[ans[1]-1];
		if ( 0 == strcmp("SHOW", key) ) {
			show_parameter("%s:  *** show parameter ***");
		} else if ( 0 == strcmp("FILENAME", key) ) {
			for (i = 0; i < sizeof(com.p)/sizeof(*com.p); i++) {
				sprintf(prompt, "FILENAME%d (END='none')", i+1);
				CLtxtrd(key, com.p[i].name, sizeof(com.p[i].name));
				if ( 0 == CLstricmp("NONE", com.p[i].name) ) {
					break;
				}
			}
		} else if ( 0 == strcmp("GTIFILE", key) ) {
			CLtxtrd(key, com.gtifile, sizeof(com.gtifile));
		} else if ( 0 == strcmp("DATE_OBS", key) ) {
			CLtxtrd(key, com.date_obs, sizeof(com.date_obs));
		} else if ( 0 == strcmp("DATE_END", key) ) {
			CLtxtrd(key, com.date_end, sizeof(com.date_end));
		} else if ( 0 == strcmp("ATTITUDE", key) ) {
			CLtxtrd(key, com.attitude, sizeof(com.attitude));
		} else if ( 0 == strcmp("EA_DEFAULT", key) ) {
			CLfdprd(" phi (degree)", &com.ea_deg.phi);
			CLfdprd("theta(degree)", &com.ea_deg.theta);
			CLfdprd(" psi (degree)", &com.ea_deg.psi);
		} else if ( 0 == strcmp("EXIT", key) ) {
			break;
		}
	}

	*status = ANL_OK;
}

void
SimASTE_PhotonRead_init(int *status)
{
	static int (*func)(fitsfile *fp) = write_history;
	int i, istat, used, hdutype;
	unsigned char buf[10];
	char *k;
	double tstart, tstop, exposure;

	EvsDef("SimASTE_PhotonRead:BEGIN");
	EvsDef("SimASTE_PhotonRead:ENTRY");
	EvsDef("SimASTE_PhotonRead:OK");
	EvsDef("SimASTE_PhotonRead:GTI_WRAP");
	EvsDef("SimASTE_PhotonRead:ATT_ERROR");

	com.prev_write_history = NULL;
	BnkGet("SimASTE:WRITE_HISTORY:FUNC", sizeof(com.prev_write_history),
		&used, &com.prev_write_history);
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

	anl_msg_info("\n");
	if ( com.enable_photongen ) {
		anl_msg_info("\
%s: PhotonGen is enabled, skip reading photon files\n", pname);
		goto skip;
	}

	com.n_photon = 0.0;
	com.geomarea = 0.0;
	for (i = 0; i < sizeof(com.p)/sizeof(*com.p); i++) {
		struct photon_file *p = &com.p[i];

		if ( 0 == CLstricmp("NONE", p->name) ) continue;

		anl_msg_info("\
%s: opening infile%d='%s'\n", pname, i+1, p->name);

		p->fp = fopen(p->name, "r");
		if ( NULL == p->fp ) {
			anl_msg_error("\
%s: '%s' open failed\n", pname, p->name);
			continue;
		}
		memset(buf, 0, sizeof(buf));
		fread(buf, 1, sizeof(buf)-1, p->fp);
		if ( 0 == strncmp("SIMPLE  = ", (char *)buf, 7) ||
			 (0x1f == buf[0] && 0x8b == buf[1] && 0x08 == buf[2]) || /* .gz */
			 (0x1f == buf[0] && 0x9d == buf[1] && 0x90 == buf[2]) || /* .Z */
			 0 ) {
			struct fitsinfo *f = &p->fits;

			fclose(p->fp);
			p->fp = NULL;
			istat = 0;
			fits_open_file(&f->fp, p->name, READONLY, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, p->name, istat);
				continue;
			}
			fits_movabs_hdu(f->fp, 2, &hdutype, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_movabs_hdu() failed (%d)\n", pname, istat);
				continue;
			}
			if (
fits_get_colnum(f->fp, CASEINSEN, k="PHOTON_TIME", &f->col.ti, &istat) ||
fits_get_colnum(f->fp, CASEINSEN, k="PHOTON_ENERGY", &f->col.en, &istat) ||
fits_get_colnum(f->fp, CASEINSEN, k="RA", &f->col.ra, &istat) ||
fits_get_colnum(f->fp, CASEINSEN, k="DEC", &f->col.de, &istat) ||
				 0 ) {
				anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
				continue;
			}
			if (
fits_read_key_lng(f->fp, k="NAXIS2", &f->nrow, NULL, &istat) ||
fits_read_key_dbl(f->fp, k="TSTART", &tstart, NULL, &istat) ||
fits_read_key_dbl(f->fp, k="TSTOP", &tstop, NULL, &istat) ||
fits_read_key_dbl(f->fp, k="EXPOSURE", &exposure, NULL, &istat) ||
				 0 ) {
				anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
				continue;
			}
			if ( f->nrow < 1 ) {
				anl_msg_error("\
%s: '%s' no line to read\n", pname, p->name);
				istat = 0;
				fits_close_file(f->fp, &istat);
				f->fp = NULL;
				continue;
			}
			if ( 0.0 == com.exposure ) {
				com.tstart = tstart;
				com.tstop = tstop;
				com.exposure = exposure;
			} else {
				if ( tstart < com.tstart ) {
					com.tstart = tstart;
					com.exposure = com.tstop - com.tstart;
				}
				if ( com.tstop < tstop ) {
					com.tstop = tstop;
					com.exposure = com.tstop - com.tstart;
				}
			}
			p->irow = 0;
fits_read_key_dbl(f->fp, k="GEOMAREA", &p->geomarea, NULL, &istat);
			if ( istat ) {
				istat = 0;	/* ignore error for old photon files */
				p->geomarea = 0.0;
			} else {
				if ( 0.0 == com.geomarea ) {
					com.geomarea = p->geomarea;
				} else if ( com.geomarea != p->geomarea ) {
					anl_msg_warning("\
%s: WARNING: GEOMAREA=%.2f does not match previous of %.2f\n",
						pname, p->geomarea, com.geomarea);
					if ( com.geomarea < p->geomarea ) {
						com.geomarea = p->geomarea;
					}
					anl_msg_warning("\
%s: WARNING: using larger value of GEOMAREA=%.2f cm2\n", pname, com.geomarea);
				} else {
					com.geomarea = p->geomarea;
				}
			}
		} else {
			fseek(p->fp, 0L, SEEK_SET);
		}
	}

	BnkPut("SimASTE:GEOMAREA", sizeof(com.geomarea), &com.geomarea);

 skip:

	if ( read_gtifile(com.gtifile) ) {
		*status = ANL_QUIT;
		return;
	}

	BnkGet("SimASTE:TELDEF", sizeof(com.teldef), &used, &com.teldef);

	com.ea_default.phi = com.ea_deg.phi * DEG2RAD;
	com.ea_default.theta = com.ea_deg.theta * DEG2RAD;
	com.ea_default.psi = com.ea_deg.psi * DEG2RAD;

	if ( open_attitude(com.attitude) ) {
		*status = ANL_QUIT;
		return;
	}

	if ( NULL == com.ap ||
		 NULL == com.ap->fp ||
fits_read_key_dbl(com.ap->fp, "RA_NOM", &com.skyref.alpha, NULL, &istat) ||
fits_read_key_dbl(com.ap->fp, "DEC_NOM", &com.skyref.delta, NULL, &istat) ||
		 0 ) {
		anl_msg_warning("\
%s: WARNING: reading RA_NOM, DEC_NOM failed, calculate from EA\n", pname);
		aste_euler2skyref(com.teldef, &com.ea_default, &com.skyref);
		com.skyref.roll = 0.0;
	}

	BnkPut("SimASTE:EULER", sizeof(com.ea_default), &com.ea_default);
	BnkPut("SimASTE:SKYREF", sizeof(com.skyref), &com.skyref);

	show_parameter("%s:  *** show parameter ***");

	*status = ANL_OK;
}

void
SimASTE_PhotonRead_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_PhotonRead_bgnrun(int *status)
{
	EvsSet("SimASTE_PhotonRead:BEGIN");
	*status = ANL_OK;
}

void
SimASTE_PhotonRead_ana(int nevent, int eventid, int *status)
{
	int i, ifile, gti_lap, used;
	struct photon_info *v;
	double photon_time;
	AtEulerAng ea;

	EvsfSetM("SimASTE_PhotonRead:ENTRY");

/* check if photongen is enabled */
	if ( com.enable_photongen ) {
	BnkfGetM("SimASTE:PHOTON_TIME", sizeof(photon_time), &used, &photon_time);
		goto skip;
	}

/* find appropreate photon file */
	ifile = -1;
	for (i = 0; i < sizeof(com.p)/sizeof(*com.p); i++) {
		struct photon_file *p = &com.p[i];

		if ( NULL == p->fits.fp && NULL == p->fp ) {
			continue;
		}
		if ( 0 == p->irow && -1 == read_photon_file(p) ) {
			continue;
		}
		if ( -1 == ifile || p->val.ti < com.p[ifile].val.ti ) {
			ifile = i;
		}
	}

	if ( -1 == ifile ) {
		com.end_of_file = 1;
		*status = ANL_QUIT;
		return;
	}

/* read photon file */
	v = &com.p[ifile].val;
	read_photon_file(&com.p[ifile]);

	if ( 0.0 == com.exposure ) {	/* read from text file */
		if ( 0.0 == com.tstart && 0.0 == com.tstop ) {
			com.tstart = com.tstop = v->ti;
		} else {
			if ( v->ti < com.tstart ) com.tstart = v->ti;
			if ( com.tstop < v->ti ) com.tstop = v->ti;
		}
	}

/* put data to BNK */
	com.n_photon += 1.0;
	BnkfPutM("SimASTE:N_PHOTON", sizeof(com.n_photon), &com.n_photon);
	BnkfPutM("SimASTE:PHOTON_ENERGY", sizeof(v->en), &v->en);
	BnkfPutM("SimASTE:RA", sizeof(v->ra), &v->ra);
	BnkfPutM("SimASTE:DEC", sizeof(v->de), &v->de);
	photon_time = v->ti;

 skip:
	com.last_event_time = photon_time;

/* check for GTI */
	if ( 0.0 != com.gti.ontime ) {
		if ( com.gti.ontime < 0.0 ) {	/* 'date_end' is not set */
			photon_time += com.gti.tstart;
		} else {
			gti_lap = (int)(photon_time / com.gti.ontime);
			if ( 0 < gti_lap && gti_lap != com.gti_wraparound ) {
				anl_msg_warning("\
%s: WARNING: GTI wraparound = %d -> %d\n", pname, com.gti_wraparound, gti_lap);
				EvsfSetM("SimASTE_PhotonRead:GTI_WRAP");
				com.gti_wraparound = gti_lap;
			}
			photon_time = photon_time - gti_lap * com.gti.ontime;
			photon_time = photon_time_in_GTI(&com.gti, photon_time);
		}

		if ( NULL != com.ap ) {
			if ( aste_att_ea(com.ap, photon_time, &ea) ) {
				if ( 0 == com.att_error ) {
					anl_msg_warning("\
%s: WARNING: reading EULER failed, using default\n", pname);
				}
				ea = com.ea_default;
				EvsfSetM("SimASTE_PhotonRead:ATT_ERROR");
				com.att_error++;
			}
			BnkfPutM("SimASTE:EULER", sizeof(ea), &ea);
		}
	}

	BnkfPutM("SimASTE:PHOTON_TIME", sizeof(photon_time), &photon_time);

	EvsfSetM("SimASTE_PhotonRead:OK");

	*status = ANL_OK;
}

void
SimASTE_PhotonRead_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_PhotonRead_exit(int *status)
{
	int i, j, used;
	double *start, *stop;
	GTI_DATA *gp = &com.gti;

/* check if photongen is enabled */
	if ( com.enable_photongen ) {
		BnkGet("SimASTE:TSTART", sizeof(com.tstart), &used, &com.tstart);
		BnkGet("SimASTE:TSTOP", sizeof(com.tstop), &used, &com.tstop);
		BnkGet("SimASTE:EXPOSURE", sizeof(com.exposure), &used, &com.exposure);
	} else {

		for (i = 0; i < sizeof(com.p)/sizeof(*com.p); i++) {
			struct photon_file *p = &com.p[i];
			if ( p->irow ) {
				anl_msg_info("\
%s: %ld photons read from '%s'\n", pname, p->irow, p->name);
			}
		}

		if ( 0 == com.end_of_file ) {
			com.tstop = com.last_event_time;
			com.exposure = com.tstop - com.tstart;
		}

		BnkPut("SimASTE:EXPOSURE", sizeof(com.exposure), &com.exposure);

	}

/* process for GTI */
	if ( 0.0 != com.gti.ontime ) {
		if ( com.gti.ontime < 0.0 ) {	/* 'date_end' is not set */
			com.tstart += com.gti.tstart;
			com.tstop += com.gti.tstart;
		} else {
			com.tstart = photon_time_in_GTI(&com.gti, com.tstart);
			com.tstop = photon_time_in_GTI(&com.gti, com.tstop);
			start = com.gti.start;
			stop = com.gti.stop;
			for (i = 0; i < com.gti.ngti; i++) {
				if ( com.tstart <= stop[i] ) {
					start[i] = com.tstart;
					break;
				}
			}
			if ( 0 < i ) {
				com.gti.ngti -= i;
				for (j = 0; j < com.gti.ngti; j++) {
					start[j] = start[i];
					stop[j] = stop[i];
					i++;
				}
			}
			for (i = 0; i < com.gti.ngti; i++) {
				if ( start[i] <= com.tstop && com.tstop < stop[i] ) {
					stop[i] = com.tstop;
					com.gti.ngti = i + 1;
					break;
				}
			}
		}
		com.gti.tstart = com.tstart;
		com.gti.tstop = com.tstop;
		com.gti.telapse = com.tstop - com.tstart;
		com.gti.ontime = com.exposure;
	}

/* BnkPut final TSTART, TSTOP, GTI */
	BnkPut("SimASTE:TSTART", sizeof(com.tstart), &com.tstart);
	BnkPut("SimASTE:TSTOP", sizeof(com.tstop), &com.tstop);
	BnkPut("SimASTE:GTI_DATA:PTR", sizeof(gp), &gp);

/* close attitude file */
	if ( NULL != com.ap ) {
		aste_att_close(com.ap);
	}

	*status = ANL_OK;
}
