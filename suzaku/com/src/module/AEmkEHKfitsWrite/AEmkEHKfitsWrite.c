/*
  AEmkEHKfitsWrite.c

	1999/12/26 Y.ISHISAKI	version 1.0

	2000/01/31 Y.ISHISAKI	version 1.1
		add 'origin' parameter, and BnkDef "ASTE:FFF_ORIGIN"

	2004/03/14 Y.ISHISAKI	version 1.2
		use AtTimeD (since atFunctions-2.2) instead of AtTime
		include unistd.h for unlink()
		include "com.h", remove "cfortran.h"
		remove unused variables
		pass pointer of COM_STD_KEYS for comFitsHeaderUtil-1.6
		set hduclas1 = "TEMPORALDATA" & hduclas2 = "HKP"

	2005/06/07 Y.ISHISAKI	version 1.3
		modified to use aeFitsHeaderUtil-2.1
		separate create_ehk() from _init()
		write task_name, task_version in _init()

	2005/06/07 Y.ISHISAKI	version 1.4
		do not touch TLM_FILE, set ATT_FILE in _init()

	2005/06/26 Y.ISHISAKI	version 1.5
		use aefits_basename() instead of local SPLITPATH()
		use CLstricmp() instead of local STRICMP()
		copy ORIGIN, OBJECT, OBSERVER, RA_*, DEC_*, MEAN_EA* from attitude file
		BnkGet ASTE:MEAN_FOV
		write TDISPn keywords

	2005/10/24 Y.ISHISAKI	version 1.6
		add SAA_HXD, T_SAA_HXD columns
		use aste_orbit() instead of atFunctions
		write LEAPFILE, ORB_FILE, TELDEF keywords
		copy OBS_MODE,OBS_ID,OBS_REM keywords from input attitude file
		remove BNK "ASTE:FFF_ORIGIN", stop using aeGetFFFname()

	2005/12/04 Y.ISHISAKI	version 1.7
		use atRigidityD() instead of atRigidity()
		comment out atRigSet(), rigidity parameter handling

	2005/12/04 Y.ISHISAKI	version 1.8
		add TN_DY_NT, TN_SAA, TN_SAA_HXD, ZE_ANG, ZE_PHI

	2006/02/09 Y.ISHISAKI	version 1.9
		use geodetic (lon, lat) for atBrazil(), at***Brazil()

	2006/07/24 Y.ISHISAKI	version 2.0
		write task_name & task_version to CREATOR
		instead of pname & module_version in _init()
		use anl_msg_xxx() functions
		print error when PILGetXXX() failed

	2006/08/02 Y.ISHISAKI	version 2.1
		bug fix in message of PIL error

	2007/04/16 Y.ISHISAKI	version 2.2
		add 'COR2' column and 'rigidity' parameter

	2007/05/07 Y.ISHISAKI	version 2.3
		change unit of COR, COR2 from GeV -> GV
		use aste_att_ea() instead of aste_att_euler(), which is obsolete
		return with ANL_ERROR if aste_orbit() fails
		return with ANL_ERROR if aste_att_ea() fails

	2007/05/14 Y.ISHISAKI	version 2.4
		change *pname = "AEmkEHKfitsWrite" -> "aemkehk"
		PILGet 'outfile', 'rigidity', 'clobber' move to AEmkEHKtimeGen
		remove ".gz" ".Z" ".z" for file name written in header keyword
		insert RIGIDITY keyword after LEAPFILE
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
#include "aste_orbit.h"
#include "aste_time.h"
#include "aste_att.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "aemkehk";
char AEmkEHKfitsWrite_version[] = "version 2.4";

static AE_STD_KEYS stdkeys;

static struct {
/* AEmkEHKfitsWrite */
	char *outfile;
	fitsfile *ofp;
	long orow;
	ORBIT *orbit;
	TELDEF *teldef;
	ATTFILE *attfile;
	char *leapfile;
	double t0, t1;
	SKYREF mean_fov;
	AtVect mean_vect;
	AtRigData2 *rdp;
} com;

static char extname[] = "ASTE_EHK";

static struct ehk_table {
	char *ttype, *tform, *tunit, *tdisp, *comment;
	int index;
} ehktbl[] = {
	{"TIME",		"1D", "s",		"F16.6",
"mission time (s)" },
	{"YYYYMMDD",	"1J", "",		"I8.8",
"year*10000 + month*100 + day"},
	{"HHMMSS",		"1J", "",		"I6.6",
"hour*10000 + minute*100 + second"},
	{"EULER1",		"1E", "deg",	"F11.6",
"satellite Euler angles phi (deg)"},
	{"EULER2",		"1E", "deg",	"F11.6",
"satellite Euler angles theta (deg)"},
	{"EULER3",		"1E", "deg",	"F11.6",
"satellite Euler angles psi (deg)"},
	{"FOC_RA",		"1E", "deg",	"F11.6",
"R.A.(J2000) of FOC center pos (deg)"},
	{"FOC_DEC",		"1E", "deg",	"F11.6",
"DEC.(J2000) of FOC center pos (deg)"},
	{"FOC_ROLL",	"1E", "deg",	"F11.6",
"roll angle of FOC coordinates (deg)"},
	{"DLT_RA",		"1E", "arcmin",	"F8.3",
"difference from mean R.A. (arcmin)"},
	{"DLT_DEC",		"1E", "arcmin",	"F8.3",
"difference from mean DEC. (arcmin)"},
	{"DLT_ROLL",	"1E", "deg",	"F8.3",
"difference from mean roll angle (deg)"},
	{"ANG_DIST",	"1E", "arcmin",	"F8.3",
"distance from mean pointing pos (arcmin)"},
	{"SAT_ALT",		"1E", "km",		"F11.6",
"altitude of satellite orbit from earth (km)"},
	{"SAT_LON",		"1E", "deg",	"F11.6",
"longitude of satellite orbit (deg)"},
	{"SAT_LAT",		"1E", "deg",	"F11.6",
"latitude of satellite orbit (deg)"},
	{"ELV",			"1E", "deg",	"F8.3",
"earth elevation of FOC center pos (deg)"},
	{"DYE_ELV",		"1E", "deg",	"F8.3",
"day earth elev. of FOC center pos (deg)"},
	{"NTE_ELV",		"1E", "deg",	"F8.3",
"night earth elev. of FOC center pos (deg)"},
	{"SUN_ALT",		"1E", "deg",	"F8.3",
"altitude of the sun from the earth rim (deg)"},
	{"T_DY_NT",		"1E", "s",		"F9.3",
"time after day <-> night transision (s)"},
	{"TN_DY_NT",	"1E", "s",		"F9.3",
"time to next day <-> night transision (s)"},
	{"COR",			"1E", "GV",	"F8.3",
"cut off rigidity (GV) with old table"},
	{"COR2",		"1E", "GV",	"F8.3",
"cut off rigidity (GV) with new table"},
	{"SAA",			"1B", "",		NULL,
"passage of South Atlantic Anomaly (0->3:deep)"},
	{"T_SAA",		"1E", "s",		"F9.3",
"time after SAA passage (s)"},
	{"TN_SAA",		"1E", "s",		"F9.3",
"time to next SAA passage (s)"},
	{"SAA_HXD",		"1B", "",		NULL,
"passage of South Atlantic Anomaly for HXD"},
	{"T_SAA_HXD",	"1E", "s",		"F9.3",
"time after SAA passage for HXD (s)"},
	{"TN_SAA_HXD",	"1E", "s",		"F9.3",
"time to next SAA passage for HXD (s)"},
	{"ZGMAG_ANG",	"1E", "deg",	"F8.3",
"z-axis angle of geomagnetic field (deg)"},
	{"ZGMAG_PHI",	"1E", "deg",	"F8.3",
"z-axis roll of geomagnetic field (deg)"},
	{"ZE_ANG",		"1E", "deg",	"F8.3",
"z-axis angle to center of the Earth (deg)"},
	{"ZE_PHI",		"1E", "deg",	"F8.3",
"z-axis roll to Earth center direction (deg)"},
	{NULL,NULL,NULL,NULL,NULL}
};

static char *
get_filename_nogz(char *filename)
{
	char *filename_nogz;
	char *base = aefits_basename(filename);
	int len = strlen(base);

	if ( 3 < len && 0 == strcmp(".gz", &base[len-3]) ) {
		filename_nogz = strdup(base);
		if ( NULL != filename_nogz ) {
			filename_nogz[len-3] = '\0';
			return filename_nogz;
		}
	} else if ( 2 < len && 0 == strcmp(".Z", &base[len-2]) ) {
		filename_nogz = strdup(base);
		if ( NULL != filename_nogz ) {
			filename_nogz[len-2] = '\0';
			return filename_nogz;
		}
	} else if ( 2 < len && 0 == strcmp(".z", &base[len-2]) ) {
		filename_nogz = strdup(base);
		if ( NULL != filename_nogz ) {
			filename_nogz[len-2] = '\0';
			return filename_nogz;
		}
	}

	return base;
}

static double
delta_phi(double phi1, double phi0)
{
	double dphi = phi1 - phi0;
	while ( 180.0 < dphi ) {
		dphi -= 180.0;
	}
	while ( dphi <= -180.0 ) {
		dphi += 180.0;
	}
	return dphi;
}

static int
create_ehk(void)
{
	static char *rigidity_cm = "name of the cut-off rigidity file for COR2";

#define NF	(sizeof(ehktbl) / sizeof(*ehktbl) - 1)
	static int nf = NF;
	static char *ttype[NF], *tform[NF], *tunit[NF];
#undef NF

	int i, bnksize;
	fitsfile *fp;
	char *k, *rigidity_file, keyname[16], bnkname[64], card[FLEN_CARD];

	int tbltype = BINARY_TBL;
	int istat = 0;

/* print information message */
	anl_msg_info("\n\
%s: creating ehk file '%s'\n", pname, com.outfile);

/* create file */
	fits_create_file(&fp, com.outfile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_file('%s') failed\n", pname, com.outfile);
		return istat;
	}

/* create primary extension */
	fits_create_img(fp, BYTE_IMG, 0, NULL, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_img failed (%d)\n", pname, istat);
		return istat;
	}

/* create table & define BNK */
	for (i = 0; i < nf; i++) {
		ttype[i] = ehktbl[i].ttype;
		tform[i] = ehktbl[i].tform;
		tunit[i] = ehktbl[i].tunit;
		if ( 2 < i ) {
			sprintf(bnkname, "ASTE:EHK:%s", ttype[i]);
			if ( 0 == strcmp("1B", tform[i]) ) {
				bnksize = sizeof(int);
			} else if ( 0 == strcmp("1I", tform[i]) ) {
				bnksize = sizeof(int);
			} else if ( 0 == strcmp("1J", tform[i]) ) {
				bnksize = sizeof(int);
			} else {
				bnksize = sizeof(double);
			}
			BnkDef(bnkname, bnksize);
		}
	}
	fits_create_tbl(fp, tbltype, 0, nf, ttype, tform, tunit, extname, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_tbl() failed for '%s'\n", pname, com.outfile);
		return istat;
	}

/* modify comments */
	for (i = 0; i < nf; i++) {
		sprintf(keyname, "TTYPE%d", i+1);
		istat = aefits_modify_comment(fp, keyname, ehktbl[i].comment);
		if ( istat ) {
			return istat;
		}
	}

/* set TDISPn keywords */
	for (i = 0; i < nf; i++) {
		char *tdisp;
		char key[16], comment[80];

		tdisp = ehktbl[i].tdisp;
		if ( NULL == tdisp || '\0' == *tdisp ) {
			continue;
		}
		sprintf(key, "TDISP%d", i+1);
		sprintf(comment, "display format of %s", ttype[i]);
		fits_write_key_str(fp, key, tdisp, comment, &istat);
		if ( istat ) {
			break;
		}
	}
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key('TDISPn') failed (%d)\n", pname, istat);
		return istat;
	}

/* write standard header keywords */
	istat = aeWriteStdKeys(fp, &stdkeys);
	if ( istat ) {
		anl_msg_error("\
%s: aeWriteStdKeys() failed (%d)\n", pname, istat);
		return istat;
	}

/* insert RIGIDITY keyword after LEAPFILE */
	if ( fits_read_card(fp, k="LEAPFILE", card, &istat) ) {
		anl_msg_error("\
%s: fits_read_card('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}
	rigidity_file = get_filename_nogz(com.rdp->filename);
	fits_insert_key_str(fp, k="RIGIDITY", rigidity_file, rigidity_cm, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_insert_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

/* keep keyword space for 'DATE' & 'CHECKSUM' */
	if ( fits_write_date(fp, &istat) ) {
		anl_msg_error("\
%s: fits_write_date() failed (status=%d)\n", pname, istat);
		return istat;
	}
	if ( fits_write_chksum(fp, &istat) ) {
		anl_msg_error("\
%s: fits_write_chksum failed (%d)\n", pname, istat);
		return istat;
	}

/* initialize row number */
	com.ofp = fp;
	com.orow = 1;

	return 0;
}

static int
copy_keys(fitsfile *ofp, ATTFILE *attfile)
{
	static char *copy_keys[] = {
		"ORIGIN",
		"OBS_MODE",
		"OBS_ID",
		"OBSERVER",
		"OBJECT",
		"OBS_REM",
		"RA_OBJ",
		"DEC_OBJ",
		"RA_NOM",
		"DEC_NOM",
		"PA_NOM",
		"MEAN_EA1",
		"MEAN_EA2",
		"MEAN_EA3",
		NULL
	};

	int i;
	char *key, card[FLEN_CARD];

	int istat = 0;

	if ( NULL == attfile->fp ) {
		anl_msg_warning("\
%s: WARNING: no header keywords in attitude file\n", pname);
		return -1;
	}

	for (i = 0; NULL != copy_keys[i]; i++) {
		key = copy_keys[i];
		fits_read_card(attfile->fp, key, card, &istat);
		if ( istat ) {
			anl_msg_warning("\
%s: WARNING: no '%s' keyword in attitude file\n", pname, key);
			istat = 0;
			continue;
		}
		fits_update_card(ofp, key, card, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_update_card('%s') failed (%d)\n", pname, key, istat);
			return istat;
		}
	}

	return 0;
}

void
AEmkEHKfitsWrite_startup(int *status)
{
	*status = ANL_OK;
}

void
AEmkEHKfitsWrite_com(int *status)
{
	*status = ANL_OK;
}

void
AEmkEHKfitsWrite_init(int *status)
{
	static char creator[80];	/* must be statically declared */

	int used;
	char *k, *task_name, *task_version;

	int istat = 0;

	task_name = anl_task_name();
	task_version = anl_task_version();

	com.ofp = NULL;
	used = 0;
	BnkGet(k="ASTE:EHK:OFILE_NAME:PTR", sizeof(char *), &used, &com.outfile);
	if ( sizeof(com.outfile) != used ) {
		goto bnk_error;
	}
	BnkGet(k="ASTE:ORBIT:PTR", sizeof(com.orbit), &used, &com.orbit);
	if ( sizeof(com.orbit) != used ) {
		goto bnk_error;
	}
	BnkGet(k="ASTE:ATTFILE:PTR", sizeof(com.attfile), &used, &com.attfile);
	if ( sizeof(com.attfile) != used ) {
		goto bnk_error;
	}
	BnkGet(k="ASTE:TELDEF:PTR", sizeof(com.teldef), &used, &com.teldef);
	if ( sizeof(com.teldef) != used ) {
		goto bnk_error;
	}
	BnkGet(k="ASTE:LEAPFILE:PTR", sizeof(com.leapfile), &used, &com.leapfile);
	if ( sizeof(com.leapfile) != used ) {
		goto bnk_error;
	}
	BnkGet(k="ASTE:RIGIDITY:PTR", sizeof(com.rdp), &used, &com.rdp);
	if ( sizeof(com.rdp) != used ) {
		goto bnk_error;
	}

	aeSetDefaultKeywordValues(&stdkeys);
	stdkeys.hduclas1 = "TEMPORALDATA";
	stdkeys.hduclas2 = "HKP";
	sprintf(creator, "%s %s", task_name, task_version);
	stdkeys.creator = creator;
	stdkeys.leapfile = get_filename_nogz(com.leapfile);
	stdkeys.att_file = get_filename_nogz(com.attfile->filename);
	stdkeys.orb_file = get_filename_nogz(com.orbit->orbit_file);
	stdkeys.teldef   = get_filename_nogz(com.teldef->actual_filename);

	istat = create_ehk();
	if ( istat ) {
		goto quit;
	}

	BnkPut("ASTE:EHK:OFP", sizeof(com.ofp), &com.ofp);

	BnkGet("ASTE:MEAN_FOV", sizeof(com.mean_fov), &used, &com.mean_fov);
	atPolDegToVect(1.0, com.mean_fov.alpha, com.mean_fov.delta, com.mean_vect);

	copy_keys(com.ofp, com.attfile);

	istat = aefits_write_module_history(com.ofp, pname);
	if ( istat ) {
		goto quit;
	}

	*status = ANL_OK;
	return;

 bnk_error:
	anl_msg_error("\
%s: BnkGet('%s') failed\n", pname, k);

 quit:
	*status = ANL_QUIT;
	return;
}

void
AEmkEHKfitsWrite_his(int *status)
{
	*status = ANL_OK;
}

void
AEmkEHKfitsWrite_bgnrun(int *status)
{
	fitsfile *fp;
	char *k;

	int used = 0;
	int istat = 0;

	BnkGet("ASTE:EHK:OFP", sizeof(fp), &used, &fp);
	if ( used == sizeof(fp) ) {

/* write TELDEF keyword manually */
		fits_update_key_str(com.ofp, k=stdkeys.keyname.teldef,
			stdkeys.teldef, stdkeys.comment.teldef, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, k, istat);
			*status = ANL_QUIT;
			return;
		}

	}

	*status = ANL_OK;
}

void
AEmkEHKfitsWrite_ana(int *nevent, int *eventid, int *status)
{
	static double last_aetime, last_dy_nt_time, next_dy_nt_time;
	static double last_saa_time, last_saa_hxd_time;
	static double next_saa_time, next_saa_hxd_time;
	static double last_sun_alt;
	static int last_saa, last_saa_hxd;

	int time_reset_flag;
	double aetime, mjd;
	AtTimeD attime;
	int yyyymmdd, hhmmss;
	int used, icol;
	AtEulerAng ea;
	double t0, t1, mjd0, alt0, alt1;
	AtVect vect, vSat0, vSun0;
	AtVect vSat, vSatG, vSun, vXRT, nvXRT, nvSun, vMag, vEarth, vSatG0;
	AtPolarVect pvSatG, geomag, pvEarth, pvSatG0;
	AtRotMat rm;
	SKYREF skyref;
	int occult, day_night;
	double elev[3];
	double lon, lat;
	double euler1, euler2, euler3;
	double foc_ra, foc_dec, foc_roll, dlt_ra, dlt_dec, dlt_roll, ang_dist;
	double sat_alt, sat_lon, sat_lat;
	double elv, dye_elv, nte_elv, sun_alt, t_dy_nt, tn_dy_nt, cor, cor2;
	double t_saa, tn_saa, t_saa_hxd, tn_saa_hxd;
	int saa, sis_saa, stt_saa, saa0, saa_hxd, saa_hxd0;
	double zgmag_ang, zgmag_phi, ze_ang, ze_phi;
	fitsfile *fp = com.ofp;

	int istat = 0;
	struct ehk_table *tbl = ehktbl;
	TELDEF *teldef = com.teldef;

	BnkfGetM("ASTE:TIME", sizeof(aetime), &used, &aetime);
	aste2attimeD(aetime, &attime);
	yyyymmdd = attime.yr*10000 + attime.mo*100 + attime.dy;
	hhmmss = attime.hr*10000 + attime.mn*100 + attime.sc;

	if ( 1 == com.orow ) {
		com.t0 = aetime;
	}
	com.t1 = aetime;

	time_reset_flag = 0;
	if ( 1 == com.orow ) {							/* first row */
		time_reset_flag = 1;
	} else if ( aetime < last_aetime ) {			/* time inversion */
		anl_msg_warning("\
%s: WARNING: time inversion %.3f -> %.3f\n", pname, last_aetime, aetime);
		time_reset_flag = 1;
	} else if ( 300.0 < aetime - last_aetime ) {	/* time jump of 10 min */
		time_reset_flag = 1;
	}

	icol = 1;
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &aetime, &istat); tbl++;
	fits_write_col_int(fp, icol++, com.orow, 1, 1, &yyyymmdd, &istat); tbl++;
	fits_write_col_int(fp, icol++, com.orow, 1, 1, &hhmmss, &istat); tbl++;

/* EULER1, EULER2, EULER3 */
	if ( aste_att_ea(com.attfile, aetime, &ea) ) {
		anl_msg_error("\
%s: aste_att_euler() failed at t=%.6f\n", pname, aetime);
		goto error;
	}
	euler1 = ea.phi * RAD2DEG;
	BnkfPut("ASTE:EHK:EULER1", &tbl->index, sizeof(euler1), &euler1);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &euler1, &istat); tbl++;
	euler2 = ea.theta * RAD2DEG;
	BnkfPut("ASTE:EHK:EULER2", &tbl->index, sizeof(euler1), &euler2);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &euler2, &istat); tbl++;
	euler3 = ea.psi * RAD2DEG;
	BnkfPut("ASTE:EHK:EULER3", &tbl->index, sizeof(euler1), &euler3);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &euler3, &istat); tbl++;
	aste_euler2skyref(teldef, &ea, &skyref);

/* FOC_RA, FOC_DEC, FOC_ROLL */
	foc_ra = skyref.alpha;
	BnkfPut("ASTE:EHK:FOC_RA", &tbl->index, sizeof(foc_ra), &foc_ra);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &foc_ra, &istat); tbl++;
	foc_dec = skyref.delta;
	BnkfPut("ASTE:EHK:FOC_DEC", &tbl->index, sizeof(foc_dec), &foc_dec);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &foc_dec, &istat); tbl++;
	foc_roll = skyref.roll;
	BnkfPut("ASTE:EHK:FOC_ROLL", &tbl->index, sizeof(foc_roll), &foc_roll);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &foc_roll, &istat); tbl++;

/* DLT_RA, DLT_DEC, DLT_ROLL, ANG_DIST */
	dlt_ra = 60.0 * delta_phi(skyref.alpha, com.mean_fov.alpha);
	BnkfPut("ASTE:EHK:DLT_RA", &tbl->index, sizeof(dlt_ra), &dlt_ra);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &dlt_ra, &istat); tbl++;
	dlt_dec = 60.0 * (skyref.delta - com.mean_fov.delta);
	BnkfPut("ASTE:EHK:DLT_DEC", &tbl->index, sizeof(dlt_dec), &dlt_dec);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &dlt_dec, &istat); tbl++;
	dlt_roll = delta_phi(skyref.roll, com.mean_fov.roll);
	BnkfPut("ASTE:EHK:DLT_ROLL", &tbl->index, sizeof(dlt_roll), &dlt_roll);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &dlt_roll, &istat); tbl++;
	atPolDegToVect(1.0, skyref.alpha, skyref.delta, vXRT);
	atAngDistance(vXRT, com.mean_vect, &ang_dist);
	ang_dist = ang_dist * RAD2ARCMIN;
/*	printf("alpha=%.6f delta=%.6f ang_dist=%.9f\n", skyref.alpha, skyref.delta, ang_dist);*/
	BnkfPut("ASTE:EHK:ANG_DIST", &tbl->index, sizeof(ang_dist), &ang_dist);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &ang_dist, &istat); tbl++;

/* SAT_ALT, SAT_LON, SAT_LAT */
	mjd = aste2mjd(aetime);
	istat = aste_orbit(com.orbit, aetime, vSat, NULL);
	if ( istat ) goto error;
	atGeodcr(mjd, vSat, &sat_alt, &lon, &lat);
	sat_lon = lon * RAD2DEG;
	sat_lat = lat * RAD2DEG;
	BnkfPut("ASTE:EHK:SAT_ALT", &tbl->index, sizeof(sat_alt), &sat_alt);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &sat_alt, &istat); tbl++;
	BnkfPut("ASTE:EHK:SAT_LON", &tbl->index, sizeof(sat_lon), &sat_lon);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &sat_lon, &istat); tbl++;
	BnkfPut("ASTE:EHK:SAT_LAT", &tbl->index, sizeof(sat_lat), &sat_lat);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &sat_lat, &istat); tbl++;

/* ELV, DYE_ELV, NTE_ELV, SUN_ALT */
	atNormVect(vXRT, nvXRT);
	atSun(mjd, vSun);
	atEarthElev(vSat, nvXRT, vSun, &occult, elev);
	elv = elev[0] * RAD2DEG;
	dye_elv = elev[1] * RAD2DEG;
	nte_elv = elev[2] * RAD2DEG;
	atNormVect(vSun, nvSun);
	atEarthOccult(vSat, nvSun, vSun, &day_night, &sun_alt);
	sun_alt = sun_alt * RAD2DEG;
/*	printf("atEarthElev : elv=%.6f, dye_elv=%.6f, nte_elv=%.6f, sun_alt=%.6f\n", elv, dye_elv, nte_elv, sun_alt);*/
	BnkfPut("ASTE:EHK:ELV", &tbl->index, sizeof(elv), &elv);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &elv, &istat); tbl++;
	BnkfPut("ASTE:EHK:DYE_ELV", &tbl->index, sizeof(dye_elv), &dye_elv);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &dye_elv, &istat); tbl++;
	BnkfPut("ASTE:EHK:NTE_ELV", &tbl->index, sizeof(nte_elv), &nte_elv);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &nte_elv, &istat); tbl++;
	BnkfPut("ASTE:EHK:SUN_ALT", &tbl->index, sizeof(sun_alt), &sun_alt);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &sun_alt, &istat); tbl++;

/* T_DY_NT & TN_DY_NT */
	if ( time_reset_flag ) {

		t0 = aetime;
		alt0 = sun_alt;
		do  {
			t1 = t0;
			alt1 = alt0;
			t0 = t1 - 60.0;
			mjd0 = aste2mjd(t0);
			atSun(mjd0, vSun0);
			atNormVect(vSun0, nvSun);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atEarthOccult(vSat0, nvSun, vSun, &day_night, &alt0);
		} while ( 0 < alt0 * alt1 );
		last_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);

		t0 = aetime;
		alt0 = sun_alt;
		do  {
			t1 = t0;
			alt1 = alt0;
			t0 = t1 + 60.0;
			mjd0 = aste2mjd(t0);
			atSun(mjd0, vSun0);
			atNormVect(vSun0, nvSun);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atEarthOccult(vSat0, nvSun, vSun, &day_night, &alt0);
		} while ( 0 < alt0 * alt1 );
		next_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);

	}  else if ( last_sun_alt * sun_alt <= 0.0 ) {
		/* day <-> night transision occur */
		t1 = aetime;
		alt1 = sun_alt;
		t0 = last_aetime;
		alt0 = last_sun_alt;
		last_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);

		t0 = aetime;
		alt0 = sun_alt;
		do  {
			t1 = t0;
			alt1 = alt0;
			t0 = t1 + 60.0;
			mjd0 = aste2mjd(t0);
			atSun(mjd0, vSun0);
			atNormVect(vSun0, nvSun);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atEarthOccult(vSat0, nvSun, vSun, &day_night, &alt0);
		} while ( 0 < alt0 * alt1 );
		next_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);
	}
	last_sun_alt = sun_alt;
	t_dy_nt = aetime - last_dy_nt_time;
	tn_dy_nt = next_dy_nt_time - aetime;
	BnkfPut("ASTE:EHK:T_DY_NT", &tbl->index, sizeof(t_dy_nt), &t_dy_nt);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &t_dy_nt, &istat); tbl++;
	BnkfPut("ASTE:EHK:TN_DY_NT", &tbl->index, sizeof(tn_dy_nt), &tn_dy_nt);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &tn_dy_nt, &istat); tbl++;

/* COR */
	atGeodetic(mjd, vSat, vSatG);
	atVectToPol(vSatG, &pvSatG);
	atRigidityD(&pvSatG, &cor);
	BnkfPut("ASTE:EHK:COR", &tbl->index, sizeof(cor), &cor);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &cor, &istat); tbl++;

/* COR2 */
	atRigidity2(com.rdp, &pvSatG, &cor2);
	BnkfPut("ASTE:EHK:COR2", &tbl->index, sizeof(cor2), &cor2);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &cor2, &istat); tbl++;

/* SAA */
	atBrazil(pvSatG.lon, pvSatG.lat, &saa);
	if ( saa ) {
		saa = 1;
	}
	atSISBrazil(pvSatG.lon, pvSatG.lat, &sis_saa);
	if ( sis_saa ) {
		saa = 2;
	}
	atSTTBrazil(pvSatG.lon, pvSatG.lat, &stt_saa);
	if ( stt_saa ) {
		saa = 3;
	}
	BnkfPut("ASTE:EHK:SAA", &tbl->index, sizeof(saa), &saa);
	fits_write_col_int(fp, icol++, com.orow, 1, 1, &saa, &istat); tbl++;

/* T_SAA & TN_SAA */
	if ( time_reset_flag ) {
		t0 = aetime;
		saa0 = saa;
		while ( ! saa0 )  {
			t0 = t0 - 60.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
		}
		while ( saa0 ) {
			t0 = t0 + 1.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
			if ( 0 == saa0 ) {
				atSISBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
				if ( 0 == saa0 ) {
					atSTTBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
				}
			}
		}
		last_saa_time = t0;

		t0 = aetime;
		saa0 = saa;
		while ( ! saa0 )  {
			t0 = t0 + 60.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
		}
		while ( saa0 ) {
			t0 = t0 - 1.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
			if ( 0 == saa0 ) {
				atSISBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
				if ( 0 == saa0 ) {
					atSTTBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
				}
			}
		}
		next_saa_time = t0;

	} else if ( 0 == saa && last_saa ) {	/* saa out */

		t0 = last_aetime;
		saa0 = last_saa;
		while ( saa0 ) {
			t0 = t0 + 1.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
			if ( 0 == saa0 ) {
				atSISBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
				if ( 0 == saa0 ) {
					atSTTBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
				}
			}
		}
		last_saa_time = t0;

		t0 = aetime;
		saa0 = saa;
		while ( ! saa0 )  {
			t0 = t0 + 60.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
		}
		while ( saa0 ) {
			t0 = t0 - 1.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
			if ( 0 == saa0 ) {
				atSISBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
				if ( 0 == saa0 ) {
					atSTTBrazil(pvSatG0.lon, pvSatG0.lat, &saa0);
				}
			}
		}
		next_saa_time = t0;

	}
	last_saa = saa;
	if ( saa ) {
		t_saa = 0.0;
		tn_saa = 0.0;
	} else {
		t_saa = aetime - last_saa_time;
		tn_saa = next_saa_time - aetime;
	}
	BnkfPut("ASTE:EHK:T_SAA", &tbl->index, sizeof(t_saa), &t_saa);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &t_saa, &istat); tbl++;
	BnkfPut("ASTE:EHK:TN_SAA", &tbl->index, sizeof(tn_saa), &tn_saa);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &tn_saa, &istat); tbl++;

/* SAA_HXD */
	atHXDBrazil(pvSatG.lon, pvSatG.lat, &saa_hxd);
	if ( saa_hxd ) {
		saa_hxd = 1;
	}
	BnkfPut("ASTE:EHK:SAA_HXD", &tbl->index, sizeof(saa_hxd), &saa_hxd);
	fits_write_col_int(fp, icol++, com.orow, 1, 1, &saa_hxd, &istat); tbl++;

/* T_SAA_HXD & TN_SAA_HXD */
	if ( time_reset_flag ) {

		t0 = aetime;
		saa_hxd0 = saa_hxd;
		while ( ! saa_hxd0 )  {
			t0 = t0 - 60.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
		}
		while ( saa_hxd0 ) {
			t0 = t0 + 1.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
		}
		last_saa_hxd_time = t0;

		t0 = aetime;
		saa_hxd0 = saa_hxd;
		while ( ! saa_hxd0 )  {
			t0 = t0 + 60.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
		}
		while ( saa_hxd0 ) {
			t0 = t0 - 1.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
		}
		next_saa_hxd_time = t0;

	} else if ( 0 == saa_hxd && last_saa_hxd ) {	/* saa out */

		t0 = last_aetime;
		saa_hxd0 = last_saa_hxd;
		while ( saa_hxd0 ) {
			t0 = t0 + 1.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
		}
		last_saa_hxd_time = t0;

		t0 = aetime;
		saa_hxd0 = saa_hxd;
		while ( ! saa_hxd0 )  {
			t0 = t0 + 60.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
		}
		while ( saa_hxd0 ) {
			t0 = t0 - 1.0;
			mjd0 = aste2mjd(t0);
			istat = aste_orbit(com.orbit, t0, vSat0, NULL);
			if ( istat ) goto error;
			atGeodetic(mjd0, vSat0, vSatG0);
			atVectToPol(vSatG0, &pvSatG0);
			atHXDBrazil(pvSatG0.lon, pvSatG0.lat, &saa_hxd0);
		}
		next_saa_hxd_time = t0;

	}
	last_saa_hxd = saa_hxd;
	if ( saa_hxd ) {
		t_saa_hxd = 0.0;
		tn_saa_hxd = 0.0;
	} else {
		t_saa_hxd = aetime - last_saa_hxd_time;
		tn_saa_hxd = next_saa_hxd_time - aetime;
	}
	BnkfPut("ASTE:EHK:T_SAA_HXD", &tbl->index, sizeof(t_saa_hxd), &t_saa_hxd);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &t_saa_hxd, &istat); tbl++;
	BnkfPut("ASTE:EHK:TN_SAA_HXD",&tbl->index, sizeof(tn_saa_hxd),&tn_saa_hxd);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &tn_saa_hxd, &istat); tbl++;

/* ZGMAG_ANG, ZGMAG_PHI */
	atGeomagSet(mjd, 8);
	/* nmax=8 recommended for atFunctions <= v2.7, nmax ignored >= v2.8 */
	atGeomag(&pvSatG, vSat, vMag);
	atEulerToRM(&ea, rm);
	atRotVect(rm, vMag, vect);
	atVectToPol(vect, &geomag);
	zgmag_ang = geomag.lat * RAD2DEG + 90.0;
	zgmag_phi = geomag.lon * RAD2DEG;
	BnkfPut("ASTE:EHK:ZGMAG_ANG", &tbl->index, sizeof(zgmag_ang), &zgmag_ang);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &zgmag_ang, &istat); tbl++;
	BnkfPut("ASTE:EHK:ZGMAG_PHI", &tbl->index, sizeof(zgmag_phi), &zgmag_phi);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &zgmag_phi, &istat); tbl++;

/* ZE_ANG, ZE_PHI */
	atInvVect(vSat, vEarth);
	atRotVect(rm, vEarth, vect);
	atVectToPol(vect, &pvEarth);
	ze_ang = 90.0 - pvEarth.lat * RAD2DEG;	/* 0.0 if seeing Earth center */
	ze_phi = pvEarth.lon * RAD2DEG;
	BnkfPut("ASTE:EHK:ZE_ANG", &tbl->index, sizeof(ze_ang), &ze_ang);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &ze_ang, &istat); tbl++;
	BnkfPut("ASTE:EHK:ZE_PHI", &tbl->index, sizeof(ze_phi), &ze_phi);
	fits_write_col_dbl(fp, icol++, com.orow, 1, 1, &ze_phi, &istat); tbl++;

/* save aetime */
	last_aetime = aetime;

/* check cfitsio error */
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_col() failed at row=%ld (%d)\n", pname, com.orow, istat);
		*status = ANL_QUIT;
		return;
	}

	com.orow++;

	*status = ANL_OK;
	return;

 error:
	*status = ANL_ERROR;
	return;
}

void
AEmkEHKfitsWrite_endrun(int *status)
{
	*status = ANL_OK;
}

void
AEmkEHKfitsWrite_exit(int *status)
{
	int istat = 0;
	fitsfile *fp = com.ofp;

	if ( fp ) {
		stdkeys.tstart = com.t0;
		stdkeys.tstop = com.t1;

		istat = aeUpdateStdTimeKeys(fp, &stdkeys);

		if ( istat ) {
			*status = ANL_QUIT;
			return;
		}

		if ( fits_write_date(fp, &istat) ) {
			anl_msg_error("\
%s: fits_write_date() failed (status=%d)\n", pname, istat);
			*status = ANL_QUIT;
			return;
		}

		if ( fits_write_chksum(fp, &istat) ) {
			anl_msg_error("\
%s: fits_write_chksum() failed (%d)\n", pname, istat);
			*status = ANL_QUIT;
			return;
		}

		fits_close_file(fp, &istat);
		anl_msg_info("\
%s: '%s' created\n", pname, com.outfile);
	}

	*status = ANL_OK;
}
