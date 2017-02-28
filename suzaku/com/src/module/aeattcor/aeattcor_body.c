/*
	aeattcor.c

	2007/04/07 Y.ISHISAKI	version 1.0

	2007/05/07 Y.ISHISAKI	version 1.1
		refer JX-ISAS-SUZAKU-MEMO-2007-04 in comment

	2007/07/14 Y.ISHISAKI	version 1.2
		bug fix in calculating SGM_YOFF
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
#include "aste_time.h"
#include "aste_orbit.h"
#include "aste_gethk.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "aeattcor";
char aeattcor_version[] = "version 1.2";

static struct {
/* aeattcor */
	char infile[PIL_LINESIZE];
	char outfile[PIL_LINESIZE];
	char orbit[PIL_LINESIZE];
	char hkfile[PIL_LINESIZE];
	double hk_time_margin;
	int clobber;

	fitsfile *ifp;
	fitsfile *ofp;
	int mjdrefi;
	double mjdreff;

	ORBIT *obp;

	ASTE_HK *hkp;
	int id_T6;
	int id_T8;

} com;

/*
 * attitude correction function by
 * Y.Uchiyama, Y.Maeda (ISAS/JAXA) and R.Fujimoto (Kanazawa-U) in
 * "Correction for thermal wobbling of the Suzaku satellite:
 *	Progress made with V1.2 data", 2007.01.29, 2007.02.26 update
 */
static void
calc_xyoffset(double b, double t_86, double t_dy, double *xoffs, double *yoffs)
{
	*xoffs = -38.5*(b/60) - 9.25 + 0.4 * t_86;
	if ( t_dy < 0.0 ) {
		*yoffs = 10 + (7.0/1000)*t_dy;
	} else if ( t_dy < 2000 ) {
		*yoffs = 10 + (5.0/2000)*t_dy;
	} else {
		*yoffs = 45 - (15.0/1000)*t_dy;
	}
}

static void
correct_euler(double xoffs, double yoffs, double ea_old[3], double ea_new[3])
{
	static double focallen = 4750.0;	/* XRT focal length (mm) */
	static double foc_xscl = 0.024;		/* FOC X scale (mm/pixel) */
	static double foc_yscl = 0.024;		/* FOC Y scale (mm/pixel) */

	double norm, alpha, delta;
	AtVect vec_ecs;		/* pointing vector in celestial coordinates */
	AtVect vec_foc;		/* pointing vector in FOC coordinates */
	AtRotMat Aij, invAij;	/* rotation matrix to describe satellite aspect */

	AtEulerAng ea;

	ea.phi   = ea_old[0] * DEG2RAD;
	ea.theta = ea_old[1] * DEG2RAD;
	ea.psi   = ea_old[2] * DEG2RAD;

/* convert Euler angles to a rotation matrix, then obtain its inverse matrix */
	atEulerToRM(&ea, Aij);
	atInvRotMat(Aij, invAij);

/* convert to a pointing vector in FOC coordinate,
   flipping y-axis to make it look-down */
	vec_foc[0] = - (0.0 - xoffs) * foc_xscl;
	vec_foc[1] =   (0.0 - yoffs) * foc_yscl;
	vec_foc[2] =   focallen;

/* convert to the pointing vector in celestial coordinates,
   taking account of satellite attitudes */
	atRotVect(invAij, vec_foc, vec_ecs);

/* convert to the RA and DEC */
	atVectToPolDeg(vec_ecs, &norm, &alpha, &delta);

	ea_new[0] = alpha;			/* 0.0 <= alpha < 360.0 */
	ea_new[1] = 90.0 - delta;	/* -90 <= delta <= 90.0 */
	ea_new[2] = ea_old[2];
}

static void
show_parameter(void)
{
	printf("\n");
	printf("%s: *** show parameter ***\n", pname);
	printf("\n");
	printf("%20s   '%s'\n", "INFILE", com.infile);
	printf("%20s   '%s'\n", "OUTFILE", com.outfile);
	printf("%20s   '%s'\n", "ORBIT", com.orbit);
	printf("%20s   '%s'\n", "HKFILE", com.hkfile);
	printf("%20s   %.1f (s)\n", "HK_TIME_MARGIN", com.hk_time_margin);
	printf("%20s   %s\n", "CLOBBER", com.clobber ? "YES" : "NO");
	printf("\n");
}

static int
open_attitude_file(void)
{
	int hdutype;
	int morekeys;
	int istat;
	char *k;

	istat = aste_orbit_init(&com.obp, com.orbit);
	if ( istat ) {
		return istat;
	}

	com.hkp = aste_gethk_init(com.hkfile);
	if ( NULL == com.hkp ) {
		return -1;
	}
	com.id_T6 = aste_gethk_id(com.hkp, k="HK_XIS_RAD6_T1_CAL");
	if ( com.id_T6 < 0 ) {
		anl_msg_error("\
%s: aste_gethk_id('%s') failed\n", pname, k);
		return -1;
	}
	com.id_T8 = aste_gethk_id(com.hkp, k="HK_XIS_RAD8_T1_CAL");
	if ( com.id_T8 < 0 ) {
		anl_msg_error("\
%s: aste_gethk_id('%s') failed\n", pname, k);
		return -1;
	}

	fits_open_file(&com.ifp, com.infile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: '%s' open failed (%d)\n", pname, com.infile, istat);
		return istat;
	}

	if ( com.clobber ) {
		unlink(com.outfile);
	}
	fits_create_file(&com.ofp, com.outfile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_file('%s') failed (%d)\n", pname, com.outfile, istat);
		return istat;
	}

	morekeys = 0;
	fits_copy_hdu(com.ifp, com.ofp, morekeys, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_copy_hdu() failed for primary header (%d)\n", pname, istat);
		return istat;
	}

	fits_movrel_hdu(com.ifp, 1, &hdutype, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_movrel_hdu() failed for infile (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

static int
calc_t_dy(long irow, double aetime, double *t_dy)
{
	static double last_aetime = 0.0;
	static double last_dy_nt_time, next_dy_nt_time;
	static double last_sun_alt;

	int istat, time_reset_flag, day_night;
	double mjd, t0, t1, mjd0, alt0, alt1;
	double sun_alt, t_dy_nt, tn_dy_nt;
	AtVect vSat, vSun, nvSun, vSat0, vSun0;

	time_reset_flag = 0;
	if ( 1 == irow ) {							/* first row */
		time_reset_flag = 1;
	} else if ( aetime < last_aetime ) {		/* time inversion */
		anl_msg_warning("\
%s: WARNING: time inversion %.3f -> %.3f\n", pname, last_aetime, aetime);
		time_reset_flag = 1;
	} else if ( 300.0 < aetime - last_aetime ) {	/* time jump of 10 min */
		time_reset_flag = 1;
	}

	mjd = aste2mjdtt(aetime, com.mjdrefi, com.mjdreff);
	istat = aste_orbit(com.obp, aetime, vSat, NULL);
	if ( istat ) {
		anl_msg_error("\
%s: aste_orbit() failed at t=%f\n", pname, aetime);
		return istat;
	}

	atSun(mjd, vSun);
	atNormVect(vSun, nvSun);
	atEarthOccult(vSat, nvSun, vSun, &day_night, &sun_alt);
	sun_alt = sun_alt * RAD2DEG;

/* T_DY_NT & TN_DY_NT */
	if ( time_reset_flag ) {

		t0 = aetime;
		alt0 = sun_alt;
		do  {
			t1 = t0;
			alt1 = alt0;
			t0 = t1 - 60.0;
			mjd0 = aste2mjdtt(t0, com.mjdrefi, com.mjdreff);
			atSun(mjd0, vSun0);
			atNormVect(vSun0, nvSun);
			istat = aste_orbit(com.obp, t0, vSat0, NULL);
			if ( istat ) {
				anl_msg_error("\
%s: aste_orbit() failed at t=%f\n", pname, t0);
				return istat;
			}
			atEarthOccult(vSat0, nvSun, vSun, &day_night, &alt0);
		} while ( 0 < alt0 * alt1 );
		last_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);

		t0 = aetime;
		alt0 = sun_alt;
		do  {
			t1 = t0;
			alt1 = alt0;
			t0 = t1 + 60.0;
			mjd0 = aste2mjdtt(t0, com.mjdrefi, com.mjdreff);
			atSun(mjd0, vSun0);
			atNormVect(vSun0, nvSun);
			istat = aste_orbit(com.obp, t0, vSat0, NULL);
			if ( istat ) {
				anl_msg_error("\
%s: aste_orbit() failed at t=%f\n", pname, t0);
				return istat;
			}
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
			mjd0 = aste2mjdtt(t0, com.mjdrefi, com.mjdreff);
			atSun(mjd0, vSun0);
			atNormVect(vSun0, nvSun);
			istat = aste_orbit(com.obp, t0, vSat0, NULL);
			if ( istat ) {
				anl_msg_error("\
%s: aste_orbit() failed at t=%f\n", pname, t0);
				return istat;
			}
			atEarthOccult(vSat0, nvSun, vSun, &day_night, &alt0);
		} while ( 0 < alt0 * alt1 );
		next_dy_nt_time = (alt1*t0 - alt0*t1) / (alt1 - alt0);
	}
	last_sun_alt = sun_alt;
	last_aetime = aetime;

	t_dy_nt = aetime - last_dy_nt_time;
	tn_dy_nt = next_dy_nt_time - aetime;
	*t_dy = ( 0 <= sun_alt ) ? t_dy_nt : - tn_dy_nt;

	return 0;
}

static int
write_history(fitsfile *fp)
{
	char *task_name, *task_version, history[1024];
	int istat = 0;

	task_name = anl_task_name();
	task_version = anl_task_version();
	istat = aefits_write_name_vers(fp, task_name, task_version);
	if ( istat ) {
		return istat;
	}
	sprintf(history, "\
  infile='%s'", com.infile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  outfile='%s'", com.outfile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  orbit='%s'", com.orbit);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  hkfile='%s'", com.hkfile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  hk_time_margin=%.1f  clobber=%s",
		com.hk_time_margin, com.clobber ? "yes" : "no");
	fits_write_history(fp, history, &istat);
	fits_write_history(fp, " ", &istat);

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

static int
write_comment(fitsfile *fp)
{
	static char *comments[] = {
/*	 0        1         2         3         4         5         6         7
	 12345678901234567890123456789012345678901234567890123456789012345678901 */
	" ",
	"This file was processed by the aeattcor task, and values in the EULER",
	"column were overwritten. The attitude correction function is described",
	"in 'Empirical Correction of Thermal Wobbling',",
	"JX-ISAS-SUZAKU-MEMO-2007-04, as",
	" ",
	"  XOFFSET = -38.5*(b/60) - 9.25 + 0.4*T_86 [pixel]",
	"  YOFFSET = 10.0 + ( 7.0/1000)*T_DY [pixel] for T_DY < 0 [s]",
	"          = 10.0 + ( 5.0/2000)*T_DY [pixel] for 0 < T_DY < 2000 [s]",
	"          = 45.0 - (15.0/1000)*T_DY [pixel] for 2000 < T_DY [s]",
	"where",
	"     b: Ecliptic latitude [deg]",
	"  T_86: Temperature difference, HK_XIS_RAD8_T1_CAL - HK_XIS_RAD6_T1_CAL",
	"  T_DY: Time after night-day transision [s], defined as",
 	"        T_DY = T_DY_NT, for day-time",
 	"        T_DY = -TN_DY_NT, for night-time",
	" ",
	"Following columns were also added.",
	" ",
	"EULER_OLD:    contains original Z-Y-Z euler angles [deg]",
	" ",
	"XOFFSET:      calculated XOFFSET by the above formula [pixel]",
	" ",
	"YOFFSET:      calculated YOFFSET by the above formula [pixel]",
	" ",
	"T_86:         HK_XIS_RAD8_T1_CAL - HK_XIS_RAD6_T1_CAL [K]",
	" ",
	"T_DY:         T_DY_NT for day-time, -TN_DY_NT for night-time [s] in EHK",
	" ",
	"FLAG_ATTCOR:  attitude correction was conducted (=1) or not (=0)",
	" ",
	NULL
	};

	int i, istat;

	istat = 0;
	for (i = 0; NULL != comments[i]; i++) {
		fits_write_comment(fp, comments[i], &istat);
	}
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_comment() failed (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

static int
process_attitude_file(void)
{
#define NCOLS	6
	static char *ttype[NCOLS] = {
		"EULER_OLD", "XOFFSET", "YOFFSET", "T_86", "T_DY", "FLAG_ATTCOR"
	};
	static char *tform[NCOLS] = {
		"3D", "1E", "1E", "1E", "1E", "1B"
	};
	static char *tunit[NCOLS] = {
		"deg", "pixel", "pixel", "K", "s", NULL
	};
	static char *tdisp[NCOLS] = {
		"F16.8", "F6.2", "F6.2", "F6.2", "F8.1", NULL
	};

	char *k, key[FLEN_KEYWORD], comment[FLEN_COMMENT];
	unsigned char *buf, flag_attcor;
	int i, morekeys, anul;
	int colnum, col_time, col_euler, col_euler_new;
	int col_euler_old, col_xoffset, col_yoffset, col_t_86, col_t_dy, col_flag;
	long irow, nrow, naxis1, num_avg;
	double t, tstart, tstop, stime[3];
	double t_6, t_8, t_86, t_dy;
	double r, alpha, delta, lambda, beta, xoffset, yoffset;
	double ea_old[3], ea_new[3], tscal;
	double theta, phi, sin_theta;
	double avg_lamb, avg_beta, avg_xoff, avg_yoff, sgm_xoff, sgm_yoff;
	AtVect avg_vec;

	int istat = 0;

	fits_copy_header(com.ifp, com.ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_copy_header() failed for 1st extension (%d)\n", pname, istat);
		return istat;
	}

/* fits_set_hdrsize() must be immediately after fits_copy_header() */
	morekeys = 80;
	fits_set_hdrsize(com.ofp, morekeys, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_set_hdrsize() failed (%d)\n", pname, istat);
		return istat;
	}

/* write history */
	istat = write_history(com.ofp);
	if ( istat ) {
		return istat;
	}

/* determine column number */
	if (
fits_get_colnum(com.ifp, CASESEN, k="TIME", &col_time, &istat) ||
fits_get_colnum(com.ifp, CASESEN, k="EULER", &col_euler, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_get_colnum('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	col_euler_new = col_euler;
	fits_get_colnum(com.ifp, CASESEN, k="EULER_OLD", &col_euler_old, &istat);
	if ( 0 == istat ) {
		anl_msg_warning("\
%s: WARNING: EULER_OLD column found, using it\n", pname);
		col_euler = col_euler_old;
	} else {
		istat = 0;		/* ignore error */
		fits_clear_errmsg();
		fits_get_num_cols(com.ifp, &colnum, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_get_num_cols() failed (%d)\n", pname, istat);
			return istat;
		}
		fits_insert_cols(com.ofp, colnum+1, NCOLS, ttype, tform, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_insert_cols() failed (%d)\n", pname, istat);
			return istat;
		}
		col_euler_old = colnum + 1;
	}
	col_xoffset = col_euler_old + 1;
	col_yoffset = col_euler_old + 2;
	col_t_86 = col_euler_old + 3;
	col_t_dy = col_euler_old + 4;
	col_flag = col_euler_old + 5;

/* reset TSCALn */
	sprintf(key, "TSCAL%d", col_euler);
	fits_read_key_dbl(com.ifp, key, &tscal, NULL, &istat);
	if ( istat ) {
		istat = 0;		/* ignore error */
		tscal = 1.0;
	}
	fits_set_tscale(com.ifp, col_euler, 1.0, 0.0, &istat);
	fits_set_tscale(com.ofp, col_euler_old, 1.0, 0.0, &istat);
	fits_set_tscale(com.ofp, col_euler_new, 1.0, 0.0, &istat);
	if ( 1.0 != tscal && col_euler != col_euler_old ) {
		sprintf(key, "TSCAL%d", col_euler_old);
		sprintf(comment, "scaling factor of EULER_OLD");
		fits_update_key_fixdbl(com.ofp, key, 1.0, 1, comment, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_update_key('%s') failed (%d)\n", pname, key, istat);
			return istat;
		}
	}

/* write TUNITn, TDISPn */
	if ( col_euler != col_euler_old ) {
		for (i = 0; i < NCOLS; i++) {
			if ( NULL != tunit[i] ) {
				sprintf(key, k="TUNIT%d", col_euler_old+i);
				fits_write_key_str(com.ofp, key, tunit[i], "\
physical unit of field", &istat);
			}
			if ( 0 == istat && NULL != tdisp[i] ) {
				sprintf(key, k="TDISP%d", col_euler_old+i);
				sprintf(comment, "display format of %s", ttype[i]);
				fits_write_key_str(com.ofp, key, tdisp[i], comment, &istat);
			}
			if ( istat ) {
				anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
				return istat;
			}
		}
	}

/* write comment */
	istat = write_comment(com.ofp);
	if ( istat ) {
		return istat;
	}

	if (
fits_read_key_lng(com.ifp, k="NAXIS1", &naxis1, NULL, &istat) ||
fits_read_key_lng(com.ifp, k="NAXIS2", &nrow, NULL, &istat) ||
fits_read_key(com.ifp, TINT, k="MJDREFI", &com.mjdrefi, NULL, &istat) ||
fits_read_key_dbl(com.ifp, k="MJDREFF", &com.mjdreff, NULL, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_read_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	buf = malloc(naxis1);
	if ( NULL == buf ) {
		anl_msg_error("\
%s: malloc(naxis1=%ld) failed for row buffer\n", pname, naxis1);
		return -1;
	}

/* write attitude data */
	tstart = tstop = -1.0;
	num_avg = 0;
	avg_vec[0] = avg_vec[1] = avg_vec[2] = 0.0;
	avg_lamb = avg_beta = 0.0;
	avg_xoff = avg_yoff = 0.0;
	sgm_xoff = sgm_yoff = 0.0;
	for (irow = 1; irow <= nrow; irow++) {
		flag_attcor = 1;

		fits_read_tblbytes(com.ifp, irow, 1, naxis1, buf, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_tblbytes() failed at irow=%ld (%d)\n", pname, irow, istat);
			return istat;
		}
		fits_write_tblbytes(com.ofp, irow, 1, naxis1, buf, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_tblbytes() failed at irow=%ld (%d)\n", pname, irow, istat);
			return istat;
		}
fits_read_col_dbl(com.ifp, col_time, irow, 1, 1, 0.0, &t, &anul, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_col('TIME') failed at irow=%ld (%d)\n", pname, irow, istat);
			return istat;
		}
fits_read_col_dbl(com.ifp, col_euler, irow, 1, 3, 0.0, ea_old, &anul, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_read_col('EULER') failed at irow=%ld (%d)\n", pname, irow, istat);
			return istat;
		}
		if ( col_euler != col_euler_old ) {
fits_write_col_dbl(com.ofp, col_euler_old, irow, 1, 3, ea_old, &istat);
			if ( istat ) {
				anl_msg_error("\
%s: fits_write_col('EULER_OLD') failed at irow=%ld (%d)\n", pname,irow,istat);
				return istat;
			}
		}
		t_6 = t_8 = 0.0;
		if ( -1.0 != tstart && t < tstart ) {
		check_tstart:
			if ( t + com.hk_time_margin < tstart ) {
				flag_attcor = 0;
			} else {
				istat = aste_gethk(com.id_T6, tstart, TDOUBLE, 1, &t_6, stime);
				if ( istat ) goto anl_gethk_error;
				istat = aste_gethk(com.id_T8, tstart, TDOUBLE, 1, &t_8, stime);
				if ( istat ) goto anl_gethk_error;
			}
		} else if ( -1.0 != tstop && tstop < t ) {
		check_tstop:
			if ( tstop < t - com.hk_time_margin ) {
				flag_attcor = 0;
			} else {
				istat = aste_gethk(com.id_T6, tstop, TDOUBLE, 1, &t_6, stime);
				if ( istat ) goto anl_gethk_error;
				istat = aste_gethk(com.id_T8, tstop, TDOUBLE, 1, &t_8, stime);
				if ( istat ) goto anl_gethk_error;
			}
		} else {
			stime[0] = stime[1] = stime[2] = -1.0;
			istat = aste_gethk(com.id_T6, t, TDOUBLE, 1, &t_6, stime);
			if ( 0 == istat ) {
				istat = aste_gethk(com.id_T8, t, TDOUBLE, 1, &t_8, stime);
			}
			if ( ASTE_GETHK_GTI_ERROR == istat ) {
				istat = 0;
				t_6 = t_8 = 0.0;
				if ( -1.0 != stime[2] ) {
					tstart = stime[2];
					anl_msg_info("\
%s: INFO: TSTART=%.1f for %s\n", pname, tstart, com.hkfile);
					goto check_tstart;
				} else if ( -1.0 != stime[1] ) {
					tstop = stime[1];
					anl_msg_info("\
%s: INFO: TSTOP=%.1f for %s\n", pname, tstop, com.hkfile);
					goto check_tstop;
				}
				flag_attcor = 0;
			} else if ( istat ) {
			anl_gethk_error:
				anl_msg_error("\
%s: anl_gethk() failed (%d)\n", pname, istat);
				return istat;
			} else if ( stime[0] < t - com.hk_time_margin ) {
				if ( t + com.hk_time_margin <= stime[1] ) {
					double tt = stime[1];
					istat = aste_gethk(com.id_T6, tt, TDOUBLE, 1, &t_6, stime);
					if ( istat ) goto anl_gethk_error;
					istat = aste_gethk(com.id_T8, tt, TDOUBLE, 1, &t_8, stime);
					if ( istat ) goto anl_gethk_error;
				} else {
					anl_msg_warning("\
%s: WARNING: time gap found in hkfile, at t=%.1f\n", pname, t);
					t_6 = t_8 = 0.0;
					flag_attcor = 0;
				}
			}
		}

		t_86 = t_8 - t_6;
fits_write_col_dbl(com.ofp, col_t_86, irow, 1, 1, &t_86, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_col('T_86') failed at irow=%ld (%d)\n", pname, irow, istat);
			return istat;
		}

		istat = calc_t_dy(irow, t, &t_dy);
		if ( istat ) {
			istat = 0;
			t_dy = 0.0;
			flag_attcor = 0;
		}
fits_write_col_dbl(com.ofp, col_t_dy, irow, 1, 1, &t_dy, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_col('T_DY') failed at irow=%ld (%d)\n", pname, irow, istat);
			return istat;
		}

fits_write_col_byt(com.ofp, col_flag, irow, 1, 1, &flag_attcor, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_col('FLAG_ATTCOR') failed at irow=%ld (%d)\n",pname,irow,istat);
			return istat;
		}

		if ( flag_attcor ) {
			ea_new[0] = tscal * ea_old[0];
			ea_new[1] = tscal * ea_old[1];
			ea_new[2] = tscal * ea_old[2];
			alpha = ea_new[0];
			delta = 90.0 - ea_new[1];
			atJ2000ToEcliptic(alpha, delta, &lambda, &beta);
			phi = ea_new[0] * DEG2RAD;
			theta = ea_new[1] * DEG2RAD;
			sin_theta = sin(theta);
			avg_vec[0] += sin_theta * cos(phi);
			avg_vec[1] += sin_theta * sin(phi);
			avg_vec[2] += cos(theta);
			calc_xyoffset(beta, t_86, t_dy, &xoffset, &yoffset);
			correct_euler(xoffset, yoffset, ea_new, ea_new);
			ea_new[0] /= tscal;
			ea_new[1] /= tscal;
			ea_new[2] /= tscal;
			avg_xoff += xoffset;
			avg_yoff += yoffset;
			sgm_xoff += xoffset * xoffset;
			sgm_yoff += yoffset * yoffset;
			num_avg++;
		} else {
			xoffset = yoffset = 0.0;
			ea_new[0] = ea_old[0];
			ea_new[1] = ea_old[1];
			ea_new[2] = ea_old[2];
		}

fits_write_col_dbl(com.ofp, col_xoffset, irow, 1, 1, &xoffset, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_col('XOFFSET') failed at irow=%ld (%d)\n", pname, irow, istat);
			return istat;
		}

fits_write_col_dbl(com.ofp, col_yoffset, irow, 1, 1, &yoffset, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_col('YOFFSET') failed at irow=%ld (%d)\n", pname, irow, istat);
			return istat;
		}

fits_write_col_dbl(com.ofp, col_euler_new, irow, 1, 3, ea_new, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_write_col('EULER') failed at irow=%ld (%d)\n", pname, irow, istat);
			return istat;
		}

	}

/* set TSCALn for EULER_OLD */
	if ( 1.0 != tscal && col_euler != col_euler_old ) {
		sprintf(key, "TSCAL%d", col_euler_old);
		fits_modify_key_fixdbl(com.ofp, key, tscal, 15, NULL, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, key, istat);
			return istat;
		}
	}

	free(buf);

/* calculate average */
	if ( 0 < num_avg ) {
		atVectToPolDeg(avg_vec, &r, &alpha, &delta);
		atJ2000ToEcliptic(alpha, delta, &avg_lamb, &avg_beta);
		avg_xoff /= num_avg;
		avg_yoff /= num_avg;
		sgm_xoff = sqrt(sgm_xoff/num_avg - avg_xoff*avg_xoff);
		sgm_yoff = sqrt(sgm_yoff/num_avg - avg_yoff*avg_yoff);
	}

	anl_msg_info("\n\
NUM_CORR = %8ld      / number of corrected Euler angles\n\
AVG_LAMB = %8.4f      / average ecliptic longitude (deg)\n\
AVG_BETA = %+8.4f      / average ecliptic latitude (deg)\n\
AVG_XOFF = %8.4f      / average DETX offset (pixel)\n\
AVG_YOFF = %8.4f      / average DETY offset (pixel)\n\
SGM_XOFF = %8.4f      / 1 sigma DETX offset standard deviation (pixel)\n\
SGM_YOFF = %8.4f      / 1 sigma DETY offset standard deviation (pixel)\n\
\n",
		num_avg, avg_lamb, avg_beta, avg_xoff, avg_yoff, sgm_xoff, sgm_yoff);

	if (
	aefits_del_write_key(com.ofp, TLONG, k="NUM_CORR", &num_avg, "\
number of corrected Euler angles", &istat) ||
	aefits_del_write_key_fixdbl(com.ofp, k="AVG_LAMB", avg_lamb, 4, "\
average ecliptic longitude (deg)", &istat) ||
	aefits_del_write_key_fixdbl(com.ofp, k="AVG_BETA", avg_beta, 4, "\
average ecliptic latitude (deg)", &istat) ||
	aefits_del_write_key_fixdbl(com.ofp, k="AVG_XOFF", avg_xoff, 4, "\
average DETX offset (pixel)", &istat) ||
	aefits_del_write_key_fixdbl(com.ofp, k="AVG_YOFF", avg_yoff, 4, "\
average DETY offset (pixel)", &istat) ||
	aefits_del_write_key_fixdbl(com.ofp, k="SGM_XOFF", sgm_xoff, 4, "\
1 sigma DETX offset standard deviation (pixel)", &istat) ||
	aefits_del_write_key_fixdbl(com.ofp, k="SGM_YOFF", sgm_yoff, 4, "\
1 sigma DETY offset standard deviation (pixel)", &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	return istat;
}

static int
close_attitude_file(void)
{
	int istat;

	istat = aste_orbit_free(com.obp);
	if ( istat ) {
		return istat;
	}

	fits_close_file(com.ifp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed for infile (%d)\n", pname, istat);
		return istat;
	}

	fits_write_chksum(com.ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_chksum() failed for outfile (%d)\n", pname, istat);
		return istat;
	}

	fits_close_file(com.ofp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_close_file() failed for outfile (%d)\n", pname, istat);
		return istat;
	}

	return istat;
}

void
aeattcor_com(int *status)
{
#define NVAL	8
	static char *names[NVAL] = {
		"SHOW_PARAMETER",
		"INFILE",
		"OUTFILE",
		"ORBIT",
		"HKFILE",
		"HK_TIME_MARGIN",
		"CLOBBER",
		"EXIT"
	};
	static char *help[NVAL] = {
		"show current setting",
		"input attitude file name",
		"output attitude file name",
		"orbit file name",
		"common HK file name",
		"HK time margin in second",
		"overwrite output file if exists",
		"exit from this menu"
	};
	char *k;
	int answer[2];
	int nreply = 1;

	if ( *status ) {	/* ftools */
		*status = ANL_QUIT;
		if (
PILGetFname(k="infile", com.infile) ||
PILGetFname(k="outfile", com.outfile) ||
PILGetFname(k="orbit", com.orbit) ||
PILGetFname(k="hkfile", com.hkfile) ||
PILGetReal (k="hk_time_margin", &com.hk_time_margin) ||
PILGetBool (k="clobber", &com.clobber) ||
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
		} else if ( 0 == strcmp("INFILE", p) ) {
			CLtxtrd(p, com.infile, sizeof(com.infile));
		} else if ( 0 == strcmp("OUTFILE", p) ) {
			CLtxtrd(p, com.outfile, sizeof(com.outfile));
		} else if ( 0 == strcmp("ORBIT", p) ) {
			CLtxtrd(p, com.orbit, sizeof(com.orbit));
		} else if ( 0 == strcmp("HKFILE", p) ) {
			CLtxtrd(p, com.hkfile, sizeof(com.hkfile));
		} else if ( 0 == strcmp("HK_TIME_MARGIN", p) ) {
			CLfdprd(p, &com.hk_time_margin);
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
aeattcor_startup(int *status)
{
	*status = ANL_OK;
}

void
aeattcor_init(int *status)
{
	int istat;

	show_parameter();

	istat = open_attitude_file();
	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}

	istat = process_attitude_file();
	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}

	*status = ANL_OK;
}

void
aeattcor_his(int *status)
{
	*status = ANL_OK;
}

void
aeattcor_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
aeattcor_ana(int *nevent, int *eventid, int *status)
{
	*status = ANL_QUIT;
}

void
aeattcor_endrun(int *status)
{
	*status = ANL_OK;
}

void
aeattcor_exit(int *status)
{
	int istat;

	istat = close_attitude_file();
	if ( istat ) {
		*status = ANL_QUIT;
		return;
	}

	anl_msg_info("\
%s: INFO: created '%s'\n", pname, com.outfile);

	*status = ANL_OK;
}
