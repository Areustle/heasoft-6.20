/*
	aste_aspect.c

	1999/12/23 Y.ISHISAKI	version 1.0

	2004/03/14 Y.ISHISAKI	version 1.1
		use AtTimeD (since atFunctions-2.2) instead of AtTime

	2005/06/07 Y.ISHISAKI	version 1.2
		renamed to aste_aspect from com/src/module/AEaspectATTFILE

	2005/06/14 Y.ISHISAKI	version 1.3
		calculate median
		use atEulerToVect(), atVectToEuler()
		add sample_sec, num_accept, num_sample in message

	2005/06/26,07/04 Y.ISHISAKI	version 1.4
		read OBSERVER keyword
		use aefits_delta_phi() instead of local delta_phi()

	2005/10/24 Y.ISHISAKI	version 1.5
		read OBS_MODE, OBS_ID, OBS_REM keywords
		free allocated memory in md_euler()

	2005/10/27 Y.ISHISAKI	version 1.6
		rename atVectToEuler -> VectToEuler(), atEulerToVect -> EulerToVect()
		use asp->mean_ea in aste_aspect_finalize()
		add TWO_PI if ea->phi negative in VectToEuler(), av_euler(), md_euler()

	2006/04/26 Y.ISHISAKI	version 1.7
		set asp->mean, depending on adopt_median in aste_aspect_finalize()

	2007/05/07 Y.ISHISAKI	version 1.8
		read NOM_PNT keyword, and set nom_pnt, nom_pnt_card
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "bnk.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_time.h"
#include "aste_att.h"
#include "aste_aspect.h"
#include "aeFitsHeaderUtil.h"

static char pname[] = "aste_aspect";
char aste_aspect_version[] = "1.8";

#define DEFAULT_SAMPLE_SEC		1.0

static char *
t2s(double t, char *s)
{
	AtTimeD attime;

	aste2attimeD(t, &attime);
	sprintf(s, "%04d-%02d-%02d %02d:%02d:%02d",
			attime.yr, attime.mo, attime.dy, attime.hr, attime.mn, attime.sc);

	return s;
}

static void
EulerToVect(AtEulerAng *ea, AtVect v)
{
	double sin_t = sin(ea->theta);

	v[0] = sin_t * cos(ea->phi);
	v[1] = sin_t * sin(ea->phi);
	v[2] = cos(ea->theta);
}

static int
VectToEuler(AtVect v, AtEulerAng *ea)
{
#define x (v[0])
#define y (v[1])
#define z (v[2])

	double r2, x2y2, rxy;

	x2y2 = x*x + y*y;
	r2 = x2y2 + z*z;
	if ( 0.0 == r2 ) {
		return -1;
	}

	if ( 0.0 == x2y2 ) {
		ea->phi = 0.0;
	} else {
		ea->phi = atan2(y, x);	/* -PI <= atan2 <= PI */
		if ( ea->phi < 0.0 ) {
			ea->phi += TWO_PI;
		}
	}

	rxy = sqrt(x2y2);
	ea->theta = atan2(rxy, z);	/* 0 <= theta, because 0 <= rxy */

#undef x
#undef y
#undef z

	return 0;
}

static int
av_euler(int ns, AtEulerAng *ea_sample, AtVect *vec_sample, AtEulerAng *av_ea)
{
	int i, istat;
	AtVect av_vec;
	double ax, x, ay, y;

	ax = ay = 0.0;
	av_vec[0] = av_vec[1] = av_vec[2] = 0.0;

	for (i = 0; i < ns; i++) {
		av_vec[0] += vec_sample[i][0];
		av_vec[1] += vec_sample[i][1];
		av_vec[2] += vec_sample[i][2];
		x = cos(ea_sample[i].psi);
		y = sin(ea_sample[i].psi);
		ax += x;
		ay += y;
	}

	istat = VectToEuler(av_vec, av_ea);
	if ( istat ) {
		fprintf(stderr, "\
%s: average pointing direction is canceled out\n", pname);
		return istat;
	}

	if ( 0.0 == ax && 0.0 == ay ) {
		fprintf(stderr, "\
%s: average roll angle is canceled out\n", pname);
		return -1;
	}
	av_ea->psi = atan2(ay, ax);		/* -PI <= atan2 <= PI */
	if ( av_ea->psi < 0.0 ) {
		av_ea->psi += TWO_PI;
	}

	return 0;
}

static int
compare_double(double *v1, double *v2)
{
	if ( *v1 > *v2 ) {
		return +1;
	}
	if ( *v1 < *v2 ) {
		return -1;
	}
	return 0;
}

static double
calc_median(int ns, double *v)
{
	int ipos;
	double median;

	qsort(v, ns, sizeof(*v), compare_double);

	ipos = ns / 2;
	if ( 1 == ns % 2 ) {
		median = v[ipos];
	} else {
		median = (v[ipos] + v[ipos+1]) / 2;
	}

	return median;
}

static int
md_euler(int ns, AtEulerAng *ea_sample, AtVect *vec_sample, AtEulerAng *md_ea)
{
	int i, istat;
	double *v;
	AtVect md_vec;
	double ax, ay;

	v = malloc(ns * sizeof(*v));
	if ( NULL == v ) {
		fprintf(stderr, "\
%s: ERROR: malloc() failed for v[ns=%d]\n", pname, ns);
		return -1;
	}

	for (i = 0; i < ns; i++) {
		v[i] = vec_sample[i][0];
	}
	md_vec[0] = calc_median(ns, v);
	for (i = 0; i < ns; i++) {
		v[i] = vec_sample[i][1];
	}
	md_vec[1] = calc_median(ns, v);
	for (i = 0; i < ns; i++) {
		v[i] = vec_sample[i][2];
	}
	md_vec[2] = calc_median(ns, v);
	for (i = 0; i < ns; i++) {
		v[i] = cos(ea_sample[i].psi);
	}
	ax = calc_median(ns, v);
	for (i = 0; i < ns; i++) {
		v[i] = sin(ea_sample[i].psi);
	}
	ay = calc_median(ns, v);

	free(v);

	istat = VectToEuler(md_vec, md_ea);
	if ( istat ) {
		fprintf(stderr, "\
%s: median pointing direction is canceled out\n", pname);
		return istat;
	}

	if ( 0.0 == ax && 0.0 == ay ) {
		fprintf(stderr, "\
%s: median roll angle is canceled out\n", pname);
		return -1;
	}
	md_ea->psi = atan2(ay, ax);		/* -PI <= atan2 <= PI */
	if ( md_ea->psi < 0.0 ) {
		md_ea->psi += TWO_PI;
	}

	return 0;
}

int
aste_aspect_init(ASTE_ASPECT *asp)
{
	char ts[2][32];

	int istat = 0;

	TELDEF *teldef = asp->teldef;
	ATTFILE *attitude = asp->attitude;

	fflush(NULL); printf("\
%s version %s\n\n", pname, aste_aspect_version);
	fflush(NULL);

	if ( NULL == attitude ) {
		fprintf(stderr, "\n\
%s: init: ERROR: attitude=NULL", pname);
		return -1;
	}

	if ( NULL == teldef ) {
		fprintf(stderr, "\n\
%s: init: ERROR: teldef=NULL", pname);
		return -1;
	}

	if ( NULL != attitude->fp ) {

		if (
fits_read_key_str(attitude->fp, "OBS_MODE", asp->obs_mode, NULL, &istat) ||
fits_read_card(attitude->fp, "OBS_MODE", asp->obs_mode_card, &istat) ) {
			istat = 0;
			strcpy(asp->obs_mode, "");
			strcpy(asp->obs_mode_card, "\
OBS_MODE= '        '           / observation mode (e.g. POINTING/SLEW)");
		}

		if (
fits_read_key_str(attitude->fp, "OBS_ID", asp->obs_id, NULL, &istat) ||
fits_read_card(attitude->fp, "OBS_ID", asp->obs_id_card, &istat) ) {
			istat = 0;
			strcpy(asp->obs_id, "");
			strcpy(asp->obs_id_card, "\
OBS_ID  = '        '           / Observation Identifier");
		}

		if (
fits_read_key_str(attitude->fp, "OBS_REM", asp->obs_rem, NULL, &istat) ||
fits_read_card(attitude->fp, "OBS_REM", asp->obs_rem_card, &istat) ) {
			istat = 0;
			strcpy(asp->obs_rem, "");
			strcpy(asp->obs_rem_card, "\
OBS_REM = '        '           / remark on observation");
		}

		if (
fits_read_key_str(attitude->fp, "OBSERVER", asp->observer, NULL, &istat) ||
fits_read_card(attitude->fp, "OBSERVER", asp->observer_card, &istat) ) {
			istat = 0;
			strcpy(asp->observer, "");
			strcpy(asp->observer_card, "\
OBSERVER= '        '           / Principal Investigator");
		}

		if (
fits_read_key_str(attitude->fp, "OBJECT", asp->object, NULL, &istat) ||
fits_read_card(attitude->fp, "OBJECT", asp->object_card, &istat) ) {
			istat = 0;
			strcpy(asp->object, "");
			strcpy(asp->object_card, "\
OBJECT  = '        '           / name of observed object");
		}

		if (
fits_read_key_str(attitude->fp, "NOM_PNT", asp->nom_pnt, NULL, &istat) ||
fits_read_card(attitude->fp, "NOM_PNT", asp->nom_pnt_card, &istat) ) {
			istat = 0;
			strcpy(asp->nom_pnt, "");
			strcpy(asp->nom_pnt_card, "\
NOM_PNT = '        '           / AimPointInDETXY:XIS=(0,0),HXD=(-3.5,0)[arcmin]");
		}

		if (
fits_read_key_dbl(attitude->fp, "RA_OBJ", &asp->ra_obj, NULL, &istat) ||
fits_read_card(attitude->fp, "RA_OBJ", asp->ra_obj_card, &istat) ) {
			istat = 0;
			if (
fits_read_key_dbl(attitude->fp, "RA", &asp->ra_obj, NULL, &istat) ||
fits_read_card(attitude->fp, "RA", asp->ra_obj_card, &istat) ) {
				istat = 0;
				asp->ra_obj = -999.0;
				strcpy(asp->ra_obj_card, "\
RA_OBJ  =               -999.0 / planned target R.A.(deg)");
			}
		}

		if (
fits_read_key_dbl(attitude->fp, "DEC_OBJ", &asp->dec_obj, NULL, &istat) ||
fits_read_card(attitude->fp, "DEC_OBJ", asp->dec_obj_card, &istat) ) {
			istat = 0;
			if (
fits_read_key_dbl(attitude->fp, "DEC", &asp->dec_obj, NULL, &istat) ||
fits_read_card(attitude->fp, "DEC", asp->dec_obj_card, &istat) ) {
				istat = 0;
				asp->dec_obj = -999.0;
				strcpy(asp->dec_obj_card, "\
DEC_OBJ =               -999.0 / planned target DEC.(deg)");
			}
		}

		if (
fits_read_key(attitude->fp, TINT, "MJDREFI", &asp->mjdrefi, NULL, &istat) ||
fits_read_key_dbl(attitude->fp, "MJDREFF", &asp->mjdreff, NULL, &istat) ) {
			istat = 0;
			fprintf(stderr, "\
%s: init: WARNING: no MJDREFI/MJDREFF keywords, assuming default\n", pname);
			asp->mjdrefi = 51544;
			asp->mjdreff = 0.00074287037037037;
		}

	} else {

		strcpy(asp->obs_mode, "");
		strcpy(asp->obs_mode_card, "\
OBS_MODE= '        '           / observation mode (e.g. POINTING/SLEW)");
		strcpy(asp->obs_id, "");
		strcpy(asp->obs_id_card, "\
OBS_ID  = '        '           / Observation Identifier");
		strcpy(asp->obs_rem, "");
		strcpy(asp->obs_rem_card, "\
OBS_REM = '        '           / remark on observation");
		strcpy(asp->observer, "");
		strcpy(asp->observer_card, "\
OBSERVER= '        '           / Principal Investigator");
		strcpy(asp->object, "");
		strcpy(asp->object_card, "\
OBJECT  = '        '           / name of observed object");
		asp->ra_obj = -999.0;
		strcpy(asp->ra_obj_card, "\
RA_OBJ  =               -999.0 / planned target R.A.(deg)");
		asp->dec_obj = -999.0;
		strcpy(asp->dec_obj_card, "\
DEC_OBJ =               -999.0 / planned target DEC.(deg)");
		asp->mjdrefi = 51544;
		asp->mjdreff = 0.00074287037037037;

	}

	if ( asp->verbose ) {
		fflush(NULL); printf("\
aspecting attitude: %s\n\
  TELESCOP='%s', OBJECT='%s', (RA,DEC)=(%.4f, %.4f)\n\
\n\
using teldef file: %s\n\
  TELESCOP='%s', INSTRUME='%s'\n\
\n\
attitude  START STOP dT: %16.6f %16.6f %16.6f\n\
  START in yyyy-mm-dd hh:mm:ss (MJD): %s (%.8f)\n\
  STOP  in yyyy-mm-dd hh:mm:ss (MJD): %s (%.8f)\n\
\n",
			attitude->filename, attitude->mission,
			asp->object, asp->ra_obj, asp->dec_obj,
			teldef->actual_filename, teldef->telescop, teldef->instrume,
			attitude->tstart, attitude->tstop, attitude->duration,
			t2s(attitude->tstart, ts[0]), aste2mjd(attitude->tstart),
			t2s(attitude->tstop,  ts[1]), aste2mjd(attitude->tstop));
		fflush(NULL);
	}

	return 0;
}

static int
aste_aspect_finalize(ASTE_ASPECT *asp, int n, AtEulerAng *ea_sample)
{
	int i;
	double r, alpha, delta, roll, offset;
	AtVect v, v0, vSun, vFov, vAbr;
	SKYREF fov;

	if ( asp->adopt_median ) {
		asp->mean = &asp->md;
		asp->mean_ea = &asp->md_ea;
	} else {
		asp->mean = &asp->av;
		asp->mean_ea = &asp->av_ea;
	}
	atEulerToRM(asp->mean_ea, asp->rm);
	atInvRotMat(asp->rm, asp->irm);
	aste_euler2skyref(asp->teldef, &asp->av_ea, &asp->av);
	aste_euler2skyref(asp->teldef, &asp->md_ea, &asp->md);

	atSun(asp->av_mjd, vSun);
	atVectToPolDeg(vSun, &r, &asp->sun_p.alpha, &asp->sun_p.delta);

	atPolDegToVect(1.0, asp->mean->alpha, asp->mean->delta, vFov);
	atAberration(asp->av_mjd, vFov, vAbr);
	atVectToPolDeg(vAbr, &r, &alpha, &delta);
	asp->aberr.alpha = aefits_delta_phi(alpha, asp->mean->alpha);
	asp->aberr.delta = delta - asp->mean->delta;

	v0[1] = v0[2] = 0.0; v0[0] = 1.0;
	atRotVect(asp->irm, v0, v);
	atVectToPolDeg(v, &r, &asp->xaxis.alpha, &asp->xaxis.delta);
	atAngDistance(v, vSun, &asp->xaxis.sun_ang);

	v0[0] = v0[2] = 0.0; v0[1] = 1.0;
	atRotVect(asp->irm, v0, v);
	atVectToPolDeg(v, &r, &asp->yaxis.alpha, &asp->yaxis.delta);
	atAngDistance(v, vSun, &asp->yaxis.sun_ang);

	v0[0] = v0[1] = 0.0; v0[2] = 1.0;
	atRotVect(asp->irm, v0, v);
	atVectToPolDeg(v, &r, &asp->zaxis.alpha, &asp->zaxis.delta);
	atAngDistance(v, vSun, &asp->zaxis.sun_ang);

	asp->mi.alpha = asp->ma.alpha = asp->av.alpha;
	asp->mi.delta = asp->ma.delta = asp->av.delta;
	asp->mi.roll  = asp->ma.roll  = asp->av.roll;

	asp->ma_offset = 0.0;
	asp->sg_offset = 0.0;
	asp->sg.alpha = asp->sg.delta = asp->sg.roll = 0.0;

	for (i = 0; i < n; i++) {
		aste_euler2skyref(asp->teldef, &ea_sample[i], &fov);
		alpha = asp->av.alpha + aefits_delta_phi(fov.alpha, asp->av.alpha);
		delta = fov.delta;
		roll  = asp->av.roll  + aefits_delta_phi(fov.roll,  asp->av.roll);
		if ( alpha < asp->mi.alpha ) asp->mi.alpha = alpha;
		if ( delta < asp->mi.delta ) asp->mi.delta = delta;
		if ( roll  < asp->mi.roll  ) asp->mi.roll  = roll;
		if ( alpha > asp->ma.alpha ) asp->ma.alpha = alpha;
		if ( delta > asp->ma.delta ) asp->ma.delta = delta;
		if ( roll  > asp->ma.roll  ) asp->ma.roll  = roll;
		atPolDegToVect(1.0, fov.alpha, fov.delta, v);
		atAngDistance(vFov, v, &offset);
		offset *= RAD2ARCMIN;
		if ( offset > asp->ma_offset ) asp->ma_offset = offset;
		asp->sg.alpha += (alpha - asp->av.alpha) * (alpha - asp->av.alpha);
		asp->sg.delta += (delta - asp->av.delta) * (delta - asp->av.delta);
		asp->sg.roll  += (roll  - asp->av.roll)  * (roll  - asp->av.roll);
		asp->sg_offset += offset * offset;
	}

	asp->sg.alpha = sqrt(asp->sg.alpha / n);
	asp->sg.delta = sqrt(asp->sg.delta / n);
	asp->sg.roll  = sqrt(asp->sg.roll  / n);
	asp->sg_offset = sqrt(asp->sg_offset / n);

	return 0;
}

int
aste_aspect_attitude(ASTE_ASPECT *asp)
{
	int i, n, ns;
	double sample_sec, t, t0, t1, dt, tz;
	double offset;
	AtEulerAng ea, *ea_sample;
	AtVect vbase, vfov, *vec_sample;
	SKYREF base, fov;
	char ts[2][32];

	sample_sec = asp->sample_sec;
	if ( sample_sec <= 0.0 ) {
		sample_sec = DEFAULT_SAMPLE_SEC;
	}

	t0 = asp->t0;
	if ( 0.0 == t0 ) {
		t0 = asp->attitude->tstart;
	}

	t1 = asp->t1;
	if ( 0.0 == t1 ) {
		t1 = asp->attitude->tstop;
	}

	dt = asp->dt = t1 - t0;
	ns = asp->num_sample = (int)(dt / sample_sec) + 1;
	tz = asp->tz = ( dt - sample_sec * (ns - 1) ) / 2.0;

	if ( asp->verbose ) {
		fflush(NULL); printf("\
aspecting START STOP dT: %16.6f %16.6f %16.6f\n\
  START in yyyy-mm-dd hh:mm:ss (MJD): %s (%.8f)\n\
  STOP  in yyyy-mm-dd hh:mm:ss (MJD): %s (%.8f)\n\
\n",
			t0, t1, t1-t0,
			t2s(t0, ts[0]), aste2mjd(t0),
			t2s(t1, ts[1]), aste2mjd(t1));
		printf("\
averaging attitude in %.0f sec sampling, %d points\n", sample_sec, ns);
		fflush(NULL);
	}

	if ( ns < 1 ) {
		fprintf(stderr, "\
%s: ERROR: too small sampling of ns=%d\n", pname, ns);
		return -1;
	}

	ea_sample = malloc( ns * sizeof(*ea_sample) + ns * sizeof(*vec_sample) );
	vec_sample = (AtVect *)(ea_sample + ns);

	if ( NULL == ea_sample ) {
		fprintf(stderr, "\
%s: ERROR: malloc() faild for ns=%d\n", pname, ns);
		return -1;
	}

	aste_euler2skyref(asp->teldef, &asp->ea_base, &base);
	atPolDegToVect(1.0, base.alpha, base.delta, vbase);

	for (i = n = 0; i < ns; i++) {

		t = t0 + tz + i * sample_sec;

		if ( aste_att_euler(t, &ea) ) {
			continue;
		}

		if ( 0 < asp->offset_tolerance || 0 < asp->roll_tolerance ) {

			aste_euler2skyref(asp->teldef, &ea, &fov);

			if ( 0 < asp->offset_tolerance ) {
				atPolDegToVect(1.0, fov.alpha, fov.delta, vfov);
				atAngDistance(vbase, vfov, &offset);
				if ( asp->offset_tolerance < offset ) {
					continue;
				}
			}

			if ( 0 < asp->roll_tolerance ) {
				if ( asp->roll_tolerance < fabs(fov.roll - base.roll) ) {
					continue;
				}
			}

		}

		ea_sample[n] = ea;
		EulerToVect(&ea, vec_sample[n]);

		n++;

	}

	asp->num_accept = n;

	if ( ns != n && n < 1 ) {
		fprintf(stderr, "\
%s: ERROR: %d samples rejected, %d samples left\n", pname, ns-n, n);
		return -1;
	} else if ( ns != n ) {
		if ( asp->verbose ) {
			fflush(NULL); printf("\
%s: %d samples rejected, %d samples left\n", pname, ns-n, n);
			fflush(NULL);
		}
	}

	asp->av_t = (t0 + t1) / 2.0;
	asp->av_mjd = aste2mjd(asp->av_t);
	md_euler(n, ea_sample, vec_sample, &asp->md_ea);
	av_euler(n, ea_sample, vec_sample, &asp->av_ea);

	aste_aspect_finalize(asp, n, ea_sample);
	free(ea_sample);

	return 0;
}

char **
aste_aspect_message(ASTE_ASPECT *asp)
{
#define MSG_LINES	32
	static char *msg[MSG_LINES];
	static char msgbuf[MSG_LINES][80];

	int i;
	double t0, t1, sample_sec;
	char ts[2][32];

	for (i = 0; i < MSG_LINES; i++) {
		msg[i] = msgbuf[i];
	}

	i = 0;

	sample_sec = asp->sample_sec;
	if ( sample_sec <= 0.0 ) {
		sample_sec = DEFAULT_SAMPLE_SEC;
	}

	t0 = asp->t0;
	if ( 0.0 == t0 ) {
		t0 = asp->attitude->tstart;
	}

	t1 = asp->t1;
	if ( 0.0 == t1 ) {
		t1 = asp->attitude->tstop;
	}

	sprintf(msg[i++], "\
  Sample Time : %.1f s   Number of Accept / Sample : %d / %d",
		sample_sec, asp->num_accept, asp->num_sample);
	sprintf(msg[i++], "\
  TIME START STOP TELAPSE (s) : %11.1f %11.1f %11.1f", t0, t1, t1-t0);
	sprintf(msg[i++], "\
  START DATE TIME in UTC (MJD): %s (%.8f)", t2s(t0, ts[0]), aste2mjd(t0));
	sprintf(msg[i++], "\
  STOP  DATE TIME in UTC (MJD): %s (%.8f)", t2s(t1, ts[1]), aste2mjd(t1));

	strcpy (msg[i++], "");

	sprintf(msg[i++], asp->adopt_median ? "\
  Mean [MEDIAN]  Euler angles :%12.6f%12.6f%12.6f" : "\
  Mean [AVERAGE] Euler angles :%12.6f%12.6f%12.6f",
		asp->mean_ea->phi*RAD2DEG,
		asp->mean_ea->theta*RAD2DEG,
		asp->mean_ea->psi*RAD2DEG);

	strcpy (msg[i++], "");
	sprintf(msg[i++], "\
                                    RA          DEC      SUN ANGLE");
	strcpy (msg[i++], "");
	sprintf(msg[i++], "\
  Mean Sun position     (deg) :%12.6f%12.6f",
		asp->sun_p.alpha, asp->sun_p.delta);

	sprintf(msg[i++], "\
  Mean aberration    (arcsec) :%12.6f%12.6f",
		asp->aberr.alpha*60*60, asp->aberr.delta*60*60);
	strcpy (msg[i++], "");

	sprintf(msg[i++], "\
  Mean satellite X-axis (deg) :%12.6f%12.6f%12.6f",
		asp->xaxis.alpha, asp->xaxis.delta, asp->xaxis.sun_ang*RAD2DEG);

	sprintf(msg[i++], "\
  Mean satellite Y-axis (deg) :%12.6f%12.6f%12.6f",
		asp->yaxis.alpha, asp->yaxis.delta, asp->yaxis.sun_ang*RAD2DEG);

	sprintf(msg[i++], "\
  Mean satellite Z-axis (deg) :%12.6f%12.6f%12.6f",
		asp->zaxis.alpha, asp->zaxis.delta, asp->zaxis.sun_ang*RAD2DEG);

	strcpy (msg[i++], "");
	sprintf(msg[i++], "\
                   RA(deg)     DEC(deg)    ROLL(deg) OFFSET(arcmin)");
	strcpy (msg[i++], "");
	sprintf(msg[i++], "\
  Median        %12.6f%12.6f%12.6f",
		asp->md.alpha, asp->md.delta, asp->md.roll);
	sprintf(msg[i++], "\
  Average       %12.6f%12.6f%12.6f",
		asp->av.alpha, asp->av.delta, asp->av.roll);
	sprintf(msg[i++], "\
  Minimum       %12.6f%12.6f%12.6f",
		asp->mi.alpha, asp->mi.delta, asp->mi.roll);
	sprintf(msg[i++], "\
  Maximum       %12.6f%12.6f%12.6f%12.6f",
		asp->ma.alpha, asp->ma.delta, asp->ma.roll, asp->ma_offset);
	sprintf(msg[i++], "\
  Sigma (RMS)   %12.6f%12.6f%12.6f%12.6f",
		asp->sg.alpha, asp->sg.delta, asp->sg.roll, asp->sg_offset);

	msg[i++] = NULL;
	if ( MSG_LINES <= i ) {
		fprintf(stderr, "\
%s: ERROR: too many message lines (%d)\n", pname, i);
		return NULL;
	}

	return msg;
}
