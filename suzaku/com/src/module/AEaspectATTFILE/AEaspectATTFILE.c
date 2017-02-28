/*
  AEaspectATTFILE.c

	1999/12/23 Y.ISHISAKI	version 1.0

	2004/03/14 Y.ISHISAKI	version 1.1
		use AtTimeD (since atFunctions-2.2) instead of AtTime
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "bnk.h"
#include "fitsio.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_att.h"
#include "aste_time.h"

static char pname[] = "AEaspectATTFILE";

char AEaspectATTFILE_version[] = "version 1.1";

/*
#define aste2mjd asca2mjd
#define aste2attime asca2attime
*/

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
average_euler(int ns, AtEulerAng *ea_sample, AtEulerAng *av_ea)
{
	int i;
	double alpha, delta;
	AtEulerAng *ea;
	AtVect av, v;
	double ax, x, ay, y, r;

	ax = ay = 0.0;
	av[0] = av[1] = av[2] = 0.0;

	for (i = 0; i < ns; i++) {
		ea = &ea_sample[i];
		alpha = RAD2DEG * ea->phi;
		delta = 90.0 - RAD2DEG * ea->theta;
		atPolDegToVect(1.0, alpha, delta, v);
		av[0] += v[0];
		av[1] += v[1];
		av[2] += v[2];
		x = cos(ea->psi);
		y = sin(ea->psi);
		ax += x;
		ay += y;
	}

	atVectToPolDeg(av, &r, &alpha, &delta);
	av_ea->phi = alpha * DEG2RAD;
	av_ea->theta = (90.0 - delta) * DEG2RAD;
	av_ea->psi = atan2(ay, ax);
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

int
AEaspectATTFILE(TELDEF *teldef, ATTFILE *attfile, double t0, double t1)
{
	int sample_time = 60;
	int istat, i, n, ns;
	char object[80], comment[80];
	double alpha, delta, roll, offset, t, dt, tz, r;
	char ts[4][32];
	AtEulerAng ea, av_ea, *ea_sample;
	AtRotMat av_rm, av_irm;
	AtVect v0, v, vSun, vFov, vAbr;
	SKYREF fov, av, av_sun, av_abr, sg;
	double av_t, av_mjd;
	double alpha0, alpha1, delta0, delta1, roll0, roll1, offset1, sg_offset;

	printf("\n\
%s %s\n\n", pname, AEaspectATTFILE_version);

	if ( NULL == attfile ) {
		printf("\n\
attfile=NULL, return -1");
		return -1;
	}

	if ( NULL != attfile->fp ) {
		istat = 0;
		fits_read_key_str(attfile->fp, "OBJECT", object, comment, &istat);
		if ( istat ) {
			strcpy(object, "NONE");
			istat = 0;
		}
		fits_read_key_dbl(attfile->fp, "RA", &alpha, comment, &istat);
		fits_read_key_dbl(attfile->fp, "DEC", &delta, comment, &istat);
		if ( istat ) {
			alpha = delta = 0.0;
		}
	} else {
		strcpy(object, "NONE");
		alpha = delta = 0.0;
	}

	if ( 0.0 == t0 ) {
		t0 = attfile->tstart;
	}
	if ( 0.0 == t1 ) {
		t1 = attfile->tstop;
	}

	printf("\
aspecting attfile: %s\n\
  TELESCOP='%s', OBJECT='%s', (RA,DEC)=(%.4f, %.4f)\n\
\n\
using teldef file: %s\n\
  TELESCOP='%s', INSTRUME='%s'\n\
\n\
attfile TSTART TSTOP dT: %16.6f %16.6f %16.6f\n\
  TSTART in yyyy-mm-dd hh:mm:ss (MJD): %s (%.8f)\n\
  TSTOP  in yyyy-mm-dd hh:mm:ss (MJD): %s (%.8f)\n\
\n\
aspecting START STOP dT: %16.6f %16.6f %16.6f\n\
  TSTART in yyyy-mm-dd hh:mm:ss (MJD): %s (%.8f)\n\
  TSTOP  in yyyy-mm-dd hh:mm:ss (MJD): %s (%.8f)\n\
\n",	   attfile->filename, attfile->mission, object, alpha, delta,
		   teldef->actual_filename, teldef->telescop, teldef->instrume,
		   attfile->tstart, attfile->tstop, attfile->duration,
		   t2s(attfile->tstart, ts[0]), aste2mjd(attfile->tstart),
		   t2s(attfile->tstop,  ts[1]), aste2mjd(attfile->tstop),
		   t0, t1, t1-t0,
		   t2s(t0, ts[2]), aste2mjd(t0), t2s(t1, ts[3]), aste2mjd(t1)
		);

	dt = t1 - t0;
	n = (int)(dt / sample_time) + 1;
	tz = (dt - sample_time * (n-1)) / 2.0;

	printf("\
averaging attitude in %d sec sampling, %d points\n\n", sample_time, n);
	if ( n < 1 ) {
		printf("\
too small sampling of n=%d, return -1\n", n);
		return -1;
	}

	ea_sample = malloc(n * sizeof(*ea_sample));
	if ( NULL == ea_sample ) {
		printf("\
malloc() faild for n=%d, return -1\n", n);
		return -1;
	}

	for (i = ns = 0; i < n; i++) {
		t = t0 + tz + i * sample_time;
		if ( aste_att_euler(t, &ea) ) {
			continue;
		}
		ea_sample[ns++] = ea;
	}

	if ( ns != n && ns < 1 ) {
		printf("\
%d samples faild, return -1\n", n-ns);
		return -1;
	} else if ( ns != n ) {
		printf("\
%d samples faild, ignored\n", n-ns);
	}

	av_t = (t0 + t1) / 2.0;
	av_mjd = aste2mjd(av_t);
	average_euler(ns, ea_sample, &av_ea);
	atEulerToRM(&av_ea, av_rm);
	atInvRotMat(av_rm, av_irm);
	aste_euler2skyref(teldef, &av_ea, &av);

	printf("\
  Mean satellite Euler angles :%15.6f%15.6f%15.6f\n",
		   av_ea.phi*RAD2DEG, av_ea.theta*RAD2DEG, av_ea.psi*RAD2DEG);

	atSun(av_mjd, vSun);
	atVectToPolDeg(vSun, &r, &av_sun.alpha, &av_sun.delta);
	printf("\
\n\
                                       RA             DEC         SUN ANGLE\n\
\n\
  Mean Sun position     (deg) :%15.6f%15.6f\n", av_sun.alpha, av_sun.delta);

	atPolDegToVect(1.0, av.alpha, av.delta, vFov);
	atAberration(av_mjd, vFov, vAbr);
	atVectToPolDeg(vAbr, &r, &av_abr.alpha, &av_abr.delta);
	alpha = delta_phi(av_abr.alpha, av.alpha);
	delta = av_abr.delta - av.delta;
	printf("\
  Mean aberration    (arcsec) :%15.6f%15.6f\n\n", alpha*60*60, delta*60*60);

	v0[1] = v0[2] = 0.0; v0[0] = 1.0;
	atRotVect(av_irm, v0, v);
	atVectToPolDeg(v, &r, &alpha, &delta);
	atAngDistance(v, vSun, &r);
	printf("\
  Mean satellite X-axis (deg) :%15.6f%15.6f%15.6f\n", alpha, delta, r*RAD2DEG);
	v0[0] = v0[2] = 0.0; v0[1] = 1.0;
	atRotVect(av_irm, v0, v);
	atVectToPolDeg(v, &r, &alpha, &delta);
	atAngDistance(v, vSun, &r);
	printf("\
  Mean satellite Y-axis (deg) :%15.6f%15.6f%15.6f\n", alpha, delta, r*RAD2DEG);
	v0[0] = v0[1] = 0.0; v0[2] = 1.0;
	atRotVect(av_irm, v0, v);
	atVectToPolDeg(v, &r, &alpha, &delta);
	atAngDistance(v, vSun, &r);
	printf("\
  Mean satellite Z-axis (deg) :%15.6f%15.6f%15.6f\n", alpha, delta, r*RAD2DEG);

	alpha0 = alpha1 = av.alpha;
	delta0 = delta1 = av.delta;
	roll0  = roll1  = av.roll;
	offset1 = 0.0;
	sg.alpha = sg.delta = sg.roll = 0.0;
	sg_offset = 0.0;
	for (i = 0; i < ns; i++) {
		aste_euler2skyref(teldef, &ea_sample[i], &fov);
		alpha = av.alpha + delta_phi(fov.alpha, av.alpha);
		delta = fov.delta;
		roll  = av.roll  + delta_phi(fov.roll,  av.roll);
		if ( alpha < alpha0 ) alpha0 = alpha;
		if ( delta < delta0 ) delta0 = delta;
		if ( roll  < roll0  ) roll0  = roll;
		if ( alpha > alpha1 ) alpha1 = alpha;
		if ( delta > delta1 ) delta1 = delta;
		if ( roll  > roll1  ) roll1  = roll;
		atPolDegToVect(1.0, fov.alpha, fov.delta, v);
		atAngDistance(vFov, v, &offset);
		offset *= RAD2ARCMIN;
		if ( offset > offset1 ) offset1 = offset;
		sg.alpha += (alpha - av.alpha) * (alpha - av.alpha);
		sg.delta += (delta - av.delta) * (delta - av.delta);
		sg.roll  += (roll  - av.roll)  * (roll  - av.roll);
		sg_offset += offset * offset;
	}
	sg.alpha = sqrt(sg.alpha/ns);
	sg.delta = sqrt(sg.delta/ns);
	sg.roll  = sqrt(sg.roll/ns);
	sg_offset = sqrt(sg_offset/ns);
	printf("\
\n\
                      RA(deg)        DEC(deg)       ROLL(deg)    OFFSET(arcmin)\n\
\n\
  Average       %15.6f%15.6f%15.6f\n", av.alpha, av.delta, av.roll);
	printf("\
  Minimum       %15.6f%15.6f%15.6f\n", alpha0, delta0, roll0);
	printf("\
  Maximum       %15.6f%15.6f%15.6f%15.6f\n", alpha1, delta1, roll1, offset1);
	printf("\
  Sigma (RMS)   %15.6f%15.6f%15.6f%15.6f\n", sg.alpha, sg.delta, sg.roll, sg_offset);

#define BnkDefPut(name, v) \
	BnkDef(name, sizeof(v)); BnkfPutM(name, sizeof(v), &v);

	BnkDefPut("ASPECT:MEAN:EULER", av_ea);
	BnkDefPut("ASPECT:MEAN:SKYREF", av);
	BnkDefPut("ASPECT:RMS:SKYREF", sg);
	BnkDefPut("ASPECT:MAX:OFFSET", offset1);
	BnkDefPut("ASPECT:RMS:OFFSET", sg_offset);

	return 0;
}
