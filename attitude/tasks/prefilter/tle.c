/*
 * $Source: /headas/headas/attitude/tasks/prefilter/tle.c,v $
 * $Revision: 1.9 $
 * $Date: 2005/09/14 21:41:12 $
 *
 * $Log: tle.c,v $
 * Revision 1.9  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.8  2003/07/21 22:21:07  rwiegand
 * Was failing to copy over the scale in vector_rotate.
 *
 * Revision 1.7  2003/07/21 15:33:19  rwiegand
 * Apply rotation from TEME (SGP coordinate system) to TOD J2000 in
 * tle_to_position.  Allow text TLE files to have blanks in international
 * designator, second derivative of motion and bstar fields.
 *
 * Revision 1.6  2002/03/15 18:09:29  rwiegand
 * Added flags to check SGP4/SDP4 modelling for TLE propagation
 *
 * Revision 1.5  2002/01/28 16:10:02  rwiegand
 * Added vector utility functions.  Make use of vector scale field.
 *
 * Revision 1.4  2001/11/06 21:05:07  rwiegand
 * Report warning when using deep space TLEs
 *
 * Revision 1.3  2001/11/06 16:25:12  rwiegand
 * Moved input and output routines to tleio.[ch]
 *
 * Revision 1.2  2001/11/01 15:43:29  rwiegand
 * Updated units in display of two line elements.  Apply debug flags
 *
 * Revision 1.1  2001/11/01 14:45:34  rwiegand
 * Initial revision
 *
 */

#include <math.h>
#include <errno.h>
#include <string.h>

#include "report.h"
#include "datetime.h"
#include "tle.h"
#include "tleio.h"
#include "sgp.h"


#define KSKelso 0
#if KSKelso
Unit SGP_Conv;
{           Author:  Dr TS Kelso }
{ Original Version:  1991 Oct 30}
{ Current Revision:  1992 Sep 03}
{          Version:  1.00 }
{        Copyright:  1992, All Rights Reserved }
#endif



double julian_date_of_epoch (double epoch);
static double thetag_jd (double julian);

static void calculate_geodetic (const vector_t *pos, const datetime_t *t,
        geodetic_t *geodetic);


void vector_normalize (vector_t *p)
{
  double inverse;

  p->scale = sqrt(p->x * p->x + p->y * p->y + p->z * p->z);

  inverse = 1 / p->scale;
  p->x *= inverse;
  p->y *= inverse;
  p->z *= inverse;
}


void vector_denormalize (vector_t *p)
{
  p->x *= p->scale;
  p->y *= p->scale;
  p->z *= p->scale;
  p->scale = 1;
}


void vector_unitize (vector_t *p)
{
  vector_normalize(p);
  p->scale = 1;
}


void vector_scale (vector_t *p, double scale)
{
  p->scale *= scale;
}


void vector_scale_x (const vector_t *in, vector_t *out, double scale)
{
  out->x = in->x;
  out->y = in->y;
  out->z = in->z;
  out->scale = in->scale * scale;
}


void vector_rotate (const vector_t *in, double rm[3][3], vector_t *out)
{
  out->x = in->x * rm[0][0] + in->y * rm[1][0] + in->z * rm[2][0];
  out->y = in->x * rm[0][1] + in->y * rm[1][1] + in->z * rm[2][1];
  out->z = in->x * rm[0][2] + in->y * rm[1][2] + in->z * rm[2][2];
  out->scale = in->scale;
}


#if KSKelso
SGP_TIME.PAS
Function Julian_Date_of_Epoch(epoch : double) : double;
  var
    year,day : double;
  begin
{ Modification to support Y2K }
{ Valid 1957 through 2056 }
  year := Int(epoch*1E-3);
  if year < 57 then
    year := year + 2000
  else
    year := year + 1900;
{ End modification }
  day  := Frac(epoch*1E-3)*1E3;
  Julian_Date_of_Epoch := Julian_Date_of_Year(year) + day;
  end; {Function Julian_Date_of_Epoch}
#endif

double julian_date_of_epoch (double epoch)
{
  double julian;
  double year, doy;
  doy = modf(epoch * 1e-3, &year) * 1e3;
  if (year < 57)
    year += 2000;
  else
    year += 1900;
  julian = julian_date_of_year(year) + doy;
  return julian;
}


/* prepares TLE for use (SGP*-SDP*) */
void preprocess_elements(tlesgp_t *tle)
{
  double ao,xnodp,dd1,dd2,delo,temp,a1,del1,r1;

  if (tle->raw)
    {
      /* Preprocess tle set */
      tle->raw = 0;
      tle->xnodeo *= sgp_de2ra;
      tle->omegao *= sgp_de2ra;
      tle->xmo *= sgp_de2ra;
      tle->xincl *= sgp_de2ra;
      temp = sgp_twopi / sgp_xmnpda / sgp_xmnpda;
      tle->xno *= temp * sgp_xmnpda;
      tle->xndt2o *= temp;
      tle->xndd6o *= temp / sgp_xmnpda;
      tle->bstar /= sgp_ae;
    }

  /* Period > 225 minutes is deep space */
  dd1 = (sgp_xke / tle->xno);
  dd2 = sgp_tothrd;
  a1 = pow(dd1, dd2);
  r1 = cos(tle->xincl);
  dd1 = (1.0 - tle->eo * tle->eo);
  temp = sgp_ck2 * 1.5 * (r1 * r1 * 3.0 - 1.0) / pow(dd1, 1.5);
  del1 = temp / (a1 * a1);
  ao = a1 * (1.0 - del1 * (sgp_tothrd * .5
        + del1 * (del1 * 1.654320987654321 + 1.0)));
  delo = temp / (ao * ao);
  xnodp = tle->xno / (delo + 1.0);

  /* Select a deep-space/near-earth ephemeris */
  if (sgp_twopi / xnodp / sgp_xmnpda >= .15625)
    tle->deepspace = 1;
  else
    tle->deepspace = 0;

}


#ifdef CRAIGS_NOTE

From: Craig Markwardt <craigm@xylo.gsfc.nasa.gov>
MIME-Version: 1.0
Content-Type: multipart/mixed; boundary="OT5ToPFVDT"
Content-Transfer-Encoding: 7bit
Message-ID: <16096.8660.723512.960353@xylo.gsfc.nasa.gov>
Date: Fri, 6 Jun 2003 01:08:36 -0400
To: craigm@milkyway.gsfc.nasa.gov
Cc: Bob Wiegand <robert.e.wiegand@nasa.gov>, pier@ssvs.gsfc.nasa.gov
Subject: Re: SGP and coordinate systems
In-Reply-To: <16091.63333.977680.883632@xylo.gsfc.nasa.gov>
References: <16090.29540.776103.264719@xylo.gsfc.nasa.gov>
	<5.0.2.1.2.20030602092626.00af1e28@pop500.gsfc.nasa.gov>
	<16091.63333.977680.883632@xylo.gsfc.nasa.gov>
X-Mailer: VM 7.01 under Emacs 20.7.1
Reply-To: craigm@milkyway.gsfc.nasa.gov


Craig Markwardt writes:
 > 
 > I''ll research what needs to be done.  I would be looking for only the
 > leading order terms in the computation, to get to ~arcsec precision.
 > 
 > You can get the time of the observation in Julian days, right?  [ As
 > in, 2400000.5 + MJDREF + (TIMEZERO + (TSTART+TSTOP)/2)/86400 ].

Greetings--

I''m attaching a routine in IDL which converts from the coordinate
system of the TLE (mean equinox of date, true equator of date), to the
standard J2000 coordinate frame that everybody expects.  I used
standard derivations from Meeus, a well known authority, and the
Explanatory Supplement to the Astronomical Almanac.  I''ve tested
against my own high-precision routines, and they agree to the extent
that the approximations apply.

While the code is in IDL, the logic is purely linear, and I''ve made
all the matrix notation very explicit, even to the extent of spelling
out the matrix multiplications.

An example is given, so you can test your code.

Since the precession and nutation does not change rapidly with time
you can simply compute the matrix once, at the middle of an
observation, as I outlined above.  The routine requires the Julian
date at the epoch of interest.  It then returns a rotation matrix
which you can apply to positions and velocities.  It should be as
simple as that!

Let me know if you have questions.

Yours,
Craig



;;
;; ORBIT_CORRECT
;;
;; Compute correction matrix for orbits found from the NORAD two-line
;; elements.  According to Kelso, these coordinates are expressed in
;; the true-of-date equator, and mean-of-date equinox.  Whatever!
;;
;; To convert to the mean equator and equinox of J2000.0, the
;; standard, a rotation must be applied to the positions and
;; velocities, which accounts for nutation of the equator, and
;; precision of the equator and equinox.
;;
;; For nutation, this procedure uses the simplified formulation of
;; Meeus.
;; 
;; For precession, this procedure uses the IAU 1976 model, as
;; described in Meeus, or the Explanatory Supplement to the
;; Astronomical Almanac.
;;
;; Meeus, J. 1991, *Astronomical Algorithms*, Willmann-Bell, Virginia
;;
;; Seidelmann, P.K. 1992, *Explanatory Supplement to the Astronomical
;;   Almanac*, ISBN 0-935702-68-7
;; 
;;
;; INPUTS:
;;    JD - julian date of interest [days]
;;
;; OUTPUTS:
;;    MAT - a 3x3 matrix, to be applied to both the positions and
;;          velocities, as in:
;;
;;          R_J2000 = DBLARR(3)
;;          FOR I = 0, 2 DO R_J2000(I) = TOTAL(MAT(*,I)*R_TLE(*))
;;  
;; Note: I''ve made all the matrix notation explicit throughout this
;; function, so there should be no ambiguity in the way that matrix
;; multiplication should be applied.
;;
;; Example:
;;    Julian date for 01 Jan 2010 is JD = 2455197.5D
;;    ORBIT_CORRECT, JD, MAT
;;    PRINT, MAT

;;          0.99999703    0.0022362243   0.00097172647
;;       -0.0022362377      0.99999750   1.2675669e-05
;;      -0.00097169570  -1.4848643e-05      0.99999953
;;

;;    Dummy state vector to convert
;;        r_tle = [3050d, 4040d, 500d]  ; [km], referred to TLE frame
;;
;;        r_j2000 = dblarr(3)        
;;        FOR I = 0, 2 DO R_J2000(I) = TOTAL(MAT(*,I)*R_TLE(*))
;;
;;    Output is referred to J2000 coordinates
;;        print, r_j2000
;;           3059.5111       4033.1757       496.97610 ; [km] ref. to J2000
;;
;;
;;  C. Markwardt, 06 Jun 2003
;;

pro orbit_correct, jd, mat

  DTOR = !dpi / 180d
  ASTOR = DTOR / 3600d


  ;; Find the time T, measured in Julian centures from the epoch J2000.0
  T = (jd - 2451545d0) / 36525d0


  ;; Meeus Ch. 21
  ;; Compute the longitude of the ascending node of the Moon''s mean
  ;; orbit on the ecliptic, measured from the mean equinox of date.
  om = 125.04452d - 1934.136261d * T   ;; [deg]

  ;; Mean longitude of the sun and moon
  ls = 280.4665d +  36000.7698d * T    ;; [deg]
  lm = 218.3165d + 481267.8813d * T    ;; [deg]

  ;; Convert arguments to radians
  om = om * DTOR & ls = ls * DTOR & lm = lm * DTOR  ; [radian]

  ;; Nutation in obliquity  ; [arcsec]
  deps = +9.20*cos(om) + 0.57*cos(2*ls) + 0.10*cos(2*lm) - 0.09*cos(2*om)
  ;; Note that nutation in longitude is not required since the TLE''s
  ;; are already referred to the mean equinox of date.

  ;; Precession angles (Meeus Ch. 20; ESAA Sec. 3.2)
  zeta  = 2306.2181d*T + 0.30188d*T^2 + 0.017998d*T^3 ; [arcsec]
  z     = 2306.2181d*T + 1.09468d*T^2 + 0.018203d*T^3 ; [arcsec]
  theta = 2004.3109d*T - 0.42665d*T^2 - 0.041833d*T^3 ; [arcsec]

  ;; Convert arcsec to radians 
  deps  = deps  * ASTOR ; [radian]
  zeta  = zeta  * ASTOR ; [radian]
  z     = z     * ASTOR ; [radian]
  theta = theta * ASTOR ; [radian]

  ;; 3x3 precession matrix, three Euler angles 
  ;;      PRECMAT = R_z(-zeta) R_y(+theta) R_z(-z) 
  precmat = dblarr(3,3)
  precmat(0,0) = cos(zeta)*cos(theta)*cos(z)-sin(zeta)*sin(z)
  precmat(0,1) = -sin(zeta)*cos(theta)*cos(z) - cos(zeta)*sin(z)
  precmat(0,2) = -sin(theta)*cos(z)
  precmat(1,0) = cos(zeta)*cos(theta)*sin(z) + sin(zeta)*cos(z)
  precmat(1,1) = -sin(zeta)*cos(theta)*sin(z) + cos(zeta)*cos(z)
  precmat(1,2) = -sin(theta)*sin(z)
  precmat(2,0) = cos(zeta)*sin(theta)
  precmat(2,1) = -sin(zeta)*sin(theta)
  precmat(2,2) = cos(theta)

  ;; 3x3 nutation matrix = R_x(-deps)
  nutmat = dblarr(3,3)
  nutmat(0,0) = 1
  nutmat(1,1) = cos(-deps)
  nutmat(1,2) = sin(-deps)
  nutmat(2,1) = -sin(-deps)
  nutmat(2,2) = cos(-deps)

  ;; Form matrix product of precession and nutation matrices
  mat = dblarr(3,3)
  for i = 0, 2 do for j = 0, 2 do $
    mat(i,j) = total(precmat(*,j)*nutmat(i,*))

  return
end

#else


void teme_to_mean_of_j2000_correction (double jd, double rm[3][3])
{
  const double DTOR = M_PI / 180;
  const double ASTOR = DTOR / 3600;
  int i, j, k;
  double om, ls, lm, deps;
  double zeta, z, theta;
  double precmat[3][3];
  double nutmat[3][3];

  /* Find the time T, measured in Julian centures from the epoch J2000.0 */
  double T = (jd - 2451545) / 36525;
  double T2 = T * T;
  double T3 = T2 * T;

  /*
  * Meeus Ch. 21
  * Compute the longitude of the ascending node of the Moon's mean
  * orbit on the ecliptic, measured from the mean equinox of date.
  */
  om = 125.04452 - 1934.136261 * T ;   /* [deg] */

  /* Mean longitude of the sun and moon */
  ls = 280.4665 +  36000.7698 * T ;    /* [deg] */
  lm = 218.3165 + 481267.8813 * T ;    /* [deg] */

  /* Convert arguments to radians */
  om = om * DTOR, ls = ls * DTOR, lm = lm * DTOR;  /* [radian] */

  /* Nutation in obliquity  [arcsec] */
  deps = +9.20 * cos(om) + 0.57 * cos(2 * ls)
      + 0.10 * cos(2 * lm) - 0.09 * cos(2 * om);
  /* Note that nutation in longitude is not required since the TLE's
  * are already referred to the mean equinox of date. */

  /* Precession angles (Meeus Ch. 20; ESAA Sec. 3.2) */
  zeta  = 2306.2181 * T + 0.30188 * T2 + 0.017998 * T3;   /* [arcsec] */
  z     = 2306.2181 * T + 1.09468 * T2 + 0.018203 * T3;   /* [arcsec] */
  theta = 2004.3109 * T - 0.42665 * T2 - 0.041833 * T3;   /* [arcsec] */

  /* Convert arcsec to radians */
  deps  = deps  * ASTOR ;   /* [radian] */
  zeta  = zeta  * ASTOR ;   /* [radian] */
  z     = z     * ASTOR ;   /* [radian] */
  theta = theta * ASTOR ;   /* [radian] */

  /*
  * 3x3 precession matrix, three Euler angles 
  * PRECMAT = R_z(-zeta) R_y(+theta) R_z(-z) 
  */
  precmat[0][0] = cos(zeta) * cos(theta) * cos(z) - sin(zeta) * sin(z);
  precmat[0][1] = -sin(zeta) * cos(theta) * cos(z) - cos(zeta) * sin(z);
  precmat[0][2] = -sin(theta) * cos(z);
  precmat[1][0] = cos(zeta) * cos(theta) * sin(z) + sin(zeta) * cos(z);
  precmat[1][1] = -sin(zeta) * cos(theta) * sin(z) + cos(zeta) * cos(z);
  precmat[1][2] = -sin(theta) * sin(z);
  precmat[2][0] = cos(zeta) * sin(theta);
  precmat[2][1] = -sin(zeta) * sin(theta);
  precmat[2][2] = cos(theta);

  /* 3x3 nutation matrix = R_x(-deps) */
  nutmat[0][0] = 1;
  nutmat[0][1] = 0;
  nutmat[0][2] = 0;
  nutmat[1][0] = 0;
  nutmat[1][1] = cos(-deps);
  nutmat[1][2] = sin(-deps);
  nutmat[2][0] = 0;
  nutmat[2][1] = -sin(-deps);
  nutmat[2][2] = cos(-deps);

  /* Form matrix product of precession and nutation matrices */
  for (i = 0; i < 3; ++i)
    for (j = 0; j < 3; ++j) {
      double total = 0;
      for (k = 0; k < 3; ++k)
        total += precmat[k][j] * nutmat[i][k];
      rm[i][j] = total;
    }

}


#endif



void test_teme_to_mean_of_j2000 ()
{
  int i, j;

  /* Julian date for 01 Jan 2010 is JD = 2455197.5D */
  double jd = 2455197.5;

  double rm[3][3];
  double r_tle[3] = { 3050, 4040, 500 };
  double r_j2000[3];

  teme_to_mean_of_j2000_correction(jd, rm);

  /* IDL varies first index fastest */
  for (j = 0; j < 3; ++j) {
    for (i = 0; i < 3; ++i)
      printf("%e ", rm[i][j]);
    printf("\n");
  }

  for (i = 0; i < 3; ++i) {
    double total = 0;
    for (j = 0; j < 3; ++j)
      total += rm[j][i] * r_tle[j];
    r_j2000[i] = total;

    printf("%e ", total);
  }
  printf("\n");

}


int tle_to_position_teme (
      const tlesgp_t *tle,
      const datetime_t *epoch,
      vector_t *position,
      vector_t *velocity,
      int flags)
{
  int error = 0;
  tlesgp_t work;
  int verbose = flags & TLE_VERBOSE;
  double minutes;
  double tle_julian;
  double epoch_julian;
  int tleflags = 0;
  void (*model)(double, const tlesgp_t *, int *, vector_t *, vector_t *) = 0;

  work = *tle;
  if (verbose)
    print_tle(stdout, &work, "before preprocessing");
  preprocess_elements(&work);
  if (verbose)
    print_tle(stdout, &work, "after preprocessing");

  if (work.deepspace)
    {
      model = &sgp_sdp4;
      report_warning("support for deep space TLEs is known to be inaccurate\n");
      if (flags & TLE_SHALLOW)
        {
          error = 1;
          report_warning("flags and TLE disagree on modelling");
        }
    }
  else
    {
      model = &sgp_sgp4;
      if (flags & TLE_DEEPSPACE)
        {
          error = 1;
          report_warning("flags and TLE disagree on modelling");
        }
    }

  if (!error)
    {
      datetime_get_julian(epoch, &epoch_julian);
      tle_julian = julian_date_of_epoch(work.epoch);
      minutes = (epoch_julian - tle_julian) * sgp_xmnpda;
      (*model)(minutes, &work, &tleflags, position, velocity);

      vector_denormalize(position);
      vector_denormalize(velocity);
    }

  if (verbose)
    {
      char buffer[80];
      sprintf(buffer, "position (ECI, TEME, km) at %f (julian)\n",
		      epoch_julian);
      vector_print(position, stdout, buffer);
      sprintf(buffer, "velocity (ECI, TEME, km/s) at %f (julian)\n",
		      epoch_julian);
      vector_print(velocity, stdout, buffer);
    }

  return error;
}


int tle_to_position (
      const tlesgp_t *tle,
      const datetime_t *epoch,
      vector_t *position,
      vector_t *velocity,
      int flags)
{
  int error = 0;
  vector_t pos_teme;
  vector_t vel_teme;
  int verbose = flags & TLE_VERBOSE;
  double epoch_julian;
  double rm[3][3];

  error = tle_to_position_teme(tle, epoch, &pos_teme, &vel_teme, flags);

  datetime_get_julian(epoch, &epoch_julian);
  teme_to_mean_of_j2000_correction(epoch_julian, rm);
  vector_rotate(&pos_teme, rm, position);
  vector_rotate(&vel_teme, rm, velocity);

  if (verbose)
    {
      char buffer[80];
      sprintf(buffer, "position (ECI, J2000, km) at %f (julian)\n",
		      epoch_julian);
      vector_print(position, stdout, buffer);
      sprintf(buffer, "velocity (ECI, J2000, km/s) at %f (julian)\n",
		      epoch_julian);
      vector_print(velocity, stdout, buffer);
    }

  return error;
}


int tle_to_geodetic (
      const tlesgp_t *tle,
      const datetime_t *epoch,
      geodetic_t *geodetic,
      int flags)
{
  int code = 0;
  vector_t position;
  vector_t velocity;
  code = tle_to_position(tle, epoch, &position, &velocity, flags);
  if (!code)
    calculate_geodetic(&position, epoch, geodetic);
  return code;
}


#if KSKelso
Procedure Calculate_LatLonAlt(pos : vector;
                             time : double;
                     var geodetic : vector);
{ Reference:  The 1992 Astronomical Almanac, page K12. }
  var
    lat,lon,alt,
    theta,r,e2,phi,c : double;
  begin
  theta := AcTan(pos[2],pos[1]);
  lon := Modulus(theta - ThetaG_JD(time),twopi);
  r := Sqrt(Sqr(pos[1]) + Sqr(pos[2]));
  e2 := f*(2 - f);
  lat := AcTan(pos[3],r);
  repeat
    phi := lat;
    c := 1/Sqrt(1 - e2*Sqr(Sin(phi)));
    lat := AcTan(pos[3] + sgp_xkmper*c*e2*Sin(phi),r);
  until Abs(lat - phi) < 1E-10;
  alt := r/Cos(lat) - sgp_xkmper*c;
  geodetic[1] := lat;   {radians}
  geodetic[2] := lon;   {radians}
  geodetic[3] := alt;   {kilometers}
  geodetic[4] := theta; {radians}
  end; {Procedure Calculate_LatLonAlt}

#else

void
calculate_geodetic (const vector_t *pos, const datetime_t *t,
        geodetic_t *geodetic)
{
  double lat, lon, alt, theta, r, e2, phi, sinphi, c, delta;
  double julian;

  datetime_get_julian(t, &julian);
  theta = sgp_actan(pos->y, pos->x);
  lon = sgp_modulus(theta - thetag_jd(julian), sgp_twopi);
  r = sqrt(pos->x * pos->x + pos->y * pos->y);
  e2 = sgp_f * (2 - sgp_f);
  lat = sgp_actan(pos->z, r);
  do
    {
      phi = lat;
      sinphi = sin(phi);
      c = 1 / sqrt(1 - e2 * sinphi * sinphi);
      lat = sgp_actan(pos->z + sgp_xkmper * c * e2 * sinphi, r);
      delta = fabs(lat - phi);
    } while (delta >= 1e-10);

  alt = r / cos(lat) - sgp_xkmper * c;

  geodetic->latitude  = lat;     /* radians */
  geodetic->longitude = lon;     /* radians */
  geodetic->altitude  = alt;     /* kilometers */
  geodetic->theta     = theta;   /* radians */
}
#endif


#if KSKelso
from SGP_TIME.PAS
Function ThetaG_JD(jd : double) : double;
{ Reference:  The 1992 Astronomical Almanac, page B6. }
  var
    UT,TU,GMST : double;
  begin
  UT   := Frac(jd + 0.5);
  jd   := jd - UT;
  TU   := (jd - 2451545.0)/36525;
  GMST := 24110.54841 + TU * (8640184.812866 + TU * (0.093104 - TU * 6.2E-6));
  GMST := Modulus(GMST + secday*omega_E*UT,secday);
  ThetaG_JD := twopi * GMST/secday;
  end; {Function ThetaG_JD}

#else

double thetag_jd (double julian)
{
  double thetag, UT, TU, GMST, jdp, jd;
  jdp = julian + 0.5;
  UT = jdp - floor(jdp);
  jd = julian - UT;
  TU = (jd - 2451545.0) / 36525;
  GMST = 24110.54841 + TU * (8640184.812866 + TU * (0.093104 - TU * 6.2e-6));
  GMST = sgp_modulus(GMST + sgp_secday * sgp_omega_E * UT, sgp_secday);
  thetag = sgp_twopi * GMST / sgp_secday;
  return thetag;
}

#endif


#undef ARG_DEFAULT
#define ARG_DEFAULT(field, default) \
	if (args->field) field = args->field; \
	else field = default;

void vector_print_x (const vector_t *v, const vector_print_t *args)
{
	char format[1024];

	const char *header, *prefix, *postfix, *real;
	int scale;
	FILE *file;

	ARG_DEFAULT(header, "vector\n");
	ARG_DEFAULT(prefix, "\t");
	ARG_DEFAULT(postfix, "\n");
	ARG_DEFAULT(real, "%f");
	ARG_DEFAULT(scale, 1);
	ARG_DEFAULT(file, stdout);

	if (header)
		fputs(header, file);

	sprintf(format, "%s%%c %s%s",
			prefix, real, postfix);

	fprintf(file, format, 'x', v->x);
	fprintf(file, format, 'y', v->y);
	fprintf(file, format, 'z', v->z);

	if (scale)
		fprintf(file, format, 's', v->scale);
}



void vector_print (const vector_t *v, FILE *file, const char *header)
{
	vector_print_t args = { 0 };
	args.header = header;
	vector_print_x(v, &args);
}

