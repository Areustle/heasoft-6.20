/** \file aber.c
 *  \brief Support routines for computing aberration
 *  \author Robert S. Hill
 *  \date $Date: 2015/05/14 22:17:15 $
 */

#include "aber.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "coordfits2.h"
#include "atFunctions.h"

#ifdef __cplusplus
}
#endif

#include "ahlog/cahlog.h"

#include <math.h>
#include <stdio.h>

#define USE_ATFUNCTIONS 1

/* Negate the components of a 3-vector. */
void negate3Vector
(
  double v[3] /* Vector to negate */
)
{
  v[0] = -v[0];
  v[1] = -v[1];
  v[2] = -v[2];
}

/* -------------------------------------------------------------------------- */
/* Several utility routines were adapted from aberrator task. */

/* Convert longitude and latitude in degrees to a Cartesion unit vector */
/* Adapted from aberrator task. */
void convertLongLatDegToUnitv 
(
  double longd,      /* Longitude in degrees */
  double latd,       /* Latitude in degrees */
  double unitv[3]    /* Unit vector */
)
{
  double longr = 0.0;
  double latr = 0.0;

  longr = longd*M_PI/180.0;
  latr = latd*M_PI/180.0;

  unitv[0] = cos(longr) * cos(latr);
  unitv[1] = sin(longr) * cos(latr);
  unitv[2] = sin(latr);
}

/* -------------------------------------------------------------------------- */

/* Convert a Cartesion unit vector to longitude and latitude in degrees */
/* Adapted from aberrator task. */
int convertUnitvToLongLatDeg
(
  double unitv[3],   /* Unit vector */
  double* longd,     /* Longitude in degrees */
  double* latd       /* Latitude in degrees */
  /* Returns 0 if ok, 1 if null unit vector was input */
)
{
  const double EPSILON_TEST = 1e-12;
  double rho2=0.0, norm=0.0, rho=0.0;
  double longr=0.0, latr=0.0;


  rho2 = unitv[0] * unitv[0] + unitv[1] * unitv[1];
  norm = sqrt(rho2 + unitv[2] * unitv[2]);

  if (norm == 0)
    {
      *longd = *latd = 0;
      fprintf(stderr,"Cannot convert null unit vector to long and lat\n");
      return 1;
    }

  rho = sqrt(rho2);
  latr = asin(unitv[2] / norm);

  if (rho < EPSILON_TEST)
    longr = 0;
  else
    {
      double c, s;

      c = unitv[0] / rho;
      s = unitv[1] / rho;

      if (fabs(s) < EPSILON_TEST)
        longr = (1 - c / fabs(c)) * M_PI / 2;
      else
        longr = 2 * atan((1 - c) / s);
    }

  if (longr > M_PI * 2)
    longr -= M_PI * 2;

  else if (longr < 0)
    longr += M_PI * 2;

  *longd = longr*180.0/M_PI;
  *latd = latr*180.0/M_PI;

  return 0;
}

/* ---------------------------------------------------------------------- */

/* Calculate the earth velocity vector in units of c (speed of light)
   at a given Modified Julian Date. */
/* Adapted from aberrator task. */
void earthVelcAtMJD
(
  double mjd,        /* Modified Julian Date */
  double vel_c[3]    /* Earth velocity in units of c */
) 
{
#if defined(USE_ATFUNCTIONS)

  const int EARTH = 2;
  const double km_per_au = 149597870;  /* Size of an A. U. */
  const double delta_s = 60; /* Estimate velocity over 2 * 30s */
  const double delta_mjd = delta_s / 86400; /* Modified Julian data difference (days) */
  const double c_km_per_s = 299792.458;
  
  int i; /* Velocity component index */
  AtVect p1[10], p2[10]; /* Earth positions */
  double size[10]; /* Visual size of object */
  double mag[10]; /* Visual magnitude of object */
  double vel_km_per_s[3]; /* Velocity vector in km/s */
  
  /* Calculate the position of the Sun relative to the Earth in
   * rectangular equatorial celestial coordinates.  The library
   * function for this also calculates the positions of the planets,
   * with index 2 representing the Sun.  Do the calculation for two
   * times that surround the desired time. */
  
  atPlanet(mjd - delta_mjd / 2, p1, size, mag);
  atPlanet(mjd + delta_mjd / 2, p2, size, mag);
  
  /* Calculate the Earth's velocity in km/s from the difference in
     Earth positions and times. Negate the vector to represent the
     Earth's position relative to the Sun. */
  
  for (i = 0; i < 3; ++i)
    vel_km_per_s[i] = -(p2[EARTH][i] - p1[EARTH][i]) * km_per_au / delta_s;
  
  /* Convert the Earth's velocity to units of c (speed of light). */
  
  for (i = 0; i < 3; ++i)
    vel_c[i] = vel_km_per_s[i] / c_km_per_s;
  
#elif defined(USE_SLALIB)
  
  const double equinox = 2000;
  double evb[3], epb[3], evh[3], eph[3];
  
  slaEvp(mjd, equinox, evb, epb, evh, eph);
  
  for (i = 0; i < 3; ++i)
    vel_c[i] = evb[i] / c_km_per_s;
  
#else
  /* attitude/libephemeris */
  
  double v, vhat[3];
  v = earth_velocity(vhat, earth_longitude(mjd));
  
  /*
   * Note that v*vhat is the velocity of the sun about the
   * earth in units of c.
   * I am suspicious of the accuracy of this model.
   * It differs by several percent on my test cases from GTDS and AST
   */
  
  for (i = 0; i < 3; ++i)
    vel_c[i] = -v * vhat[i];
  
#endif
}

/* ---------------------------------------------------------------------- */

/* Compute the vector sum of earth and satellite velocities. */

int calcTotalSatVelocity 
(
  double *v_total,       /* Magnitude of total velocity */
  double vhat_total[3],  /* Unit vector parallel to total velocity */
  double v_earth,        /* Magnitude of earth velocity */
  double vhat_earth[3],  /* Unit vector parallel to earth velocity */
  double v_sat,          /* Magnitude of satellit velocity */
  double vhat_sat[3]     /* Unit vector parallel to satellite velocity */
  /* Returns 0 if OK, 1 if magnitude of total velocity is zero */
)
{

  double v[3];  /* Sum of velocity components. */
  int i = 0;    /* Velocity component index. */
  const int num_i = 3;  /* Number of velocity components. */

  /* int sign[3] = {-1, -1, 1};  Sign flips to apply to orbital velocity components. */
  int sign[3] = {1, 1, 1};

  /*
   * The satellite orbit velocities in the orbit files are defined in the
   * Earth-Centered-Inertial frame and referenced to the center of the
   * Earth. So the Z-axis points toward the North Pole (and NCP), the
   * X-axis points to the Vernal Equinox and the Y-axis to the Summer
   * Solstice. This system is thus fixed to the celestial sphere and
   * doesn't change as the earth rotates or revolves around the Sun.
   *
   * In the case of the Earth velocity used in the annual aberration
   * correction, the frame is the same, but the axis definitions are
   * different, since in that case one is looking out toward the Earth from
   * the center of the Sun. So X points toward the Autumnal Equinox and Y
   * toward the Winter Solstice.
   *
   * Thus to first order, adding the velocities is simple, just flip the
   * signs of X and Y orbital velocities and add. To higher order, there
   * could well be some small subtleties like a slight misalignment
   * between the Earth North Pole vector and the North Celestial Pole,
   * but to first order they share a common Z axis. */
   
  /* Sum the velocities, scaling the unit vectors by their magnitudes. */

  for(i = 0; i < num_i; i++)
  {
    v[i] = v_earth*vhat_earth[i] + sign[i]*v_sat*vhat_sat[i];
  }

  /* Calculate the magnitude of the total velocity.
   * Exit with non-zero return value if the magnitude is 0. */

  *v_total = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);

  if (*v_total == 0) return 1;

  /* Calculate the unit vector of the total velocity. */

  for (i = 0; i < num_i; i++)
  {
    vhat_total[i] = v[i] / *v_total;
  }
  return 0;

}

/* -------------------------------------------------------------------------- */

/* General routine to find the total satellite velocity for use in the aberration correction.  This 
 * can include the velocity of the satellite around the earth (found in an orbit file) and/or the 
 * velocity of the earth around the sun (calculated from first principles given the current time).  
 * The velocity is scaled by the speed of light and returned as two separate variables:  
 * magnitude and 3-vector direction.  This separation is needed for compatibility with 
 * downstream routines.
 * IMPORTANT NOTE:  The correction (from aberrated -> mean position) is the negative of the 
 * total velocity vector.  Therefore this routine returns the negative of the velocity (unless 
 * invert_earth_vel or invert_sat_vel are true). */

int findAberrationCorrection 
  (
                        /* Input args: */
    double mjd,         /* MJD at which correction is to be made */
    double mjdref,      /* Reference MJD for mission */
    GENORBFILE* orb,    /* Structure with information about orbit file */
    int use_earth_vel,  /* Flag: is the earth velocity calculated and returned? */
    int use_sat_vel,    /* Flag: Is the satellite velocity calculated and returned? */
    int invert_earth_vel,  /* Is the earth velocity calculated and returned? */
    int invert_sat_vel, /* Is the satellite velocity calculated and returned? */
                        /* Output args: */
    double *v_sat_total, /* Magnitude of total satellite velocity */
    double vhat_sat_total[3]   /* Unit vector in direction of total satellite velocity */
    /* Returns 1 if orbital velocity was found in the orbit file, otherwise 0 */
  )
  {

/* Local variables */
  int i = 0;            /* Loop counter */
  int status = 0;       /* Return code from called functions */
  double met;           /* Mission Elapsed Time  */
  double vc[3];         /* Velocity components  */
  double norm = 0.0;    /* Norm of 3-vector */
  double v_sat_orbit = 0.0;     /* Magnitude of satellite orbital velocity */     
  double vhat_sat_orbit[3];     /* Direction of satellite orbital velocity (unit vec) */
  double v_earth_orbit = 0.0;   /* Magnitude of earth orbital velocity */        
  double vhat_earth_orbit [3];  /* Direction of earth orbital velocity (unit vec) */
  const double c_km_per_s = 299792.458;

/* Initialize all velocity variables to zero. */
  v_earth_orbit = 0.0;
  vhat_earth_orbit [0] = 0.0;
  vhat_earth_orbit [1] = 0.0;
  vhat_earth_orbit [2] = 0.0;
  v_sat_orbit = 0.0;
  vhat_sat_orbit [0] = 0.0;
  vhat_sat_orbit [1] = 0.0;
  vhat_sat_orbit [2] = 0.0;
  *v_sat_total = 0.0;
  vhat_sat_total [0] = 0.0;
  vhat_sat_total [1] = 0.0;
  vhat_sat_total [2] = 0.0;

  if (use_earth_vel) 
    {
      /* Calculate the Earth velocity vector. */
      earthVelcAtMJD(mjd, vc);

      /* Calculate the Earth speed. */
      norm = sqrt(vc[0] * vc[0] + vc[1] * vc[1] + vc[2] * vc[2]);


      /* Normalize the velocity vector. */
      for (i = 0; i < 3; ++i)
        vhat_earth_orbit[i] = vc[i] / norm;

      v_earth_orbit  = norm; 
   
    }

  if (invert_earth_vel)
    negate3Vector(vhat_earth_orbit);

  if (use_sat_vel)
    {
      /* Derive the mission elapsed time (MET) from MJD and MJDREF. */
      met = 86400.0 * (mjd - mjdref);

      /* Find the satellite velocity vector from the orbit file. */
      status = findSplitOrbVelocityInGenOrbFile(orb, &v_sat_orbit, vhat_sat_orbit, met);

      v_sat_orbit /= c_km_per_s;

    }

  if (invert_sat_vel)
    negate3Vector(vhat_sat_orbit);

  /* Update the total satellite velocity. */
  calcTotalSatVelocity(v_sat_total, vhat_sat_total, v_earth_orbit, vhat_earth_orbit, 
    v_sat_orbit, vhat_sat_orbit);

  /* Important final step:  Need to negate the total velocity vector 
   * since we want to pass the correction. */
  negate3Vector(vhat_sat_total);

  return status;
}

/* Revision Log
 $Log: aber.c,v $
 Revision 1.1  2015/05/14 22:17:15  rshill
 Cleaned up and converted to plain C.

*/

/* 
 Old revision log from previous cvs location:

 Revision 1.6  2015/03/18 23:09:22  rshill
 Assume input orbital velocity units are km/s.

 Revision 1.5  2015/02/11 23:03:28  rshill
 Improved the internal documentation.

 Revision 1.4  2014/09/05 01:09:07  rshill
 Inserted canonical doxygen structure.


*/


