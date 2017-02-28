/** \file aber.h
 *  \brief Support routines for computing aberration
 *  \author Robert S. Hill
 *  \date $Date: 2015/05/14 22:17:15 $
 */

/** \addtogroup mod_aber
 *  \section aber_aber Compute aberration of light - aber
 * 
 *  Routines to compute correction to attitude or coordinates
 *  due to "aberration of light" caused by finite velocity of light
 *  combined with motion of observer.
 */

#ifndef ABER_ABER_H
#define ABER_ABER_H

#ifdef __cplusplus
extern "C" {
#endif

#include "coordfits2.h"

#ifdef __cplusplus
}
#endif

/** \addtogroup mod_aber
 *  @{
 */

/** \brief Negate a three-element vector 
 *  \param[inout] v Vector to be negated 
 */
void negate3Vector (double v[3]);

/** \brief Convert longitude and latitude in degrees to a Cartesion unit vector
 *  \param[in] longd Longitude in degrees
 *  \param[in] latd Latitude in degrees
 *  \param[out] unitv Unit vector
 */
void convertLongLatDegToUnitv (double longd, double latd, double unitv[3]);

/** \brief Convert a Cartesion unit vector to longitude and latitude in degrees 
 *  \param[in] unitv Unit vector
 *  \param[out] *longd Longitude in degrees
 *  \param[out] *latd Latitude in degrees
 *  \return 0 if ok, 1 if null unit vector was input
 */
int convertUnitvToLongLatDeg (double unitv[3], double* longd, double* latd);

/** \brief Compute earth velocity in units of C at a given MJD (routine
 *     taken from aberrator tool)
 *  \param[in] mjd Modified Julian Date
 *  \param[out] vel_c Earth velocity in units of C
 */
void earthVelcAtMJD (double mjd, double vel_c[3]);

/** \brief Compute vector sum of earth velocity and satellite velocity
 *  \param[out] v_total Magnitude of total velocity
 *  \param[out] vhat_total Unit vector parallel to total velocity
 *  \param[in] v_earth Magnitude of earth velocity
 *  \param[in] vhat_earth Unit vector parallel to earth velocity
 *  \param[in] v_sat Magnitude of satellite velocity
 *  \param[in] vhat_sat Unit vector parallel to satellite velocity
 *  \return 0 if OK, 1 if magnitude of total velocity is zero
 */
int calcTotalSatVelocity (double *v_total, double vhat_total[3],
  double v_earth, double vhat_earth[3], double v_sat, double vhat_sat[3]);

/** \brief Find total aberration correction, not including the effect of
 *     map projection
 *  \param[in] mjd MJD at which correction is to be made
 *  \param[in] mjdref Reference MJD for mission
 *  \param[in] *orb Structure with information about orbit file
 *  \param[in] use_earth_vel Flag to take the earth velocity into account
 *  \param[in] use_sat_vel Flag to take the satellite velocity into account
 *  \param[in] invert_earth_vel Flag to reverse the sense of the earth velocity
 *  \param[in] invert_sat_vel Flag to reverse the sense of the satellite velocity
 *  \param[out] *v_sat_total Magnitude of total satellite velocity
 *  \param[out] vhat_sat_total Unit vector in direction of total satellite velocity 
 *  \return 1 if orbital velocity was found in the orbit file, otherwise 0
 */
int findAberrationCorrection ( double mjd, double mjdref,
    GENORBFILE* orb, int use_earth_vel, int use_sat_vel, int invert_earth_vel, 
    int invert_sat_vel, double *v_sat_total, double vhat_sat_total[3]);

/** @} */

/* ABER_ABER_H */
#endif

/* Revision Log
 $Log: aber.h,v $
 Revision 1.1  2015/05/14 22:17:15  rshill
 Cleaned up and converted to plain C.

*/

/*
 Old revision log from previous cvs location:

 Revision 1.5  2015/02/11 23:03:28  rshill
 Improved the internal documentation.

 Revision 1.4  2014/09/05 01:10:01  rshill
 Corrected a CVS keyword.


*/
