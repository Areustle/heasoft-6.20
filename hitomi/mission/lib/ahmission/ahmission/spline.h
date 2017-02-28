/// \file spline.h
/// \brief Read CALDB file containing spline coefficients.
/// \author Robert S. Hill
/// \date $Date: 2015/03/18 17:34:56 $

/// \addtogroup mod_ahmission
/// \section ahmission_spline Spline CALDB - spline
///
/// This library will read a CALDB file containing spline coefficients and 
/// use those coefficients to compute the spline for given input values.
///

#ifndef AHMISSION_SPLINE_H
#define AHMISSION_SPLINE_H

#include "ahgen/ahversion.h"
AHVERSION(AHMISSION_SPLINE,"$Id: spline.h,v 1.2 2015/03/18 17:34:56 asargent Exp $")

#include <string>
#include <map>

/// \ingroup mod_ahmission

namespace ahmission {

namespace spline {

/** \addtogroup mod_ahmission
 *  @{
 */

/// \brief struct to hold the gain calibration coefficients for one interval
typedef struct {
  double m_coeffx[4];
} SplineInterval;

/// \brief struct to hold one set of gain calibration coefficients
typedef struct {
  double m_x_domain_min;                     // lower bound of all intervals
  double m_x_domain_max;                     // upper bound of all intervals
  std::map<double, SplineInterval> m_spline; // index is upper bound of interval
} SplineSet;
  
/// \brief map to hold multiple sets of spline coefficients; the index of the
///  map is an integer label for the spline
typedef std::map<int,SplineSet> SplineSetMap;

/// \brief Load FITS file containing several sets of spline coefficients.
/// \param[in] filename name of spline FITS file
/// \param[in] extname name of extension in file with spline coefficients
/// \param[in] indexcol name of column to use a spline index
/// \param[in] instrume value of INSTRUME keyword for desired extension
///            (use empty string to skip this check)
/// \param[in] detnam value of DETNAM keyword for desired extension
///            (use empty string to skip this check)
/// \param[out] spline_table SplineSetMap structure containing spline data
void load(const std::string& filename, const std::string& extname,
          const std::string& indexcol, const std::string& instrume,
          const std::string& detnam, SplineSetMap& spline_table);

/// \brief Retrieve spline coefficients for single interval.
/// \param[in] spline_table SplineSetMap structure containing spline data
/// \param[in] index index of spline set to access
/// \param[in] x return interval containing this value
/// \param[out] at_boundary true if x exactly at a spline node
/// \param[out] coeff_set1 coefficients for interval
/// \param[out] coeff_set2 2nd set of coefficients if x at boundary
/// \return validity code 0=good, 1=x out-of-range
int get_coeff(SplineSetMap& spline_table, int index, double x, 
              bool& at_boundary, double coeff_set1[4], double coeff_set2[4]);

/// \brief Retrieve spline coefficients for single interval.
/// \param[in] spline_table SplineSetMap structure containing spline data
/// \param[in] index index of spline set to access
/// \param[in] x return interval containing this value
/// \param[out] y spline value at x
/// \return validity code 0=good, 1=x out-of-range
int evaluate(SplineSetMap& spline_table, int index, double x, double& y);


/** @} */

}  // namespace spline

}  // namespace ahmission

#endif /* AHMISSION_SPLINE_H */

/* Revision Log

 $Log: spline.h,v $
 Revision 1.2  2015/03/18 17:34:56  asargent
 Changed DETNAME -> DETNAM

 Revision 1.1  2015/03/03 17:35:10  mwitthoe
 ahmission library: add general library for cubic spline FITS files


*/
