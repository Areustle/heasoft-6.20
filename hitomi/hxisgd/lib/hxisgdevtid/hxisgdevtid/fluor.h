/// \file fluor.h
/// \brief functions to act on the CALDB fluorescence file for HXI/SGD
/// \author Mike Witthoeft
/// \date $Date: 2015/07/15 19:14:44 $

/// \addtogroup mod_hxisgdevtid
/// \section hxisgdevtid_fluor Fluorescence data for HXI/SGD - fluor
///
/// Description...
///

#ifndef HXISGDEVTID_FLUOR_H
#define HXISGDEVTID_FLUOR_H

#include "ahgen/ahversion.h"
AHVERSION(HXISGDEVTID_FLUOR,"$Id: fluor.h,v 1.6 2015/07/15 19:14:44 klrutkow Exp $")

#include <string>

/// \ingroup mod_hxisgdevtid

namespace hxisgdevtid {

namespace fluor {

/** \addtogroup mod_hxisgdevtid
 *  @{
 */

struct DataRow {

  DataRow(): m_energy(0.), m_linemin(0.), m_linemax(0.) {}

  std::string m_material;         ///< material element: Si, Cd, or Te
  std::string m_fluorescence;     ///< fluorescence feature: K-[alpha|beta] [Si|Cd|Te]
  double m_energy;                ///< energy of feature
  double m_linemin;               ///< lower end of feature detection range
  double m_linemax;               ///< upper end of feature detection range
};

struct DataType {

  DataType(): m_resol(0.), m_data(0) {}

  ~DataType() {
    if (m_data != 0) {
      delete [] m_data;
      m_data=0;
    }
  }

  double m_resol;                 ///< HXI feature resolution (from keyword)
  double m_sgd_errsi_a_sqr;       ///< SGD Si resolution: A^2 coefficient (keyword)
  double m_sgd_errsi_b_sqr;       ///< SGD Si resolution: B^2 coefficient (keyword)
  double m_sgd_errsi_c_sqr;       ///< SGD Si resolution: C^2 coefficient (keyword)
  double m_sgd_errcdbtm_a_sqr;    ///< SGD CdTe bottom resolution: A^2 coefficient (keyword)
  double m_sgd_errcdbtm_b_sqr;    ///< SGD CdTe bottom resolution: B^2 coefficient (keyword)
  double m_sgd_errcdbtm_c_sqr;    ///< SGD CdTe bottom resolution: C^2 coefficient (keyword)
  double m_sgd_errcdsid_a_sqr;    ///< SGD CdTe top resolution: A^2 coefficient (keyword)
  double m_sgd_errcdsid_b_sqr;    ///< SGD CdTe top resolution: B^2 coefficient (keyword)
  double m_sgd_errcdsid_c_sqr;    ///< SGD CdTe top resolution: C^2 coefficient (keyword)
  int m_size;                     ///< size of data array
  DataRow* m_data;                ///< array to store all row data
};




/// \brief read CALDB file
/// \param[in] filename name of fluorescence file
/// \param[in] instrume name of detector to expect: HXI or SGD
/// \param[out] fluor_table structure containing CALDB data
void load(const std::string& filename, const std::string& instrume, 
          DataType & fluor_table);

/// \brief return true if given energy is in detection range of a feature for 
///  the given material.
/// \param[in] material search features for this element: Si, Cd, or Te
/// \param[in] energy energy to search for
/// \param[in] fluor_table structure containing CALDB data
/// \param[out] fluorescence name of matching feature or empty string if no match
bool hasMatch(std::string material, double energy, const DataType& fluor_table, 
              std::string& fluorescence);

/** @} */

}  // namespace fluor

}  // namespace hxisgdevtid

#endif /* HXISGDEVTID_FLUOR_H */

/* Revision Log

 $Log: fluor.h,v $
 Revision 1.6  2015/07/15 19:14:44  klrutkow
 updated doxygen with correct file type

 Revision 1.5  2015/03/03 18:24:45  mwitthoe
 hxisgdevtid library: add common structures/functions for reconstruction and expand tools; add new keywords to fluorescence CALDB file for SGD energy test

 Revision 1.4  2014/05/09 19:51:36  mwitthoe
 hxisgdevtidlib: update common CALDB access routines to use new versions of CALDB files

 Revision 1.3  2014/01/31 14:59:53  rshill
 Added SGD resolutions for Si and CdTe.

 Revision 1.2  2014/01/16 18:15:54  mwitthoe
 hxisgdevtid library: change outer namespace from hxisgd to hxisgdevtid

 Revision 1.1  2014/01/16 18:00:01  mwitthoe
 add hxisgdevtid library with CALDB access libraries used by the hxievtid and sgdevtid tools


*/
