/// \file camsio.h
/// \brief Defines a data structure and routines for a CAMS unit data file
/// \author Timothy Reichard
/// \date $Date: 2015/07/31 20:51:45 $
///
/// This header file defines a CAMSUnitDataRow structure for reading and writing a CAMS unit data file with TIME, X, Y, and QUALITY columns.

#ifndef AHMISSION_CAMSIO_H
#define AHMISSION_CAMSIO_H(arg) const char arg##AHMISSION_CAMSIO_rcsId_svnId[] = "$Version$";

#include "ahgen/ahversion.h"
AHVERSION(AHMISSION_CAMSIO,"$Id: camsio.h,v 1.3 2015/07/31 20:51:45 rshill Exp $")

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include <string>

/// \ingroup mod_ahmission
namespace ahmission {

/// \brief Read/write CAMS unit data files
namespace camsio {

/** \addtogroup mod_ahmission
 * @{
 */

/// \brief Number of bits in PROC_STATUS column
const int LEN_PROC_STATUS=32;

/// \brief Stores one row of CAMS position data for one CAMS unit.
///
/// Stores the time, x and y position, and quality flags for a single row of the CAMS unit data file.
struct UnitDataRow
{
  /// \brief Connect the structure members to the columns of a CAMS unit data FITS file.
  ///
  /// Connect each structure member to the corresponding column of a
  /// CAMS unit data FITS file in a certain read/write mode.
  /// \return Pointer to FITS i/o router  
  ahfits::Router* connectColumns(  
    ahfits::FilePtr& fp,           ///< [in, out] File pointer
    const ahfits::RWModeEnum mode  ///< [in] Read/write mode
  );
  
  double m_t; ///< Time
  int m_x_raw;  ///< X
  int m_y_raw;  ///< Y
  double m_x;    ///< X temperature corrected
  double m_y;    ///< Y temperature corrected
  int m_q;    ///< Quality flags
  char m_proc_status[LEN_PROC_STATUS];  ///< Processing status
  ahfits::IndexType m_num_proc_status;  ///< Number of proc_status bits last read
};

/** @} */

} // end namespace camsio

} // end namespace ahmission

#endif

/* Revision log
  $Log: camsio.h,v $
  Revision 1.3  2015/07/31 20:51:45  rshill
  Include temperature-corrected CAMS data columns.

  Revision 1.2  2015/07/31 18:36:28  rshill
  Add support for proc_status column.

  Revision 1.1  2014/01/07 19:37:17  treichar
  Moved teldef and CAMS unit data file I/O data structures and routines from cams2det task to ahmission library.

*/
