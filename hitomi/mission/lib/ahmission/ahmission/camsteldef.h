/// \file camsteldef.h
/// \brief Defines a CAMSTelDef structure.
/// \author Timothy Reichard
/// \date $Date: 2015/03/25 21:52:53 $
///
/// This header file defines a CAMSTelDef structure for use in the 
/// cams2det task.

#ifndef AHMISSION_CAMSTELDEF_H
#define AHMISSION_CAMSTELDEF_H(arg) const char arg##AHMISSION_CAMSTELDEF_rcsId_svnId[] = "$Version$";

#include "ahgen/ahversion.h"
AHVERSION(AHMISSION_CAMSTELDEF,"$Id: camsteldef.h,v 1.2 2015/03/25 21:52:53 asargent Exp $")

#include "ahlog/ahlog.h"
#include <string>

/// \ingroup mod_ahmission
namespace ahmission {

/// \brief Read non-standard HXI and CAMS TelDef keywords
namespace teldef {

/** \addtogroup mod_ahmission
 * @{
 */

/// \brief Geometric properties of a CAMS unit.
///
/// CAMSTelDef is a structure holding geometric properties of a CAMS
/// unit.  The properties are read from TelDef file keywords.
struct CAMSTelDef
{
  // Methods

  /// \brief Initialize the members with default values.
  /// \return New CAMSTelDef structure
  ///
  /// The default constructor assigned default values to the members.
  CAMSTelDef();
 
  /// \brief Read keyword values from a CAMS TelDef file to initialize the structure.
  /// \return New CAMSTelDef structure
  /// \param[in] cams_teldef_file CAMS TelDef filename
  /// \param[in] cams_unit_number Name of CAMS unit (CAMS1 or CAMS2);
  ///
  /// This constructor opens an HXI TelDef file, checks that the file is intended for the correct Astro-H 
  /// HXI unit, and reads the keyword values from the primary header and into the structure attributes.
  CAMSTelDef(const std::string& cams_teldef_file, const int cams_unit_number);

  /// \brief Overload the << stream operator and inserts the contents of the structure to the stream.
  /// \return Stream that includes structure contents
  /// \param[in] str stream defined in ahlog library
  /// \param[in] c CAMSTelDef structure
  friend st_stream::OStream& operator<< (st_stream::OStream& str, const CAMSTelDef& c);

  // File and unit values

  std::string m_filename;    ///< TelDef filename
  std::string m_unit_name;   ///< CAMS unit name 
  int m_unit_number;         ///< CAMS unit number

  // TelDef keywords

  double m_x_location;       ///< X location (mm) in spacecraft coords.
  double m_y_location;       ///< Y location (mm) in spacecraft coords.
  double m_z_location;       ///< Z location (mm) in spacecraft coords.
  double m_rot_angle;        ///< Rotation angle (deg) wrt spacecraft coord. axes
  long m_x_flip;             ///< X-axis flipped (-1) or not flipped (+1) wrt. spacecraft X-axis
  long m_y_flip;             ///< Y-axis flipped (-1) or not flipped (+1) wrt. spacecraft Y-axis
  double m_scale;            ///< Pixel measurement unit (mm) for position data
  double m_x_offset;         ///< Measured X offset of unit
  double m_y_offset;         ///< Measured Y offset of unit
};

/** @} */

} // end namespace teldef

} // end namespace ahmission

#endif

/* Revision log
  $Log: camsteldef.h,v $
  Revision 1.2  2015/03/25 21:52:53  asargent
  Added new keyword access x location

  Revision 1.1  2014/01/07 19:37:18  treichar
  Moved teldef and CAMS unit data file I/O data structures and routines from cams2det task to ahmission library.

  Revision 1.4  2013/07/25 19:18:39  treichar
  Removed cleanup parameter, which is no longer needed due to switch from using ftcreate to ahfits for writing the output file.  Updated doxygen mark-up and comments
  throughout code.

  Revision 1.3  2012/09/17 15:04:47  treichar
  Updated to for compliance with ahversion.

  Revision 1.2  2012/09/12 20:46:24  treichar
  More development.  Added doxygen comments to source code.

  Revision 1.1  2012/09/07 20:51:03  treichar
  Initial version of new tool, cams2det, which converts bit-formatted extended optical bench positions detected by the Astro-H CAMS units into HXI RAW pixel offsets.

*/
