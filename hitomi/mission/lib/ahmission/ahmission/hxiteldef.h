/// \file hxiteldef.h
/// \brief Defines an HXITelDef structure to hold non-standard HXI TelDef keyword values
/// \author Timothy Reichard
/// \date $Date: 2015/03/25 21:52:53 $
///
/// Declares an HXITelDef structure to hold HXI
/// TelDef keywords that are not part of the TelDef v0.2 standard.


#ifndef AHMISSION_HXITELDEF_H
#define AHMISSION_HXITELDEF_H(arg) const char arg##AHMISSION_HXITELDEF_rcsId_svnId[] = "$Version$";

#include "ahgen/ahversion.h"
AHVERSION(TASKS_CAMS2DET_HXITELDEF,"$Id: hxiteldef.h,v 1.2 2015/03/25 21:52:53 asargent Exp $")

#include "ahlog/ahlog.h"
#include <string>

/// \ingroup mod_ahmission
namespace ahmission {

/// \brief Read non-standard HXI and CAMS TelDef keywords
namespace teldef {

/** \addtogroup mod_ahmission
 *  @{
 */

/// \brief Geometric properties of an HXI unit.
///
/// HXITelDef is a structure holding the geometric properties of an HXI
/// unit.  These properties are *non-standard* keywords in an HXI TelDef
/// file.  (The data structure and routines for working with the *standard*
/// keywords and tables of an HXI TelDef file are part of the general
/// TelDef2 part of the coordfits library in the attitude module.)
struct HXITelDef
{
  /// \brief Initialize the members with default values.
  /// \return New HXITelDef structure
  ///
  /// The default constructor assigns default values to the members.
  HXITelDef();

  /// \brief Read keyword values from an HXI TelDef file to initialize the structure.
  /// \return New HXITelDef structure
  /// \param[in] hxi_teldef_file HXI TelDef filename
  /// \param[in] hxi_unit_number Name of HXI unit (HXI1 or HXI2);
  ///
  /// This constructor opens an HXI TelDef file, checks that the file is intended for the correct Astro-H 
  /// HXI unit, and reads the keyword values from the primary header and into the structure attributes.
  HXITelDef(std::string hxi_teldef_file, int hxi_unit_number);

  /// \brief Overload the << stream operator and inserts the contents of the structure to the stream.
  /// \return Stream that includes structure contents
  /// \param[in] str Stream defined in ahlog library
  /// \param[in] c HXITelDef structure
  friend st_stream::OStream& operator<< (st_stream::OStream& str, const HXITelDef& c);

  std::string m_filename;   ///< TelDef filename
  std::string m_unit_name;  ///< HXI unit name
  int m_unit_number;        ///< HXI unit number

  double m_x_location;      ///< X location (mm) in spacecraft coords.
  double m_y_location;      ///< Y location (mm) in spacecraft coords.
  double m_z_location;      ///< Z location (mm) in spacecraft coords.
  double m_rot_angle;       ///< Rotation angle (deg) wrt spacecraft coord. axes
  double m_x_phys_size;     ///< Physical X size of sensor (mm)
  double m_y_phys_size;     ///< Physical Y size of sensor (mm)
};

/** @} */

} // End namespace teldef
} // End namespace ahmission

#endif // Define AHMISSION_HXITELDEF_H 

/* Revision Log
 $Log: hxiteldef.h,v $
 Revision 1.2  2015/03/25 21:52:53  asargent
 Added new keyword access x location

 Revision 1.1  2014/01/07 19:37:17  treichar
 Moved teldef and CAMS unit data file I/O data structures and routines from cams2det task to ahmission library.

 Revision 1.4  2013/07/25 19:18:39  treichar
 Removed cleanup parameter, which is no longer needed due to switch from using ftcreate to ahfits for writing the output file.  Updated doxygen mark-up and comments
 throughout code.

 Revision 1.3  2012/09/17 15:04:47  treichar
 Updated to for compliance with ahversion.

 Revision 1.2  2012/09/12 20:46:24  treichar
 More development.  Added doxygen comments to source code.

*/
