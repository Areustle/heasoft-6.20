/// \file hxisgdsfflib.h
/// \brief functions to act on the CALDB ASIC_ID and READOUT_ID remapping files
///    for HXI and SGD
/// \author Robert S. Hill
/// \date $Date: 2014/06/05 22:49:25 $

/// \addtogroup tool_hxisgdsff
/// \section hxisgdsff_hxisgdsfflib HXI/SGD ASIC and READOUT Remapping - hxisgdsfflib
///
/// Looks up ASIC_ID_RMAP for each ASIC_ID, and READOUT_ID_RMAP for each
/// valid combination of ASIC_ID and READOUT_ID.  The ASIC_ID_RMAP numbers
/// form a consecutive sequence for a whole HXI or SGD camera, and similarly for
/// READOUT_ID_RMAP.  The purpose is to make these simplified values available
/// for use as keys in retrieving information from FITS tables.
///

#ifndef HXISGDSFF_HXISGDSFFLIB_H
#define HXISGDSFF_HXISGDSFFLIB_H

#include "ahgen/ahversion.h"
AHVERSION(HXISGDSFF_HXISGDSFFLIB,"$Id: hxisgdsfflib.h,v 1.4 2014/06/05 22:49:25 rshill Exp $")

#include "ahfits/ahfits.h"

#include <string>
#include <vector>
#include <map>

/// \ingroup tool_hxisgdsff

namespace hxisgdsfflib {

/** \addtogroup tool_hxisgdsff
 *  @{
 */

/// \brief Define a data structure to hold one (ASIC_ID,
///   READOUT_ID) combination.
typedef std::pair<int, int> ElementID;

/// \brief Define a data structure to hold one (ASIC_ID_RMAP,
///   READOUT_ID_RMAP, LAYER) combination.
struct ElementIDRmap {
  int asic_id_rmap;
  int readout_id_rmap;
  int layer;
};

/// \brief Define a data structure that will contain the data from 
///   the remapping file, using std::map and std::pair, which are 
///   container types in the C++ Standard Template Library (STL).  
///   This structure provides the combination of READOUT_ID_RMAP and 
///   LAYER that corresponds to each combination of
///   ASIC_ID and READOUT_ID.
typedef std::map<ElementID, ElementIDRmap> RemapTable;

/// \brief Read remapping file and load all the data into a structure.
/// \param[in] fp_caldb ahfits descriptor of CALDB file (already open)
/// \param[out] remap_table structure to contain the remapping data
void loadRemapping(ahfits::FilePtr & fp_caldb, RemapTable & remap_table);

/// \brief Given one combination of ASIC_ID and READOUT_ID, return the correct
///   values of ASIC_ID_RMAP and READOUT_ID_RMAP.
/// \param[in] remap_table structure that contains the remapping data
/// \param[in] asic_id asic_id from telemetry
/// \param[in] readout_id readout_id from telemetry
/// \param[out] asic_id_rmap remapped READOUT number, to be stored in SFF
/// \param[out] readout_id_rmap remapped READOUT number, to be stored in SFF
/// \param[out] layer layer number, to be used in FLAG_SEU check
/// \return status code: true=success, false=failure
bool lookupRemapping(RemapTable & remap_table, 
  const int asic_id, const int readout_id, 
  int & asic_id_rmap, int & readout_id_rmap, int & layer);

/** @} */

}  // namespace hxisgdsfflib

#endif /* HXISGDSFF_REMAP_H */

/* Revision Log

 $Log: hxisgdsfflib.h,v $
 Revision 1.4  2014/06/05 22:49:25  rshill
 Response to TRF review.  LAYER supported in remap file.

 Revision 1.3  2014/05/28 22:46:05  rshill
 Recoded variable declarations for greater clarity.

 Revision 1.2  2014/01/09 20:14:48  klrutkow
 klrutkow: code review: comments

 Revision 1.1  2014/01/03 21:14:37  rshill
 Converted to standard main and to standard configuration of multiple
 source code files.

 Revision 1.11  2013/04/10 21:31:05  rshill
 Got rid of hxisgdsff:: namespace.

 Revision 1.10  2013/04/04 22:56:50  rshill
 Moved remap code out of the old ahcaldb.  Got rid of static
 global storage area.  Streamlined the CALDB struct.

 Revision 1.9  2013/04/02 17:55:50  rshill
 Get rid of instrument enums and CALDB file extensions named after instruments.

 Revision 1.8  2013/03/25 15:55:54  rshill
 Eliminated ahmission enums.

 Revision 1.7  2013/01/24 19:19:35  rshill
 Added doxygen prologue.

 Revision 1.6  2013/01/24 18:54:09  mwitthoe
 update Doxygen for ahcaldb library

 Revision 1.5  2013/01/15 21:38:21  rshill
 Made failure to find the ASIC_ID+READOUT_ID key non-fatal.

 Revision 1.4  2013/01/10 01:16:32  rshill
 Corrected doxygen markup.

 Revision 1.3  2013/01/03 19:10:47  rshill
 Deleted stale comments; made m_icur long.

 Revision 1.2  2012/11/27 23:12:01  rshill
 Fixed compilation errors

 Revision 1.1  2012/11/27 22:44:34  rshill
 Remap ASIC_ID and READOUT_ID to ASIC_ID_RMAP and READOUT_ID_RMAP

*/
