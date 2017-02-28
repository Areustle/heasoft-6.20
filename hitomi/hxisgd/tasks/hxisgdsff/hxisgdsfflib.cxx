/// \file hxisgdsfflib.cxx
/// \brief functions to act on the CALDB ASIC_ID and READOUT_ID remapping files
///    for HXI and SGD
/// \author Robert S. Hill
/// \date $Date: 2014/06/05 22:49:25 $

#define AHLABEL tool_hxisgdsff_hxisgdsfflib
#define AHCVSID "$Id: hxisgdsfflib.cxx,v 1.4 2014/06/05 22:49:25 rshill Exp $"

#include "hxisgdsfflib.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

#include <iostream>
#include <fstream>
#include <string.h>
#include <sstream>
#include <cmath>

namespace hxisgdsfflib {

/// \callgraph
void loadRemapping(ahfits::FilePtr & fp_caldb, RemapTable & remap_table) {

  // This function loads the data from the remapping CALDB file
  // into a data structure.

  // Declarations
  //
  ahfits::Router * router;         // Router for table columns 

  // Local variables for CALDB table columns:

  ElementID id = std::make_pair(-1,-1);    //  Input values:    (ASIC_ID, READOUT_ID)
  ElementIDRmap id_rmap;                   //  Remapped values: (ASIC_ID_RMAP, READOUT_ID_RMAP, LAYER)

  // Clear the structure that is to hold the remapping data.
  //
  remap_table.clear();
  
  // Connect the CALDB table columns to local variables.
  //
  router = new ahfits::Router(fp_caldb);
  router->connectScalar(ahfits::e_READONLY, "ASIC_ID", id.first);
  router->connectScalar(ahfits::e_READONLY, "READOUT_ID", id.second);
  router->connectScalar(ahfits::e_READONLY, "ASIC_ID_RMAP", id_rmap.asic_id_rmap);
  router->connectScalar(ahfits::e_READONLY, "READOUT_ID_RMAP", id_rmap.readout_id_rmap);
  router->connectScalar(ahfits::e_READONLY, "LAYER", id_rmap.layer);

  // Read in the CALDB table, row by row.
  //
  for (ahfits::firstRow(fp_caldb); ahfits::readOK(fp_caldb); ahfits::nextRow(fp_caldb)) {

    // Declare Standard Template Library (STL) structures local to this loop.
    //

    // Read a row of the table.  This automatically fills id and id_rmap.
    //
    ahfits::readRow(fp_caldb);

    // Store the row into the remap_table structure.
    //
    remap_table.insert( std::make_pair(id, id_rmap) );

  }

  // Free the Router.
  //
  delete router;
}

// --------------------------------------------------------------------

/// \callgraph
bool lookupRemapping(RemapTable & remap_table, const int asic_id, const int readout_id, 
    int & asic_id_rmap, int & readout_id_rmap, int & layer) {

  // This function looks up the READOUT_ID_RMAP and LAYER corresponding
  // to input values of ASIC_ID and READOUT_ID, using functions from the
  // Standard Template Library (STL).

  //  Declarations
  //
  bool retflag = false;                    // Flag to be returned by this function
  RemapTable::iterator remap_row;          // Holds result of the table lookup
  ElementID id = std::make_pair(asic_id, readout_id);   // Input key for lookup

  // Make sure that remap_table contains something, using the STL empty function.
  //
  if (remap_table.empty()) AH_THROW_RUNTIME("remap data not loaded");

  //AH_DEBUG << "# table entries = " << remap_table.size() << std::endl;

  // Do the table lookup, using the STL find() function.
  //
  remap_row = remap_table.find(id);

  // Check whether the relevant table entry was found, using the STL end() function.
  //
  if (remap_row == remap_table.end()) {

    retflag = false;      //  Entry not found

    //  Store dummy results into the function arguments.
    //
    readout_id_rmap = -999;
    layer = -999;

  } else {

    retflag = true;       //  Entry found

    //  Store results into the function arguments.
    //
    asic_id_rmap = (remap_row->second).asic_id_rmap;
    readout_id_rmap = (remap_row->second).readout_id_rmap;
    layer = (remap_row->second).layer;

  }

  return retflag;
  
}

// --------------------------------------------------------------------

}  // namespace hxisgdsfflib

/* Revision Log
 $Log: hxisgdsfflib.cxx,v $
 Revision 1.4  2014/06/05 22:49:25  rshill
 Response to TRF review.  LAYER supported in remap file.

 Revision 1.3  2014/05/28 22:46:05  rshill
 Recoded variable declarations for greater clarity.

 Revision 1.2  2014/03/05 01:40:53  rshill
 Commented out AH_DEBUG statements to improve timing.

 Revision 1.1  2014/01/03 21:14:37  rshill
 Converted to standard main and to standard configuration of multiple
 source code files.

 Revision 1.19  2013/10/17 20:04:04  rshill
 Change connect to connectScalar.

 Revision 1.18  2013/09/12 15:22:25  mwitthoe
 hxisgdsff: in remap.cxx, explicitly move to first binary table to prepare for redefinition of an empty string to represent the primary extension in ahfits (see issue #270)

 Revision 1.17  2013/05/09 00:39:59  rshill
 Get will now detect an error. Previously, it automatically
 inserted table entries for erroneous keys.

 Revision 1.16  2013/04/10 21:31:05  rshill
 Got rid of hxisgdsff:: namespace.

 Revision 1.15  2013/04/04 22:56:50  rshill
 Moved remap code out of the old ahcaldb.  Got rid of static
 global storage area.  Streamlined the CALDB struct.

 Revision 1.14  2013/04/02 17:55:50  rshill
 Get rid of instrument enums and CALDB file extensions named after instruments.

 Revision 1.13  2013/03/25 15:56:28  rshill
 Eliminated ahmission enums.

 Revision 1.11  2013/03/20 17:20:57  rshill
 Get rid of some commenting-out that was for debugging.

 Revision 1.10  2013/03/18 23:41:35  rshill
 Uses new version of ahfits; some debugging code included.

 Revision 1.9  2013/01/16 19:59:54  rshill
 CALDB column names consistent with EVENTS files.

 Revision 1.8  2013/01/15 21:38:21  rshill
 Made failure to find the ASIC_ID+READOUT_ID key non-fatal.

 Revision 1.7  2013/01/03 19:11:24  rshill
 Accommodate CALDB file provided by Japan.

 Revision 1.6  2012/12/11 21:44:41  rshill
 Scrubbed for coding standards and current ahfits usage.

 Revision 1.5  2012/12/10 18:15:01  mwitthoe
 MCW add comment for RH in remap.cxx about removing obsolete ahfits::open function

 Revision 1.4  2012/11/30 20:58:49  rshill
 Added some error and debugging outputs.

 Revision 1.3  2012/11/29 20:41:27  rshill
 Added an AH_DEBUG line.

 Revision 1.2  2012/11/27 23:11:02  rshill
 Fixed compilation errors.

 Revision 1.1  2012/11/27 22:42:41  rshill
 Remap ASIC_ID and READOUT_ID to ASIC_ID_RMAP and READOUT_ID_RMAP.


*/
