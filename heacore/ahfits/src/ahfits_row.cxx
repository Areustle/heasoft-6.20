/// \file ahfits_row.cxx
/// \brief ahfits: Connect to column data and read/write rows
/// \author James Peachey
/// \date $Date: 2015/04/02 15:03:21 $

#define AHLABEL ahfits_ahfits_row
#define AHCVSID "$Id: ahfits_row.cxx,v 1.42 2015/04/02 15:03:21 mwitthoe Exp $"

#include "ahfits/ahfits_row.h"
#include "ahfits/ahfits_colinfo.h"
#include "ahfits/ahfits_connect.h"
#include "ahfits/ahfits_router.h"
#include "ahfits/ahfits_header.h"
#include "ahlog/ahlog.h"

#include "fitsio.h"

#include <limits>
#include <sstream>

namespace ahfits {

// -----------------------------------------------------------------------------

void readRow(FilePtr ahffp) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // If anything remains in the file, move on to next row, and fill all
  // connected local variables.
  if (readOK(ahffp)) {

    // loop over all routers (then skip if not correct FITS file)
    for (AhFitsFile::RouterIteratorType irout=ahffp->m_router.begin(); irout != ahffp->m_router.end(); ++irout) {

      // loop over connections of each router
      for (Router::ConnectionIteratorType icon=(*irout)->connectionBegin(); icon != (*irout)->connectionEnd(); ++icon) {
        icon->second->readCol();
      }
    }
  }
}

// -----------------------------------------------------------------------------
void writeRow(FilePtr ahffp) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // cannot write to read-only files
  if (ahffp->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"file opened as read-only; cannot write");

  // stamp parameters to header (only does it once)
  ahfits::stamp(ahffp);

  // if current row is greater than number of data rows, then force the value to
  // be one more than the number of data rows
  if (ahffp->m_currow > ahffp->m_numrow) ahffp->m_currow=ahffp->m_numrow+1;

  // loop over all routers (then skip if not correct FITS file)
  for (AhFitsFile::RouterIteratorType irout=ahffp->m_router.begin(); irout != ahffp->m_router.end(); ++irout) {

    // loop over connections of each router
    for (Router::ConnectionIteratorType icon=(*irout)->connectionBegin(); icon != (*irout)->connectionEnd(); ++icon) {
      icon->second->writeCol();
    }
  }

  // Grow the table if needed.
  // When adding a row to the end of the table:
  //  1. the current row index will be larger than the previous number of rows,
  //     so update the number of rows
  //  2. if the number of rows is updated, then ensure that the padded number of
  //     rows is at least as big
  if (ahffp->m_currow > ahffp->m_numrow) ahffp->m_numrow=ahffp->m_currow;
  if (ahffp->m_numrow > ahffp->m_numrow_with_padding) ahffp->m_numrow_with_padding=ahffp->m_numrow;
}

// -----------------------------------------------------------------------------

ahfits::IndexType currentRow(FilePtr ahffp) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  return ahffp->m_currow;
}

// -----------------------------------------------------------------------------

void firstRow(FilePtr ahffp) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  ahffp->m_currow = 1;
}

// -----------------------------------------------------------------------------

void lastRow(FilePtr ahffp) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  ahffp->m_currow = ahfits::numRows(ahffp);
}

// -----------------------------------------------------------------------------

bool atLastRow(FilePtr ahffp) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  if (ahfits::numRows(ahffp) == ahffp->m_currow) return true;
  return false;
}

// -----------------------------------------------------------------------------

void nextRow(FilePtr ahffp) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // Move to next row. Note advancing past end of table is possible.
  ++ahffp->m_currow;
}

// -----------------------------------------------------------------------------

void previousRow(FilePtr ahffp) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  --ahffp->m_currow;
}

// -----------------------------------------------------------------------------

void gotoRow(FilePtr ahffp, ahfits::IndexType row) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // set row, ensuring it is in-range
  ahffp->m_currow=row;
  if (ahffp->m_currow < 1) ahffp->m_currow=1;
  ahfits::IndexType numrow=ahfits::numRows(ahffp);
  if (ahffp->m_currow > numrow) ahffp->m_currow=numrow+1;
}

// -----------------------------------------------------------------------------

ahfits::IndexType numRows(FilePtr ahffp) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  return ahffp->m_numrow;
}

// -----------------------------------------------------------------------------

bool readOK(FilePtr ahffp) {
  return (0 != ahffp && 0 != ahffp->m_cfitsfp && ahffp->m_currow <= ahfits::numRows(ahffp));
}

// -----------------------------------------------------------------------------

} // namespace ahfits

/* Revision Log
   $Log: ahfits_row.cxx,v $
   Revision 1.42  2015/04/02 15:03:21  mwitthoe
   ahfits: add checks for NULL ahfits or cfitsio FITS pointers; see issue 461

   Revision 1.41  2014/05/23 21:06:03  mwitthoe
   ahfits: make changes according to James' recommendations resulting from teh ahfits code review; see issue 376, updates 11 & 12

   Revision 1.40  2014/05/13 14:52:37  mwitthoe
   ahfits: add documentation and code tweaks based on the ahfits code review; see issue 376

   Revision 1.39  2014/03/31 18:07:49  mwitthoe
   ahfits: store temporary table row size in ahfits FilePtr instead of buffer instance; see issue 369

   Revision 1.38  2014/03/26 18:59:44  mwitthoe
   ahfits: stop automatically removing extra rows in buffer read/write; fix code calculating how many rows to read into buffer; use numRows() function in ahfits_row.cxx which returns the number of data rows instead of ahffp->m_numrow which returns the current number of rows in the FITS table (the latter can be larger than the former when the buffer has appended rows to the file

   Revision 1.37  2014/01/14 19:02:39  mwitthoe
   ahfits: the numRows() function in ahfits_row was returning the value of m_numrow in the ahfits pointer; however when adding rows to the end of a FITS file, the buffer will add a block of rows at once making the m_numrow value unreliable; the numRows() function has been updated to get the correct row count from the Buffer instance (if present)

   Revision 1.36  2013/10/22 19:23:31  mwitthoe
   from Andy: ahfits: add function numRows() to ahfits_row; add test for new function in ut_ahfits_row.cxx

   Revision 1.35  2013/10/16 01:40:50  mwitthoe
   ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

   Revision 1.34  2013/07/17 20:40:58  mwitthoe
   ahfits refactoring: change setNull() to use column name instead of column number; remove getColumn* functions from colinfo (they were confusing); add functions to clear/reload information from single column; remove unused statusMessage function; mark several functions as internal

   Revision 1.33  2013/07/12 13:30:33  mwitthoe
   ahfits: write function to provide standard error message prefix; use prefix throughout ahfits library

   Revision 1.32  2013/07/10 14:50:51  mwitthoe
   ahfits: remove requirement that FITS file have at least one row for firstRow() and lastRow(); in setTNull() and setTUnit() make sure that column information is loaded before inserting keyword

   Revision 1.31  2013/05/23 19:58:46  mwitthoe
   restore buffered version for ahfits_row.cxx and ut_ahfits_row.cxx, but keep it untagged for now

   Revision 1.29  2013/05/23 19:08:49  mwitthoe
   revert ahfits_row.cxx and ut_ahfits_row.cxx to versions 1.25 and 1.15; some buffer stuff was inadvertantly tagged

   Revision 1.25  2013/04/10 19:56:19  mwitthoe
   add new function, atLastRow(), to ahfits_row which will return true if currently at last row in active extension of a FITS file

   Revision 1.24  2013/04/03 00:48:12  peachey
   Fix indentation/white space.

   Revision 1.23  2013/04/02 16:53:09  peachey
   Issue #168:
   Step 1: refactor readRow:
   * Encapsulate call to fits_read_col and all other cfitsio-specific
     aspects into an internal function readCol.
   * Change readCol so it loops through the connections, calling readCol
     for each.

   Revision 1.22  2013/03/19 20:46:21  rshill
   Column ead/write flags implemented; inner-loop debugging code deleted.

   Revision 1.21  2013/03/18 23:48:20  rshill
   Column-specific readable/writeable flag added; some debugging code included.

   Revision 1.20  2013/03/09 04:33:12  peachey
   Use local container of routers and their connectors
   instead of using ahfits_connect facilities.

   Revision 1.19  2013/03/05 23:01:06  rshill
   Corrected handling of -Ttype constants.

   Revision 1.18  2013/03/01 23:04:21  rshill
   Small changes extrapolated from code review by JP, RSH.

   Revision 1.17  2013/03/01 21:21:38  peachey
   A couple small clean-up items during code walkthrough (JP and RSH).

   Revision 1.16  2013/02/28 21:59:20  rshill
   Added TNULL and NaN capability for flagging undefined values.

   Revision 1.15  2013/01/18 18:26:19  mwitthoe
   revert ahfits connections to string variables to the stable, but-leaky version; this problem will be sorted out in build 3

   Revision 1.14  2013/01/15 13:48:24  mwitthoe
   ahfits: after inserting a column in a FITS file, the router indices for that file/extension need to be adjusted; there was an issue with the string buffers in the router connection list; changed how the string buffers are stored in order to avoid the issue

   Revision 1.13  2013/01/02 19:05:45  mwitthoe
   add gotoRow function to ahfits

   Revision 1.12  2012/12/26 21:16:22  mwitthoe
   ahfits: add currentRow() and previousRow() functions needed by ahgti

   Revision 1.11  2012/12/26 15:56:56  mwitthoe
   ahfits: add insert and delete row functions needed for ahgti

   Revision 1.10  2012/12/07 23:00:12  mwitthoe
   make changes to ahfits recommended by Dec 7 code review (see http://zuul:8080/redmine/projects/astroh/wiki/2012-12-07_Ahfits_Code_Review)

   Revision 1.9  2012/11/07 20:10:17  mwitthoe
   ahfits: add function, num2Name, in ahfits_colinfo which returns the column name given the column index; this function is used in ahfits::writeRow() to provide a more complete error message

   Revision 1.8  2012/11/05 01:35:50  mwitthoe
   add m_readonly member to AhFitsFile struct to mark a FITS file as read-only; all writing routines (row and headers) now check the readonly status before writing, but there is no way currently to set the read-only false to true (except as the backup readonly fits_open_file call in ahfits::open

   Revision 1.7  2012/11/04 23:25:20  mwitthoe
   ahfits: add cfitsio error messages based on error status; made clone/create functions that return a fits pointer to the newly created FITS file

   Revision 1.6  2012/10/24 15:00:07  mwitthoe
   ahfits: move stamp() from ahfits_file to ahfits_header; new function addHDU in ahfits_file which will make a new (empty) extension based on the header of an existing extension from the same or different file

   Revision 1.5  2012/10/22 17:20:53  mwitthoe
   fix in ahfits to allow reading/writing of variable-length columns

   Revision 1.4  2012/10/11 17:56:56  mwitthoe
   include parameter stamping in ahfits

   Revision 1.3  2012/10/10 20:30:56  mwitthoe
   ahfits: complete overloaded connection functions; add test code

   Revision 1.2  2012/10/01 17:21:57  mwitthoe
   new version of ahfits with connections stored outside of FITS file object

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/
