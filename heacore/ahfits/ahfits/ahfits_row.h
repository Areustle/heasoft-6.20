/// \file ahfits_row.h
/// \brief ahfits: Connect to column data and read/write rows
/// \author James Peachey
/// \date $Date: 2013/11/20 14:26:34 $

/// \addtogroup mod_ahfits
/// \section ahfits_row Row Access Library - ahfits_row
///
/// This library contains functions for navigating and reading/writing row data.  
/// The reading and writing routines will operate on all active routers of the
/// given ahfits file pointer.
///

#ifndef AHFITS_AHFITS_ROW_H
#define AHFITS_AHFITS_ROW_H

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS_ROW,"$Id: ahfits_row.h,v 1.12 2013/11/20 14:26:34 mwitthoe Exp $")

#include "ahfits/ahfits_base.h"

#include "fitsio.h"

/// \ingroup mod_ahfits
namespace ahfits {


/** \addtogroup mod_ahfits
 *  @{
 */

/// \brief Read the next row from the input FITS file(s), filling
///   previously connected local variables with the appropriate data.
/// \param[in] ahffp The FITS file object to be read from.
///
/// For example, if the local tstart variable were previously connected
/// to the TSTART column, this would copy the TSTART value from the
/// current row to the tstart variable.
/// Note that for vector-valued data that were connected, the associated
/// count variable will be set to the number of elements read.
void readRow(FilePtr ahffp);

/// \brief Write the next row to the output FITS file(s), taking
///   data from previously connected local variables.
/// \param[in] ahffp The FITS file object to be written to.
///
/// For example, if the local tstart variable were previously connected
/// to the TSTART column, this would copy the tstart variable to the
/// next row of the TSTART column.
/// Note that for vector-valued data that were connected, the associated
/// count variable will be used to determine the number of elements to
/// write. If the calling code attempts to write more elements than will
/// fit in the column, no data are written, and ahFitsWriteRow will
/// throw an exception.
void writeRow(FilePtr ahffp);

/// \brief return current row index
/// \param[in] ahffp The FITS file objec to be written to.
long long currentRow(FilePtr ahffp);

/// \brief Go to first row of table.
/// \param[in] ahffp The FITS file objec to be written to.
void firstRow(FilePtr ahffp);

/// \brief Go to last row of table.
/// \param[in] ahffp The FITS file object
void lastRow(FilePtr ahffp);

/// \brief return true if at last row in FITS file of active extension
/// \param[in] ahffp The FITS file object
bool atLastRow(FilePtr ahffp);

/// \brief Advance to next row of table.
/// \param[in] ahffp The FITS file object
void nextRow(FilePtr ahffp);

/// \brief Go to previous row of table.
/// \param[in] ahffp The FITS file object
void previousRow(FilePtr ahffp);

/// \brief Go to given row number
/// \param[in] ahffp The FITS file object
/// \param[in] row row number
void gotoRow(FilePtr ahffp, long long row);

/// \brief Get number of rows
/// \param[in] ahffp The FITS file object
long long numRows(FilePtr ahffp);

/// \brief Return true if there is at least one unread row in the
///   given FITS file object, and the FITS file object has no errors currently.
/// \param[in] ahffp The FITS file object.
bool readOK(FilePtr ahffp);

/** @} */


} // namespace ahfits

#endif   /* AHFITS_AHFITS_ROW_H */

/* Revision Log
   $Log: ahfits_row.h,v $
   Revision 1.12  2013/11/20 14:26:34  mwitthoe
   ahfits: make corrections to Doxygen documenation suggested by Bob; see issue #289

   Revision 1.11  2013/10/22 19:23:31  mwitthoe
   from Andy: ahfits: add function numRows() to ahfits_row; add test for new function in ut_ahfits_row.cxx

   Revision 1.10  2013/10/16 18:51:32  mwitthoe
   ahfits library: update doxygen documentation

   Revision 1.9  2013/05/13 19:45:51  mwitthoe
   add buffering to ahfits (no TBIT support yet)

   Revision 1.8  2013/04/10 19:56:18  mwitthoe
   add new function, atLastRow(), to ahfits_row which will return true if currently at last row in active extension of a FITS file

   Revision 1.7  2013/02/28 21:59:19  rshill
   Added TNULL and NaN capability for flagging undefined values.

   Revision 1.6  2013/01/02 19:05:45  mwitthoe
   add gotoRow function to ahfits

   Revision 1.5  2012/12/26 21:16:21  mwitthoe
   ahfits: add currentRow() and previousRow() functions needed by ahgti

   Revision 1.4  2012/12/26 15:56:55  mwitthoe
   ahfits: add insert and delete row functions needed for ahgti

   Revision 1.3  2012/10/17 02:38:22  mwitthoe
   add Doxygen comments to each ahfits header which will be output to the ahfits module

   Revision 1.2  2012/10/01 17:21:56  mwitthoe
   new version of ahfits with connections stored outside of FITS file object

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/
