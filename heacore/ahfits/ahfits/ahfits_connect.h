/// \file ahfits_connect.h
/// \brief ahfits: FITS file column connections
/// \author James Peachey
/// \date $Date: 2014/05/13 14:52:36 $

/// \addtogroup mod_ahfits
/// \section ahfits_connect Connections Library - ahfits_connect
/// 
/// Set up a connection between a single FITS column and a local variable.  This
/// is a thin class that merely sets up an ahfits Buffer object which will do 
/// the actual reading and writing of FITS data.


#ifndef AHFITS_AHFITS_CONNECT_H
#define AHFITS_AHFITS_CONNECT_H

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS_CONNECT,"$Id: ahfits_connect.h,v 1.25 2014/05/13 14:52:36 mwitthoe Exp $")

#include "ahfits/ahfits_base.h"
#include "ahfits/ahfits_buffer.h"

/// \ingroup mod_ahfits
namespace ahfits {


/// \brief connections between FITS columns and local variables (internal)
namespace connect {

/** \addtogroup mod_ahfits
 *  @{
 */

/// \brief Structure defining a single connection between a local variable
///  and a FITS column
class Connection {

  public:

  /// \brief standard constructor
  /// \param[in] ahffp ahfits file pointer
  /// \param[in] colname name of column to buffer
  /// \param[in] rwmode read/write mode
  /// \param[in] numrows number of rows to buffer; -1: let cfitsio decide 
  ///  optimum value; 0: no buffering (read one row at a time)
  /// \param[in] typecode type of data to output
  /// \param[in] overlap number of rows to keep before current row when filling buffer
  /// \param[in] data pointer to local variable to hold data
  /// \param[in] data_count pointer to variable with number of elements in row
  /// \param[in] dnull_flags pointer to array with NULL flags
  Connection(ahfits::FilePtr ahffp, const std::string& colname, 
             RWModeEnum rwmode, long numrows, int typecode, int overlap,
             void* data, ahfits::IndexType* data_count=0, char* dnull_flags=0);

  /// \brief destructor
  ~Connection();

  /// \brief return true if connection is read-only
  bool readOnly(void);

  /// \brief return true if connection is write-only
  bool writeOnly(void);

  /// \brief return true if connection is read-write
  bool readWrite(void);

  /// \brief populate local variable with data from current row in FITS file
  void readCol(void);

  /// \brief write local data from local variable to FITS file
  void writeCol(void);

  /// \brief force the buffer to flush(); then empty buffer
  void flushAndClearBuffer(void);

  /// \brief return size of buffer
  int getBufferSize(void);

  private:

  // general info about connection
  ahfits::FilePtr m_ahffp;   ///< pointer to ahfits pointer
  std::string m_colname;     ///< name of column
  RWModeEnum m_rwmode;       ///< read/write mode.
  int m_typecode;            ///< data type of local variable
  int m_element_size;        ///< size of each element in bytes

  // local variable
  void * m_data;                     ///< Pointer to the caller's defined local variable to hold data read/written to/from FITS files. May not be 0.
  ahfits::IndexType * m_data_count;  ///< MAY BE 0 for fixed size columns. Pointer to the caller's defined local variable.
  char * m_dnull_flags;              ///< Null data values are flagged with true in this array.

  // internal buffering of FITS data
  ahfits::Buffer* m_buffer;  ///< data buffer

};

/** @} */

} // namespace connect

} // namespace ahfits

#endif   /* AHFITS_AHFITS_CONNECT_H */

/* Revision Log
   $Log: ahfits_connect.h,v $
   Revision 1.25  2014/05/13 14:52:36  mwitthoe
   ahfits: add documentation and code tweaks based on the ahfits code review; see issue 376

   Revision 1.24  2014/03/31 18:07:49  mwitthoe
   ahfits: store temporary table row size in ahfits FilePtr instead of buffer instance; see issue 369

   Revision 1.23  2014/01/14 19:02:39  mwitthoe
   ahfits: the numRows() function in ahfits_row was returning the value of m_numrow in the ahfits pointer; however when adding rows to the end of a FITS file, the buffer will add a block of rows at once making the m_numrow value unreliable; the numRows() function has been updated to get the correct row count from the Buffer instance (if present)

   Revision 1.22  2013/10/30 18:01:21  mwitthoe
   ahfits: update documentation according to Kristin's recommendations in update 3 of issue #289

   Revision 1.21  2013/10/16 01:40:50  mwitthoe
   ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

   Revision 1.20  2013/10/04 15:39:09  mwitthoe
   ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

   Revision 1.19  2013/07/18 14:21:04  mwitthoe
   ahfits: add optional argument to Router constructor where the overlap can be provided, the default is to have no overlap

   Revision 1.18  2013/07/17 17:26:56  mwitthoe
   ahfits: add support for TDISP; refactoring related to code review (issue 266)

   Revision 1.17  2013/07/12 22:16:26  mwitthoe
   ahfits buffering: fix bug where buffer was not properly flushed and reset when moving to a new extension but keeping router connections active

   Revision 1.16  2013/05/14 14:17:32  mwitthoe
   ahfits buffering: add row overlapping when reading next buffer, add support for buffer size options; various debugging

   Revision 1.15  2013/05/13 19:45:51  mwitthoe
   add buffering to ahfits (no TBIT support yet)

   Revision 1.14  2013/03/18 23:44:38  rshill
   Column-specific readable/writeable flag added.

   Revision 1.13  2013/03/11 16:40:24  peachey
   Remove ConnectionList struct and various functions that use it, which
   are no longer needed. Take unit test for these out of the build.

   Revision 1.12  2013/03/09 03:59:51  peachey
   Add a copy constructor and destructor to manage the string buffer.
   Also store the width of the connected column.

   Revision 1.11  2013/02/28 21:59:19  rshill
   Added TNULL and NaN capability for flagging undefined values.

   Revision 1.10  2013/01/24 15:56:28  mwitthoe
   update afits Doxygen

   Revision 1.9  2013/01/18 18:26:18  mwitthoe
   revert ahfits connections to string variables to the stable, but-leaky version; this problem will be sorted out in build 3

   Revision 1.8  2013/01/15 13:52:17  mwitthoe
   add [] to delete of char* in ahfits_connect.h

   Revision 1.7  2013/01/15 13:48:22  mwitthoe
   ahfits: after inserting a column in a FITS file, the router indices for that file/extension need to be adjusted; there was an issue with the string buffers in the router connection list; changed how the string buffers are stored in order to avoid the issue

   Revision 1.6  2013/01/11 19:40:52  mwitthoe
   move insert column functions from colinfo to file; add function to update column indices in routers after insertion of a column

   Revision 1.5  2012/11/26 21:22:05  mwitthoe
   add brief descriptions to namespaces in gen

   Revision 1.4  2012/10/18 14:38:19  mwitthoe
   fixed a bug in ahfits where column information was not being updated when moving to a new extension when routers were set up outside of the HDU loop; added a new function, reloadColInfo(), to ahfits_connect to perform this update; added test to ut_ahfits_row.cxx to check this scenario

   Revision 1.3  2012/10/17 02:38:22  mwitthoe
   add Doxygen comments to each ahfits header which will be output to the ahfits module

   Revision 1.2  2012/10/10 20:30:56  mwitthoe
   ahfits: complete overloaded connection functions; add test code

   Revision 1.1  2012/10/01 17:21:56  mwitthoe
   new version of ahfits with connections stored outside of FITS file object


*/
