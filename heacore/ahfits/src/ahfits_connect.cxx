/// \file ahfits_connect.cxx
/// \brief ahfits: FITS file column connections; only intended to be used
///  internally
/// \author James Peachey
/// \date $Date: 2014/03/31 18:07:49 $

#define AHLABEL ahfits_ahfits_connect
#define AHCVSID "$Id: ahfits_connect.cxx,v 1.27 2014/03/31 18:07:49 mwitthoe Exp $"

#include "ahfits/ahfits_colinfo.h"
#include "ahfits/ahfits_connect.h"
#include "ahlog/ahlog.h"

#include <cstring>
#include <cmath>
#include <sstream>

namespace ahfits {

namespace connect {

Connection::Connection(ahfits::FilePtr ahffp, const std::string& colname, 
            RWModeEnum rwmode, long numrows, int typecode, int overlap,
            void* data, ahfits::IndexType* data_count, char* dnull_flags) :
  m_ahffp(ahffp), m_colname(colname), m_rwmode(rwmode), m_typecode(typecode), 
  m_data(data), m_data_count(data_count), m_dnull_flags(dnull_flags),
  m_buffer(0) {

  // allocate buffer
  bool keep_null=false;
  if (0 != dnull_flags) keep_null=true;
  m_buffer=new ahfits::Buffer(m_ahffp,m_colname,m_rwmode,m_typecode,keep_null,
                              numrows,overlap);
}

Connection::~Connection() {
  if (0 != m_buffer) {
    delete m_buffer;
    m_buffer=0;
  }
}

bool Connection::readOnly(void) {
  if (m_rwmode == ahfits::e_READONLY) return true;
  return false;
}

bool Connection::writeOnly(void) {
  if (m_rwmode == ahfits::e_WRITEONLY) return true;
  return false;
}

bool Connection::readWrite(void) {
  if (m_rwmode == ahfits::e_READWRITE) return true;
  return false;
}

void Connection::readCol(void) {

  if (!writeOnly()) {
    m_buffer->read(m_data,m_data_count,m_dnull_flags);
  }
}

void Connection::writeCol(void) {
  if (!readOnly()) m_buffer->write(m_data,m_data_count,m_dnull_flags);
}

void Connection::flushAndClearBuffer(void) {
  m_buffer->flush();
  m_buffer->clear();
}

int Connection::getBufferSize(void) {
  return m_buffer->getBufferSize();
}

} // namespace connect

} // namespace ahfits

/* Revision Log
   $Log: ahfits_connect.cxx,v $
   Revision 1.27  2014/03/31 18:07:49  mwitthoe
   ahfits: store temporary table row size in ahfits FilePtr instead of buffer instance; see issue 369

   Revision 1.26  2014/01/14 19:02:39  mwitthoe
   ahfits: the numRows() function in ahfits_row was returning the value of m_numrow in the ahfits pointer; however when adding rows to the end of a FITS file, the buffer will add a block of rows at once making the m_numrow value unreliable; the numRows() function has been updated to get the correct row count from the Buffer instance (if present)

   Revision 1.25  2013/10/16 01:40:50  mwitthoe
   ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

   Revision 1.24  2013/10/10 17:00:30  peachey
   Clean out some unneeded code and remove the test for vectors of strings.
   The vector of strings case is now handled where connection is made.

   Revision 1.23  2013/10/04 15:39:09  mwitthoe
   ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

   Revision 1.22  2013/07/18 14:21:04  mwitthoe
   ahfits: add optional argument to Router constructor where the overlap can be provided, the default is to have no overlap

   Revision 1.21  2013/07/17 17:26:56  mwitthoe
   ahfits: add support for TDISP; refactoring related to code review (issue 266)

   Revision 1.20  2013/07/12 22:16:26  mwitthoe
   ahfits buffering: fix bug where buffer was not properly flushed and reset when moving to a new extension but keeping router connections active

   Revision 1.19  2013/07/12 13:30:33  mwitthoe
   ahfits: write function to provide standard error message prefix; use prefix throughout ahfits library

   Revision 1.18  2013/06/20 21:10:14  mwitthoe
   fix bug in ahfits where READ/WRITEONLY state was being ignore for variable-length FITS columns; add test cases for each in ut_ahfits_row

   Revision 1.17  2013/05/15 17:34:05  mwitthoe
   add comments/documentation to ahfits_buffer; uncomment variable-length bit column unit test

   Revision 1.16  2013/05/14 17:45:44  mwitthoe
   ahfits buffering: disable buffering to TBIT output types; fix bug causing wrong number of rows to be written for fixed-length types; fix bug where NULLs were not being written for fixed-length types

   Revision 1.15  2013/05/14 14:17:32  mwitthoe
   ahfits buffering: add row overlapping when reading next buffer, add support for buffer size options; various debugging

   Revision 1.14  2013/05/13 19:45:51  mwitthoe
   add buffering to ahfits (no TBIT support yet)

   Revision 1.13  2013/04/02 19:24:15  rshill
   Removed some AH_DEBUG output statements.

   Revision 1.12  2013/03/19 20:46:21  rshill
   Column ead/write flags implemented; inner-loop debugging code deleted.

   Revision 1.11  2013/03/18 23:47:00  rshill
   Column-specific readable/writeable flag added.

   Revision 1.10  2013/03/11 16:40:24  peachey
   Remove ConnectionList struct and various functions that use it, which
   are no longer needed. Take unit test for these out of the build.

   Revision 1.9  2013/03/09 04:02:25  peachey
   Add a copy constructor and destructor to manage the string buffer.
   Also store the width of the connected column.

   Revision 1.8  2013/01/18 18:26:19  mwitthoe
   revert ahfits connections to string variables to the stable, but-leaky version; this problem will be sorted out in build 3

   Revision 1.7  2013/01/15 13:48:23  mwitthoe
   ahfits: after inserting a column in a FITS file, the router indices for that file/extension need to be adjusted; there was an issue with the string buffers in the router connection list; changed how the string buffers are stored in order to avoid the issue

   Revision 1.6  2013/01/11 19:40:53  mwitthoe
   move insert column functions from colinfo to file; add function to update column indices in routers after insertion of a column

   Revision 1.5  2012/12/07 23:00:11  mwitthoe
   make changes to ahfits recommended by Dec 7 code review (see http://zuul:8080/redmine/projects/astroh/wiki/2012-12-07_Ahfits_Code_Review)

   Revision 1.4  2012/10/18 17:24:08  mwitthoe
   fixed but in ahfits involving iterating over multimap in getConnectedColumns in ahfits_connect

   Revision 1.3  2012/10/18 14:38:19  mwitthoe
   fixed a bug in ahfits where column information was not being updated when moving to a new extension when routers were set up outside of the HDU loop; added a new function, reloadColInfo(), to ahfits_connect to perform this update; added test to ut_ahfits_row.cxx to check this scenario

   Revision 1.2  2012/10/10 20:30:56  mwitthoe
   ahfits: complete overloaded connection functions; add test code

   Revision 1.1  2012/10/01 17:21:57  mwitthoe
   new version of ahfits with connections stored outside of FITS file object


*/
