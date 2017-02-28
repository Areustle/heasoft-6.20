/// \file ahfits_router.cxx
/// \brief ahfits: Handles connections from local scope to FITS columns
/// \author James Peachey
/// \date $Date: 2015/04/02 15:03:21 $

#define AHLABEL ahfits_ahfits_router
#define AHCVSID "$Id: ahfits_router.cxx,v 1.50 2015/04/02 15:03:21 mwitthoe Exp $"

#include "ahfits/ahfits_router.h"
#include "ahfits/ahfits_colinfo.h"
#include "ahfits/ahfits_header.h"
#include "ahlog/ahlog.h"

#include "fitsio.h"      // used for typecode constants

#include <cstdlib>
#include <sstream>
#include <utility>

namespace ahfits {

// -----------------------------------------------------------------------------

Router::Router(FilePtr ahffp) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) 
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"router cannot be initialized with NULL pointer");

  if (!isBintable(ahffp))
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"routers currently support binary tables only");

  m_ahffp=ahffp;
  m_ahffp->m_router.insert(this);
  m_overlap=0;
  m_report_log=false;
  m_report_fits=false;
}

// -----------------------------------------------------------------------------

Router::Router(FilePtr ahffp, int overlap) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) 
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"router cannot be initialized with NULL pointer");

  if (!isBintable(ahffp))
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"routers currently support binary tables only");

  m_ahffp=ahffp;
  m_ahffp->m_router.insert(this);
  m_overlap=0;
  if (m_overlap >= 0) m_overlap=overlap;
  m_report_log=false;
  m_report_fits=false;
}

// -----------------------------------------------------------------------------

Router::~Router() {
  if (0 != m_ahffp) m_ahffp->m_router.erase(this);
  disconnect();
}

// -----------------------------------------------------------------------------

int Router::index() {
  return m_ridx;
}

// -----------------------------------------------------------------------------

int Router::getOverlap(void) {
  return m_overlap;
}

// -----------------------------------------------------------------------------

void Router::connect_generic(const RWModeEnum rwmode, const std::string & colname,
                             void* data, IndexType* data_count, bool scalar, int out_type,
                             char* dnull_flags) {

  if (0 == m_ahffp) {
    AH_THROW_LOGIC("attempt to connect to a router that has been disconnected from its file");
  }


  //if a connection with NULL support is made between a floating/non-floating 
  //type column/local variable, throw logic exception 
  if (rwmode != e_READONLY) {
    if (dnull_flags != 0) {
      if (isIntegerTypeColumn(m_ahffp, colname) && 
          (out_type == TFLOAT || out_type == TDOUBLE)) { 
        AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp)+"can not connect floating point" 
                       +" variable to integer type column with NULL support: "+colname); 
      } //end if int type
      if (isFloatTypeColumn(m_ahffp, colname) && 
          (out_type == TBYTE || out_type == TINT ||
           out_type == TSHORT || out_type == TLONG ||
           out_type == TLONGLONG || out_type == TUINT || 
           out_type == TUSHORT || out_type == TULONG)) { 
        AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp)+"can not connect integer type" 
                       +" variable to floating point column with NULL support: "+colname); 
      }//end if float type 
    }//end if dnull_flag != 0
  }


  // Load column information and get column number
  loadColInfo(m_ahffp,colname);
  int colnum=name2Num(m_ahffp,colname);
  int typecode=columnType(m_ahffp,colname);
  int repeat=columnRepeat(m_ahffp,colname);



  // if column has type TBIT, then out_type must be -TBIT (the negative
  // indicates a non-scalar output type)
  if (typecode == TBIT && out_type != -TBIT)
    AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp,true,false)+" ("+colname+") must connect to TBIT columns with connectBit() function; column: "+colname);

  // if dnull_flags given and integer type, check that column has TNULL keyword
  if (0 != dnull_flags && ahfits::isIntegerTypeColumn(m_ahffp,colname)) {
    std::stringstream tnullname;
    tnullname << "TNULL" << colnum;
    try {
      ahfits::getKeyValLLong(m_ahffp,tnullname.str());
    } catch (...) {
      AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp,true,false)+" ("+colname+") cannot read/write NULL values from column with no TNULL set: "+colname);
    }
  }

  // check that expected column conforms to FITS column in terms of length
  // strings are exempt since string arrays are not yet supported
  if (scalar) {       // wanting scalar column
    if (typecode < 0 || (typecode != TSTRING && repeat > 1)) {
      AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp,true,false)+" ("+colname+") connecting to non-scalar column with scalar local variable"); }
  } else {                  // wanting non-scalar column (fixed > 1 or variable)
    if (typecode != TBIT && typecode > 0 && repeat == 1) {
      AH_ERR << "Column = " << colname << std::endl;
      AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp,true,false)+" ("+colname+") connecting to scalar column with non-scalar local variable"); 
    }
    if (out_type == TSTRING) {
      AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp,true,false)+" ("+colname+") connecting to client vector of strings not supported"); 
    }
  }

  if (m_connection.end() != m_connection.find(colname)) {
    std::stringstream msg;
    msg << "only one connection to column, " << colname << ", allowed per router";
    AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp,true,false)+msg.str());
  }

  // Check inputs; make sure data_count is defined for fixed-width columns.
  if (0 == data) AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp,true,false)+" ("+colname+") null pointer passed");
  if (0 == data_count && 0 > typecode)
    AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp,true,false)+"null data_count pointer given for a variable-size column");

  // Connect this column with the given data pointer.
  int buffer=ahfits::getBuffer();
  connect::Connection* conn=new connect::Connection(m_ahffp,colname,rwmode,buffer,out_type,m_overlap,data,data_count,dnull_flags);
  m_connection[colname]=conn;

  // report buffer size to log file
  if (!m_report_log) {
    std::string filename=ahfits::getFileAndHDUString(m_ahffp);
    AH_INFO(ahlog::LOW) << "Buffering " << filename << ", size="
                        << conn->getBufferSize() << ", overlap="
                        << m_overlap << std::endl;
    m_report_log=true;
  }
  
  // report buffer size to log file and FITS file
  if (!m_report_fits && !m_ahffp->m_readonly && !conn->readOnly()) {
    std::stringstream bufstr;
    bufstr << ahlog::get_executable_name() << ": Buffer size=" 
           << conn->getBufferSize() << ", " << "overlap=" << getOverlap();
    ahfits::writeKeyHistory(m_ahffp,bufstr.str());
    m_report_fits=true;
  }

}


//
//  Scalars
//

// -----------------------------------------------------------------------------


void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname, 
                           char & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TBYTE,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname, 
                           unsigned char & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TBYTE,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           bool & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TLOGICAL,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           short & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TSHORT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           long & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TLONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           long long & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TLONGLONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           int & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TINT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           unsigned int & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TUINT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           unsigned short & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TUSHORT,dnull_flags);;
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           unsigned long & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TULONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           unsigned long long & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TLONGLONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           float & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TFLOAT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           double & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TDOUBLE,dnull_flags);
}


// -----------------------------------------------------------------------------

void Router::connectScalar(const RWModeEnum rwmode, const std::string & colname,  
                           std::string & data, char* dnull_flags) {
  connect_generic(rwmode,colname,&data,0,true,TSTRING,dnull_flags);
}

// -----------------------------------------------------------------------------


//
//  Fixed Length Arrays
//

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname,  
                                     char* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TBYTE,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname,  
                                     unsigned char* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TBYTE,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname,  
                                     bool* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TLOGICAL,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     short* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TSHORT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     long* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TLONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     long long* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TLONGLONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     int* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TINT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     unsigned int* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TUINT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     unsigned short* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TUSHORT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     unsigned long* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TULONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     unsigned long long* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TLONGLONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     float* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TFLOAT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectFixedLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                                     double* data, char* dnull_flags) {
  connect_generic(rwmode,colname,data,0,false,-TDOUBLE,dnull_flags);
}

// -----------------------------------------------------------------------------

//
//  Variable Length Arrays
//

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     char* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TBYTE,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     unsigned char* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TBYTE,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     bool* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TLOGICAL,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     short* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TSHORT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     long* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TLONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     long long* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TLONGLONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     int* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TINT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     unsigned int* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TUINT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     unsigned short* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TUSHORT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     unsigned long* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TULONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     unsigned long long* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TLONGLONG,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     float* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TFLOAT,dnull_flags);
}

// -----------------------------------------------------------------------------

void Router::connectVariableLengthArray(const RWModeEnum rwmode, const std::string & colname, 
                     double* data, IndexType & data_count, char* dnull_flags) {
  connect_generic(rwmode,colname,data,&data_count,false,-TDOUBLE,dnull_flags);
}

// -----------------------------------------------------------------------------


//
//  Bit
//

// -----------------------------------------------------------------------------

void Router::connectBit(const RWModeEnum rwmode, const std::string & colname,
                        char* data, IndexType & data_count) {
  connect_generic(rwmode,colname,data,&data_count,false,-TBIT,0);
}

// -----------------------------------------------------------------------------

void Router::disconnect(void) {
  clearConnections();
  m_ahffp = 0;
}

// -----------------------------------------------------------------------------

void Router::clearConnections(void) {
  for (ConnectionIteratorType it=m_connection.begin(); it != m_connection.end(); it++) {
    delete it->second;
    it->second=NULL;
  }
  m_connection.clear();
}

// -----------------------------------------------------------------------------

void Router::flushAndClearBuffers(void) {
  for (ConnectionIteratorType it=m_connection.begin(); it != m_connection.end(); it++) {
    it->second->flushAndClearBuffer();
  }  
}

// -----------------------------------------------------------------------------

Router::ConnectionIteratorType Router::connectionBegin() { return m_connection.begin(); }

// -----------------------------------------------------------------------------

Router::ConnectionIteratorType Router::connectionEnd(){ return m_connection.end(); }

// -----------------------------------------------------------------------------

size_t Router::numConnection(){ return m_connection.size(); }

// -----------------------------------------------------------------------------

} // namespace ahfits

/* Revision Log
   $Log: ahfits_router.cxx,v $
   Revision 1.50  2015/04/02 15:03:21  mwitthoe
   ahfits: add checks for NULL ahfits or cfitsio FITS pointers; see issue 461

   Revision 1.49  2014/12/16 21:37:07  mwitthoe
   ahfits: only refuse cross-type, null connections when write-mode is enabled; see issue 341

   Revision 1.48  2014/12/16 16:29:11  mdutka
   updated exception for issue #341 to occur as soon as the colmuns are connected

   Revision 1.47  2014/11/26 15:12:45  mwitthoe
   ahfits: add clobber, buffer, and history states to library; library now accesses these states instead of those in ahgen; see issue 437

   Revision 1.46  2014/05/13 14:52:37  mwitthoe
   ahfits: add documentation and code tweaks based on the ahfits code review; see issue 376

   Revision 1.45  2014/04/04 17:05:07  mwitthoe
   ahfits: discovered two functions with similar capability: isIntegerTypeColumn in ahfits_base and isInteger in ahfits_colinfo; I like the former's name, but the latters implementation, so removed the first and renamed the second; changed all appropriate calls to these functions which were restricted within the ahfits library; the ahfits unit test and all tool unit tests still pass

   Revision 1.44  2014/03/27 15:06:49  mwitthoe
   ahfits: add unit tests to test buffering when creating a file or editing an entire file

   Revision 1.43  2014/03/26 18:47:42  mwitthoe
   ahfits: remove old connect() functions; these have been replaced by, for example, connectScalar()

   Revision 1.42  2014/02/03 16:00:40  rshill
   Added column names to connect_generic throw messages.

   Revision 1.41  2014/02/01 01:07:33  rshill
   Allowed byte array to be connected to 1X column for type TBIT.

   Revision 1.40  2014/01/07 21:42:17  mwitthoe
   ahfits: added test cases to ut_ahfits_row.cxx to check for invalid connects to TBIT columns; see issue 256

   Revision 1.39  2014/01/07 21:31:16  mwitthoe
   ahfits: update connect_generic (ahfits_router) to refuse to connect a TBIT column with a non-TBIT output type; if violated, a logic error is thrown with a clear description of the problem; see issue 256

   Revision 1.38  2013/12/02 15:26:18  asargent
   Fixed a bug where ahfits_router.cxx was calling an old function (executable_name())

   Revision 1.37  2013/10/16 01:14:30  mwitthoe
   ahfits library: add isBintable() calls to restrict certain functions to binary tables, mainly column information routines and the router; remove the function, column(), from ahfits_colinfo since it was only being used by the function, columnType()

   Revision 1.36  2013/10/10 17:05:50  peachey
   Internal refactoring of connect_generic and functions that call it.
   Connections to scalar/vector client data are now indicated by a new
   argument (bool scalar) rather than keying off the sign of the out_type
   variable. Changed all functions that call connect_generic to use the
   new signature.

   Added a test to confirm connecting to vectors of strings leads to
   an exception being thrown. Moved the check for this case from the
   Connection class into the connect_generic function.

   Revision 1.35  2013/10/04 21:20:46  mwitthoe
   ahfits library: previous version of writing buffer information to the FITS header would fail if a keyword was written to the output file before making a table connection; this has been fixed by moving the recording of buffer information to FITS files from stamp() to connect_generic()

   Revision 1.34  2013/10/04 15:39:09  mwitthoe
   ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

   Revision 1.33  2013/10/03 23:27:45  klrutkow
   removed dnull_flags from argument to connectBit and just passed 0 to connectGeneral

   Revision 1.32  2013/09/11 20:17:47  klrutkow
   fixed typos in doxygen

   Revision 1.31  2013/09/11 20:01:33  klrutkow
   finished creating new overloaded connect functions.  now we have
   connectScalar(), connectFixedLengthArray(), and connectVariableLengthArray(),
   all of which are overloaded to accept different data types (float, double, etc)
   and to use the same functions for the null flag (dnull_flag is now default set
   to NULL). Also, added \param tags that were missing.  Changed the ut to call
   the new functions rather than the old connect() functions.

   Revision 1.30  2013/09/09 18:52:22  mwitthoe
   ahfits library: preliminary addition of new connect functions (non-null double scalar and array)

   Revision 1.29  2013/07/20 03:09:39  mwitthoe
   ahfits router: remove unnecessary typedef

   Revision 1.28  2013/07/18 15:54:35  mwitthoe
   ahfits: add ahfits_buffer.h to ahfits.h; the 2nd argument of the constructor had a default value which was messing up the building of tools (I do not know why), changed to an overloaded constructor for now

   Revision 1.27  2013/07/18 14:21:04  mwitthoe
   ahfits: add optional argument to Router constructor where the overlap can be provided, the default is to have no overlap

   Revision 1.26  2013/07/17 20:40:58  mwitthoe
   ahfits refactoring: change setNull() to use column name instead of column number; remove getColumn* functions from colinfo (they were confusing); add functions to clear/reload information from single column; remove unused statusMessage function; mark several functions as internal

   Revision 1.25  2013/07/17 17:26:56  mwitthoe
   ahfits: add support for TDISP; refactoring related to code review (issue 266)

   Revision 1.24  2013/07/12 22:16:26  mwitthoe
   ahfits buffering: fix bug where buffer was not properly flushed and reset when moving to a new extension but keeping router connections active

   Revision 1.23  2013/07/12 13:30:33  mwitthoe
   ahfits: write function to provide standard error message prefix; use prefix throughout ahfits library

   Revision 1.22  2013/07/08 18:25:39  mwitthoe
   ahfits library: add setTNull() which will write the TNULLL keyword for integer-like columns; when making router connection with local NULL flags, require target, integer-like columns to have the TNULL keyword defined

   Revision 1.21  2013/07/01 19:56:15  mwitthoe
   have ahfits router use the global buffer state when setting the buffer size for new connections

   Revision 1.20  2013/06/17 15:54:34  mwitthoe
   ahfits: add function, clearConnections(), to the router class to allow for changing extension within a file which has different columns than the original extension; needed to have the reloadColInfo() function check to see if a column exists before reloading the information from the header

   Revision 1.19  2013/05/14 14:17:32  mwitthoe
   ahfits buffering: add row overlapping when reading next buffer, add support for buffer size options; various debugging

   Revision 1.18  2013/05/13 19:45:51  mwitthoe
   add buffering to ahfits (no TBIT support yet)

   Revision 1.17  2013/03/19 20:46:21  rshill
   Column ead/write flags implemented; inner-loop debugging code deleted.

   Revision 1.16  2013/03/18 23:47:00  rshill
   Column-specific readable/writeable flag added.

   Revision 1.15  2013/03/09 04:21:55  peachey
   Use local container of Connections rather than the ahfits_connect
   facilities.

   Revision 1.14  2013/03/04 20:27:08  rshill
   Corrected two calls to connect_generic.

   Revision 1.13  2013/03/02 00:55:29  rshill
   Added connect overloads for scalars with nulls.

   Revision 1.12  2013/02/28 21:59:19  rshill
   Added TNULL and NaN capability for flagging undefined values.

   Revision 1.11  2012/12/28 20:51:14  peachey
   Add and test support for bit-field type columns (type X).

   Revision 1.10  2012/12/17 15:23:31  mwitthoe
   remove debugging print statement from ahfits_router

   Revision 1.9  2012/12/07 23:00:12  mwitthoe
   make changes to ahfits recommended by Dec 7 code review (see http://zuul:8080/redmine/projects/astroh/wiki/2012-12-07_Ahfits_Code_Review)

   Revision 1.8  2012/10/23 21:45:56  mwitthoe
   add connections for unsigned short, long, and long long to the ahfits router

   Revision 1.7  2012/10/23 20:10:44  mwitthoe
   add connection for array of unsigned ints to ahfits router

   Revision 1.6  2012/10/22 17:20:53  mwitthoe
   fix in ahfits to allow reading/writing of variable-length columns

   Revision 1.5  2012/10/18 20:12:57  mwitthoe
   add router connection for unsigned long long scalar variables

   Revision 1.4  2012/10/18 19:26:58  mwitthoe
   added router connection for unsigned char to ahfits

   Revision 1.3  2012/10/12 22:50:00  mwitthoe
   ahfits: use pass-by-reference in scalar connections; add columnLength() function

   Revision 1.2  2012/10/10 20:30:56  mwitthoe
   ahfits: complete overloaded connection functions; add test code

   Revision 1.1  2012/10/01 17:21:57  mwitthoe
   new version of ahfits with connections stored outside of FITS file object


*/
