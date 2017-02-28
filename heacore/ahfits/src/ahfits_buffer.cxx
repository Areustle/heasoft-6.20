/// \brief <brief-file-description>
/// \author <author-name>
/// \date $Date: 2016/04/07 17:32:27 $

// Need to define a couple macros expected by the version macros used in the header
// files.  For a safe value of AHLABEL use the lower case value of the first 
// argument in the associated header file (e.g. ahtime_ahtimeassign).  For tools, 
// use TOOL_<TOOLNAME>, e.g. tool_ahtime.
// Note: replace \$ with $ below
#define AHLABEL cxx_standard
#define AHCVSID "$Id: ahfits_buffer.cxx,v 1.39 2016/04/07 17:32:27 mwitthoe Exp $"

#include "ahfits/ahfits_buffer.h"
#include "ahfits/ahfits_file.h"
#include "ahfits/ahfits_colinfo.h"

#include "ahlog/ahlog.h"

#include <sstream>
#include <cstring>
#include <cstdlib>
#include <limits>

namespace ahfits {

/// \brief byte size for buffer to store element size of variable length columns
const int VARCOL_ELEM_SIZE=8;    

// -----------------------------------------------------------------------------

int sizeOfType(int typecode) {
  if (typecode < 0) typecode=-typecode;
  switch (typecode) {
    case TBIT:
    case TBYTE:
    case TLOGICAL:
      return sizeof(char);
    case TSHORT:
      return sizeof(short);
    case TUSHORT:
      return sizeof(unsigned short);
    case TINT:
      return sizeof(int);
    case TUINT:
      return sizeof(unsigned int);
    case TLONG:
      return sizeof(long);
    case TULONG:
      return sizeof(unsigned long);
    case TLONGLONG:
      return sizeof(long long);
    case TFLOAT:
      return sizeof(float);
    case TDOUBLE:
      return sizeof(double);
    case TSTRING:
      return -1;     // special case, STRING variable length
  }
  std::stringstream msg;
  msg << "invalid data type given to Buffer: " << typecode;
  AH_THROW_RUNTIME(msg.str());
  return 0;
}

// -----------------------------------------------------------------------------

// +++ 2014-01-27 MCW Currenty, setNulls() may not work correctly if writing
// +++ 2014-01-27 MCW a float-type to an integer column or vice versa.  This
// +++ 2014-01-27 MCW is due to how NULLs are assigned for these types.  The
// +++ 2014-01-27 MCW fix for this is going to be put off until Build 5.
// +++ 2014-01-27 MCW See issue 341.
void setNulls(FilePtr ahffp, const std::string& colname, int typecode, 
              void * data, ahfits::IndexType num_el, char * dnull_flags) {

//  Supported types in ahfits, with classification of
//      null-value support:
//
//    char                  TBYTE       TNULL  
//    int                   TINT        TNULL
//    short                 TSHORT      TNULL
//    long                  TLONG       TNULL
//    long long             TLONGLONG   TNULL
//    unsigned char         TBYTE       TNULL
//    unsigned int          TUINT       TNULL
//    unsigned short        TUSHORT     TNULL
//    unsigned long         TULONG      TNULL
//    unsigned long long    TLONGLONG   TNULL
//    float                 TFLOAT      NaN
//    double                TDOUBLE     NaN
//    bool                  TLOGICAL    no null support
//    std::string           TSTRING     no null support

  std::stringstream ss;

  if (dnull_flags != 0) {
    
    if (isIntegerTypeColumn(ahffp, colname)) {
      long long tnull;
      if (columnNull(ahffp, colname, tnull)) {
        for (ahfits::IndexType i_el=0; i_el<num_el; ++i_el) {
          if (dnull_flags[i_el] == 1) {
            switch (typecode) {
              case TBYTE:      reinterpret_cast<char *>(data)[i_el] = (char) tnull; break;
              case TINT:       reinterpret_cast<int *>(data)[i_el] = (int) tnull; break;
              case TSHORT:     reinterpret_cast<short *>(data)[i_el] = (short) tnull; break;
              case TLONG:      reinterpret_cast<long *>(data)[i_el] = (long) tnull; break;
              case TLONGLONG:  reinterpret_cast<long long *>(data)[i_el] = (long long) tnull; break;
              case TUINT:      reinterpret_cast<unsigned int *>(data)[i_el] = (unsigned int) tnull; break;
              case TUSHORT:    reinterpret_cast<unsigned short *>(data)[i_el] = (unsigned short) tnull; break;
              case TULONG:     reinterpret_cast<unsigned long *>(data)[i_el] = (unsigned long) tnull; break;
              default: 
                ss << typecode;
                AH_THROW_LOGIC(ahfits::errPrefix(ahffp)+"unsupported integer column type = "+ss.str());
                } 
          } else if (dnull_flags[i_el] != 0) {
            AH_THROW_RUNTIME(ahfits::errPrefix(ahffp)+"error writing column "+colname+
                             "; bad value of dnull_flags (must be 0 or 1)");
          }
        }
      } else {
        AH_THROW_RUNTIME(ahfits::errPrefix(ahffp)+"error writing column "+colname+
                         "; attempt to set null values for integer column without a TNULL in the header");
      }
    } else if (isFloatTypeColumn(ahffp, colname)) {
      for (ahfits::IndexType i_el=0; i_el<num_el; ++i_el) {
        if (dnull_flags[i_el] == 1) {              
          switch (typecode) {
            case TFLOAT:    ((float *)data)[i_el] = std::numeric_limits<float>::quiet_NaN(); break;
            case TDOUBLE:   ((double *)data)[i_el] = std::numeric_limits<double>::quiet_NaN(); break;
            default: 
              ss << typecode;
              AH_THROW_LOGIC(ahfits::errPrefix(ahffp)+"unsupported floating column type = "+ss.str());
          } 
        } else if (dnull_flags[i_el] != 0) {
          AH_THROW_RUNTIME(ahfits::errPrefix(ahffp)+"error writing column "+colname+
                           "; bad value of dnull_flags (must be 0 or 1)");
        }
      }
    } else {
      ss << "error writing column: " << colname << "; attempt to set null values for unsupported type: " << typecode;
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp)+ss.str());
    }
  }
}

// -----------------------------------------------------------------------------

Buffer::Buffer(FilePtr ahffp, const std::string & colname, RWModeEnum rwmode, 
               int typecode, bool keep_null, long numrows, int overlap):
  m_ahffp(ahffp), m_colname(colname), m_disable(false), m_varcol(false), 
  m_rwmode(rwmode), m_typecode(typecode), m_keep_null(keep_null), 
  m_element_size(1), m_num_per_row(1), m_buf_max(0), m_overlap(overlap),
  m_buf_first(0), m_buf_last(0), m_current_row(1), m_buffer(0), m_nullbuf(0),
  m_strbuf(0), m_strpointers(0) {

  // ensure that type code is not negative
  if (0 > m_typecode) m_typecode=-m_typecode;

  // buffering must be disabled for TBIT types
  if (TBIT == m_typecode) m_disable=true;

  // check if FITS column has variable-length; get element size and number
  // per row
  if (0 > ahfits::columnType(m_ahffp,m_colname)) m_varcol=true;
  m_element_size=sizeOfType(m_typecode);
  m_num_per_row=ahfits::columnRepeat(m_ahffp,m_colname);

  if (TSTRING == m_typecode) {
    m_element_size=ahfits::columnWidth(m_ahffp,m_colname);
    m_num_per_row=m_element_size/m_num_per_row;
    m_element_size++;                // +1 for final \0 in char*
  }

  // allocate buffer (function will set m_buf_first/last and m_buf_max)
  allocate(numrows);
}

// -----------------------------------------------------------------------------

Buffer::~Buffer() {
  if (m_buffer != 0) {
    try {
      flush();                            // write buffer to FITS file, if necessary
    } catch (...) {} 
    ahfits::removeExtraRows(m_ahffp);   // remove extra blank rows from end of file
    deallocate();                       // free memory
  }
}

// -----------------------------------------------------------------------------

void Buffer::read(void* data, ahfits::IndexType* data_count, char* dnull_flags) {
  if (0 == data) AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp)+"Null data pointer passed");

  // prepare for reading
  ahfits::IndexType rowidx=m_ahffp->m_currow;  // current row index
  ahfits::IndexType nelem=0;                   // number of elements to read
  ahfits::IndexType bidx=0;                    // buffer index
  void* tdata=0;                               // pointer to output data

  // Need to set up the void pointer which will store the values read from the
  // FITS file.  Normally, the pointer is the same as the data pointer passed
  // into this function.  However, for strings, where cfitsio expects a char*
  // variable, we set our pointer to the intermediate string buffer set up
  // for this purpose: m_strbuf.  Another twist: when buffering is disabled
  // and we read directly from the FITS file, the cfitsio reading function
  // expects to get the address of the char* variable (&m_strbuf).  However with
  // buffering enabled, the memcpy function we use to copy the buffer contents
  // into m_strbuf expects the pointer itself.
  if (TSTRING == m_typecode) {
    if (m_disable)
      tdata=&m_strbuf;
    else
      tdata=m_strbuf;
  } else {                        // column is not of type TSTRING
    tdata=data;
  }

  // The main purpose of this block of code is to determine the number of 
  // elements (nelem) to read.  The number of elements is 1 for scalar columns,
  // but can be greater than 1 for array or bit columns.  In addition, when
  // buffering is enabled (not disabled), the following is done:
  //  1. if necessary, read FITS data into the buffer - fill()
  //  2. get the position in the buffer to start reading from (bidx)
  if (!m_disable) {
    
    // check if buffer is empty or current row is not in buffered interval
    if (0 == m_buf_first || rowidx < m_buf_first || rowidx > m_buf_last) fill();
  
    // get index relative to buffer
    bidx=rowidx-m_buf_first;
  
    // get number of elements to read from variable-length column
    if (m_varcol) {
      ahfits::IndexType* sizes=(ahfits::IndexType*) m_buffer;
      nelem=sizes[bidx];
    }
  
  } else if (m_varcol) {    // AND buffering disabled

    // need to get number of elements to read for variable-length column
    int retcode=0;
    int status=0;
    int colnum=ahfits::name2Num(m_ahffp,m_colname);
    retcode=fits_read_descriptll(m_ahffp->m_cfitsfp,colnum,m_ahffp->m_currow,
                                  &nelem,0,&status);
    if (0 != retcode) {
      std::stringstream ss;
      ss << colnum;
      AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp)+"error reading number of elements from "+
                       "variable-length column: "+ss.str()+statusMsg(status));
    }
  } else {                 // buffering disabled for non-variable-length columns
    nelem=m_num_per_row;
  }

  // read row
  if (m_disable || m_varcol) {      // buffering disabled or variable-length
    int retcode=0;
    int anynul=0;
    int status=0;
    int colnum=ahfits::name2Num(m_ahffp,m_colname);
    if (0 == dnull_flags) {
      retcode=fits_read_col(m_ahffp->m_cfitsfp,m_typecode,colnum,rowidx,1,nelem,
                            0,tdata,&anynul,&status);
    } else {
      retcode=fits_read_colnull(m_ahffp->m_cfitsfp,m_typecode,colnum,rowidx,1,
                                nelem,tdata,dnull_flags,&anynul,&status);
    }
    if (0 != retcode) {
      std::stringstream ss;
      ss << colnum;
      AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp)+"error reading column: "+ss.str()+statusMsg(status));
    }
  } else {                            // buffering enabled for fixed-length
    int length=m_element_size*m_num_per_row;
    memcpy(tdata,(char*)m_buffer+bidx*length,length);
    if (0 != dnull_flags) {
      length=m_num_per_row;
      memcpy(dnull_flags,m_nullbuf+bidx*length,length);
    }
  }

  // set data count
  if (0 != data_count) *data_count=nelem;

  // need to convert char* data in m_strbuf to std::string in data (local 
  // variable); since data is a void*, need to have compiler interpret it
  // as std::string* so that the equals sign will perform the char* ->
  // std::string conversion automatically
  if (TSTRING == m_typecode)
    *reinterpret_cast<std::string*>(data)=m_strbuf;
}

// -----------------------------------------------------------------------------

void Buffer::write(void* data, ahfits::IndexType* data_count, char* dnull_flags) {
  if (0 == data) AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp)+"Null data pointer passed");

  // data_count is required if writing a variable-length column
  if (m_varcol && 0 == data_count) {
    AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp)+"non-NULL data_count is required when writing "+
                   " variable-length column: "+m_colname);
  }

  // prepare for writing
  ahfits::IndexType rowidx=m_ahffp->m_currow;      // current row index
  ahfits::IndexType bidx=0;                        // buffer index  (zero-based index)
  ahfits::IndexType nelem=m_num_per_row;           // number of elements to write
  if (m_varcol) nelem=*data_count;
  if (!m_disable) {
    // make sure current row is buffered; note: fill() calls flush()
    // 1. m_buf_first == 0 when opening a FITS file before any reading/writing
    // 2. rowidx < m_buf_first || rowidx > m_buf_last when current row index
    //    is outside range of currently buffered rows
    // Note: the fill() call will populate m_buffer
    if (0 == m_buf_first || rowidx < m_buf_first || rowidx > m_buf_last) fill();
  
    // get index relative to buffer
    bidx=rowidx-m_buf_first;
  }

  // since cfitsio handles strings as char* quantities, we must convert our
  // std::string variable into a special char* buffer, m_strbuf, which will
  // be written into the FITS file
  if (TSTRING == m_typecode) {
    const char* tmp=(reinterpret_cast<std::string*>(data))->c_str();
    std::strncpy(m_strbuf,tmp,m_element_size-1);
  }

  // write value
  void* tdata=data;
  if (m_disable || m_varcol) {     // buffering disabled or variable-length
    if (TSTRING == m_typecode) tdata=&m_strbuf;
    int retcode=0;
    int status=0;
    // Null (undefined) value processing cannot have a parallel form in
    // writing to what it has in reading because of the way cfitsio 
    // structures its calls.  Here we handle nulls ourselves.
    if (dnull_flags != 0) setNulls(m_ahffp,m_colname,m_typecode,tdata,nelem,
                                   dnull_flags);

    int colnum=ahfits::name2Num(m_ahffp,m_colname);
    retcode=fits_write_col(m_ahffp->m_cfitsfp,m_typecode,colnum,rowidx,1,
                           nelem,tdata,&status);
    if (0 != retcode) {
      AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp)+"error writing column: "+
                       m_colname+statusMsg(status));
    }
  } else {                           // buffering enabled for fixed-length
    if (TSTRING == m_typecode) tdata=m_strbuf;
    int length=m_element_size*m_num_per_row;
    memcpy((char*)m_buffer+bidx*length,tdata,length);
    if (0 != dnull_flags) {
      length=m_num_per_row;
      memcpy(m_nullbuf+bidx*length,dnull_flags,length);
    }
  }

  // update last buffered index and number of rows in ahffp pointer; 
  // should only change by 0 or 1
  if (!m_disable && rowidx > m_buf_last) m_buf_last=rowidx;
  if (m_ahffp->m_currow > m_ahffp->m_numrow) m_ahffp->m_numrow=m_ahffp->m_currow;
}

// -----------------------------------------------------------------------------

void Buffer::clear(void) {
  // Buffer is cleared by setting all elements to 0.
  if (0 != m_buffer) {
    if (m_varcol) {
      std::memset(m_buffer,0,m_buf_max*VARCOL_ELEM_SIZE);
    } else {
      std::memset(m_buffer,0,m_buf_max*m_element_size*m_num_per_row);
      if (m_keep_null) std::memset(m_nullbuf,0,m_buf_max*m_num_per_row);
    }
  }
  m_buf_first=0;
  m_buf_last=0;
  m_current_row=1;
}

// -----------------------------------------------------------------------------

void Buffer::flush(void) {

  // variable declarations
  ahfits::IndexType lastrow=0;     // last buffered row
  void* tbuf=0;                    // pointer to data to be written
  long nelem=0;                    // number of data elements to write
  int status=0;                    // cfitsio return status
  int colnum=0;                    // FITS index of column to write

  // variable-length or read-only columns do not need to be flushed
  if (m_disable || m_varcol || m_rwmode == e_READONLY) return;
  if (m_buf_first < 1) return;     // ... nor an empty buffer

  // get last buffer index to write
  lastrow=m_buf_last;
  if (lastrow > m_ahffp->m_numrow) lastrow=m_ahffp->m_numrow;   // just in case

  // get number of elements to write; TSTRING value are handled differently
  tbuf=m_buffer;
  nelem=m_num_per_row*(lastrow-m_buf_first+1);
  if (TSTRING == m_typecode) {
    tbuf=m_strpointers;
  }

  // Null (undefined) value processing cannot have a parallel form in writing
  // to what it has in reading because of the way cfitsio structures its calls. 
  // Here we set NULL values manually based on column type.
  if (m_nullbuf != 0) setNulls(m_ahffp,m_colname,m_typecode,tbuf,nelem,m_nullbuf);

  status=0;
  colnum=ahfits::name2Num(m_ahffp,m_colname);
  fits_write_col(m_ahffp->m_cfitsfp,m_typecode,colnum,m_buf_first,1,nelem,tbuf,&status);
  if (0 != status) {
    AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp)+"error writing column "+m_colname+statusMsg(status));
  }
}

// -----------------------------------------------------------------------------

void Buffer::allocate(long bufrows) {
  if (0 != m_buffer)
    AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp)+"cannot allocate; value buffer already allocated");
  if (TSTRING == m_typecode) {
    if (0 != m_strbuf)
      AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp)+"cannot allocate; string buffer already allocated");
    if (0 != m_strpointers)
      AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp)+"cannot allocate; string pointers already allocated");
  }

  if (m_keep_null && 0 != m_nullbuf)
    AH_THROW_LOGIC(ahfits::errPrefix(m_ahffp)+"cannot allocate; NULL buffer already allocated");

  // determine number of rows to buffer (or disable)
  if (0 == bufrows) {              // buffering is disabled
    m_buf_max=1;
    m_disable=true;
  } else if (0 > bufrows) {        // use cfitsio recommended buffer size
    int status=0;
    if (0 != fits_get_rowsize(m_ahffp->m_cfitsfp,&m_buf_max,&status))
      AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp)+"fits_get_rowsize returned a non-zero status"+statusMsg(status));
  } else {                         // buffering is manually set
    m_buf_max=bufrows;
  }

  // remove overlap if m_buf_max too small; otherwise the overlap will add
  // too much overhead to the buffering operation
  if (m_buf_max < 2*m_overlap) {
    m_overlap=0;
    if (!m_disable && m_buf_max > 1)
      AH_DEBUG << "Overlap (" << m_overlap << ") too large a fraction of buffer size (" 
               << m_buf_max << "); resetting overlap to zero to avoid inefficient reading of the FITS file"
               << std::endl;
  }

  // determine size of buffer; for variable-length columns we buffer array size
  // per row instead of actual data
  long size=0;
  if (m_varcol) {
    size=VARCOL_ELEM_SIZE*m_buf_max; // to store result of fits_read_descriptll
  } else {
    size=m_element_size*m_num_per_row*m_buf_max;
  }

  // allocate buffers
  if (!m_disable) {
    m_buffer=new char[size];
    std::memset(m_buffer,0,size);                     // data buffer

    if (m_keep_null && !m_varcol) {
      m_nullbuf=new char[m_buf_max*m_num_per_row];    // null buffer
      std::memset(m_nullbuf,0,m_buf_max*m_num_per_row);
    }
  }

  // Allocate string buffer and set string pointers to buffer positions.
  // Note: this step is necessary in order to later convert the char* that
  // cfitsio uses for strings into the std::string variables that ahfits uses.
  if (TSTRING == m_typecode) {
    m_strbuf=new char[m_element_size];
    std::memset(m_strbuf,'\0',m_element_size);
    if (!m_disable) {
      m_strpointers=new char*[m_buf_max*m_num_per_row];
      for (long ip=0; ip < m_buf_max; ip++)
        m_strpointers[ip]=(char*)m_buffer+ip*m_element_size;
    }
  }

}

// -----------------------------------------------------------------------------

void Buffer::deallocate(void) {
  if (0 != m_buffer) {
    delete [] (char*)m_buffer;
    m_buffer=0;
  }
  if (0 != m_nullbuf) {
    delete [] m_nullbuf;
    m_nullbuf=0;
  }
  if (0 != m_strbuf) {
    delete [] m_strbuf;
    m_strbuf=0;
  }
  if (0 != m_strpointers) {
    delete [] m_strpointers;
    m_strpointers=0;
  }
}

// -----------------------------------------------------------------------------

void Buffer::fill(void) {
  int status=0;
  if (m_disable) return;

  // flush and empty current buffer contents
  flush();    // READ-ONLY is checked within flush()
  clear();

  // determine number of rows to read
  // if the last buffered row is greater than the current number of rows in the
  // file, then:
  // 1. when only editing a file, update the last buffered row to the last
  //    row index in the file, or
  // 2. when allowing extra rows to be added to the end of the file (see
  //    Performance section of documentation), add extra rows to the end of
  //    the FITS file to allow the full buffer to be used.
  ahfits::IndexType firstrow=m_ahffp->m_currow-m_overlap;
  if (firstrow < 1) firstrow=1;
  ahfits::IndexType numrows=m_buf_max;
  ahfits::IndexType lastrow=firstrow+numrows-1;
  if (lastrow > m_ahffp->m_numrow_with_padding) {
    if (m_rwmode == e_READONLY || !m_ahffp->m_add_extrarows_when_buffering) {
      numrows=m_ahffp->m_numrow_with_padding-firstrow+1;
    } else {
      ahfits::IndexType numtoadd=lastrow-m_ahffp->m_numrow_with_padding;
      ahfits::addExtraRows(m_ahffp,numtoadd);
    }
  }

  // number of elements to read
  ahfits::IndexType nelem=m_num_per_row*numrows;

  // For TSTRING columns, cfitsio cannot write directly to the buffer.  Instead,
  // we give cfitsio an array of pointers to the starting positions of each
  // char* string allocated in the buffer.  The m_strpointers variable was
  // set up in allocate().
  void* tbuf=m_buffer;
  if (TSTRING == m_typecode) tbuf=m_strpointers;

  // read rows from FITS file; for variable-length columns, just read size of
  // each row
  int colnum=ahfits::name2Num(m_ahffp,m_colname);
  int anynul=0;
  int retcode=0;
  if (m_varcol) {
    retcode=fits_read_descriptsll(m_ahffp->m_cfitsfp,colnum,firstrow,numrows,
                                  (ahfits::IndexType*)tbuf,0,&status);
  } else if (m_keep_null) {
    retcode = fits_read_colnull(m_ahffp->m_cfitsfp,m_typecode,colnum,firstrow,
                                1,nelem,tbuf,m_nullbuf,&anynul,&status);
  } else {
    retcode=fits_read_col(m_ahffp->m_cfitsfp,m_typecode,colnum,firstrow,1,
                          nelem,0,tbuf,&anynul,&status);
  }
  if (0 != retcode) {
    std::stringstream ss;
    ss << colnum;
    AH_THROW_RUNTIME(ahfits::errPrefix(m_ahffp)+"error reading column #"+ss.str()+
                     statusMsg(status));
  }
  m_buf_first=firstrow;
  m_buf_last=firstrow+numrows-1;
}

// -----------------------------------------------------------------------------

int Buffer::getBufferSize(void) {
  return m_buf_max;
}

// -----------------------------------------------------------------------------

} // Ends namespace ahfits

/* Revision Log
 * $Log: ahfits_buffer.cxx,v $
 * Revision 1.39  2016/04/07 17:32:27  mwitthoe
 * ahfits: clarify message about overlap being too large compared to the buffer size; also change this message from INFO to DEBUG
 *
 * Revision 1.38  2016/01/29 18:34:23  mwitthoe
 * ahfits: change warn message to info statement when buffer overlap parameter gets reset
 *
 * Revision 1.37  2014/12/15 22:28:05  mdutka
 * updated exception for issue #341 to occur as soon as the colmuns are connected
 *
 * Revision 1.36  2014/12/12 22:47:11  mdutka
 * Exception is thrown if buffer is set and crosstypes with nulls are connected (see issue #341)
 *
 * Revision 1.35  2014/09/12 20:25:14  mwitthoe
 * ahfits: fix bug in setNulls() (in ahfits_buffer) where NULL was not set correctly for a TFLOAT column stored in a double variable for vice versa; the FITS column type code was being used instead of the local variables when typecasting Nan for float-type columns
 *
 * Revision 1.34  2014/05/23 21:06:03  mwitthoe
 * ahfits: make changes according to James' recommendations resulting from teh ahfits code review; see issue 376, updates 11 & 12
 *
 * Revision 1.33  2014/05/13 14:52:37  mwitthoe
 * ahfits: add documentation and code tweaks based on the ahfits code review; see issue 376
 *
 * Revision 1.32  2014/04/17 15:38:27  mwitthoe
 * ahfits: fix bug in buffer which sometimes causes an d an extra row to be added to the end of an appended file (issue 379); simplified bug fix from issue 344
 *
 * Revision 1.31  2014/04/04 17:05:07  mwitthoe
 * ahfits: discovered two functions with similar capability: isIntegerTypeColumn in ahfits_base and isInteger in ahfits_colinfo; I like the former's name, but the latters implementation, so removed the first and renamed the second; changed all appropriate calls to these functions which were restricted within the ahfits library; the ahfits unit test and all tool unit tests still pass
 *
 * Revision 1.30  2014/04/01 14:57:06  mwitthoe
 * ahfits: add member to ahfits FilePtr indicating whether extra rows are added to the end of the FITS file when buffering; see issue 368
 *
 * Revision 1.29  2014/03/31 18:07:49  mwitthoe
 * ahfits: store temporary table row size in ahfits FilePtr instead of buffer instance; see issue 369
 *
 * Revision 1.28  2014/03/27 15:06:49  mwitthoe
 * ahfits: add unit tests to test buffering when creating a file or editing an entire file
 *
 * Revision 1.27  2014/03/26 18:59:44  mwitthoe
 * ahfits: stop automatically removing extra rows in buffer read/write; fix code calculating how many rows to read into buffer; use numRows() function in ahfits_row.cxx which returns the number of data rows instead of ahffp->m_numrow which returns the current number of rows in the FITS table (the latter can be larger than the former when the buffer has appended rows to the file
 *
 * Revision 1.26  2014/02/24 15:11:47  asargent
 * Removed function removeExtraRows() from ahfits::flush()
 *
 * Revision 1.24  2014/01/27 18:55:30  mwitthoe
 * ahfits: fix bug in the setNulls() function (ahfits_buffer) where the FITS column type was used when assigning NULL values to the buffer array when the local variable type should have been used instead; this caused strange values to appear in the buffer.  This fix does not solve all problems with this function; see issue 341
 *
 * Revision 1.23  2014/01/14 19:02:39  mwitthoe
 * ahfits: the numRows() function in ahfits_row was returning the value of m_numrow in the ahfits pointer; however when adding rows to the end of a FITS file, the buffer will add a block of rows at once making the m_numrow value unreliable; the numRows() function has been updated to get the correct row count from the Buffer instance (if present)
 *
 * Revision 1.22  2013/10/16 01:40:50  mwitthoe
 * ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270
 *
 * Revision 1.21  2013/10/07 21:10:17  mwitthoe
 * ahfits buffering: fixed bug which sometimes caused a negative number of extra buffer rows to be removed leading to a cfitsio error; this bug was triggered when trying to write to different extensions of a file with a single router (as with the GTI files is mxstime)
 *
 * Revision 1.20  2013/10/04 15:39:09  mwitthoe
 * ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files
 *
 * Revision 1.19  2013/07/18 14:21:04  mwitthoe
 * ahfits: add optional argument to Router constructor where the overlap can be provided, the default is to have no overlap
 *
 * Revision 1.18  2013/07/17 21:38:02  mwitthoe
 * ahfits: remove duplicated code from insertCol* functions by adding a function insertColumnAt; change buffer OVERlAP constant to 0
 *
 * Revision 1.17  2013/07/17 20:40:58  mwitthoe
 * ahfits refactoring: change setNull() to use column name instead of column number; remove getColumn* functions from colinfo (they were confusing); add functions to clear/reload information from single column; remove unused statusMessage function; mark several functions as internal
 *
 * Revision 1.16  2013/07/17 17:26:56  mwitthoe
 * ahfits: add support for TDISP; refactoring related to code review (issue 266)
 *
 * Revision 1.15  2013/07/16 20:10:06  mwitthoe
 * ahfits: fix large inefficiency when writing a new file with varaible-length columns; now, when appending a file, a number of blank rows (equal to the buffer size) are added to the end of the file once the end of file is reached; extra rows are removed when the buffer closes
 *
 * Revision 1.14  2013/07/16 14:09:26  peachey
 * Use sizeof to compute size of returned variables rather than hardwired values.
 *
 * Revision 1.13  2013/07/12 22:16:26  mwitthoe
 * ahfits buffering: fix bug where buffer was not properly flushed and reset when moving to a new extension but keeping router connections active
 *
 * Revision 1.12  2013/07/12 13:30:33  mwitthoe
 * ahfits: write function to provide standard error message prefix; use prefix throughout ahfits library
 *
 * Revision 1.11  2013/07/09 13:59:08  mwitthoe
 * ahfits: add some +++ comments to buffer source related to code review; see issue 266
 *
 * Revision 1.10  2013/07/02 17:12:41  mwitthoe
 * ahfits: fixed segmentation fault when trying to read/write a string column with buffering disabled
 *
 * Revision 1.9  2013/06/18 14:10:27  mwitthoe
 * fix bug in ahfits_buffer occuring when the buffer size (m_buf_max) is smaller than the overlap size (m_overlap); now, when m_buf_max < 2*m_overlap, set m_overlap=0
 *
 * Revision 1.8  2013/05/16 23:36:32  mwitthoe
 * fix bug where fill() was not being called (and setting m_buf_first) when writing to a file before reading the file; this bug affects the case where you are writing a file from scratch
 *
 * Revision 1.7  2013/05/15 17:55:36  mwitthoe
 * fix typo in ahfits buffer description; CVS Log section in ahfits_buffer source was malformed causing it not to be updated with commit messages
 *
 * Revision 1.1  2013/04/03 00:48:53  peachey
 * Initial version of Buffer class.
 *
 */
