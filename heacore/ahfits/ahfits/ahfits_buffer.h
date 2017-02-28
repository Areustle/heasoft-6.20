/// \file ahfits_buffer.h
/// \brief Encapsulation of buffering facilities.
/// \author James Peachey
/// \date $Date: 2014/12/15 22:28:05 $
///
/// \addtogroup mod_ahfits
/// \section ahfits_buffer Buffer Library - ahfits_buffer
///
/// This class is in charge of reading/writing row data directly from/to FITS 
/// files.  A Buffer instance is a member of a Connection instance in charge of
/// routing a single column to a local variable.  As indicated by its name, the
/// Buffer class will store several rows of data in an internal buffer to limit 
/// direct access to the FITS file.  By default, the size of the buffer is 
/// determined by CFITSIO, however a user can choose a custom size or disable
/// buffering entirely.  In the latter case, the Buffer instance is still in
/// charge of accessing the FITS file, but read and write operations will occur
/// row by row.  The size of the buffer used for each extension is written to
/// the tool log file and, for non-read-only files, the FITS header as a 
/// COMMENT.
///
/// The Buffer class supports all functionality of the Router and Connection
/// classes: reading/writing single-value columns and fixed- or variable-length
/// arrays.  For variable-length arrays, the row data is not buffered, however
/// the row sizes are.  For TBIT columns, buffering is disabled due to padding
/// of bit sequences in the FITS file to obtain integral byte fields.  Removing
/// this padding in order to fill a buffer is just as inefficient as not 
/// buffering at all.
///
/// In memory, the FITS file contents are ordered by row.  Since each row is
/// the same size, this allows CFITSIO to access a specific element quickly.
/// However, with variable-length columns, the data cannot be stored with the
/// rest of the row since the size of these columns is not fixed.  Instead,
/// variable-length column data is stored at the end of the FITS table in the
/// 'heap'.  This has consequences when trying to add rows to the end of the
/// table causing the entire heap to be moved.  When the heap is large, this 
/// can be a slow operation.  Therefore, after creating a new file, the ahfits
/// buffer will add many empty rows at a time and then fill them in.  The number
/// of rows added is equal to the buffer size.  When moving to a new extension
/// or closing the file, any extra blank rows at the end of the file are 
/// removed.  Adding rows in this way helps to limit the number of times the 
/// heap has to be moved, greatly speeding up the writing of variable-length 
/// data.  For files that are edited with open() or clone(), ahfits will not
/// add extra rows by default.  However, separate functions exist to turn this
/// behavior on and off.
///
/// For TSTRING fields, the char* quantities required by CFITSIO are internally
/// transformed to/from std::string values.  If a char* output type is desired,
/// the TBYTE type should be used instead of TSTRING.
///
/// The buffer supports an overlap parameter.  This overlap determines how many
/// rows are buffered prior to the current row each time the buffer is filled 
/// with a read operation.  For example, if we start at row 100 in a table with
/// a buffer size of 1000 and an overlap of 10, the buffer will load rows 
/// 90 - 1089.  The overlap parameter prevents unnecessary reloading of the 
/// buffer when an algorithm requires operations on several rows at once.  The 
/// overlap parameter is provided in the constructor of the Router, so it will 
/// apply to all connections created with that router.
///

#ifndef ahfits_ahfits_buffer_h
#define ahfits_ahfits_buffer_h

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS_BUFFER,"$Id: ahfits_buffer.h,v 1.21 2014/12/15 22:28:05 mdutka Exp $")

#include "ahfits/ahfits_base.h"

/// \ingroup mod_ahfits
namespace ahfits {

/** \addtogroup mod_ahfits
 *  @{
 */

/// \brief return number of bytes needed for given data type
int sizeOfType(int typecode);

// +++ 2013-05-13 MCW should the setNulls() function live somewhere else?
/// \brief Set null (undefined) numeric values in a column of the current row
/// \param[in] ahffp The FITS file object
/// \param[in] colname name of column
/// \param[in] typecode type of data stored in buffer (i.e. output type)
/// \param[in,out] data Array of numerical data
/// \param[in] num_el Number of elements in data array
/// \param[in] dnull_flags NULL flags corresponding to each data element
void setNulls(FilePtr ahffp, const std::string& colname, int typecode,
              void * data, ahfits::IndexType num_el, char * dnull_flags);


/// \class Buffer Buffer to hold any kind of FITS table data (scalar, 
///  fixed-vector, variable-vector, any supported data type).
class Buffer {
public:

  /// \brief standard constructor to create a buffer
  /// \param[in] ahffp The AhFitsFile being buffered
  /// \param[in] colname name of column to be buffered
  /// \param[in] rwmode The mode of this buffer, i.e., read/write/both.
  /// \param[in] typecode type of data stored in buffer (i.e. output type)
  /// \param[in] keepnull true to buffer NULL flags (default: false)
  /// \param[in] numrows number of rows to buffer; -1: let cfitsio decide
  ///  optimum value; 0: disable buffering; >0 custom size
  /// \param[in] overlap number of rows to keep before current row when filling buffer
  Buffer(FilePtr ahffp, const std::string & colname, RWModeEnum rwmode, 
         int typecode, bool keep_null, long numrows, int overlap);

  /// \brief Destroy a Buffer, freeing all dynamically allocated resources.
  ~Buffer();

  /// \brief get values from buffer corresponding to current row number in the
  ///  ahfits file pointer; if value is NULL, set associated value of
  ///  dnull_flags to true (i.e. =1)
  /// \param[out] data pointer to local variable to store output value
  /// \param[out] data_count pointer to local variable to store number of
  ///             retrieved values; set to NULL if not using
  /// \param[out] dnull_flags pointer to local variable to store NULL flags;
  ///             set to NULL if not using
  void read(void* data, ahfits::IndexType* data_count, char* dnull_flags);

  /// \brief write value to buffer corresponding to current row number in the
  ///  ahfits file pointer
  /// \param[in] data pointer to local variable to store output value
  /// \param[in] data_count pointer to local variable to store number of
  ///             retrieved values; set to NULL if not using
  /// \param[in] dnull_flags pointer to local variable to store NULL flags;
  ///             set to NULL if not using
  void write(void* data, ahfits::IndexType* data_count, char* dnull_flags);

  /// \brief Clear the buffer (but do not deallocate memory).
  void clear(void);

  /// \brief Write buffer contents to FITS file, but do not clear the buffer.
  void flush(void);

  /// \brief return buffer size
  int getBufferSize(void);

private:

  /// \brief allocate buffer to given number of rows
  /// \param[in] num_rows number of rows to buffer (-1 = auto; 0 = no buffer)
  void allocate(long bufrows=-1);

  /// \brief deallocate buffer memory
  void deallocate(void);

  /// \brief populate buffer around current row number in ahffp
  void fill(void);


  FilePtr m_ahffp;        ///< AhFitsFile object associated with this buffer
  std::string m_colname;  ///< column name to be buffered
  bool m_disable;         ///< true if buffering disabled
  bool m_varcol;          ///< true if variable-length column in FITS file
  RWModeEnum m_rwmode;    ///< read/write mode
  int m_typecode;         ///< output data type
  bool m_keep_null;       ///< true if storing NULL flags

  int m_element_size;     ///< size of each element
  int m_num_per_row;      ///< number of elements per row
  long m_buf_max;         ///< maximum number of rows allowed in buffer
  int m_overlap;          ///< number of rows to buffer prior to current row

  ahfits::IndexType m_buf_first;     ///< absolute FITS row number of first element in buffer
  ahfits::IndexType m_buf_last;      ///< absolute FITS row number of last element in buffer
  ahfits::IndexType m_current_row;   ///< current row

  void* m_buffer;         ///< buffer holding data values
  char* m_nullbuf;        ///< buffer holding null values
  char* m_strbuf;         ///< to hold intermediate char* values for TSTRING
  char** m_strpointers;   ///< pointers to string start positions in m_buffer
};

// Functions follow all types.

/** @} */

} // Ends namespace ahfits

#endif // Ends the block which begins "#define ahfits_ahfits_buffer_h"

/* Revision Log
 * $Log: ahfits_buffer.h,v $
 * Revision 1.21  2014/12/15 22:28:05  mdutka
 * updated exception for issue #341 to occur as soon as the colmuns are connected
 *
 * Revision 1.20  2014/05/13 14:52:36  mwitthoe
 * ahfits: add documentation and code tweaks based on the ahfits code review; see issue 376
 *
 * Revision 1.19  2014/03/31 18:07:49  mwitthoe
 * ahfits: store temporary table row size in ahfits FilePtr instead of buffer instance; see issue 369
 *
 * Revision 1.18  2014/01/27 18:55:30  mwitthoe
 * ahfits: fix bug in the setNulls() function (ahfits_buffer) where the FITS column type was used when assigning NULL values to the buffer array when the local variable type should have been used instead; this caused strange values to appear in the buffer.  This fix does not solve all problems with this function; see issue 341
 *
 * Revision 1.17  2014/01/14 19:02:39  mwitthoe
 * ahfits: the numRows() function in ahfits_row was returning the value of m_numrow in the ahfits pointer; however when adding rows to the end of a FITS file, the buffer will add a block of rows at once making the m_numrow value unreliable; the numRows() function has been updated to get the correct row count from the Buffer instance (if present)
 *
 * Revision 1.16  2013/11/20 14:26:33  mwitthoe
 * ahfits: make corrections to Doxygen documenation suggested by Bob; see issue #289
 *
 * Revision 1.15  2013/10/30 18:01:21  mwitthoe
 * ahfits: update documentation according to Kristin's recommendations in update 3 of issue #289
 *
 * Revision 1.14  2013/10/16 18:51:32  mwitthoe
 * ahfits library: update doxygen documentation
 *
 * Revision 1.13  2013/10/16 01:40:50  mwitthoe
 * ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270
 *
 * Revision 1.12  2013/10/04 15:39:09  mwitthoe
 * ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files
 *
 * Revision 1.11  2013/07/25 19:44:17  mwitthoe
 * update doxygen tags
 *
 * Revision 1.10  2013/07/18 14:21:04  mwitthoe
 * ahfits: add optional argument to Router constructor where the overlap can be provided, the default is to have no overlap
 *
 * Revision 1.9  2013/07/17 20:40:58  mwitthoe
 * ahfits refactoring: change setNull() to use column name instead of column number; remove getColumn* functions from colinfo (they were confusing); add functions to clear/reload information from single column; remove unused statusMessage function; mark several functions as internal
 *
 * Revision 1.8  2013/07/17 17:26:56  mwitthoe
 * ahfits: add support for TDISP; refactoring related to code review (issue 266)
 *
 * Revision 1.7  2013/07/16 20:10:05  mwitthoe
 * ahfits: fix large inefficiency when writing a new file with varaible-length columns; now, when appending a file, a number of blank rows (equal to the buffer size) are added to the end of the file once the end of file is reached; extra rows are removed when the buffer closes
 *
 * Revision 1.6  2013/05/15 17:55:36  mwitthoe
 * fix typo in ahfits buffer description; CVS Log section in ahfits_buffer source was malformed causing it not to be updated with commit messages
 *
 * Revision 1.5  2013/05/15 17:34:05  mwitthoe
 * add comments/documentation to ahfits_buffer; uncomment variable-length bit column unit test
 *
 * Revision 1.4  2013/05/14 14:17:32  mwitthoe
 * ahfits buffering: add row overlapping when reading next buffer, add support for buffer size options; various debugging
 *
 * Revision 1.3  2013/05/13 19:45:51  mwitthoe
 * add buffering to ahfits (no TBIT support yet)
 *
 * Revision 1.2  2013/04/03 01:11:32  peachey
 * Correct some sort of weird check-in problem which added
 * strange whitespace.
 *
 * Revision 1.1  2013/04/03 00:48:53  peachey
 * Initial version of Buffer class.
 *
 */
