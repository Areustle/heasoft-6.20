/// \file ahfits_base.h
/// \brief ahfits: Structures and typedefs
/// \author James Peachey
/// \date $Date: 2015/06/12 18:00:07 $

/// \addtogroup mod_ahfits
/// \section ahfits_base Base Library - ahfits_base
///
/// This library defines the structs used to store FITS file and column 
/// information.  There are also general helper functions such as those to 
/// interrogate the type of the active HDU extension: bintable, image, or 
/// ASCII.  The base library underlies all other ahfits libraries.
///

#ifndef AHFITS_AHFITS_BASE_H
#define AHFITS_AHFITS_BASE_H

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS_BASE,"$Id: ahfits_base.h,v 1.30 2015/06/12 18:00:07 mwitthoe Exp $")

#include "fitsio.h"

#include <map>
#include <set>
#include <list>
#include <string>

/// \brief FITS access routines
/// \ingroup mod_ahfits
namespace ahfits {

class Router;
class Buffer;
class Connection;

/** \addtogroup mod_ahfits
 *  @{
 */

// -----------------------------------------------------------------------------
//
// The functions accessing clobber, buffer, and history states are defined at
// the top of the header so that struct definitions can access them.
//

/// \brief Set the ahfits clobber state
/// \param[in] clobber clobber state
void setClobber(bool clobber);

/// \brief Get the ahfits clobber state
bool getClobber(void);

/// \brief Set the ahfits buffer state
/// \param[in] buffer buffer state
void setBuffer(int buffer);

/// \brief Get the ahfits buffer state
int getBuffer(void);

/// \brief Return ahfits history state (true/false).
/// \return ahfits history state
bool getHistory(void);

/// \brief Set ahfits history state.
/// \param[in] history desired state (default=true)
void setHistory(bool history=true);

// -----------------------------------------------------------------------------

/// \brief enum defining read/write mode flags per column.
enum RWModeEnum {e_READONLY, e_READWRITE, e_WRITEONLY};

/// \brief enum defining HDU types
enum HDUTypeEnum {e_ANY_HDU, e_PRIMARY_HDU, e_BINARY_TBL, e_ASCII_TBL, e_IMAGE_HDU };

/// \brief Define type used by cfitsio for row numbers, and for counts
///   of elements read/written, etc.
typedef long long IndexType;


/// \brief Structure containing column information
struct ColumnInfo {
  ColumnInfo(int colnum, std::string name, std::string units, std::string disp,
             int typecode, ahfits::IndexType repeat, ahfits::IndexType width, 
             std::string tmin, std::string tmax, bool has_nullval, long long tnull):
               m_colnum(colnum), m_name(name), m_units(units), m_disp(disp),
               m_typecode(typecode), m_repeat(repeat), m_width(width),
               m_minval(tmin), m_maxval(tmax), m_has_nullval(has_nullval), 
               m_nullval(tnull) {}

  int m_colnum;                ///< column index
  std::string m_name;          ///< column label
  std::string m_units;         ///< column units
  std::string m_disp;          ///< column display format
  int m_typecode;              ///< type of column defined by cfitsio (TINT, TDOUBLE, etc); negative for variable column
  ahfits::IndexType m_repeat;  ///< number of elements in binary table.
  ahfits::IndexType m_width;   ///< width of each element in binary table.
  std::string m_minval;             ///< smallest allowed value of column (TLMIN or TDMIN)
  std::string m_maxval;             ///< largest allowed value of column (TLMAX or TDMAX); m_maxval < m_minval if no range specified
  bool m_has_nullval;          ///< is the nullval to be used?
  long long m_nullval;         ///< special value encoding an "undefined" or "no data" condition for integer types
};


/// \brief Structure to hold information about interface with single HDU of a
///  FITS file: column information, connections, etc.
struct AhFitsFile {
  typedef std::set<Router *> RouterContType;
  typedef RouterContType::iterator RouterIteratorType;

  AhFitsFile(const std::string & filename): m_filename(filename), m_cfitsfp(0), 
             m_readonly(false), m_hduidx(0), m_numrow(0), m_currow(1), 
             m_stamppar(ahfits::getHistory()), m_numrow_with_padding(0),
             m_add_extrarows_when_buffering(false) {}

  std::string m_filename;                  ///< name of loaded FITS file
  fitsfile * m_cfitsfp;                    ///< cfitsio FITS file pointer
  bool m_readonly;                         ///< mark file as readonly
  int m_hduidx;                            ///< current HDU index (0=not set)
  ahfits::IndexType m_numrow;              ///< number of rows in active HDU (excluding any temporary rows adding during buffering; see numrow_with_padding)
  ahfits::IndexType m_currow;              ///< number of current row
  bool m_stamppar;                         ///< true if stamping parameters
  std::set<int> m_stamped;                 ///< set of HDU indices wher parameters have been stamped
  std::map<std::string,int> m_name2num;    ///< map of column name to column number
  std::map<int,std::string> m_num2name;    ///< map of column number to column name
  std::map<int,ColumnInfo*> m_colinfo;     ///< map of column number to column info
  RouterContType m_router;                 ///< list of routers that rout to this file

  ahfits::IndexType m_numrow_with_padding; ///< number of rows in table (data + padding added by Buffer)
  bool m_add_extrarows_when_buffering;     ///< for effeciency, allow many rows to be added at once to FITS file when writing a new file  

};

/// \brief Typedef to reduce the number of levels of pointer, to
/// clarify/simplify what is going on in the code.
typedef AhFitsFile * FilePtr;

/// \brief list of strings containing column names
typedef std::list<std::string> ColumnList;

/// \brief Return true if HDU is a binary table.
/// \param[in] ahffp The FITS file object.
/// \internal
/// \note has not been tested for false
bool isBintable(FilePtr ahffp);

/// \brief Return true if HDU is an image.
/// \param[in] ahffp The FITS file object.
/// \internal
/// \note has not been tested for true
bool isImage(FilePtr ahffp);

/// \brief Return true if HDU is an ASCII table.
/// \param[in] ahffp The FITS file object.
/// \internal
/// \note has not been tested for true
bool isASCII(FilePtr ahffp);

/// \brief Return true if in Primary HDU.
/// \param[in] ahffp The FITS file object.
bool isPrimary(FilePtr ahffp);

/// \brief Return cfitsio message associated with given status code
/// \param[in] status status code
std::string statusMsg(int status);

/// \brief Provide a standard prefix for ahfits error messages which include
///  the file name, extension, and (optionally) row number
/// \param[in] ahffp The FITS file object.
/// \param[in] includeHDU true if HDU is included in prefix (default: true)
/// \param[in] includeRow true if row number is included in prefix (default: true)
std::string errPrefix(FilePtr ahffp, bool includeHDU=true, bool includeRow=true);

/// \brief allow parameters to be stamped to FITS files when changed
/// \param[in] ahffp The FITS file object.
void enableParameterStamping(ahfits::FilePtr ahffp);

/// \brief disallow parameters to be stamped to FITS files when changed
/// \param[in] ahffp The FITS file object.
void disableParameterStamping(ahfits::FilePtr ahffp);

/// \brief set parameter stamping to the given state
/// \param[in] ahffp The FITS file object.
/// \param[in] state true/false
void setParameterStamping(ahfits::FilePtr ahffp, bool state);

/// \brief return true if parameter stamping enabled
/// \param[in] ahffp The FITS file object.
bool isParameterStamping(ahfits::FilePtr ahffp);

/// \brief convert ahfits enumerated value for HDU type (e.g. e_BINARYTBL) into
///  the cfitsio enumerated value (e.g. BINARY_TBL).
/// \param[in] hdutype ahfits enumerated value
/// \return cfitsio enumerated value
int convertAhfitsToCfitsioHDUType(int hdutype);

/// \brief create a string containing the file name and active extension of
///  the given FITS file pointer
/// \param[in] ahffp The FITS file object.
std::string getFileAndHDUString(ahfits::FilePtr ahffp);

/// \brief When writing to FITS file, allow extra rows to be added to file
///  to ensure buffer is always filled.  This should only be needed in special
///  circumstances.  See performance section of ahfits documentation.  
/// \param[in] ahffp The FITS file object.
void enableBufferPadding(ahfits::FilePtr ahffp);

/// \brief when writing to FITS file, disallow extra rows to be added to file
///  to ensure buffer is always filled.  This should only be needed in special
///  circumstances.  See performance section of ahfits documentation.  
/// \param[in] ahffp The FITS file object.
void disableBufferPadding(ahfits::FilePtr ahffp);


/** @} */

} // namespace ahfits

#endif   /* AHFITS_AHFITS_BASE_H */

/* Revision Log
   $Log: ahfits_base.h,v $
   Revision 1.30  2015/06/12 18:00:07  mwitthoe
   ahfits: add two functions to get/set parameter stamping state

   Revision 1.29  2014/11/26 15:12:44  mwitthoe
   ahfits: add clobber, buffer, and history states to library; library now accesses these states instead of those in ahgen; see issue 437

   Revision 1.28  2014/10/07 14:38:58  mdutka
   TLMIN and TLMAX column attributed are stored as strings, TDMIN/TDMAX are ignored see redmine issue #418 for more details

   Revision 1.27  2014/04/04 17:05:07  mwitthoe
   ahfits: discovered two functions with similar capability: isIntegerTypeColumn in ahfits_base and isInteger in ahfits_colinfo; I like the former's name, but the latters implementation, so removed the first and renamed the second; changed all appropriate calls to these functions which were restricted within the ahfits library; the ahfits unit test and all tool unit tests still pass

   Revision 1.26  2014/04/02 21:08:03  mwitthoe
   ahfits: add documentation for performance relating to buffering and variable-length columns; see issue 368

   Revision 1.25  2014/04/01 14:57:05  mwitthoe
   ahfits: add member to ahfits FilePtr indicating whether extra rows are added to the end of the FITS file when buffering; see issue 368

   Revision 1.24  2014/03/31 18:07:49  mwitthoe
   ahfits: store temporary table row size in ahfits FilePtr instead of buffer instance; see issue 369

   Revision 1.23  2013/12/10 20:32:46  mwitthoe
   ahfits bug fixes: the create() function needed to add necessary keywords to the Primary HDU (now accomplished with fits_create_img); the old version was okay if immediately creating a new table after ahfits::create(), but created an incomplete FITS file if the newly created file was closed before any other action; the addHDU() function did not work if the active HDU was the primary HDU

   Revision 1.22  2013/10/16 18:51:32  mwitthoe
   ahfits library: update doxygen documentation

   Revision 1.21  2013/10/16 01:40:50  mwitthoe
   ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

   Revision 1.20  2013/10/04 15:39:09  mwitthoe
   ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

   Revision 1.19  2013/09/10 14:10:35  mwitthoe
   ahfits library: begin to add support for primary HDU access; add enumerated type to identify type of extension (e.g. e_BINARY_TBL) and a function to convert from this enumeration to the equivalent cfitsio enumeration (e.g. BINARY_TBL); add optional argument to nextHDU() specifying the desired HDU type to go to (default: e_ANY_HDU); add unit tests to check that new argument acts appropriately

   Revision 1.18  2013/07/17 17:26:56  mwitthoe
   ahfits: add support for TDISP; refactoring related to code review (issue 266)

   Revision 1.17  2013/07/12 13:30:33  mwitthoe
   ahfits: write function to provide standard error message prefix; use prefix throughout ahfits library

   Revision 1.16  2013/07/11 17:07:20  mwitthoe
   ahfits: add hdu index as a member of FilePtr in order to eliminate access to the disk in the stamp() function; stamp() is potentially called with every writeRow() call and, with buffering, the whole idea is not to access the file all the time.

   Revision 1.15  2013/07/10 18:25:57  mwitthoe
   ahfits: add flag to FilePtr structure indicating if parametes should be written to the FITS header for modified files; the default value is set by the current global history state in ahgen; functions are added to ahfits to change the flag for an individual FITS file

   Revision 1.14  2013/07/08 18:25:38  mwitthoe
   ahfits library: add setTNull() which will write the TNULLL keyword for integer-like columns; when making router connection with local NULL flags, require target, integer-like columns to have the TNULL keyword defined

   Revision 1.13  2013/05/13 19:45:51  mwitthoe
   add buffering to ahfits (no TBIT support yet)

   Revision 1.12  2013/03/09 03:59:30  peachey
   Add a container of Router objects to the AhFitsFile struct, so that
   it can easily track its routers and connections.

   Revision 1.11  2013/02/28 21:59:19  rshill
   Added TNULL and NaN capability for flagging undefined values.

   Revision 1.10  2013/01/24 15:56:28  mwitthoe
   update afits Doxygen

   Revision 1.9  2012/12/11 18:52:50  mwitthoe
   add new function, searchColumn(), to ahfits which will search for a column name matching the given search string (containing wildcards)

   Revision 1.8  2012/11/26 21:22:05  mwitthoe
   add brief descriptions to namespaces in gen

   Revision 1.7  2012/11/05 01:35:50  mwitthoe
   add m_readonly member to AhFitsFile struct to mark a FITS file as read-only; all writing routines (row and headers) now check the readonly status before writing, but there is no way currently to set the read-only false to true (except as the backup readonly fits_open_file call in ahfits::open

   Revision 1.6  2012/11/04 23:25:19  mwitthoe
   ahfits: add cfitsio error messages based on error status; made clone/create functions that return a fits pointer to the newly created FITS file

   Revision 1.5  2012/10/17 02:38:22  mwitthoe
   add Doxygen comments to each ahfits header which will be output to the ahfits module

   Revision 1.4  2012/10/11 21:23:50  mwitthoe
   add min/max column values to ahfits_colinfo

   Revision 1.3  2012/10/11 17:56:55  mwitthoe
   include parameter stamping in ahfits

   Revision 1.2  2012/10/01 17:21:56  mwitthoe
   new version of ahfits with connections stored outside of FITS file object

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/
