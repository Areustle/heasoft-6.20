/// \file ahfits_file.h
/// \brief ahfits: FITS file operations and HDU navigation
/// \author James Peachey
/// \date $Date: 2015/10/09 21:09:21 $

/// \addtogroup mod_ahfits
/// \section ahfits_file File Access Library - ahfits_file
///
/// This library is in charge of general file operations such as opening, 
/// closing, and creating FITS files.  Functions for navigating the HDU 
/// extensions are also contained here.
///

#ifndef AHFITS_AHFITS_FILE_H
#define AHFITS_AHFITS_FILE_H

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS_FILE,"$Id: ahfits_file.h,v 1.35 2015/10/09 21:09:21 mdutka Exp $")

#include "ahfits/ahfits_base.h"

#include <string>

/// \ingroup mod_ahfits
namespace ahfits {


/** \addtogroup mod_ahfits
 *  @{
 */

/// \brief Open the fits file(s) referenced by the filename, and move to the
///   given extension. Set up all internal buffering.
/// \param[in] filename The name of the file, (or @file_list for text file
///   listing multiple input files).
/// \param[in] extension The name of the extension in the file required.  An
///   empty string ("") will default to the 2nd extension (after Primary).
/// \param[out] ahffp The address of the output FITS file pointer that will
///   contain all the data needed to access the file(s).
///
/// If the given extension name is an empty string, then the file will be
/// opened to the primary HDU, unless extended syntax is included in the 
/// filename.  If not an empty string, the extname argument has priority
/// over any extension specified by the extended syntax.
///
/// Note: this function will ignore any preceding bang (!) in the filename.
///
/// Note: See the performance section of the ahfits documentation for
/// performance issues regarding buffering.
void open(const std::string & filename, const std::string & extname,
          FilePtr * ahffp);

/// \brief Create the fits file(s) referenced by the filename, using the
///   provided FITS template. Set up all internal buffering.
/// \param[in] filename The name of the file, (or @file_list for text file
///   listing multiple output files).
/// \param[in] fits_template The name of the template file to use to create the
///   FITS file.
/// \param[out] ahffp The address of the output FITS file pointer that will
///   contain all the data needed to access the newly created file(s).
///
/// See the performance section of the ahfits documentation for performance
/// issues regarding buffering.
void create(const std::string & filename, const std::string & fits_template, 
            FilePtr * ahffp);

/// \brief Copy all extensions of a FITS file to a new file.
/// \param[in] srcfile filename to copy
/// \param[in] destfile name of new file
/// \param[out] ahffp address of output FITS file pointer opened to 
///  destination file
/// \param[in] overwriteOnClobber (optional) open srcfile if same as destfile
///  and clobber=yes
///
/// When clone returns, ahffp is opened to the Primary HDU unless extended
/// syntax is included in srcfile.  All extensions will be copied regardless
/// of extended syntax being present.
///
/// Note: See the performance section of the ahfits documentation for
/// performance issues regarding buffering.
void clone(const std::string & srcfile, const std::string & destfile, 
           FilePtr * ahffp, bool overwriteOnClobber=false);

/// \brief Copy primary header and single extension of FITS file.
/// \param[in] src filename to copy
/// \param[in] dest name of new file
/// \param[in] extname name of extension (HDU) to copy 
/// \param[out] ahffp FITS file pointer, on output: opened to destination file
/// \param[in] overwriteOnClobber (optional) open srcfile if same as destfile
///  and clobber=yes
///
/// See the performance section of the ahfits documentation for performance
/// issues regarding buffering.
void cloneSingleHDU(const std::string & src, const std::string & dest, 
                    const std::string & extname, FilePtr & ahffp, 
                    bool overwriteOnClobber=false);

/// \brief Check for an HDU of the given name.
/// \param[in] ahffp_src FITS file pointer of input file
/// \param[in] extname name of HDU to look for
/// \return true if HDU exists in file, false otherwise
bool HDUExists(FilePtr & ahffp, const std::string & extname);

/// \brief Create new HDU in destination file with the name hdudest based on
///  the HDU, hdusrc, in the source file.  The source and destination files
///  can be the same only if the new HDU name is different than hdusrc.
/// \param[in] ahffp_src FITS file pointer of input file
/// \param[in] extsrc name of source HDU
/// \param[in] ahffp_dest FITS file pointer of output file
/// \param[in] extdest name of new HDU
void addHDU(FilePtr & ahffp_src, const std::string & extsrc, 
            FilePtr & ahffp_dest, const std::string & extdest);


/// \brief Create empty BINTABLE extension in destination file.
/// \param[in] ahffp FITS file pointer of output file
/// \param[in] extname name of new HDU
void addEmptyTbl(FilePtr & ahffp, const std::string & extname);

/// \brief Move to the given extension in the FITS file object.
/// \param[in] ahffp The FITS file pointer.
/// \param[in] extname The name of the extension in the file required. An
///   empty string ("") will default to the Primary HDU.
void move(FilePtr ahffp, const std::string & extname);

/// \brief Move to the given extension in the FITS file object.
/// \param[in] ahffp The FITS file pointer.
/// \param[in] hdunum The number of extension to move to (HDU numbering starts 
///   with 1 for the primary array)
void move(FilePtr ahffp, int hdunum);

/// \brief Move to first HDU of a certain type in FITS file object.
/// \param[in] ahffp The FITS file pointer.
/// \param[in] hdutype type of HDU to move to (default: e_ANY_HDU)
/// \return success of move
bool firstHDU(FilePtr ahffp, HDUTypeEnum hdutype=e_ANY_HDU);

/// \brief Move to next extension in FITS file object.
/// \param[in] ahffp The FITS file pointer.
/// \param[in] hdutype type of HDU to move to (default: e_ANY_HDU)
/// \return success of move
bool nextHDU(FilePtr ahffp, HDUTypeEnum hdutype=e_ANY_HDU);

/// \brief Close the given file object. Flush output, update checksum and
///   close the affected FITS files, free internal buffers.
/// \param[in] ahffp The FITS file pointer to be closed.
void close(FilePtr & ahffp);

/// \brief Insert a new column before the given reference column name
/// \param[in] ahffp FITS file pointer
/// \param[in] colname name of new column
/// \param[in] format format code of new column, e.g. 'D' for double 
///            (see http://heasarc.gsfc.nasa.gov/fitsio/c/c_user/node80.html)
/// \param[in] refcol reference column name; empty string for first column 
///            (default)
void insertColBefore(FilePtr ahffp, const std::string & colname, 
                     const std::string & format, const std::string & refcol="");

/// \brief Insert a new column after the given reference column name
/// \param[in] ahffp FITS file pointer
/// \param[in] colname name of new column
/// \param[in] format format code of new column, e.g. 'D' for double 
///            (see http://heasarc.gsfc.nasa.gov/fitsio/c/c_user/node80.html)
/// \param[in] refcol reference column name; empty string for last column 
///            (default)
void insertColAfter(FilePtr ahffp, const std::string & colname, 
                    const std::string & format, const std::string & refcol="");

/// \brief (internal) Insert a new column at given column number
/// \param[in] ahffp FITS file pointer
/// \param[in] colname name of new column
/// \param[in] format format code of new column, e.g. 'D' for double 
///            (see http://heasarc.gsfc.nasa.gov/fitsio/c/c_user/node80.html)
/// \param[in] colnum index of new column
void insertColAt(FilePtr ahffp, const std::string & colname, 
                 const std::string & format, int colnum);

/// \brief (internal) Update all information in FilePtr: column information and
///  number of rows.
/// \param[in] ahffp FITS file pointer
void updateFilePtr(FilePtr ahffp);

/// \brief (internal) Flush and reset all buffers associated with FilePtr to 
///  initialized state.
/// \param[in] ahffp FITS file pointer
void flushAndClearBuffers(FilePtr ahffp);

/// \brief (internal) Read NAXIS2 from header of active HDU of FITS file and
///  update the number of rows
/// \param[in] ahffp FITS file pointer
void updateNumberOfRows(FilePtr ahffp);

/// \brief (internal) Insert given number of rows to end of file; all rows are
///  blank and do not count towards the final row count unless/until data is
///  is written to them.
/// \param[in] ahffp FITS file pointer
/// \param[in] n number of rows to add
void addExtraRows(FilePtr ahffp, ahfits::IndexType n);

/// \brief (internal) Remove temporary rows from end of FITS file added in
///  the buffering process
/// \param[in] ahffp FITS file pointer
void removeExtraRows(FilePtr ahffp);

/// \brief Removes all rows in an extension
/// \param[out] ahffp FITS file pointer
void removeAllRows(FilePtr ahffp);

/** @} */

} // namespace ahfits

#endif   /* AHFITS_AHFITS_FILE_H */

/* Revision Log
   $Log: ahfits_file.h,v $
   Revision 1.35  2015/10/09 21:09:21  mdutka
   adding new function removeAllRows to ahfits file

   Revision 1.34  2015/10/02 18:09:22  klrutkow
   added line to move() documentation

   Revision 1.33  2015/06/29 14:40:33  mwitthoe
   ahfits: add new function (ahfits::move) to allow moving to a FITS extension by number; the new function is an overload of ahfits::move where the argument is the extension name

   Revision 1.32  2014/08/20 21:12:32  mwitthoe
   ahfits: update function descriptions of open and clone to describe itheir interaction with extended syntax; update the dox file for ahfits to describe extended syntax support

   Revision 1.31  2014/05/13 14:52:36  mwitthoe
   ahfits: add documentation and code tweaks based on the ahfits code review; see issue 376

   Revision 1.30  2014/04/02 21:08:03  mwitthoe
   ahfits: add documentation for performance relating to buffering and variable-length columns; see issue 368

   Revision 1.29  2014/03/31 18:07:49  mwitthoe
   ahfits: store temporary table row size in ahfits FilePtr instead of buffer instance; see issue 369

   Revision 1.28  2014/01/06 16:32:41  asargent
   Added HDUExists function to check whether HDU extension exists

   Revision 1.27  2013/10/16 18:51:32  mwitthoe
   ahfits library: update doxygen documentation

   Revision 1.26  2013/10/16 01:40:50  mwitthoe
   ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

   Revision 1.25  2013/09/16 20:12:06  klrutkow
   changed meaning of empty string to go to Primary HDU, in move() and open(), instead of 1st HDU, updated doxygen

   Revision 1.24  2013/09/10 20:41:32  klrutkow
   added firstHDU function, and tests

   Revision 1.23  2013/09/10 14:10:35  mwitthoe
   ahfits library: begin to add support for primary HDU access; add enumerated type to identify type of extension (e.g. e_BINARY_TBL) and a function to convert from this enumeration to the equivalent cfitsio enumeration (e.g. BINARY_TBL); add optional argument to nextHDU() specifying the desired HDU type to go to (default: e_ANY_HDU); add unit tests to check that new argument acts appropriately

   Revision 1.22  2013/07/17 21:38:02  mwitthoe
   ahfits: remove duplicated code from insertCol* functions by adding a function insertColumnAt; change buffer OVERlAP constant to 0

   Revision 1.21  2013/07/17 20:40:58  mwitthoe
   ahfits refactoring: change setNull() to use column name instead of column number; remove getColumn* functions from colinfo (they were confusing); add functions to clear/reload information from single column; remove unused statusMessage function; mark several functions as internal

   Revision 1.20  2013/07/17 17:26:56  mwitthoe
   ahfits: add support for TDISP; refactoring related to code review (issue 266)

   Revision 1.19  2013/07/16 20:10:06  mwitthoe
   ahfits: fix large inefficiency when writing a new file with varaible-length columns; now, when appending a file, a number of blank rows (equal to the buffer size) are added to the end of the file once the end of file is reached; extra rows are removed when the buffer closes

   Revision 1.18  2013/07/12 22:16:26  mwitthoe
   ahfits buffering: fix bug where buffer was not properly flushed and reset when moving to a new extension but keeping router connections active

   Revision 1.17  2013/07/09 17:42:54  rshill
   Added functionality to build tables from scratch.

   Revision 1.16  2013/07/01 15:13:52  rshill
   Deleted deprecated routines (which returned FilePtr).

   Revision 1.15  2013/03/09 04:19:53  peachey
   Add updateConnections functions.

   Revision 1.14  2013/01/11 19:40:53  mwitthoe
   move insert column functions from colinfo to file; add function to update column indices in routers after insertion of a column

   Revision 1.13  2012/12/10 19:21:19  mwitthoe
   ahfits: added obsolete warnings to open/create/clone functions returning a FilePtr; made FilePtr argument for clone consistent with open/create; changed ahfits unit tests to use non-obsolete open/create/clone functions

   Revision 1.12  2012/12/07 23:00:11  mwitthoe
   make changes to ahfits recommended by Dec 7 code review (see http://zuul:8080/redmine/projects/astroh/wiki/2012-12-07_Ahfits_Code_Review)

   Revision 1.11  2012/11/13 18:17:00  mwitthoe
   add open/create/clone functions in ahfits which return a FilePtr instead of passing it by reference; the old functions are still present, but slated for removal

   Revision 1.10  2012/11/04 23:25:19  mwitthoe
   ahfits: add cfitsio error messages based on error status; made clone/create functions that return a fits pointer to the newly created FITS file

   Revision 1.9  2012/11/02 17:13:05  mwitthoe
   ahfits: cleanp up how clobber is handled in open/create/clone cunctions

   Revision 1.8  2012/11/01 20:49:37  mwitthoe
   ahfits: clobber is not handled internally in create/clone functions; open() ignores bang in filename; open/create now use strings as arguments

   Revision 1.7  2012/10/24 17:02:59  mwitthoe
   changed ahfits::addHDU() to work on file names instead of open FITS pointers

   Revision 1.6  2012/10/24 15:00:06  mwitthoe
   ahfits: move stamp() from ahfits_file to ahfits_header; new function addHDU in ahfits_file which will make a new (empty) extension based on the header of an existing extension from the same or different file

   Revision 1.5  2012/10/17 02:38:22  mwitthoe
   add Doxygen comments to each ahfits header which will be output to the ahfits module

   Revision 1.4  2012/10/11 17:56:56  mwitthoe
   include parameter stamping in ahfits

   Revision 1.3  2012/10/10 20:30:56  mwitthoe
   ahfits: complete overloaded connection functions; add test code

   Revision 1.2  2012/10/01 17:21:56  mwitthoe
   new version of ahfits with connections stored outside of FITS file object

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/
