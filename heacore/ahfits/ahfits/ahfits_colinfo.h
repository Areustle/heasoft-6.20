/// \file ahfits_colinfo.h
/// \brief ahfits: Function to load/retrieve column information
/// \author James Peachey
/// \date $Date: 2015/05/18 14:30:50 $

/// \addtogroup mod_ahfits
/// \section ahfits_colinfo Column Information Library - ahfits_colinfo
///
/// This library contains functions to retrieve column definitions as listed in 
/// the header.  When information is requested for a particular column, all
/// header information about that column is loaded into memory so that
/// subsequent calls do not require the FITS file to be accessed.
///

#ifndef AHFITS_AHFITS_COLINFO_H
#define AHFITS_AHFITS_COLINFO_H

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS_COLINFO,"$Id: ahfits_colinfo.h,v 1.27 2015/05/18 14:30:50 asargent Exp $")

#include "ahfits/ahfits_base.h"

#include "fitsio.h"

#include <string>
#include <list>

/// \ingroup mod_ahfits
namespace ahfits {


/** \addtogroup mod_ahfits
 *  @{
 */

/// \brief get column typecode; load column information if needed
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
int columnType(FilePtr ahffp, const std::string & name);

/// \brief get column units; load column information if needed
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
std::string columnUnits(FilePtr ahffp, const std::string & name);

/// \brief get column repeat; load column information if needed
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
/// \returns repeat, can be 0 if invalid or empty variable width column
ahfits::IndexType columnRepeat(FilePtr ahffp, const std::string & name);

/// \brief get column width; load column information if needed
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
ahfits::IndexType columnWidth(FilePtr ahffp, const std::string & name);

/// \brief (internal) get column null (undefined) value; 
///   load column information if needed
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
/// \param[out] tnull null value (integer columns only)
bool columnNull(FilePtr ahffp, const std::string & name, long long & tnull);

/// \brief get column display format; load column information if needed
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
/// \return display format string
std::string columnDisplay(FilePtr ahffp, const std::string & name);

/// \brief return the minimum and maximum values for the given column; if maxval is
///  smaller than minval, then no range is specified (in which case the function 
///  returns false); load column information if needed
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
/// \param[out] minval minimum value
/// \param[out] maxval maximum value
/// \return true if range exists
bool columnRange(FilePtr ahffp,  const std::string & name, 
                 std::string & minval, std::string & maxval);


/// \brief return the minimum and maximum values for the given column; if 
///        maxval is smaller than minval, then no range is specified (in 
///        which case the function returns false); load column information 
///        if needed
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
/// \param[out] minval minimum value
/// \param[out] maxval maximum value
/// \return true if range exists
bool columnRange(FilePtr ahffp, const std::string & name, long long & minval, 
                 long long & maxval);


/// \brief return the minimum and maximum values for the given column; if maxval is
///  smaller than minval, then no range is specified (in which case the function 
///  returns false); load column information if needed
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
/// \param[out] minval minimum value
/// \param[out] maxval maximum value
/// \return true if range exists
bool columnRange(FilePtr ahffp, const std::string & name, double & minval, 
                 double & maxval);


/// \brief return length of a variable length column; load column information if
///  needed
/// \param[in] ahffp The FITS file object.
/// \param[in] colnum column number
///
/// This function will throw an exception if used on a column containing
/// scalar quantities.  For string columns, the length is the repeat value
/// divided by the width; otherwise the length is just the repeat value.
ahfits::IndexType columnLength(FilePtr ahffp, const std::string & name);

/// \brief check if column exists
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
bool haveColumn(FilePtr ahffp, const std::string & name);

/// \brief check if a numerical column is integer type
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
bool isIntegerTypeColumn(FilePtr ahffp, const std::string & name);

/// \brief check if a numerical column is floating type
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
bool isFloatTypeColumn(FilePtr ahffp, const std::string & name);


/// \brief (internal) read column information and store in column info map
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
void loadColInfo(FilePtr ahffp, const std::string & name);

/// \brief (internal) clear all column information
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
void clearColInfo(FilePtr ahffp, const std::string& name);

/// \brief (internal) clear all column information
/// \param[in] ahffp The FITS file object.
void clearAllColInfo(FilePtr ahffp);

/// \brief (internal) reload information for single column
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
void reloadColInfo(FilePtr ahffp, const std::string& name);

/// \brief (internal) reload all column information
/// \param[in] ahffp The FITS file object.
void reloadAllColInfo(FilePtr ahffp);

/// \brief (internal) return column index given the column label
/// \param[in] ahffp The FITS file object.
/// \param[in] name column label
int name2Num(FilePtr ahffp, const std::string & name);

/// \brief (internal) return column label given the column index
/// \param[in] ahffp The FITS file object.
/// \param[in] name column number
std::string num2Name(FilePtr ahffp, int colnum);

/// \brief (internal) search for column names matching given name fragment
/// \param[in] ahffp The FITS file object.
/// \param[in] srch column name fragment (may contain wildcards: *, ?, #)
/// \param[out] collist list of matching column names
/// \param[in] casesen true to make search case sensitive (default: false)
/// \return number of matches
int searchColumn(FilePtr ahffp, const std::string & srch, ColumnList & collist, 
                 bool casesen=false);

/// \brief (internal) form column attribute keyword name, e.g. TUNIT4
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] prefix attribute label prefix, e.g. TUNIT
/// \param[out] out output keyword label
void formColumnAttribute(FilePtr ahffp, const std::string& name, 
                         const std::string& prefix, std::string& out);

/// \brief Set a column-specific keyword as a string.
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] keyword keyword template (e.g. TNULL)
/// \param[in] value to assign
/// \param[in] chkintcol require that column is an integer-type column
///
/// This function will set a column-specific keyword (e.g. TNULL) for the
/// column with the given name.  The function needs to locate the index of
/// the column and then assign the keyword value.  Some keywords are only
/// meaningful for integer-type columns (e.g. J type); the parameter chkintcol
/// will perform this check before writing the keyword (an error is thrown
/// if the check fails).
void setColumnAttribute(FilePtr ahffp, const std::string &name,
                        const std::string& keyword, const std::string& value,
                        bool chkintcol);

/// \brief Set a column-specific keyword as a long long.
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] keyword keyword template (e.g. TNULL)
/// \param[in] value to assign
/// \param[in] chkintcol require that column is an integer-type column
///
/// This function will set a column-specific keyword (e.g. TNULL) for the
/// column with the given name.  The function needs to locate the index of
/// the column and then assign the keyword value.  Some keywords are only
/// meaningful for integer-type columns (e.g. J type); the parameter chkintcol
/// will perform this check before writing the keyword (an error is thrown
/// if the check fails).
void setColumnAttribute(FilePtr ahffp, const std::string &name,
                        const std::string& keyword, long long value,
                        bool chkintcol);

/// \brief Set TNULL keyword for given column name of integer-type
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] nullval value to use for NULL
void setTNull(FilePtr ahffp, const std::string &name, long long nullval);

/// \brief Set TUNIT keyword for given column name
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] unit unit string
void setTUnit(FilePtr ahffp, const std::string &name, const std::string& unit);

/// \brief Set TDisp keyword for given column name
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] form display format
void setTDisp(FilePtr ahffp, const std::string &name, const std::string& disp);

/// \brief Set TZERO keyword for given column name
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] zero value of keyword
void setTZero(FilePtr ahffp, const std::string &name, long long zero);

/// \brief Set TSCALE keyword for given column name
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] scale value of keyword
void setTScale(FilePtr ahffp, const std::string &name, long long scale);

/// \brief Set TLMIN keyword for given column name
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] minval value of keyword
void setTLmin(FilePtr ahffp, const std::string &name, long long minval);

/// \brief Set TLMAX keyword for given column name
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] maxval value of keyword
void setTLmax(FilePtr ahffp, const std::string &name, long long maxval);

/// \brief Set TTYPE keyword description for given column name
/// \param[in] ahffp The FITS file object.
/// \param[in] name column name
/// \param[in] desc column description 
void setColumnDescription(FilePtr ahffp, const std::string &name, const std::string& desc);

/** @} */


} // namespace ahfits

#endif   /* AHFITS_AHFITS_COLINFO_H */

/* Revision Log
   $Log: ahfits_colinfo.h,v $
   Revision 1.27  2015/05/18 14:30:50  asargent
   Added modifyKeyComment and setColumnDescription functions

   Revision 1.26  2014/11/03 21:38:29  mwitthoe
   ahfits: change names of setTlmin() & setTlmax() to setTLmin() & setTLmax to make them consistent with other functions

   Revision 1.25  2014/11/03 21:30:55  mwitthoe
   ahfits: add functions to set TLMIN/TLMAX for a column; refactored functions setting column attributes to reduce code duplication; related to issue 459

   Revision 1.24  2014/11/03 20:48:21  mwitthoe
   ahfits: add functions, setTZero() & setTScale(), in ahfits_colinfo for setting additional column properties; related to issue 459

   Revision 1.23  2014/10/07 14:38:58  mdutka
   TLMIN and TLMAX column attributed are stored as strings, TDMIN/TDMAX are ignored see redmine issue #418 for more details

   Revision 1.22  2014/04/04 17:05:07  mwitthoe
   ahfits: discovered two functions with similar capability: isIntegerTypeColumn in ahfits_base and isInteger in ahfits_colinfo; I like the former's name, but the latters implementation, so removed the first and renamed the second; changed all appropriate calls to these functions which were restricted within the ahfits library; the ahfits unit test and all tool unit tests still pass

   Revision 1.21  2013/11/20 14:26:34  mwitthoe
   ahfits: make corrections to Doxygen documenation suggested by Bob; see issue #289

   Revision 1.20  2013/10/16 18:51:32  mwitthoe
   ahfits library: update doxygen documentation

   Revision 1.19  2013/10/16 01:40:50  mwitthoe
   ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

   Revision 1.18  2013/10/16 01:14:30  mwitthoe
   ahfits library: add isBintable() calls to restrict certain functions to binary tables, mainly column information routines and the router; remove the function, column(), from ahfits_colinfo since it was only being used by the function, columnType()

   Revision 1.17  2013/10/15 17:49:00  peachey
   (On behalf of Andy Sargent): correct loadColInfo and columnRepeat
   functions so that they accurately report the maximum repeat
   count of variable-width columns.

   Revision 1.16  2013/07/17 20:40:58  mwitthoe
   ahfits refactoring: change setNull() to use column name instead of column number; remove getColumn* functions from colinfo (they were confusing); add functions to clear/reload information from single column; remove unused statusMessage function; mark several functions as internal

   Revision 1.15  2013/07/17 17:26:56  mwitthoe
   ahfits: add support for TDISP; refactoring related to code review (issue 266)

   Revision 1.14  2013/07/10 02:11:22  mwitthoe
   ahfits: add function to set the TUNIT value for a column: setTUnit()

   Revision 1.13  2013/07/08 18:25:38  mwitthoe
   ahfits library: add setTNull() which will write the TNULLL keyword for integer-like columns; when making router connection with local NULL flags, require target, integer-like columns to have the TNULL keyword defined

   Revision 1.12  2013/03/01 23:04:37  rshill
   Small changes extrapolated from code review by JP, RSH.

   Revision 1.11  2013/02/28 21:59:19  rshill
   Added TNULL and NaN capability for flagging undefined values.

   Revision 1.10  2013/01/24 15:56:28  mwitthoe
   update afits Doxygen

   Revision 1.9  2013/01/11 19:40:52  mwitthoe
   move insert column functions from colinfo to file; add function to update column indices in routers after insertion of a column

   Revision 1.8  2013/01/09 22:05:59  mwitthoe
   add insert column functions to ahfits: insertColBefore() and insertColAfter(); also add function, reloadColInfo(), needed to reset loaded column indices after adding a column

   Revision 1.7  2012/12/11 18:52:50  mwitthoe
   add new function, searchColumn(), to ahfits which will search for a column name matching the given search string (containing wildcards)

   Revision 1.6  2012/12/07 23:00:11  mwitthoe
   make changes to ahfits recommended by Dec 7 code review (see http://zuul:8080/redmine/projects/astroh/wiki/2012-12-07_Ahfits_Code_Review)

   Revision 1.5  2012/11/07 20:10:17  mwitthoe
   ahfits: add function, num2Name, in ahfits_colinfo which returns the column name given the column index; this function is used in ahfits::writeRow() to provide a more complete error message

   Revision 1.4  2012/10/17 02:38:22  mwitthoe
   add Doxygen comments to each ahfits header which will be output to the ahfits module

   Revision 1.3  2012/10/12 22:50:00  mwitthoe
   ahfits: use pass-by-reference in scalar connections; add columnLength() function

   Revision 1.2  2012/10/11 21:23:50  mwitthoe
   add min/max column values to ahfits_colinfo

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/
