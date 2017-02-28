/// \file ahfits_header.h
/// \brief ahfits: Read/write header values
/// \author James Peachey
/// \date $Date: 2015/07/29 01:10:12 $

/// \addtogroup mod_ahfits
/// \section ahfits_header Header Library - ahfits_header
///
/// This library contains functions to read and write header keywords of the
/// active extension.  Functions exist for each value type, e.g. double.
///

#ifndef AHFITS_AHFITS_HEADER_H
#define AHFITS_AHFITS_HEADER_H

#include "ahgen/ahversion.h"
AHVERSION(AHFITS_AHFITS_HEADER,"$Id: ahfits_header.h,v 1.13 2015/07/29 01:10:12 mwitthoe Exp $")

#include "ahfits/ahfits_base.h"

#include "fitsio.h"

#include <cstddef>
#include <string>

/// \ingroup mod_ahfits
namespace ahfits {

/** \addtogroup mod_ahfits
 *  @{
 */

/// \brief maximum line size of a comment
///
/// Note this constant is set to the value reported in the cfitsio manual
/// (i.e. 70 characters), however FITS files can actually support 72 characters
/// per COMMENT line.
const std::size_t MAXCOMMENTLENGTH=70;

/// \brief return true if keyword exists in header
/// \param[in] ahffp The FITS file object.
/// \param[in] key header key label
/// \return true/false
///
/// The function returns true if a keyword exists, even if the keyword 
/// has an empty value.  It only looks in the currently open HDU.
bool keywordExists(FilePtr ahffp, const std::string & key);

/// \brief From the active HDU, return header value from given key as a string.
/// \param[in] ahffp The FITS file object.
/// \param[in] key header key label
/// \return header value
std::string getKeyValStr(FilePtr ahffp, const std::string & key);

/// \brief In the active HDU, write header value from given key as a string.
/// \param[in] ahffp The FITS file object.
/// \param[in] key header key label
/// \param[in] value header value
/// \param[in] comment header comment
void writeKeyValStr(FilePtr ahffp, const std::string & key,
                          const std::string & value,
                          const std::string & comment);

/// \brief copy keyword from one HDU to another as a string
/// \param[in] ahffp_src source FITS file object
/// \param[in] ahffp_dest destination FITS file object
/// \param[in] key header key label
void copyKeyStr(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key);

/// \brief From the active HDU, return header value from given key as a double.
/// \param[in] ahffp The FITS file object.
/// \param[in] key header key label
/// \return header value
double getKeyValDbl(FilePtr ahffp, const std::string & key);

/// \brief In the active HDU, write header value from given key as a double.
/// \param[in] ahffp The FITS file object.
/// \param[in] key header key label
/// \param[in] value header value
/// \param[in] comment header comment
void writeKeyValDbl(FilePtr ahffp, const std::string & key,
                          double value, const std::string & comment);

/// \brief copy keyword from one HDU to another as a double
/// \param[in] ahffp_src source FITS file object
/// \param[in] ahffp_dest destination FITS file object
/// \param[in] key header key label
void copyKeyDbl(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key);

/// \brief From the active HDU, return header value from given key as a long
///  long.
/// \param[in] ahffp The FITS file object.
/// \param[in] key header key label
/// \return header value
long long getKeyValLLong(FilePtr ahffp, const std::string & key);

/// \brief In the active HDU, write header value from given key as a long long.
/// \param[in] ahffp The FITS file object.
/// \param[in] key header key label
/// \param[in] value header value
/// \param[in] comment header comment
void writeKeyValLLong(FilePtr ahffp, const std::string & key,
                            long long value, const std::string & comment);

/// \brief copy keyword from one HDU to another as a long long
/// \param[in] ahffp_src source FITS file object
/// \param[in] ahffp_dest destination FITS file object
/// \param[in] key header key label
void copyKeyLLong(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key);

/// \brief From the active HDU, return header value from given key as a boolean.
/// \param[in] ahffp The FITS file object.
/// \param[in] key header key label
/// \return header value
bool getKeyValBool(FilePtr ahffp, const std::string & key);

/// \brief In the active HDU, write header value from given key as a boolean.
/// \param[in] ahffp The FITS file object.
/// \param[in] key header key label
/// \param[in] value header value
/// \param[in] comment header comment
void writeKeyValBool(FilePtr ahffp, const std::string & key,
                           bool value, const std::string & comment);

/// \brief copy keyword from one HDU to another as a boolean (logical)
/// \param[in] ahffp_src source FITS file object
/// \param[in] ahffp_dest destination FITS file object
/// \param[in] key header key label
void copyKeyBool(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key);

/// \brief Modify (overwrite) the comment field of an existing keyword
/// \param[in] ahffp source FITS file object
/// \param[in] key header key label
/// \param[in] comment header key comment
void modifyKeyComment(FilePtr ahffp, const std::string &key, const std::string& comment);

/// \brief write a comment to the header of the current HDU
/// \param[in] ahffp The FITS file pointer
/// \param[in] comment comment string to be written
void writeKeyComment(FilePtr ahffp, std::string comment);

/// \brief write a history string to the header of the current HDU
/// \param[in] ahffp The FITS file pointer
/// \param[in] history history string to be written
void writeKeyHistory(FilePtr ahffp, std::string history);

/// \brief Copy a keyword from one HDU to another regardless of value type.
/// \param[in] ahffp_src source FITS file object
/// \param[in] ahffp_dest destination FITS file object
/// \param[in] key header key label
void copyKey(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key);

/// \brief If not stamped already, stamp parameter file to current HDU
/// \param[in] ahffp The FITS file pointer
void stamp(FilePtr & ahffp);

/** @} */

} // namespace ahfits

#endif   /* AHFITS_AHFITS_HEADER_H */

/* Revision Log
   $Log: ahfits_header.h,v $
   Revision 1.13  2015/07/29 01:10:12  mwitthoe
   ahfits: add new function to copy a header keyword from one HDU to another regardless of value type: ahfits::copyKey()

   Revision 1.12  2015/05/18 14:30:50  asargent
   Added modifyKeyComment and setColumnDescription functions

   Revision 1.11  2014/08/04 14:22:49  peachey
   Use size_t instead of a signed in for MAXCOMMENTLENGTH, to avoid warnings.

   Revision 1.10  2014/01/29 21:45:51  mwitthoe
   ahfits: allow HISTORY keywords to be automatically split across several lines if too long; see issue 332

   Revision 1.9  2014/01/14 21:33:49  mwitthoe
   ahfits: update writeKeyComments() to automatically split lines based on the maximum COMMENT size and new line characters (\n); this routine will split the comment at space characters whereas cfitsio just divides at the max size; see issue 332

   Revision 1.8  2013/12/10 22:08:16  mwitthoe
   ahfits: add functions to copy keywords from one HDU to another, e.g. copyKeyDbl()

   Revision 1.7  2013/10/16 18:51:32  mwitthoe
   ahfits library: update doxygen documentation

   Revision 1.6  2013/10/04 15:39:09  mwitthoe
   ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

   Revision 1.5  2013/09/16 20:09:47  klrutkow
   updated keywordExists function and its doxygen

   Revision 1.4  2013/09/09 17:43:09  mwitthoe
   ahfits library: added function, keywordExists(), to check if a keyword is present in the header; added test case to test function

   Revision 1.3  2012/10/24 15:00:07  mwitthoe
   ahfits: move stamp() from ahfits_file to ahfits_header; new function addHDU in ahfits_file which will make a new (empty) extension based on the header of an existing extension from the same or different file

   Revision 1.2  2012/10/17 02:38:22  mwitthoe
   add Doxygen comments to each ahfits header which will be output to the ahfits module

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/
