/// \file ahmission.h
/// \brief General, mission-specific functions
/// \author Mike Witthoeft
/// \date $Date: 2015/12/23 20:16:44 $
 
/// \addtogroup mod_ahmission
/// \section ahmission_ahmission Mission-specific constants and functions - ahmission
///
/// The library provides mission-specific constants and functions.
///

#ifndef AHMISSION_AHMISSION_H
#define AHMISSION_AHMISSION_H

#include "ahgen/ahversion.h"
AHVERSION(AHMISSION_AHMISSION,"$Id: ahmission.h,v 1.30 2015/12/23 20:16:44 mwitthoe Exp $")

#include "ahfits/ahfits.h"

#include <string>

/// \brief mission-specific constants and functions
/// \ingroup mod_ahmission
namespace ahmission {

/** \addtogroup mod_ahmission
 *  @{
 */

enum INSTRUMENT {
  e_CAMS,
  e_HXI,
  e_SGD,
  e_SXI,
  e_SXS
};

/// \brief Returns the expected value of the TELESCOP keyword
///        as a string
std::string getTELESCOPString();

/// \brief Returns true if given string matches accepted Astro-H TELESCOP
///  keyword value.
/// \param[in] telescop value of TELESCOP keyword
///
/// This function is created to allow multiple TELESCOP keywords values to
/// be accepted.  After launch, Astro-H will be renamed and we will need 
/// to support the old and new names during the transition phase.
bool isValidTELESCOP(const std::string& telescop);

/// \briefs Return the current date in the form YYYY-mm-dd
std::string getCurrentDate();

/// \brief Throw a run-time error if the active FITS table has no rows.
/// \param[in] ahffp ahfits file pointer opened to active extension
/// \param[in] filename name of file to write in error message
void checkEmptyTable(ahfits::FilePtr ahffp, const std::string& filename);


/** @} */

}  // namespace ahmission


/// \brief functions acting of PROC_STATUS values
/// \ingroup mod_ahmission
namespace procstatus {

/** \addtogroup mod_ahmission
 *  @{
 */

/// \brief Check if it is okay to process row based on PROC_STATUS
/// \param[in] proc_status value of PROC_STATUS column as a character array
///  of 32 bytes
/// \return true if row should be processed
///
/// The PROC_STATUS column has 32 bits where the first 16 bits are assigned by
/// the prepipeline processing in Japan and the remaining 16 bits are assigned
/// by the SCT software.  There are two conditions which need to be met in 
/// order to process the row:
///   1. the first two prepipeline bits (indices 0 & 1) must be 00 or 10
///   2. the first SCT bits (index 16) must be 0
/// otherwise the row should not be processed.
/// Note: in the prepipeline test, it doesn't really matter what the value
/// of the leading bit is, the test passes or fails based on the value of
/// the second bit (index 1).
bool processRow(const char* proc_status);

/** @} */

}  // namespace procstatus


#endif /* AHMISSION_AHMISSION_H */

/* Revision Log
 $Log: ahmission.h,v $
 Revision 1.30  2015/12/23 20:16:44  mwitthoe
 ahmission library: add function, checkEmptyTable(), which will throw an error if the given FITS table has no rows

 Revision 1.29  2015/05/12 17:57:17  mwitthoe
 ahmission: add new function, isValidTELESCOP(), which checks if the given string matches an acceptable TELESCOP keyword value for Astro-H; this function will be used so that multiple TELESCOP will be acceptable once Astro-H is renamed after launch.

 Revision 1.28  2015/04/23 13:49:31  mdutka
 adding function getCurrentDate will be used in some tools where the current date will be used to query CALDB

 Revision 1.27  2014/11/04 17:12:02  mwitthoe
 ahmission: update procstatus::processRow() to include check on Astro-H bit (index 16); updated library test to check new behavior; remove procstatus::rowOK() function which doesn't really mean anything anymore; see issue 460

 Revision 1.26  2014/11/04 16:37:44  mdutka
 Added function getTELESCOPString and removed getTELESCOP and setTELESCOP see issue #454 in redmine

 Revision 1.25  2014/10/02 14:31:31  mdutka
 changed the order of parameters passed to setTELESCOP

 Revision 1.24  2014/10/01 13:55:31  mdutka
 Added new function readTELESCOP and setTELESCOP

 Revision 1.23  2014/09/16 18:32:51  mwitthoe
 ahmission: remove instrument argument from procstatus::processRow(); see issue 412

 Revision 1.22  2014/07/08 19:35:57  mwitthoe
 ahmission library: give the instrument argument of procstatus::processRow() a default value of zero; all instrument-specific behavior has been removed from this function; a new function, loadAndGet(), has been added to delay.h/cxx which will read delay times from the given CALDB file corresponding to the given time since there is no need anymore to keep the whole delay table in memory; the old functions are still present, but will be removed once it is sure that they will not be needed (in Build 6)

 Revision 1.21  2014/01/31 19:49:48  rshill
 Added a close brace for doxygen.

 Revision 1.20  2014/01/13 22:16:02  mwitthoe
 ahmission: remove procStatFirstBitBad() which has been replaced by processRow(); see issue 314

 Revision 1.19  2014/01/03 16:23:04  mwitthoe
 ahmission: add new functions for checking Japanese bits of PROC_STATUS; the new function, procstatus::rowOK(), replaces the old function, procStatFirstBitBad(); the old function will remain until tools can be switched over to the new functions

 Revision 1.18  2013/12/30 21:35:39  mwitthoe
 ahmission: add function to read first two bits of PROC_STATUS and return an enumerated value giving the process type; see issue 314

 Revision 1.17  2013/07/01 15:48:35  mwitthoe
 add function, procStatFirstBitBad(), to ahmission which will return true if the first bit (set in Japan) is 1 indicating that there is a problem with the FITS file row; see issue #126

 Revision 1.16  2013/04/10 14:57:35  mwitthoe
 remove instrument/detector enumerations and associated functions from ahmission

 Revision 1.15  2013/01/24 19:46:16  mwitthoe
 update Doxygen for ahmission library

 Revision 1.14  2013/01/08 19:47:56  mwitthoe
 fixed misspelling of enumeration name, ahtelescopes, as pointed out by RSH

 Revision 1.13  2013/01/08 19:40:05  rshill
 Marked a misspelling for attention.

 Revision 1.12  2012/12/14 22:12:39  mwitthoe
 split SGD-Shield into SGD-Shield1 and SGD-Shield2; while this distinction reflects different electronics instead of different instruments, it will be represented in DETNAME

 Revision 1.11  2012/12/11 19:42:45  mwitthoe
 change SGD WAM to SGD SHIELD in ahmission

 Revision 1.10  2012/12/06 16:28:21  mwitthoe
 ahmission: changed SGD_CC detector enumeration to CC1, CC2, CC3, and CCALL; changed instrument and detector enumeration values to be powers of two; added functions, checkInst() and checkDet()

 Revision 1.9  2012/11/28 19:58:44  mwitthoe
 added new member AhFitsHeaderInfo struct in ahmission which indicates is the loaded file is an HK file; removed INST_HK and DET_HK

 Revision 1.8  2012/11/26 21:23:20  mwitthoe
 add brief descriptions to namespaces in mission

 Revision 1.7  2012/11/13 18:14:51  mwitthoe
 ahmission: changed HXI & SGD instruments to HXI1/2 and SGD1/2

 Revision 1.6  2012/10/11 18:00:47  mwitthoe
 convert ahmission over to new ahfits

 Revision 1.5  2012/09/14 23:41:18  mwitthoe
 apply version standards to ahmission

 Revision 1.4  2012/09/05 18:58:47  mwitthoe
 added enumeration->string functions to ahmission for telescope, instrument, and detector

 Revision 1.3  2012/08/31 20:56:21  mwitthoe
 in ahmission: add HK instrument and detector, add macro in source to improve readability of array to set conversion of allowed values

 Revision 1.2  2012/08/31 19:36:00  mwitthoe
 in ahmission: hide intermediate constant arrays from public view; ensure non-BAD enumerations do not have overlapping values; add extension name (as string) to header info struct

 Revision 1.1  2012/08/30 20:12:17  mwitthoe
 add ahmission library


*/
