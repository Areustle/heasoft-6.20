/// \file ahgen.h
/// \brief Declarations of public members of the libahgen library.
/// \author James Peachey
/// \date $Date: 2014/09/29 23:32:51 $

/// \addtogroup mod_ahgen
/// \section ahgen_ahgen General Stuff - ahgen
///
/// This library contains general, non-Astro-H functions which do not have
/// another home.  Current occupants are:
///  - functions to set/get the global clobber state
///  - functions to add/remove the bang (!) preceding filenames to indicate the clobber state
///  - functions to operate on file paths
///

#ifndef AHGEN_AHGEN_H
#define AHGEN_AHGEN_H 

#include "ahgen/ahversion.h"
AHVERSION(AHGEN_AHGEN,"$Id: ahgen.h,v 1.29 2014/09/29 23:32:51 mwitthoe Exp $")

#include "ahlog/ahlog.h"

#include "ahgen/ahfile.h"
#include "ahgen/ahrandom.h"
#include "ahgen/ahtest.h"
#include "ahgen/BitBuf.h"

/// \brief miscellaneous generic functions utilities
/// \ingroup mod_ahgen
namespace ahgen {

/** \addtogroup mod_ahgen
 *  @{
 */

/// \brief convert string to uppercase
/// \param[in] instr mixed case input string
/// \return capitalized version of input string
std::string strtoupper(std::string instr);

/// \brief check if a string value represents a number (integer or 
///  floating-point).
/// \param[in] val input string
/// \return true if input string represents a number
bool isNumber(std::string val);

/// \brief Set the global clobber state
/// \param[in] clobber clobber state
void setClobber(bool clobber);

/// \brief Get the global clobber state
bool getClobber(void);

/// \brief Set the global buffer state
/// \param[in] buffer buffer state
void setBuffer(int buffer);

/// \brief Get the global buffer state
int getBuffer(void);

/// \brief Return global history state (true/false).
/// \return global history state
bool getHistory(void);

/// \brief Set global history state.
/// \param[in] history desired state (default=true)
void setHistory(bool history=true);

/// \brief Set global history state to false.
void unsetHistory(void);

} // namespace ahgen

/** @} */

#endif   /* AHGEN_AHGEN_H */

/* Revision Log
 $Log: ahgen.h,v $
 Revision 1.29  2014/09/29 23:32:51  mwitthoe
 ahgen: add function to check if the given string represents a number: isNumber()

 Revision 1.28  2014/09/23 04:00:05  mwitthoe
 ahgen: split some functions from main ahgen file into ahfile and ahrandom files; see issue 437

 Revision 1.27  2014/07/28 18:33:28  mdutka
 added changes to getRefDataPath

 Revision 1.26  2014/06/27 18:41:37  asargent
 Added new function, fileExists.

 Revision 1.25  2014/06/23 14:38:58  mdutka
 Added function getRefDataPath.

 Revision 1.24  2014/04/03 20:48:53  klrutkow
 added freeRandom() to free seed state object - must be called by caller after calling seedRandom

 Revision 1.23  2014/03/04 02:42:19  mwitthoe
 ahgen: add function to generate random integer in range: getRandomInt()

 Revision 1.22  2013/11/27 15:23:55  mwitthoe
 ahgen: implement new random number scheme (Mersenne Twister) as discussed in issues 295 and 267

 Revision 1.21  2013/07/10 18:20:04  mwitthoe
 ahgen: add global history state which will be set by ahapp (from the history standard parameter) and accessed by ahfits for writing parameters to the header of modified FITS files

 Revision 1.20  2013/07/09 18:19:25  mwitthoe
 add two functions to ahgen to get random numbers: seedRandom() ad getRandom(); the underlying RNG is from native C++ and is only temporary; see issue 267

 Revision 1.19  2013/07/01 19:48:29  mwitthoe
 add global buffer state to ahgen for storing the buffer standard parameter

 Revision 1.18  2013/01/24 17:44:30  mwitthoe
 update Doxygen for ahgen

 Revision 1.17  2012/11/26 21:22:05  mwitthoe
 add brief descriptions to namespaces in gen

 Revision 1.16  2012/11/02 17:11:49  mwitthoe
 ahgen: add functions to add/remove bang to filename to indicate clobber

 Revision 1.15  2012/11/02 13:07:47  mwitthoe
 remove chatter and debug from ahgen as these are controlled by ahlog

 Revision 1.14  2012/11/01 19:44:14  mwitthoe
 add function, isFileClobbered(), to ahgen

 Revision 1.13  2012/11/01 14:37:31  mwitthoe
 add global clobber state to ahgen

 Revision 1.12  2012/10/24 15:31:17  mwitthoe
 add parameter to ahgen::filePathSymbolic() to ignore the case where the given file path does not exist; modified test code to create and remove the test symbolic link since it cannot be saved to the repository

 Revision 1.11  2012/10/23 20:24:20  mwitthoe
 add parameter to ahgen::filePathsEquivalent() allowing second file not to exist

 Revision 1.10  2012/10/23 19:03:06  mwitthoe
 remove instrument stuff from ahgen (now in ahmission); add functions for determining if two file paths refer to the same file and if a file path is a symbolic link

 Revision 1.9  2012/10/16 15:15:35  mwitthoe
 removed redundant standard main routines from ahgen (they are now in ahapp)

 Revision 1.8  2012/09/14 23:55:24  mwitthoe
 apply version standards to ahgen

 Revision 1.7  2012/08/30 20:08:43  mwitthoe
 add function to ahgen which converts all characters in a std::string to uppercase

 Revision 1.6  2012/07/20 14:54:01  mwitthoe
 add Doxygen ahgen

 Revision 1.5  2012/05/14 17:19:30  mwitthoe
 add const to argument of atoinst to match .cxx file

 Revision 1.4  2012/04/25 21:34:25  peachey
 Add chatter and debug parameters to the startUp step.

 Revision 1.3  2012/04/24 15:26:07  mwitthoe
 add instrument enumeration and conversion function to ahgen

 Revision 1.2  2012/02/03 15:30:45  peachey
 Add and test getClobber facility, from parameter file.

 Revision 1.1  2012/01/31 22:23:33  peachey
 Add first version of general support library.

*/
