/// \file ahgen.cxx
/// \brief Implementation of public members of the libahgen library.
/// \author James Peachey
/// \date $Date: 2014/09/29 23:32:51 $

#define AHLABEL ahgen_ahgen
#define AHCVSID "$Id: ahgen.cxx,v 1.30 2014/09/29 23:32:51 mwitthoe Exp $"

#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include "headas_rand.h"

#include <string>
#include <locale>       // std::toupper
#include <cstdlib>      // strtod


namespace ahgen {

static bool s_clobber=false;
static int s_buffer=-1;
static bool s_history=true;

// -----------------------------------------------------------------------------

std::string strtoupper(std::string instr) {
  std::string outstr=instr;
  std::string::iterator it=outstr.begin();
  while (it != outstr.end()) {
    *it=std::toupper((unsigned char)*it);
    ++it;
  }
  return outstr;
}

// -----------------------------------------------------------------------------

bool isNumber(std::string val) {
  if (val == "") return false;
  char* p=0;
  strtod(val.c_str(),&p);
  return *p == 0;
}

// -----------------------------------------------------------------------------

void setClobber(bool clobber) {
  s_clobber=clobber;
}

// -----------------------------------------------------------------------------

bool getClobber(void) {
  return s_clobber;
}

// -----------------------------------------------------------------------------

void setBuffer(int buffer) {
  s_buffer=buffer;
  if (s_buffer < 0) s_buffer=-1;
}

// -----------------------------------------------------------------------------

int getBuffer(void) {
  return s_buffer;
}

// -----------------------------------------------------------------------------

bool getHistory(void) {
  return s_history;
}

// -----------------------------------------------------------------------------

void setHistory(bool history) {
  s_history=history;
}

// -----------------------------------------------------------------------------

void unsetHistory(void) {
  s_history=false; 
}

// -----------------------------------------------------------------------------


} // namespace ahgen

/* Revision Log
 $Log: ahgen.cxx,v $
 Revision 1.30  2014/09/29 23:32:51  mwitthoe
 ahgen: add function to check if the given string represents a number: isNumber()

 Revision 1.29  2014/09/23 04:00:05  mwitthoe
 ahgen: split some functions from main ahgen file into ahfile and ahrandom files; see issue 437

 Revision 1.28  2014/07/28 16:27:53  mdutka
 Changed getrefdatapath, will now accept uppercase REFDATA keyword

 Revision 1.27  2014/06/27 18:42:02  asargent
 Added new function, fileExists.

 Revision 1.26  2014/06/23 14:38:58  mdutka
 Added function getRefDataPath.

 Revision 1.25  2014/04/03 20:48:53  klrutkow
 added freeRandom() to free seed state object - must be called by caller after calling seedRandom

 Revision 1.24  2014/04/03 20:38:53  klrutkow
 updated seedRandom() to use system time if user passes seed=0, copied from Dave R.'s code

 Revision 1.23  2014/03/04 13:13:17  mwitthoe
 ahgen: put in some range checking for getRandomInt

 Revision 1.22  2014/03/04 02:42:20  mwitthoe
 ahgen: add function to generate random integer in range: getRandomInt()

 Revision 1.21  2013/11/27 15:23:55  mwitthoe
 ahgen: implement new random number scheme (Mersenne Twister) as discussed in issues 295 and 267

 Revision 1.20  2013/07/10 18:20:04  mwitthoe
 ahgen: add global history state which will be set by ahapp (from the history standard parameter) and accessed by ahfits for writing parameters to the header of modified FITS files

 Revision 1.19  2013/07/09 18:19:26  mwitthoe
 add two functions to ahgen to get random numbers: seedRandom() ad getRandom(); the underlying RNG is from native C++ and is only temporary; see issue 267

 Revision 1.18  2013/07/01 19:48:29  mwitthoe
 add global buffer state to ahgen for storing the buffer standard parameter

 Revision 1.17  2012/11/02 17:11:49  mwitthoe
 ahgen: add functions to add/remove bang to filename to indicate clobber

 Revision 1.16  2012/11/02 13:07:48  mwitthoe
 remove chatter and debug from ahgen as these are controlled by ahlog

 Revision 1.15  2012/11/01 19:44:15  mwitthoe
 add function, isFileClobbered(), to ahgen

 Revision 1.14  2012/11/01 14:37:31  mwitthoe
 add global clobber state to ahgen

 Revision 1.13  2012/10/24 15:31:17  mwitthoe
 add parameter to ahgen::filePathSymbolic() to ignore the case where the given file path does not exist; modified test code to create and remove the test symbolic link since it cannot be saved to the repository

 Revision 1.12  2012/10/23 20:24:20  mwitthoe
 add parameter to ahgen::filePathsEquivalent() allowing second file not to exist

 Revision 1.11  2012/10/23 19:03:06  mwitthoe
 remove instrument stuff from ahgen (now in ahmission); add functions for determining if two file paths refer to the same file and if a file path is a symbolic link

 Revision 1.10  2012/10/16 15:15:35  mwitthoe
 removed redundant standard main routines from ahgen (they are now in ahapp)

 Revision 1.9  2012/09/14 23:55:24  mwitthoe
 apply version standards to ahgen

 Revision 1.8  2012/08/30 20:08:44  mwitthoe
 add function to ahgen which converts all characters in a std::string to uppercase

 Revision 1.7  2012/05/23 22:18:54  mwitthoe
 ahgen::startUp() will now take the log file name from the parameter file

 Revision 1.6  2012/05/16 20:13:54  mwitthoe
 change setup() arguments to match updated ahlog; add ahlog::shutdown() to shutDown() to properly close log file

 Revision 1.5  2012/05/10 19:16:08  mwitthoe
 make ahlog::setup() call in ahgen consistent with current ahlog version

 Revision 1.4  2012/04/25 21:34:25  peachey
 Add chatter and debug parameters to the startUp step.

 Revision 1.3  2012/04/24 15:26:07  mwitthoe
 add instrument enumeration and conversion function to ahgen

 Revision 1.2  2012/02/03 15:30:45  peachey
 Add and test getClobber facility, from parameter file.

 Revision 1.1  2012/01/31 22:23:33  peachey
 Add first version of general support library.

*/
