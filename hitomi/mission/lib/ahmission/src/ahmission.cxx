/// \file ahmission.cxx
/// \brief General, mission-specific functions
/// \author Mike Witthoeft
/// \date $Date: 2016/02/22 15:08:30 $
 
#define AHLABEL ahmission_ahmission
#define AHCVSID "$Id: ahmission.cxx,v 1.35 2016/02/22 15:08:30 klrutkow Exp $"

#include "ahmission/ahmission.h"
#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"

#include <iostream>
#include <sstream>
#include <string>
#include <time.h>

namespace ahmission {

// ---------------------------------------------------------------------------

std::string getTELESCOPString() {
  std::string TELESCOP = "HITOMI";
  return TELESCOP;
}

// ---------------------------------------------------------------------------

bool isValidTELESCOP(const std::string& telescop) {
  if (ahgen::strtoupper(telescop) == "ASTRO-H") return true;
  if (ahgen::strtoupper(telescop) == "HITOMI") return true;
  return false;
}

// ---------------------------------------------------------------------------

std::string getCurrentDate() {
  time_t     now = time(0);
  struct tm  tstruct;
  char       date[80];
  tstruct = *localtime(&now);
  strftime(date, sizeof(date), "%Y-%m-%d", &tstruct);
  return date;
}

// ---------------------------------------------------------------------------

void checkEmptyTable(ahfits::FilePtr ahffp, const std::string& filename) {
  if (0 == ahfits::numRows(ahffp))
    AH_THROW_RUNTIME("FITS table contains no rows; tool cannot continue: "+filename);
}

// ---------------------------------------------------------------------------

}  // namespace ahmission



namespace procstatus {

// ---------------------------------------------------------------------------

bool processRow(const char* proc_status) {
  if (proc_status[1] == 1) return false;    // don't really need to check 1st bit (index=0)
  if (proc_status[16] == 1) return false;
  return true;
}

// ---------------------------------------------------------------------------

}  // namespace procstatus

/* Revision Log
 $Log: ahmission.cxx,v $
 Revision 1.35  2016/02/22 15:08:30  klrutkow
 isValidTELESCOP: instead of replacing ASTRO-H with HITOMI, added the HITOMI line

 Revision 1.34  2016/02/18 22:01:15  klrutkow
 changed mission name to HITOMI

 Revision 1.33  2015/12/29 16:27:34  rshill
 Change comma to semicolon in message.

 Revision 1.32  2015/12/23 20:16:44  mwitthoe
 ahmission library: add function, checkEmptyTable(), which will throw an error if the given FITS table has no rows

 Revision 1.31  2015/05/12 17:57:17  mwitthoe
 ahmission: add new function, isValidTELESCOP(), which checks if the given string matches an acceptable TELESCOP keyword value for Astro-H; this function will be used so that multiple TELESCOP will be acceptable once Astro-H is renamed after launch.

 Revision 1.30  2015/04/23 13:57:43  mdutka
 adding function getCurrentDate will be used in some tools where the current date will be used to query CALDB

 Revision 1.29  2014/11/04 17:12:02  mwitthoe
 ahmission: update procstatus::processRow() to include check on Astro-H bit (index 16); updated library test to check new behavior; remove procstatus::rowOK() function which doesn't really mean anything anymore; see issue 460

 Revision 1.28  2014/11/04 16:37:45  mdutka
 Added function getTELESCOPString and removed getTELESCOP and setTELESCOP see issue #454 in redmine

 Revision 1.27  2014/10/02 14:26:27  mdutka
 changed the order of parameters passed to setTELESCOP

 Revision 1.26  2014/10/01 13:44:04  mdutka
 Added two new functions readTELESCOP and setTELESCOP

 Revision 1.25  2014/09/16 18:32:51  mwitthoe
 ahmission: remove instrument argument from procstatus::processRow(); see issue 412

 Revision 1.24  2014/07/08 19:35:58  mwitthoe
 ahmission library: give the instrument argument of procstatus::processRow() a default value of zero; all instrument-specific behavior has been removed from this function; a new function, loadAndGet(), has been added to delay.h/cxx which will read delay times from the given CALDB file corresponding to the given time since there is no need anymore to keep the whole delay table in memory; the old functions are still present, but will be removed once it is sure that they will not be needed (in Build 6)

 Revision 1.23  2014/02/03 17:30:40  mwitthoe
 ahmission: fix processRow() which had no return value (thus returned 0) for SXI/HXI/SGD when proc_status[1] == 1; added unit tests for HXI and SGD

 Revision 1.22  2014/01/31 19:11:20  rshill
 Fixed coding error in switch statment.

 Revision 1.21  2014/01/31 18:37:56  rshill
 Added HXI and SGD to list of instruments affected
 by two-bit PROC_STATUS condition.

 Revision 1.20  2014/01/13 22:16:03  mwitthoe
 ahmission: remove procStatFirstBitBad() which has been replaced by processRow(); see issue 314

 Revision 1.19  2014/01/03 16:23:04  mwitthoe
 ahmission: add new functions for checking Japanese bits of PROC_STATUS; the new function, procstatus::rowOK(), replaces the old function, procStatFirstBitBad(); the old function will remain until tools can be switched over to the new functions

 Revision 1.18  2013/12/30 21:35:39  mwitthoe
 ahmission: add function to read first two bits of PROC_STATUS and return an enumerated value giving the process type; see issue 314

 Revision 1.17  2013/07/02 17:49:22  mwitthoe
 remove obsolete, commented-out function from ahmission.cxx

 Revision 1.16  2013/07/01 15:48:35  mwitthoe
 add function, procStatFirstBitBad(), to ahmission which will return true if the first bit (set in Japan) is 1 indicating that there is a problem with the FITS file row; see issue #126

 Revision 1.15  2013/04/10 14:57:35  mwitthoe
 remove instrument/detector enumerations and associated functions from ahmission

 Revision 1.14  2012/12/14 22:12:39  mwitthoe
 split SGD-Shield into SGD-Shield1 and SGD-Shield2; while this distinction reflects different electronics instead of different instruments, it will be represented in DETNAME

 Revision 1.13  2012/12/11 19:42:46  mwitthoe
 change SGD WAM to SGD SHIELD in ahmission

 Revision 1.12  2012/12/07 21:42:59  mwitthoe
 ahmission: allow the string, SGD_CC, to resolve to the detector enumeration, SGD_CCALL, as to not break the loading of the column definitions CALDB file (timecoldef in ahcaldb)

 Revision 1.11  2012/12/06 16:28:21  mwitthoe
 ahmission: changed SGD_CC detector enumeration to CC1, CC2, CC3, and CCALL; changed instrument and detector enumeration values to be powers of two; added functions, checkInst() and checkDet()

 Revision 1.10  2012/11/28 20:47:25  mwitthoe
 readHeaderInfo() in ahmission no longer returns false for INST_BAD or DET_BAD is file is HK

 Revision 1.9  2012/11/28 19:58:44  mwitthoe
 added new member AhFitsHeaderInfo struct in ahmission which indicates is the loaded file is an HK file; removed INST_HK and DET_HK

 Revision 1.8  2012/11/13 18:14:51  mwitthoe
 ahmission: changed HXI & SGD instruments to HXI1/2 and SGD1/2

 Revision 1.7  2012/10/11 18:00:48  mwitthoe
 convert ahmission over to new ahfits

 Revision 1.6  2012/09/25 19:23:55  mwitthoe
 changed the preferred detector names in ahmission to match table in SCT 021

 Revision 1.5  2012/09/14 23:41:18  mwitthoe
 apply version standards to ahmission

 Revision 1.4  2012/09/05 18:58:48  mwitthoe
 added enumeration->string functions to ahmission for telescope, instrument, and detector

 Revision 1.3  2012/08/31 20:56:21  mwitthoe
 in ahmission: add HK instrument and detector, add macro in source to improve readability of array to set conversion of allowed values

 Revision 1.2  2012/08/31 19:36:01  mwitthoe
 in ahmission: hide intermediate constant arrays from public view; ensure non-BAD enumerations do not have overlapping values; add extension name (as string) to header info struct

 Revision 1.1  2012/08/30 20:12:18  mwitthoe
 add ahmission library


*/
