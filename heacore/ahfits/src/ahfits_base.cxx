/// \file ahfits_base.cxx
/// \brief ahfits: Structures and typedefs
/// \author James Peachey
/// \date $Date: 2015/06/12 18:00:07 $

#define AHLABEL ahfits_ahfits_base
#define AHCVSID "$Id: ahfits_base.cxx,v 1.16 2015/06/12 18:00:07 mwitthoe Exp $"

#include "ahfits/ahfits_base.h"
#include "ahlog/ahlog.h"

#include "fitsio.h"

#include <sstream>

namespace ahfits {

static bool s_clobber=false;
static int s_buffer=-1;
static bool s_history=true;

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

bool isBintable(FilePtr ahffp) {
  int status=0;
  int hdutype;
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  if (0 != fits_get_hdu_type(ahffp->m_cfitsfp,&hdutype,&status))
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,false,false)+"cannot obtain extension type");
  if (BINARY_TBL == hdutype) return true;
  return false;
}


// -----------------------------------------------------------------------------

bool isImage(FilePtr ahffp) {
  int status=0;
  int hdutype;
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  if (0 != fits_get_hdu_type(ahffp->m_cfitsfp,&hdutype,&status))
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,false,false)+"cannot obtain extension type");
  if (IMAGE_HDU == hdutype) return true;
  return false;
}


// -----------------------------------------------------------------------------

bool isASCII(FilePtr ahffp) {
  int status=0;
  int hdutype;
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  if (0 != fits_get_hdu_type(ahffp->m_cfitsfp,&hdutype,&status))
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,false,false)+"cannot obtain extension type");
  if (ASCII_TBL == hdutype) return true;
  return false;
}

// -----------------------------------------------------------------------------

bool isPrimary(FilePtr ahffp) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  if (ahffp->m_hduidx == 1) return true;
  return false;
}

// -----------------------------------------------------------------------------

std::string statusMsg(int status) {
  if (0 == status) return "";
  char err[FLEN_STATUS] = "";
  fits_get_errstatus(status,err);
  std::stringstream msg;
  msg << " [CFITSIO STATUS " << status << ": " << err << "]";
  return msg.str();
}

// -----------------------------------------------------------------------------

std::string errPrefix(FilePtr ahffp, bool includeHDU, bool includeRow) {
  std::stringstream out;
  out << getFileAndHDUString(ahffp);
  if (includeRow) out << " (row " << ahffp->m_currow << ")";
  out << ": ";
  return out.str();
}

// -----------------------------------------------------------------------------

void enableParameterStamping(ahfits::FilePtr ahffp) {
  ahffp->m_stamppar=true;
}

// -----------------------------------------------------------------------------

void disableParameterStamping(ahfits::FilePtr ahffp) {
  ahffp->m_stamppar=false;
}

// -----------------------------------------------------------------------------

void setParameterStamping(ahfits::FilePtr ahffp, bool state) {
  ahffp->m_stamppar=state;
}

// -----------------------------------------------------------------------------

bool isParameterStamping(ahfits::FilePtr ahffp) {
  return ahffp->m_stamppar;
}

// -----------------------------------------------------------------------------

int convertAhfitsToCfitsioHDUType(int hdutype) {
  switch (hdutype) {
    case e_ANY_HDU:
      return ANY_HDU;
    case e_PRIMARY_HDU:
      return IMAGE_HDU;
    case e_BINARY_TBL:
      return BINARY_TBL;
    case e_ASCII_TBL:
      return ASCII_TBL;
    case e_IMAGE_HDU:
      return IMAGE_HDU;
  }
  AH_THROW_LOGIC("invalid enumerated value for ahfits HDU type ");
}

// -----------------------------------------------------------------------------

std::string getFileAndHDUString(ahfits::FilePtr ahffp) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // get extension name
  std::string extname="";
  char value[FLEN_CARD];
  int status=0;
  if (0 == fits_read_key(ahffp->m_cfitsfp,TSTRING,"EXTNAME",value,NULL,&status))
    extname=value;
  else {
    std::stringstream tmp;
    tmp << "HDU" << ahffp->m_hduidx;
    extname=tmp.str();
  }
  extname="["+extname+"]";

  // get base file name (strip off any extended syntax)
  std::string filename=ahffp->m_filename;
  std::size_t loc=filename.find("[");
  if (loc != std::string::npos) {
    filename=filename.substr(0,loc);
  }

  return filename+extname;
}

// -----------------------------------------------------------------------------

void enableBufferPadding(ahfits::FilePtr ahffp) {
  ahffp->m_add_extrarows_when_buffering=true;
}

// -----------------------------------------------------------------------------

void disableBufferPadding(ahfits::FilePtr ahffp) {
  ahffp->m_add_extrarows_when_buffering=false;
}

// -----------------------------------------------------------------------------

} // namespace ahfits

/* Revision Log
   $Log: ahfits_base.cxx,v $
   Revision 1.16  2015/06/12 18:00:07  mwitthoe
   ahfits: add two functions to get/set parameter stamping state

   Revision 1.15  2015/04/02 15:03:21  mwitthoe
   ahfits: add checks for NULL ahfits or cfitsio FITS pointers; see issue 461

   Revision 1.14  2014/11/26 15:12:45  mwitthoe
   ahfits: add clobber, buffer, and history states to library; library now accesses these states instead of those in ahgen; see issue 437

   Revision 1.13  2014/09/12 18:02:43  mwitthoe
   ahfits: modify getFileAndHDUString() to strip off any extended syntax from the filename before appending the current HDU name

   Revision 1.12  2014/04/04 17:05:07  mwitthoe
   ahfits: discovered two functions with similar capability: isIntegerTypeColumn in ahfits_base and isInteger in ahfits_colinfo; I like the former's name, but the latters implementation, so removed the first and renamed the second; changed all appropriate calls to these functions which were restricted within the ahfits library; the ahfits unit test and all tool unit tests still pass

   Revision 1.11  2014/04/01 14:57:06  mwitthoe
   ahfits: add member to ahfits FilePtr indicating whether extra rows are added to the end of the FITS file when buffering; see issue 368

   Revision 1.10  2014/02/07 01:05:15  rshill
   Added absolute value to isIntegerTypeColumn.

   Revision 1.9  2013/12/10 20:32:46  mwitthoe
   ahfits bug fixes: the create() function needed to add necessary keywords to the Primary HDU (now accomplished with fits_create_img); the old version was okay if immediately creating a new table after ahfits::create(), but created an incomplete FITS file if the newly created file was closed before any other action; the addHDU() function did not work if the active HDU was the primary HDU

   Revision 1.8  2013/10/04 15:39:09  mwitthoe
   ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

   Revision 1.7  2013/09/10 14:10:35  mwitthoe
   ahfits library: begin to add support for primary HDU access; add enumerated type to identify type of extension (e.g. e_BINARY_TBL) and a function to convert from this enumeration to the equivalent cfitsio enumeration (e.g. BINARY_TBL); add optional argument to nextHDU() specifying the desired HDU type to go to (default: e_ANY_HDU); add unit tests to check that new argument acts appropriately

   Revision 1.6  2013/07/12 13:30:33  mwitthoe
   ahfits: write function to provide standard error message prefix; use prefix throughout ahfits library

   Revision 1.5  2013/07/10 18:25:58  mwitthoe
   ahfits: add flag to FilePtr structure indicating if parametes should be written to the FITS header for modified files; the default value is set by the current global history state in ahgen; functions are added to ahfits to change the flag for an individual FITS file

   Revision 1.4  2013/07/08 18:25:38  mwitthoe
   ahfits library: add setTNull() which will write the TNULLL keyword for integer-like columns; when making router connection with local NULL flags, require target, integer-like columns to have the TNULL keyword defined

   Revision 1.3  2013/03/09 04:29:20  peachey
   Remove some unnecessary dynamic memory allocations in favor
   of automatic variables.

   Revision 1.2  2012/11/04 23:25:20  mwitthoe
   ahfits: add cfitsio error messages based on error status; made clone/create functions that return a fits pointer to the newly created FITS file

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/
