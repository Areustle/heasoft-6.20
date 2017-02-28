/// \file ahfits_header.cxx
/// \brief ahfits: Read/write header values
/// \author James Peachey
/// \date $Date: 2015/07/29 01:10:12 $

#define AHLABEL ahfits_ahfits_header
#define AHCVSID "$Id: ahfits_header.cxx,v 1.21 2015/07/29 01:10:12 mwitthoe Exp $"

#include "ahfits/ahfits_header.h"
#include "ahfits/ahfits_router.h"
#include "ahfits/ahfits_connect.h"
#include "ahlog/ahlog.h"

#include "fitsio.h"
#include "headas.h"

#include <sstream>
#include <list>

namespace ahfits {

// -----------------------------------------------------------------------------

bool keywordExists(FilePtr ahffp, const std::string & key) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  int status=0;
  char value[FLEN_CARD];
  fits_read_key(ahffp->m_cfitsfp,TSTRING,key.c_str(),value,NULL,&status);
  // status=VALUE_UNDEFINED if the keyword exists but has no value 
  if (0 == status || VALUE_UNDEFINED == status) 
    return true;
  else
    return false;

}

// -----------------------------------------------------------------------------

std::string getKeyValStr(FilePtr ahffp, const std::string & key) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  int status=0;
  char value[FLEN_CARD];
  if (0 != fits_read_key(ahffp->m_cfitsfp,TSTRING,key.c_str(),value,NULL,
      &status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to read header keyword: "+key+
                     statusMsg(status));
  }

  // prepare output and clean-up
  std::string out=value;

  return out;
}

// -----------------------------------------------------------------------------

void writeKeyValStr(FilePtr ahffp, const std::string & key, 
                    const std::string & value, const std::string & comment) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // cannot write to read-only files
  if (ahffp->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"file opened as read-only; cannot write");

  // stamp parameters to header (only does it once)
  ahfits::stamp(ahffp);

  int status=0;
  char* tvalue=(char*)value.c_str();
  char* tcomment=(char*)comment.c_str();
  if (0 != fits_update_key(ahffp->m_cfitsfp,TSTRING,key.c_str(),tvalue,
                           tcomment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write/update header keyword: "+key+
                     statusMsg(status));
  }
}

// -----------------------------------------------------------------------------

void copyKeyStr(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key) {

  // Check inputs.
  if (0 == ahffp_src || 0 == ahffp_src->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for source");
  if (0 == ahffp_dest || 0 == ahffp_dest->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for destination");

  // cannot write to read-only files
  if (ahffp_dest->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_dest,true,false)+"file opened as read-only; cannot write");

  int status=0;
  char value[FLEN_CARD];
  char comment[FLEN_CARD];
  if (0 != fits_read_key(ahffp_src->m_cfitsfp,TSTRING,key.c_str(),value,comment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to read header keyword: "+
                     key+" from source"+statusMsg(status));
  }

  if (0 != fits_update_key(ahffp_dest->m_cfitsfp,TSTRING,key.c_str(),value,comment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to write header keyword: "+
                     key+" to destination"+statusMsg(status));
  }

}

// -----------------------------------------------------------------------------

double getKeyValDbl(FilePtr ahffp, const std::string & key) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  int status=0;
  double value;
  if (0 != fits_read_key(ahffp->m_cfitsfp,TDOUBLE,key.c_str(),&value,NULL,
      &status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to read header keyword: "+key+
                     statusMsg(status));
  }

  return value;
}


// -----------------------------------------------------------------------------

void writeKeyValDbl(FilePtr ahffp, const std::string & key,
                          double value, const std::string & comment) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // cannot write to read-only files
  if (ahffp->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"file opened as read-only; cannot write");

  // stamp parameters to header (only does it once)
  ahfits::stamp(ahffp);

  int status=0;
  double tvalue=(double)value;
  char* tcomment=(char*)comment.c_str();
  if (0 != fits_update_key(ahffp->m_cfitsfp,TDOUBLE,key.c_str(),&tvalue,
                           tcomment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write/update header keyword: "+key+
                     statusMsg(status));
  }
}

// -----------------------------------------------------------------------------

void copyKeyDbl(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key) {

  // Check inputs.
  if (0 == ahffp_src || 0 == ahffp_src->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for source");
  if (0 == ahffp_dest || 0 == ahffp_dest->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for destination");

  // cannot write to read-only files
  if (ahffp_dest->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_dest,true,false)+"file opened as read-only; cannot write");

  int status=0;
  double value;
  char comment[FLEN_CARD];
  if (0 != fits_read_key(ahffp_src->m_cfitsfp,TDOUBLE,key.c_str(),&value,comment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to read header keyword: "+
                     key+" from source"+statusMsg(status));
  }

  if (0 != fits_update_key(ahffp_dest->m_cfitsfp,TDOUBLE,key.c_str(),&value,comment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to write header keyword: "+
                     key+" to destination"+statusMsg(status));
  }

}

// -----------------------------------------------------------------------------

long long getKeyValLLong(FilePtr ahffp, const std::string & key) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  int status=0;
  long long value;
  if (0 != fits_read_key(ahffp->m_cfitsfp,TLONGLONG,key.c_str(),&value,NULL,
      &status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to read header keyword: "+key+
                     statusMsg(status));
  }

  return value;
}

// -----------------------------------------------------------------------------

void writeKeyValLLong(FilePtr ahffp, const std::string & key,
                          long long value, const std::string & comment) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // cannot write to read-only files
  if (ahffp->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"file opened as read-only; cannot write");

  // stamp parameters to header (only does it once)
  ahfits::stamp(ahffp);

  int status=0;
  long long tvalue=(long long)value;
  char* tcomment=(char*)comment.c_str();
  if (0 != fits_update_key(ahffp->m_cfitsfp,TLONGLONG,key.c_str(),&tvalue,
                           tcomment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write/update header keyword: "+key+
                     statusMsg(status));
  }
}

// -----------------------------------------------------------------------------

void copyKeyLLong(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key) {

  // Check inputs.
  if (0 == ahffp_src || 0 == ahffp_src->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for source");
  if (0 == ahffp_dest || 0 == ahffp_dest->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for destination");

  // cannot write to read-only files
  if (ahffp_dest->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_dest,true,false)+"file opened as read-only; cannot write");

  int status=0;
  long long value;
  char comment[FLEN_CARD];
  if (0 != fits_read_key(ahffp_src->m_cfitsfp,TLONGLONG,key.c_str(),&value,comment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to read header keyword: "+
                     key+" from source"+statusMsg(status));
  }

  if (0 != fits_update_key(ahffp_dest->m_cfitsfp,TLONGLONG,key.c_str(),&value,comment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to write header keyword: "+
                     key+" to destination"+statusMsg(status));
  }

}

// -----------------------------------------------------------------------------

bool getKeyValBool(FilePtr ahffp, const std::string & key) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  int status=0;
  int value=0; // Note value must be an int because that is what cfitsio uses.
  if (0 != fits_read_key(ahffp->m_cfitsfp,TLOGICAL,key.c_str(),&value,NULL,
      &status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to read header keyword: "+key+
                     statusMsg(status));
  }

  // prepare output and cleanup
  bool out=(0 != value);

  return out;
}

// -----------------------------------------------------------------------------

void writeKeyValBool(FilePtr ahffp, const std::string & key,
                           bool value, const std::string & comment) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // cannot write to read-only files
  if (ahffp->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"file opened as read-only; cannot write");

  // stamp parameters to header (only does it once)
  ahfits::stamp(ahffp);

  int status=0;
  int tvalue=0;
  if (value) tvalue=1;
  char* tcomment=(char*)comment.c_str();
  if (0 != fits_update_key(ahffp->m_cfitsfp,TLOGICAL,key.c_str(),&tvalue,
                           tcomment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write/update header keyword: "+key+
                     statusMsg(status));
  }
}

// -----------------------------------------------------------------------------

void copyKeyBool(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key) {

  // Check inputs.
  if (0 == ahffp_src || 0 == ahffp_src->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for source");
  if (0 == ahffp_dest || 0 == ahffp_dest->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for destination");

  // cannot write to read-only files
  if (ahffp_dest->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_dest,true,false)+"file opened as read-only; cannot write");

  int status=0;
  int value;
  char comment[FLEN_CARD];
  if (0 != fits_read_key(ahffp_src->m_cfitsfp,TLOGICAL,key.c_str(),&value,comment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to read header keyword: "+
                     key+" from source"+statusMsg(status));
  }

  if (0 != fits_update_key(ahffp_dest->m_cfitsfp,TLOGICAL,key.c_str(),&value,comment,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to write header keyword: "+
                     key+" to destination"+statusMsg(status));
  }

}

// -----------------------------------------------------------------------------

void modifyKeyComment(FilePtr ahffp, const std::string &key, const std::string& comment) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // cannot write to read-only files
  if (ahffp->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"file opened as read-only; cannot write");

  if(!keywordExists(ahffp,key)) AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"keyword "+key+
                     " does not exist.");

  int status=0;
  fits_modify_comment(ahffp->m_cfitsfp,key.c_str(),comment.c_str(),&status);
  if(status) AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"could not update keyword comment for: "+key+
                     statusMsg(status));

}

// -----------------------------------------------------------------------------

// note: passing string by value on purpose so the local value can be modified
void writeKeyComment(FilePtr ahffp, std::string comment) {
  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // cannot write to read-only files
  if (ahffp->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"file opened as read-only; cannot write");

  // stamp parameters to header (only does it once)
  ahfits::stamp(ahffp);

  // Long comments may need to be split over several lines.  Arrange given
  // comment string into a list of strings, each small enough to fit on a
  // single line.  The original comment string will only be split at spaces.
  // New line characters (\n) will also be searched for.
  std::list<std::string> comlist;

  // look for new lines
  std::string eol="\n";
  while (1) {
    std::size_t index=comment.find(eol);
    if (std::string::npos == index) break;         // end-of-line character not found
    comlist.push_back(comment.substr(0,index));    // get all but the new line character
    comment.erase(0,index+1);                      // erase segment including new line character
  }
  comlist.push_back(comment);                      // rest of comment goes onto list

  // make sure each line is short enough
  std::list<std::string>::iterator it;
  for (it=comlist.begin(); it != comlist.end(); it++) {
    if (it->size() <= (std::size_t)MAXCOMMENTLENGTH) continue;

    // look for first space backwards from max size
    std::size_t ichar=MAXCOMMENTLENGTH-1;
    while ((*it)[ichar] != ' ') {
      ichar--;
      if (ichar < 0) break;     // no spaces 
    }
    if (ichar < 0) ichar=MAXCOMMENTLENGTH-1;    // if no spaces, let CFITSIO split at maxsize
    comlist.insert(it,it->substr(0,ichar));     // inserts substring immediately before iterator; iterator still points at original string
    it->erase(0,ichar+1);                       // erase substring (and dividing space) from large string
    it--;                                       // decrement iterator so large string gets processed again
  }

  // write comment to FITS file
  for (it=comlist.begin(); it != comlist.end(); it++) {
    int status=0;
    char* tcomment=(char*)(it->c_str());
    if (0 != fits_write_comment(ahffp->m_cfitsfp,tcomment,&status)) {
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write comment to FITS header: "+comment+
                       statusMsg(status));
    }
  }
}

// -----------------------------------------------------------------------------

// note: passing string by value on purpose so the local value can be modified
void writeKeyHistory(FilePtr ahffp, std::string history) {
  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // cannot write to read-only files
  if (ahffp->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"file opened as read-only; cannot write");

  // stamp parameters to header (only does it once)
  ahfits::stamp(ahffp);

  // Long history lines may need to be split over several lines.  Arrange given
  // history string into a list of strings, each small enough to fit on a
  // single line.  The original history string will only be split at spaces.
  // New line characters (\n) will also be searched for.
  std::list<std::string> comlist;

  // look for new lines
  std::string eol="\n";
  while (1) {
    std::size_t index=history.find(eol);
    if (std::string::npos == index) break;         // end-of-line character not found
    comlist.push_back(history.substr(0,index));    // get all but the new line character
    history.erase(0,index+1);                      // erase segment including new line character
  }
  comlist.push_back(history);                      // rest of history goes onto list

  // make sure each line is short enough
  std::list<std::string>::iterator it;
  for (it=comlist.begin(); it != comlist.end(); it++) {
    if (it->size() <= (std::size_t)MAXCOMMENTLENGTH) continue;

    // look for first space backwards from max size
    std::size_t ichar=MAXCOMMENTLENGTH-1;
    while ((*it)[ichar] != ' ') {
      ichar--;
      if (ichar < 0) break;     // no spaces 
    }
    if (ichar < 0) ichar=MAXCOMMENTLENGTH-1;    // if no spaces, let CFITSIO split at maxsize
    comlist.insert(it,it->substr(0,ichar));     // inserts substring immediately before iterator; iterator still points at original string
    it->erase(0,ichar+1);                       // erase substring (and dividing space) from large string
    it--;                                       // decrement iterator so large string gets processed again
  }

  // write history to FITS file
  for (it=comlist.begin(); it != comlist.end(); it++) {
    int status=0;
    char* thistory=(char*)(it->c_str());
    if (0 != fits_write_history(ahffp->m_cfitsfp,thistory,&status)) {
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to write history string to FITS header: "+thistory+
                       statusMsg(status));
    }
  }
}

// -----------------------------------------------------------------------------

void copyKey(FilePtr ahffp_src, FilePtr ahffp_dest, const std::string & key) {

  // Check inputs.
  if (0 == ahffp_src || 0 == ahffp_src->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for source");
  if (0 == ahffp_dest || 0 == ahffp_dest->m_cfitsfp) 
    AH_THROW_LOGIC("null pointer passed for destination");

  // cannot write to read-only files
  if (ahffp_dest->m_readonly)
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_dest,true,false)+"file opened as read-only; cannot write");

  int status=0;
  char card[FLEN_CARD];
  if (0 != fits_read_card(ahffp_src->m_cfitsfp,key.c_str(),card,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to read header keyword: "+
                     key+" from source"+statusMsg(status));
  }
  if (0 != fits_update_card(ahffp_dest->m_cfitsfp,key.c_str(),card,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_src,true,false)+"failed to write header keyword: "+
                     key+" to destination"+statusMsg(status));
  }
}

// -----------------------------------------------------------------------------

void stamp(FilePtr & ahffp) {

  // This function is not checking if ahffp refers to an open FITS file or
  // if that file is read-only.  This should be okay since this function is
  // intended for internal ahfits access only.

  if (!ahffp->m_stamppar) return;
  if (0 == ahffp->m_stamped.count(ahffp->m_hduidx)) {
    int status=0;
    HDpar_stamp(ahffp->m_cfitsfp,0,&status);  // 2nd argument = 0 => stamp active HDU
    if (status > 0) {
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"failed to stamp parameters to file"+
                       statusMsg(status));
    }
    ahffp->m_stamped.insert(ahffp->m_hduidx);
  }
}

// -----------------------------------------------------------------------------

} // namespace ahfits

/* Revision Log
   $Log: ahfits_header.cxx,v $
   Revision 1.21  2015/07/29 01:10:12  mwitthoe
   ahfits: add new function to copy a header keyword from one HDU to another regardless of value type: ahfits::copyKey()

   Revision 1.20  2015/05/18 14:31:08  asargent
   Added modifyKeyComment and setColumnDescription functions

   Revision 1.19  2015/04/02 15:03:21  mwitthoe
   ahfits: add checks for NULL ahfits or cfitsio FITS pointers; see issue 461

   Revision 1.18  2014/08/05 12:06:39  mwitthoe
   ahfits: make a couple implicit type conversions explicit to avoid compiler error

   Revision 1.17  2014/01/29 21:45:51  mwitthoe
   ahfits: allow HISTORY keywords to be automatically split across several lines if too long; see issue 332

   Revision 1.16  2014/01/14 21:33:49  mwitthoe
   ahfits: update writeKeyComments() to automatically split lines based on the maximum COMMENT size and new line characters (\n); this routine will split the comment at space characters whereas cfitsio just divides at the max size; see issue 332

   Revision 1.15  2013/12/10 22:08:16  mwitthoe
   ahfits: add functions to copy keywords from one HDU to another, e.g. copyKeyDbl()

   Revision 1.14  2013/10/04 21:20:46  mwitthoe
   ahfits library: previous version of writing buffer information to the FITS header would fail if a keyword was written to the output file before making a table connection; this has been fixed by moving the recording of buffer information to FITS files from stamp() to connect_generic()

   Revision 1.13  2013/10/04 15:39:09  mwitthoe
   ahfits: add functions to write HISTORY and COMMENT header values; add functions to get buffer size from connections; ahfits will now log all buffer sizes to the log file and non-READONLY FITS files

   Revision 1.12  2013/09/16 20:09:47  klrutkow
   updated keywordExists function and its doxygen

   Revision 1.11  2013/09/09 17:43:09  mwitthoe
   ahfits library: added function, keywordExists(), to check if a keyword is present in the header; added test case to test function

   Revision 1.10  2013/07/12 13:30:33  mwitthoe
   ahfits: write function to provide standard error message prefix; use prefix throughout ahfits library

   Revision 1.9  2013/07/11 17:07:20  mwitthoe
   ahfits: add hdu index as a member of FilePtr in order to eliminate access to the disk in the stamp() function; stamp() is potentially called with every writeRow() call and, with buffering, the whole idea is not to access the file all the time.

   Revision 1.8  2013/07/10 18:25:58  mwitthoe
   ahfits: add flag to FilePtr structure indicating if parametes should be written to the FITS header for modified files; the default value is set by the current global history state in ahgen; functions are added to ahfits to change the flag for an individual FITS file

   Revision 1.7  2013/03/09 04:30:59  peachey
   Remove some unnecessary dynamic memory allocations in favor
   of automatic variables. Other misc. clean-up.

   Revision 1.6  2012/12/07 23:00:11  mwitthoe
   make changes to ahfits recommended by Dec 7 code review (see http://zuul:8080/redmine/projects/astroh/wiki/2012-12-07_Ahfits_Code_Review)

   Revision 1.5  2012/11/05 01:35:50  mwitthoe
   add m_readonly member to AhFitsFile struct to mark a FITS file as read-only; all writing routines (row and headers) now check the readonly status before writing, but there is no way currently to set the read-only false to true (except as the backup readonly fits_open_file call in ahfits::open

   Revision 1.4  2012/11/04 23:25:20  mwitthoe
   ahfits: add cfitsio error messages based on error status; made clone/create functions that return a fits pointer to the newly created FITS file

   Revision 1.3  2012/10/24 15:00:07  mwitthoe
   ahfits: move stamp() from ahfits_file to ahfits_header; new function addHDU in ahfits_file which will make a new (empty) extension based on the header of an existing extension from the same or different file

   Revision 1.2  2012/10/11 17:56:56  mwitthoe
   include parameter stamping in ahfits

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/
