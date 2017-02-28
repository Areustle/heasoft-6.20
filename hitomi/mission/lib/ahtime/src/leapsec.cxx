/// \file leapsec.cxx
/// \brief Load and store leap second table from CALDB FITS file
/// \author Mike Witthoeft
/// \date $Date: 2015/10/02 20:09:57 $

#define AHLABEL ahtime_leapsec
#define AHCVSID "$Id: leapsec.cxx,v 1.4 2015/10/02 20:09:57 mwitthoe Exp $"

#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahtime/leapsec.h"

#include "fitsio.h"
#include "hdcal.h"

#include <sstream>
#include <stdlib.h>
#include <string.h>
#include <cmath>

// ===========================================================================

namespace ahtime {

namespace leapsec {

// ---------------------------------------------------------------------------

void load(const std::string & leapsecfile, LeapSecTable & dat) {

  // initialize data
  ahtime::leapsec::clear(dat);

  // open leapsecond file
  int status=0;
  fitsfile* fptr=0;
  if (0 != fits_open_file(&fptr,leapsecfile.c_str(),READONLY,&status)) {
    AH_THROW_RUNTIME("unable to open leap second file for reading: "+leapsecfile);
  }

  // move to first extension of leapsecond file (index=2)
  int hdutype=0;                    // to check extension type after moving
  if (0 != fits_movabs_hdu(fptr,2,&hdutype,&status)) {
    AH_THROW_RUNTIME("unable to move to first extension of leap second file");
  }
  if (hdutype != BINARY_TBL) {
    AH_THROW_RUNTIME("first extension of leap second file is not a binary table");
  }

  // get number of rows in leapsecond file
  long nrows=0;
  if (0 != fits_read_key(fptr,TLONG,"NAXIS2",&nrows,0,&status)) {
    AH_THROW_RUNTIME("unable to read NAXIS2 keyword from leap second file");
  }

  // get column number for DATE column
  char* colname_date=const_cast<char*>("DATE");
  int colnum_date=0;
  if (0 != fits_get_colnum(fptr,CASEINSEN,colname_date,&colnum_date,&status)) {
    AH_THROW_RUNTIME("unable to get column number for DATE column in leapsecond file");
  }

  // get column number for MJD column
  char* colname_mjd=const_cast<char*>("MJD");
  int colnum_mjd=0;
  if (0 != fits_get_colnum(fptr,CASEINSEN,colname_mjd,&colnum_mjd,&status)) {
    AH_THROW_RUNTIME("unable to get column number for MJD column in leapsecond file");
  }

  // get column numbers for LEAPSECS column
  char* colname_leapsecs=const_cast<char*>("LEAPSECS");
  int colnum_leapsecs=0;
  if (0 != fits_get_colnum(fptr,CASEINSEN,colname_leapsecs,&colnum_leapsecs,&status)) {
    AH_THROW_RUNTIME("unable to get column number for LEAPSECS column in leapsecond file");
  }


  // read leap second file
  for (int irow=1; irow <= nrows; irow++) {

    // column variable
    char date[20];
    char* pdate=date;
    double mjd;
    double fleapsecs;

    // read DATE column
    if (0 != fits_read_col(fptr,TSTRING,colnum_date,irow,1,1,0,&pdate,0,&status)) {
      std::stringstream msg;
      msg << "unable to read DATE column for row " << irow;
      AH_THROW_RUNTIME(msg.str());
    }

    // read MJD column
    if (0 != fits_read_col(fptr,TDOUBLE,colnum_mjd,irow,1,1,0,&mjd,0,&status)) {
      std::stringstream msg;
      msg << "unable to read MJD column for row " << irow;
      AH_THROW_RUNTIME(msg.str());
    }

    // read LEAPSECS column
    if (0 != fits_read_col(fptr,TDOUBLE,colnum_leapsecs,irow,1,1,0,&fleapsecs,0,&status)) {
      std::stringstream msg;
      msg << "unable to read LEAPSECS column for row " << irow;
      AH_THROW_RUNTIME(msg.str());
    }

    // convert date string into integer
    char bufyear[5];    // 4 digits + null character
    char bufmonth[3];   // 2 digits + null character
    char bufday[3];     // 2 digits + null character
    int dateint=0;
    strncpy(bufyear,date,4);
    bufyear[4]='\0';
    strncpy(bufmonth,date+5,2);
    bufmonth[2]='\0';
    strncpy(bufday,date+8,2);
    bufday[2]='\0';
    dateint=10000*atoi(bufyear)+100*atoi(bufmonth)+atoi(bufday);

    // leap second should be 0 or +/- 1
    if (std::abs(fleapsecs) != 1. && fleapsecs != 0.) 
      AH_THROW_RUNTIME("leap second file has invalid number of leapseconds; must be 0, +/- 1");
    int ileap=0;    // leap second value as an integer
    if (0. == fleapsecs) {
      ileap=0;
    } else if (+1. == fleapsecs) {
      ileap=1;
    } else if (-1. == fleapsecs) {
      ileap=-1;
    }
    dat.cal[dateint]=ileap;
    dat.mjd[mjd]=ileap;

  }

  // close leap second FITS file
  if (0 != fits_close_file(fptr,&status)) {
    AH_THROW_RUNTIME("unable to close leapseconds file");
  }

}

// ---------------------------------------------------------------------------

bool isLoaded(LeapSecTable & dat) {
  if (dat.cal.size() > 0) return true;
  return false;
}

// ---------------------------------------------------------------------------

void clear(LeapSecTable & dat) {
  dat.cal.clear();
  dat.mjd.clear();
}

// ---------------------------------------------------------------------------

}  // namespace leapsec

}  // namespace ahtime

/* Revision Log
 $Log: leapsec.cxx,v $
 Revision 1.4  2015/10/02 20:09:57  mwitthoe
 ahtime library: remove redundant CALDB resolve functions; see issue 535

 Revision 1.3  2015/07/06 04:01:01  klrutkow
 made string comparisons case-insensitive ; added more chatter to search

 Revision 1.2  2015/03/18 17:38:02  asargent
 Changed detname to detnam

 Revision 1.1  2014/09/10 02:43:23  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv



 ==== Messages below refer to old location: mission/lib/ahmission ====

 Revision 1.6  2014/07/28 19:13:49  mwitthoe
 ahmission/leapsec: add support for REFDATA using new function implemented through issue #324; now move to first binary table of leap second file instead of LEAPSEC extension since the leapsec file in REFDATA has no extension name

 Revision 1.5  2013/10/14 16:32:28  mwitthoe
 ahmission::leapsec: change how leap second table is stored; the old way used std::strings which slow down the ahtime library due to a bunch of string comparisons; new way uses an integer (10000*year+100*month+day) to store the leap second date making comparisons much faster; also changed ut_ahmission to access the proc_status.fits test file as READONLY instead of READWRITE since the latter causes buffering information to be written to the file

 Revision 1.4  2013/10/08 14:02:42  mwitthoe
 ahmission library: switch over to new ahfits connect functions (see issue 270)

 Revision 1.3  2013/04/11 18:47:06  mwitthoe
 update former ahcaldb libraries now residing in ahmission: camstoffset, delay, and leapsec; changed namespace, add read/write flags to ahfits connections, removed static instance of DataType holding CALDB data in memory

 Revision 1.2  2012/12/10 18:13:32  mwitthoe
 in ahcaldb, revert to using old versions of open/create which return void instead of FilePtr

 Revision 1.1  2012/11/15 02:24:23  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.19  2012/11/13 18:23:25  mwitthoe
 ahtime library: use new open/create/clone functions from ahfits; change time delay CALDB file and implementation to match latest Oct 31 TRF; change column definitions CALDB file and implementation to match Oct 31 TRF; read and use SXS lookup tables in time assignment; use new instrument names for HXI1/2 and SGD1/2

 Revision 1.18  2012/10/12 22:52:38  mwitthoe
 align ahtime library with latest version of ahfits

 Revision 1.17  2012/10/11 18:05:16  mwitthoe
 convert ahtime library over to new ahfits

 Revision 1.16  2012/09/26 20:32:42  mwitthoe
 check if CALDB data is loaded before trying to read FITS file

 Revision 1.15  2012/09/26 20:12:21  mwitthoe
 apply new CALDB library standards to ahdelay, ahleapsec, and ahcolumndef in the ahtime library

 Revision 1.14  2012/09/13 18:16:58  mwitthoe
 clean up of ahtime library source files

 Revision 1.13  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

 Revision 1.12  2012/08/28 21:34:44  mwitthoe
 changes to ahtime library: clean up; change leap second value from double to int; fix memory leak

 Revision 1.11  2012/08/21 00:18:20  mwitthoe
 tweaks to ahleapsec library and conform ahdelay library to standard for CALDB access libraries

 Revision 1.10  2012/08/18 02:38:26  mwitthoe
 apply CALDB standards to ahleapsec library

 Revision 1.9  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.8  2012/08/15 16:11:53  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.7  2012/08/15 15:24:13  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.6  2012/08/07 17:56:25  mwitthoe
 explicity check if CALDB environment variable is NULL before querying caldb; this is to avoid a possible bug in HDgtcalf (segmentation fault) occurring on Ron's machine, grok

 Revision 1.5  2012/07/17 22:36:36  mwitthoe
 allow ahtime tool to access leap second file from REFDATA

 Revision 1.4  2012/07/13 18:32:08  mwitthoe
 clean up ahtime code


*/
