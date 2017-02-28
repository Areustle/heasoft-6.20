/// \file delay.cxx
/// \brief functions to act on the CALDB instrument delay file for AstroH.
/// \author Mike Witthoeft
/// \date $Date: 2016/03/18 15:04:29 $
 
#define AHLABEL ahmission_delay
#define AHCVSID "$Id: delay.cxx,v 1.12 2016/03/18 15:04:29 asargent Exp $"

#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "ahmission/delay.h"

#include "hdcal.h"                // CALDB query utilities

#include <iostream>
#include <fstream>
#include <string.h>
#include <sstream>
#include <cmath>

// ===========================================================================

namespace ahmission {

namespace delay {

// ---------------------------------------------------------------------------

/// \callgraph
void load(const std::string & filename, const std::string& extname, DataType & dat) {

  // open file
  ahfits::FilePtr fptr;
  ahfits::open(filename,extname,&fptr);
  if (!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open instrument delay FITS file: "+filename);
  }

  // initialize data structure
  ahmission::delay::clear(dat);
  dat.m_icur=1;          // always start with 2nd time (index: 1)
  dat.m_lasttime=-1.;

  // setup router
  ahfits::Router router(fptr);

  // make connections to local variables
  double l_time;
  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  double l_delay1;
  router.connectScalar(ahfits::e_READONLY,"DELAY1",l_delay1);
  double l_delay2;
  router.connectScalar(ahfits::e_READONLY,"DELAY2",l_delay2);

  // read in table (assume sorted)
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);
    dat.m_times.push_back(l_time);
    dat.m_delay1s.push_back(l_delay1);
    dat.m_delay2s.push_back(l_delay2);
  }

  // close FITS file
  ahfits::close(fptr);
}

// ---------------------------------------------------------------------------

void loadAndGet(const std::string & filename, const std::string& extname,
                double time, double& delay1, double& delay2) {

  // open file
  ahfits::FilePtr fptr;
  ahfits::open(filename,extname,&fptr);
  if (!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open instrument delay FITS file: "+filename);
  }

  // setup router
  ahfits::Router router(fptr);

  // make connections to local variables
  double l_time=0.;
  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  double l_delay1=0.;
  router.connectScalar(ahfits::e_READONLY,"DELAY1",l_delay1);
  double l_delay2=0.;
  router.connectScalar(ahfits::e_READONLY,"DELAY2",l_delay2);

  // store time from previous row
  double p_time=-1.;

  // read in table (assume sorted)
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);

    if (l_time > time) break;
    p_time=l_time;
    delay1=l_delay1;
    delay2=l_delay2;
  }

  // close FITS file
  ahfits::close(fptr);

  // If 1st row had a time larger than given time, then return delay
  // values from 1st row
  if (p_time < 0.) {
    delay1=l_delay1;
    delay2=l_delay2;
  }

}

// ---------------------------------------------------------------------------

void clear(DataType & dat) {
  dat.m_times.clear();
  dat.m_delay1s.clear();
  dat.m_delay2s.clear();
  dat.m_icur=1;
  dat.m_lasttime=-1.;
}

// ---------------------------------------------------------------------------

/// \callgraph
double get(DataType & dat, double time, int idx) {

  if (dat.m_times.empty())
    AH_THROW_RUNTIME("delay data not loaded");

  // check if time out of range
  if (time < dat.m_times[0])
    AH_THROW_RUNTIME("given time out-of-range (too small)");
  if (time > dat.m_times.back())
    AH_THROW_RUNTIME("given time out-of-range (too large)");

  // if only one point in file, return that delay
  if (1 == dat.m_times.size()) {
    AH_INFO(ahlog::HIGH) << " *** only one time delay in CALDB, no interpolation"
                         << std::endl;
    if (idx == 1) return dat.m_delay1s[0];
    return dat.m_delay2s[0];
  }

  // reset starting index if given time is smaller than previous
  if (time < dat.m_lasttime) {
    dat.m_icur=1;      // always start with 2nd time (index: 1)
  }
  dat.m_lasttime=time;

  // find where time fits in times list
  int j;
  for (j=dat.m_icur; j < (int)dat.m_times.size()-1; j++) {
    if (time < dat.m_times[j]) break;
  }
  dat.m_icur=j;
  if (idx == 1) return dat.m_delay1s[j-1];
  return dat.m_delay2s[j-1];
}

// ---------------------------------------------------------------------------

/// \callgraph
void write_delay_template(const std::string & tplfile) {

  std::vector<std::string> ext;
  ext.push_back("SXS");
  ext.push_back("HXI");
  ext.push_back("SGD");
  ext.push_back("SXI");

  std::ofstream out;
  out.open(tplfile.c_str());
  out << "# Template file for instrument delay FITS file" << std::endl;
  out << "#" <<std::endl;
  out << "# each extension has two columns: " << std::endl;
  out << "#   1) time since epoch in seconds" << std::endl;
  out << "#   2) instrument delay in seconds" << std::endl;
  out << "# " << std::endl;
  out << "" << std::endl;

  std::vector<std::string>::iterator it;
  for (it=ext.begin(); it < ext.end(); it++) {
    out << "" << std::endl;
    out << "# ===== " << *it << " =====" << std::endl;
    out << "xtension = bintable" << std::endl;
    out << "extname = '" << *it << "'" << std::endl;
    out << "naxis2 = 0" << std::endl;
    out << "" << std::endl;
    out << "ttype# = TIME" << std::endl;
    out << "tform# = D" << std::endl;
    out << "tunit# = 's'" <<std::endl;
    out << "" << std::endl;
    out << "ttype# = DELAY1" << std::endl;
    out << "tform# = D" << std::endl;
    out << "tunit# = 's'" <<std::endl;
    out << "" << std::endl;
    out << "ttype# = DELAY2" << std::endl;
    out << "tform# = D" << std::endl;
    out << "tunit# = 's'" <<std::endl;
    out << "" << std::endl;
  }

  out.close();
}

// ---------------------------------------------------------------------------

/// \callgraph
void create_delay_fits(const std::string & filename, 
                       const std::string & tplfile) {

  // make FITS file from template
  std::string tfile='!'+filename;
  ahfits::FilePtr fptr;
  ahfits::create(tfile,tplfile,&fptr);
  ahfits::close(fptr);

}

// ---------------------------------------------------------------------------

/// \callgraph
void write_delay_times(const std::string & filename, const std::string & inst,
                       const std::vector<double> times,
                       const std::vector<double> delay1s,
                       const std::vector<double> delay2s) {

  // check if times and delays arrays are of equal size
  if (times.size() != delay1s.size() || times.size() != delay2s.size()) {
    AH_THROW_RUNTIME("arrays for times and delays have different lengths");
  }

  // open file
  ahfits::FilePtr fptr;
  ahfits::open(filename,inst,&fptr);

  // make connections with local variable
  double time=0;
  double delay1=0.0;
  double delay2=0.0;
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_WRITEONLY,"TIME",time);
  router.connectScalar(ahfits::e_WRITEONLY,"DELAY1",delay1);
  router.connectScalar(ahfits::e_WRITEONLY,"DELAY2",delay2);

  int nrow=times.size();
  for (int irow=0; irow < nrow; irow++) {
    time=times[irow];
    delay1=delay1s[irow];
    delay2=delay2s[irow];
    ahfits::writeRow(fptr);
    ahfits::nextRow(fptr);
  }

  // close FITS file
  ahfits::close(fptr);
}

// ---------------------------------------------------------------------------

}  // namespace delay

}  // namespace ahmission

/* Revision Log
 $Log: delay.cxx,v $
 Revision 1.12  2016/03/18 15:04:29  asargent
 Replaced AH_WARN with AH_INFO

 Revision 1.11  2015/10/02 20:07:57  mwitthoe
 ahmission library: remove redundant CALDB resolve functions; see issue 535

 Revision 1.10  2015/07/06 14:09:07  klrutkow
 added resolve function to get CALDB filename

 Revision 1.9  2014/07/08 19:35:58  mwitthoe
 ahmission library: give the instrument argument of procstatus::processRow() a default value of zero; all instrument-specific behavior has been removed from this function; a new function, loadAndGet(), has been added to delay.h/cxx which will read delay times from the given CALDB file corresponding to the given time since there is no need anymore to keep the whole delay table in memory; the old functions are still present, but will be removed once it is sure that they will not be needed (in Build 6)

 Revision 1.8  2014/02/14 01:49:21  peachey
 Check for empty container (no times loaded) in get() method.

 Revision 1.7  2013/10/08 14:02:42  mwitthoe
 ahmission library: switch over to new ahfits connect functions (see issue 270)

 Revision 1.6  2013/08/30 20:17:46  mwitthoe
 ahmission:delay - since instrument delays are only needed for one instrument at a time, only load that instrument's delays instead of the entire instrument delay FITS file (see issue 236)

 Revision 1.5  2013/07/01 14:05:00  mwitthoe
 ahmission::delay.cxx: fix deprecated call to ahfits::create()

 Revision 1.4  2013/04/11 18:47:06  mwitthoe
 update former ahcaldb libraries now residing in ahmission: camstoffset, delay, and leapsec; changed namespace, add read/write flags to ahfits connections, removed static instance of DataType holding CALDB data in memory

 Revision 1.3  2013/03/29 19:50:16  mwitthoe
 ahcaldb/delay: use strings from the INSTRUME keyword instead of the ahmission enumerations

 Revision 1.2  2012/12/10 18:13:32  mwitthoe
 in ahcaldb, revert to using old versions of open/create which return void instead of FilePtr

 Revision 1.1  2012/11/15 02:24:23  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.26  2012/11/13 18:23:25  mwitthoe
 ahtime library: use new open/create/clone functions from ahfits; change time delay CALDB file and implementation to match latest Oct 31 TRF; change column definitions CALDB file and implementation to match Oct 31 TRF; read and use SXS lookup tables in time assignment; use new instrument names for HXI1/2 and SGD1/2

 Revision 1.25  2012/11/08 03:03:52  mwitthoe
 change instrument delay CALDB file to match timing TRF (Oct 31); implemented instrument delays in time assignment library; empty HKEXTNAME value for SXI row in the column definitions FITS file

 Revision 1.24  2012/11/01 20:03:02  mwitthoe
 remove clobber arguments from ahfits create/clone calls from ahtime library and unit tests

 Revision 1.23  2012/10/24 18:02:30  mwitthoe
 add CALDB libary for frequency vs. temperature data (missing algorithm for loading/retrieving data

 Revision 1.22  2012/10/18 17:35:40  mwitthoe
 ahtime library: created single header file, ahtime.h, to include entire ahtime library (old ahtime -> ahtime_base); separated CAMS data from instrument delay CALDB file into new file: associated libraries created

 Revision 1.21  2012/10/12 22:52:38  mwitthoe
 align ahtime library with latest version of ahfits

 Revision 1.20  2012/10/11 18:05:16  mwitthoe
 convert ahtime library over to new ahfits

 Revision 1.19  2012/09/26 20:32:42  mwitthoe
 check if CALDB data is loaded before trying to read FITS file

 Revision 1.18  2012/09/26 20:12:21  mwitthoe
 apply new CALDB library standards to ahdelay, ahleapsec, and ahcolumndef in the ahtime library

 Revision 1.17  2012/09/26 19:00:45  mwitthoe
 apply new CALDB library standards to ahdelay; use ahgen::isEqual in unit tests to compare doubles; add CAMS data to instrument delay file and use it in time assignment library instead of hard-coded values

 Revision 1.16  2012/09/18 01:47:45  mwitthoe
 change interpolation in ahdelay to use only doubles

 Revision 1.15  2012/09/13 18:16:58  mwitthoe
 clean up of ahtime library source files

 Revision 1.14  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

 Revision 1.13  2012/09/07 17:39:30  mwitthoe
 remove obsolete include to ahtest in ahtime source files

 Revision 1.12  2012/09/05 18:26:10  mwitthoe
 switch column definition library to new CALDB library format; switch over to using ahmission enumerations in ahdelay

 Revision 1.11  2012/08/29 22:12:24  mwitthoe
 in ahtime library, move unit test functions from src to test

 Revision 1.10  2012/08/28 21:34:44  mwitthoe
 changes to ahtime library: clean up; change leap second value from double to int; fix memory leak

 Revision 1.9  2012/08/27 19:37:51  mwitthoe
 add marker in ahdelay to keep track of current position in time/delay list (increase search efficiency); add unit test for ahdelay

 Revision 1.8  2012/08/24 18:53:22  mwitthoe
 clean up argument lists in ahtime library

 Revision 1.7  2012/08/21 00:18:20  mwitthoe
 tweaks to ahleapsec library and conform ahdelay library to standard for CALDB access libraries

 Revision 1.6  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.5  2012/08/15 16:11:53  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.4  2012/08/15 15:24:13  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.3  2012/07/13 18:32:08  mwitthoe
 clean up ahtime code


*/
