/// \file ahtimelib.cxx
/// \brief functions ahtime
/// \author Mike Witthoeft
/// \date $Date: 2016/03/18 15:09:58 $

#define AHLABEL tool_ahtime_ahtimelib
#define AHCVSID "$Id: ahtimelib.cxx,v 1.10 2016/03/18 15:09:58 asargent Exp $"

#include "ahtimelib.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahmath/ahmath.h"

#include <iostream>
#include <fstream>
#include <string.h>
#include <sstream>

// =============================================================================
// General functions
// =============================================================================


// ---------------------------------------------------------------------------

long long u32tol32(long long u32ti) {
  return (u32ti<<6)&0xFFFFFFFF;
}

// ---------------------------------------------------------------------------

void initLookupTable(LookupTable& table, long size, double ymax) {
  if (size < 1) AH_THROW_RUNTIME("can only allocate lookup tables with positive size");
  clearLookupTable(table);     // for safety

  table.m_size=size;
  table.m_ymax=ymax;
  table.m_xdat=new double[size];
  table.m_ydat=new double[size];
  for (int i=0; i < size; i++) {
    table.m_xdat[i]=0.;
    table.m_ydat[i]=0.;
  }
}

// ---------------------------------------------------------------------------

void clearLookupTable(LookupTable& table) {
  table.m_size=0;
  table.m_ymax=0.;
  if (table.m_xdat != 0) delete [] table.m_xdat;
  if (table.m_ydat != 0) delete [] table.m_ydat;
  table.m_xdat=0;
  table.m_ydat=0;
  table.m_luidx=0;
}

// ---------------------------------------------------------------------------

double lookupTIM(ahmission::timfile::TimFileData& timdat, double stime,
                 double fineti, double l32timax, int interp,
                 unsigned long& timidx, bool& extrap, bool& badtim,
                 char* ptr_proc_status) {

  double time=-1.;    // output TIME; bad if negative
  badtim=false;       // reset badtime

  // search for S_TIME position in TIM file
  // This search is in the first TIM lookup table; a check for an illegal
  // STATUS value is made below (before interpolation to get TIME)
  //
  // Note: the 8th argument of search_sawY2X which is set to 0.5 causes
  // the function to search for a solution in an adjacent sawtooth if
  // the L32TI nearest to the input S_TIME is more than 0.5*l32timax 
  // away than the input fine-TI.
  extrap=false;          // used to check if routine will extrapolate
  try {
    timidx=ahmath::search_sawY2X(stime,fineti,l32timax,timdat.m_time1,
                                 timdat.m_l32ti1,timdat.m_size1,0.5,extrap,
                                 timidx);
  } catch (std::exception& e) {
    AH_INFO(ahlog::HIGH) << "TIM file search failed: " << e.what() << std::endl;
    badtim=true;
    return time;
  }
  AH_DEBUG << "  Search position in TIM file: " << timidx << std::endl;

  // timidx now should refer to the TIM value with the largest TIME value 
  // which is smaller than S_TIME; meaning that timidx and timidx+1 will
  // bracket S_TIME.  Need to check if either of these points has an illegal
  // STATUS, in which case set TIME=NULL.
  if (ahmission::timfile::isTimStatusIllegal(timdat.m_status1[timidx]) || 
      ahmission::timfile::isTimStatusIllegal(timdat.m_status1[timidx+1])) {
    AH_INFO(ahlog::HIGH) << "Input adjacent to bad TIM row" << std::endl;
    badtim=true;
  } else {    // get index in valid-only TIM lookup table and interpolate
    unsigned long timidxp=timdat.m_map[timidx];
    time=ahmath::interpolate_sawY2X(fineti,timdat.m_time2,timdat.m_l32ti2,
                                    timdat.m_size2,interp,timidxp);
  }

  // If not given a NULL pointer, copy TIM statuses to PROC_STATUS variable.
  if (ptr_proc_status != 0) {
    setProcStatusCopyTimStatus(ptr_proc_status,timdat.m_status1[timidx],
                               timdat.m_status1[timidx+1]);
  }

  return time;

}

// ---------------------------------------------------------------------------

void calcFineTI_SXI(long long irow, const std::string& optype, const SXIParams& sxipars,
                    long long l32ti, double& stime, int rawy, double ltime1evt,
                    char& ltime1evtnull, char& timenull, double& fineti,
                    Counters& count_ext) {

  // For SXI, fine-TI is calculated by combining the leading bits of L32TI with
  // SEQ_START_TIME.
  if (ltime1evtnull != 0) {
    count_ext.num_timenull++;
    timenull=1;
    AH_INFO(ahlog::LOW) << "  row " << irow << "; LOCAL_TIME is NULL, setting TIME=NULL" << std::endl;
  } else {
    fineti=sxipars.m_seqfac*ltime1evt+(double)(l32ti & sxipars.m_timask);
    AH_DEBUG << "  SXI fine-TI from SEQ_START_TIME: " << fineti << std::endl;
  }

  // For SXI in windowing mode, need to adjust fine-TI based on
  // the window frame (identified by the RAWY value).  S_TIME is
  // also adjusted.
  if (optype == "SXI" && timenull==0) {
    int ibin=rawy/sxipars.m_dely;    // window frame index
    double shift=sxipars.m_subcycleshift+sxipars.m_subcycletime*ibin;
    if (sxipars.m_nexp > 1 && ibin == sxipars.m_nexp-1)     // last exposure a little longer in windowing mode
      shift+=sxipars.m_adjlasttime;
    AH_DEBUG << "  SXI windowing mode shift: " << shift << std::endl;
    fineti+=64.*shift;     // convert from seconds to L32TI units
    stime+=shift;
    AH_DEBUG << "  SXI windowing mode adjustment: S_TIME, fine-TI = " << stime << ", " << fineti << std::endl;
  }
}

// ---------------------------------------------------------------------------

void calcFineTI_Lookup(long long irow, double ltime1evt, char& ltime1evtnull,
                       char& timenull, LookupTable& lutable, int interp,
                       double& fineti, Counters& count_ext) {

  if (ltime1evtnull != 0) {
    count_ext.num_timenull++;
    timenull=1;
    AH_INFO(ahlog::LOW) << "  row " << irow << "; LOCAL_TIME is NULL, setting TIME=NULL" << std::endl;
  } else {
    // For HXI/SGD, as a consequence of precision differences between the 
    // HK and event local times, we need to make sure that the event local
    // time has the same maximum value as the HK look up table.  This is done
    // via modulus.
    double ltime1evt_adj=fmod(ltime1evt,lutable.m_ymax);

    bool extrap=false;
    try {
      lutable.m_luidx=ahmath::search_sawY2X(fineti,ltime1evt_adj,lutable.m_ymax,
                              lutable.m_xdat,lutable.m_ydat,lutable.m_size,0.5,
                              extrap,lutable.m_luidx);
    } catch (std::exception& e) {
      timenull=1;
      AH_INFO(ahlog::HIGH) << "FineTI search failed for row " << irow << " (setting TIME=NULL): " << e.what() << std::endl;
      return;
    }

    fineti=ahmath::interpolate_sawY2X(ltime1evt_adj,lutable.m_xdat,lutable.m_ydat,
                                      lutable.m_size,interp,lutable.m_luidx);
    AH_DEBUG << "  Lookup table: search position: " << lutable.m_luidx << std::endl;

    if (extrap) {
      AH_INFO(ahlog::HIGH) << " *** Extrapolating to get Fine TI from lookup table for row " 
                           << irow << std::endl;
    }
  }

}

// ---------------------------------------------------------------------------

void addDelayToGTI(EventDat& rowdat, double delay) {
  if (rowdat.m_startnull == 0) rowdat.m_start+=delay;
  if (rowdat.m_stopnull == 0) rowdat.m_stop+=delay;
}

// ---------------------------------------------------------------------------

void setProcStatusOutOfOrder(char* val_proc_status) {
  val_proc_status[21]=1;
}

// ---------------------------------------------------------------------------

void setProcStatusCopyTimStatus(char* val_proc_status, char* status1,
                                char* status2) {
  // first 5 bits of status1
  val_proc_status[22]=status1[0];
  val_proc_status[23]=status1[1];
  val_proc_status[24]=status1[2];
  val_proc_status[25]=status1[3];
  val_proc_status[26]=status1[4];

  // first 5 bits of status2
  val_proc_status[27]=status2[0];
  val_proc_status[28]=status2[1];
  val_proc_status[29]=status2[2];
  val_proc_status[30]=status2[3];
  val_proc_status[31]=status2[4];
}

// ---------------------------------------------------------------------------

void setProcStatusGSFCBad(char* val_proc_status) {
  val_proc_status[16]=1;
}

// ---------------------------------------------------------------------------

void hashReplace(std::string& str, const std::string repl) {
  std::size_t found=str.find("#");
  if (found == std::string::npos) AH_THROW_RUNTIME("failed to find hash character: "+str);
  str.replace(found,1,repl);
}


// =============================================================================
// Functions to read column definitions CALDB file
// =============================================================================

namespace timecoldef {

// ---------------------------------------------------------------------------

void load(const std::string& filename, ColDefInfo& coldat) {

  /// constants
  std::string extname="TIMECOLDEF";
  std::string col_system="SYSTEM";
  std::string col_tisc="SciTI";
  std::string col_sendtime="SciS_Time";
  std::string col_ltime1evt="LTIME1EVT";
  std::string col_ltime1bits="BITS";
  std::string col_ltime2evt="LTIME2EVT";
  std::string col_hkextname="HKEXTNAME";
  std::string col_ltime1hk="LTIME1HK";
  std::string col_ltime2hk="LTIME2";
  std::string col_ti1hk="TI1HK";
  std::string col_ltfacthk="LTFACTHK";
  std::string col_ltfactbits="LTFACTBITS";
  std::string col_ltresevt="LTRESEVT";
  std::string col_ltreshk="LTRESHK";

  // local variables to connect with FITS columns
  std::string c_system;
  std::string c_tisc;
  std::string c_sendtime;
  std::string c_ltime1evt;
  int c_ltime1bits=0;
  std::string c_ltime2evt;
  std::string c_hkextname;
  std::string c_ltime1hk;
  std::string c_ltime2hk;
  std::string c_ti1hk;
  int c_ltfacthk=0;
  int c_ltfactbits=0;
  double c_ltresevt=0.;
  double c_ltreshk=0.;

  // open file
  ahfits::FilePtr fptr;
  ahfits::open(filename,extname,&fptr);
  if (!ahfits::readOK(fptr)) {
    std::stringstream msg;
    msg << "failed to open timecoldef FITS file: " << filename;
    AH_THROW_RUNTIME(msg.str());
  }

  // make connections
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_READONLY,col_system,c_system);
  router.connectScalar(ahfits::e_READONLY,col_tisc,c_tisc);
  router.connectScalar(ahfits::e_READONLY,col_sendtime,c_sendtime);
  router.connectScalar(ahfits::e_READONLY,col_ltime1evt,c_ltime1evt);
  router.connectScalar(ahfits::e_READONLY,col_ltime1bits,c_ltime1bits);
  router.connectScalar(ahfits::e_READONLY,col_ltime2evt,c_ltime2evt);
  router.connectScalar(ahfits::e_READONLY,col_hkextname,c_hkextname);
  router.connectScalar(ahfits::e_READONLY,col_ltime1hk,c_ltime1hk);
  router.connectScalar(ahfits::e_READONLY,col_ltime2hk,c_ltime2hk);
  router.connectScalar(ahfits::e_READONLY,col_ti1hk,c_ti1hk);
  router.connectScalar(ahfits::e_READONLY,col_ltfacthk,c_ltfacthk);
  router.connectScalar(ahfits::e_READONLY,col_ltfactbits,c_ltfactbits);
  router.connectScalar(ahfits::e_READONLY,col_ltresevt,c_ltresevt);
  router.connectScalar(ahfits::e_READONLY,col_ltreshk,c_ltreshk);

  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);

    // clear string if just a single space
    if (c_tisc == " ") c_tisc.clear();
    if (c_sendtime == " ") c_sendtime.clear();
    if (c_ltime1evt == " ") c_ltime1evt.clear();
    if (c_ltime2evt == " ") c_ltime2evt.clear();
    if (c_hkextname == " ") c_hkextname.clear();
    if (c_ltime1hk == " ") c_ltime1hk.clear();
    if (c_ltime2hk == " ") c_ltime2hk.clear();
    if (c_ti1hk == " ") c_ti1hk.clear();

    // add column names to output struct
    coldat[c_system]=new ColumnNames(c_tisc,c_sendtime,c_ltime1evt,
                     c_ltime1bits,c_ltime2evt,c_hkextname,c_ltime1hk,
                     c_ltime2hk,c_ti1hk,c_ltfacthk,c_ltfactbits,c_ltresevt,
                     c_ltreshk);
  }

  // close FITS file
  ahfits::close(fptr);

}

// ---------------------------------------------------------------------------

void get(ColDefInfo& coldat, const std::string& system, ColumnNames& colnames) {
  if (coldat.count(system) == 0) AH_THROW_RUNTIME("Invalid system name");
  colnames.m_tisc=coldat[system]->m_tisc;
  colnames.m_sendtime=coldat[system]->m_sendtime;
  colnames.m_ltime1evt=coldat[system]->m_ltime1evt;
  colnames.m_ltime1bits=coldat[system]->m_ltime1bits;
  colnames.m_ltime2evt=coldat[system]->m_ltime2evt;
  colnames.m_hkextname=coldat[system]->m_hkextname;
  colnames.m_ltime1hk=coldat[system]->m_ltime1hk;
  colnames.m_ltime2hk=coldat[system]->m_ltime2hk;
  colnames.m_ti1hk=coldat[system]->m_ti1hk;
  colnames.m_ltfacthk=coldat[system]->m_ltfacthk;
  colnames.m_ltfactbits=coldat[system]->m_ltfactbits;
  colnames.m_ltresevt=coldat[system]->m_ltresevt;
  colnames.m_ltreshk=coldat[system]->m_ltreshk;
}

// ---------------------------------------------------------------------------

}  // namespace timecoldef


/* Revision Log
 $Log: ahtimelib.cxx,v $
 Revision 1.10  2016/03/18 15:09:58  asargent
 Replaced AH_INFO with AH_WARN

 Revision 1.9  2015/10/13 23:47:19  mwitthoe
 ahtime tool: set TIME=NULL instead of throwing an error when there is a failure in the fine-TI lookup operation

 Revision 1.8  2015/10/13 21:07:42  mwitthoe
 ahtime: if TIM lookup fails in the index search, now set TIME=NULL instead of throwing an error

 Revision 1.7  2015/08/24 01:33:49  mwitthoe
 ahtime: 1) do not load column definitions file, delay file, or lookup tables if calctime is false; 2) re-read column names from column definitions file for each extension so that multiple SGD extensions in the HK file (CC1, CC2, CC3) can be processed correctly

 Revision 1.6  2015/08/19 14:45:17  mwitthoe
 ahtime tool: add support for HXI/SGD HISTOGRAM files; DETNAM for SGDshield files can be SHIELD1 or SHIELD2 (not just SHIELD)

 Revision 1.5  2015/08/11 02:04:55  mwitthoe
 ahtime tool: the HK and event LOCAL_TIME columns for HXI and SGD have different precisions, ahtime now converts the HK LOCAL_TIME into the units of the vent LOCAL_TIME using information in the column definitions CALDB file (new columns added); the conversion of the HK LOCAL_TIME also lowers the maximum allowed value of the event LOCAL_TIME (the height of the sawtooth function is smaller), so the event LOCAL_TIME values also need to be modified by taking the modulus with the new maximum value; these changes have been tested with both HXI and SGD test data from funcd

 Revision 1.4  2015/08/05 21:01:09  mwitthoe
 ahtime tool: add support for general, instrument GTI

 Revision 1.3  2015/07/28 16:43:38  mwitthoe
 ahtime tool: perform tool clean-up; see issues 532, 533, and 534

 Revision 1.2  2014/07/08 21:23:23  mwitthoe
 ahtime tool: make changes described in issue 400; primary changes include allowing science data in HK files (instead of assuming event data in a single-extension FITS file) and adding support for SXS_GTI files which have two TIMEs to assign

 Revision 1.1  2013/11/20 23:12:04  mwitthoe
 ahtime tool: replace code reading the TIM file with new functions in the ahtime library; switch the STATUS column in the TIM file from I-type to 10X-type; move variable declarations to top of scope; rename the timecoldef library to ahtimelib; add tests for ahtimelib



 BELOW IS THE REVISION LOG FOR timecoldef.cxx BEFORE RENAMING TO ahtimelib.cxx

 Revision 1.8  2013/10/07 15:43:32  mwitthoe
 ahtime tool: switch to use new ahfits connect functions (issue 270)

 Revision 1.7  2013/04/04 21:35:41  mwitthoe
 ahtime tool: linked with local version timecoldef CALDB library; updated ahfits connects to use read/write flags

 Revision 1.6  2013/03/29 19:55:55  mwitthoe
 ahcaldb/timecoldef: remove reliance on ahmission; update contents to match latest timing TRF

 Revision 1.5  2013/01/17 22:04:36  mwitthoe
 changed column definitions CALDB file to match latest version in timing TRF

 Revision 1.4  2012/12/14 22:15:14  mwitthoe
 changes to column definitions FITS file: re-add LTIME2EVT and LTIME2HK and fill values to PSP_ID for SXS (empty otherwise); split SGD-Shield into SGD-Shield1 and SGD-Shield2 where the only difference between the two will be the value of HKEXTNAME (values TBD)

 Revision 1.3  2012/12/10 17:50:46  mwitthoe
 bring column definitions CALDB file (for time assignment) into agreement with the latest version of the TRF (Dec 6); a couple column names were changed and the LTIME2EVT/HK columns were eliminated

 Revision 1.2  2012/11/28 20:23:36  mwitthoe
 update timecoldef CALDB library to use new version of ahmission where HK files are indentified in a new way; added a new function get_hk() to retrieve HK column definitions since DET_HK no longer exists; changed test code accordingly

 Revision 1.1  2012/11/15 02:24:23  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.17  2012/11/13 18:23:25  mwitthoe
 ahtime library: use new open/create/clone functions from ahfits; change time delay CALDB file and implementation to match latest Oct 31 TRF; change column definitions CALDB file and implementation to match Oct 31 TRF; read and use SXS lookup tables in time assignment; use new instrument names for HXI1/2 and SGD1/2

 Revision 1.16  2012/11/08 03:03:52  mwitthoe
 change instrument delay CALDB file to match timing TRF (Oct 31); implemented instrument delays in time assignment library; empty HKEXTNAME value for SXI row in the column definitions FITS file

 Revision 1.15  2012/11/07 04:32:44  mwitthoe
 edit column definitions CALDB file to match new timing TRF

 Revision 1.14  2012/11/01 20:03:02  mwitthoe
 remove clobber arguments from ahfits create/clone calls from ahtime library and unit tests

 Revision 1.13  2012/10/12 22:52:38  mwitthoe
 align ahtime library with latest version of ahfits

 Revision 1.12  2012/10/11 18:05:16  mwitthoe
 convert ahtime library over to new ahfits

 Revision 1.11  2012/09/26 20:32:42  mwitthoe
 check if CALDB data is loaded before trying to read FITS file

 Revision 1.10  2012/09/26 20:12:21  mwitthoe
 apply new CALDB library standards to ahdelay, ahleapsec, and ahcolumndef in the ahtime library

 Revision 1.9  2012/09/26 00:37:21  mwitthoe
 renamed TI to L32TI in the ahtime library to reflect change in SCT 021

 Revision 1.8  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

 Revision 1.7  2012/09/05 19:35:14  mwitthoe
 fix make_columndef.cxx and general clean up of ahdelay and ahcolumndef in the ahtime library

 Revision 1.6  2012/09/05 18:26:09  mwitthoe
 switch column definition library to new CALDB library format; switch over to using ahmission enumerations in ahdelay

 Revision 1.5  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.4  2012/08/15 16:11:53  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.3  2012/08/15 15:24:13  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.2  2012/07/13 18:32:08  mwitthoe
 clean up ahtime code


*/
