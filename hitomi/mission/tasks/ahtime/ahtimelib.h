/// \file ahtimelib.h
/// \brief functions for ahtime
/// \author Mike Witthoeft
/// \date $Date: 2016/04/07 17:43:27 $

/// \addtogroup tool_ahtime
/// \section tool_ahtime_ahtimelib Supplementary functions for ahtime
///
/// \subsection ahcaldb_timecoldef Time-assignment Column Definitions - timecoldef
///
/// List of columns needed for time-assignment for each instrument/detector.
///

#ifndef TOOL_AHTIME_AHTIMELIB_H
#define TOOL_AHTIME_AHTIMELIB_H

#include "ahgen/ahversion.h"
AHVERSION(TOOL_AHTIME_AHTIMELIB,"$Id: ahtimelib.h,v 1.7 2016/04/07 17:43:27 mwitthoe Exp $")

#include "ahfits/ahfits.h"
#include "ahmission/timfile.h"

#include <string>
#include <map>


// =============================================================================
// General functions
// =============================================================================

/// \brief Largest size of group of Suzaku-mode points (GPS not synchronized)
const int NMAXSUZAKUGRP=1000;


/// \brief Hold data for a single row of the input event/HK file.
///
/// Note: not all of the members of the struct are applicable to each file type.
/// Note: With the exception of RAWY (used by SXI only), the actual column names
///  are recorded in the column definitions CALDB file, thus the variables are
///  named after the columns in that file, not the input event/HK file.
struct EventDat {
  // constructor
  EventDat(): m_tisc(0), m_sendtime(0.), m_ltime1evt(0.), m_ltime1evtnull(0), 
              m_sendtimesp(0.), m_tiscsp(0),
              m_ltime1evt_2(0.), m_ltime1evt_2null(0),
              m_ltime2evt(0), m_time(0.), m_timenull(0), 
              m_timetrig(0.), m_timetrignull(0), m_yyyy(0),
              m_ddd(0), m_hh(0), m_mm(0), m_ss(0), m_us(0), num_proc_status(0) {
              for (int ii=0; ii < 32; ii++) m_proc_status[ii]=0; }

  long long m_tisc;                    ///< TI column to use; always L32TI
  double m_sendtime;                   ///< Sending time column; always S_TIME
//  long long m_ltime1evt;               ///< Local time 1; needed for lookup tables
  double m_ltime1evt;                  ///< Local time 1; needed for lookup tables
  char m_ltime1evtnull;                ///< flag indicating if LTIME1EVT is NULL

  double m_sendtimesp;                 ///< Sending time column for STOP in GTI extensions
  long long m_tiscsp;                  ///< L32TI column for STOP in GTI extensions

  double m_ltime1evt_2;                ///< Some extensions require a 2nd local time
  char m_ltime1evt_2null;              ///< null flag for 2nd local time

//  double m_ltime1evtsxs1;              ///< 1st SXS local time; needs to be double instead of long long
//  char m_ltime1evtsxs1null;            ///< flag indicating if LTIME1EVTSXS1 is NULL
//  double m_ltime1evtsxs2;              ///< 2nd SXS local time
//  char m_ltime1evtsxs2null;            ///< flag indicating if LTIME1EVTSXS2 is NULL

  long long m_ltime2evt;               ///< Local time 2; only used by SXS (PSP_ID)
  double m_time;                       ///< Output time column: TIME
  char m_timenull;                     ///< flag to set TIME column to NULL
  double m_timetrig;                   ///< for SXS_TRIG files; TIMETRIG column
  char m_timetrignull;                 ///< flag to set TIMETRIG column to NULL
  double m_start;                      ///< for SXS_GTILOST files; GTI start time
  char m_startnull;                    ///< flag to set START column to NULL
  double m_stop;                       ///< for SXS_GTILOST files; GTI stop time
  char m_stopnull;                     ///< flag to set STOP column to NULL
  int m_yyyy;                          ///< 4 digit year; HK output only
  char m_yyyynull;                     ///< flag to set YYYY column to NULL
  int m_ddd;                           ///< 3 day (1-365/6); HK output only
  char m_dddnull;                      ///< flag to set DDD column to NULL
  int m_hh;                            ///< 2 digit hour; HK output only
  char m_hhnull;                       ///< flag to set HH column to NULL
  int m_mm;                            ///< 2 digit minute; HK output only
  char m_mmnull;                       ///< flag to set MM column to NULL
  int m_ss;                            ///< 2 digit second; HK output only
  char m_ssnull;                       ///< flag to set SS column to NULL
  int m_us;                            ///< 6 digit microsecond; HK output only
  char m_usnull;                       ///< flag to set US column to NULL
  char m_proc_status[32];              ///< PROC_STATUS column
  ahfits::IndexType num_proc_status;   ///< size of PROC_STATUS column
  int m_rawy;                          ///< RAWY column; SXI only
};

/// \brief container to hold single lookup table (sawtooth)
struct LookupTable {
  LookupTable(): m_size(0), m_xdat(0), m_ydat(0), m_ymax(0.), m_luidx(0) {}

  unsigned long m_size;     ///< size of lookup table
  double* m_xdat;           ///< x-values of lookup table
  double* m_ydat;           ///< y-values of lookup table
  double m_ymax;            ///< maximum y-value of lookup table
  unsigned long m_luidx;    ///< current search position
};

/// \brief container to hold row counters
struct Counters {
  Counters(): num_rows(0), num_extrap(0), num_badprocstatus(0), num_timenull(0) { }

  long long num_rows;                  ///< number of rows processed
  long long num_extrap;                ///< number of extrapolated TIMEs
  long long num_badprocstatus;         ///< number of rows with bad PROC_STATUS
  long long num_timenull;              ///< number of rows with TIME assigned to NULL
};

/// \brief container to hold constants needed for SXI time assignment
struct SXIParams {
  SXIParams(): m_timask(0), m_seqfac(0.), m_subcycleshift(0.), m_subcycletime(0.),
               m_adjlasttime(0.), m_nexp(0), m_dely(0) {}

  unsigned long m_timask;    // mask for U32TI
  double m_seqfac;           // convert SEQ_START_TIME to U32TI units
  double m_subcycleshift;    // time from start of the subcycle to TIME reference
  double m_subcycletime;     // total time of a single subcycle
  double m_adjlasttime;      // additional shift if in last subcycle
  int m_nexp;                // number of exposures in cycle
  int m_dely;                // number of Y pixels for each exposure
};


/// \brief convert U32TI to L32TI
/// \param[in] u32ti U32TI value
/// \return L32TI
long long u32tol32(long long u32ti);

/// \brief Allocate lookup table arrays.
/// \param[in,out] table lookup table to allocate
/// \param[in] size size of arrays
/// \param[in] ymax maximum value of sawtooth
void initLookupTable(LookupTable& table, long size, double ymax);

/// \brief De-allocate lookup table arrays.
/// \param[in,out] table lookup table to deallocate
void clearLookupTable(LookupTable& table);

/// \brief Perform TIM look-up to get TIME
/// \param[in] timdat look up tables between L32TI and TIME from TIM file
/// \param[in] stime input S_TIME value
/// \param[in] fineti L32TI value to get TIME for
/// \param[in] l32timax Maximum value of L32TI
/// \param[in] interp interpolation type enumeration
/// \param[in,out] timidx input: starting search position; 
///                       output: final search position
/// \param[out] extrap true if TIM search results in extrapolation
/// \param[out] badtim true if TIM search is adjacent to bad STATUS TIM row
/// \param[out] if not NULL, fill PROC_STATUS with TIM status
/// \return TIME from TIM lookup; ignore if badtim=true
double lookupTIM(ahmission::timfile::TimFileData& timdat, double stime,
                 double fineti, double l32timax, int interp,
                 unsigned long& timidx, bool& extrap, bool& badtim,
                 char* ptr_proc_status);

/// \brief Compute fine-TI for SXI events
/// \param[in]      irow current row number (just used in error messages)
/// \param[in]      optype type of time-assignment to perform
/// \param[in]      sxipars container with SXI parameters needed for time assignment
/// \param[in]      l32ti L32TI column value
/// \param[in,out]  stime S_TIME value is modified by the windowing shift
/// \param[in]      rawy RAWY position of event
/// \param[in]      ltime1evt local time of event
/// \param[in]      ltime1evtnull =1 if local time is NULL
/// \param[in,out]  timenull =1 if output TIME column is NULL
/// \param[out]     fineti output fine-TI
/// \param[out]     count_ext row counters
void calcFineTI_SXI(long long irow, const std::string& optype, const SXIParams& sxipars,
                    long long l32ti, double& stime, int rawy, double ltime1evt,
                    char& ltime1evtnull, char& timenull, double& fineti,
                    Counters& count_ext);

/// \brief Compute fine-TI using a lookup table (SXS/HXI/SGD events)
/// \param[in]      irow current row number (just used in error messages)
/// \param[in]      ltime1evt local time of event
/// \param[in]      ltime1evtnull =1 if local time is NULL
/// \param[in,out]  timenull =1 if output TIME column is NULL
/// \param[in,out]  lutable lookup table (search index is updated)
/// \param[in]      interp interpolation method (enumerated value)
/// \param[in,out]  fineti output fine-TI
/// \param[out]     count_ext row counters
void calcFineTI_Lookup(long long irow, double ltime1evt, char& ltime1evtnull,
                       char& timenull, LookupTable& lutable, int interp,
                       double& fineti, Counters& count_ext);

/// \brief Add delay to START and STOP times
/// \param[in,out] rowdat container with output column values
/// \param[in] delay delay time to add (in seconds)
void addDelayToGTI(EventDat& rowdat, double delay);

/// \brief set PROC_STATUS bits to indicate that times in TIM file are
///  out-of-order.
/// \param[in,out] val_proc_status local variable for PROC_STATUS column
void setProcStatusOutOfOrder(char* val_proc_status);

/// \brief copy first 5 bits (of 10 total) from TIM file statuses, status1 and
///  status2, into the last 10 bits of PROC_STATUS
/// \param[in,out] val_proc_status local variable for PROC_STATUS column
/// \param[in] status1 TIM STATUS value with smaller TIME
/// \param[in] status2 TIM STATUS value with larger TIME
void setProcStatusCopyTimStatus(char* val_proc_status, char* status1,
                                char* status2);

/// \brief set PROC_STATUS bits to indicate a bad operation from GSFC software
/// \param[in,out] val_proc_status local variable for PROC_STATUS column
void setProcStatusGSFCBad(char* val_proc_status);

/// \brief Replace the first # character in str with repl.
/// \param[in,out] str the string to modify
/// \param[in] repl what to replace '#' with
void hashReplace(std::string& str, const std::string repl);


// =============================================================================
// Functions to read column definitions CALDB file
// =============================================================================

/// \brief access column definitions CALDB file
namespace timecoldef {

/** \addtogroup tool_ahtime
 *  @{
 */

/// \brief define struct to hold column names for a single instrument/detector
struct ColumnNames {
  // constructor
  ColumnNames(void): m_ltime1bits(0), m_ltfacthk(0), m_ltfactbits(0),
                     m_ltresevt(0.), m_ltreshk(0.) {}
  ColumnNames(const std::string& tisc, const std::string& sendtime,
              const std::string& ltime1evt, int ltime1bits,
              const std::string& ltime2evt, const std::string& hkextname,
              const std::string& ltime1hk, const std::string& ltime2hk,
              const std::string& ti1hk, int ltfacthk, int ltfactbits,
              double ltresevt, double ltreshk): m_tisc(tisc), 
              m_sendtime(sendtime), m_ltime1evt(ltime1evt),
              m_ltime1bits(ltime1bits), m_ltime2evt(ltime2evt), 
              m_hkextname(hkextname), m_ltime1hk(ltime1hk), 
              m_ltime2hk(ltime2hk), m_ti1hk(ti1hk),
              m_ltfacthk(ltfacthk), m_ltfactbits(ltfactbits),
              m_ltresevt(ltresevt), m_ltreshk(ltreshk) {}

  std::string m_tisc;         ///< TI column for science data
  std::string m_sendtime;     ///< S_TIME column for science data
  std::string m_ltime1evt;    ///< 1st column from event file
  int m_ltime1bits;           ///< number of bits used by LTIME1EVT column
  std::string m_ltime2evt;    ///< 2nd column from event file
  std::string m_hkextname;    ///< name of extension in HK file to use
  std::string m_ltime1hk;     ///< 1st column from HK file
  std::string m_ltime2hk;     ///< 2nd column from HK file
  std::string m_ti1hk;        ///< TI column for HK data
  int m_ltfacthk;             ///< ratio of event to HK local time precisions
  int m_ltfactbits;           ///< log2(m_ltfacthk)
  double m_ltresevt;          ///< precision of event local time in seconds
  double m_ltreshk;           ///< precision of HK local time in seconds
};

/// \brief define struct to hold column names all instruments/detectors;
///  information is stored as a map of the System name (1st column) to
///  a ColumnNames variable.
typedef std::map<std::string, ColumnNames*> ColDefInfo;

/// \brief read column definitions file and load data into global data map
/// \param[in] name of column definition file
/// \param[in] system value of SYSTEM column for desired row
/// \param[out] coldat DataType instance to hold column names
void load(const std::string& filename, ColDefInfo& coldat);

/// \brief Return the column names for a given instrument/detector 
///  via the System name.
/// \param[in] coldat ColDefInfo instance to hold column names
/// \param[in] system value of System column for desired row
/// \param[out] colnames filled ColumnNames variable
void get(ColDefInfo& coldat, const std::string& system, ColumnNames& colnames);


/** @} */

}  // namespace timecoldef


#endif /* TOOL_AHTIME_AHTIMELIB_H */

/* Revision Log
 $Log: ahtimelib.h,v $
 Revision 1.7  2016/04/07 17:43:27  mwitthoe
 ahtime tool: change INFO log messages to DEBUG inside the row loop; add a counter for extrapolated TIMEs

 Revision 1.6  2015/08/24 01:33:49  mwitthoe
 ahtime: 1) do not load column definitions file, delay file, or lookup tables if calctime is false; 2) re-read column names from column definitions file for each extension so that multiple SGD extensions in the HK file (CC1, CC2, CC3) can be processed correctly

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



 BELOW IS THE REVISION LOG FOR timecoldef.h BEFORE RENAMING TO ahtimelib.h

 Revision 1.8  2013/07/25 19:43:28  mwitthoe
 update doxygen tags

 Revision 1.7  2013/03/29 19:55:55  mwitthoe
 ahcaldb/timecoldef: remove reliance on ahmission; update contents to match latest timing TRF

 Revision 1.6  2013/01/24 18:54:09  mwitthoe
 update Doxygen for ahcaldb library

 Revision 1.5  2012/12/14 22:15:13  mwitthoe
 changes to column definitions FITS file: re-add LTIME2EVT and LTIME2HK and fill values to PSP_ID for SXS (empty otherwise); split SGD-Shield into SGD-Shield1 and SGD-Shield2 where the only difference between the two will be the value of HKEXTNAME (values TBD)

 Revision 1.4  2012/12/10 17:50:45  mwitthoe
 bring column definitions CALDB file (for time assignment) into agreement with the latest version of the TRF (Dec 6); a couple column names were changed and the LTIME2EVT/HK columns were eliminated

 Revision 1.3  2012/11/28 20:23:35  mwitthoe
 update timecoldef CALDB library to use new version of ahmission where HK files are indentified in a new way; added a new function get_hk() to retrieve HK column definitions since DET_HK no longer exists; changed test code accordingly

 Revision 1.2  2012/11/26 21:23:19  mwitthoe
 add brief descriptions to namespaces in mission

 Revision 1.1  2012/11/15 02:24:22  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.12  2012/11/13 18:23:25  mwitthoe
 ahtime library: use new open/create/clone functions from ahfits; change time delay CALDB file and implementation to match latest Oct 31 TRF; change column definitions CALDB file and implementation to match Oct 31 TRF; read and use SXS lookup tables in time assignment; use new instrument names for HXI1/2 and SGD1/2

 Revision 1.11  2012/11/07 04:32:44  mwitthoe
 edit column definitions CALDB file to match new timing TRF

 Revision 1.10  2012/09/26 20:12:21  mwitthoe
 apply new CALDB library standards to ahdelay, ahleapsec, and ahcolumndef in the ahtime library

 Revision 1.9  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

 Revision 1.8  2012/09/05 19:35:14  mwitthoe
 fix make_columndef.cxx and general clean up of ahdelay and ahcolumndef in the ahtime library

 Revision 1.7  2012/09/05 18:26:09  mwitthoe
 switch column definition library to new CALDB library format; switch over to using ahmission enumerations in ahdelay

 Revision 1.6  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.5  2012/08/15 16:11:52  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.4  2012/08/15 15:24:12  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.3  2012/07/16 21:02:52  mwitthoe
 update ahtime library/tool header comments

 Revision 1.2  2012/07/13 18:32:07  mwitthoe
 clean up ahtime code

 Revision 1.1  2012/06/19 21:31:33  mwitthoe
 move core/lib/ahtimeconv to core/lib/ahtime

 Revision 1.1  2012/06/07 11:11:39  mwitthoe
 ahtime: add ahcolumndef, ahdelay, and ahtimeassign with test programs and HK FITS file for testing

*/
