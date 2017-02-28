/// \file delay.h
/// \brief functions to act on the CALDB instrument delay file for AstroH.
/// \author Mike Witthoeft
/// \date $Date: 2015/10/02 20:07:57 $

/// \addtogroup mod_ahmission
/// \section ahmission_delay Instrument Delay Times - delay
///
/// Instrument delays for each Astro-H instrument/detector.  The delays can
/// change over time, so there is one extension per instrument with columns
/// for TIME and DELAY.  There are actually two allowed DELAY columns when
/// an instrument has two detectors.
///
/// Note that when retrieving a delay for a given time, the delay time is
/// not interpolated.  The delay occurring immediately before the given time
/// is returned.
///

#ifndef AHMISSION_DELAY_H
#define AHMISSION_DELAY_H

#include "ahgen/ahversion.h"
AHVERSION(AHMISSION_DELAY,"$Id: delay.h,v 1.9 2015/10/02 20:07:57 mwitthoe Exp $")

#include <string>
#include <vector>
#include <map>

/// \ingroup mod_ahmission
namespace ahmission {

/// \brief access instrument delay CALDB file
namespace delay {

/** \addtogroup mod_ahmission
 *  @{
 */

/// \brief define struct to hold time vs delay vectors
///
/// Optimized for dealing with large blocks of pre-sorted times
typedef struct {
  std::vector<double> m_times;     // list of times (sec from epoch)
  std::vector<double> m_delay1s;   // list of delays (microsec)
  std::vector<double> m_delay2s;   // list of delays (microsec)
  int m_icur;                      // index of current search location
  double m_lasttime;               // last time searched (indicate when to reset counter)
} DataType;

/// \brief read instrument delay file and load data into global data map
/// \param[in] filename name of instrument delay file
/// \param[in] extname name of extension to get delays from 
/// \param[out] dat structure to hold data
void load(const std::string & filename, const std::string& extname, DataType & dat);

/// \brief read instrument delay file and return delay times corresponding
///  to given time.  The returned delay values will have the largest time
///  smaller than the given time.
/// \param[in] filename name of instrument delay file
/// \param[in] extname name of extension to get delays from 
/// \param[in] time time to search for
/// \param[out] delay1 value from 1st delay column
/// \param[out] delay2 value from 2nd delay column
void loadAndGet(const std::string & filename, const std::string& extname,
                double time, double& delay1, double& delay2);

/// \brief clear data
void clear(DataType & dat);

/// \brief Return the delay with the largest time smaller than the given time.
/// \param[in] dat structure holding data
/// \param[in] time (in seconds since epoch)
/// \param[in] idx column index (1 or 2)
/// \return instrument delay in microseconds
double get(DataType & dat, double time, int idx);

/// \brief write a template for the instrument delay FITS file
/// \param[in] tplfile name of output FITS template
/// \internal
/// \note this routine may be removed in the future once the official
///  FITS file is defined
void write_delay_template(const std::string & tplfile);

/// \brief create instrument delay FITS file from template
/// \param[in] filename name of output FITS file
/// \param[in] tplfile name of template file to use
/// \internal
/// \note this routine may be removed in the future once the official
///  FITS file is defined
void create_delay_fits(const std::string & filename,
                       const std::string & tplfile);

/// \brief write list of times and delays for a single instrument
/// \param[in] filename name of FITS file to modify
/// \param[in] inst name of instrument
/// \param[in] times vector of times; seconds from TT
/// \param[in] delay1s 1st vector of delays in microseconds
/// \param[in] delay2s 2nd vector of delays in microseconds
/// \internal
/// \note this routine may be removed in the future once the official
///  FITS file is defined
void write_delay_times(const std::string & filename, const std::string & inst,
                       const std::vector<double> times,
                       const std::vector<double> delay1s,
                       const std::vector<double> delay2s);

/** @} */

}  // namespace delay

}  // namespace ahmission

#endif /* AHMISSION_DELAY_H */

/* Revision Log
 $Log: delay.h,v $
 Revision 1.9  2015/10/02 20:07:57  mwitthoe
 ahmission library: remove redundant CALDB resolve functions; see issue 535

 Revision 1.8  2015/07/06 14:11:47  klrutkow
 added resolve function to get CALDB filename

 Revision 1.7  2014/07/08 19:35:57  mwitthoe
 ahmission library: give the instrument argument of procstatus::processRow() a default value of zero; all instrument-specific behavior has been removed from this function; a new function, loadAndGet(), has been added to delay.h/cxx which will read delay times from the given CALDB file corresponding to the given time since there is no need anymore to keep the whole delay table in memory; the old functions are still present, but will be removed once it is sure that they will not be needed (in Build 6)

 Revision 1.6  2013/08/30 20:17:45  mwitthoe
 ahmission:delay - since instrument delays are only needed for one instrument at a time, only load that instrument's delays instead of the entire instrument delay FITS file (see issue 236)

 Revision 1.5  2013/04/11 18:47:06  mwitthoe
 update former ahcaldb libraries now residing in ahmission: camstoffset, delay, and leapsec; changed namespace, add read/write flags to ahfits connections, removed static instance of DataType holding CALDB data in memory

 Revision 1.4  2013/03/29 19:50:16  mwitthoe
 ahcaldb/delay: use strings from the INSTRUME keyword instead of the ahmission enumerations

 Revision 1.3  2013/01/24 18:54:09  mwitthoe
 update Doxygen for ahcaldb library

 Revision 1.2  2012/11/26 21:23:19  mwitthoe
 add brief descriptions to namespaces in mission

 Revision 1.1  2012/11/15 02:24:22  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.17  2012/11/08 03:03:52  mwitthoe
 change instrument delay CALDB file to match timing TRF (Oct 31); implemented instrument delays in time assignment library; empty HKEXTNAME value for SXI row in the column definitions FITS file

 Revision 1.16  2012/10/24 18:02:30  mwitthoe
 add CALDB libary for frequency vs. temperature data (missing algorithm for loading/retrieving data

 Revision 1.15  2012/10/18 17:35:40  mwitthoe
 ahtime library: created single header file, ahtime.h, to include entire ahtime library (old ahtime -> ahtime_base); separated CAMS data from instrument delay CALDB file into new file: associated libraries created

 Revision 1.14  2012/09/26 20:12:21  mwitthoe
 apply new CALDB library standards to ahdelay, ahleapsec, and ahcolumndef in the ahtime library

 Revision 1.13  2012/09/26 19:00:43  mwitthoe
 apply new CALDB library standards to ahdelay; use ahgen::isEqual in unit tests to compare doubles; add CAMS data to instrument delay file and use it in time assignment library instead of hard-coded values

 Revision 1.12  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

 Revision 1.11  2012/09/05 19:35:14  mwitthoe
 fix make_columndef.cxx and general clean up of ahdelay and ahcolumndef in the ahtime library

 Revision 1.10  2012/09/05 18:26:09  mwitthoe
 switch column definition library to new CALDB library format; switch over to using ahmission enumerations in ahdelay

 Revision 1.9  2012/08/29 22:12:24  mwitthoe
 in ahtime library, move unit test functions from src to test

 Revision 1.8  2012/08/27 19:37:51  mwitthoe
 add marker in ahdelay to keep track of current position in time/delay list (increase search efficiency); add unit test for ahdelay

 Revision 1.7  2012/08/24 18:53:21  mwitthoe
 clean up argument lists in ahtime library

 Revision 1.6  2012/08/21 00:18:20  mwitthoe
 tweaks to ahleapsec library and conform ahdelay library to standard for CALDB access libraries

 Revision 1.5  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.4  2012/08/15 16:11:52  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.3  2012/08/15 15:24:12  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.2  2012/07/16 21:02:52  mwitthoe
 update ahtime library/tool header comments

 Revision 1.1  2012/06/19 21:31:34  mwitthoe
 move core/lib/ahtimeconv to core/lib/ahtime

 Revision 1.1  2012/06/07 11:11:39  mwitthoe
 ahtime: add ahcolumndef, ahdelay, and ahtimeassign with test programs and HK FITS file for testing

*/
