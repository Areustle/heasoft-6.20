/// \file leapsec.h
/// \brief Load and store leap second table from CALDB FITS file
/// \author Mike Witthoeft
/// \date $Date: 2015/10/02 20:09:57 $

/// \addtogroup mod_ahtime
/// \section ahtime_leapsec Leap second table - leapsec
///
/// List of leap seconds added since their introduction in 1972.  The date
/// of each addition is given along with the number of leap seconds (typically
/// +1 second).
///

#ifndef AHTIME_LEAPSEC_H
#define AHTIME_LEAPSEC_H

#include "ahgen/ahversion.h"
AHVERSION(AHTIME_LEAPSEC,"$Id: leapsec.h,v 1.2 2015/10/02 20:09:57 mwitthoe Exp $")

#include <string>
#include <map>

/// \ingroup mod_ahtime
namespace ahtime {

namespace leapsec {

/** \addtogroup mod_ahtime
 *  @{
 */

/// \brief Map between a UTC date in the calendar format and the number of leap
///  seconds inserted.  The UTC date is an integer: 10000*YYYY+100*MM+DD.
typedef std::map<int,int> LeapSecCal;

/// \brief Map between an UTC date in the MJD format and the number of leap
///  seconds inserted.
typedef std::map<double,int> LeapSecMJD;

/// \brief Container to hold the leap second tables with both calendar and MJD
///  reference dates.
struct LeapSecTable {
  LeapSecCal cal;
  LeapSecMJD mjd;
};

/// \brief read leap second FITS file and load data in global data map
/// \param[in] leapsecfile name of leap second file
/// \param[in] dat structure containing leap second data
void load(const std::string & leapsecfile, LeapSecTable & dat);

/// \brief return True if leap second data is loaded
/// \param[in] dat structure containing leap second data
/// \return True if leap second data loaded
bool isLoaded(LeapSecTable & dat);

/// \brief clear data
/// \param[in] dat structure containing leap second data
void clear(LeapSecTable & dat);

/** @} */

}  // namespace leapsec

}  // namespace ahtime

#endif /* AHTIME_LEAPSEC_H */

/* Revision Log
 $Log: leapsec.h,v $
 Revision 1.2  2015/10/02 20:09:57  mwitthoe
 ahtime library: remove redundant CALDB resolve functions; see issue 535

 Revision 1.1  2014/09/10 02:43:23  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv



 ==== Messages below refer to old location: mission/lib/ahmission ====

 Revision 1.6  2014/07/28 19:13:49  mwitthoe
 ahmission/leapsec: add support for REFDATA using new function implemented through issue #324; now move to first binary table of leap second file instead of LEAPSEC extension since the leapsec file in REFDATA has no extension name

 Revision 1.5  2013/10/14 16:32:28  mwitthoe
 ahmission::leapsec: change how leap second table is stored; the old way used std::strings which slow down the ahtime library due to a bunch of string comparisons; new way uses an integer (10000*year+100*month+day) to store the leap second date making comparisons much faster; also changed ut_ahmission to access the proc_status.fits test file as READONLY instead of READWRITE since the latter causes buffering information to be written to the file

 Revision 1.4  2013/04/11 18:47:06  mwitthoe
 update former ahcaldb libraries now residing in ahmission: camstoffset, delay, and leapsec; changed namespace, add read/write flags to ahfits connections, removed static instance of DataType holding CALDB data in memory

 Revision 1.3  2013/01/24 18:54:09  mwitthoe
 update Doxygen for ahcaldb library

 Revision 1.2  2012/11/26 21:23:19  mwitthoe
 add brief descriptions to namespaces in mission

 Revision 1.1  2012/11/15 02:24:22  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.13  2012/09/26 20:12:21  mwitthoe
 apply new CALDB library standards to ahdelay, ahleapsec, and ahcolumndef in the ahtime library

 Revision 1.12  2012/09/13 17:58:59  mwitthoe
 implement new version method in the ahtime library

 Revision 1.11  2012/08/28 21:34:44  mwitthoe
 changes to ahtime library: clean up; change leap second value from double to int; fix memory leak

 Revision 1.10  2012/08/21 00:18:20  mwitthoe
 tweaks to ahleapsec library and conform ahdelay library to standard for CALDB access libraries

 Revision 1.9  2012/08/18 02:38:26  mwitthoe
 apply CALDB standards to ahleapsec library

 Revision 1.8  2012/08/17 20:43:18  mwitthoe
 apply standards to ahtime library

 Revision 1.7  2012/08/15 16:11:52  mwitthoe
 fix to ahfits, ahlog, ahtime versioning

 Revision 1.6  2012/08/15 15:24:12  mwitthoe
 add versioning macros to ahtime header and source files

 Revision 1.5  2012/07/16 21:02:52  mwitthoe
 update ahtime library/tool header comments

 Revision 1.4  2012/07/13 18:32:07  mwitthoe
 clean up ahtime code

 Revision 1.3  2012/07/11 16:26:19  mwitthoe
 new ahtime libraries and test codes: ahleapsec - new library to read leap second data from CALDB; AhTimeTAI - library to store number of seconds relative to an epoch; AhTimeTT - library to store modified Julian date (MJD); AhTimeUTC - library to store UTC times; ahtimeconv - conversion functions between 3 AhTime* classes


*/
