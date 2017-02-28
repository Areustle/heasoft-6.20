/// \file ahmktimlib.h
/// \brief Functions for ahmktim
/// \author Mike Witthoeft
/// \date $Date: 2016/07/11 19:48:39 $

/// \addtogroup tool_ahmktim
/// \section tool_ahmktim_ahmktimlib Supplementary functions for ahmktim
///
/// \subsection tool_ahmktim_freqtemp Frequency vs. Temperature Trend - freqtemp
///
/// This CALDB file contains frequency vs temperature trend of the Astro-H
/// SMU which is needed for time assignment when the satellite is not 
/// synchronized with GPS.  The file is produced by the tool: ahtrendtemp.
///

#ifndef AHTOOL_AHMKTIM_AHMKTIMLIB_H
#define AHTOOL_AHMKTIM_AHMKTIMLIB_H

#include "ahgen/ahversion.h"
AHVERSION(AHTOOL_AHMKTIM_AHMKTIMLIB,"$Id: ahmktimlib.h,v 1.9 2016/07/11 19:48:39 mwitthoe Exp $")

#include "ahmath/ahmath.h"
#include "ahfits/ahfits.h"

#include <string>

// =============================================================================

// Forward declarations; struct is defined below 
namespace hce { struct HCE; }
namespace freqtemp { struct DataType; }

/// \brief enumeration for GPS modes
enum { 
  GPS_UNDEF=0,      ///< initialization of gpsmode variables
  GPS_SYNCH=1,      ///< GPS is synchronized and used for TIME assignment
  GPS_SYNCHTRAN=2,  ///< end of transition mode, GPS synchronized
  GPS_SUZAKU=3,     ///< GPS is unsynchronized; use Suzaku method for TIME
  GPS_TRAN=4,       ///< transition mode: time is smoothly adjusted to match GPS
  GPS_ILLEGAL=5     ///< Invalid combination of GPS flags
};

// number of bits in STATUS column (TIM file)
const int LENSTATUS=10;

typedef std::vector<double> DblVector;
typedef std::vector<int> IntVector;
typedef std::vector<char> CharVector;
typedef std::vector<bool> BoolVector;
typedef std::vector<CharVector> StatusVector;

struct Par {
  Par(): m_gaptime(0.), m_suzdrifttime(0.) {};   // initialize non-string values

  std::string m_infile;             ///< input generic HK file
  std::string m_frqtemfile;         ///< existing freq vs. temp CALDB file
  std::string m_timfile;            ///< input TIM file (with one extension)
  std::string m_outtimfile;         ///< output TIM file
  std::string m_outgtifile;         ///< output GPS GTI file
  std::string m_leapsecfile;        ///< Input leap second file (or CALDB/REFDATA)
  std::string m_hkgpsext;           ///< extension of HK with GPS data
  std::string m_hktempext;          ///< extension of HK with temperature data
  std::string m_timext;             ///< name of output extension in TIM file
  double m_gaptime;                 ///< minimum value in sec to define a gap
  std::string m_stimecol;           ///< column containing S_TIME
  std::string m_l32ticol;           ///< column containing L32TI
  std::string m_tempcol;            ///< column containing quartz temperature
  std::string m_packheadcol;        ///< column containing packet header
  std::string m_gpsacol;            ///< column containing the GPS_A on/off flag
  std::string m_gpsbcol;            ///< column containing the GPS_B on/off flag
  std::string m_gpsccol;            ///< column containing the GPS_C on/off flag
  std::string m_gpsdcol;            ///< column containing the GPS_D on/off flag
  double m_suzdrifttime;            ///< minimum time in seconds allowed for drift integral before calibrating with TIME_PACKETS HDU
};



/// \brief This structure will store the S_TIME vs L32TI table in the
///  TIME_PACKETS extension of the input TIM file.  These data are used
///  to anchor Suzaku-mode points when GPS is unsynchronized for long
///  periods of time.
struct AnchorData {
  AnchorData(): m_nrows(0), m_tstart(0.), m_l32tistart(0.), m_tstop(0.), 
                m_l32tistop(0.) {}

  DblVector m_stime;
  DblVector m_l32ti;
  int m_nrows;             // number of rows in HDU (size of m_stime/m_l32ti)
  double m_tstart;         // TSTART keyword value
  double m_l32tistart;     // LTISTART keyword value
  double m_tstop;          // TSTOP keyword value
  double m_l32tistop;      // LTISTOP keyword value
};

// =============================================================================

/// \brief Return GPS mode given the four GPS flags; reset GPS status flags
///   according to GPS flags
/// \param[in] gpsacol GPS A flag
/// \param[in] gpsbcol GPS B flag
/// \param[in] gpsccol GPS C flag
/// \param[in] gpsdcol GPS D flag
/// \param[out] status GPS status flags
/// \return return GPS state enumeration
int getGPSMode(char gpsacol, char gpsbcol, char gpsccol, char gpsdcol, CharVector& status);

/// \brief Flag GPS_STATUS as illegal point
/// \param[out] status GPS status variable
void set_status_illegal(CharVector& status);

/// \brief Return true if illegal flag is set
/// \param[in] status GPS status variable
/// \return true if illegal flag is set
bool has_status_illegal(CharVector& status);

/// \brief Flag GPS_STATUS as skip region
/// \param[out] status GPS status variable
void set_status_skip(CharVector& status);

/// \brief Flag GPS_STATUS as duplicate region
/// \param[out] status GPS status variable
void set_status_duplicate(CharVector& status);

/// \brief Flag GPS_STATUS as bad start/end of Suzaku-mode region; first/last point
///  should have GPS_SYNCH
/// \param[out] status GPS status variable
void set_status_badend(CharVector& status);

/// \brief calculate TIME from TI value using formula from SCT 021
/// \param[in] l32ti input L32TI value to convert
/// \param[in] stime S_TIME value to find number of L32TI cycles
/// \param[in] offset seconds between GPS and Astro-H epochs
/// \param[in] cyclepow gives maximum L32TI value as 2^cyclepow
/// \param[in] tifactor conversion factor from L32TI to seconds
///
/// If the calculated time is further than half the cycle time 
/// (0.5*2**cyclepow), then this function returns -1.
double calc_time(long long l32ti, double stime, long long offset,
                 int cyclepow, double tifactor);

/// \brief Perform drift intergral for Suzaku-mode points; L32TI values are
///  updated.
/// \param[in,out] dat_l32ti array with L32TI values for Suzaku-mode points
/// \param[in] dat_stime array with S_TIME values for Suzaku-mode points
/// \param[in] dat_status array with STATUS values for Suzaku-mode points
/// \param[in] ifirst first index in dat_* arrays to use
/// \param[in] ilast last index in dat_* arrays to use
/// \param[in] first_l32ti L32TI of leading anchor point
/// \param[in] first_time TIME of leading anchor point
/// \param[in] last_l32ti L32TI of trailing anchor point
/// \param[in] last_time TIME of trailing anchor point
/// \param[in] skiplastadj do not adjust slope to last_l32ti/last_time; instead
///            use last drift-corrected point
/// \param[in] hcedat structure used to search HK_HCE extension
/// \param[in] freqtempdata data from freq v. temp CALDB file
/// \param[in] ftidx current search index in freq v. temp table
void drift_integral(DblVector& dat_l32ti, DblVector& dat_stime,
                    StatusVector& dat_status, int ifirst, int ilast,
                    double first_l32ti, double first_time, 
                    double last_l32ti, double last_time, bool skiplastadj,
                    hce::HCE& hcedat, freqtemp::DataType& freqtempdata, 
                    unsigned long ftidx);


/// \brief create new GTI FITS file
/// \param[in] filename name of new file
/// \param[out] gp FilePtr of new file, opened to GTI extension
void create_gti_file(const std::string& filename, ahfits::FilePtr& gp);

/// \brief create new extension in output TIM file
/// \param[in] fp ahfits file pointer to add extension
/// \param[in] extname name of new extension
/// \param[in] smuunit value of SMUUNIT keyword
void add_tim_extension(ahfits::FilePtr fp, const std::string& extname,
                       const std::string& smuunit);

// =============================================================================

/// \brief access TEMPERATURE vs S_TIME data from HK_HCE extension of HK file
namespace hce {

/** \addtogroup tool_ahmktim
 *  @{
 */

/// \brief This structure is used to store all data needed to find the
///  the temperature in the HK_HCE extension of the input file which
///  has an S_TIME nearest to the S_TIME of a row in the HK_GPS extension.
struct HCE {
  // constructor
  HCE(): m_ahffp(0), m_routptr(0), m_stime(0.), m_temp(0.), m_prev_stime(0.),
         m_prev_temp(0.) {}

  // destructor needs to free memory for router; also close file
  ~HCE() {
    if (m_routptr != 0) delete m_routptr, m_routptr=0;
    ahfits::close(m_ahffp);
  }

  ahfits::FilePtr m_ahffp;     ///< ahfits FITS pointer to HK_HCE extension
  ahfits::Router* m_routptr;   ///< ahfits router for reading rows
  double m_stime;              ///< S_TIME for current row
  double m_temp;               ///< TEMPERATURE for current row
  double m_prev_stime;         ///< S_TIME for previous row
  double m_prev_temp;          ///< TEMPERATURE for previous row
};

/// \brief prepare HCE structure for reading temperatures from HK_HCE extension.
/// \param[in] filename name of file with HK_HCE extension
/// \param[in] extname name of HK_HCE extension
/// \param[in] stimecolumn name of S_TIME column
/// \param[in] tempcolumn name of TEMPERATURE column
/// \param[out] hcedat HCE structure to fill
///
/// This function will open the FITS file to the HK_HCE extension and
/// read the first two rows for S_TIME and TEMPERATURE.  The structure
/// can then be passed to another routine to search for the TEMPERATURE
/// having an S_TIME closest to the given S_TIME.
void setupHCE(const std::string& filename, const std::string& extname, 
              const std::string& stimecolumn, const std::string& tempcolumn,
              HCE& hcedat);

/// \brief return the temperature whose S_TIME is closest to the given S_TIME.
/// \param[in] hcedat structure containing FITS file information
/// \param[in] stime S_TIME to search for
/// \return value from TEMPERATURE column
double searchHCE(HCE& hcedat, double stime);

/// \brief free memory in the HCE structure and close the input file
/// \param[in] hcedat structure containing FITS file information
void closeHCE(HCE& hcedat);


/** @} */

}  // namespace hce

// =============================================================================

/// \brief access on-board quartz clock frequency vs. temperature CALDB file
namespace freqtemp {

/** \addtogroup tool_ahmktim
 *  @{
 */

/// \brief define struct to hold data
struct DataType {

  DataType(): m_temp(0), m_freq(0), m_size(0) {}

  ~DataType() {
    if (m_temp != 0) delete [] m_temp, m_temp=0;
    if (m_freq != 0) delete [] m_freq, m_freq=0;
    m_size=0;
  }

  double* m_temp;            // array of temperatures (units?)
  double* m_freq;            // array of frequencies (units?)
  ahmath::IndexType m_size;
};

/// \brief clear data
/// \param[in] dat freq vs. temperature dataset from CALDB file
void clean(DataType & dat);

/// \brief read data into global instance of struct
/// \param[in] filename name of datafile
/// \param[in] dat freq vs. temperature dataset from CALDB file
void load(const std::string & filename, DataType & dat);

/// \brief return frequency by interpolating dataset on given temperature
/// \param[in] temp temperature where to perform interpolation
/// \param[in,out] idx on input: index where to start search; on output:
///  index corresponding to search result
/// \param[in] dat freq vs. temperature dataset from CALDB file
/// \return interpolated frequency
double getFrequency(double temp, unsigned long idx, DataType & dat);

/** @} */

}  // namespace freqtemp

// =============================================================================

#endif /* AHTOOL_AHMKTIM_AHMKTIMLIB_H */

/* Revision Log
 $Log: ahmktimlib.h,v $
 Revision 1.9  2016/07/11 19:48:39  mwitthoe
 ahmktim: add correction for case where the S_TIME and L32TI values in the HK GPS extension are in different L32TI cycles

 Revision 1.8  2015/07/08 16:03:20  asargent
 Clean up of code, new log statements, new counters

 Revision 1.7  2015/07/01 15:19:47  mdutka
 using extended syntax now

 Revision 1.6  2015/06/30 20:38:23  mdutka
 Implementing CALDB query

 Revision 1.5  2014/12/22 16:11:54  mwitthoe
 ahmktim: update tool to include anchor points from the TIME_PACKETS extension of the input TIM file when computing TIME in Suzaku-mode; see issue 457

 Revision 1.4  2014/01/21 21:24:07  mwitthoe
 ahmktim: fix doxygen file tag in ahmktimlib.h

 Revision 1.3  2013/11/21 16:59:43  mwitthoe
 ahmktim: add (and call) destructor for freqtemp structure

 Revision 1.2  2013/11/20 23:53:14  mwitthoe
 ahmktim: update ahmktimlib to use new version of ahmath library which uses C arrays instead of std::vectors (see issue 315)

 Revision 1.1  2013/11/14 22:57:43  mwitthoe
 ahmktim: rename freqtemp library to ahmktimlib; move variable declarations to top of scope; change STATUS column from I type to 10X



 BELOW IS THE REVISION LOG FOR freqtemp.h BEFORE RENAMING TO ahmktimlib.h

 Revision 1.6  2013/07/25 19:42:58  mwitthoe
 fix doxygen tags

 Revision 1.5  2013/04/08 21:05:35  mwitthoe
 ahmktim tool: modify freqtemp CALDB library to remove staticly-loaded data, finish load() function, and add getFrequency() function; re-order functions in main tool source to put standard functions at top

 Revision 1.4  2013/04/05 15:40:35  mwitthoe
 ahmktim: incorporate local freqtemp CALDB library; use read/write flags in ahfits connections

 Revision 1.3  2013/01/24 18:54:09  mwitthoe
 update Doxygen for ahcaldb library

 Revision 1.2  2012/11/26 21:23:19  mwitthoe
 add brief descriptions to namespaces in mission

 Revision 1.1  2012/11/15 02:24:22  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.1  2012/10/24 18:02:30  mwitthoe
 add CALDB libary for frequency vs. temperature data (missing algorithm for loading/retrieving data


*/
