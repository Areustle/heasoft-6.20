/// \file camstoffset.h
/// \brief functions to act on the CALDB file containing CAMS offset parameters
/// \author Mike Witthoeft
/// \date $Date: 2015/10/02 20:07:57 $

/// \addtogroup mod_ahmission
/// \section ahmission_camstoffset CAMS Offset Times - camstoffset
///
/// Simple table containing time adjustments to apply to CAMS data based on 
/// the TIME_CODE.  Also contains a header keyword with the CAMS period.
///

#ifndef AHMISSION_CAMSTOFFSET_H
#define AHMISSION_CAMSTOFFSET_H

#include "ahgen/ahversion.h"
AHVERSION(AHMISSION_CAMSTOFFSET,"$Id: camstoffset.h,v 1.6 2015/10/02 20:07:57 mwitthoe Exp $")

#include <string>
#include <vector>
#include <map>

/// \ingroup mod_ahmission
namespace ahmission {

/// \brief access CALDB file with CAMS time offsets
namespace camstoffset {

/** \addtogroup mod_ahmission
 *  @{
 */

/// \brief define type holding CAMS offset information
typedef struct {
  double m_cams_period;      ///< length of CAMS period in seconds
  int m_cams_freq;           ///< number of measurements per period
  std::map<int,double> m_cams_offsets;   ///< offset times versus time code
} DataType;

/// \brief read CAMS offset file and load data into global data map
/// \param[in] filename name of CAMS offset file
/// \param[in] dat structure containing CAMS time offset data
void load(const std::string & filename, DataType & dat);

/// \brief clear data
/// \param[in] dat structure containing CAMS time offset data
void clear(DataType & dat);

/// \brief return the telemetry period for CAMS
/// \param[in] dat structure containing CAMS time offset data
/// \return period in seconds
double get_period(const DataType & dat);

/// \brief return the number of measurements per period
/// \param[in] dat structure containing CAMS time offset data
/// \return number of measurements per period
double get_frequency(const DataType & dat);

/// \brief return the CAMS offset given the TIME_CODE
/// \param[in] dat structure containing CAMS time offset data
/// \param[in] timecode input time code
/// \return offset in seconds
double get_offset(DataType & dat, int timecode);

/// \brief return number of offsets
/// \param[in] dat structure containing CAMS time offset data
int num_offset(const DataType & dat);

/// \brief write a template for the CAMS offset FITS file
/// \param[in] tplfile name of output FITS template
/// \param[in] period offset period appearing the the header
/// \param[in] freq number of measurements per period
/// \internal
/// \note this routine may be removed in the future once the official
///  FITS file is defined
void write_cams_template(const std::string & tplfile, double period,
                         int freq);

/// \brief create CAMS offset FITS file from template
/// \param[in] filename name of output FITS file
/// \param[in] tplfile name of template file to use
/// \internal
/// \note this routine may be removed in the future once the official
///  FITS file is defined
void create_cams_fits(const std::string & filename,
                      const std::string & tplfile);

/// \brief write list of CAMS offset
/// \param[in] filename name of FITS file to modify
/// \param[in] offsets list of offsets in seconds
/// \param[in] tcs list of time codes
/// \internal
/// \note this routine may be removed in the future once the official
///  FITS file is defined
void write_cams_data(const std::string & filename, 
                     const std::vector<double> offsets,
                     const std::vector<int> tcs);


/** @} */

}  // namespace camstoffset

}  // namespace ahmission

#endif /* AHMISSION_CAMSTOFFSET_H */

/* Revision Log
 $Log: camstoffset.h,v $
 Revision 1.6  2015/10/02 20:07:57  mwitthoe
 ahmission library: remove redundant CALDB resolve functions; see issue 535

 Revision 1.5  2013/04/11 18:47:06  mwitthoe
 update former ahcaldb libraries now residing in ahmission: camstoffset, delay, and leapsec; changed namespace, add read/write flags to ahfits connections, removed static instance of DataType holding CALDB data in memory

 Revision 1.4  2013/01/24 18:54:08  mwitthoe
 update Doxygen for ahcaldb library

 Revision 1.3  2012/11/26 21:23:19  mwitthoe
 add brief descriptions to namespaces in mission

 Revision 1.2  2012/11/16 18:52:26  mwitthoe
 add TFREQ keyword to CALDB file with CAMS offsets which gives the number of measurements per CAMS period

 Revision 1.1  2012/11/15 02:24:22  mwitthoe
 add ahcaldb folder under mission; moved from ahtime: leapsec, delay, timecoldef, camstoffset, freqtemp (these names have changed)



 ==== Messages below refer to old location: mission/lib/ahtime ====

 Revision 1.2  2012/11/05 21:02:25  mwitthoe
 modify CAMS FITS file to match definition in SCT 021 (Sep 7, 2012)

 Revision 1.1  2012/10/18 17:35:40  mwitthoe
 ahtime library: created single header file, ahtime.h, to include entire ahtime library (old ahtime -> ahtime_base); separated CAMS data from instrument delay CALDB file into new file: associated libraries created


*/
