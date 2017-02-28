/// \file badpix.cxx
/// \brief functions to act on the CALDB bad pixel file for HXI & SGD
/// \author Mike Witthoeft
/// \date $Date: 2015/07/15 19:21:38 $

/// \addtogroup mod_hxisgdevtid
/// \section hxisgdevtid_badpix Bad pixel CALDB file for HXI/SGD - badpix
///
/// Description...
///

#ifndef HXISGDEVTID_BADPIX_H
#define HXISGDEVTID_BADPIX_H

#include "ahgen/ahversion.h"
AHVERSION(HXISGDEVTID_BADPIX,"$Id: badpix.h,v 1.12 2015/07/15 19:21:38 klrutkow Exp $")

#include <sstream>
#include <string>
#include <map>

/// \ingroup mod_hxisgdevtid

namespace hxisgdevtid {

namespace badpix {

/** \addtogroup mod_hxisgdevtid
 *  @{
 */

/// \brief define legal values for active channel column
enum ActiveChanVal {
  e_BAD_PIXEL = 0,
  e_GOOD_PIXEL = 1,
  e_STRANGE_PIXEL = 2
};

/// \brief data type to relate READOUT_ID_RMAP with EPI_THRE (threshold)
typedef std::map<int, double> ThresholdTable;

/// \brief declare map to hold one retrived flag set for the run.
typedef std::map<int, ActiveChanVal> ActiveChanTable;

/// \brief number of unique READOUT_ID_RMAPs for HXI
const unsigned int NUM_READOUT_HXI=1280;

/// \brief number of unique READOUT_ID_RMAPs for SGD
const unsigned int NUM_READOUT_SGD=13312;



/// \brief read event thresholds for READOUT_ID_RMAPs immediately following
///  given TIME
/// \param[in] filename name of remapping file
/// \param[in] instrume require given filename to have INSTRUME keyword set to
///  this value
/// \param[in] detnam require given filename to have DETNAM keyword set to
///  this value (this will select the proper HDU for the SGD CALDB file)
/// \param[in] datamode post-underscore portion of DATAMODE keyword from event file
/// \param[out] thresh_table map between READOUT_ID_RMAP and EPI_THRE
/// \param[out] alive_asic value of ALIVE_ASIC column at given time
void loadThreshold(const std::string& filename, const std::string& instrume, 
                   const std::string& detnam, const std::string& datamode, 
                   ThresholdTable & thresh_table, int& alive_asic);

/// \brief read active channel file and load data into global data map
/// \param[in] filename name of active channel file
/// \param[in] instrume value of INSTRUME keyword for desired extension
/// \param[in] detnam value of DETNAM keyword for desired extension
/// \param[in] datamode DATAMODE to be used for event file
/// \param[out] activechan_flags data structure to hold active channel info
void loadActiveChan(const std::string & filename, const std::string & instrume,
                    const std::string & detnam, const std::string & datamode, 
                    ActiveChanTable & activechan_flags);

/** @} */

}  // namespace badpix

}  // namespace hxisgdevtid

#endif /* HXISGDEVTID_BADPIX_H */

/* Revision Log

 $Log: badpix.h,v $
 Revision 1.12  2015/07/15 19:21:38  klrutkow
 removed local resolve function in favor of ahmission caldb resolve

 Revision 1.11  2015/07/13 00:15:19  klrutkow
 added resolve() function to get CALDB filename

 Revision 1.10  2015/03/18 19:49:26  mwitthoe
 hxisgdevtid library: change DETNAME to DETNAM

 Revision 1.9  2015/03/16 17:54:34  mwitthoe
 hxisgdevtid library: move load function for the active channel (badpix) CALDB file from the hxisgdpha tool since it is now needed by sgdevtid

 Revision 1.8  2014/12/30 16:29:36  mwitthoe
 hxisgdevtid library: require only a portion of the DATAMODE keyword in the load() function for the badpix CALDB file, e.g. NORMAL1 instead of CAMERA_NORMAL1

 Revision 1.7  2014/12/16 15:44:39  mwitthoe
 hxievtidlib: now search badpix CALDB files using DATAMODE instead of TIME; given an event DATAMODE (e.g. CAMERA_NORMAL1 for HXI), search the DATAMODE column for the last part of the event DATAMODE (e.g. NORMAL1); see issues 466/467

 Revision 1.6  2014/12/10 16:01:24  mwitthoe
 hxievtidlib: load() function for the badpix file will now also return the value of the ALIVE_ASIC column in addition to the EPI thresholds; see issues 466 & 467

 Revision 1.5  2014/05/09 19:51:36  mwitthoe
 hxisgdevtidlib: update common CALDB access routines to use new versions of CALDB files

 Revision 1.4  2014/03/12 20:53:59  mwitthoe
 hxisgdevtid library: include extension name in argument list for load() function for the badpix library; this is needed since the SGD badpixel CALDB file will have 3 extensions: CC1, CC2, and CC3; the old version just loaded the 1st extension

 Revision 1.3  2014/01/17 21:05:43  mwitthoe
 hxisgdevtid library: update badpix library to match corrected versions of CALDB files (included at src/test/input/test_caldb_*fits)

 Revision 1.2  2014/01/16 18:15:54  mwitthoe
 hxisgdevtid library: change outer namespace from hxisgd to hxisgdevtid

 Revision 1.1  2014/01/16 18:00:01  mwitthoe
 add hxisgdevtid library with CALDB access libraries used by the hxievtid and sgdevtid tools


*/
