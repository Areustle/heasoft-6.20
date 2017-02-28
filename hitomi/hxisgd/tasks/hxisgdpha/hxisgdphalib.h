/// \file hxisgdphalib.h
/// \brief CALDB files with active channels and with PHA spline coefficients
///    for HXI and SGD
/// \author Robert S. Hill
/// \date $Date: 2015/03/18 19:56:04 $
/// \addtogroup tool_hxisgdpha
/// \section hxisgdpha_hxisgdphalib HXI/SGD channel-related CALDB files
///
/// \subsection hxisgdpha_activechan HXI/SGD active/dead/noisy channels - activechan
///
/// Looks up dead/noisy and active flags for HXI/SGD readout channels
/// using READOUT_ID_RMAP as the key.
///

#ifndef HXISGDPHA_HXISGDPHALIB_H
#define HXISGDPHA_HXISGDPHALIB_H

#include "ahgen/ahversion.h"
AHVERSION(HXISGDPHA_HXISGDPHALIB,"$Id: hxisgdphalib.h,v 1.9 2015/03/18 19:56:04 asargent Exp $")

#include "ahmission/spline.h"
#include "ahmission/ahmission.h"

#include <string>
#include <vector>
#include <map>

/// \ingroup tool_hxisgdpha
/// \subsection hxisgdpha_pha HXI/SGD PHA gain calibration - pha
///
/// Looks up spline interpolation parameters for the gain calibration curve
/// using READOUT_ID_RMAP as the key.
///

namespace pha {

/** \addtogroup tool_hxisgdpha
 *  @{
 */

/// \brief read regular gain calibration file and load data into structure
/// \param[in] filename name of gain calibration file
/// \param[in] instrume value of INSTRUME keyword for desired extension
/// \param[in] detnam value of DETNAM keyword for desired extension
/// \param[out] gain_table structure containing gain calibration
void loadGainTable(const std::string & filename, const std::string & instrume,
                   const std::string & detnam, 
                   ahmission::spline::SplineSetMap & gain_table);

/// \brief read alternative gain calibration file and load data into structure
/// \param[in] filename name of gain calibration file
/// \param[in] instrume value of INSTRUME keyword for desired extension
/// \param[in] detnam value of DETNAM keyword for desired extension
/// \param[out] gain_table structure containing gain calibration
void loadAltGainTable(const std::string & filename, const std::string & instrume,
                   const std::string & detnam,
                   ahmission::spline::SplineSetMap & gain_table);

/** @} */

}  // namespace pha

#endif /* HXISGDPHA_HXISGDPHALIB_H */

/* Revision Log

 $Log: hxisgdphalib.h,v $
 Revision 1.9  2015/03/18 19:56:04  asargent
 Changed DETNAME to DETNAM

 Revision 1.8  2015/03/16 17:57:42  mwitthoe
 hxisgdpha: move active channel structure and load function into the hxisgdevtid library (these are now needed by sgdevtid)

 Revision 1.7  2015/03/03 20:29:21  rshill
 Strange SGD readouts; switch to mission spline file routines.

 Revision 1.6  2014/07/22 17:59:06  mwitthoe
 hxisgdpha: add checks for INSTRUME/DETNAME to CALDB load functions; add datamode parameter to parameter file; remove compiler warnings

 Revision 1.5  2014/06/05 22:51:24  rshill
 Response to TRF review.  CALDB formats updated,
 documentation improved.

 Revision 1.4  2014/05/01 19:17:56  rshill
 Partially fixed up while editing the TRF.  Does not build.

 Revision 1.3  2014/01/17 15:46:39  rshill
 Fixed code review comments

 Revision 1.2  2014/01/09 19:58:44  klrutkow
 klrutkow: code review: comments

 Revision 1.1  2014/01/03 22:10:38  rshill
 Consolidaion of pha and activechan CALDB access into one source code file.


 Log messages inherited from defunct activechan.h:

 Revision 1.5  2013/04/10 21:33:13  rshill
 Got rid of hxisgdpha:: namespace.

 Revision 1.4  2013/04/04 22:58:48  rshill
 Move CALDB code out of old ahcaldb area. Got rid of static global
 data structures. Streamlined data structures containing CALDB data.
 Revamped CALDB format per discussions with Japan.

 Revision 1.3  2013/04/02 17:49:10  rshill
 Simplify static data structure.

 Revision 1.2  2013/02/21 19:18:44  rshill
 Corrected typo.

 Revision 1.1  2013/02/19 23:21:04  rshill
 Initial checkin, active/dead/noisy channel map for HXI/SGD.

 Log messages inherited from defunct pha.h:

 Revision 1.10  2013/06/14 18:27:40  rshill
 Made the internal table structure simpler and more robust.

 Revision 1.9  2013/04/10 21:33:13  rshill
 Got rid of hxisgdpha:: namespace.

 Revision 1.8  2013/04/04 22:58:48  rshill
 Move CALDB code out of old ahcaldb area. Got rid of static global
 data structures. Streamlined data structures containing CALDB data.
 Revamped CALDB format per discussions with Japan.

 Revision 1.7  2013/04/02 17:52:34  rshill
 Simplify static data structure.  Return two sets of coefficients
 at nodal points.

 Revision 1.6  2013/01/24 19:19:36  rshill
 Added doxygen prologue.

 Revision 1.5  2013/01/24 18:54:09  mwitthoe
 update Doxygen for ahcaldb library

 Revision 1.4  2013/01/19 01:22:59  rshill
 Needed pass by ref for output args.

 Revision 1.3  2013/01/19 00:10:06  rshill
 Return coefficients rather than EPI value.

 Revision 1.2  2013/01/11 00:15:19  rshill
 Added code to really use a proposed PHA CALDB file format.

 Revision 1.1  2012/11/30 21:05:24  rshill
 Placeholder for interface to pulse-height cal file for HXI and SGD.


*/
