/// \file hxisgdphalib.cxx
/// \brief CALDB files with active channels and with PHA spline coefficients
///    for HXI and SGD
/// \author Robert S. Hill
/// \date $Date: 2015/08/14 20:08:10 $

#define AHLABEL hxisgdpha_hxisgdphalib
#define AHCVSID "$Id: hxisgdphalib.cxx,v 1.13 2015/08/14 20:08:10 rshill Exp $"

#include "hxisgdphalib.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahmission/ahmission.h"

#include <iostream>
#include <fstream>
#include <string.h>
#include <sstream>
#include <cmath>
#include <limits>


namespace pha {

// ---------------------------------------------------------------------------

/// \callgraph

void loadGainTable(const std::string & filename, const std::string & instrume,
                   const std::string & detnam, 
                   ahmission::spline::SplineSetMap & gain_table) {
  ahmission::spline::load(filename, "GAIN", "READOUT_ID_RMAP", instrume, detnam, gain_table);
  AH_INFO(ahlog::HIGH) << "Loadded GAIN for INSTRUME=" << instrume << " DETNAM=" << detnam << std::endl;
}

// ---------------------------------------------------------------------------
                   
/// \callgraph

void loadAltGainTable(const std::string & filename, const std::string & instrume,
                   const std::string & detnam, 
                   ahmission::spline::SplineSetMap & gain_table) {
  ahmission::spline::load(filename, "ALTGAIN", "READOUT_ID_RMAP", instrume, detnam, gain_table);
  AH_INFO(ahlog::HIGH) << "Loadded ALTGAIN for INSTRUME=" << instrume << " DETNAM=" << detnam << std::endl;
}
  
}  // namespace pha

/* Revision Log
 
 $Log: hxisgdphalib.cxx,v $
 Revision 1.13  2015/08/14 20:08:10  rshill
 Additional log statements.

 Revision 1.12  2015/03/18 19:56:04  asargent
 Changed DETNAME to DETNAM

 Revision 1.11  2015/03/16 17:57:42  mwitthoe
 hxisgdpha: move active channel structure and load function into the hxisgdevtid library (these are now needed by sgdevtid)

 Revision 1.10  2015/03/03 20:29:21  rshill
 Strange SGD readouts; switch to mission spline file routines.

 Revision 1.9  2015/01/22 00:43:22  rshill
 Fixed bug in detecting missing readout_id_rmap;
 added a more informative AH_THROW_RUNTIME message for this condition.

 Revision 1.8  2014/12/24 16:03:19  rshill
 Moved DATAMODE underscore split to initialize.

 Revision 1.7  2014/12/18 15:50:39  rshill
 DATAMODE followin underscore extracted.

 Revision 1.6  2014/07/22 17:59:06  mwitthoe
 hxisgdpha: add checks for INSTRUME/DETNAME to CALDB load functions; add datamode parameter to parameter file; remove compiler warnings

 Revision 1.5  2014/06/05 22:51:24  rshill
 Response to TRF review.  CALDB formats updated,
 documentation improved.

 Revision 1.4  2014/05/01 19:17:56  rshill
 Partially fixed up while editing the TRF.  Does not build.

 Revision 1.3  2014/03/26 18:27:48  mwitthoe
 hxisgdpha: in hxisgdphalib, change connect() to connectScalar or connectVariableLengthArray() (ahfits)

 Revision 1.2  2014/01/17 04:21:14  rshill
 Revised to current TRF; dealt with code review comments.

 Revision 1.1  2014/01/03 22:10:38  rshill
 Consolidaion of pha and activechan CALDB access into one source code file.


 Log messages inherited from defunct activechan.cxx:

 Revision 1.8  2013/09/25 18:32:50  rshill
 Added call to ahfits::firstHDU to move to BINTABLE.

 Revision 1.7  2013/06/18 00:36:34  rshill
 Additional debug statements

 Revision 1.6  2013/04/10 21:33:13  rshill
 Got rid of hxisgdpha:: namespace.

 Revision 1.5  2013/04/04 22:58:48  rshill
 Move CALDB code out of old ahcaldb area. Got rid of static global
 data structures. Streamlined data structures containing CALDB data.
 Revamped CALDB format per discussions with Japan.

 Revision 1.4  2013/04/02 17:49:10  rshill
 Simplify static data structure.

 Revision 1.2  2013/02/21 19:20:40  rshill
 Corrected typos.

 Revision 1.1  2013/02/19 23:20:44  rshill
 Initial checkin, active/dead/noisy channel map for HXI/SGD.

 Log messages inherited from defunct pha.cxx:

 Revision 1.13  2013/12/02 22:48:36  asargent
 Changed ahlog::debug() to ahlog::get_debug() due to migration of ahlog

 Revision 1.12  2013/09/25 18:32:50  rshill
 Added call to ahfits::firstHDU to move to BINTABLE.

 Revision 1.11  2013/06/18 00:36:34  rshill
 Additional debug statements

 Revision 1.10  2013/06/14 18:27:16  rshill
 Simplified and commented the algorithms; standardized statement order;
 made the internal table structure more robust.

 Revision 1.9  2013/04/10 21:33:13  rshill
 Got rid of hxisgdpha:: namespace.

 Revision 1.8  2013/04/04 22:58:48  rshill
 Move CALDB code out of old ahcaldb area. Got rid of static global
 data structures. Streamlined data structures containing CALDB data.
 Revamped CALDB format per discussions with Japan.

 Revision 1.7  2013/04/02 17:52:34  rshill
 Simplify static data structure.  Return two sets of coefficients
 at nodal points.

 Revision 1.5  2013/01/19 01:22:37  rshill
 Needed pass by ref for output args.

 Revision 1.4  2013/01/19 00:08:06  rshill
 Return coefficients rather than EPI value.

 Revision 1.3  2013/01/12 00:19:42  rshill
 Added checks for invalid stl::map keys.

 Revision 1.2  2013/01/11 00:15:24  rshill
 Added code to really use a proposed PHA CALDB file format.

 Revision 1.1  2012/11/30 21:03:37  rshill
 Placeholder for interface to pulse-height cal file for HXI and SGD.

*/
