/// \file sxsdt.cxx
/// \brief Read CALDB file containing time intervals needed by SXS tasks,
///  e.g. sxsflagpix and sxssecid
/// \author Mike Witthoeft
/// \date $Date: 2015/12/03 19:33:57 $

#define AHLABEL ahsxs_sxsdt
#define AHCVSID "$Id: sxsdt.cxx,v 1.10 2015/12/03 19:33:57 mwitthoe Exp $"

#include "ahsxs/sxsdt.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"
#include "ahgen/ahgen.h"
#include "hdcal.h"

#include <sstream>
#include <stdlib.h>
#include <string.h>

namespace ahsxs {

namespace sxsdt {

void loadSXSTimeIntervals(const std::string & filename, double tstart,
                       SXSTimeIntervals & dts) {

  // local variables to connect with FITS columns
  double l_time=0.;
  SXSTimeIntervals l_dts;

  // ahfits file accessor for FITS file
  ahfits::FilePtr fptr=0;        

  // open file
  ahfits::open(filename,"",&fptr);

  // if extended syntax not used, move to the DELTIMES HDU
  if (ahfits::isPrimary(fptr)) ahfits::move(fptr,"DELTIMES");

  // check if able to read file
  if (!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open SXS Time Interval CALDB file: "+filename);
  }

  // setup router for reading rows
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  router.connectScalar(ahfits::e_READONLY,"DTPRIMARY",l_dts.m_dtprimary);
  router.connectScalar(ahfits::e_READONLY,"DTLOWMID",l_dts.m_dtlowmid);
  router.connectScalar(ahfits::e_READONLY,"DTMIDHIGH",l_dts.m_dtmidhigh);
  router.connectScalar(ahfits::e_READONLY,"ANTDTPRE",l_dts.m_antdtpre);
  router.connectScalar(ahfits::e_READONLY,"ANTDTFOL",l_dts.m_antdtfol);
  router.connectFixedLengthArray(ahfits::e_READONLY,"ANTSHIFT",l_dts.m_antshift);
  router.connectScalar(ahfits::e_READONLY,"PROXDT",l_dts.m_proxdt);
  router.connectScalar(ahfits::e_READONLY,"CTRECDT",l_dts.m_ctrecdt);
  router.connectScalar(ahfits::e_READONLY,"CTELDT",l_dts.m_cteldt);
  router.connectScalar(ahfits::e_READONLY,"MXSDT",l_dts.m_mxsdt);

  // +++ 2015-12-03 MCW Temporarily checking for existence of the CTEL2DT column
  // +++ 2015-12-03 MCW before connecting since the new CALDB file has not yet
  // +++ 2015-12-03 MCW been created.  When the CALDB file is finished, then
  // +++ 2015-12-03 MCW this connection should always be made.  The sxsflagpix
  // +++ 2015-12-03 MCW tool will throw an error if the returned cteldt2 value
  // +++ 2015-12-03 MCW is zero.
  if (ahfits::haveColumn(fptr,"CTEL2DT")) router.connectScalar(ahfits::e_READONLY,"CTEL2DT",l_dts.m_cteldt2);

  // Store time from previous row
  double p_time=-1.;

  // read table until reach row with TIME > TSTART
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);

    if (l_time > tstart) break;
    p_time=l_time;
    dts=l_dts;
  }

  // close FITS file
  ahfits::close(fptr);

  // If 1st row had a time larger than the given time, then return 
  // values from first row
  if (p_time < 0.) dts=l_dts;

}

// ---------------------------------------------------------------------------

}  // namespace sxsdt

}  // namespace ahsxs

/* Revision Log
 $Log: sxsdt.cxx,v $
 Revision 1.10  2015/12/03 19:33:57  mwitthoe
 ahsxs library: support CTEL2DT column in delta-times CALDB file; assign 2 bits in STATUS for a 2nd electrical cross-talk flagging

 Revision 1.9  2015/10/29 20:05:25  mwitthoe
 ahsxs library: change search of SXSDT CALDB file so that the returned values have the largest TIME smaller than the given TSTART value

 Revision 1.8  2015/10/02 20:17:48  mwitthoe
 ahsxs library: remove redundant CALDB resolve functions; see issue 535

 Revision 1.7  2015/07/13 15:01:18  mdutka
 adding caldb queries for pixmap and dt file

 Revision 1.6  2015/03/23 17:37:37  mwitthoe
 ahsxs library: update format of DT CALDB file; see issues 496/497

 Revision 1.5  2015/03/11 17:08:08  mwitthoe
 ahsxs library: add PROXDT column to sxsdt CALDB file

 Revision 1.4  2014/11/19 19:04:57  mwitthoe
 ahsxs: add library to read CALDB file containing coefficients needed for computing the SampleCnt column for event files (sxssamcnt); see issue 457

 Revision 1.3  2014/10/01 01:26:01  mwitthoe
 ahsxs: column names for time interval CALDB file were changed

 Revision 1.2  2014/09/30 08:28:32  mwitthoe
 ahsxs: change column names secbefore/seclow/secmid to dtbefore/dtlow/dtmid to match TRF

 Revision 1.1  2014/09/30 01:33:49  mwitthoe
 Add ahsxs library with a structure & routine to read a time interval CALDB file used by sxsflagpix and sxssecid


*/
