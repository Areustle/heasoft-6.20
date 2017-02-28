/// \file sxsdt.h
/// \brief Read CALDB file containing time intervals needed by SXS tasks,
///  e.g. sxsflagpix and sxssecid
/// \author Mike Witthoeft
/// \date $Date: 2015/12/03 19:33:57 $

/// \addtogroup mod_ahsxs
/// \section ahsxs_sxsdt SXS time intervals - sxsdt
///
/// Read time intervals needed by SXS tasks from CALDB file.  The interval
/// values may change occasionally, so each set of values is associated with
/// a mission time.  The intervals are:
///
/// - dtprimary: used to determing if an event (A) is a primary or secondary;
///   if another event (B) occurs within the number of seconds given by this
///   column, the event (A) is a secondary, otherwise it is a primary.  This
///   value is needed by the sxssecid task.
///
/// - dtlowmid: If an event (A) is followed by another event (B) within the
///   number of seconds given by this value, the event (B) is a low-resolution
///   secondary (Ls) of event A.  If event (A) is a primary (see dtprimary),
///   then it will be labeled as a low-resolution primary (Lp).  This value
///   is needed by the sxssecid task.
///  
/// - dtmidhigh: If an event (A) is followed by another event (B) within the
///   number of seconds given by this value, the event (B) is a mid-resolution
///   secondary (Ms) of event A.  If event (A) is a primary (see dtprimary),
///   then it will be labeled as a mid-resolution primary (Mp).  This value
///   is needed by the sxssecid task.
///
/// - antdtpre: A PIXEL event is considered coincident with an ANTICO event if
///   it occurs within the number of seconds given by this value before the
///   ANTICO event.  This value is needed by the sxsflagpix task.
///
/// - antdtfol: A PIXEL event is considered coincident with an ANTICO event if
///   it occurs within the number of seconds given by this value after the
///   ANTICO event.  This value is needed by the sxsflagpix task.
///
/// - antshift: All ANTICO events are shifted by this number of seconds before
///   checking for coincidence with PIXEL events; see the andtpre and antdtfol
///   values.  This value is needed by the sxsflagpix task.  This is a two-
///   element array: index 0: PSP A; index 1: PSP B.
///
/// - proxdt: Flag events as being near in time if they occur within this
///   interval.
///
/// - ctrecdt: In order for two events to be flagged as recoil cross-talk, they
///   must occur within the number of seconds given by this value.  This value
///   is needed by the sxsflagpix task.
///
/// - cteldt: In order for two events to be flagged as electrical cross-talk,
///   they must occur within the number of seconds given by this value.  This
///   value is needed by the sxsflagpix task.
///
/// - cteldt2: Same as cteldt, but a different magnitude
///
/// - mxsdt: If an event occurs within the number of seconds given by this value
///   after then end of an MXS GTI, the it will be flagged as MXS afterglow.
///   This value is needed by the sxsflagpix task.
///


#ifndef AHSXS_SXSDT_H
#define AHSXS_SXSDT_H

#include "ahgen/ahversion.h"
AHVERSION(AHSXS_SXSDT,"$Id: sxsdt.h,v 1.9 2015/12/03 19:33:57 mwitthoe Exp $")

#include <string>

/// \ingroup mod_ahsxs

namespace ahsxs {

namespace sxsdt {

/** \addtogroup mod_ahsxs
 *  @{
 */

/// \brief Structure containing a single set of time intervals.  All quantities
///  are defined at the top of the header file.
struct SXSTimeIntervals {

  SXSTimeIntervals(): m_dtprimary(0.), m_dtlowmid(0.), m_dtmidhigh(0.),
                      m_antdtpre(0.), m_antdtfol(0.), m_proxdt(0.),
                      m_ctrecdt(0.), m_cteldt(0.), m_cteldt2(0.), m_mxsdt(0.) {
                        m_antshift[0]=0.; 
                        m_antshift[1]=0.;
                      }

  double m_dtprimary;    // task: sxssecid
  double m_dtlowmid;     // task: sxssecid
  double m_dtmidhigh;    // task: sxssecid
  double m_antdtpre;     // task: sxsflagpix
  double m_antdtfol;     // task: sxsflagpix
  double m_antshift[2];  // task: sxsflagpix
  double m_proxdt;       // task: sxsflagpix
  double m_ctrecdt;      // task: sxsflagpix
  double m_cteldt;       // task: sxsflagpix
  double m_cteldt2;      // task: sxsflagpix
  double m_mxsdt;        // task: sxsflagpix
};


/// \brief Read CALDB file with SXS time intervals and load set corresponding
///  to given TSTART.  The chosen set will come from the first row with a 
///  mission time greater than or equal to TSTART.  If there are no rows with
///  a TIME >= TSTART, then the row with the closest TIME will be used.
/// \param[in] filename name of CALDB file
/// \param[in] tstart time used to select set of interval times
/// \param[out] dts SXSTimeIntervals structure with output
void loadSXSTimeIntervals(const std::string & filename, double tstart,
                       SXSTimeIntervals & dts);


/** @} */

}  // namespace sxsdt

}  // namespace ahsxs

#endif /* AHSXS_SXSDT */

/* Revision Log

 $Log: sxsdt.h,v $
 Revision 1.9  2015/12/03 19:33:57  mwitthoe
 ahsxs library: support CTEL2DT column in delta-times CALDB file; assign 2 bits in STATUS for a 2nd electrical cross-talk flagging

 Revision 1.8  2015/10/02 20:17:47  mwitthoe
 ahsxs library: remove redundant CALDB resolve functions; see issue 535

 Revision 1.7  2015/07/13 15:10:35  mdutka
 adding caldb query for pixmap and dt file

 Revision 1.6  2015/03/23 17:37:37  mwitthoe
 ahsxs library: update format of DT CALDB file; see issues 496/497

 Revision 1.5  2015/03/11 17:08:08  mwitthoe
 ahsxs library: add PROXDT column to sxsdt CALDB file

 Revision 1.4  2014/11/19 19:04:56  mwitthoe
 ahsxs: add library to read CALDB file containing coefficients needed for computing the SampleCnt column for event files (sxssamcnt); see issue 457

 Revision 1.3  2014/10/01 01:26:01  mwitthoe
 ahsxs: column names for time interval CALDB file were changed

 Revision 1.2  2014/09/30 08:28:32  mwitthoe
 ahsxs: change column names secbefore/seclow/secmid to dtbefore/dtlow/dtmid to match TRF

 Revision 1.1  2014/09/30 01:33:49  mwitthoe
 Add ahsxs library with a structure & routine to read a time interval CALDB file used by sxsflagpix and sxssecid


*/
