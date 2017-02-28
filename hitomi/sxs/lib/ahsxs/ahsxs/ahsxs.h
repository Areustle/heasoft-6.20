/// \file ahsxs.h
/// \brief Common routines for SXS tasks
/// \author Mike Witthoeft
/// \date $Date: 2015/10/02 20:39:18 $

#ifndef AHSXS_AHSXS_H
#define AHSXS_AHSXS_H

#include "ahgen/ahversion.h"
AHVERSION(AHSXS_AHSXS,"$Id: ahsxs.h,v 1.7 2015/10/02 20:39:18 mwitthoe Exp $")

#include "ahsxs/sxsdt.h"        // CALDB file with various SXS time intervals
#include "ahsxs/samcntcoeff.h"  // CALDB file with coefficients used to compute SampleCnt
#include "ahsxs/sxsstatus.h"    // functions to set/check the SXS STATUS column

/// \ingroup mod_ahsxs

namespace ahsxs {

/** \addtogroup mod_ahsxs
 *  @{
 */

  
  
/// \brief Number of pixels for SXS
const int NPIXEL=36;
  
  
  
/// \brief Check if time intervals used in event grading are valid.
bool areGradeIntervalsValid(double dtprimary,double dtlowmid, double dtmidhigh);

/// \brief Calculate the grade (ITYPE) of an event.
/// \param[in] tprior time (sec) of prior event
/// \param[in] tevent time (sec) of event whose grade is to be calculated
/// \param[in] tpost time (sec) of post event 
/// \param[in] dtprimary time interval (sec) defining primary vs secondary
/// \param[in] dtlowmid time interval (sec) defining low vs mid resolution
/// \param[in] dtmidhigh time interval (sec) defining mid vs high resolution
/// \return grade as reported in the ITYPE column
///
/// In order to calculate the grade of an event, its time and the times of 
/// the previous and following events are needed.  If the time between the 
/// event and the previous event is greater than dtprimary, then the event
/// is designated as a primary event, otherwise it is secondary.  If the
/// time between the event and the following event is greater than dtmidhigh,
/// then the event is a high-resolution event; if the time difference is 
/// between dtlowmid and dtmidhigh, then the event has medium resolution;
/// otherwise it is a low-resolution event.  The combination of resolution
/// and the primary/secondary state is encoded as:
/// 
///  ITYPE      abbrev        description
///    0          Hp           high-resolution, primary
///    1          Mp           mid-resolution, primary
///    2          Ms           mid-resolution, secondary
///    3          Lp           low-resolution, primary
///    4          Ls           low-resolution, secondary
///
/// Note: high-resolution secondaries are not allowed, so if that combination
/// is found using the algorithm above, then the event is labeled as Ms.  This
/// can only happen if dtprimary != dtmidhigh.
///
/// It is recommended that ahsxs::areGradeIntervalsValid() is called first
/// to check the validity of the time interval constants.
int calcGrade(double tprior, double tevent, double tpost, double dtprimary,
              double dtlowmid, double dtmidhigh);

/** @} */

}  // namespace ahsxs

#endif   /* AHSXS_AHSXS_H */

/* Revision Log
   $Log: ahsxs.h,v $
   Revision 1.7  2015/10/02 20:39:18  mwitthoe
   ahsxs library: remove unnecessary pixmap include statement from header file

   Revision 1.6  2015/07/16 16:39:11  mdutka
   adding pixmap to ahsxs.h

   Revision 1.5  2015/04/13 18:24:50  klrutkow
   added NPIXEL constant

   Revision 1.4  2014/11/19 19:04:56  mwitthoe
   ahsxs: add library to read CALDB file containing coefficients needed for computing the SampleCnt column for event files (sxssamcnt); see issue 457

   Revision 1.3  2014/11/19 15:54:24  mwitthoe
   ahsxs library: add general function for grading SXS event data based on the time between adjacent events; this function will be used by sxssecid and sxssimbranch

   Revision 1.2  2014/11/18 22:14:44  mwitthoe
   ahsxs: add library to set/check SXS STATUS column bits; these functions will be used by the sxsflagpix and sxsdrift tasks; see issue 445

   Revision 1.1  2014/09/30 01:33:49  mwitthoe
   Add ahsxs library with a structure & routine to read a time interval CALDB file used by sxsflagpix and sxssecid


*/
