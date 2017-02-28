/// \file samcntcoeff.h
/// \brief Read CALDB file containing coefficients used to compute the
///  SampleCnt column in SXS event files
/// \author Mike Witthoeft
/// \date $Date: 2015/03/23 17:37:37 $

/// \addtogroup mod_ahsxs
/// \section ahsxs_samcntcoeff SXS SampleCnt Coefficients - samcntcoeff
///


#ifndef AHSXS_SAMCNTCOEFF_H
#define AHSXS_SAMCNTCOEFF_H

#include "ahgen/ahversion.h"
AHVERSION(AHSXS_SAMCNTCOEFF,"$Id: samcntcoeff.h,v 1.2 2015/03/23 17:37:37 mwitthoe Exp $")

#include <string>

/// \ingroup mod_ahsxs

namespace ahsxs {

namespace samcntcoeff {

/** \addtogroup mod_ahsxs
 *  @{
 */

/// \brief number of SXS pixels
const int NUMPIXEL=36;

/// \brief Structure containing a set of coefficients valid at a single time.
struct Coefficients {

  Coefficients() {
    for (int i=0; i < NUMPIXEL; i++) {
      m_ah[i]=0.;
      m_bh[i]=0.;
      m_ch[i]=0.;
      m_am[i]=0.;
      m_bm[i]=0.;
      m_cm[i]=0.;
      m_al[i]=0.;
      m_bl[i]=0.;
      m_cl[i]=0.;
    }
  }

  double m_ah[NUMPIXEL];     // A coefficient for high-resolution events
  double m_bh[NUMPIXEL];     // B coefficient for high-resolution events
  double m_ch[NUMPIXEL];     // C coefficient for high-resolution events
  double m_am[NUMPIXEL];     // A coefficient for mid-resolution events
  double m_bm[NUMPIXEL];     // B coefficient for mid-resolution events
  double m_cm[NUMPIXEL];     // C coefficient for mid-resolution events
  double m_al[NUMPIXEL];     // A coefficient for low-resolution events
  double m_bl[NUMPIXEL];     // B coefficient for low-resolution events
  double m_cl[NUMPIXEL];     // C coefficient for low-resolution events
};


/// \brief Read CALDB file with SXS SampleCnt coefficients and load set 
///  corresponding to given TSTART.  The chosen set will come from the first
///  row with a time greater than or equal to TSTART.  If there are no rows
///  with a TIME >= TSTART, then the row with the closest TIME will be used.
/// \param[in] filename name of CALDB file
/// \param[in] tstart time used to select set of interval times
/// \param[out] coeff Coefficients structure with output
void loadCoefficients(const std::string & filename, double tstart,
                      Coefficients& coeff);


/** @} */

}  // namespace samcntcoeff

}  // namespace ahsxs

#endif /* AHSXS_SAMCNTCOEFF */

/* Revision Log

 $Log: samcntcoeff.h,v $
 Revision 1.2  2015/03/23 17:37:37  mwitthoe
 ahsxs library: update format of DT CALDB file; see issues 496/497

 Revision 1.1  2014/11/19 19:04:56  mwitthoe
 ahsxs: add library to read CALDB file containing coefficients needed for computing the SampleCnt column for event files (sxssamcnt); see issue 457


*/
