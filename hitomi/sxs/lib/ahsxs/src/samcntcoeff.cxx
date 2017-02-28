/// \file samcntcoeff.cxx
/// \brief Read CALDB file containing coefficients used to compute the
///  SampleCnt column in SXS event files
/// \author Mike Witthoeft
/// \date $Date: 2015/10/29 20:14:30 $

#define AHLABEL ahsxs_samcntcoeff
#define AHCVSID "$Id: samcntcoeff.cxx,v 1.4 2015/10/29 20:14:30 mwitthoe Exp $"

#include "ahsxs/samcntcoeff.h"
#include "ahfits/ahfits.h"
#include "ahlog/ahlog.h"

namespace ahsxs {

namespace samcntcoeff {

void loadCoefficients(const std::string & filename, double tstart,
                      Coefficients& coeff){

  // local variables to connect with FITS columns
  double l_time=0.;
  Coefficients l_coeff;

  // ahfits file accessor for FITS file
  ahfits::FilePtr fptr=0;

  // open file
  ahfits::open(filename,"",&fptr);

  // if extended syntax not used, move to COEFFS HDU
  if (ahfits::isPrimary(fptr)) ahfits::move(fptr,"ARRCOEFFS");

  // check if able to read file
  if (!ahfits::readOK(fptr)) {
    AH_THROW_RUNTIME("failed to open SXS Time Interval CALDB file: "+filename);
  }

  // setup router for reading rows
  ahfits::Router router(fptr);
  router.connectScalar(ahfits::e_READONLY,"TIME",l_time);
  router.connectFixedLengthArray(ahfits::e_READONLY,"AH",l_coeff.m_ah);
  router.connectFixedLengthArray(ahfits::e_READONLY,"BH",l_coeff.m_bh);
  router.connectFixedLengthArray(ahfits::e_READONLY,"CH",l_coeff.m_ch);
  router.connectFixedLengthArray(ahfits::e_READONLY,"AM",l_coeff.m_am);
  router.connectFixedLengthArray(ahfits::e_READONLY,"BM",l_coeff.m_bm);
  router.connectFixedLengthArray(ahfits::e_READONLY,"CM",l_coeff.m_cm);
  router.connectFixedLengthArray(ahfits::e_READONLY,"AL",l_coeff.m_al);
  router.connectFixedLengthArray(ahfits::e_READONLY,"BL",l_coeff.m_bl);
  router.connectFixedLengthArray(ahfits::e_READONLY,"CL",l_coeff.m_cl);

  // Store time from previous row
  double p_time=-1.;

  // read table until reach row with TIME > TSTART
  for (ahfits::firstRow(fptr); ahfits::readOK(fptr); ahfits::nextRow(fptr)) {
    ahfits::readRow(fptr);

    if (l_time > tstart) break;
    p_time=l_time;
    coeff=l_coeff;
  }

  // close FITS file
  ahfits::close(fptr);

  // If 1st row had a time larger than the given time, then return 
  // values from first row
  if (p_time < 0.) coeff=l_coeff;

}

// ---------------------------------------------------------------------------

}  // namespace samcntcoeff

}  // namespace ahsxs

/* Revision Log
 $Log: samcntcoeff.cxx,v $
 Revision 1.4  2015/10/29 20:14:30  mwitthoe
 ahsxs library: change search of ARRCOEFFS CALDB file so that the returned values have the largest TIME smaller than the given TSTART value

 Revision 1.3  2015/03/23 18:46:28  mwitthoe
 ahsxs library: change name of COEFFS extension to ARRCOEFS

 Revision 1.2  2015/03/23 17:37:37  mwitthoe
 ahsxs library: update format of DT CALDB file; see issues 496/497

 Revision 1.1  2014/11/19 19:04:56  mwitthoe
 ahsxs: add library to read CALDB file containing coefficients needed for computing the SampleCnt column for event files (sxssamcnt); see issue 457


*/
