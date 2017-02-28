/// \file AhMJDTime.cxx
/// \brief represent time using the Modified Julian Date (MJD) format.
/// \author Mike Witthoeft
/// \date $Date: 2014/09/10 02:41:19 $

#define AHLABEL ahtime_AhMJDTime
#define AHCVSID "$Id: AhMJDTime.cxx,v 1.2 2014/09/10 02:41:19 mwitthoe Exp $"

#include "ahlog/ahlog.h"
#include "ahtime/AhMJDTime.h"

#include <cmath>

namespace ahtime {

// ---------------------------------------------------------------------------

AhMJDTime::AhMJDTime() {
  m_mjdi=0;
  m_mjdf=0.0;
}

// ---------------------------------------------------------------------------

AhMJDTime::AhMJDTime(long mjdi, double mjdf) {
  set(mjdi,mjdf);
}

// ---------------------------------------------------------------------------

AhMJDTime::AhMJDTime(double mjd) {
  set(mjd);
}

// ---------------------------------------------------------------------------

void AhMJDTime::set(long mjdi, double mjdf) {
  m_mjdi=mjdi;
  m_mjdf=mjdf;
}

// ---------------------------------------------------------------------------

void AhMJDTime::set(double mjd) {
  m_mjdi=(long)mjd;
  m_mjdf=mjd-(double)m_mjdi;
}

// ---------------------------------------------------------------------------

double AhMJDTime::mjd() const {
  return (double)m_mjdi+m_mjdf;
}

// ---------------------------------------------------------------------------

long AhMJDTime::mjdi() const {
  return m_mjdi;
}

// ---------------------------------------------------------------------------

double AhMJDTime::mjdf() const {
  return m_mjdf;
}

// ---------------------------------------------------------------------------

int AhMJDTime::compare(const AhMJDTime & dt) const {
  if (mjdi() < dt.mjdi()) return -1;
  if (mjdi() > dt.mjdi()) return +1;

  if (mjdf() < dt.mjdf()) return -1;
  if (mjdf() > dt.mjdf()) return +1;

  return 0;
}

// ---------------------------------------------------------------------------

void AhMJDTime::addSeconds(double seconds) {
  // Convert seconds into whole days and fractional days.  The algorithm
  // attempts to maximize the precision of the fractional value.
  long dmjdi=(int)(seconds/ahtime::SEC_IN_DAY);
  double dmjdf=(seconds-dmjdi*ahtime::SEC_IN_DAY)/ahtime::SEC_IN_DAY;

  // add to existing time; ensure valid range of mjdf
  m_mjdi+=dmjdi;
  m_mjdf+=dmjdf;
  if (m_mjdf >= 1.) {
    m_mjdi++;
    m_mjdf-=1.;
  }
}

// ---------------------------------------------------------------------------

void AhMJDTime::subtractSeconds(double seconds) {
  // Convert seconds into whole days and fractional days.  The algorithm
  // attempts to maximize the precision of the fractional value.
  long dmjdi=(int)(seconds/ahtime::SEC_IN_DAY);
  double dmjdf=(seconds-dmjdi*ahtime::SEC_IN_DAY)/ahtime::SEC_IN_DAY;

  // add to existing time; ensure valid range of mjdf
  m_mjdi-=dmjdi;
  m_mjdf-=dmjdf;
  if (m_mjdf < 0.) {
    m_mjdi--;
    m_mjdf+=1.;
  }
}

// ---------------------------------------------------------------------------

}

/* Revision Log
 $Log: AhMJDTime.cxx,v $
 Revision 1.2  2014/09/10 02:41:19  mwitthoe
 ahtime library: move timfile CALDB library to ahmission; move leapsecond CALDB library from ahmission; update AhDateTime & AhMJDTime classes and functions in ahtimeconv to support new version of ahtimeconv

 Revision 1.1  2013/09/18 14:09:13  mwitthoe
 ahtime library: add classes to store times in the DateTime and MJD formats


*/
