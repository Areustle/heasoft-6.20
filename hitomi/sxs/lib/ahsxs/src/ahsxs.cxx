/// \file ahsxs.cxx
/// \brief General SXS functions
/// \author Mike Witthoeft
/// \date $Date: 2015/02/11 15:03:53 $

#define AHLABEL ahsxs_ahsxs
#define AHCVSID "$Id: ahsxs.cxx,v 1.2 2015/02/11 15:03:53 mdutka Exp $"

#include "ahsxs/ahsxs.h"
#include "ahlog/ahlog.h"

namespace ahsxs {


// ---------------------------------------------------------------------------

bool areGradeIntervalsValid(double dtprimary,double dtlowmid, double dtmidhigh) {
  bool okay=true;
  if (dtprimary < 0.) {
    okay=false;
    AH_INFO(ahlog::HIGH) << "dtprimary must be positive" << std::endl;
  }
  if (dtlowmid < 0.) {
    okay=false;
    AH_INFO(ahlog::HIGH) << "dtlowmid must be positive" << std::endl;
  }
  if (dtmidhigh < 0.) {
    okay=false;
    AH_INFO(ahlog::HIGH) << "dtmidhigh must be positive" << std::endl;
  }
  if (dtlowmid > dtmidhigh) {
    okay=false;
    AH_INFO(ahlog::HIGH) << "dtlowmid cannot be larger than dtmidhigh" << std::endl;
  }
  return okay;
}

// ---------------------------------------------------------------------------

int calcGrade(double tprior, double tevent, double tpost, double dtprimary,
              double dtlowmid, double dtmidhigh) {

  // argument validity checks
  if (tevent < tprior || tevent > tpost) 
    AH_THROW_RUNTIME("input times not in order; cannot assign grade");

  // check if event is a primary
  bool isprimary=false;
  if (tevent-tprior > dtprimary) isprimary=true;

  // get resolution of event
  int grade=0;    // 0=low, 1=mid, 2=high
  if ((tpost-tevent > dtlowmid) && (tevent-tprior > dtlowmid)) grade=1;
  if ((tpost-tevent > dtmidhigh) && (tevent-tprior > dtmidhigh)) grade=2;

  // figure out ITYPE
  int out=-1;
  if (grade == 2 && isprimary)
    out=0;                                            // Hp
  else if (grade == 1 && isprimary)
    out=1;                                            // Mp
  else if (grade == 0 && isprimary)
    out=3;                                            // Lp
  else if (grade == 0 && !isprimary)
    out=4;                                            // Ls
  else      // grade == mid/high && !isprimary)
    out=2;                                            // Ms
  if (out < 0) AH_THROW_LOGIC("grade never assigned");
  return out;

}

// ---------------------------------------------------------------------------

}  // namespace ahsxs

/* Revision Log
 $Log: ahsxs.cxx,v $
 Revision 1.2  2015/02/11 15:03:53  mdutka
 Change grade such that the previous event is taken into account when assigning medium and low grades

 Revision 1.1  2014/11/19 15:54:24  mwitthoe
 ahsxs library: add general function for grading SXS event data based on the time between adjacent events; this function will be used by sxssecid and sxssimbranch


*/
