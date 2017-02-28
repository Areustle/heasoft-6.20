#include <XSFit/StatMethod/Cstat/CstatVariants.h>
#include <XSFit/StatMethod/Cstat/LorStatT.h>
#include <XSUtil/Numerics/LnBang.h>

#include <XSstreams.h>
#include <cmath>
#include <iostream>

const string StdCstat::cmdName = string("cstat");
const string StdCstat::fullName = string("C-Statistic");
const string StdCstat::scriptName = string("C-Statistic");
const Real StdCstat::FLOOR = 1.0e-5;

// calculate the statistic value in the case of no background.

int StdCstat::specificPerform(const RealArray& s, const RealArray& ts, 
			      const RealArray& y, const int n, Real& stat)
{
  using namespace std;
  
  // bin up
  RealArray sbin, tsbin, ybin;
  cstatRebinNoB(s,ts,y,n,sbin,tsbin,ybin);

  size_t N = sbin.size();
  for (size_t i=0; i<N; i++) {

    Real si = sbin[i];
    Real yi = ybin[i];
    Real tsi = tsbin[i];

    yi = std::max(yi, FLOOR/tsi);
    stat += tsi*yi;
    if ( si > 0 ) stat += si*(std::log(si)-std::log(tsi*yi)-1);

    if ( isnan(stat) && !isnan(yi) ) return i;

  }
  return -1;

} // end specificPerform

// calculate the statistic value in the presence of background. Note that bierr is not used in 
// this case. This code is to calculate W.

int StdCstat::specificPerformB(const RealArray& s, const RealArray& b, 
			       const RealArray& berr, const RealArray& ts, 
			       const RealArray& tb, const RealArray& y, 
			       const int n, Real& stat)
{
  // bin up
  RealArray sbin, bbin, tsbin, tbbin, ybin;
  cstatRebinB(s,b,ts,tb,y,n,sbin,bbin,tsbin,tbbin,ybin);

  size_t N = sbin.size();
  for (size_t i=0; i<N; i++) {
    Real si = sbin[i];
    Real bi = bbin[i];
    Real tsi = tsbin[i];
    Real tbi = tbbin[i];
    Real yi = ybin[i];

    Real ti = tsi + tbi;
    yi = std::max(yi, FLOOR/tsi);
    if (si == 0.0) {
      stat += tsi*yi - bi*std::log(tbi/ti);
    } else {
      if (bi == 0.0) {
	if ( yi <= si/ti ) {
	  stat += -tbi*yi - si*std::log(tsi/ti);
	} else {
	  stat += tsi*yi + si*(std::log(si)-std::log(tsi*yi)-1);
	}
      } else {
	// now the main case where both data and background !=0
	// Solve quadratic equation for f. Use the positive root to ensure
	// that f > 0.
	Real a = ti;
	Real b = ti*yi - si - bi;
	Real c = -bi*yi;
	Real d = sqrt(b*b - 4.0*a*c);
	// Avoid round-off error problems if b^2 >> 4ac (see eg Num.Recipes)
	Real f;
	if ( b >= 0.0 ) {
	  f = -2*c / (b + d);
	} else {
	  f = -(b - d) / (2*a);
	}

	// note that at this point f must be > 0 so the log 
	// functions below will be valid.

	stat += tsi*yi + ti*f - si*std::log(tsi*yi+tsi*f)
	  - bi*std::log(tbi*f) - si*(1-std::log(si)) 
	  - bi*(1-std::log(bi));
      }

    }

  }

  return -1;

} // end specificPerformB

// calculate statistic derivatives in the case of no background. Note that
// pDiff1 is minus half the first derivative and pDiff2 is half the second derivative

int StdCstat::specificResetCalc(const RealArray& s, const RealArray& ts, 
				const RealArray& y, const int n, RealArray& diff1, 
				RealArray& diff2)
{
  // bin up
  RealArray sbin, tsbin, ybin;
  cstatRebinNoB(s,ts,y,n,sbin,tsbin,ybin);

  size_t N = sbin.size();
  for (size_t i=0; i<N; i++) {
    Real si = sbin[i];
    Real tsi = tsbin[i];
    Real yi = ybin[i];

    yi = std::max(yi, FLOOR/tsi);
    diff1[i] = (si/yi) - tsi;
    diff2[i] = si/(yi*yi);
  }
  return -1;
}

// calculate the statistic derivatives in the case of background. Note that bierr 
// is not used in this case. This code is to calculate W. The simple case of no 
// background (ie C) is calculated in Cstat.h. pDiff1 returns minus half the 
// derivative of W and pDiff2 returns half the second derivative (where second
// derivatives of yi wrt the parameters are assumed to be small).

int StdCstat::specificResetCalcB(const RealArray& s, const RealArray& b, 
				 const RealArray& berr, const RealArray& ts, 
				 const RealArray& tb, const RealArray& y, 
				 const int n, RealArray& diff1, RealArray& diff2)
{
  // bin up
  RealArray sbin, bbin, tsbin, tbbin, ybin;
  cstatRebinB(s,b,ts,tb,y,n,sbin,bbin,tsbin,tbbin,ybin);

  size_t N = sbin.size();
  for (size_t i=0; i<N; i++) {
    Real si = sbin[i];
    Real bi = bbin[i];
    Real tsi = tsbin[i];
    Real tbi = tbbin[i];
    Real yi = ybin[i];

    Real ti = tsi + tbi;
    yi = std::max(yi, FLOOR/tsi);
    if (si == 0.0) {
      // Special case if the data = 0 - note that in this case the 
      // derivatives are independent of the background values
      diff1[i] = -tsi;
      diff2[i] = 0.;
   } else {
      if (bi ==0.0) {
	// If background bin is 0 but data is non-zero
	if (yi <= si/ti) {
	  diff1[i] = tbi;
	  diff2[i] = 0.;
	} else {
	  diff1[i] = (si/yi) - tsi;
	  diff2[i] = si/(yi*yi);
	}
      } else {
	// Solve quadratic equation for fi
	// Note that b^2-4ac is always >= b^2 so we use the positive
	// square root to ensure that f is > 0. 
	Real a = ti;
	Real b = ti*yi - si - bi;
	Real c = -bi*yi;
	Real d = sqrt(b*b - 4.0*a*c);
	// Avoid round-off error problems if b^2 >> 4ac (see eg Num.Recipes)
	Real f;
	if ( b >= 0.0 ) {
	  f = -2*c / (b + d);
	} else {
	  f = -(b - d) / (2*a);
	}
	Real g = (ti*yi - si + bi - d)/(2.0*d);
	Real h = 2.0*ti*si*bi/(d*d*d);
	diff1[i] = si*(1+g)/(yi+f) + bi*g/f - tsi -g*ti;
        diff2[i] = -si*h/(yi+f) + si*(1+g)*(1+g)/((yi+f)*(yi+f)) 
	  - bi*h/f + bi*g*g/(f*f) + ti*h;
      }
    }
  }
  return -1;

} // end specificResetCalcB

bool StdCstat::specificValidStatistic(bool PoissonSource, bool PoissonBackground)
{
  return (PoissonSource && PoissonBackground);
}

void cstatRebinNoB(const RealArray& s, const RealArray& ts, const RealArray& y,
		   const int n, RealArray& sbin, RealArray& tsbin, RealArray& ybin)
{
  if ( n == 1 ) {

    sbin.resize(s.size());
    tsbin.resize(ts.size());
    ybin.resize(y.size());
    for (size_t i=0; i<s.size(); i++) {
      sbin[i] = s[i];
      tsbin[i] = ts[i];
      ybin[i] = y[i];
    }

  } else {

    // first run through to find the number of output bins
    int nout = 0;
    Real ssum = 0.0;
    Real tssum = 0.0;
    for (size_t i=0; i<s.size(); i++) {
      ssum += s[i]*ts[i];
      tssum += ts[i];
      if ( ssum >= n ) {
	nout++;
	ssum = 0.0;
	tssum = 0.0;
      }
    }

    sbin.resize(nout);
    tsbin.resize(nout);
    ybin.resize(nout);
    
    size_t ibin = 0;
    int nbin = 0;
    ssum = 0.0;
    tssum = 0.0;
    Real ysum = 0.0;
    for (size_t i=0; i<s.size(); i++) {
      ssum += s[i]*ts[i];
      tssum += ts[i];
      ysum += y[i]*ts[i];
      nbin++;
      if ( ssum >= n ) {
	tssum /= nbin;
	sbin[ibin] = ssum/tssum;
	tsbin[ibin] = tssum;
	ybin[ibin] = ysum/tssum;
	nbin = 0;
	ssum = 0.0;
	tssum = 0.0;
	ysum = 0.0;
	ibin++;
      }
    }
  }

}

void cstatRebinB(const RealArray& s, const RealArray& b, const RealArray& ts, 
		 const RealArray& tb, const RealArray& y, const int n, 
		 RealArray& sbin, RealArray& bbin, RealArray& tsbin, 
		 RealArray& tbbin, RealArray& ybin)
{
  if ( n == 1 ) {

    sbin.resize(s.size());
    bbin.resize(b.size());
    tsbin.resize(ts.size());
    tbbin.resize(tb.size());
    ybin.resize(y.size());
    for (size_t i=0; i<s.size(); i++) {
      sbin[i] = s[i];
      bbin[i] = b[i];
      tsbin[i] = ts[i];
      tbbin[i] = tb[i];
      ybin[i] = y[i];
    }

  } else {

    // first run through to find the number of output bins
    int nout = 0;
    Real ssum = 0.0;
    Real tssum = 0.0;
    for (size_t i=0; i<s.size(); i++) {
      ssum += s[i]*ts[i];
      tssum += ts[i];
      if ( ssum >= n ) {
	nout++;
	ssum = 0.0;
	tssum = 0.0;
      }
    }

    sbin.resize(nout);
    bbin.resize(nout);
    tsbin.resize(nout);
    tbbin.resize(nout);
    ybin.resize(nout);
    
    size_t ibin = 0;
    int nbin = 0;
    ssum = 0.0;
    Real bsum = 0.0;
    tssum = 0.0;
    Real tbsum = 0.0;
    Real ysum = 0.0;
    for (size_t i=0; i<s.size(); i++) {
      ssum += s[i]*ts[i];
      bsum += b[i]*tb[i];
      tssum += ts[i];
      tbsum += tb[i];
      ysum += y[i]*ts[i];
      nbin++;
      if ( ssum >= n ) {
	tssum /= nbin;
	tbsum /= nbin;
	sbin[ibin] = ssum/tssum;
	bbin[ibin] = bsum/tbsum;
	tsbin[ibin] = tssum;
	tbbin[ibin] = tbsum;
	ybin[ibin] = ysum/tssum;
	nbin = 0;
	ssum = 0.0;
	bsum = 0.0;
	tssum = 0.0;
	tbsum = 0.0;
	ysum = 0.0;
	ibin++;
      }
    }
  }

}

// end StdCstat

const string PGstat::cmdName = string("pgstat");
const string PGstat::fullName = string("PG-Statistic");
const string PGstat::scriptName = string("PG-Statistic");
const Real PGstat::FLOOR = 1.0e-5;

// calculate the statistic value if no background

int PGstat::specificPerform(const RealArray& s, const RealArray& ts, 
			    const RealArray& y, const int n, Real& stat)
{
  using namespace std;
  
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {

    Real si = s[i];
    Real yi = y[i];
    Real tsi = ts[i];

    yi = std::max(yi, FLOOR/tsi);
    stat += tsi*yi;
    if ( si > 0 ) stat += si*(std::log(si)-std::log(tsi*yi)-1);

    if ( isnan(stat) && !isnan(yi) ) return i;

  }
  return -1;

} // end specificPerform

// calculate the statistic value in the presence of background. bierr is the sigma for the background

int PGstat::specificPerformB(const RealArray& s, const RealArray& b, 
			     const RealArray& berr, const RealArray& ts, 
			     const RealArray& tb, const RealArray& y, 
			     const int n, Real& stat)
{
  using namespace std; // Musn't hardcode "std::" to isnan on Solaris.

  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real bi = b[i];
    Real bierr = berr[i];
    Real tsi = ts[i];
    Real tbi = tb[i];
    Real yi = y[i];

    Real tri = tsi / tbi;
    Real fi=0.;

    // special case for bierr = 0
    if ( bierr == 0.0 ) {

      Real ybi = std::max(yi+bi/tbi, FLOOR/tsi);
      stat += tsi*ybi;
      if ( si > 0 ) stat += si*(std::log(si)-std::log(tsi*ybi)-1);

    } else {

      if (si == 0.0) {
	stat += tsi*yi + bi*tri - 0.5*bierr*tri*tri;
      } else {
	// Solve quadratic equation for fi, using Numerical Recipes technique
	// to avoid round-off error that can easily cause problems here
	// when b^2 >> ac.
	Real a = tbi*tbi;
	Real b = tsi*bierr - tbi*bi + tbi*tbi*yi;
	Real c = tsi*bierr*yi -si*bierr - tbi*bi*yi;
	Real sign = (b >= .0) ? 1.0 : -1.0;
	Real q = -.5*(b + sign*sqrt(b*b - 4.0*a*c));
	fi = q/a;
	if (fi < .0) fi = c/q;
	
	// note that at this point fi must be > 0 so the log 
	// functions below will be valid.

	stat += tsi*(yi+fi) - si*std::log(tsi*yi+tsi*fi)
	  + 0.5*(bi-tbi*fi)*(bi-tbi*fi)/bierr - si*(1-std::log(si)); 

	if (isnan(stat) && !isnan(yi) && !isinf(yi)) return i;
      }

    }

  }

  return -1;

} // end specificPerformB

// calculate statistic derivatives in the case of no background. Note that
// pDiff1 is minus the first derivative and pDiff2 is half the second derivative.

int PGstat::specificResetCalc(const RealArray& s, const RealArray& ts, 
			      const RealArray& y, const int n, RealArray& diff1, 
			      RealArray& diff2)
{
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real tsi = ts[i];
    Real yi = y[i];

    yi = std::max(yi, FLOOR/tsi);
    diff1[i] = (si/yi) - tsi;
    diff2[i] = si/(yi*yi);
  }
  return -1;
}

// calculate statistic derivatives in the case of background

int PGstat::specificResetCalcB(const RealArray& s, const RealArray& b, 
			       const RealArray& berr, const RealArray& ts, 
			       const RealArray& tb, const RealArray& y, 
			       const int n, RealArray& diff1, RealArray& diff2)
{
  using namespace std; // Musn't hardcode "std::" to isnan on Solaris.

  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real bi = b[i];
    Real bierr = berr[i];
    Real tsi = ts[i];
    Real tbi = tb[i];
    Real yi = y[i];

    // special case for bierr = 0
    if ( bierr == 0 ) {

      Real ybi = std::max(yi+bi/tbi, FLOOR/tsi);
      diff1[i] = (si/ybi) - tsi;
      diff2[i] = si/(ybi*ybi);

    } else {

      yi = std::max(yi, FLOOR/tsi);
      if (si == 0.0) {
	// Special case if the data = 0 - note that in this case the 
	// derivatives are independent of the background values
	diff1[i] = -tsi;
	diff2[i] = 0.;
      } else {
	// Solve quadratic equation for fi
	// Use the positive root to ensure that fi > 0
	Real a = tbi*tbi;
	Real b = tsi*bierr - tbi*bi + tbi*tbi*yi;
	Real b1 = -tsi*bierr + tbi*bi + tbi*tbi*yi;
	Real c = tsi*bierr*yi -si*bierr - tbi*bi*yi;
	Real d = sqrt(b*b - 4.0*a*c);
	// Avoid round-off error problems if b^2 >> 4ac (see eg Num.Recipes)
	Real f;
	if ( b >= 0.0 ) {
	  f = -2*c / (b + d);
	} else {
	  f = -(b - d) / (2*a);
	}
	Real g = (b1 - d)/(2.0*d);
	Real h = tbi*tbi*(d*d - b1*b1)/(2.0*d*d*d);

	diff1[i] = si*(1+g)/(yi+f) + (bi-tbi*f)*tbi*g/bierr - tsi -g*tsi;
	diff2[i] = -si*h/(yi+f) + si*(1+g)*(1+g)/((yi+f)*(yi+f)) 
	  + tbi*tbi*g*g/bierr - (bi-tbi*f)*tbi*h/bierr + tsi*h;

	if ( isnan(diff1[i]) && !isnan(yi) && !isinf(yi) ) return i;
	if ( isnan(diff2[i]) && !isnan(yi) && !isinf(yi) ) return i;

      }

    }

  }

  return -1;

} // end specificResetCalcB

bool PGstat::specificValidStatistic(bool PoissonSource, bool PoissonBackground)
{
  return PoissonSource;
}

// end PGstat

const string Pstat::cmdName = string("pstat");
const string Pstat::fullName = string("P-Statistic");
const string Pstat::scriptName = string("P-Statistic");
const Real Pstat::FLOOR = 1.0e-5;

// calculate the statistic value if no background

int Pstat::specificPerform(const RealArray& s, const RealArray& ts, 
			   const RealArray& y, const int n, Real& stat)
{
  using namespace std;
  
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {

    Real si = s[i];
    Real yi = y[i];
    Real tsi = ts[i];

    yi = std::max(yi, FLOOR/tsi);
    stat += tsi*yi;
    if ( si > 0 ) stat += si*(std::log(si)-std::log(tsi*yi)-1);

    if ( isnan(stat) && !isnan(yi) ) return i;
  }
  return -1;

} // end specificPerform

// calculate the statistic value in the presence of background. bierr is assumed to be zero

int Pstat::specificPerformB(const RealArray& s, const RealArray& b, 
			    const RealArray& berr, const RealArray& ts, 
			    const RealArray& tb, const RealArray& y, 
			    const int n, Real& stat)
{
  using namespace std; // Musn't hardcode "std::" to isnan on Solaris.

  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real bi = b[i];
    Real tsi = ts[i];
    Real tbi = tb[i];
    Real yi = y[i];

    Real ybi = std::max(yi+bi/tbi, FLOOR/tsi);
    stat += tsi*ybi;
    if ( si > 0 ) stat += si*(std::log(si)-std::log(tsi*ybi)-1);
  }
  return -1;

} // end specificPerformB

// calculate statistic derivatives in the case of no background. Note that
// pDiff1 is minus the first derivative and pDiff2 is half the second derivative.

int Pstat::specificResetCalc(const RealArray& s, const RealArray& ts, 
			     const RealArray& y, const int n, RealArray& diff1, 
			     RealArray& diff2)
{
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real tsi = ts[i];
    Real yi = y[i];

    yi = std::max(yi, FLOOR/tsi);
    diff1[i] = (si/yi) - tsi;
    diff2[i] = si/(yi*yi);
  }
  return -1;
}

// calculate statistic derivatives in the case of background. bierr is assumed to be zero.

int Pstat::specificResetCalcB(const RealArray& s, const RealArray& b, 
			      const RealArray& berr, const RealArray& ts, 
			      const RealArray& tb, const RealArray& y, 
			      const int n, RealArray& diff1, RealArray& diff2)
{
  using namespace std; // Musn't hardcode "std::" to isnan on Solaris.

  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real bi = b[i];
    Real tsi = ts[i];
    Real tbi = tb[i];
    Real yi = y[i];

    Real ybi = std::max(yi+bi/tbi, FLOOR/tsi);
    diff1[i] = (si/ybi) - tsi;
    diff2[i] = si/(ybi*ybi);
  }
  return -1;

} // end specificResetCalcB

bool Pstat::specificValidStatistic(bool PoissonSource, bool PoissonBackground)
{
  return PoissonSource;
}

// end PGstat

const string LorStat::cmdName = string("lstat");
const string LorStat::fullName = string("L-Statistic");
const string LorStat::scriptName = string("L-Statistic");
const Real LorStat::FLOOR = 1.0e-5;

const int LorStat::Dterm0::flag = 0;
const int LorStat::Dterm1::flag = 1;
const int LorStat::Dterm2::flag = 2;

// calculate the statistic value if no background

int LorStat::specificPerform(const RealArray& s, const RealArray& ts, 
			     const RealArray& y, const int n, Real& stat)
{
  using namespace std;
  
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {

    Real si = s[i];
    Real yi = y[i];
    Real tsi = ts[i];

    yi = std::max(yi, FLOOR/tsi);
    stat += tsi*yi;
    if ( si > 0 ) stat += si*(std::log(si)-std::log(tsi*yi)-1);

    if ( isnan(stat) && !isnan(yi) ) return i;

  }
  return -1;

} // end specificPerform

// calculate the statistic value in the presence of background. Note that bierr is not 
// used in this case

int LorStat::specificPerformB(const RealArray& s, const RealArray& b, 
			      const RealArray& berr, const RealArray& ts, 
			      const RealArray& tb, const RealArray& y, 
			      const int n, Real& stat)
{
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real bi = b[i];
    Real tsi = ts[i];
    Real tbi = tb[i];
    Real yi = y[i];

    const int sourceCounts = static_cast<int>(std::floor(si+.5));
    const int backgroundCounts = static_cast<int>(std::floor(bi+.5));
    Real totScale = tsi + tbi;
    Real sat = si/tsi - bi/tbi;
    sat = std::max(.0, sat);

    // The lorsum routine returns the log of the summation 
    // from 0 to nobs.
    Real term1 = lorSum<Dterm0>(yi*totScale, sourceCounts, backgroundCounts);
    stat += yi*tsi - term1;
    Real term2 = lorSum<Dterm0>(sat*totScale, sourceCounts, backgroundCounts);
    stat += term2 - sat*tsi;

  }
  return -1;

} // end specificPerformB

// calculate statistic derivatives in the case of no background. Note that
// pDiff1 is minus the first derivative and pDiff2 is half the second derivative.

int LorStat::specificResetCalc(const RealArray& s, const RealArray& ts, 
			       const RealArray& y, const int n, RealArray& diff1, 
			       RealArray& diff2)
{
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real tsi = ts[i];
    Real yi = y[i];

    yi = std::max(yi, FLOOR/tsi);
    diff1[i] = (si/yi) - tsi;
    diff2[i] = si/(yi*yi);
  }
  return -1;
}

// calculate the statistic derivative in the case of background. Note that bierr is not 
// used in this case

int LorStat::specificResetCalcB(const RealArray& s, const RealArray& b, 
				const RealArray& berr, const RealArray& ts, 
				const RealArray& tb, const RealArray& y, 
				const int n, RealArray& diff1, RealArray& diff2)
{
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real bi = b[i];
    Real tsi = ts[i];
    Real tbi = tb[i];
    Real yi = y[i];

    const Real totalScale = tsi + tbi;
    const Real t1 = std::log(totalScale);
    const Real t2 = 2.0*t1;
    int sourceCounts = static_cast<int>(std::floor(si+.5));
    int backgroundCounts = static_cast<int>(std::floor(bi+.5));
    Real f = lorSum<Dterm0>(totalScale*yi, sourceCounts, backgroundCounts);
    Real g = lorSum<Dterm1>(totalScale*yi, sourceCounts, backgroundCounts) + t1;
    Real h = lorSum<Dterm2>(totalScale*yi, sourceCounts, backgroundCounts) + t2;

    diff1[i] = std::exp(g - f) - tsi;
    diff2[i] = std::exp(2.0*(g - f)) - std::exp(h - f);
  }
  return -1;

} // end specificResetCalcB

bool LorStat::specificValidStatistic(bool PoissonSource, bool PoissonBackground)
{
  return (PoissonSource && PoissonBackground);
}

Real LorStat::lorTerm (const int n1, const int n2, const int n3)
{
   // Function used in the calculation of the 
   // background-marginalized likelihood.  
   // Returns  LOG ( (n1+n2-n3)!/(n1-n3)/!n3! ).
   Numerics::LnBang lnBang;
   Real result = lnBang((long)(n1+n2-n3)) - lnBang((long)(n1-n3)) - lnBang((long)n3);
   return result;
}


// end LorStat

const string WhittleStat::cmdName = string("whittle");
const string WhittleStat::fullName = string("Whittle");
const string WhittleStat::scriptName = string("Whittle");
const Real WhittleStat::FLOOR = 1.0e-15;

// calculate the statistic value in the case of no background. n is the number of PDFs
// which have been averaged.

int WhittleStat::specificPerform(const RealArray& s, const RealArray& ts, 
			      const RealArray& y, const int n, Real& stat)
{
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {

    Real si = s[i];
    Real yi = y[i];
    Real tsi = ts[i];

    yi = std::max(yi, FLOOR/tsi);
    stat += n*(si/(tsi*yi) + log(tsi*yi));
    if ( n > 1 ) {
      if ( si > 0.0 ) {
	stat += (1-n)*log(si);
      } else {
	stat += (1-n)*log(FLOOR);
      }
    }

  }
  return -1;

} // end specificPerform

// calculate the statistic value in the presence of background. This does not do anything
// with any background information.

int WhittleStat::specificPerformB(const RealArray& s, const RealArray& b, 
				  const RealArray& berr, const RealArray& ts, 
				  const RealArray& tb, const RealArray& y, 
				  const int n, Real& stat)
{
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real tsi = ts[i];
    Real yi = y[i];

    yi = std::max(yi, FLOOR/tsi);
    stat += n*(si/(tsi*yi) + log(tsi*yi));
    if ( n > 1 ) {
      if ( si > 0.0 ) {
	stat += (1-n)*log(si);
      } else {
	stat += (1-n)*log(FLOOR);
      }
    }
  }
  return -1;

} // end specificPerformB

// calculate statistic derivatives in the case of no background. Note that
// pDiff1 is minus the first derivative and pDiff2 is half the second derivative.

int WhittleStat::specificResetCalc(const RealArray& s, const RealArray& ts, 
				   const RealArray& y, const int n, RealArray& diff1, 
				   RealArray& diff2)
{
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real tsi = ts[i];
    Real yi = y[i];

    yi = std::max(yi, FLOOR/tsi);
    diff1[i] = si/(tsi*yi*yi) - 1.0/yi;
    diff2[i] = 2.0*si/(tsi*yi*yi*yi) - 1.0/(yi*yi);
  }
  return -1;
}

// calculate the statistic derivatives in the case of background. This does not do anything
// with any background information

int WhittleStat::specificResetCalcB(const RealArray& s, const RealArray& b, 
				    const RealArray& berr, const RealArray& ts, 
				    const RealArray& tb, const RealArray& y, 
				    const int n, RealArray& diff1, RealArray& diff2)
{
  size_t N = s.size();
  for (size_t i=0; i<N; i++) {
    Real si = s[i];
    Real tsi = ts[i];
    Real yi = y[i];

    yi = std::max(yi, FLOOR/tsi);
    diff1[i] = si/(tsi*yi*yi) - 1.0/yi;
    diff2[i] = 2.0*si/(tsi*yi*yi*yi) - 1.0/(yi*yi);
  }
  return -1;
} // end specificResetCalcB

// The Whittle statistic is always assumed to be valid - it runs on power spectra so
// the data are not Poisson.

bool WhittleStat::specificValidStatistic(bool PoissonSource, bool PoissonBackground)
{
  return true;
}

// end WhittleStat


