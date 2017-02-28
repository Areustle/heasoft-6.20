// xrrtscatter.hh
//
// Class definition for X-ray mirror scattering
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/23 Upgrade documentation. R. Fink
// 1999/02/04 Added ISAS code by A. Furuzawa to support Astro-E scatter 
//            function. R. Fink
// 2002/07/16 Modify astroeFfluct() to support Hidaka Astro-E frontside 
//            scatter function and bug fix. H. Mori
//
// 2005/12/15 Y.ISHISAKI	version 6.2.2
//	significant changes on backside parameter variables
//	rewrite astroeBackStprnd()
//	call setAstroeBxrrtback() before loop in astroeBacksideNvfluct()
// 2006/06/19 Y.ISHISAKI	version 6.3.9
//	move struct AstroeBackA, struct AstroeBackE declarations outside class
// 2006/08/05 Y.ISHISAKI	version 6.4.4
//	add declaration of theScatterInfo()
// 2008/03/02 Y.ISHISAKI	version 6.5.0
//	back astroeFdiffract() [global func] -> XrrtScatter::astroeFdiffract()
//	add astroeWnorm, astroeWpower, astroeGnorm
//	add astroeGcA, astroeGcB, astroePcolGwA, astroePcolGwB
//	modify args of setAstroeBackParams(),setAstroeParams(),setAscaParams()
//	comment out several unused variables
//	rename precollimator -> pcol
//	change private -> public defaultScatterModeSet, defaultScatterMode

#ifndef XRRTSCATTER_HH
#define XRRTSCATTER_HH

//
// System interfaces used
//
// Modified by H. Mori (2005/09/14)
// #include <math.h>
#include <cmath>
#include <exception>
#include "fitsio.h"

// 
// XRRT interfces used
//
#include "xrrt_types.hh"
#include "xrrtmirror.hh"
#include "xrrtcollimator.hh"
#include "xrrtphoton.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// Local Defines
//
enum XrrtScatterErrorCode {
        XrrtScatterErrors,
        noSuchScatterMode,        //"Unsupported scatter mode found!"
        rxOutOfRegion,            // Range error in XrrtScatter::ascaStprnd
        XrrtScatterErrorsEnd
};

enum ScatterType {
        NO_SCATTER=0,
        ASCA_SCATTER=1,
        ASTROE_SCATTER=2
};
// End Local Defines

struct AstroeBackA {
	double bangle;	// [deg]
	double norm;
	double pitch;	// [radian]
	int numBackP;
	double *stpx;	// [radian]
	double *stpy;
};

struct AstroeBackE {
	double energy;	// [keV]
	int numBackA;
	struct AstroeBackA *backA;
};

//
// XrrtScatter handles all scattering of photons
// Note that NO documentation exists for how ASCA scattering is organized
//      since none was available in the original Nagoya ray2areav6_0cal program
//
class  XrrtScatter
{
    // Obtain the only scatter object
    friend XrrtScatter& theScatterInfo();

    public:
          // 
          // Scatter a photon off the front surface of a mirror
          //
          void frontScatter(const XrrtMirror* mirror, XrrtPhoton& photon);
          //
          // Scatter a photon off the back surface of a mirror
          //
          void backScatter(const XrrtMirror* mirror, XrrtPhoton& photon);

          // Add collimator scatter case for ASTRO-E2 Pre-Collimator
          // (added by H. Mori : date 2003/01/14)
          // 
          // Scatter a photon off the front surface of a Pre-Collimator
          //
          void pcolScatter(const XrrtCollimator* pcol, XrrtPhoton& photon);

	  double astroeFdiffract(double refsi, double reflectAngleInRadians, double energyInKeV, double diffractAngleInRadians);

          //
          // Set the ASCA scatter params
          //
          void setAscaParams(const int& consct, 
                             const double& sigma1, 
                             const double& sigma2,
                             const double& alin, 
                             const double& s3, 
                             const double& c3);
          //
          // Return whether ASCA scatter params are set
          //
          bool getAscaParamStatus();

          // Set the ASTROE scatter params
          //
          void setAstroeParams(const int& nvfsw, 
                               const double& gausigma,
			       const double& ngau,
			       const double& expsigma,
			       const double& nexp,
			       const double& lorgamma,
                               const double& nlor,
                               const int& scatsw,
                               const double& w_norm,
                               const double& w_power,
                               const double& g_norm,
                               const double& gc_a,
                               const double& gc_b,
                               const double& gw_a,
                               const double& gw_b);
          //
          // Return whether ASTROE scatter params are set
          //
          bool getAstroeParamStatus();

          // Set the ASTROE backside params
          //
          int readBackProfVer1(fitsfile *fp);
          int readBackProfVer2(fitsfile *fp);
          int setAstroeBackParams(string backprofFileName);

          //
          // Set an override scatter mode to always use
          //
          void setDefaultScatterMode( const SurfaceScatterMode& mode);

          //
          // Convert error codes to error messages
          //
          string errorMessage(XrrtScatterErrorCode errorCode);

          // defaultScatterModeSet variable:
          //     TRUE indicates that the mirror fragments scatter mode are
          //          to be overridden with the value in defaultScatterMode.
          //     FALSE indicates that mirror fragments should use their
          //          intrinsic surface scatter mode.
          bool defaultScatterModeSet;
          SurfaceScatterMode defaultScatterMode;

    private:
          // Constructor
          XrrtScatter(); 
          // Copy Constructor
          XrrtScatter( const XrrtScatter& scatter ); 

          //
          // Member functions to replicate ASCA scattering from the 
          // Nagoya program ray2areav6_0cal
          //
          void ascaScatter(const XrrtMirror* mirror, XrrtPhoton& photon);
          double ascaScat(double reflectAngleInRadians, double energyInKeV);
          double ascaFlor(double sigma2);
          double ascaNvfluct();
          void   ascaStprnd(double& rx, double& y);
          double ascaFfluct(double rx);
          // Astro-E functions
          double astroeScat(const XrrtMirror* mirror, double reflectAngleInRadians, double energyInKeV);
          double astroeBacksideScat(const XrrtMirror* mirror, double reflectAngleInRadians, double energyInKeV);
          double astroeNvfluct();
          void astroeStprnd(double& rx, double& y);
	  double gauRandom(void);
	  double expRandom(void);
	  double lorRandom(void);
          double astroeFfluct(double rz);
          double astroeBacksideNvfluct(double IncidentAngleInRadians, double energyInKeV);
          void astroeBackStprnd(double &rx, double &y);
          double astroeBfluct(double rx, double IncidentAngleInRadians, double energyInkeV);
          double integralFunction(double x,double rx);
          double coreProfile(double theta, double center);
          double trapzd(double a, double b, double rx, int n);
 
          // ASTRO-E2 xrrtback component functions (modified by R.Iizuka : date 2005/01/20)
	  void setAstroeBxrrtback(double IncidentAngleInRadians, double energyInKeV);
	  double astroeBxrrtback(double rx);
        
	  // ASTRO-E2 Pre-Collimator Normal Vector Fluctuation and Scatter component functions
          // (modified by H. Mori : date 2003/01/14)
	  double astroePcolNvfluct(double IncidentAngleInRadians);
          double astroePcolScat(const XrrtCollimator* collimator, double reflectAngleInRadians, double energyInKeV);

          //
          // Place holders for specialized Astro-E scattering when it is ready
          //
          void astroeFrontScatter(const XrrtMirror* mirror, XrrtPhoton& photon);
          void astroeBackScatter(const XrrtMirror* mirror, XrrtPhoton& photon);

          // Add the case of ASTRO-E2 Pre-Collimator
          // Place holders for specialized ASTRO-E2 scattering when it is ready
          // (added by H. Mori : date 2003/01/14)
          //
          void astroePcolScatter(const XrrtCollimator* collimator, XrrtPhoton& photon);

          // ascaParamsSet variable:
          //     TRUE indicates that the ASCA scattering control params have
          //          already been set once.
          //     FALSE indicates that the ASCA scattering control params have
          //          not yet been set.
          bool ascaParamsSet;
          int ascaConsct;
          double ascaSigma1;
          double ascaSigma2;
          double ascaAlin;
          double ascaS3;
          double ascaC3;
          double ascaArd;
          double ascaBrd;
          double ascaCrd;
          double ascaXmaxrd;
          double ascaStpx1;
          double ascaStpx2;
          double ascaStpy0;
          double ascaStpy1;
          double ascaStpy2;
          double ascaStps0;
          double ascaStps1;
          double ascaStps2;
          double ascaStpc;

          // astroeParamsSet variable:
          bool astroeParamsSet;
          int astroeNvfsw;
          double astroeGaussSigma;
          double astroeNgau;
          double astroeExpSigma;
          double astroeNexp;
          double astroeLorentzGamma;
          double astroeNlor;
          int astroeScatsw;
	  double astroeWnorm;
	  double astroeWpower;
	  double astroeGnorm;

          double astroeGaussSigma2InvHalf;	// 0.5*GaussSigma*GaussSigma
          double astroeExpSigmaInv;		// 1/ExpSigma
          double astroeLorentzGammaInv;		// 1/LorentzGamma
	  double astroeBRgau;
	  double astroeBRgauPlusBRexp;

          double astroeXmaxrd;
          double astroeStpx1;
          double astroeStpx2;
          double astroeStpy0;
          double astroeStpy1;
          double astroeStpy2;
          double astroeStps0;
          double astroeStps1;
          double astroeStps2;
          double astroeStpc;

          // ASTRO-E2 backside parameter set variable
          //double astroeBackNgau;
          //double astroeBackNlor;
          //double astroeBackSigma;
          //double astroeBacklorSigma;
          double backNorm;
	  double backPitch;		// pitch in radian
	  int numBackProf;
          double *astroeBackStpx;	// array of numBackProf
	  double *astroeBackStpy;	// array of numBackProf
	  double lastIncidentAngle, lastEnergy;

	  int numBackE;
	  struct AstroeBackE *backE, *lastBackE;

          // ASTRO-E2 Pre-Collimator parameter set variable
          //bool astroePreCollimatorParamsSet;
	  double astroePcolGcA;
	  double astroePcolGcB;
	  double astroePcolGwA;
	  double astroePcolGwB;
          //double astroePreCollimatorXmaxrd;
          //double astroePreCollimatorStpx1;
          //double astroePreCollimatorStpx2;
          //double astroePreCollimatorStpy0;
          //double astroePreCollimatorStpy1;
          //double astroePreCollimatorStpy2;
          //double astroePreCollimatorStps0;
          //double astroePreCollimatorStps1;
          //double astroePreCollimatorStps2;
          //double astroePreCollimatorStpc;

}; 

XrrtScatter& theScatterInfo();

#endif
