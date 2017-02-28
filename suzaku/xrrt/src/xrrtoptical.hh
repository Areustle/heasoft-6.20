// xrrtoptical.hh
//
// Class definition for Optical constants
// Richard L Fink GSFC/631
// 1997/06/19
// 1997/09/19 Upgraded documentation. R. Fink

#ifndef XRRTOPTICAL_HH
#define XRRTOPTICAL_HH

//
// System interfaces used
//
#include <exception>
#include <complex>
// Modified by H. Mori (2005/09/14)
// #include <std/dcomplex.h>

// 
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrtmolecule.hh"
#include "xrrtatomicdata.hh"
#include "xrrtatomscatfactor.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// XrrtOpticalConstants computes index of refraction n, optical Delta,
// and optical Beta using Atomic Scatter Factors.
// See: "Soft X-Ray Optics" by Eberhard Spiller  ISBN 0-8194-1654-1
//      Page 13.
//
class  XrrtOpticalConstants
{
    public:
          // Constructor
          XrrtOpticalConstants(); 
          // Copy Constructor
          XrrtOpticalConstants( const XrrtOpticalConstants& opticalConstants ); 

          //
          // Uses Atomic Scatter Factors to make computation.
          void computeOpticalConstants(const double& energyInKeV,
                                       const XrrtMolecule& compound,
                                       const double& cgsDensity,
                                             double& opticalDelta,
                                             double& opticalBeta,
                                       // Modified by H. Mori (2005/09/14)
				       //    double_complex& refractiveIndexN);
                                             complex<double>& refractiveIndexN);
          // 
          // Computes a single layer reflection probability for the given
          // parameters. This is for a reflection between an incident
          // medium of VACUMN and an infinite thickness layer with the
          // given molecular formula.
          double computeSingleLayerReflect(const XrrtMolecule& molecule,
                                           const double& cgsDensity,
                                           const double& energyInKev,
                                           const double& roughness,
                                           const double& grazingAngleInRadians);

    private:
           // There is no data since the class is a shell.
};

#endif
