// xrrtbetamodel.hh
//
// Class definition for photon source (Beta model)
// Richard L Fink GSFC/631
// 1997/06/17
// 1997/09/19 Upgraded documentation. R. Fink

#ifndef XRRTBETAMODEL_HH
#define XRRTBETAMODEL_HH

// 
// System interfaces used
//
#include <exception>
// Modified by H. Mori (2005/09/14)
// #include <math.h>
#include <cmath>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// XrrtBetaModel creates sky locations where a Beta model photon whould appear.
//
class  XrrtBetaModel
{
    public:
          // Constructor
          XrrtBetaModel(); 
          // Copy Constructor
          XrrtBetaModel( const XrrtBetaModel& betaModel ); 

          // Set the Beta model core radius in arcminutes
          void setCoreRadius(double radiusInArcMin);

          // Set the Beta model beta parameter
          void setBeta(double beta);

          // Return beta model angles
          void getBetaModelAngles(double& modelOffAxisAngleRadian, 
                                  double& modelRotationAngleRadian);

    private:
           // Core radius of Beta model
           double coreRadiusArcMin;
           // Beta
           double beta;

          
}; 



#endif
