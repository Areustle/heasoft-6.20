// xrrtflatmodel.hh
//
// Class definition for photon source (Flat/Diffuse model)
// Richard L Fink GSFC/631
// 1997/06/17
// 1997/09/19 Upgraded documentation. R. Fink

#ifndef XRRTFLATMODEL_HH
#define XRRTFLATMODEL_HH

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
// XrrtFlatModel creates sky photons positions of the flat kind.
//
class  XrrtFlatModel
{
    public:
          // Constructor
          XrrtFlatModel(); 
          // Copy Constructor
          XrrtFlatModel( const XrrtFlatModel& flatModel ); 

          // Set the Flat model radius
          void setFlatRadius(double radiusInArcmin);

          // Return Flat model angles
          void getFlatModelAngles(double& modelOffAxisAngleRadian,
                                  double& modelRotationAngleRadian);

    private:
           // Radius of Flat/diffuse model
           double flatRadiusInArcmin;

          
}; 

#endif
