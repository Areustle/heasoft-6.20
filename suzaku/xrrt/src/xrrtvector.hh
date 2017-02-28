// xrrtvector.hh
//
// Class definition for vectors in the ray tracing library 
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/23 Upgrade documentation. R. Fink

#ifndef XRRTVECTOR_HH
#define XRRTVECTOR_HH

//
// System interfaces used
//
#include <exception>
// Modified by H. Mori (2005/09/14)
// #include <math.h>
#include <cmath>
#include <string>
// Add include file for sprintf(), standard I/O library
// (modified by H. Mori : date 2002/08/29)
// Compile for GCC 3.3.2 
// Modified by H. Mori (2005/09/14)
// #include <stdio.h>
#include <cstdio>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// Local enums
//
enum XrrtVectorErrorCode
        {
        XrrtVectorErrors,
        zeroZChgNotAllowed,  // A vector can not move from one place to another
                             // if the z direction vector is zero; this derives
                             // from an early design feature to move photons
                             // to specific Z-axis values. It could be removed
                             // by converting all moves to by TIME.
        XrrtVectorErrorsEnd
        };

//
// Xrrtvector was designed to handle photon location/movement but also
// is used to handle elements of plane mirror slope vectors and surface normal
// vectors.
//
class XrrtVector
{

    public:
          // Constructor
          XrrtVector();
          // Copy Constructor
          XrrtVector( const XrrtVector& vector);

          // setRadius sets the current vector location parameter for the 
          //      radius relative to the telescope central axis.
          void setRadius (const RadiusInMM& parameter);

          // setRadius returns the current vector radius location.
	  RadiusInMM getRadius() const;

          // setRotationAngle set the current vector location parameter for the
          //      rotation angle relative to the defined telescope standard.
          void setRotationAngle (const AngleInRadians& parameter);

          // getRotationAngle returns the current vector rotation angle.
	  AngleInRadians getRotationAngle() const;

          // setFPDistance sets the current vector distance from the focal
          //      plane.
          void setFPDistance (const FPDistanceInMM& parameter);

          // getFPDistance returns the current vector distance from the focal
          //      plane.
	  FPDistanceInMM getFPDistance() const;

          // setDirection modifies the unit vector to a new direction.
          void setVectorDirection(const UnitVectorMag& radial,
                                  const AngleInRadians& phi,
                                  const UnitVectorMag& z);
          void setVectorDirectionXYZ(const UnitVectorMag& xDirection,
                                     const UnitVectorMag& yDirection,
                                     const UnitVectorMag& zDirection);

          // getDirection returns the current unit vector.
          void getVectorDirection(UnitVectorMag& radial,
                                  AngleInRadians& phi,
                                  UnitVectorMag& z) const;
          void getVectorDirectionXYZ(UnitVectorMag& xDirection,
                                     UnitVectorMag& yDirection,
                                     UnitVectorMag& zDirection) const;

          // projectVector projects the current vector from its present position
          // to its new poition at the given distance from the focal plane.
          void projectVector(const FPDistanceInMM& parameter);

          // getXY returns the current location in the x,y plane
          void getXY(VertexInMM* x, VertexInMM* y) const;

          // setXY sets the current location in the x,y plane
          // (added by Hideyuki MORI : date 2006/01/28)
          void setXY(const VertexInMM& x, const VertexInMM& y);

          //
          // angleBetween returns the radian angle between two vectors
          // (radianAngle = xrrtvector.angleBetween(xrrtvector))
          double angleBetween(const XrrtVector& vector);

          // zeroVector resets the vector location and direction to zero
          void zeroVector();

          //
          // Convert error codes to error messages
          //
          string errorMessage(XrrtVectorErrorCode errorCode);

    private:
          //
          // Location info is stored and computed for both Rho,Phi,Z
          // and X,Y,Z since both are needed from time to time.
          // The same is true for direction vectors.
          //
          // Direction vectors
          UnitVectorMag radialDirection;
          AngleInRadians phiAngle;
          UnitVectorMag zUnitDirection;
          UnitVectorMag xUnitDirection;
          UnitVectorMag yUnitDirection;
          //
          // Location
          RadiusInMM    positionRadius;
          AngleInRadians positionAngle;
          double xLocationInMM;
          double yLocationInMM;
          FPDistanceInMM     fpDistance;

};

inline RadiusInMM
XrrtVector::getRadius() const
{
     return (positionRadius);
}

inline void
XrrtVector::getVectorDirection(UnitVectorMag& radial,
                               AngleInRadians& phi,
                               UnitVectorMag& z) const
{
    radial = radialDirection;
    phi    = phiAngle;
    z      = zUnitDirection;
}

inline void
XrrtVector::setVectorDirection(const UnitVectorMag& radial,
                               const AngleInRadians& phi,
                               const UnitVectorMag& z)
{
    radialDirection = radial;
    phiAngle = phi;
    zUnitDirection = z;
    xUnitDirection = radial*cos(phi);
    yUnitDirection = radial*sin(phi);
}

inline void
XrrtVector::setRotationAngle(const AngleInRadians& parameter)
{
const double TWOPI = 2.0e0*3.14159265358979323846e0;
     if (parameter > TWOPI)
        {
        positionAngle = parameter - TWOPI;
        }
     else if (parameter < 0.0e0)
        {
        positionAngle = parameter + TWOPI;
        }
     else
        {
        positionAngle = parameter;
        }
     xLocationInMM = positionRadius*cos(positionAngle);
     yLocationInMM = positionRadius*sin(positionAngle);
}

inline void
XrrtVector::getXY(VertexInMM* x, VertexInMM* y) const
{
    *x = xLocationInMM;
    *y = yLocationInMM;
}

// (added by Hideyuki MORI : date 2006/01/28)
inline void 
XrrtVector::setXY(const VertexInMM& x, const VertexInMM& y)
{
const double TWOPI = 3.14159265358979323846e0*2.0e0;

    xLocationInMM = x;
    yLocationInMM = y;

    //
    // Update the radius and rotation angle based on the new (x, y)
    // 
    positionRadius = sqrt(xLocationInMM*xLocationInMM +
                          yLocationInMM*yLocationInMM);
    positionAngle  =
        ( 0.0 == positionRadius ) ? 0.0 : atan2(yLocationInMM, xLocationInMM);
    if (positionAngle < 0.0e0)
       {
       positionAngle = positionAngle + TWOPI;
       }
}

inline void
XrrtVector::getVectorDirectionXYZ(UnitVectorMag& xDirection,
                                  UnitVectorMag& yDirection,
                                  UnitVectorMag& zDirection) const
{
    xDirection = xUnitDirection;
    yDirection = yUnitDirection;
    zDirection = zUnitDirection;
}

inline void
XrrtVector::zeroVector()
{
    radialDirection = 0;
    phiAngle = 0;
    xUnitDirection = 0;
    yUnitDirection = 0;
    zUnitDirection = 0;
    positionRadius = 0;
    positionAngle = 0;
    xLocationInMM = 0;
    yLocationInMM = 0;
    fpDistance = 0;
}

inline void
XrrtVector::setVectorDirectionXYZ(const UnitVectorMag& xDirection,
                                  const UnitVectorMag& yDirection,
                                  const UnitVectorMag& zDirection)
{
const double TWOPI = 3.14159265358979323846e0*2.0e0;
    xUnitDirection = xDirection;
    yUnitDirection = yDirection;
    zUnitDirection = zDirection;
    radialDirection = sqrt(xUnitDirection*xUnitDirection +
                           yUnitDirection*yUnitDirection);
    phiAngle =
       ( 0.0 == radialDirection ) ? 0.0 : atan2(yUnitDirection,xUnitDirection);
    if (phiAngle < 0.0e0)
       {
       phiAngle = phiAngle + TWOPI;
       }
}
#endif
