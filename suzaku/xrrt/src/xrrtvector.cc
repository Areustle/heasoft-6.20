// xrrtvector.cc
//
// Member functions for XrrtVector class
//
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/25 Upgrade documentation. R. Fink
//
// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 1.1  2000/10/19 11:10:38  mori
// Initial revision
//
// 2006/06/28 Y.ISHISAKI	version 6.3.10
//    check if both arguments are 0.0 before atan()

#include "xrrtvector.hh"


XrrtVector::XrrtVector():
    radialDirection(0),
    phiAngle(0),
    zUnitDirection(0),
    xUnitDirection(0),
    yUnitDirection(0),
    positionRadius(0),
    positionAngle(0),
    xLocationInMM(0),
    yLocationInMM(0),
    fpDistance(0)
{
// A simple constructor
}

XrrtVector::XrrtVector(const XrrtVector& vector):
    radialDirection(0),
    phiAngle(0),
    zUnitDirection(0),
    xUnitDirection(0),
    yUnitDirection(0),
    positionRadius(0),
    positionAngle(0),
    xLocationInMM(0),
    yLocationInMM(0),
    fpDistance(0)
{
//
// A simple copy constructor
//
    radialDirection = vector.radialDirection;
    phiAngle        = vector.phiAngle;
    zUnitDirection  = vector.zUnitDirection;
    xUnitDirection  = vector.xUnitDirection;
    yUnitDirection  = vector.yUnitDirection;
    positionRadius  = vector.positionRadius;
    positionAngle   = vector.positionAngle;
    xLocationInMM   = vector.xLocationInMM;
    yLocationInMM   = vector.yLocationInMM;
    fpDistance      = vector.fpDistance;
}

string
XrrtVector::errorMessage(XrrtVectorErrorCode errorCode)
{
//
// Convert error codes to error messages.
//
string errorMessage;

    switch (errorCode)
        {
        case zeroZChgNotAllowed:
            errorMessage = 
            "A photon with no change in z-direction tried to move";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                           "XrrtVector::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}

void 
XrrtVector::projectVector(const FPDistanceInMM& newFPDistance)
{
//
// Move a vector from where it is to where it will be when it has a fixed Z
// coord.
//
const double TWOPI = 3.14159265358979323846e0*2.0e0;
    //
    // use geometry to compute where the vector will be if it follows
    // its current path

    //
    // dimensionless time for unit z to traverse current Z to new Z?
    //
    if (zUnitDirection == (UnitVectorMag) 0)
       {
       //
       // From early one we forbid a photon to travel around the mirror system
       // with a zero z direction vector.
       //
       throw zeroZChgNotAllowed;
       }

    double time = (newFPDistance - fpDistance) / zUnitDirection;

    //
    // Move in the x,y,z system because it does not have degenerate points
    //
    xLocationInMM = xLocationInMM + xUnitDirection*time;
    yLocationInMM = yLocationInMM + yUnitDirection*time;
    fpDistance    = fpDistance    + zUnitDirection*time;

    /*
    // OKADA 20061108
    // modify fpDistance (ま、どっちでもいいや)
    FPDistanceInMM arereFPDistance;
    arereFPDistance    = newFPDistance;
    fprintf(stdout,"subNewByOldFPDistance = %lf\n",arereFPDistance-fpDistance);
    */


    //
    // Drag along rho,phi,z
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

void 
XrrtVector::setRadius( const  RadiusInMM& parameter)
{
     positionRadius = parameter;
     xLocationInMM = positionRadius*cos(positionAngle);
     yLocationInMM = positionRadius*sin(positionAngle);
}



AngleInRadians
XrrtVector::getRotationAngle() const
{
     return (positionAngle);
}

void 
XrrtVector::setFPDistance(const FPDistanceInMM& parameter)
{
     fpDistance = parameter;
}

FPDistanceInMM
XrrtVector::getFPDistance() const
{
     return (fpDistance);
}


double 
XrrtVector::angleBetween(const XrrtVector& vector)
{
    return acos(xUnitDirection*vector.xUnitDirection +
                yUnitDirection*vector.yUnitDirection +
                zUnitDirection*vector.zUnitDirection);
}
