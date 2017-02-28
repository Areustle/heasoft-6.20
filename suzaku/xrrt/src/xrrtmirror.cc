// xrrtmirror.cc
//
// Member functions for Mirror class
//
// Richard L Fink GSFC/631
// 1997/04/24
// 1997/09/24 Upgrade documentation. R. Fink
// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 1.1  2000/10/19 11:14:16  mori
// Initial revision

#include <cstring>
#include "xrrtmirror.hh"

XrrtMirror::XrrtMirror():
     fitsLayer(0),
     fitsAssembly(0),
     fitsMirrorNumber(0),
     fitsFragmentNumber(0),
     surfaceFunction(0),
     scatterMode(0),
     startAngle(0),
     mayPhotonCrossStartAngle(false),
     endAngle(0),
     mayPhotonCrossEndAngle(false),
     topInnerRadius(0),
     topOuterRadius(0),
     bottomInnerRadius(0),
     bottomOuterRadius(0),
     topDistance(0),
     bottomDistance(0),
     frontReflectTable(0),
     backReflectTable(0), 
     planeMirrorFrontAngle(0),
     planeMirrorBackAngle(0),
     planeMirrorFrontLength(0),
     planeMirrorBackLength(0),
     planeMirrorFrontVector(),
     planeMirrorBackVector(),
     planeMirrorFrontNormal(),
     planeMirrorBackNormal(),
     layer(0),
     startAngleNeighbor(0),
     endAngleNeighbor(0),
     innerMirror(0),
     outerMirror(0)
{
// A simple constructor
}

string
XrrtMirror::errorMessage(XrrtMirrorErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case noSuchMirror:
            errorMessage = 
            "Request for a non-existent mirror";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                           "XrrtMirror::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}



int 
XrrtMirror::getFitsLayer() const
{
    return fitsLayer;
}

void 
XrrtMirror::setFitsLayer(const int& layer)
{
    fitsLayer = layer;
}

int 
XrrtMirror::getFitsAssembly() const
{
    return fitsAssembly;
}

void 
XrrtMirror::setFitsAssembly(const int& assembly)
{
    fitsAssembly = assembly;
}

int 
XrrtMirror::getFitsMirrorNumber() const
{
    return fitsMirrorNumber;
}

void 
XrrtMirror::setFitsMirrorNumber(const int& mirrorNumber)
{
    fitsMirrorNumber = mirrorNumber;
}

int 
XrrtMirror::getFitsFragmentNumber() const
{
    return fitsFragmentNumber;
}

void 
XrrtMirror::setFitsFragmentNumber(const int& fragmentNumber)
{
    fitsFragmentNumber = fragmentNumber;
}

Count 
XrrtMirror::getLayer() const
{
    return layer;
}

void 
XrrtMirror::setSurfaceFunction(const SurfaceFunction parameter)
{
    surfaceFunction = parameter;
}

SurfaceFunction
XrrtMirror::getSurfaceFunction() const
{
    return (surfaceFunction);
}

void 
XrrtMirror::setScatterMode(const SurfaceScatterMode parameter)
{
     scatterMode = parameter;
}

SurfaceFunction
XrrtMirror::getScatterMode() const
{
     return (scatterMode);
}

void 
XrrtMirror::setStartAngle(const AngleInRadians parameter)
{
     startAngle = parameter;
}

AngleInRadians
XrrtMirror::getStartAngle() const
{
     return (startAngle);
}

void 
XrrtMirror::setStartAngleCross (const bool parameter)
{
    mayPhotonCrossStartAngle = parameter;
}

bool 
XrrtMirror::canPhotonCrossStartAngle() const
{
    return(mayPhotonCrossStartAngle);
}

void 
XrrtMirror::setEndAngle(const AngleInRadians parameter)
{
     endAngle = parameter;
}

AngleInRadians
XrrtMirror::getEndAngle() const
{
     return (endAngle);
}

void 
XrrtMirror::setEndAngleCross (const bool parameter)
{
    mayPhotonCrossEndAngle = parameter;
}

bool 
XrrtMirror::canPhotonCrossEndAngle() const
{
    return(mayPhotonCrossEndAngle);
}

void 
XrrtMirror::setTopInnerRadius(const RadiusInMM parameter)
{
     double difference;
     if(strncmp(missAlignmentMode.c_str(), "uniform", 7) == 0){
       difference = ((xrrtrandom() - 5.0e-1) / 5.0e-1) * missAlign;
     }
     else if(strncmp(missAlignmentMode.c_str(), "gauss", 5) == 0){
       difference = xrrtgaussrandom() * missAlign;
     }
     else{
       difference = 0.0e0;
     }
     topInnerRadius = parameter + difference;
}

RadiusInMM
XrrtMirror::getTopInnerRadius() const
{
     return (topInnerRadius);
}

void 
XrrtMirror::setTopOuterRadius(const RadiusInMM parameter)
{
     double correspondingInnerRadius;
     correspondingInnerRadius = getTopInnerRadius();
     topOuterRadius = correspondingInnerRadius + thickness;
}


void 
XrrtMirror::setBottomInnerRadius(const RadiusInMM parameter)
{
     double difference;
     if(strncmp(missAlignmentMode.c_str(), "uniform", 7) == 0){
       difference = ((xrrtrandom() - 5.0e-1) / 5.0e-1) * missAlign;
     }
     else if(strncmp(missAlignmentMode.c_str(), "gauss", 5) == 0){
       difference = xrrtgaussrandom() * missAlign;
     }
     else{
       difference = 0.0e0;
     }
     bottomInnerRadius = parameter + difference;
}

RadiusInMM
XrrtMirror::getBottomInnerRadius() const
{
     return (bottomInnerRadius);
}

void 
XrrtMirror::setBottomOuterRadius(const RadiusInMM parameter)
{
     double correspondingInnerRadius;
     correspondingInnerRadius = getBottomInnerRadius();
     bottomOuterRadius = correspondingInnerRadius + thickness;
}

RadiusInMM
XrrtMirror::getBottomOuterRadius() const
{
     return (bottomOuterRadius);
}

void 
XrrtMirror::setTopDistance(const FPDistanceInMM parameter)
{
     topDistance = parameter;
}

FPDistanceInMM
XrrtMirror::getTopDistance() const
{
     return (topDistance);
}

void 
XrrtMirror::setBottomDistance(const FPDistanceInMM parameter)
{
     bottomDistance = parameter;
}

FPDistanceInMM
XrrtMirror::getBottomDistance() const
{
     return (bottomDistance);
}

void 
XrrtMirror::setFrontReflectTable(XrrtTable* parameter)
{
     frontReflectTable = parameter;
}

XrrtTable*
XrrtMirror::getFrontReflectTable() const
{
     return (frontReflectTable);
}

void 
XrrtMirror::setBackReflectTable(XrrtTable* parameter)
{
     backReflectTable = parameter;
}

XrrtTable*
XrrtMirror::getBackReflectTable() const
{
     return (backReflectTable);
}

bool
XrrtMirror::sortMirrorDescend(const XrrtMirror* x, const XrrtMirror* y)
{
    return (x->topDistance > y->topDistance);
}

void 
XrrtMirror::setLayer (const Count parameter)
{
     layer = parameter;
}

 bool 
XrrtMirror::operator< (const XrrtMirror& rhs) const
{
return (topDistance < rhs.topDistance)&&(topOuterRadius < rhs.topOuterRadius);
}

 bool 
XrrtMirror::operator> (const XrrtMirror& rhs) const
{
return (topDistance > rhs.topDistance)&&(topOuterRadius > rhs.topOuterRadius);
}

double
XrrtMirror::getTanOfFrontMirrorAngle() const
{
   return (topInnerRadius - bottomInnerRadius)/(topDistance - bottomDistance);
}

double
XrrtMirror::getTanOfBackMirrorAngle() const
{
   return (topOuterRadius - bottomOuterRadius)/(topDistance - bottomDistance);
}

double
XrrtMirror::getCosOfFrontMirrorAngle() const
{
   return (topDistance-bottomDistance) / planeMirrorFrontLength;
}

double
XrrtMirror::getCosOfBackMirrorAngle() const
{
   return (topDistance-bottomDistance) / planeMirrorBackLength;
}

XrrtMirror* 
XrrtMirror::getStartAngleNeighbor() const
{
    return startAngleNeighbor;
}

void 
XrrtMirror::setStartAngleNeighbor(XrrtMirror* mirror)
{
    startAngleNeighbor = mirror;
}

XrrtMirror* 
XrrtMirror::getEndAngleNeighbor() const
{
    return endAngleNeighbor;
}

void 
XrrtMirror::setEndAngleNeighbor(XrrtMirror* mirror)
{
    endAngleNeighbor = mirror;
}

void
XrrtMirror::computePlaneMirrorData()
{
    // Compute the length of the front face of the mirror
    planeMirrorFrontLength = sqrt((topInnerRadius-bottomInnerRadius) *
                                  (topInnerRadius-bottomInnerRadius) +
                                  (topDistance-bottomDistance) *
                                  (topDistance-bottomDistance));
    // Compute the length of the back face of the mirror
    planeMirrorBackLength = sqrt((topOuterRadius-bottomOuterRadius) *
                                 (topOuterRadius-bottomOuterRadius) +
                                 (topDistance-bottomDistance) *
                                 (topDistance-bottomDistance));
    // Compute the angle between the z-axis and the front face
    planeMirrorFrontAngle =  acos((topDistance-bottomDistance) / 
                                   planeMirrorFrontLength);
    // Compute the angle between the z-axis and the back face
    planeMirrorBackAngle =  acos((topDistance-bottomDistance) / 
                                   planeMirrorBackLength);
    // Compute the front face unit vector; the rotation angle is uncertain
    // since it depends on where you want it
    planeMirrorFrontVector.setRadius(topInnerRadius);
    planeMirrorFrontVector.setRotationAngle(0.0e0);
    planeMirrorFrontVector.setFPDistance(topDistance);
    // Compute the back face unit vector; the rotation angle is uncertain
    // since it depends on where you want it
    planeMirrorBackVector.setRadius(topOuterRadius);
    planeMirrorBackVector.setRotationAngle(0.0e0);
    planeMirrorBackVector.setFPDistance(topDistance);
    // The convention is that the radial vector is positive and the z vector
    // is negative. You thus need to know the phi angle in order to compute
    // the full vector; this is done by the accessor function
    double radial = (topInnerRadius - bottomInnerRadius) /
                    planeMirrorFrontLength;
    double phiAngle = 0.0e0;
    double z = (bottomDistance - topDistance)/planeMirrorFrontLength;
    planeMirrorFrontVector.setVectorDirection(radial, phiAngle, z);
    // Back face
    radial = (topOuterRadius - bottomOuterRadius)/planeMirrorBackLength;
    phiAngle = 0.0e0;
    z = (bottomDistance - topDistance)/planeMirrorBackLength;
    planeMirrorBackVector.setVectorDirection(radial, phiAngle, z);
    // The normal vector for the front points INWARD and the normal to
    // the back points OUTWARD but this must be controlled by the accessor
    // function
    radial = cos(planeMirrorFrontAngle);
    z      = sin(planeMirrorFrontAngle);
    phiAngle  = 0.0e0;;
    planeMirrorFrontNormal.setVectorDirection(radial, phiAngle, z);
    radial = cos(planeMirrorBackAngle);
    z      = -sin(planeMirrorBackAngle);
    phiAngle  = 0.0e0;;
    planeMirrorBackNormal.setVectorDirection(radial, phiAngle, z);
}

XrrtVector 
XrrtMirror::getPlaneMirrorFrontVector(const AngleInRadians& phiAngle) const
{
const double xrrtPI = 3.14159265358979323846e0;
XrrtVector planeFrontVector;
double radial;
double phiRotation;
double z;

    planeFrontVector = planeMirrorFrontVector;
    planeFrontVector.setRotationAngle(phiAngle);
    planeFrontVector.getVectorDirection(radial, phiRotation, z);
    phiRotation = phiAngle + xrrtPI;
    planeFrontVector.setVectorDirection(radial, phiRotation, z);
    return planeFrontVector;
}

XrrtVector 
XrrtMirror::getPlaneMirrorBackVector(const AngleInRadians& phiAngle) const
{
const double xrrtPI = 3.14159265358979323846e0;
XrrtVector planeBackVector;
double radial;
double phiRotation;
double z;

    planeBackVector = planeMirrorBackVector;
    planeBackVector.setRotationAngle(phiAngle);
    planeBackVector.getVectorDirection(radial, phiRotation, z);
    phiRotation = phiAngle + xrrtPI;
    planeBackVector.setVectorDirection(radial, phiRotation, z);
    return planeBackVector;
}

XrrtVector 
XrrtMirror::getPlaneMirrorFrontNormal(const AngleInRadians& phiAngle) const
{
const double xrrtPI = 3.14159265358979323846e0;
XrrtVector planeFrontNormal;
double radial;
double phiRotation;
double z;

    planeFrontNormal = planeMirrorFrontNormal;
    planeFrontNormal.setRotationAngle(phiAngle);
    planeFrontNormal.getVectorDirection(radial, phiRotation, z);
    phiRotation = phiAngle + xrrtPI;
    planeFrontNormal.setVectorDirection(radial, phiRotation, z);
    return planeFrontNormal;
}

XrrtVector 
XrrtMirror::getPlaneMirrorBackNormal(const AngleInRadians& phiAngle) const
{
// const double xrrtPI = 3.14159265358979323846e0;
XrrtVector planeBackNormal;
double radial;
double phiRotation;
double z;

    planeBackNormal = planeMirrorBackNormal;
    planeBackNormal.setRotationAngle(phiAngle);
    planeBackNormal.getVectorDirection(radial, phiRotation, z);
    phiRotation = phiAngle;
    planeBackNormal.setVectorDirection(radial, phiRotation, z);
    return planeBackNormal;
}

double 
XrrtMirror::getInnerRadiusAtZ(double& z) const
{
     return bottomInnerRadius +
            ((z - bottomDistance)*getTanOfFrontMirrorAngle());
}

double 
XrrtMirror::getOuterRadiusAtZ(double& z)const
{
    return bottomOuterRadius +
           ((z - bottomDistance)*getTanOfBackMirrorAngle());
}

XrrtMirror* 
XrrtMirror::getInnerMirror() const
{
    return innerMirror;
}

void        
XrrtMirror::setInnerMirror(const XrrtMirror* innerMirrorPtr)
{
    innerMirror = (XrrtMirror*) innerMirrorPtr;
}

void        
XrrtMirror::setOuterMirror(const XrrtMirror* outerMirrorPtr)
{
    outerMirror = (XrrtMirror*) outerMirrorPtr;
}

// (modified by H. Mori)
void
XrrtMirror::setMirrorThickness(const double parameter)
{
    thickness = parameter;
}

void
XrrtMirror::setMirrorMissAlignmentMode(const string parameter)
{
    missAlignmentMode = parameter;
}

void
XrrtMirror::setMirrorMissAlignment(const double parameter)
{
    missAlign = parameter;
}
