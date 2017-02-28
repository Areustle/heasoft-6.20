// xrrtcollimator.cc
//
// Member functions for Collimator class
// Use xrrtmirror.cc as prototype code
//
// Hideyuki Mori    ISAS/XRT team
// 2000/11/09

#include <cstring>
#include "xrrtcollimator.hh"

XrrtCollimator::XrrtCollimator():
     fitsColLayer(0),
     fitsColAssembly(0),
     fitsColNumber(0),
     fitsColFragmentNumber(0),
     colSurfaceFunction(0),
     colScatterMode(0),
     colStartAngle(0),
     mayPhotonCrossColStartAngle(false),
     colEndAngle(0),
     mayPhotonCrossColEndAngle(false),
     colTopInnerRadius(0),
     colTopOuterRadius(0),
     colBottomInnerRadius(0),
     colBottomOuterRadius(0),
     colTopDistance(0),
     colBottomDistance(0),
     precollimatorReflectTable(0),
     planeCollimatorFrontAngle(0),
     planeCollimatorBackAngle(0),
     planeCollimatorFrontLength(0),
     planeCollimatorBackLength(0),
     planeCollimatorFrontVector(),
     planeCollimatorBackVector(),
     planeCollimatorFrontNormal(),
     planeCollimatorBackNormal(),
     colLayer(0),
     colStartAngleNeighbor(0),
     colEndAngleNeighbor(0),
     innerCollimator(0),
     outerCollimator(0)
{
// A simple constructor
}

string
XrrtCollimator::errorMessage(XrrtCollimatorErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case noSuchCollimator:
            errorMessage = 
            "Request for a non-existent collimator";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                           "XrrtCollimator::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}



int 
XrrtCollimator::getFitsColLayer() const
{
    return fitsColLayer;
}

void 
XrrtCollimator::setFitsColLayer(const int& layer)
{
    fitsColLayer = layer;
}

int 
XrrtCollimator::getFitsColAssembly() const
{
    return fitsColAssembly;
}

void 
XrrtCollimator::setFitsColAssembly(const int& assembly)
{
    fitsColAssembly = assembly;
}

int 
XrrtCollimator::getFitsColNumber() const
{
    return fitsColNumber;
}

void 
XrrtCollimator::setFitsColNumber(const int& collimatorNumber)
{
    fitsColNumber = collimatorNumber;
}

int 
XrrtCollimator::getFitsColFragmentNumber() const
{
    return fitsColFragmentNumber;
}

void 
XrrtCollimator::setFitsColFragmentNumber(const int& collimatorFragmentNumber)
{
    fitsColFragmentNumber = collimatorFragmentNumber;
}

Count 
XrrtCollimator::getColLayer() const
{
    return colLayer;
}

void 
XrrtCollimator::setColSurfaceFunction(const ColSurfaceFunction parameter)
{
    colSurfaceFunction = parameter;
}

ColSurfaceFunction
XrrtCollimator::getColSurfaceFunction() const
{
    return (colSurfaceFunction);
}

void 
XrrtCollimator::setColScatterMode(const ColSurfaceScatterMode parameter)
{
     colScatterMode = parameter;
}

ColSurfaceFunction
XrrtCollimator::getColScatterMode() const
{
     return (colScatterMode);
}

void 
XrrtCollimator::setColStartAngle(const ColAngleInRadians parameter)
{
     colStartAngle = parameter;
}

ColAngleInRadians
XrrtCollimator::getColStartAngle() const
{
     return (colStartAngle);
}

void 
XrrtCollimator::setColStartAngleCross (const bool parameter)
{
    mayPhotonCrossColStartAngle = parameter;
}

bool 
XrrtCollimator::canPhotonCrossColStartAngle() const
{
    return(mayPhotonCrossColStartAngle);
}

void 
XrrtCollimator::setColEndAngle(const ColAngleInRadians parameter)
{
     colEndAngle = parameter;
}

ColAngleInRadians
XrrtCollimator::getColEndAngle() const
{
     return (colEndAngle);
}

void 
XrrtCollimator::setColEndAngleCross (const bool parameter)
{
    mayPhotonCrossColEndAngle = parameter;
}

bool 
XrrtCollimator::canPhotonCrossColEndAngle() const
{
    return(mayPhotonCrossColEndAngle);
}

// modify the code for Pre-Collimator blade to be located at the
// random position created by uniform or gaussian random generator
// (modified by H. Mori : date 2002/08/16)
void 
XrrtCollimator::setColTopInnerRadius(const ColRadiusInMM parameter)
{
     double difference;
     if(strncmp(missAlignmentModeCollimator.c_str(), "uniform", 7) == 0){
	difference = ((xrrtrandom() - 5.0e-1) / 5.0e-1) * missAlignCollimator;
     }
     else if(strncmp(missAlignmentModeCollimator.c_str(), "gauss", 5) == 0){
	difference = xrrtgaussrandom() * missAlignCollimator;
     }
     else{
        difference = 0.0e0;
     }
     colTopInnerRadius = parameter + difference; 
     // fprintf(stdout, "%lf ", colTopInnerRadius);
}

ColRadiusInMM
XrrtCollimator::getColTopInnerRadius() const
{
     return (colTopInnerRadius);
}

void 
XrrtCollimator::setColTopOuterRadius(const ColRadiusInMM parameter)
{    
     double correspondingCollimatorInnerRadius;
     correspondingCollimatorInnerRadius = getColTopInnerRadius();
     colTopOuterRadius = correspondingCollimatorInnerRadius + collimatorThickness;
     // fprintf(stdout, "%lf ", colTopOuterRadius);
}


void 
XrrtCollimator::setColBottomInnerRadius(const ColRadiusInMM parameter)
{
     colBottomInnerRadius = getColTopInnerRadius();
     // fprintf(stdout, "%lf ", colBottomInnerRadius);	
}

ColRadiusInMM
XrrtCollimator::getColBottomInnerRadius() const
{
     return (colBottomInnerRadius);
}

void 
XrrtCollimator::setColBottomOuterRadius(const ColRadiusInMM parameter)
{
     double correspondingCollimatorInnerRadius;
     correspondingCollimatorInnerRadius = getColTopInnerRadius();
     colBottomOuterRadius = correspondingCollimatorInnerRadius + collimatorThickness;
     // fprintf(stdout, "%lf\n", colBottomOuterRadius);	
}

ColRadiusInMM
XrrtCollimator::getColBottomOuterRadius() const
{
     return (colBottomOuterRadius);
}

void 
XrrtCollimator::setColTopDistance(const ColFPDistanceInMM parameter)
{
     colTopDistance = parameter;
}

ColFPDistanceInMM
XrrtCollimator::getColTopDistance() const
{
     return (colTopDistance);
}

void 
XrrtCollimator::setColBottomDistance(const ColFPDistanceInMM parameter)
{
     colBottomDistance = parameter;
}

ColFPDistanceInMM
XrrtCollimator::getColBottomDistance() const
{
     return (colBottomDistance);
}

void 
XrrtCollimator::setPreCollimatorReflectTable(XrrtTable* parameter)
{
     precollimatorReflectTable = parameter;
}

XrrtTable*
XrrtCollimator::getPreCollimatorReflectTable() const
{
     return (precollimatorReflectTable);
}

bool
XrrtCollimator::sortCollimatorDescend(const XrrtCollimator* x, const XrrtCollimator* y)
{
    return (x->colTopDistance > y->colTopDistance);
}

void 
XrrtCollimator::setColLayer (const Count parameter)
{
     colLayer = parameter;
}

 bool 
XrrtCollimator::operator< (const XrrtCollimator& rhs) const
{
return (colTopDistance < rhs.colTopDistance)&&(colTopOuterRadius < rhs.colTopOuterRadius);
}

 bool 
XrrtCollimator::operator> (const XrrtCollimator& rhs) const
{
return (colTopDistance > rhs.colTopDistance)&&(colTopOuterRadius > rhs.colTopOuterRadius);
}

double
XrrtCollimator::getTanOfFrontCollimatorAngle() const
{
   return (colTopInnerRadius - colBottomInnerRadius)/(colTopDistance - colBottomDistance);
}

double
XrrtCollimator::getTanOfBackCollimatorAngle() const
{
   return (colTopOuterRadius - colBottomOuterRadius)/(colTopDistance - colBottomDistance);
}

double
XrrtCollimator::getCosOfFrontCollimatorAngle() const
{
   return (colTopDistance-colBottomDistance) / planeCollimatorFrontLength;
}

double
XrrtCollimator::getCosOfBackCollimatorAngle() const
{
   return (colTopDistance-colBottomDistance) / planeCollimatorBackLength;
}

XrrtCollimator* 
XrrtCollimator::getColStartAngleNeighbor() const
{
    return colStartAngleNeighbor;
}

void 
XrrtCollimator::setColStartAngleNeighbor(XrrtCollimator* collimator)
{
    colStartAngleNeighbor = collimator;
}

XrrtCollimator* 
XrrtCollimator::getColEndAngleNeighbor() const
{
    return colEndAngleNeighbor;
}

void 
XrrtCollimator::setColEndAngleNeighbor(XrrtCollimator* collimator)
{
    colEndAngleNeighbor = collimator;
}

void
XrrtCollimator::computePlaneCollimatorData()
{
    // Compute the length of the front face of the collimator
    planeCollimatorFrontLength = sqrt((colTopInnerRadius-colBottomInnerRadius) *
				      (colTopInnerRadius-colBottomInnerRadius) +
				      (colTopDistance-colBottomDistance) *
				      (colTopDistance-colBottomDistance));
    // Compute the length of the back face of the collimator
    planeCollimatorBackLength = sqrt((colTopOuterRadius-colBottomOuterRadius) *
				     (colTopOuterRadius-colBottomOuterRadius) +
				     (colTopDistance-colBottomDistance) *
				     (colTopDistance-colBottomDistance));
    // Compute the angle between the z-axis and the front face
    planeCollimatorFrontAngle =  acos((colTopDistance-colBottomDistance) / 
				      planeCollimatorFrontLength);
    // Compute the angle between the z-axis and the back face
    planeCollimatorBackAngle =  acos((colTopDistance-colBottomDistance) / 
				     planeCollimatorBackLength);
    // Compute the front face unit vector; the rotation angle is uncertain
    // since it depends on where you want it
    planeCollimatorFrontVector.setRadius(colTopInnerRadius);
    planeCollimatorFrontVector.setRotationAngle(0.0e0);
    planeCollimatorFrontVector.setFPDistance(colTopDistance);
    // Compute the back face unit vector; the rotation angle is uncertain
    // since it depends on where you want it
    planeCollimatorBackVector.setRadius(colTopOuterRadius);
    planeCollimatorBackVector.setRotationAngle(0.0e0);
    planeCollimatorBackVector.setFPDistance(colTopDistance);
    // The convention is that the radial vector is positive and the z vector
    // is negative. You thus need to know the phi angle in order to compute
    // the full vector; this is done by the accessor function
    double radial = (colTopInnerRadius - colBottomInnerRadius) /
                    planeCollimatorFrontLength;
    double phiAngle = 0.0e0;
    double z = (colBottomDistance - colTopDistance)/planeCollimatorFrontLength;
    planeCollimatorFrontVector.setVectorDirection(radial, phiAngle, z);
    // Back face
    radial = (colTopOuterRadius - colBottomOuterRadius)/planeCollimatorBackLength;
    phiAngle = 0.0e0;
    z = (colBottomDistance - colTopDistance)/planeCollimatorBackLength;
    planeCollimatorBackVector.setVectorDirection(radial, phiAngle, z);
    // The normal vector for the front points INWARD and the normal to
    // the back points OUTWARD but this must be controlled by the accessor
    // function
    radial = cos(planeCollimatorFrontAngle);
    z      = sin(planeCollimatorFrontAngle);
    phiAngle  = 0.0e0;;
    planeCollimatorFrontNormal.setVectorDirection(radial, phiAngle, z);
    radial = cos(planeCollimatorBackAngle);
    z      = -sin(planeCollimatorBackAngle);
    phiAngle  = 0.0e0;;
    planeCollimatorBackNormal.setVectorDirection(radial, phiAngle, z);
}

XrrtVector 
XrrtCollimator::getPlaneCollimatorFrontVector(const ColAngleInRadians& phiAngle) const
{
const double xrrtPI = 3.14159265358979323846e0;
XrrtVector planeFrontVector;
double radial;
double phiRotation;
double z;

    planeFrontVector = planeCollimatorFrontVector;
    planeFrontVector.setRotationAngle(phiAngle);
    planeFrontVector.getVectorDirection(radial, phiRotation, z);
    phiRotation = phiAngle + xrrtPI;
    planeFrontVector.setVectorDirection(radial, phiRotation, z);
    return planeFrontVector;
}

XrrtVector 
XrrtCollimator::getPlaneCollimatorBackVector(const ColAngleInRadians& phiAngle) const
{
const double xrrtPI = 3.14159265358979323846e0;
XrrtVector planeBackVector;
double radial;
double phiRotation;
double z;

    planeBackVector = planeCollimatorBackVector;
    planeBackVector.setRotationAngle(phiAngle);
    planeBackVector.getVectorDirection(radial, phiRotation, z);
    phiRotation = phiAngle + xrrtPI;
    planeBackVector.setVectorDirection(radial, phiRotation, z);
    return planeBackVector;
}

XrrtVector 
XrrtCollimator::getPlaneCollimatorFrontNormal(const ColAngleInRadians& phiAngle) const
{
const double xrrtPI = 3.14159265358979323846e0;
XrrtVector planeFrontNormal;
double radial;
double phiRotation;
double z;

    planeFrontNormal = planeCollimatorFrontNormal;
    planeFrontNormal.setRotationAngle(phiAngle);
    planeFrontNormal.getVectorDirection(radial, phiRotation, z);
    phiRotation = phiAngle + xrrtPI;
    planeFrontNormal.setVectorDirection(radial, phiRotation, z);
    return planeFrontNormal;
}

XrrtVector 
XrrtCollimator::getPlaneCollimatorBackNormal(const ColAngleInRadians& phiAngle) const
{
// const double xrrtPI = 3.14159265358979323846e0;
XrrtVector planeBackNormal;
double radial;
double phiRotation;
double z;

    planeBackNormal = planeCollimatorBackNormal;
    planeBackNormal.setRotationAngle(phiAngle);
    planeBackNormal.getVectorDirection(radial, phiRotation, z);
    phiRotation = phiAngle;
    planeBackNormal.setVectorDirection(radial, phiRotation, z);
    return planeBackNormal;
}

double 
XrrtCollimator::getColInnerRadiusAtZ(double& z) const
{
     return colBottomInnerRadius +
            ((z - colBottomDistance)*getTanOfFrontCollimatorAngle());
}

double 
XrrtCollimator::getColOuterRadiusAtZ(double& z)const
{
    return colBottomOuterRadius +
           ((z - colBottomDistance)*getTanOfBackCollimatorAngle());
}

XrrtCollimator* 
XrrtCollimator::getInnerCollimator() const
{
    return innerCollimator;
}

void        
XrrtCollimator::setInnerCollimator(const XrrtCollimator* innerCollimatorPtr)
{
    innerCollimator = (XrrtCollimator*) innerCollimatorPtr;
}

void        
XrrtCollimator::setOuterCollimator(const XrrtCollimator* outerCollimatorPtr)
{
    outerCollimator = (XrrtCollimator*) outerCollimatorPtr;
}

// add for Pre-Collimator blade missplacement and thickness setting
// (modified by H. Mori : date 2002/08/16)
void
XrrtCollimator::setCollimatorThickness(const double parameter)
{
    collimatorThickness = parameter;
}

void
XrrtCollimator::setCollimatorMissAlignmentMode(const string parameter)
{
    missAlignmentModeCollimator = parameter;
}

void
XrrtCollimator::setCollimatorMissAlignment(const double parameter)
{
    missAlignCollimator = parameter;
}
