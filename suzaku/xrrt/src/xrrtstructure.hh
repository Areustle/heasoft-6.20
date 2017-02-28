// xrrtstructure.hh
//
// Class definition for X-ray telescope
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/23 Upgrade documention. R. Fink

// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 2.2  2000/11/21 08:54:30  mori
// Include ID and LOG macro

#ifndef XRRTSTRUCT_HH
#define XRRTSTRUCT_HH

//
// System interfaces used
//
// Modified by H. Mori (2005/09/14)
// #include <stl.h>
#include <numeric>
#include <exception>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrtmirror.hh"
#include "xrrtobstruction.hh"
#include "xrrtcollimator.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// LOCAL DEFINES
//
enum XrrtStructureErrorCode
        {
        XrrtStructureErrors,
        badPointerInObstructionList,  // Internal error in list of obstructions
        XrrtStructureErrorsEnd
        };

// Add new LayerType called COLLIMATOR for Astro-E2 pre-collimator
// (modified by Hideyuki MORI)
enum LayerType  {MIRROR, OBSTRUCTION, COLLIMATOR};

//END LOCAL DEFINES

//
// XrrtStructure stores the code that determines how telescopes are ordered
// from the information about their parts. For speed of access, that
// structure info is devolved to the parts themselves after it is determined
// rather than storing it in XrrtStructure.
//
class  XrrtStructure
{
    // theStructure() is a friend since it needs private access to the
    // constructor to create the static XrrtStructure.
    friend XrrtStructure& theStructure();

    public:
          //
          // XrrtStructure is responsible for creating mirrors and obstructions
          // as needed since it stores their pointers. NO ONE ever deletes them
          // in the current implementation.
          //
          // Add creating collimators as needed by XrrtStructure for pre-collimator
          // This stores their pointers for later use
          // (modified by Hideyuki MORI)
          XrrtMirror* createMirror();
          XrrtObstruction* createObstruction();
          XrrtCollimator* createCollimator();

          //
          // Once the mirrors and obstructions are loaded, this function
          // determines the telescope structure.
          //
          void organizeStructure();

          //
          // Accessors to obtain information about the telescope structure
          //
          // Return number of distinct layers in the tlescope
          Count getNumberOfLayers();
          //
          // Return the distance of a given layer from the nominal focal plane
          // z=0.0
          FPDistanceInMM getLayerDistance(const Count layer);
          //
          // Return the type (MIRROR/OBSTRUCTION) of a layer
          LayerType getLayerType(const Count layer);
          //
          // Access/set the outer maximum radius for the telescope
          void setOuterHousingRadius(const RadiusInMM& parameter);
          RadiusInMM getOuterHousingRadius() const;
          //
          // Access/set the inner minimum radius for the telescope
          void setInnerHousingRadius(const RadiusInMM& parameter);
          RadiusInMM getInnerHousingRadius() const;
          //
          // Access/set the nominal focal length of the telescope
          double getFocalLengthMM() const;
          void   setFocalLengthMM(const double& nominalFocalLengthMM);

          //
          // Get the next obstruction in a layer; lastObstruction set
          // to zero for the 1st obstruction; zero returned after the
          // last obstruction.
          XrrtObstruction* getObstructionByLayer(const Count layer,
                               const TelescopeQuadrant telescopeQuadrant);

          //
          // Obtain the outer mirror relative to the point specified
          //
          XrrtMirror* getOuterMirror(const Count& layer, 
                                     const RadiusInMM& radius, 
                                     const AngleInRadians& angle);

          // Add the case of pre-collimator (added by Hideyuki MORI)
          // Obtain the outer collimator relative to the point specified
          //
          XrrtCollimator* getOuterCollimator(const Count& layer, 
					 const ColRadiusInMM& radius, 
					 const ColAngleInRadians& angle);

          //
          // Return which quadrant a point is in for the telescope
          TelescopeQuadrant getTelescopeQuadrant(double& x, double& y);
          // 
          // Overloaded function for Astro-E2 telescopes
          // (added by Hideyuki MORI : date 2006/01/28)
          TelescopeQuadrant getTelescopeQuadrant(AngleInRadians& rotationAngle);

          //
          // Convert error codes to error messages
          //
          // Add more error code for XrrtCollimator
          // (modified by Hideyuki MORI)
          string errorMessage(XrrtStructureErrorCode errorCode);
          string errorMessage(XrrtMirrorErrorCode errorCode);
          string errorMessage(XrrtObstructionErrorCode errorCode);
          string errorMessage(XrrtCollimatorErrorCode errorCode);

    private:
          // Constructor
          XrrtStructure(); 
          // Copy Constructor
          XrrtStructure( const XrrtStructure& structure ); 
          // Create the relationship for inner/outer mirrors
          void createInnerOuterMirrorStructure();

          // Create the relationship for inner/outer collimators
          // (modeified by Hideyuki MORI)
          void createInnerOuterCollimatorStructure();

          // The total number of derived layers in the telescope
	  Count numberOfLayers;

          // The inner minimum allowed telescope radius
          RadiusInMM innerHousingRadius;

          // The outer maximum allowed telescope radius
          RadiusInMM outerHousingRadius;

          // Nominal telescope focal length
          FPDistanceInMM focalLengthInMM;

          // The list of pointers to all the mirrors
          vector<XrrtMirror*>    mirrorList;
          vector<double>         mirrorOuterRadiusList;

          // The list of pointers to all the obstructions
          vector<XrrtObstruction*> shadowList;
          vector< vector<XrrtObstruction*> > shadowByQuadrantBylayer;
  
          // The list of pointers to all the collimators
          // (modified by Hideyuki MORI)
          vector<XrrtCollimator*> collimatorList;
          vector<double>          collimatorOuterRadiusList;

          // The list of the starting distance (from the focal plane)
          //     of each layer.
          vector<FPDistanceInMM> layerDistance;

          // The list of the Layer Types (Mirror, Obstruction) of each layer
          // Add one more layer type (Collimator) of the layer
          // (modified by Hideyuki MORI)
          vector<LayerType>      layerType;

          // The location of the 1st [Mirror|Obstruction] in the layer;
          //     this acts as an index to either mirrorList or ObstructionList.
          // Add the location of the 1st Collimator in the layer but no modification
          // (comment by Hideyuki MORI)
          vector<Count>          layerStart;
          // The location of the last item in the layer
          // Add the location of the last item in the layer but no modification
          // (comment by HIDEYUKI MORI)
          vector<Count>          layerEnd;

}; 

XrrtStructure& theStructure();

inline TelescopeQuadrant 
XrrtStructure::getTelescopeQuadrant(double& x, double& y)
{
    if (x >= 0.0e0 && y >= 0.0e0)
       {
       return firstQuadrant;
       }
    else if (x <= 0.0e0 && y >= 0.0e0)
       {
       return secondQuadrant;
       }
    else if (x <= 0.0e0 && y <= 0.0e0)
       {
       return thirdQuadrant;
       }
    else if (x >= 0.0e0 && y <= 0.0e0)
       {
       return fourthQuadrant;
       }
    return noQuadrant;
}

// 
// Since the parameters in Astro-E2 telescope definition file rotate 45 degree
// around the telescope axis (see fstart, fend parameters), we should decide 
// the quadrant in which a photon exists, using its position angle
// This member function is overloaded by the function with the same name, 
// which has two double-type arguments (see above)
// (added by Hideyuki MORI : 2006/01/28)
//
inline TelescopeQuadrant 
XrrtStructure::getTelescopeQuadrant(AngleInRadians& rotationAngle)
{
const double TWOPI = 3.14159265358979323846e0*2.0e0;
const double DEG2RAD = 0.017453292519943295769e0;

    if(rotationAngle < 0.0e0){
      rotationAngle = rotationAngle + TWOPI;
    }
    double rotationAngleInDegree = rotationAngle / DEG2RAD;

    if (rotationAngleInDegree >=45.0e0 && rotationAngleInDegree < 135.0e0)
       {
       return firstQuadrant;
       }
    else if (rotationAngleInDegree >=135.0e0 && rotationAngleInDegree < 225.0e0)
       {
       // (Bug fix by Hideyuki MORI : date 2006/03/09)
       return fourthQuadrant;
       }
    else if (rotationAngleInDegree >=225.0e0 && rotationAngleInDegree < 315.0e0)
       {
       return thirdQuadrant;
       }
    else if ((rotationAngleInDegree >= 315.0e0 && rotationAngleInDegree < 360.0e0) ||
             (rotationAngleInDegree >= 0.0e0 && rotationAngleInDegree < 45.0e0))
       {
       // (Bug fix by Hideyuki MORI : date 2006/03/09)
       return secondQuadrant;
       }
    return noQuadrant;
}
       
inline XrrtObstruction*
XrrtStructure::getObstructionByLayer(const Count layer,
                                     const TelescopeQuadrant quadrant)
{
//
// Return the first obstruction in the layer, quadrant.
//
    return shadowByQuadrantBylayer[layer][quadrant];
}
#endif
