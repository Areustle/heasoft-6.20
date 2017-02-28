// xrrtphoton.hh
//
// Class definition for traced photon
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/19 Upgraded documentation. R. Fink
// 1998/10/10 Made modification to PhotonStatusCodes to track which layer
//            a single reflection occured from. Also modified 
//            XrrtPhoton::classifyPhotonByReflections() to support the change.

// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 2.1  2000/11/29 04:28:59  mori
// Add the case of existing pre-collimator for ASTRO-E-II
//
// Revision 1.2  2000/10/23 08:05:57  mori
// Change definition of PhotonReflectionSurface which is enum type
// Primary outer 1, Secondary outer 2, Primary inner 3, Secondary inner 4
//
// Revision 1.1  2000/10/19 06:31:01  mori
// Initial revision

#ifndef XRRTPHOTON_HH
#define XRRTPHOTON_HH

// 
// System interfaces used
//
#include <exception>
// Modified by H. Mori (2005/09/14)
// #include <iostream.h>
#include <iostream>

// 
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrtvector.hh"
#include "xrrtobstruction.hh"
#include "xrrtmirror.hh"
#include "xrrtstructure.hh"
// Add for Quadrant-level ray-tracing
// Added by Hideyuki MORI : date 2006/01/27)
#include "xrrtquadrant.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// Globals needed by anyone using XrrtPhoton
//
enum XrrtPhotonErrorCode
        {
        XrrtPhotonErrors,
        invalidPhotonClass,   // A variable of the enum type 
                              // PhotonReflectionClass occured that program code
                              // was not designed to handle. This indicates
                              // that the PhotonReflectionClass was updated
                              // and not all code that does SWITCH or IF
                              // tests.
        XrrtPhotonErrorsEnd
        };
//
// These are the possible states that a photon can get into while being traced.
// The cases of the interaction with a pre-collimator are included
// (added by Hideyuki MORI)
enum PhotonStatusCodes
        {
        PHOTON_CONTINUES,
        PHOTON_HITS_OBSTRUCTION,
        PHOTON_HITS_OUTER_HOUSING,
        PHOTON_HITS_INNER_HOUSING,
        PHOTON_HITS_TOP_OF_MIRROR,
        PHOTON_ABSORBED_ON_OUTER_MIRROR,
        PHOTON_ABSORBED_ON_INNER_MIRROR,
        PHOTON_REVERSES_Z_DIRECTION,
        PHOTON_CAN_NOT_LEAVE_MIRROR,
        PHOTON_HIT_FOCAL_PLANE,
        // Interaction with a pre-collimator for Astro-E2
        // (added by Hideyuki MORI)
        PHOTON_HITS_TOP_OF_COLLIMATOR,
	PHOTON_CAN_NOT_LEAVE_COLLIMATOR,
        PHOTON_ABSORBED_ON_OUTER_COLLIMATOR,
        PHOTON_ABSORBED_ON_INNER_COLLIMATOR,
        ERROR
        };

//
// IF a photon was absorbed in some fashion, these are the allowed places.
enum AbsorptionMirrorFace
        {
        NO_ABSORPTION,
        MIRROR_TOP,
        OUTER_MIRROR_FACE,
        INNER_MIRROR_FACE
        };

//
// IF a photon was absorbed in some fashion, these are the allowed places.
// In the case of a pre-collimator (added by Hideyuki MORI)
enum AbsorptionCollimatorFace
        {
        NO_COLLIMATOR_ABSORPTION,
        COLLIMATOR_TOP,
        OUTER_COLLIMATOR_FACE,
        INNER_COLLIMATOR_FACE
        };

//
// Photons after they reach the focal plane are classified by the number
// of times they impact on mirrors. LAYER here refers to Mirror Layers in the 
// telescope.
enum PhotonReflectionClass
        {
        NO_REFLECTION_CLASS,
        ONE_LAYER_ONLY,
        ONE_PER_LAYER,
        NO_REFLECTIONS,
        ABNORMAL_PATH,
        FIRST_LAYER_ONLY,
        SECOND_LAYER_ONLY
        };

// Photons are first classified in detail by the reflection on 
// the inner or outer mirrors. (modified by Hideyuki MORI)
enum ReflectionSurfaceSide
{
  REFLECTION_SIDE_ERROR,
  INNER,
  OUTER
};

// 
// Next photons are classified in detail by the reflection on 
// the primary or secondary mirrors. (modified by Hideyuki MORI)
enum PhotonReflectionSurface
{
  REFLECTION_PATH_ERROR,
  PRIMARY_OUTER,
  SECONDARY_OUTER,
  PRIMARY_INNER,
  SECONDARY_INNER,
  COLLIMATOR_OUTER,
  COLLIMATOR_INNER
};

// End Globals

//
// XrrtPhoton contains all the stored info on a photon as it is traced.
//
class  XrrtPhoton
{
          // thePhoton() is a friend so that it can access the private
          // constructor to create the static photon (1 only).
          friend XrrtPhoton& thePhoton();

    public:
          // 
          // Set the photon to a known, untraced status.
          //
          void clearPhoton();

          // Accessor functions for photon data.
          //
          // Access for the current condition of the photon. Used
          // by different parts of the ray tracing to communicate.
          void setPhotonStatus(const PhotonStatusCodes code);
          PhotonStatusCodes photonStatus() const;

          // setEnergy sets the photon energy.
          void setEnergy (const EnergyInKev& parameter);

          // getEnergy returns the photon energy.
	  double getEnergy() const;

          // There are 3 states that a photon has:
          //     1) Initial State
          //        Where the photon was before it was traced.
          //     2) Current State
          //        Where the photon is now.
          //     3) Virtual State
          //        Where the photon could be if an event happened.
          // Different accessors exist for these states.

          //
          // Initial State Accessors
          //
          // Return the photon initial location
          XrrtVector getInitialVector() const;
          //
          // Set the photon Initial location to the Current location.
          void setInitialToCurrent();

          //
          // Current State Accessors
          //
          // Set the Current location to the Virtual location.
          void setCurrentToVirtual();
          // Return the photon current location
          XrrtVector getCurrentVector() const;
          // Distance of the current photon from the focal plane
          void setPhotonDistance( const FPDistanceInMM& parameter);
          FPDistanceInMM getPhotonDistance() const;
          //
          // Distance of the photon from the z-axis
          void  setRadius(const RadiusInMM& parameter);
          RadiusInMM getRadius() const;
          //
          // Rotation of the photon in the x,y plane (rho,phi phi coord)
          void setRotationAngle (const AngleInRadians& parameter);
          AngleInRadians  getRotationAngle() const;
          //
          // Accessors for the photon direction
          // in Rho,Phi,Z and X,Y,Z
          void setPhotonDirection(const UnitVectorMag& radial,
                                  const UnitVectorMag& theta,
                                  const UnitVectorMag& z); 
          //
          void getPhotonDirection(UnitVectorMag& radial,
                                  UnitVectorMag& theta,
                                  UnitVectorMag& z); 
          //
          void setPhotonDirectionXYZ(const UnitVectorMag& xDirection,
                                     const UnitVectorMag& yDirection,
                                     const UnitVectorMag& zDirection); 
          //
          void getPhotonDirectionXYZ(UnitVectorMag& xDirection,
                                     UnitVectorMag& yDirection,
                                     UnitVectorMag& zDirection);
          //
          // Accessor for the Current location of the photon in the x,y plane
          void getXY(VertexInMM* x, VertexInMM* y) const;
          //
          // Modify the current location to where the photon will be at
          // the given z-axis absolute distance from the focal plane (z=0).
          void projectPhoton(const FPDistanceInMM& distance);

          //
          // Virtual State Accessors
          //
          // Set the Virtual location to the Current Location.
          void setVirtualToCurrent();
          //
          // Return the Virtual location
          XrrtVector getVirtualVector() const;
          //
          // Return the Virtual location radius
          RadiusInMM getVirtualRadius() const;
          //
          // Return the Virtual location rotation angle Phi
          AngleInRadians  getVirtualRotationAngle() const;
          //
          // Accessors for the Virtual State direction vectors
          // in Rho,Phi and X,Y,Z systems.
          void setVirtualDirection(const UnitVectorMag& radial,
                                   const UnitVectorMag& theta,
                                   const UnitVectorMag& z);
          //
          void getVirtualDirection(UnitVectorMag& radial,
                                   UnitVectorMag& theta,
                                   UnitVectorMag& z);
          //
          void setVirtualDirectionXYZ(const UnitVectorMag& xDirection,
                                      const UnitVectorMag& yDirection,
                                      const UnitVectorMag& zDirection);
          //
          void getVirtualDirectionXYZ(UnitVectorMag& xDirection,
                                      UnitVectorMag& yDirection,
                                      UnitVectorMag& zDirection);
          // 
          // Return the Virtual state location in the X,Y,Z system
          void getVirtualXY(VertexInMM* x, VertexInMM* y);
          //
          // Project the virtual photon state to where it would be at
          // the given z-axis value.
          void projectVirtualPhoton(const FPDistanceInMM& distance);

          //
          // Functions to save related state for the photon
          //
          // Save the obstruction that the photon impacted on.
          void setImpactObstruction(const XrrtObstruction* obstruction);
          //
          // Save the Mirror that the photon was absorbed on
          void setAbsorptionMirror(const XrrtMirror* mirror);
          //
          // Save the mirror face that the photon was absorbed on
          void setAbsorptionMirrorFace(const AbsorptionMirrorFace face);
          //
          // In the case of an absorption on a pre-collimator
          // (added by Hideyuki MORI)
          // Save the pre-collimator that the photon was absorbed on
          void setAbsorptionCollimator(const XrrtCollimator* collimator);
          //
          // Save the pre-collimator face that the photon was absorbed on
          void setAbsorptionCollimatorFace(const AbsorptionCollimatorFace face);

          //
          // Functions that compute and manage the classification of the 
          // patterns of reflection a photon undergoes
          //
          // Add a reflection in the given layer
          void addReflection(const int& layer);
          //
          // Add a reflection surface in the ReflectionPath array. 
          // But this routine only gives a mirror selection.
          // Actual setting is done by next function
          // (modified by Hideyuki MORI : date 2003/01/28)
          void addReflectionSurfaceWithPreCollimator(const int& layer, ReflectionSurfaceSide SurfaceSide);
          void addReflectionSurfaceWithoutPreCollimator(const int& layer, ReflectionSurfaceSide SurfaceSide);
          //
          // Set a reflection surface in the ReflectionPath array
          // (modified by Hideyuki MORI)
          void setReflectionSurface(const PhotonReflectionSurface code);
          //
          // Convert reflection path array to long integer for convenience
          // (modified by Hideyuki MORI)
          long getReflectionPath() const;
          //
          // Return the total number of reflections the photon saw
          int getTotalReflections() const;
          //
          // Compute and update the Photon Reflection Class
          void classifyPhotonByReflections();
          //
          // Return the current photon reflection class
          PhotonReflectionClass getPhotonReflectionClass() const;
          //
          // Clear (zero) the photon reflection data
          void zeroReflectionsByLayer(const int& layerCount);

          // 
          // Convert the direction vector of a current photon in the
          // telescope (base) coordinate to that in the quadrant coordinate.
          // (added by Hideyuki MORI : date 2005/12/15)
          // Add the argument of xrrtquadrant class to get the information 
          // about the quadrant's offset
          // (modified by Hideyuki MORI : date 2006/01/27)
          //
          void setTelescopeToQuadrantCoordinate(const XrrtQuadrant& quadrant);
          // And vise vasa
          void setQuadrantToTelescopeCoordinate(const XrrtQuadrant& quadrant);

          //
          // Convert error codes to error messages
          //
          string errorMessage(XrrtPhotonErrorCode errorCode);
          string errorMessage(XrrtVectorErrorCode errorCode);

    private:
          // Constructor
          XrrtPhoton(); 
          // Copy Constructor
          XrrtPhoton( const XrrtPhoton& photon ); 

          //
          // Member data
          //
          // Keep track of how many times the photon is initialized
          int       photonCounter;
          //
          // What state the photon is in
          PhotonStatusCodes  currentPhotonStatus;
          //
          // The current photon energy
          EnergyInKev        energy;
          //
          // Where the photon was when it was initialized
          XrrtVector         initialLocation;
          //
          // Where the photon currently is
          XrrtVector         location;
          //
          // Where the phtotn could end up; used to test events without
          // disturbing the current photon location.
          XrrtVector         virtualLocation;
          //
          // Which obstruction the photon impacted on (if any)
          XrrtObstruction*   impactObstruction;
          //
          // Which mirror the photon was absorbed on (if it was)
          XrrtMirror*        absorptionMirror;
          // 
          // Which face of the above mirror
          AbsorptionMirrorFace absorptionFace;
          //
          // In the case of an absorption on the Astro-E2 pre-collimator 
          // (added by Hideyuki MORI)
          // Which collimator the photon was absorbed on (if it was)
          XrrtCollimator*        absorptionCollimator;
          // 
          // Which face of the above collimator
          AbsorptionCollimatorFace absorptionColFace;
          //
          // Array of the reflection count in each layer of the telescope
          vector<int> reflectionsByLayer;
          // Array of the reflection surface in the telescope for a detail 
          // investigation of the photon pass (added by Hideyuki MORI)
          vector<int> ReflectionPath;
          //
          // The derived class of the reflection pattern based on the contents
          // of reflectionsByLayer.
          PhotonReflectionClass classFromReflections;
}; 

XrrtPhoton& thePhoton();

inline void
XrrtPhoton::setVirtualToCurrent()
{
     virtualLocation = location;
}

inline void
XrrtPhoton::setCurrentToVirtual()
{
     location = virtualLocation;
}

inline AngleInRadians
XrrtPhoton::getRotationAngle() const
{
      return location.getRotationAngle();
}

inline RadiusInMM
XrrtPhoton::getRadius() const
{
      return location.getRadius();
}

inline PhotonStatusCodes
XrrtPhoton::photonStatus() const
{
    return currentPhotonStatus;
}

inline void
XrrtPhoton::getXY(VertexInMM* x, VertexInMM* y) const
{
      location.getXY(x,y);
}

inline RadiusInMM
XrrtPhoton::getVirtualRadius() const
{
      return virtualLocation.getRadius();
}
#endif
