// xrrtmirror.hh
//
// Class definition for X-ray mirrors
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/19 Upgraded documentation. R. Fink

#ifndef XRRTMIRROR_HH
#define XRRTMIRROR_HH

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
#include "xrrttable.hh"
#include "xrrtvector.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

// Global defines needed by all users of XrrtMirror
enum SurfaceTypeCodes
        {
        PLANE_MIRROR
        };
enum XrrtMirrorErrorCode
        {
        XrrtMirrorErrors,
        noSuchMirror,             //"Invalid mirror number requested"
        XrrtMirrorErrorsEnd
        };
// End Globals

//
// XrrtMirror stores information about individual mirrors. It also stores
// relationships to other mirrors but it does not derive these relationships
// itself. XrrtStructure does that. The relationships are stored here for 
// implementation speed.
//
class  XrrtMirror
{
    friend class XrrtStructure;
    public:
          // Constructor
          XrrtMirror(); 
          // Copy Constructor
          XrrtMirror( const XrrtMirror& mirror ); 

          //
          // Accessors to access Mirror labels from the FITS file
          //
          int  getFitsLayer() const;
          void setFitsLayer(const int& layer);

          int  getFitsAssembly() const;
          void setFitsAssembly(const int& assembly);

          int  getFitsMirrorNumber() const;
          void setFitsMirrorNumber(const int& mirrorNumber);

          int  getFitsFragmentNumber() const;
          void setFitsFragmentNumber(const int& fragmentNumber);

          void  setLayer (const Count parameter);
	  Count getLayer() const;

          void setSurfaceFunction (const SurfaceFunction parameter);
	  SurfaceFunction getSurfaceFunction() const;

          void setScatterMode (const SurfaceScatterMode parameter);
	  SurfaceScatterMode getScatterMode() const;

          void setStartAngle (const AngleInRadians parameter);
	  AngleInRadians getStartAngle() const;

          void setStartAngleCross (const bool parameter);
	  bool canPhotonCrossStartAngle() const;

          void setEndAngle (const AngleInRadians parameter);
	  AngleInRadians getEndAngle() const;

          void setEndAngleCross (const bool parameter);
	  bool canPhotonCrossEndAngle() const;

          void setTopInnerRadius (const RadiusInMM parameter);
	  RadiusInMM getTopInnerRadius() const;

          void setTopOuterRadius (const RadiusInMM parameter);
	  RadiusInMM getTopOuterRadius() const;

          void setBottomInnerRadius (const RadiusInMM parameter);
	  RadiusInMM getBottomInnerRadius() const;

          void setBottomOuterRadius (const RadiusInMM parameter);
	  RadiusInMM getBottomOuterRadius() const;

          void setTopDistance (const FPDistanceInMM parameter);
	  FPDistanceInMM getTopDistance() const;

          void setBottomDistance (const FPDistanceInMM parameter);
	  FPDistanceInMM getBottomDistance() const;

          void setFrontReflectTable(XrrtTable* parameter);
          XrrtTable* getFrontReflectTable() const;

          void setBackReflectTable(XrrtTable* parameter);
          XrrtTable* getBackReflectTable() const;

          //
          // Functions to return needed trig values for mirrors
          //
          double getTanOfFrontMirrorAngle() const;
          double getCosOfFrontMirrorAngle() const;
          double getTanOfBackMirrorAngle() const;
          double getCosOfBackMirrorAngle() const;

          //
          // XrrtMirror operators
          //
          bool operator< (const XrrtMirror& rhs) const;
          bool operator> (const XrrtMirror& rhs) const;


          //
          // Accessor functions for mirror neighbor relationships
          //
          XrrtMirror* getStartAngleNeighbor() const;
          void setStartAngleNeighbor(XrrtMirror* mirror);
          XrrtMirror* getEndAngleNeighbor() const;
          void setEndAngleNeighbor(XrrtMirror* mirror);

          //
          // Accessor functions for mirror relative radius relationships
          //
          XrrtMirror* getInnerMirror() const;
          void        setInnerMirror(const XrrtMirror* innerMirrorPtr);
          XrrtMirror* getOuterMirror() const;
          void        setOuterMirror(const XrrtMirror* outerMirrorPtr);


          //
          // Function to compute derived values for mirror after their
          // factual data has been set up.
          //
          void computePlaneMirrorData();

          // 
          // Accessor function for Mirror derived data
          //
          XrrtVector getPlaneMirrorFrontVector(const double& phiAngle) const;
          XrrtVector getPlaneMirrorBackVector(const double& phiAngle) const;
          XrrtVector getPlaneMirrorFrontNormal(const double& phiAngle) const;
          XrrtVector getPlaneMirrorBackNormal(const double& phiAngle) const;
          double getInnerRadiusAtZ(double& z) const;
          double getOuterRadiusAtZ(double& z)const;

          //
          // Convert error code to error message
          //
          string errorMessage(XrrtMirrorErrorCode errorCode);

          // This actually is a mirror comparison function
          // that does not yet work with the STL sort function.
          bool sortMirrorDescend(const XrrtMirror* x, const XrrtMirror* y);

          // Set mirror thickness freely (modified by H. Mori)
          void setMirrorThickness(const double parameter);

          // Set foil mirror miss alignment random generator
          // mode (modified by H. Mori)
          void setMirrorMissAlignmentMode(const string parameter);

          // Set foil mirror miss alignment value (modified by H. Mori)
          void setMirrorMissAlignment(const double parameter);

    private:
          //
          // Factual data about mirrors that comes from the telescope
          // description file
          //
          // User mirror classification/ID data
          int                fitsLayer;
          int                fitsAssembly;
          int                fitsMirrorNumber;
          int                fitsFragmentNumber;
          //
          // Type of mirror profile (plane, parabola, etc.)
          SurfaceFunction    surfaceFunction;
          //
          // The method used to compute photon scatter off the mirror
          SurfaceScatterMode scatterMode;
          //
          // The beginning of the mirror as a section of rotation
	  AngleInRadians     startAngle;
          //
          // Are photons allowed to cross this edge onto the next mirror?
          bool               mayPhotonCrossStartAngle;
          //
          // The end of the mirror as a section of rotation
	  AngleInRadians     endAngle;
          // Are photons allowed to cross this edge onto the next mirror?
          bool               mayPhotonCrossEndAngle;
          //
          // The radius in millimeters of the inside and outside of the mirror
          // at the top (most distant from the focal plane).
          RadiusInMM         topInnerRadius;
          RadiusInMM         topOuterRadius;
          //
          // The radius in millimeters of the inside and outside of the mirror
          // at the bottom (closest to the focal plane).
          RadiusInMM         bottomInnerRadius;
	  RadiusInMM         bottomOuterRadius;
          //
          // Distace from the focal plane (top/bottom).
          FPDistanceInMM     topDistance;
          FPDistanceInMM     bottomDistance;
          //
          // A pointer to a pre-stored reflection table for the front surface
          // (front == smallest radius)
	  XrrtTable*         frontReflectTable;
          //
          // A pointer to a pre-stored reflection table for the back surface
          // (back == largest radius)
	  XrrtTable*         backReflectTable;
          //
          // Derived Mirror data = quantities derived from the information
          // above. These are for PLANE mirrors only.
          //
          // Angle the surface makes with the z-axis
          AngleInRadians     planeMirrorFrontAngle;
          AngleInRadians     planeMirrorBackAngle;
          //
          // The distance along the mirror surface
          DistanceInMM       planeMirrorFrontLength;
          DistanceInMM       planeMirrorBackLength;
          //
          // A partial vector pointing down along the mirror. The full vector
          // can only be calculated when a point on the mirror has been chosen.
          // This is because the mirrors are LINES rotated thru space at
          // fixed angles to the z-axis.
          XrrtVector         planeMirrorFrontVector;
          XrrtVector         planeMirrorBackVector;
          //
          // Similiar vectors to the front vectors except these are 
          // perpendicular to the surface at a point. By necessity, they point
          // towards the z-axis. If the mirror surface is _rough_, they would
          // not point towards the z-axis in general.
          XrrtVector         planeMirrorFrontNormal;
          XrrtVector         planeMirrorBackNormal;
          //
          // Metadata about the relationship between the mirror fragment
          // and other mirror fragments/structures
          //
          // Mirrors are organized into layers relative to the focal plane
          Count              layer;
          //
          // Next mirror in rotation
          XrrtMirror*        startAngleNeighbor;
          XrrtMirror*        endAngleNeighbor;
          //
          // Next mirror inwards or outwards in radius
          XrrtMirror*        innerMirror;
          XrrtMirror*        outerMirror;

          // definition of mirror thickness (modified by H. Mori)
          double thickness;
  
          // Foil mirror miss alignment influences on EA, PSF, and so on
          // (modified by H. Mori)
          string missAlignmentMode;
          double missAlign;

}; 

inline RadiusInMM
XrrtMirror::getTopOuterRadius() const
{
     return (topOuterRadius);
}

inline XrrtMirror*
XrrtMirror::getOuterMirror() const
{
    return outerMirror;
}
#endif
