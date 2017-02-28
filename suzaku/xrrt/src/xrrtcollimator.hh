// xrrtcollimator.hh
//
// Class definition for ASTRO-E-II pre-Collimator
// Use xrrtmirror.hh as prototype code 
// Hideyuki Mori   ISAS/XRT team
// 2000/11/09

#ifndef XRRTCOLLIMATOR_HH
#define XRRTCOLLIMATOR_HH

//
// System interfaces used
//
#include <exception>
#include <math.h>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrttable.hh"
#include "xrrtvector.hh"

// Global defines needed by all users of XrrtMirror
enum CollimatorSurfaceTypeCodes
        {
        PLANE_COLLIMATOR
        };
enum XrrtCollimatorErrorCode
        {
        XrrtCollimatorErrors,
        noSuchCollimator,             //"Invalid collimator number requested"
        XrrtCollimatorErrorsEnd
        };
// End Globals

//
// XrrtCollimator stores information about individual collimator. It also stores
// relationships to other collimators but it does not derive these relationships
// itself. XrrtStructure does that. The relationships are stored here for 
// implementation speed.
//
class  XrrtCollimator
{
    friend class XrrtStructure;
    public:
          // Constructor
          XrrtCollimator(); 
          // Copy Constructor
          XrrtCollimator( const XrrtCollimator& collimator ); 

          //
          // Accessors to access Collimator labels from the FITS file
          //
          int  getFitsColLayer() const;
          void setFitsColLayer(const int& layer);

          int  getFitsColAssembly() const;
          void setFitsColAssembly(const int& assembly);

          int  getFitsColNumber() const;
          void setFitsColNumber(const int& collimatorNumber);

          int  getFitsColFragmentNumber() const;
          void setFitsColFragmentNumber(const int& collimatorFragmentNumber);

          void  setColLayer (const Count parameter);
	  Count getColLayer() const;

          void setColSurfaceFunction (const ColSurfaceFunction parameter);
	  ColSurfaceFunction getColSurfaceFunction() const;

          void setColScatterMode (const ColSurfaceScatterMode parameter);
	  ColSurfaceScatterMode getColScatterMode() const;

          void setColStartAngle (const ColAngleInRadians parameter);
	  ColAngleInRadians getColStartAngle() const;

          void setColStartAngleCross (const bool parameter);
	  bool canPhotonCrossColStartAngle() const;

          void setColEndAngle (const ColAngleInRadians parameter);
	  ColAngleInRadians getColEndAngle() const;

          void setColEndAngleCross (const bool parameter);
	  bool canPhotonCrossColEndAngle() const;

          void setColTopInnerRadius (const ColRadiusInMM parameter);
	  ColRadiusInMM getColTopInnerRadius() const;

          void setColTopOuterRadius (const ColRadiusInMM parameter);
	  ColRadiusInMM getColTopOuterRadius() const;

          void setColBottomInnerRadius (const ColRadiusInMM parameter);
	  ColRadiusInMM getColBottomInnerRadius() const;

          void setColBottomOuterRadius (const ColRadiusInMM parameter);
	  ColRadiusInMM getColBottomOuterRadius() const;

          void setColTopDistance (const ColFPDistanceInMM parameter);
	  ColFPDistanceInMM getColTopDistance() const;

          void setColBottomDistance (const ColFPDistanceInMM parameter);
	  ColFPDistanceInMM getColBottomDistance() const;

	  // Combine member functions of approaching frontside and 
          // backside blade reflection tables
	  // (modified by H. Mori : date 2003/01/14)
          void setPreCollimatorReflectTable(XrrtTable* parameter);
          XrrtTable* getPreCollimatorReflectTable() const;

          //
          // Functions to return needed trig values for collimators
          //
          double getTanOfFrontCollimatorAngle() const;
          double getCosOfFrontCollimatorAngle() const;
          double getTanOfBackCollimatorAngle() const;
          double getCosOfBackCollimatorAngle() const;

          //
          // XrrtCollimator operators
          //
          bool operator< (const XrrtCollimator& rhs) const;
          bool operator> (const XrrtCollimator& rhs) const;


          //
          // Accessor functions for collimator neighbor relationships
          //
          XrrtCollimator* getColStartAngleNeighbor() const;
          void setColStartAngleNeighbor(XrrtCollimator* collimator);
          XrrtCollimator* getColEndAngleNeighbor() const;
          void setColEndAngleNeighbor(XrrtCollimator* collimator);

          //
          // Accessor functions for collimator relative radius relationships
          //
          XrrtCollimator* getInnerCollimator() const;
          void        setInnerCollimator(const XrrtCollimator* innerCollimatorPtr);
          XrrtCollimator* getOuterCollimator() const;
          void        setOuterCollimator(const XrrtCollimator* outerCollimatorPtr);


          //
          // Function to compute derived values for collimator after their
          // factual data has been set up.
          //
          void computePlaneCollimatorData();

          // 
          // Accessor function for Collimator derived data
          //
          XrrtVector getPlaneCollimatorFrontVector(const double& phiAngle) const;
          XrrtVector getPlaneCollimatorBackVector(const double& phiAngle) const;
          XrrtVector getPlaneCollimatorFrontNormal(const double& phiAngle) const;
          XrrtVector getPlaneCollimatorBackNormal(const double& phiAngle) const;
          double getColInnerRadiusAtZ(double& z) const;
          double getColOuterRadiusAtZ(double& z)const;

          //
          // Convert error code to error message
          //
          string errorMessage(XrrtCollimatorErrorCode errorCode);

          // This actually is a collimator comparison function
          // that does not yet work with the STL sort function.
          bool sortCollimatorDescend(const XrrtCollimator* x, const XrrtCollimator* y);

         // Set Pre-Collimator blade thickness freely 
         // (modified by H. Mori : date 2002/08/16)
          void setCollimatorThickness(const double parameter);

          // Set Pre-Collimator blade miss alignment random generator
          // mode (modified by H. Mori : date 2002/08/16)
          void setCollimatorMissAlignmentMode(const string parameter);

          // Set Pre-Collimator blade miss alignment value 
          // (modified by H. Mori : date 2002/08/16)
          void setCollimatorMissAlignment(const double parameter);

    private:
          //
          // Factual data about collimators that comes from the collimator
          // description file
          //
          // User collimator classification/ID data
          int                fitsColLayer;
          int                fitsColAssembly;
          int                fitsColNumber;
          int                fitsColFragmentNumber;
          //
          // Type of collimator profile (plane, parabola, etc.)
          ColSurfaceFunction    colSurfaceFunction;
          //
          // The method used to compute photon scatter off the collimator
          ColSurfaceScatterMode colScatterMode;
          //
          // The beginning of the collimator as a section of rotation
	  ColAngleInRadians     colStartAngle;
          //
          // Are photons allowed to cross this edge onto the next collimator?
          bool               mayPhotonCrossColStartAngle;
          //
          // The end of the collimator as a section of rotation
	  ColAngleInRadians     colEndAngle;
          // Are photons allowed to cross this edge onto the next collimator?
          bool               mayPhotonCrossColEndAngle;
          //
          // The radius in millimeters of the inside and outside of the collimator
          // at the top (most distant from the focal plane).
          ColRadiusInMM         colTopInnerRadius;
          ColRadiusInMM         colTopOuterRadius;
          //
          // The radius in millimeters of the inside and outside of the collimator
          // at the bottom (closest to the focal plane).
          ColRadiusInMM         colBottomInnerRadius;
	  ColRadiusInMM         colBottomOuterRadius;
          //
          // Distace from the focal plane (top/bottom).
          ColFPDistanceInMM     colTopDistance;
          ColFPDistanceInMM     colBottomDistance;
          //
          // A pointer to a pre-stored reflection table for the Pre-Collimator
          // (front == smallest radius)
          // (modified by H. Mori : date 2003/01/14)
	  XrrtTable*         precollimatorReflectTable;
          //
          // Derived Collimator data = quantities derived from the information
          // above. These are for PLANE collimators only.
          //
          // Angle the surface makes with the z-axis
          ColAngleInRadians     planeCollimatorFrontAngle;
          ColAngleInRadians     planeCollimatorBackAngle;
          //
          // The distance along the collimator surface
          ColDistanceInMM       planeCollimatorFrontLength;
          ColDistanceInMM       planeCollimatorBackLength;
          //
          // A partial vector pointing down along the collimator. The full vector
          // can only be calculated when a point on the collimator has been chosen.
          // This is because the collimators are LINES rotated thru space at
          // fixed angles to the z-axis.
          XrrtVector         planeCollimatorFrontVector;
          XrrtVector         planeCollimatorBackVector;
          //
          // Similiar vectors to the front vectors except these are 
          // perpendicular to the surface at a point. By necessity, they point
          // towards the z-axis. If the collimator surface is _rough_, they would
          // not point towards the z-axis in general.
          XrrtVector         planeCollimatorFrontNormal;
          XrrtVector         planeCollimatorBackNormal;
          //
          // Metadata about the relationship between the collimator fragment
          // and other collimator fragments/structures
          //
          // Collimators are organized into layers relative to the focal plane
          Count              colLayer;
          //
          // Next collimator in rotation
          XrrtCollimator*        colStartAngleNeighbor;
          XrrtCollimator*        colEndAngleNeighbor;
          //
          // Next collimator inwards or outwards in radius
          XrrtCollimator*        innerCollimator;
          XrrtCollimator*        outerCollimator;

          // definition of Pre-Collimator blade thickness 
          // (modified by H. Mori : date 2002/08/16)
          double collimatorThickness;

          // Pre-Collimator blade miss placement 
          // (modified by H. Mori : date 2002/08/16)
          string missAlignmentModeCollimator;
          double missAlignCollimator;

}; 

inline ColRadiusInMM
XrrtCollimator::getColTopOuterRadius() const
{
     return (colTopOuterRadius);
}

inline XrrtCollimator*
XrrtCollimator::getOuterCollimator() const
{
    return outerCollimator;
}
#endif
