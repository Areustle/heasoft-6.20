// xrrtobstruct.hh
//
// Class definition for X-ray mirror obstructions that shadow the mirrors.
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/19 Upgraded documentation. R. Fink
// 1999/02/03 Added support for separating obstructions by telescope quadrant.
//            This was needed to accelerate the code since it was simply too 
//            slow.

#ifndef XRRTOBSTRUCT_HH
#define XRRTOBSTRUCT_HH

//
// System interfaces used
//
#include <exception>
#include <string>
// Modified by H. Mori (2005/09/14)
// #include <stdio.h>
#include <cstdio>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrtpolygon.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori
using namespace std;

//
// Local enums
//
enum XrrtObstructionErrorCode
        {
        XrrtObstructionErrors,
        noSuchVertex,             //"This vertex was not defined"
        XrrtObstructionErrorsEnd
        };

//
// XrrtObstruction srots data on objects that shadow mirrors in the telescope.
// Example: a telescope mask or a quadrant housing.
//
class  XrrtObstruction 
{
    public:
          // Constructor
          XrrtObstruction( ); 
          // Copy Constructor
          XrrtObstruction( const XrrtObstruction& obstruction );  

          // 
          // Accessors for Obstruction contents
          //
          void setFitsLayer(const int& userlayer);
          int  getFitsLayer() const;

          void setFitsPolynum(const int& polygonNumber);
          int  getFitsPolynum() const;

          // setFPDistance sets the distance of the obstruction from the focal
          //       plane.
          void setFPDistance(const FPDistanceInMM parameter);
          // getFPDistance returns the obstruction distance from the focal 
          //       plane.
          FPDistanceInMM getFPDistance() const;

          // setVertex inserts a vertex of a current polygon into the
          //       vertex list.
          void setVertex(const VertexInMM x, const VertexInMM y);

          //
          // Operators
          //
          bool sortObstructionDescend(const XrrtObstruction* const x, 
                                      const XrrtObstruction* const y);
          bool operator< (const XrrtObstruction& rhs) const;
          bool operator> (const XrrtObstruction& rhs) const;

          //
          // Determine whether a point (x,y) is in the obstruction.
          //
          bool xyInObstruction(VertexInMM x, VertexInMM y);

          //
          // Accessors for structure info that XrrtObstruction manages
          // but does not create.
          //
          // These provide a chain from the 1st obstruction in a layer
          // to the last.
          void setNextInLayer(const XrrtObstruction* nextObstruction);
          XrrtObstruction* getNextInLayer() const;

          // setLayer sets the nominal layer that the obstruction lies in
          //       for layer tracing purposes.
          void setLayer(const Count parameter);
          // getLayer returns the obstruction layer.
          Count getLayer() const;

          //
          // Set the Telescope Quadrant the obstruction belongs to
          void setQuadrant();
          TelescopeQuadrant getTelescopeQuadrant();

          //
          // Convert Error codes to Error messages
          //
          string errorMessage(XrrtObstructionErrorCode errorCode);
          string errorMessage(XrrtPolygonErrorCode errorCode);


    private:
           //
           // Data members
           //
           // FITS file obstruction layer ID
           int            fitsLayer;
           //
           // FITS file obstruction polygon number 
           int            fitsPolynum;
           //
           // XrrtStructure created telescope layer structure
           Count          layer;
           //
           // Telescope quadrant the obstruction lies in
           TelescopeQuadrant quadrant;
           //
           // Distance of the obstruction from the focal plane.
           FPDistanceInMM fpDistance;
           //
           // The actual shape of the obstruction in 2D (z-axis constant).
           XrrtPolygon    polygon;
           //
           // Pointer to the next obstruction in the current layer.
           XrrtObstruction* nextInLayer;

}; 


inline FPDistanceInMM
XrrtObstruction::getFPDistance() const
{
     return(fpDistance);
}

inline XrrtObstruction*
XrrtObstruction::getNextInLayer() const
{
    return nextInLayer;
}

inline void
XrrtObstruction::setQuadrant()
{
    quadrant = polygon.getTelescopeQuadrant();
}

inline TelescopeQuadrant 
XrrtObstruction::getTelescopeQuadrant()
{
    return quadrant;
}
#endif
