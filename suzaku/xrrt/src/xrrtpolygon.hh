// xrrtpolygon.hh
//
// Class definition for polygons needed by ray tracing
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/22 Upgraded documentation. R. Fink

#ifndef XRRTPOLYGON_HH
#define XRRTPOLYGON_HH

//
// System interfaces used
//
#include <exception>
#include <string>
// Modified by H. Mori (2005/09/14)
// #include <stl.h>
#include <vector>
// #include <stdio.h>
#include <cstdio>
#include <cmath>

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
enum XrrtPolygonErrorCode
        {
        XrrtPolygonErrors,
        noSuchPolyVertex,    // Polygon vertex was requested that did not exist
        invalidPolygonType,  // polygonComplete function needs updated
        XrrtPolygonErrorsEnd
        };
enum XrrtPolygonType
        {
        ascaGIS2Polygon,
        ascaGIS3Polygon,
        ascaSIS0Polygon,
        ascaSIS1Polygon,
        astroeXIS0Polygon,
        astroeXIS1Polygon,
        astroeXIS2Polygon,
        astroeXIS3Polygon,
        astroeXRSPolygon,
        generalPolygon
        };
enum TelescopeQuadrant
        {
        noQuadrant,
        firstQuadrant,
        secondQuadrant,
        thirdQuadrant,
        fourthQuadrant,
        multiQuadrant
        };


//
// XrrtPolygon handles polygons needed by XrrtObstruction and any other
// XRRT function (XrrtStat)
//
class  XrrtPolygon 
{
    public:
          // Constructor
          XrrtPolygon( ); 
          // Copy Constructor
          XrrtPolygon( const XrrtPolygon& polygon );  

          // 
          // setPolygonType defines the type of polygon this is
          void setPolygonType(XrrtPolygonType polygonType);
          
          //
          // setPolygonTranslation defines how to translate the existing
          // polygon params to the final coordinate system
          void setPolygonTranslation(double& xTranslation, double& yTranslation,
                                     double& rotateTranslationDegrees);

          //
          // setVertex inserts a vertex of a polygon into the
	  //       vertex list.
          //
          void setVertex(const VertexInMM x, const VertexInMM y);

          //
          // Return the size of the polygon in vertexes
          //
          int getPolygonSize() const;

          //
          // Return the polygon X/Y vertex
          //
          double getPolygonX(const int& vertexNumber) const;
          double getPolygonY(const int& vertexNumber) const;

          //
          // pointInside determines whether a point lies inside a polygon
          //       obstruction or not.
          //
          bool pointInside(const VertexInMM x, const VertexInMM y);

          //
          // Signal definition of polygon is complete
          void polygonComplete();

          //
          // Return the Telescope Quadrant the polygon falls in
          TelescopeQuadrant getTelescopeQuadrant();
          //
          // Convert error codes to error messages
          //
          string errorMessage(XrrtPolygonErrorCode errorCode);

    private:
           // 
           // Rectangular polygon shape accellerators
	   VertexInMM     xMinimum;
	   VertexInMM     yMinimum;
	   VertexInMM     xMaximum;
	   VertexInMM     yMaximum;
           TelescopeQuadrant polygonQuadrant;
           //
           // Polygon corners in x,y plane
           vector<VertexInMM> xVertex;
           vector<VertexInMM> yVertex;
           int numberOfVertii;
           //
           // polygon type
           XrrtPolygonType polygonType;
           //
           // Polygon translations variables
           bool overrideDefaults;
           double xTranslation;
           double yTranslation;
           double rotationAngleRadians;

           //
           // ASCA GIS DEFAULTS
           double gis2RadiusInMM;
           double gis2XTranslation;
           double gis2YTranslation;
           double gis3RadiusInMM; 
           double gis3XTranslation;
           double gis3YTranslation;

           //
           // ASCA SIS DEFAULTS
           double sis0Size;
           double sis0XTranslation;
           double sis0YTranslation;
           double sis0AngleRotation;
           double sis1Size;
           double sis1XTranslation;
           double sis1YTranslation;
           double sis1AngleRotation;

           //
           // ASTRO-E DEFAULTS
           double xrs;

           double xis0Size;
           double xis0XTranslation;
           double xis0YTranslation;
           double xis0AngleRotation;
           double xis1Size;
           double xis1XTranslation;
           double xis1YTranslation;
           double xis1AngleRotation;
           double xis2Size;
           double xis2XTranslation;
           double xis2YTranslation;
           double xis2AngleRotation;
           double xis3Size;
           double xis3XTranslation;
           double xis3YTranslation;
           double xis3AngleRotation;


}; 

inline TelescopeQuadrant
XrrtPolygon::getTelescopeQuadrant()
{
   return polygonQuadrant;
}

#endif
