// xrrtpolygon.cc
//
// Member functions for XrrtPolygon class
//
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/24 Upgrade documentation. R. Fink

#include "xrrtpolygon.hh"

XrrtPolygon::XrrtPolygon(): 
    xMinimum(0),
    yMinimum(0),
    xMaximum(0),
    yMaximum(0),
    polygonQuadrant(multiQuadrant),
    xVertex(),
    yVertex(),
    numberOfVertii(0),
    polygonType(generalPolygon),
    overrideDefaults(false),
    xTranslation(0.0e0),
    yTranslation(0.e0),
    rotationAngleRadians(0.0e0),
    gis2RadiusInMM(26.0),
    gis2XTranslation(0.0),
    gis2YTranslation(0.0),
    gis3RadiusInMM(26.0),
    gis3XTranslation(0.0),
    gis3YTranslation(0.0),
    sis0Size(22.0/2.0),        // Nominal 22mmx22mm square so -11 -> +11
    sis0XTranslation(0.0),
    sis0YTranslation(0.0),
    sis0AngleRotation(0.0),
    sis1Size(22.0/2.0),        // Nominal 22mmx22mm square so -11 -> +11
    sis1XTranslation(0.0),
    sis1YTranslation(0.0),
    sis1AngleRotation(0.0),
    xis0Size(25.0/2.0),        // Nominal 25mmx25mm square so -12.5 -> +12.5
    xis0XTranslation(0.0),
    xis0YTranslation(0.0),
    xis0AngleRotation(0.0),
    xis1Size(25.0/2.0),        // Nominal 25mmx25mm square so -12.5 -> +12.5
    xis1XTranslation(0.0),
    xis1YTranslation(0.0),
    xis1AngleRotation(0.0),
    xis2Size(25.0/2.0),        // Nominal 25mmx25mm square so -12.5 -> +12.5
    xis2XTranslation(0.0),
    xis2YTranslation(0.0),
    xis2AngleRotation(0.0),
    xis3Size(25.0/2.0),       // Nominal 25mmx25mm square so -12.5 -> +12.5
    xis3XTranslation(0.0),
    xis3YTranslation(0.0),
    xis3AngleRotation(0.0)
{
// Set the rectangular accellerators to the opposite values that they
// will take on as the polygon vertexes are loaded.

     xMaximum = MIN_VERTEX;
     yMaximum = MIN_VERTEX;
     xMinimum = MAX_VERTEX;
     yMinimum = MAX_VERTEX;
}

XrrtPolygon::XrrtPolygon(const XrrtPolygon& polygon):
    xMinimum(0),
    yMinimum(0),
    xMaximum(0),
    yMaximum(0),
    xVertex(),
    yVertex(),
    polygonType(generalPolygon),
    xTranslation(0.0e0),
    yTranslation(0.e0),
    rotationAngleRadians(0.0e0)
{
//
// Copy constructor
//
    //
    // Transfer the rectangular accel info
    //
    xMinimum = polygon.xMinimum;
    yMinimum = polygon.yMinimum;
    xMaximum = polygon.xMaximum;
    yMaximum = polygon.yMaximum;
    //
    // Transfer the contents of the polygons to the new vectors
    //
    copy(polygon.xVertex.begin(), polygon.xVertex.end(), xVertex.begin());
    copy(polygon.yVertex.begin(), polygon.yVertex.end(), yVertex.begin());
    //
    polygonType = polygon.polygonType;
    polygonQuadrant = polygon.polygonQuadrant;
    numberOfVertii = polygon.numberOfVertii;
    overrideDefaults = polygon.overrideDefaults;
    xTranslation = polygon.xTranslation;
    yTranslation = polygon.yTranslation;
    rotationAngleRadians = polygon.rotationAngleRadians;
}

string
XrrtPolygon::errorMessage(XrrtPolygonErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case noSuchPolyVertex:
            errorMessage = 
            "A non-existent polygon vetex was requested";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                           "XrrtPolygon::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}


bool 
XrrtPolygon::pointInside(const  VertexInMM x, const VertexInMM y)
{
//
// Return true if a point lies inside a polygon
//


// handle the request according to the type of polygon implied by the
// polygon type

     if (polygonType == generalPolygon || polygonType == astroeXRSPolygon)
        {
        // Use the bounding box of the polygon as the 1st test
        if ( x < xMinimum || x > xMaximum || 
             y < yMinimum || y > yMaximum)
	   {
	   return(false);
	   }


        // Now try point by point

        ///*The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
        //  with some minor modifications for readability.
        //  comp.graphics.algorithms FAQ 
        //  References:
        //    [Gems IV]  pp. 24-46
        //    [O'Rourke] pp. 233-238
        //    [Glassner:RayTracing] */
 
        //int pnpoly(int npol, double *xp, double *yp, double x, double y)
        //{
        //  int i, j, c = 0;
        //  for (i = 0, j = npol-1; i < npol; j = i++) {
        //    if ((((yp[i]<=y) && (y<yp[j])) ||
        //         ((yp[j]<=y) && (y<yp[i]))) &&
        //        (x < (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i]))
        //      c = !c;
        //  }
        //  return c;
        //}

        int i, j, c = 0;
        for (i = 0, j = numberOfVertii-1; i < numberOfVertii; j = i++) 
            {
            if (
               (
               ((yVertex[i] <= y) && (y < yVertex[j])) ||
               ((yVertex[j] <= y) && (y < yVertex[i]))
               ) &&
               (x < (xVertex[j] - xVertex[i]) * (y - yVertex[i]) / 
                    (yVertex[j] - yVertex[i]) + xVertex[i])
               )
               {
               c = !c;
               }
            }
        return c;
        }
// Convert focal plane location to focal plane mask coords
double maskX = 0.0e0;
double maskY = 0.0e0;
if (rotationAngleRadians != 0.0e0)
   {
   maskX = (x-xTranslation)*cos(rotationAngleRadians) +
           (y-yTranslation)*sin(rotationAngleRadians);
   maskY = (y-yTranslation)*cos(rotationAngleRadians) +
           (x-xTranslation)*sin(rotationAngleRadians);
   }
else
   {
   maskX = x - xTranslation;
   maskY = y - yTranslation;
   }
 
     if (polygonType == ascaGIS2Polygon)
        {
        // is location within the GIS radius?
        double maskRadius = sqrt(maskX*maskX+maskY*maskY);
        if (maskRadius < gis2RadiusInMM)
           {
           return true;
           }
        else
           {
           return false;
           }
        }
     else if (polygonType == ascaGIS3Polygon)
        {
        // is location within the GIS radius?
        double maskRadius = sqrt(maskX*maskX+maskY*maskY);
        if (maskRadius < gis3RadiusInMM)
           {
           return true;
           }
        else
           {
           return false;
           }
        }
     else if (polygonType == ascaSIS0Polygon)
        {
        // is location within the square?
        if (fabs(maskX) < sis0Size && fabs(maskY) < sis0Size)
           {
           return true;
           }
        else
           {
           return false;
           }
        }
     else if (polygonType == ascaSIS1Polygon)
        {
        // is location within the square?
        if (fabs(maskX) < sis1Size && fabs(maskY) < sis1Size)
           {
           return true;
           }
        else
           {
           return false;
           }
        }
     else if (polygonType == astroeXIS0Polygon)
        {
        // is location within the square?
        if (fabs(maskX) < xis0Size && fabs(maskY) < xis0Size)
           {
           return true;
           }
        else
           {
           return false;
           }
        }
     else if (polygonType == astroeXIS1Polygon)
        {
        // is location within the square?
        if (fabs(maskX) < xis1Size && fabs(maskY) < xis1Size)
           {
           return true;
           }
        else
           {
           return false;
           }
        }
     else if (polygonType == astroeXIS2Polygon)
        {
        // is location within the square?
        if (fabs(maskX) < xis2Size && fabs(maskY) < xis2Size)
           {
           return true;
           }
        else
           {
           return false;
           }
        }
     else if (polygonType == astroeXIS3Polygon)
        {
        // is location within the square?
        if (fabs(maskX) < xis3Size && fabs(maskY) < xis3Size)
           {
           return true;
           }
        else
           {
           return false;
           }
        }

    return false;
}

void
XrrtPolygon::polygonComplete()
{
    switch(polygonType)
        {
        case ascaGIS2Polygon:
             if (! overrideDefaults)
                {
                xTranslation = gis2XTranslation;
                yTranslation = gis2YTranslation;
                rotationAngleRadians = 0.0;
                }
             break;
        case ascaGIS3Polygon:
             if (! overrideDefaults)
                {
                xTranslation = gis3XTranslation;
                yTranslation = gis3YTranslation;
                rotationAngleRadians = 0.0;
                }
             break;
        case ascaSIS0Polygon:
             if (! overrideDefaults)
                {
                xTranslation = sis0XTranslation;
                yTranslation = sis0YTranslation;
                rotationAngleRadians = sis0AngleRotation;
                }
             break;
        case ascaSIS1Polygon:
             if (! overrideDefaults)
                {
                xTranslation = sis1XTranslation;
                yTranslation = sis1YTranslation;
                rotationAngleRadians = sis1AngleRotation;
                }
             break;
        case astroeXIS0Polygon:
             if (! overrideDefaults)
                {
                xTranslation = xis0XTranslation;
                yTranslation = xis0YTranslation;
                rotationAngleRadians = xis0AngleRotation;
                }
             break;
        case astroeXIS1Polygon:
             if (! overrideDefaults)
                {
                xTranslation = xis1XTranslation;
                yTranslation = xis1YTranslation;
                rotationAngleRadians = xis1AngleRotation;
                }
             break;
        case astroeXIS2Polygon:
             if (! overrideDefaults)
                {
                xTranslation = xis2XTranslation;
                yTranslation = xis2YTranslation;
                rotationAngleRadians = xis2AngleRotation;
                }
             break;
        case astroeXIS3Polygon:
             if (! overrideDefaults)
                {
                xTranslation = xis3XTranslation;
                yTranslation = xis3YTranslation;
                rotationAngleRadians = xis3AngleRotation;
                }
             break;
        case astroeXRSPolygon:
             // use the corners of the area to define a polygon as a 1st cut
             // from the XRS pixel cal file xrs_pixel-map_1998-12-12.fits
             setVertex(4.0000002e-03,  5.3369999e+00); // pixel 23 corner 4
             setVertex(1.2350000e+00,  5.3429999e+00); //       23        3
             setVertex(1.2470000e+00,  5.2249999e+00); //       31        4
             setVertex(2.4770000e+00,  5.2249999e+00); //       31        3
             setVertex(2.4670000e+00, -1.2000000e-01); //        7        2
             setVertex(1.2350000e+00, -1.2200000e-01); //        7        1
             setVertex(1.2210000e+00,  2.0000001e-03); //       15        2
             setVertex(0.0000000e+00,  0.0000000e+00); //       15        1
             break;
        case generalPolygon:
             break;
        default:
             throw invalidPolygonType;
             break;
        }
}




void 
XrrtPolygon::setVertex(const VertexInMM x, const VertexInMM y)
{
        // add point to vector end
        xVertex.push_back( x);
        yVertex.push_back( y);
        // Update the minimum and maximum x and y values
        if (x < xMinimum)
           {
           xMinimum = x;
           }
        if (x > xMaximum)
           {
           xMaximum = x;
           }
        if (y < yMinimum)
           {
           yMinimum = y;
           }
        if (y > yMaximum)
           {
           yMaximum = y;
           }
        // reset vertex count
        numberOfVertii = xVertex.size();
        // reset the polygon quadrant
        if ((xMinimum >= 0.0e0 && yMinimum >= 0.0e0) &&
            (xMaximum >= 0.0e0 && yMaximum >= 0.0e0))
           {
           polygonQuadrant = firstQuadrant;
           }
        else if ((xMinimum <= 0.0e0 && yMinimum >= 0.0e0) &&
                 (xMaximum <= 0.0e0 && yMaximum >= 0.0e0))
           {
           polygonQuadrant = secondQuadrant;
           }
        else if ((xMinimum <= 0.0e0 && yMinimum <= 0.0e0) &&
                 (xMaximum <= 0.0e0 && yMaximum <= 0.0e0))
           {
           polygonQuadrant = thirdQuadrant;
           }
        else if ((xMinimum >= 0.0e0 && yMinimum <= 0.0e0) &&
                 (xMaximum >= 0.0e0 && yMaximum <= 0.0e0))
           {
           polygonQuadrant = fourthQuadrant;
           }
        else
           {
           polygonQuadrant = multiQuadrant;
           }
}

int 
XrrtPolygon::getPolygonSize() const
{
    return xVertex.size();
}

double 
XrrtPolygon::getPolygonX(const int& vertexNumber) const
{
    if (0 <= vertexNumber && (unsigned int)vertexNumber < xVertex.size())
       {
       return xVertex[vertexNumber];
       }
    else
       {
       throw noSuchPolyVertex;
       }
}

double 
XrrtPolygon::getPolygonY(const int& vertexNumber) const
{
    if (0 <= vertexNumber && (unsigned int)vertexNumber < yVertex.size())
       {
       return yVertex[vertexNumber];
       }
    else
       {
       throw noSuchPolyVertex;
       }
}


void 
XrrtPolygon::setPolygonType(XrrtPolygonType newPolygonType)
{
    polygonType = newPolygonType;
}

void 
XrrtPolygon::setPolygonTranslation(double& xTranslationValue, 
                                   double& yTranslationValue,
                                   double& rotateTranslationDegrees)
{
const double DEGREE2RADIAN =  0.017453292519943295769e0;
    overrideDefaults = true;
    xTranslation = xTranslationValue;
    yTranslation = yTranslationValue;
    rotationAngleRadians = rotateTranslationDegrees*DEGREE2RADIAN;
}

