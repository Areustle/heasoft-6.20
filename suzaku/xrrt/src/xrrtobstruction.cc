// xrrtobstruction.cc
//
// Member functions for XrrtObstruction class
//
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/24 Upgrade documentation. R. Fink
// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 1.1  2000/10/19 12:19:46  mori
// Initial revision

#include "xrrtobstruction.hh"

XrrtObstruction::XrrtObstruction():
    fitsLayer(0),
    fitsPolynum(0),
    layer(0),
    quadrant(noQuadrant),
    fpDistance(0),
    polygon(),
    nextInLayer(0)
{
// A simple constructor
}

string
XrrtObstruction::errorMessage(XrrtObstructionErrorCode errorCode)
{
//
// Convert error codes to error messages
//
string errorMessage;

    switch (errorCode)
        {
        case noSuchVertex:
            errorMessage = 
            "A request was made for a vertex in an obstruction that does not exist";
            break;
        default:
                {
                char charNumber[1024];
                sprintf(charNumber, "%d",errorCode);
                errorMessage = 
                           "XrrtObstruction::errorMessage Unknown error code: ";
                errorMessage.append(charNumber);
                }
             break;
        }
     return errorMessage;
}

string
XrrtObstruction::errorMessage(XrrtPolygonErrorCode errorCode)
{
//
// A pass thru for error codes that belong to XrrtPolygon
//
string errorMessage;

     errorMessage = polygon.errorMessage(errorCode);
     return errorMessage;
}


void 
XrrtObstruction::setFitsLayer(const int& userlayer)
{
    fitsLayer = userlayer;
}

int  
XrrtObstruction::getFitsLayer() const
{
    return fitsLayer;
}

void 
XrrtObstruction::setFitsPolynum(const int& polygonNumber)
{
    fitsPolynum = polygonNumber;
}

int  
XrrtObstruction::getFitsPolynum() const
{
    return fitsPolynum;
}



void
XrrtObstruction::setVertex(const VertexInMM x, const VertexInMM y)
{
    polygon.setVertex(x,y);
}

void 
XrrtObstruction::setFPDistance(const FPDistanceInMM parameter)
{
    fpDistance = parameter;
}

bool
XrrtObstruction::sortObstructionDescend(const XrrtObstruction* const x, 
                                        const XrrtObstruction* const y)
{
    return (x->getFPDistance() > y->getFPDistance());
}
         
void 
XrrtObstruction::setLayer(const Count parameter)
{
    layer = parameter;
}


bool
XrrtObstruction::operator< (const XrrtObstruction& rhs) const
{
     return (fpDistance < rhs.fpDistance);
}

bool
XrrtObstruction::operator> (const XrrtObstruction& rhs) const
{
     return (fpDistance > rhs.fpDistance);
}

void 
XrrtObstruction::setNextInLayer(const XrrtObstruction* nextObstruction)
{
    nextInLayer = (XrrtObstruction*) nextObstruction;
}

bool
XrrtObstruction::xyInObstruction(VertexInMM x, VertexInMM y)
{
    if (quadrant == firstQuadrant)
       {
       if (x >= 0.0e0 && y >= 0.0e0)
          {
          return polygon.pointInside(x,y);
          }
       else
          {
          return false;
          }
       }
   else if (quadrant == secondQuadrant)
       {
       if (x <= 0.0e0 && y >= 0.0e0)
          {
          return polygon.pointInside(x,y);
          }
       else
          {
          return false;
          }
      }
   else if (quadrant == thirdQuadrant)
      { 
       if (x <= 0.0e0 && y <= 0.0e0)
          {
          return polygon.pointInside(x,y);
          }
       else
          {
          return false;
          }
      }
   else if (quadrant == fourthQuadrant)
      {
      if (x >= 0.0e0 && y <= 0.0e0)
          {
          return polygon.pointInside(x,y);
          }
       else
          {
          return false;
          }
      }
   else if (quadrant == multiQuadrant)
       {
       return polygon.pointInside(x,y);
       }

   return false;
}
