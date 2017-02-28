// xrrtquadrant.hh
//
// Class definition for quadrants of X-ray telescopes
// Hideyuki Mori, High Energy Astrophysics Division, ISAS/JAXA
// 2005/12/15 Created framework
// 2006/08/05 Y.ISHISAKI	version 6.4.4
//	add declaration of theQuadrant()

#ifndef XRRTQUADRANT_HH
#define XRRTQUADRANT_HH

//
// System interfaces used
//
#include <exception>
#include <math.h>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrtpolygon.hh"

class  XrrtQuadrant
{
    // theQuadrant() is a friend since it needs private access to the
    // constructor to create the static XrrtQuadrant.
    friend XrrtQuadrant& theQuadrant();

    public:
          // Constructor
          XrrtQuadrant();
          // Copy Constructor
          XrrtQuadrant(const XrrtQuadrant& quadrant);
          //
          // Accessors to access quadrant infomations (offset values,
          // current telescope,  current quadrant, current layer, etc.)
          // during executing the ray trace
          //
          void setCurrentOffset();
          void setCurrentAngleOffset();
          void setCurrentTelescope(const string& telescopeName);

          RadiusInMM getCurrentOffsetX() const;
          RadiusInMM getCurrentOffsetY() const;
          RadiusInMM getCurrentOffsetZ() const;

          AngleInRadians getCurrentAngleOffsetThetaX() const;
          AngleInRadians getCurrentAngleOffsetThetaY() const;
          AngleInRadians getCurrentAngleOffsetThetaZ() const;

          int getCurrentQuadrant() const;
          void setCurrentQuadrant(const TelescopeQuadrant& parameter);

          int getCurrentLayer() const;
          void setCurrentLayer(const Count& parameter);

          // Temporal hard coding
          void clearQuadrant();

          // Initialize the parameters about the quadrant displacements 
          // and rotation angles
          // (modified by Hideyuki MORI : date 2006/03/04)
          void setFitsQuadrant(const int& parameter);
          void setFitsQuadrantLayer(const int& parameter);
          void setDisplacement(const double& deltaX,
			       const double& deltaY,
			       const double& deltaZ);
          void setRotationAngle(const double& deltaTx,
				const double& deltaTy,
				const double& deltaTz);

          // Sub member function to check whether the initialization of the
          // quadrant informations works correctly
          // (added by Hideyuki MORI : date 2006/03/04)
          void checkInitialization();

    private:
          // 
          // Factual data about quadrants that comes from the telescope
          // description file
          //
          AngleInRadians angleOffsetThetaX[6][6];
          AngleInRadians angleOffsetThetaY[6][6];
          AngleInRadians angleOffsetThetaZ[6][6];
          
          VertexInMM offsetX[6][6];
          VertexInMM offsetY[6][6];
          FPDistanceInMM offsetZ[6][6];
          
          //
          // Current quadrant parameters which we focus on
          //
          int currentQuadrant;
          int currentLayer;
          //
          // Add another index to classify the current telescope (XRT-I0, 
          // XRT-I1, XRT-I2, XRT-I3, XRT-S/XRT-I for Astro-E2 XRTs)
          // (temporary modified by Hideyuki MORI : date 2006/01/31)
          //
          int currentTelescope;

          AngleInRadians currentAngleOffsetThetaX;
          AngleInRadians currentAngleOffsetThetaY;
          AngleInRadians currentAngleOffsetThetaZ;

          VertexInMM currentOffsetX;
          VertexInMM currentOffsetY;
          FPDistanceInMM currentOffsetZ;

          // Another index for initializing all of the quadrant parameters
          // The parametes are obtained from the telescope description files
          // (added by Hideyuki MORI : date 2006/03/04)
          int fitsQuadrant;
          int fitsQuadrantLayer;
};

XrrtQuadrant& theQuadrant();

inline RadiusInMM
XrrtQuadrant::getCurrentOffsetX() const
{
  return (currentOffsetX);
}

inline RadiusInMM
XrrtQuadrant::getCurrentOffsetY() const
{
  return (currentOffsetY);
}

inline RadiusInMM
XrrtQuadrant::getCurrentOffsetZ() const
{
  return (currentOffsetZ);
}

inline AngleInRadians
XrrtQuadrant::getCurrentAngleOffsetThetaX() const
{
  return  (currentAngleOffsetThetaX);
}

inline AngleInRadians
XrrtQuadrant::getCurrentAngleOffsetThetaY() const
{
  return  (currentAngleOffsetThetaY);
}

inline AngleInRadians
XrrtQuadrant::getCurrentAngleOffsetThetaZ() const
{
  return  (currentAngleOffsetThetaZ);
}

inline int 
XrrtQuadrant::getCurrentQuadrant() const
{
  return  (currentQuadrant);
}

inline int 
XrrtQuadrant::getCurrentLayer() const
{
  return  (currentLayer);
}
#endif
