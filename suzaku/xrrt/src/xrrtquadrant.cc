// xrrtquadrant.cc
//
// Member functions for Quadrant class
// Hideyuki Mori, High Energy Astrophysics Division, ISAS/JAXA
// 2005/12/17 Created framework


#include <cstring>
#include "xrrtquadrant.hh"

XrrtQuadrant&
theQuadrant()
{
// 
// Returns static quadrant info for XRRT
//
	static XrrtQuadrant quadrant;

	return quadrant;
}

XrrtQuadrant::XrrtQuadrant():
    currentQuadrant(0),
    currentLayer(0),
    currentAngleOffsetThetaX(0),
    currentAngleOffsetThetaY(0),
    currentAngleOffsetThetaZ(0),
    currentOffsetX(0),
    currentOffsetY(0),
    currentOffsetZ(0)
{
  // A simple constructor
}
  
void
XrrtQuadrant::setCurrentQuadrant(const TelescopeQuadrant& parameter)
{
    currentQuadrant = parameter;
}

void
XrrtQuadrant::setCurrentLayer(const Count& parameter)
{
    currentLayer = parameter;
}

//
// Temporal coding to set the current telescope we now execute
// the ray-tracing
// (added by Hideyuki MORI : date 2006/01/31)
//
void
XrrtQuadrant::setCurrentTelescope(const string& telescopeName)
{
  // int i, j, k;

  if((strncasecmp(telescopeName.c_str(), "XRT-S", 5)) == 0)
    {
    currentTelescope = 0;
    }
  else if((strncasecmp(telescopeName.c_str(), "XRT-I0", 6)) == 0)
    {
    currentTelescope = 1;
    }
  else if((strncasecmp(telescopeName.c_str(), "XRT-I1", 6)) == 0)
    {
    currentTelescope = 2;
    }
  else if((strncasecmp(telescopeName.c_str(), "XRT-I2", 6)) == 0)
    {
    currentTelescope = 3;
    }
  else if((strncasecmp(telescopeName.c_str(), "XRT-I3", 6)) == 0)
    {
    currentTelescope = 4;
    }
  else if((strncasecmp(telescopeName.c_str(), "XRT-I", 5)) == 0)
    {
    currentTelescope = 5;
    }
  // else
  //  {
  //  throw invalidCurrentTelescopeName;
  //  }

  // initialized the offset values
  // After introducing the new telescope description files which contain
  // the offset values, this initializing routine does not need
  // (comment out by Hideyuki MORI : 2006/03/05)
  // for(i = 0; i < 6; i++){
  //   for(j = 0 ; j < 6; j++){
  //     offsetX[i][j] = 0.0e0;
  //     offsetY[i][j] = 0.0e0;
  //     offsetZ[i][j] = 0.0e0;
  //     angleOffsetThetaX[i][j] = 0.0e0;
  //     angleOffsetThetaY[i][j] = 0.0e0;
  //     angleOffsetThetaZ[i][j] = 0.0e0;
  //   }
  // }

  currentQuadrant = 0;
  currentLayer = 0;

  currentAngleOffsetThetaX = 0.0e0;
  currentAngleOffsetThetaY = 0.0e0;
  currentAngleOffsetThetaZ = 0.0e0;

  currentOffsetX = 0.0e0;
  currentOffsetY = 0.0e0;

}

void
XrrtQuadrant::setCurrentOffset()
{
    currentOffsetX = offsetX[currentQuadrant][currentLayer];
    currentOffsetY = offsetY[currentQuadrant][currentLayer];
    currentOffsetZ = offsetZ[currentQuadrant][currentLayer];
}

void
XrrtQuadrant::setCurrentAngleOffset()
{
    currentAngleOffsetThetaX = angleOffsetThetaX[currentQuadrant][currentLayer];
    currentAngleOffsetThetaY = angleOffsetThetaY[currentQuadrant][currentLayer];
    currentAngleOffsetThetaZ = angleOffsetThetaZ[currentQuadrant][currentLayer];
}

// Temporal hard coding to remove as soon as possible
// initialize arrays of the offset parameters
// (added by Hideyuki MORI : date 2006/01/28)
void 
XrrtQuadrant::clearQuadrant()
{
const double ARCMIN2DEGREE = 1.0e0/60.0e0;
const double DEGREE2RADIANS = 0.017453292519943295769e0;
const double ARCMIN2RADIAN = ARCMIN2DEGREE * DEGREE2RADIANS;
// int i, j, k;
int j; 

  if(currentTelescope == 0)
    {
    }
  else if(currentTelescope == 1)
    {
      // Telescope : XRT-I0
      for(j = 0; j < 6; j++){
	// Offset parameters:
	// Move the first quadrant (45 - 135 degree in azimuth) by 
	// (0.07468mm, 0.1017mm) in (x, y) plane
	// Rotate the first quadrant by (-2.769', -0.094') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[1][j] = 0.07468e0;
	offsetY[1][j] = 0.1017e0;
	offsetZ[1][j] = 0.0e0;
	angleOffsetThetaX[1][j] = -2.769e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[1][j] = -0.094e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[1][j] = 0.0e0;
	// Move the fourth quadrant (135 - 225 degree in azimuth) by 
	// (-0.02142mm, -0.07316mm) in (x, y) plane
	// Rotate the first quadrant by (-0.381', -2.477') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[2][j] = -0.02142e0;
	offsetY[2][j] = -0.07316e0;
	offsetZ[2][j] = 0.0e0;
	angleOffsetThetaX[2][j] = -0.381e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[2][j] = -2.477e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[2][j] = 0.0e0;
	// Move the third quadrant (225 - 315 degree in azimuth) by 
	// (-0.07542mm, -0.1272mm) in (x, y) plane
	// Rotate the first quadrant by (+2.721', +0.194') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[3][j] = -0.07542e0;
	offsetY[3][j] = -0.1272e0;
	offsetZ[3][j] = 0.0e0;
	angleOffsetThetaX[3][j] = +2.721e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[3][j] = +0.194e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[3][j] = 0.0e0;
	// Move the second quadrant (315 - 45 degree in azimuth) by 
	// (0.02216mm, 0.09862mm) in (x, y) plane
	// Rotate the first quadrant by (+0.370', +2.827') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[4][j] = 0.02216e0;
	offsetY[4][j] = 0.09862e0;
	offsetZ[4][j] = 0.0e0;
	angleOffsetThetaX[4][j] = +0.370e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[4][j] = +2.827e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[4][j] = 0.0e0;
      }
    }
  else if(currentTelescope == 2)
    {
      // Telescope : XRT-I1
      for(j = 0; j < 6; j++){
	// Offset parameters:
	// Move the first quadrant (45 - 135 degree in azimuth) by 
	// (-0.00233mm, 0.07006mm) in (x, y) plane
	// Rotate the first quadrant by (-2.245', +0.206') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[1][j] = -0.00233e0;
	offsetY[1][j] = 0.07006e0;
	offsetZ[1][j] = 0.0e0;
	angleOffsetThetaX[1][j] = -2.245e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[1][j] = +0.206e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[1][j] = 0.0e0;
	// Move the fourth quadrant (135 - 225 degree in azimuth) by 
	// (0.03023mm, -0.01353mm) in (x, y) plane
	// Rotate the first quadrant by (-0.633', -1.871') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[2][j] = 0.03023e0;
	offsetY[2][j] = -0.01353e0;
	offsetZ[2][j] = 0.0e0;
	angleOffsetThetaX[2][j] = -0.633e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[2][j] = -1.871e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[2][j] = 0.0e0;
	// Move the third quadrant (225 - 315 degree in azimuth) by 
	// (0.00129mm, 0.00089mm) in (x, y) plane
	// Rotate the first quadrant by (+1.989', +0.123') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[3][j] = 0.00129e0;
	offsetY[3][j] = 0.00089e0;
	offsetZ[3][j] = 0.0e0;
	angleOffsetThetaX[3][j] = +1.989e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[3][j] = +0.123e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[3][j] = 0.0e0;
	// Move the second quadrant (315 - 45 degree in azimuth) by 
	// (-0.02919mm, -0.05743mm) in (x, y) plane
	// Rotate the first quadrant by (+0.581', +1.623') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[4][j] = -0.02919e0;
	offsetY[4][j] = -0.05743e0;
	offsetZ[4][j] = 0.0e0;
	angleOffsetThetaX[4][j] = +0.581e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[4][j] = +1.623e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[4][j] = 0.0e0;
      }
    }
  else if(currentTelescope == 3)
    {
      // Telescope : XRT-I2
      for(j = 0; j < 6; j++){
	// Offset parameters:
	// Move the first quadrant (45 - 135 degree in azimuth) by 
	// (-0.05480mm, 0.3830mm) in (x, y) plane
	// Rotate the first quadrant by (-3.557', +0.666') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[1][j] = -0.05480e0;
	offsetY[1][j] = 0.3830e0;
	offsetZ[1][j] = 0.0e0;
	angleOffsetThetaX[1][j] = -3.557e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[1][j] = +0.666e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[1][j] = 0.0e0;
	// Move the fourth quadrant (135 - 225 degree in azimuth) by 
	// (0.1072mm, -0.1023mm) in (x, y) plane
	// Rotate the first quadrant by (-0.766', -3.242') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[2][j] = 0.1072e0;
	offsetY[2][j] = -0.1023e0;
	offsetZ[2][j] = 0.0e0;
	angleOffsetThetaX[2][j] = -0.766e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[2][j] = -3.242e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[2][j] = 0.0e0;
	// Move the third quadrant (225 - 315 degree in azimuth) by 
	// (0.05641mm, -0.2040mm) in (x, y) plane
	// Rotate the first quadrant by (+3.587', +0.079') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[3][j] = 0.05641e0;
	offsetY[3][j] = -0.2040e0;
	offsetZ[3][j] = 0.0e0;
	angleOffsetThetaX[3][j] = +3.587e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[3][j] = +0.079e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[3][j] = 0.0e0;
	// Move the second quadrant (315 - 45 degree in azimuth) by 
	// (-0.1088mm, -0.07599mm) in (x, y) plane
	// Rotate the first quadrant by (+0.468', +2.816') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[4][j] = -0.1088e0;
	offsetY[4][j] = -0.07599e0;
	offsetZ[4][j] = 0.0e0;
	angleOffsetThetaX[4][j] = +0.468e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[4][j] = +2.816e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[4][j] = 0.0e0;
      }
    }
  else if(currentTelescope == 4)
    {
      // Telescope : XRT-I3
      for(j = 0; j < 6; j++){
	// Offset parameters:
	// Move the first quadrant (45 - 135 degree in azimuth) by 
	// (-0.1052mm, 0.08251mm) in (x, y) plane
	// Rotate the first quadrant by (-2.739', +0.182') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[1][j] = -0.1052e0;
	offsetY[1][j] = 0.08251e0;
	offsetZ[1][j] = 0.0e0;
	angleOffsetThetaX[1][j] = -2.739e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[1][j] = +0.182e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[1][j] = 0.0e0;
	// Move the fourth quadrant (135 - 225 degree in azimuth) by 
	// (-0.07868mm, 0.02273mm) in (x, y) plane
	// Rotate the first quadrant by (-0.582', -1.633') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[2][j] = -0.07868e0;
	offsetY[2][j] = 0.02273e0;
	offsetZ[2][j] = 0.0e0;
	angleOffsetThetaX[2][j] = -0.582e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[2][j] = -1.633e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[2][j] = 0.0e0;
	// Move the third quadrant (225 - 315 degree in azimuth) by 
	// (0.09943mm, -0.1989mm) in (x, y) plane
	// Rotate the first quadrant by (+2.479', +0.604') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[3][j] = 0.09943e0;
	offsetY[3][j] = -0.1989e0;
	offsetZ[3][j] = 0.0e0;
	angleOffsetThetaX[3][j] = +2.479e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[3][j] = +0.604e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[3][j] = 0.0e0;
	// Move the second quadrant (315 - 45 degree in azimuth) by 
	// (0.08445mm, 0.09368mm) in (x, y) plane
	// Rotate the first quadrant by (+0.518', +2.055') around X & Y-axes
	// (modified by Hideyuki MORI : date 2006/02/26)
	offsetX[4][j] = 0.08445e0;
	offsetY[4][j] = 0.09368e0;
	offsetZ[4][j] = 0.0e0;
	angleOffsetThetaX[4][j] = +0.518e0 * ARCMIN2RADIAN;
	angleOffsetThetaY[4][j] = +2.055e0 * ARCMIN2RADIAN;
	angleOffsetThetaZ[4][j] = 0.0e0;
      }
    }
  else if(currentTelescope == 5)
    {
    }

}

// Initialize the parameters about the quadrant displacements 
// and rotation angles, which are obtained from the telescope 
// description files
// (modified by Hideyuki MORI : date 2006/03/04)

// Get the quadrant/layer we now initialize
void 
XrrtQuadrant::setFitsQuadrant(const int& parameter)
{
    fitsQuadrant = parameter;
}
  
void 
XrrtQuadrant::setFitsQuadrantLayer(const int& parameter)
{
    fitsQuadrantLayer = parameter;
}

// Get the displacements of each quadrant focus
void 
XrrtQuadrant::setDisplacement(const double& deltaX,
			      const double& deltaY,
			      const double& deltaZ)
{
    offsetX[fitsQuadrant][fitsQuadrantLayer] = deltaX;
    offsetY[fitsQuadrant][fitsQuadrantLayer] = deltaY;
    offsetZ[fitsQuadrant][fitsQuadrantLayer] = deltaZ;
}

// Get the rotation angles of each quadrant focus
void 
XrrtQuadrant::setRotationAngle(const double& deltaTx,
			       const double& deltaTy,
			       const double& deltaTz)
{
const double ARCMIN2DEGREE = 1.0e0/60.0e0;
const double DEGREE2RADIANS = 0.017453292519943295769e0;
const double ARCMIN2RADIAN = ARCMIN2DEGREE * DEGREE2RADIANS;

    angleOffsetThetaX[fitsQuadrant][fitsQuadrantLayer] = deltaTx * ARCMIN2RADIAN;
    angleOffsetThetaY[fitsQuadrant][fitsQuadrantLayer] = deltaTy * ARCMIN2RADIAN;
    angleOffsetThetaZ[fitsQuadrant][fitsQuadrantLayer] = deltaTz * ARCMIN2RADIAN;
}

// Sub member function to check whether the initialization of the
// quadrant informations works correctly
// (added by Hideyuki MORI : date 2006/03/04)
void
XrrtQuadrant::checkInitialization()
{
	fprintf(stdout, "fitsQuadrant=%d, fitsQuadrantLayer=%d\n", fitsQuadrant, fitsQuadrantLayer);
    fprintf(stdout, "Offset : %f %f %f\n", offsetX[fitsQuadrant][fitsQuadrantLayer],
	                                      offsetY[fitsQuadrant][fitsQuadrantLayer],
	                                      offsetZ[fitsQuadrant][fitsQuadrantLayer]);
    fprintf(stdout, "Rotation angle : %f %f %f\n", angleOffsetThetaX[fitsQuadrant][fitsQuadrantLayer],
	                                              angleOffsetThetaY[fitsQuadrant][fitsQuadrantLayer],
	                                              angleOffsetThetaZ[fitsQuadrant][fitsQuadrantLayer]);
}
