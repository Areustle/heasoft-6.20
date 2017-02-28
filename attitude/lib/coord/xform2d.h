#ifndef XFORM2D_INCLUDED

#include <stdlib.h>
#include <stdio.h>

#define XFORM2D_ROUNDOFF_ERR 1e-15

#include "rotmatrix.h"

/*****************************************************************************
******************************************************************************
* XFORM2D data structure which describes an arbitrary linear transformation
* between two 2-dimensional coordinate systems.
* It is composed of a 2x2 rotation matrix and a translation.
* A transformation consists of first applying the rotation (about the origin)
* and then the translation.
*****************************************************************************/
typedef struct {

double rot[2][2];

double xshift;
double yshift;

} XFORM2D;


/*****************************************************************************
******************************************************************************
* allocate space for an XFORM2D structure
*****************************************************************************/
XFORM2D* allocateXform2d(void);


/*****************************************************************************
******************************************************************************
* free space for an XFORM2D structure
*****************************************************************************/
void destroyXform2d(XFORM2D* xform);

/*****************************************************************************
******************************************************************************
* copy one xform2d into another
*****************************************************************************/
void copyXform2d(XFORM2D* copy, XFORM2D* xform);

/*****************************************************************************
******************************************************************************
* Perform a transformation on a pair of discrete pixel coordinates.
* dx and dy give the exact location of the event with respect to the 
* center of the pixel. Note that in general the exact location of an 
* event within a pixel is unknown, so the standard practice is to use
* random numbers between -5. and +.5 for dx and dy.
*****************************************************************************/
void applyXform2dToDiscreteCoords(XFORM2D* xform, double* x1, double* y1, 
                                  int i, int j, double dx, double dy);

/*****************************************************************************
******************************************************************************
* Perform a transformation on a pair of real-valued coordinates.
*****************************************************************************/
void applyXform2dToContinuousCoords(XFORM2D* xform, double* x1, double* y1, 
                                    double x0, double y0 );

/*****************************************************************************
******************************************************************************
* transform the difference in position between two sets of coordinates
* This is equivalent to (x1,y1) = trans(x0,y0) - trans(0,0) .
*****************************************************************************/
void applyXform2dToOffset(XFORM2D* xform, double* x1, double* y1,
                                          double x0, double y0 );

/*****************************************************************************
******************************************************************************
* Find the transform (xform) equivalent to 
* first applying xform1 and then xform2
*****************************************************************************/
void combineXform2ds(XFORM2D* xform,XFORM2D* xform1, XFORM2D* xform2);

/*****************************************************************************
******************************************************************************
* set the XFORM2D structure for a simple translation, such that the
* old origin will be at new coordinates (deltax, deltay)
*****************************************************************************/
void setXform2dToTranslation(XFORM2D* xform, double deltax, double deltay );

/*****************************************************************************
******************************************************************************
* set the XFORM2D structure for a simple scaling, such that the
* position of the point (x0,y0) remains fixed.
* Note: negative scale factors mean a coordinate inversion.
*****************************************************************************/
void setXform2dToScaling(XFORM2D* xform, double xscale, double yscale,
                         double x0, double y0 );

/*****************************************************************************
******************************************************************************
* set the XFORM2D structure for rotation about the point (x0,y0) in the
* old coordinate system. This is the same as translating by (-x0, -y0),
* doing a rotation about the origin, and then translating by (x0,y0).
*****************************************************************************/
void setXform2dToRotation(XFORM2D* xform, double sinang, double cosang, 
                          double x0, double y0);

/*****************************************************************************
******************************************************************************
* modify a transform to be the original followed by a translation.
* This is similar to setXform2dToTranslation followed by combineXform2ds,
* but can be much more efficient.
*****************************************************************************/
void applyTranslationToXform2d(XFORM2D* xform, double deltax, double deltay );

/*****************************************************************************
******************************************************************************
* Aberration is the pedestrian cousin to relativistic beaming.
* A moving observer will see a star in a position shifted away from the
* direction of motion.
* For the full relativistic equation see Rybicki and Lightman eq 4.8.
* The dominant component of the observer's velocity is the motion of the 
* Earth around the Sun which has v/c~1e-4, so in the following
* calculation we discard all terms of order higher than (v/c)^1, and
* are accurate to one part in a few x 1e-4. 
* With this approximation a star's position is shifted by an angle 
* (v/c)sin(theta),
* where theta is the angle between the position of the star and
* the direction of motion.
* This means that projected onto the tangent plane, the aberration shift
* in radians is just the velocity vector in units of c (also projected
* onto the plane).
*
* Aberration is at most ~20 arcsec.
* 
* This routine adds the affects of aberration to an existing
* detector to sky transformation.
*
* rot is the rotation matrix specifying the orientation of the tangent
* plane coordinates with respect to the celestial coordinates.
* Note that by convention the tangent plan X axis is inverted with respect to
* to the three dimensional X axis.
* v is the magnitude of the observer's (i.e. the Earth's) velocity
* and vhat is the three dimensional unit vector giving the
* direction of that velocity. in cartesian celestial coordinates.
* pix_per_radian gives the plate scale.
*****************************************************************************/
void addAberrationToXform2d(XFORM2D* xform, ROTMATRIX* rot,
                            double v, double vhat[3],
                            double pix_per_radian );


/*****************************************************************************
******************************************************************************
* Aberration is the pedestrian cousin to relativistic beaming.
* A moving observer will see a star in a position shifted away from the
* direction of motion.
* For the full relativistic equation see Rybicki and Lightman eq 4.8.
* The dominant component of the observer's velocity is the motion of the 
* Earth around the Sun which has v/c~1e-4, so in the following
* calculation we discard all terms of order higher than (v/c)^1, and
* are accurate to within a few x 1e-4. 
* With this approximation a star's position is shifted by an angle 
* (v/c)sin(theta),
* where theta is the angle between the position of the star and
* the direction of motion.
* This means that projected onto the tangent plane, the aberration shift
* in radians is just the velocity vector in units of c (also projected
* onto the plane).
*
* Aberration is at most ~20 arcsec.
* 
* This routine adds the affects of aberration to an existing
* DET -> SKY transformation.
*
* xhat and yhat are three dimensional unit vectors which define the
* SKYX and SKYY directions in the tangent plane projection.
* v is the magnitude of the observer's (i.e. the Earth's) velocity
* and vhat is the three dimensional unit vector giving the
* direction of that velocity.
* pix_per_radian gives the plate scale.
*****************************************************************************/
void old_addAberrationToXform2d(XFORM2D* xform, double xhat[3], double yhat[3],
                            double v, double vhat[3],
                            double pix_per_radian );


/*****************************************************************************
*****************************************************************************
* This routine sets xform2 to the inverse of xform1
*****************************************************************************/
void invertXform2d(XFORM2D* xform2, XFORM2D* xform1);

/***************************************************************************
****************************************************************************
* Determine the best transformation from the four points
* (0,0) (1,0) (1,1) (0,1) to the four points specified by (x,y)
*
* Note three points define the transform, but we
* have four points. We are in effect finding the parallegram
* whose corners match the measured corners the best.
* "The best" is defined as having the smallest sum of
* the squares of distances between measured points and parallegram 
* corners.
* Chosing the four corners above simplifies the math considerably,
* and most of the least squares calculation is done analytically.
* to obtain the transformation from a different set of points to 
* the four given points, combine the output of this function
* with another transform using combineXform2ds.
*
* This function returns the R.M.S. error between the given points and the
* ones specified by the transformation.
****************************************************************************/
double setXform2dFromCornerPixels(XFORM2D* xform, double* x, double* y);


/*****************************************************************************
*****************************************************************************
* print the components of an XFORM2D structure to a particular stream.
* This is mostly useful for debugging.
*****************************************************************************/
void printXform2d(XFORM2D* xform, FILE* stream);

/*****************************************************************************
******************************************************************************
* Determine the rigid transformation which takes the set of points
* (x0,y0) as close as possible to (x1,y1). A rigid transform consists
* of only rotations and translations.
* "Closest" is defined as the weighted sum of the squares of the distances 
* from the transformed (x0,y0) to the (x1,y1). the "wgt" array gives
* this weighting. Usually wgt would be set to 1./(sigma*sigma), and the
* closeness criterion would be "chi-squared". If a null pointer
* is given for wgt, then all the weights are assumed to equal one.
******************************************************************************/
void findBestRigidXform2d(XFORM2D* xform, double x0[], double y0[],
                                     double x1[], double y1[],
                          double wgt[], int npoints);
			  

/********************************************************************************
* Similar to findBestRigidXform2d, but it sets trans to the least squares
* best linear transform without restricting to rotations and translations.
********************************************************************************/
void findBestXform2d(XFORM2D* trans, double* x0, double* y0, double* x1, double* y1,
                     double* wgt, int npoints);
		

#define XFORM2D_INCLUDED
#endif /* XFORM2D_INCLUDED */
