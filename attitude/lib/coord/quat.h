#ifndef QUAT_INCLUDED

#include "rotmatrix.h"
#include "euler.h"
#include "xform2d.h"

/**********************************************************
* roudoff error tolerance used in quaternion calculations *
**********************************************************/
#define QUAT_ROUNDOFF_ERR 1e-14

/*************************************************************
* Quat structure
**************************************************************/
typedef struct {

double p[4];

} QUAT;



/***************************************************************
****************************************************************
* allocate space for a new quaternion structure
****************************************************************/
QUAT* allocateQuat();

/***************************************************************
****************************************************************
* allocate space for an array of quaternion structures
* note allocateQuatArray(1) is the same as allocateQuat
****************************************************************/
QUAT* allocateQuatArray(int dimen);

/*********************************************************************
*********************************************************************
* Change the size of a previously allocated quaternion array
********************************************************************/
QUAT* changeQuatArraySize(QUAT* q, int newdimen);


/***************************************************************
****************************************************************
* free storage for a quaternion structure
****************************************************************/
void destroyQuat(QUAT* q);

/**************************************************************************
***************************************************************************
* Set the four components of a quaternion and makes sure they are normalized
***************************************************************************/
void setQuat(QUAT* q, double q0, double q1, double q2, double q3);

/**************************************************************************
***************************************************************************
* Set a quaternion to the identity quaternion (0,0,0,1)
***************************************************************************/
void setQuatToIdentity(QUAT* q);

/* Copy a quaternion. */
void copyQuat(QUAT* dest, QUAT* source);

/* Renormalize a quaternion if its norm is not close to unity. */
void renormalizeQuat(QUAT* q);

/* Renormalize a quaternion regardless of its norm. */
void forceRenormalizeQuat(QUAT * q);

/**************************************************************************
***************************************************************************
* Report that there is an inconsistency in a quaternion
* This would usually be due to bad normalization
* The program prints a message to stderr and exits with status "1"
***************************************************************************/
void badQuatError(QUAT* q);

/***************************************************************
****************************************************************
* give the magnitude of a quaternion
* attitude quaternions should be normalized to 1.
****************************************************************/
double normOfQuat(QUAT* q);


/***************************************************************
****************************************************************
* Computes the quaternion product q = q1 q2
* This gives the result of sucessive rotations by q1 and then q2
* Note that the quaternion product does not commute.
****************************************************************/
void productOfQuats(QUAT* q, QUAT* q1, QUAT* q2);

/***************************************************************
****************************************************************
* determine the quaternion q such that q1*q=q2
* In terms of rotations, this gives the quaternion of the rotation
* between the orientations specified by q1 and q2.
* Intuitively it's the "difference" - though mathematically it's
* more like division: q=inv(q1)*q2.
* Note this can also be done using invertQuat and productofQuats,
* but this function is more efficient since it is coded directly 
* with the quaternion components.
****************************************************************/
void getQuatOfChange(QUAT* q, QUAT* q1, QUAT* q2);

/***************************************************************
****************************************************************
* calculate the compliment of a quaternion. The compliment of a 
* quaternion represents a rotation in the opposite direction.
****************************************************************/
void invertQuat(QUAT* q, QUAT* q1);

/***************************************************************
****************************************************************
* calculate the compliment of a quaternion. The compliment of a 
* quaternion represents a rotation in the opposite direction.
****************************************************************/
void invertQuatInPlace(QUAT* q);

/***************************************************************
****************************************************************
* Calculates the angle in radians of the rotation represented by the
* quaternion q divided by 2.
* This routine assumes the quaternion is properly normalized
* also note that by convention, the rotation angle is always
* positive.
****************************************************************/
double getQuatHalfRotation(QUAT* q);

/***************************************************************
****************************************************************
* q represents a rotation about the same axis as q1, but through 
* a rotation angle of a times the rotation angle of q1
* This is useful for interpolation and extrapolation.
* Note this routine assumes the quaternions are normalized 
* to within roundoff error. 
****************************************************************/
void multiplyQuatByScalar(QUAT* q, QUAT* q1,double a);


/***************************************************************************
****************************************************************************
* get the quaternion which will give the shortest "great circle" rotation
* which will transform unit vector hat1 into unit vector hat2
***************************************************************************/
void greatCircleQuat(QUAT* q, double hat1[3], double hat2[3]);


/***************************************************************************
****************************************************************************
* convert a quaternion to a direction cosine rotation matrix
***************************************************************************/
void convertQuatToRotMatrix(ROTMATRIX* rot,QUAT* q);

/***************************************************************************
****************************************************************************
* convert  a direction cosine rotation matrix to a quaternion
***************************************************************************/
void convertRotMatrixToQuat(QUAT* q, ROTMATRIX* rot);

/***************************************************************************
****************************************************************************
* convert a quaternion to a set of euler angles
* This routine achieves the same result as first converting to a rotation
* matrix and then converting to euler angles, but is more efficient
* since not all the rotation matrix elements are needed.
* On the other hand if you already have the rotation matrix available
* it is more efficient to convert that into the euler angles.
***************************************************************************/
void convertQuatToEuler(EULER* e, QUAT* q);

/***************************************************************************
****************************************************************************
* convert  a set of Euler angles to a quaternion
* This routine does exactly the same calculation ar first converting
* to a rotation matrix and then to a quaternion, but it saves having
* to handle the intermediate storage.
* If you already have the rotation matrix, then it is much more
* efficient to convert that to a quaternion.
*****************************************************************************/
void convertEulerToQuat(QUAT* q, EULER* e);

/***************************************************************************
****************************************************************************
* Determine the two dimensional transformation from one tangent plane
* projection coordinate system to another, which has been rotated by
* a given quaternion from the first. This transformation can be used
* for detector to sky transformations. 
* Note that the exact transformation is non-linear but the linear 
* approximation is very good for small angles.
* (oldx0, oldy0) and (newx0, newy0) are the tangent points in the
* old and new projected coordinate systems. 
* pixels_per_radian is the scale of both new and old coordinate systems
* at the tangent point.
* deltaq is the rotation from the new coordinate system to the old one.
* note this may seem counter-intuitive at first, but the usual use of this
* is from a tilting coordinate system (detector) to a fixed one (sky).
* In this case deltaq is the relative rotation with respect to the
* fixed reference attitude
*
* The equations used in this function can be derived in the following way.
* First, calculate the three dimensional unit vector direction 
* corresponding to a set of coordinates in the tangent plane. 
* The trick here is that by convention the X axis in the tangent plane points 
* in the opposite direction of the X axis in 3-D space. 
* Next, rotate this vector by the inverse of
* deltaq and convert the resulting unit vector back to tangent plane
* coordinates. The resulting transformation becomes linear in the limit
* where -m20*(oldx-oldx0)/pixels_per_radian +
*        m21*(oldy-oldy0)/pixels_per_radian   << m22
* which is true if the sin of the "theta" Euler angle corresponding to deltaq
* is small or if the event is near the tangent point in radians.
* For typical applications both conditions are met. 
* 
* This function returns the cosine of the angle between the z axes of the 
* old and new coordinate systems. That value can be used to detect absurdly
* large tilt angles (e.g. >= 90 degrees ).
* 
****************************************************************************/
double convertQuatToXform2d(XFORM2D* xform, QUAT* deltaq,
                            double oldx0, double oldy0, 
                            double newx0, double newy0,
                            double pixels_per_radian);

/*****************************************************************************
******************************************************************************
* This is the inverse of convertQuatToXform2d. It takes an arbitrary
* two dimensional transform from one tangent plane coordinate
* system to another, and gives the quaternion representing the
* relative orientation of these two coordinate systems.
* Note that an arbitrary 2 dimensional transform may not correspond
* exactly to any quaternion. For instance xform may contain extra
* scaling. In this case the function does the best it can to
* return a quaternion which corresponds to a 2-d transform which is
* similar to xform. The resulting quaternion is guaranteed to be correctly
* normalized for any input xform.
******************************************************************************/
void convertXform2dToQuat(QUAT* q, XFORM2D* xform, double oldx0, double oldy0, 
                                                   double newx0, double newy0,
                          double pixels_per_radian);


/***************************************************************************
****************************************************************************
* for a pointing defined by the quaternion q, determine the three unit
* vectors, xhat, yhat, and zhat which define the plane tangent to the line 
* of sight.
* The vector xhat points in the negative "R.A." direction, yhat in the positive
* "Dec" direction, and zhat points along the line of sight.
* 
* At the pole xhat points along the meridian (R.A.=0).
***************************************************************************/
void getPlaneTangentToQuat(QUAT* q, 
                           double xhat[3], double yhat[3], double zhat[3] );

/*****************************************************************************
******************************************************************************
* Given a set of points (x0,y0), find the linear transform which moves
* the points closest to (x1,y1). The linear transform is restricted to 
* be a transform corresponding to a three dimensional rotation between
* two tangent plane coordinate systems.
* "Closeness" is defined in the same way as for findBestRigidXform2d.
* And the linear transform is defined as in convertQuatToXform2d.
*
* The function returns the error in the transform, and sets q to the
* three dimensional rotation and xform to the corresponding 2 dimensional
* transformation. Using NULL for wgt is the same as setting all weights
* to unity.
*
* We use an iterative method where we find the closest rigid transform,
* then find the "quat" transform which approximates the rigid transform,
* and then repeat this proceedure on the residuals left from the
* previous approximation. The routine converges rapidly for small
* transforms. It begins to diverge if the Euler angles corresponding to
* q have theta > about 40 degrees, but even then +it will give a reasonable
* approximation if you restrict max_iterations to only a few iterations.
* There is probably a way to solve for q directly without iterating, but
* this method works in practical cases and who needs all that algebra?
* 
******************************************************************************/
double findBestQuatXform2d(QUAT* q, XFORM2D* xform, double x0[], double y0[], 
                                                  double x1[], double y1[], 
                           double wgt[], int npoints,
                           double oldx0, double oldy0, 
                           double newx0, double newy0, 
                           double pixels_per_radian, 
                           double tolerance, int max_iterations);

/**************************
 * Print a Quat to a stream.
 **************************/
void printQuat(QUAT* q, FILE* stream);

/*******************************************************
 * Set a null QUAT (all elements zero, not normalized) *
 *******************************************************/
void setNullQuat(QUAT* q);

/********************************************************************************
 * Determine if a QUAT is null (all elements essentially zero, not normalized). *
 ********************************************************************************/
int isQuatNull(QUAT* q);

/* Linearly interpolate the attitude quaternion given a pair of 
 * bracketing quaternions.  The independent variable (t in q(t))
 * is called time in this function, but it could be any 
 * floating-point quantity. The half-angle calculation is 
 * performed using its sine rather than its cosine because
 * the sine retains more numerical precision, and this is needed
 * for interpolating within small-angle rotations. */
void interpolateQuat
(
 QUAT* q,      /* Desired interpolated quaternion at a time t (output) */
 QUAT* q0,     /* Earlier bracketing quaternion at time t0 (input) */
 QUAT* q1,     /* Later bracketing quaternion at time t1 (input) */
 QUAT* dq,     /* Quaternion of change from q0 to q1 (input) */
 QUAT* dq_int, /* Quaternion of change from q0 to interpolated q (output) */
 double tfrac  /* Time fraction, tfrac = (t - t0)/(t1 - t0) */
 );

#define QUAT_INCLUDED
#endif /* QUAT_INCLUDED */


