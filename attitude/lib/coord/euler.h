/*****************************************************************************
* this is a data structure and set of functions for handling a set of z-y-z
* Euler angles
*
* 2013-03-07: T. Reichard - Added printEuler and printEulerDeg functions.
*****************************************************************************/
#ifndef EULER_INCLUDED

#include <stdlib.h>

#include "rotmatrix.h"

#define EULER_ROUNDOFF_ERR 1e-15

/***************************************************************************
****************************************************************************
* structure containing euler angles.
***************************************************************************/
typedef struct {

double phi;   /* first rotation  (longitude) */
double theta; /* second rotation (latitude)  */
double psi;   /* third rotation  (twist)     */

} EULER;

/***************************************************************************
****************************************************************************
* allocate storage for a set of euler angles
***************************************************************************/
EULER* allocateEuler(void);


/***************************************************************************
****************************************************************************
* free storage for a set of euler angles
***************************************************************************/
void destroyEuler(EULER* e);

/***************************************************************************
****************************************************************************
* set the phi, theta and psi values of euler angles.
***************************************************************************/
void setEulerAngles(EULER* e, double phi, double theta, double psi);


/***************************************************************************
****************************************************************************
* Convert a set of Euler angles to R.A. Dec. and Roll angle in decimal degrees
* Note that this is just another way of specifying the same coordinates,
* except for the units, 
* and 0<= RA <= 360. , -90 <= Dec <= 0. and 0. <= roll <= 360.
***************************************************************************/
/*
void convertEulerToRADecRoll(EULER* e, double* ra, double* dec, double* roll);
*/

/***************************************************************************
****************************************************************************
* Convert R.A., Dec. and Roll angles in decimal degrees
* tp a set of Euler angles.
* Note that this is just another way of specifying the same coordinates,
* except for the units, 
* and 0<= RA <= 360. , -90 <= Dec <= 0. and 0. <= roll <= 360.
***************************************************************************/
/*
void convertRADecRollToEuler(EULER* e, double ra, double dec, double roll);
*/


/***************************************************************************
****************************************************************************
* convert a direction cosine rotation matrix to a set of euler angles
***************************************************************************/
void convertRotMatrixToEuler(EULER* e, ROTMATRIX* rot);

/*****************************************************************************
******************************************************************************
* determine the direction cosine rotation matrix corresponding to
* a given set of Euler angles.
******************************************************************************/
void convertEulerToRotMatrix(ROTMATRIX* rot, EULER* e);


/***************************************************************************
****************************************************************************
* for a pointing defined by Euler angles, e, determine the three unit
* vectors, xhat, yhat, and zhat which define the plane tangent to the line 
* of sight.
* The vector xhat points in the negative "R.A." direction, yhat in the positive
* "Dec" direction, and zhat points along the line of sight.
* 
* At the pole xhat points along the meridian (R.A.=0).
***************************************************************************/
void getPlaneTangentToEuler(EULER* e, 
                            double xhat[3], double yhat[3], double zhat[3]);


/* Print an Euler angle trio in radians to a stream. */
void printEuler(EULER* e, FILE* stream);


/* Print an Euler angle trio in degrees to a stream. */
void printEulerDeg(EULER* e, FILE* stream);

#define EULER_INCLUDED
#endif /* EULER_INCLUDED */
