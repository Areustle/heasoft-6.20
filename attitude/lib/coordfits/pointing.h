#ifndef POINTING_INCLUDED

#include "euler.h"
#include "quat.h"
#include "align.h"

/* Define a tiny margin of error at the limits of numerical precision
 * in ra, dec, and roll angles. */

#define POINTING_ROUNDOFF_ERR 1.0e-13

/* Define a POINTING structure that holds the right ascension,
 * declination, and roll angle of a telescope pointing. */

typedef struct 
{
  double ra;    /* right ascension in degrees, 0 <= ra < 360 */
  double dec;   /* declination in degrees, -90 <= dec <= 90 */
  double roll;  /* roll angle in degrees, 0 <= roll < 360 */
} POINTING;

/* Allocate a POINTING structure. */
POINTING* allocatePointing();

/* Deallocate a POINTING structure. */
void destroyPointing();

/* Change ra or roll by a multiple of 360 deg. if either is outside
 * the range [0, 360).  Report an error and quit if dec is outside
 * the range [-90, 90] by more than the round-off error. */
void maintainPointing(POINTING* p);

/* Set a pointing structure with values of ra, dec, and roll. Put ra 
 * and roll into their allowed range [0, 360) if they are outside this range. */
void setPointing(POINTING* p, double ra, double dec, double roll);

/* Display an error about out-of-range declination and quit. */
void badPointingError(POINTING* p);

/* Convert the pointing (degrees) to ZYZ Euler angles (radians)
 * using the roll angle convention and alignment matrix of
 * the ALIGN structure. */
void convertPointingToEuler(EULER* e, POINTING* p, ALIGN* a);

/* Convert the ZYZ Euler angles (radians) to pointing (degrees)
 * using the roll angle convention and alignment matrix of
 * the ALIGN structure. */
void convertEulerToPointing(POINTING* p, EULER* e, ALIGN* a);

/* Convert the pointing (degrees) to a quaternion
 * using the roll angle convention and alignment matrix of
 * the ALIGN structure. */
void convertPointingToQuat(QUAT* q, POINTING* p, ALIGN* a);

/* Convert the quaternion to a pointing (degrees)
 * using the roll angle convention and alignment matrix of
 * the ALIGN structure. */
void convertQuatToPointing(POINTING* p, QUAT* q, ALIGN* a);

/* Print the pointing components to a stream. Set with_labels to
 * a nonzero value to label the values or to 0 to print only the values. */
void printPointing(POINTING* p, FILE* stream, int with_labels);

#define POINTING_INCLUDED
#endif /* POINTING_INCLUDED */
