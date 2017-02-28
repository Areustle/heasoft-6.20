#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#include "pointing.h"

/* Allocate a POINTING structure. */
POINTING* allocatePointing()
{
  POINTING* p;

  p = (POINTING*)malloc(sizeof(POINTING));

  return p;
}

/* Deallocate a POINTING structure. */
void destroyPointing(POINTING* p)
{
  if(p != NULL)
    free(p);
}

/* Change ra or roll by a multiple of 360 deg. if either is outside
 * the range [0, 360).  Report an error and quit if dec is outside
 * the range [-90, 90] by more than the round-off error. */
void maintainPointing(POINTING* p)
{
  long n_periods = 0;

  /* Report an error if dec is outside the range [-90, 90]. */

  if(p->dec > 90. + POINTING_ROUNDOFF_ERR ||
     p->dec < -90. - POINTING_ROUNDOFF_ERR)
    badPointingError(p);

  /* Put ra in the range [0, 360). */

  if(p->ra < 0.)
    {
      n_periods = floor(-p->ra/360.) + 1;
      p->ra += n_periods * 360.;
    }
  if(p->ra >= 360.)
    {
      n_periods = floor(p->ra/360.);
      p->ra -= n_periods * 360.;
    }

  /* Put roll in the range [0, 360). */

  if(p->roll < 0.)
    {
      n_periods = floor(-p->roll/360.) + 1;
      p->roll += n_periods * 360.;
    }
  if(p->roll >= 360.)
    {
      n_periods = floor(p->roll/360.);
      p->roll -= n_periods * 360.;
    }
}

/* Set a pointing structure with values of ra, dec, and roll. Put ra 
 * and roll into their allowed range [0, 360) if they are outside this range. */
void setPointing(POINTING* p, double ra, double dec, double roll)
{
  p->ra = ra;
  p->dec = dec;
  p->roll = roll;

  maintainPointing(p);
}

/* Display an error about out-of-range declination and quit. */
void badPointingError(POINTING* p)
{
  fprintf(stderr, "Out-of-range declination in pointing: ");
  printPointing(p, stderr, 1);
  fprintf(stderr, "\n");

  exit(1);
}

/* Convert the pointing (degrees) to ZYZ Euler angles (radians)
 * using the roll angle convention and alignment matrix of
 * the ALIGN structure. */
void convertPointingToEuler(EULER* e, POINTING* p, ALIGN* a)
{
  QUAT* q = allocateQuat();

  convertPointingToQuat(q, p, a);
  convertQuatToEuler(e, q);

  destroyQuat(q);
}

/* Convert the ZYZ Euler angles (radians) to pointing (degrees)
 * using the roll angle convention and alignment matrix of
 * the ALIGN structure. */
void convertEulerToPointing(POINTING* p, EULER* e, ALIGN* a)
{
  QUAT* q = allocateQuat();

  convertEulerToQuat(q, e);
  convertQuatToPointing(p, q, a);

  destroyQuat(q);
}

/* Convert the pointing (degrees) to a quaternion
 * using the roll angle convention and alignment matrix of
 * the ALIGN structure. */
void convertPointingToQuat(QUAT* q, POINTING* p, ALIGN* a)
{
  convertRADecRollToQuat(a, q, p->ra, p->dec, p->roll);
}

/* Convert the quaternion to a pointing (degrees)
 * using the roll angle convention and alignment matrix of
 * the ALIGN structure. */
void convertQuatToPointing(POINTING* p, QUAT* q, ALIGN* a)
{
  convertQuatToRADecRoll(a, q, &(p->ra), &(p->dec), &(p->roll));
  maintainPointing(p);
}

/* Print the pointing components to a stream. Set with_labels to
 * a nonzero value to label the values or to 0 to print only the values. */
void printPointing(POINTING* p, FILE* stream, int with_labels)
{
  if(with_labels)
    fprintf(stream, "(R.A.=%.15g Dec.=%.15g Roll=%.15g)", p->ra, p->dec, p->roll);
  else
    fprintf(stream, "(%.15g %.15g %.15g)", p->ra, p->dec, p->roll);
}
/* Revision log:
 * $Log: pointing.c,v $
 * Revision 1.1  2013/11/21 16:27:46  treichar
 * Added the POINTING structt, which contains ra, dec, and roll.  Added functions to create, destroy, and manipulate this struct and to convert
 * it to and from the QUAT and EULER struct using an ALIGN struct.  These additions put POINTING on an equal footing with QUAT and
 * EULER for use in the attconvert tool, which converts between these 3 representations of attitude.
 *
 */
