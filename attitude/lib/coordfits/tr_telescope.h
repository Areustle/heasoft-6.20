#ifdef __cplusplus
extern "C" {
#endif

#ifndef TR_TELESCOPE_INCLUDED

#include "xform2d.h"
#include "coorddef.h"
#include "fitsio.h"

/* TR_TELESCOPE structure holds the parameters needed for a basic linear
 * transformation from a “normal” TelDef system to the output frame of the 
 * raytracing program xrtraytrace. */

typedef struct {

  /* Coordinate system identification */

  /* The names of the the four following structure members are kept 
   * for consistency with the other similar TR_XXXX structures, but some 
   * of the meanings have been changed. */

  /* Name of originating coord. sys. */
  char lowcoordsysname[FLEN_VALUE];

  /* Name of destination coord. sys. In this context, this is the system 
   * in which the optical axis is defined, the value of the OPTCOORD 
   * keyword. */

  /* In typical usage, lowcoordsysname = highcoordsysname.*/
  char highcoordsysname[FLEN_VALUE];

  int low_sys; /* Number of originating coord. sys. */
  int high_sys; /* Number of destination coord. sys. */

  /* Basic transformation parameters */

  double xoffset; /* Value of OPTAXISX keyword. */
  double yoffset; /* Value of OPTAXISY keyword. */

  /* Value of xxx_xSCL keyword = scale of the originating
   * coordinate system in mm/pixel. */
  double scale; 

  long xflip; /* Value of OPTFLIPX keyword. */
  long yflip; /* Value of OPTFLIPY keyword. */

  /* Value of OPT_ROTD keyword = clockwise rotation angle between 
   * phi=0 and X axis, in degrees. */
  double rotangle; 

} TR_TELESCOPE;

/* Read the TR_TELESCOPE structure from a TelDef file. */

int readTrTelescope (
  fitsfile* fp, /* TelDef file pointer */
  TR_TELESCOPE** p_telescopeparam, /* Pointer to TR_TELESCOPE structure pointer */
  char* filename); /* TelDef filename */

/* Convert the telescope parameters to an Xform2D transformation. */

XFORM2D* getXform2dFromTrTelescopeParameters (
  COORDDEF* origcoord, /* originating coordinate system structure */
  TR_TELESCOPE* telescopeparam); /* transformation structure */

/* Print the contents of a TR_TELESCOPE structure. */

void printTrTelescope (
  TR_TELESCOPE* basicparam, /* TR_TELESCOPE structure to print */
  FILE* stream); /* destination stream */

/* Free a TR_TELESCOPE structure. */

void destroyTrTelescope (
 TR_TELESCOPE* telescopeparam);  /* TR_BASIC structure to destroy */

#define TR_TELESCOPE_INCLUDED
#endif  /* TR_TELESCOPE_INCLUDED */

#ifdef __cplusplus
} /* end extern "C" */
#endif
