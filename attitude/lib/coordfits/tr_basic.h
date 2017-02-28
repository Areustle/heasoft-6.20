#ifdef __cplusplus
extern "C" {
#endif

#ifndef TR_BASIC_INCLUDED

#include "fitsio.h"


/* TR_BASIC structure holds the parameters needed for a basic linear
 * transformation. */
typedef struct {

  /* Coordinate system identification */

  char lowcoordsysname[FLEN_VALUE]; /* Name of originating coord. sys. */
  char highcoordsysname[FLEN_VALUE]; /* Name of destination coord. sys. */
  int low_sys; /* Number of orig. coord. sys. */
  int high_sys; /* Number of dest. coord. sys. */

  /* Basic transformation parameters */

  double xoffset; /* Value of xxx_XOFF x offset keyword. */
  double yoffset; /* Value of xxx_YOFF y offset keyword. */
  double scale;   /* Value of xxx_SCAL scaling keyword. */
  long xflip;      /* Value of xxxFLIPX x flip keyword. */
  long yflip;      /* Value of xxxFLIPY y flip keyword. */
  double rotangle;    /* Value of xxx_ROTD rotation (deg.) keyword. */
		    
} TR_BASIC;


/* Create a new TR_BASIC structure and read its contents from a TelDef file. */
int readTrBasic /* Returns CFITSIO status */
(
 fitsfile* fp, /* TelDef file pointer */
 TR_BASIC** p_basicparam,  /* Pointer to TR_BASIC structure pointer */
 int is_rawtodet, /* Is the transformation type actually RAWTODET? */
 char* lowcoordsysname,  /* Name of orig. coord. sys. */
 char* highcoordsysname, /* Name of dest. coord. sys. */
 int low_sys, /* Number of orig. coord. sys. */
 char* filename /* TelDef filename */
 );

/* Print a TR_BASIC structure to a stream */
void printTrBasic
(
 TR_BASIC* basicparam, /* TR_BASIC structure to print */
 FILE* stream /* destination stream */
 );

/* Destroy a TR_BASIC structure. */
void destroyTrBasic
(
 TR_BASIC* basicparam /* TR_BASIC structure to destroy */
 );

#define TR_BASIC_INCLUDED
#endif  /* TR_BASIC_INCLUDED */

#ifdef __cplusplus
} /* end extern "C" */
#endif
