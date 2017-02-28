/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/tle.h,v $
 * $Revision: 1.5 $
 * $Date: 2003/07/21 15:33:23 $
 *
 * $Log: tle.h,v $
 * Revision 1.5  2003/07/21 15:33:23  rwiegand
 * Apply rotation from TEME (SGP coordinate system) to TOD J2000 in
 * tle_to_position.  Allow text TLE files to have blanks in international
 * designator, second derivative of motion and bstar fields.
 *
 * Revision 1.4  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 * Revision 1.3  2002/11/26 20:13:20  rwiegand
 * Corrected problem with duplicate final record in output.  Made TLE input
 * more robust.
 *
 * Revision 1.2  2002/03/15 16:28:17  rwiegand
 * Added flags for TLE propagation to enforce SGP or SDP modelling for input
 *
 * Revision 1.1  2002/02/20 14:39:17  miket
 * Merging Bob's original RCS files with HEAdas revisions
 *
 * Revision 1.4  2002/01/28 19:19:16  rwiegand
 * Added vector utility functions
 *
 * Revision 1.3  2001/11/06 16:32:41  rwiegand
 * Updated labelling convention
 *
 * Revision 1.2  2001/11/01 15:44:32  rwiegand
 * Added debug flag
 *
 * Revision 1.1  2001/11/01 14:45:55  rwiegand
 * Initial revision
 *
 */

#ifndef SWIFT_TLE_H
#define SWIFT_TLE_H 1

#include <stdio.h>

#include "datetime.h"


enum
{
  TLE_BAD_FILE = 1,
  TLE_NOT_FOUND,
  TLE_BAD_FORMAT,
  TLE_DUMMY
};


/* flags for tle_to_xxx */
#define TLE_VERBOSE   0x0001
#define TLE_DEBUG     0x0002

#define TLE_SHALLOW   0x0010
#define TLE_DEEPSPACE 0x0020

#define TLE_UNNAMED   0x0100 /* do not read identifier line with TLE */
#define TLE_NO_CHECKSUM  0x0200 /* do not read identifier line with TLE */


/* two-line-element satellite orbital data */
typedef struct
{
  int raw;
  int deepspace;

  double epoch;
  double xndt2o;
  double xndd6o;
  double bstar;
  double xincl;
  double xnodeo;
  double eo;
  double omegao;
  double xmo;
  double xno;
} tlesgp_t; 



/* vector type stucture */
typedef struct
{
  /* components in three dimensions */
  double x;
  double y;
  double z;

  /* scale factor for x,y,z */
  double scale;
} vector_t;


/* for pretty printing vectors */
typedef struct
{
	const char *header;
	const char *prefix;
	const char *real;     /* number format (%f) */
	const char *postfix;
	FILE *file;
	int scale;
} vector_print_t;


typedef struct
{
  double latitude;     /* radians */
  double longitude;    /* radians */
  double altitude;     /* kilometers */
  double theta;        /* radians */
} geodetic_t; 



/*
 * note that the vector_t class keeps both x,y,z components and a scale factor
 *
 * unitize updates a vector to have scale 1 and x,y,z are unit vector
 * normalize updates a vector so its x,y,z components are a unit vector
 * denormalize updates a vector to have scale 1, but expands x,y,z
 */
void vector_unitize (vector_t *v);
void vector_normalize (vector_t *v);
void vector_denormalize (vector_t *v);

void vector_scale (vector_t *v, double scale);
void vector_scale_x (const vector_t *in, vector_t *out, double scale);

void vector_print (const vector_t *v, FILE *fp, const char *header);

void vector_print_x (const vector_t *v, const vector_print_t *args);


int tle_to_geodetic(const tlesgp_t *tle, const datetime_t *epoch,
        geodetic_t *geodetic, int flags);

int tle_to_position(const tlesgp_t *tle, const datetime_t *epoch,
        vector_t *position, vector_t *velocity, int flags);

void print_tle(FILE *fp, const tlesgp_t *tle, const char *header);


#endif
