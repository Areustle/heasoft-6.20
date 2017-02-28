/* $Id: teldef.h,v 1.12 2007/05/07 16:45:31 ishisaki Exp $ */
/* teldef.h

	1999-09-02	Y.ISHISAKI	version 1.0
		almost rewrite from teldef.h by Ed Pier
*/

#ifndef _TELDEF_H_
#define _TELDEF_H_

/***************************************************************************
* The COORDDEF structure holds information defining a single coordinate
* system.
***************************************************************************/
typedef struct coorddef {
	char name[64];		/* name of this coordinates, eg. 'RAW','DET' */
	int xsiz, ysiz;		/* x/y pixel size */
	int xpix1, ypix1;	/* x/y first pixel number */
	double xcen, ycen;	/* x/y center, calculated from xsiz & xpix1 */
	double xscl, yscl;	/* x/y scaling factor for pixel -> 'unit' */
	char xcol[64], ycol[64];	/* x/y column name in event file */
	char xunit[64], yunit[64];	/* physical unit of coordinates */
} COORDDEF;


/*****************************************************************************
* A TELDEF structure defines the coordinate systems used for a given
* instrument.
*****************************************************************************/

#include "aste_teldef.h"	/* definition of TELDEF_ASTROE */

/* general definition of teldef */

typedef struct teldef {
/* common keywords */
	char telescop[64];	/* copy of teldef header, eg, 'ASTRO-E' */
	char instrume[64];	/* copy of teldef header, eg, 'XRS' */
	char filename[80];	/* copy of teldef header */
	char date[64];		/* copy of teldef header */
	int version;		/* copy of teldef header */
/* definition of coordinates */
	int ncoords;		/* number of coordinates defined in teldef */
	COORDDEF *coord;	/* allocated array of coordinate definition */
/* for internal use */
	char *actual_filename;	/* set from init function argument */
	int id;			/* id number for internal use */
	void *p;		/* arbitrary pointer for internal use */
/* mission dependent information */
	union teldef_of_missions {
		TELDEF_ASTROE *aste;
	} mission;
} TELDEF;

#endif
