#ifndef EXTRACT_H
#define EXTRACT_H

/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotimgrism/include/extract.h,v $
 * $Revision: 1.3 $
 * $Date: 2011/09/12 16:43:12 $
 *
 *
 * $Log: extract.h,v $
 * Revision 1.3  2011/09/12 16:43:12  rwiegand
 * Added parameters that allow user to define overlap required for a non-null
 * output pixel and the value to use for null output pixels.
 *
 * Revision 1.2  2003/05/14 18:05:26  rwiegand
 * Use librew instead of local implementation.
 *
 * Revision 1.1  2003/05/14 13:58:04  rwiegand
 * Interface to ugrism/rectext.
 *
 * Revision 1.1  2003/04/01 22:48:29  rwiegand
 * Initial revision
 *
 */


#include "genimage.h"


typedef struct
{
	DImage * from;
	DImage * to;

	double x0;	/* from.x = x0 + to.x * cos(angle) */
	double y0;	/* from.y = y0 + to.y * sin(angle) */
	double angle;	/* radians */

	double null;	/* value to use for null output pixels */
	double overlap; /* minimum overlap for non-null output pixel */

} Extraction;


int extract_rectangle (Extraction * e);


#endif

