/*
 * $Source: /headas/headas/swift/uvot/lib/uvotcal/uvotquality.h,v $
 * $Revision: 1.4 $
 * $Date: 2010/05/27 14:26:49 $
 *
 * $Log: uvotquality.h,v $
 * Revision 1.4  2010/05/27 14:26:49  rwiegand
 * *** empty log message ***
 *
 * Revision 1.3  2005/05/24 20:53:21  rwiegand
 * Moved CALDB interface from UVOT specific to attitude library.
 *
 * Revision 1.2  2005/03/04 19:38:19  rwiegand
 * Relocated functions for loading bad pixel table and applying it to an
 * image to library.
 *
 * Revision 1.1  2004/11/01 16:30:08  rwiegand
 * Added definitions of UVOT quality flags.
 *
 */

#ifndef UVOTQUALITY_H
#define UVOTQUALITY_H


typedef enum
{
	QUALITY_GOOD,
	QUALITY_DEAD    = 1,
	QUALITY_COLD    = 2,
	QUALITY_HOT     = 4,
	QUALITY_FLICK   = 8,       /* flickering */
	QUALITY_DAMAGE  = 128,     /* compression damage */
	QUALITY_NULL    = 256,     /* NULL value */

	QUALITY_TERMINATOR

} UVOTQuality;


typedef short QualityPrimitive;

typedef struct
{
	int count;
	char path[1024];

	int *x;
	int *yTop;
	int *yLength;
	int *reason;
	double *time;

} BadPixelList;



int load_bad_pixel_list (BadPixelList * badpixels, const char * path);

void release_bad_pixel_list (BadPixelList * badpixels);


#endif
