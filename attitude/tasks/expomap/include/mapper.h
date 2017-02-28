#ifndef MAPPER_H
#define MAPPER_H

/*
 * $Source: /headas/headas/attitude/tasks/expomap/include/mapper.h,v $
 * $Revision: 1.6 $
 * $Date: 2004/10/17 12:00:24 $
 *
 *	based on Chandra mkexpomap
 *
 * $Log: mapper.h,v $
 * Revision 1.6  2004/10/17 12:00:24  rwiegand
 * Removed obsolete polygons.
 *
 * Revision 1.5  2003/09/04 17:36:07  rwiegand
 * Allow creating exposure maps in detector coordinates.
 *
 * Revision 1.4  2003/07/28 21:13:21  rwiegand
 * Added history and checksum flags.
 *
 * Revision 1.3  2003/07/18 21:14:39  rwiegand
 * Pass in binning parameters instead of trying to determine from WCS keywords
 * and TELDEF information.
 *
 * Revision 1.2  2003/07/18 20:11:21  rwiegand
 * Relocated to headas/attitude package.  Use coord/image.c support for
 * windowing/binning based on WCS keywords.
 *
 * Revision 1.1  2003/05/14 18:14:44  rwiegand
 * Tool for creating exposure maps from instrument map, good time intervals,
 * and attitude information.
 *
 * Revision 1.3  2003/05/14 13:35:30  rwiegand
 * Transfer keywords from input to output.
 *
 * Revision 1.2  2003/05/12 14:15:20  rwiegand
 * Generic exposure map generator.
 * Given an instrument map, good time interval(s) and attitude information,
 * creates an exposure map.
 *
 * Revision 1.1  2003/05/02 18:24:17  rwiegand
 * Exposure map generator
 *
 */


#include "genimage.h"
#include "coordfits.h"
#include "coord.h"
#include "att_iterator.h"



typedef struct
{
	double start;
	double stop;
} GTI;


typedef struct
{
	FImage * exposure;
	FImage * instrument;

	int is_det;
	int gtis;
	GTI * interval;
	double exptime;

	ATTITERATOR * attiter;
	TELDEF * teldef;

	ATTFILE * attfile;

	XFORM2D * inst2exp;
	XFORM2D * exp2inst;
	XFORM2D * inst2tel;
	XFORM2D * tel2exp;
	XFORM2D * alpha;

	int aberration;

	FITSHeader * header;

	int checksum;
	int history;
	int debug;

} ExposureMapper;


typedef struct
{
	double duration;
	QUAT * quat;

	int xmin;
	int xmax;
	int ymin;
	int ymax;

} ExposureDelta;


int initialize_exposure_map (ExposureMapper * mapper, double cx, double cy,
		int width, int height);
int create_sky_exposure_map (ExposureMapper * mapper);
int create_det_exposure_map (ExposureMapper * mapper);
int release_mapper (ExposureMapper * mapper);


#endif

