#ifndef MAPPER_H
#define MAPPER_H

/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotexpmap/include/mapper.h,v $
 * $Revision: 1.3 $
 * $Date: 2003/05/14 13:35:30 $
 *
 *	based on Chandra mkexpomap
 *
 * $Log: mapper.h,v $
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



#ifndef GTI_TYPE
#define GTI_TYPE float
#endif

typedef struct
{
	GTI_TYPE start;
	GTI_TYPE stop;
} GTI;



typedef struct
{
	FImage * exposure;
	FImage * instrument;

	int gtis;
	GTI * interval;

	AttitudeIterator * attiter;
	TELDEF * teldef;

	ATTFILE * attfile;
	XFORM2D * sky2det;

	int aberration;

	FITSHeader * header;

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


int initialize_exposure_map (ExposureMapper * mapper, double ra, double dec);
int create_exposure_map (ExposureMapper * mapper, double ra, double dec);


#endif

