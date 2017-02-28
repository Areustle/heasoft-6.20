#ifndef EXPIO_H
#define EXPIO_H

/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotexpmap/include/expio.h,v $
 * $Revision: 1.2 $
 * $Date: 2003/05/12 14:15:20 $
 *
 *	Perform input and output for exposure map generation
 *
 * $Log: expio.h,v $
 * Revision 1.2  2003/05/12 14:15:20  rwiegand
 * Generic exposure map generator.
 * Given an instrument map, good time interval(s) and attitude information,
 * creates an exposure map.
 *
 * Revision 1.1  2003/05/02 18:24:17  rwiegand
 * Exposure map generator
 *
 */


#include "mapper.h"


int load_good_time_intervals (ExposureMapper * mapper, const char * path);
int load_instrument_map (ExposureMapper * mapper, const char * path);
int load_bad_pixel_map (ExposureMapper * mapper, const char * path);
int load_telescope_definition (ExposureMapper * mapper, const char * path);
int initialize_attitude (ExposureMapper * mapper, const char * path,
				double attdelta);
int write_exposure_map (ExposureMapper * mapper, const char * path);


#endif

