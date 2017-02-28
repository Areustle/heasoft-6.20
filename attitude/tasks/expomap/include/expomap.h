#ifndef EXPOMAP_H
#define EXPOMAP_H

/*
 * $Source: /headas/headas/attitude/tasks/expomap/include/expomap.h,v $
 * $Revision: 1.2 $
 * $Date: 2003/07/18 20:11:21 $
 *
 *
 * $Log: expomap.h,v $
 * Revision 1.2  2003/07/18 20:11:21  rwiegand
 * Relocated to headas/attitude package.  Use coord/image.c support for
 * windowing/binning based on WCS keywords.
 *
 * Revision 1.1  2003/05/14 18:14:44  rwiegand
 * Tool for creating exposure maps from instrument map, good time intervals,
 * and attitude information.
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


enum
{
	EXPOMAP_OK,
	EXPOMAP_SETUP_ERROR,
	EXPOMAP_INPUT_ERROR,
	EXPOMAP_OUTPUT_ERROR,
	EXPOMAP_MEMORY_ERROR,
	EXPOMAP_DUMMY
};


#endif

