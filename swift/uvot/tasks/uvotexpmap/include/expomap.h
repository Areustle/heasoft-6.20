#ifndef EXPOMAP_H
#define EXPOMAP_H

/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotexpmap/include/expomap.h,v $
 * $Revision: 1.2 $
 * $Date: 2003/05/12 14:15:20 $
 *
 *
 * $Log: expomap.h,v $
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

