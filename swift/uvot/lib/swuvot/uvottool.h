/*
 * $Source: /headas/headas/swift/uvot/lib/swuvot/uvottool.h,v $
 * $Revision: 1.1 $
 * $Date: 2004/12/02 21:29:45 $
 *
 * $Log: uvottool.h,v $
 * Revision 1.1  2004/12/02 21:29:45  rwiegand
 * A Swift/UVOT tool library.
 *
 */

#ifndef UVOTTOOL_H
#define UVOTTOOL_H


enum
{
	TASK_OK,
	TASK_SETUP_ERROR,
	TASK_INPUT_ERROR,
	TASK_OUTPUT_ERROR,
	TASK_FITS_ERROR,
	TASK_CALDB_ERROR,
	TASK_ENUM_COUNT
};


#define _STRINGIFY0(x) # x
#define _STRINGIFY(x) _STRINGIFY0(x)



#endif
