/*
 * $Source: /headas/headas/attitude/tasks/aberrator/tool.h,v $
 * $Revision: 1.1 $
 * $Date: 2005/01/30 03:15:12 $
 *
 * $Log: tool.h,v $
 * Revision 1.1  2005/01/30 03:15:12  rwiegand
 * Fairly generic tidbits.
 *
 */

#ifndef ATTITUDE_TOOL_H
#define ATTITUDE_TOOL_H


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
