#ifndef PREFILTER_ITERATE_H
#define PREFILTER_ITERATE_H

/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/iterate.h,v $
 * $Revision: 1.1 $
 * $Date: 2002/12/06 20:14:21 $
 *
 *
 * $Log: iterate.h,v $
 * Revision 1.1  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 */

#include "prefilter.h"


Iterator iterate_context;
Iterator iterate_timestamp_delta;
Iterator iterate_timestamp_report;
Iterator iterate_position_nullify;
Iterator iterate_position_tle;
Iterator iterate_position_atSatPos;
Iterator iterate_position_report;
Iterator iterate_position_validate;
Iterator iterate_attitude_nullify;
Iterator iterate_attitude_attfile;
Iterator iterate_attitude_report;


#endif

