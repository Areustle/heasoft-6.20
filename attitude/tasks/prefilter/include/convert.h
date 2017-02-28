#ifndef DERIVE_CONVERT_H
#define DERIVE_CONVERT_H

/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/convert.h,v $
 * $Revision: 1.2 $
 * $Date: 2004/02/02 15:54:06 $
 *
 *
 * $Log: convert.h,v $
 * Revision 1.2  2004/02/02 15:54:06  rwiegand
 * Now there is a single vector defining the current pointing derived from
 * the alignment file parameter.
 * There used to be fields defined in terms of the primary spacecraft axis
 * and others in terms of the instrument boresight.
 *
 * Revision 1.1  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 */

double radians_to_degrees (const Constants * constants, double radians);
double degrees_to_radians (const Constants * constants, double degrees);

int vector_to_AtVect (const vector_t * vector, AtVect atvect);
int QUAT_to_AtQuat (const QUAT * coordquat, AtQuat atquat);
int AtQuat_to_QUAT (const AtQuat atquat, QUAT * coordquat);
int AtTime_compare (const AtTime * t1, const AtTime * t2);
double AtTime_to_yyyyddd_fraction (const AtTime * t);
int AtVect_to_AtVect (const AtVect in, AtVect out);
int AtQuat_to_AtQuat (const AtQuat in, AtQuat out);
int string_to_AtTime (const char * s, AtTime * t);
int missionTime_to_AtTime (double x, AtTime * t);


#endif

