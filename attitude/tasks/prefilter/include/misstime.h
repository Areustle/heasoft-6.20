#ifndef MISSTIME_H
#define MISSTIME_H

/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/misstime.h,v $
 * $Revision: 1.2 $
 * $Date: 2016/10/25 20:00:23 $
 *
 * Modified from ascalib/src/general/ascatime.h for Swift
 *
 *
 * $Log: misstime.h,v $
 * Revision 1.2  2016/10/25 20:00:23  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.1  2002/06/05 14:23:52  rwiegand
 * Updated time interface for mission independence.
 *
 * Revision 1.1  2002/02/20 14:39:17  miket
 * Merging Bob's original RCS files with HEAdas revisions
 *
 * Revision 1.1  2002/01/28 19:23:26  rwiegand
 * Initial revision
 *
 */


#include "atFunctions.h"         /* AtTime */


#if 0

int setMissionEpoch (const AtTime *epoch);
int readLeapTable (const char *leaptable);

double AtTime_to_missionTime (const AtTime *attime);
int missionTime_to_AtTime (double swifttime, AtTime *attime);


#else


int readLeapTable (const char *leaptable, double mjd0);

/* gets leap seconds from mjd0_base to start seconds later; and the mjd of the next leap after +start */
double leaps_from_to_next(double mjd0_base, double start, double *mjd_next);


#endif


#endif

