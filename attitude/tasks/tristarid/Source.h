/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Source.h,v $
 * $Revision: 1.9 $
 * $Date: 2010/07/26 04:38:00 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Source.h,v $
 * Revision 1.9  2010/07/26 04:38:00  rwiegand
 * The tool now keeps track of two sorts of ambiguous matches: those which
 * are slightly better or worse in terms of distance relative to the primary
 * group, and those which do not yield a unique mapping between observed and
 * reference objects.  During the match filtering process, the hope is that
 * some ambiguities will resolve themselves allowing unambiguous matches to
 * be identified.
 *
 * Revision 1.8  2006/02/22 15:50:57  rwiegand
 * Reworked match disambiguation.  Reverted change that did not not identify
 * which was the observation and which was the reference in a Match.
 *
 * Revision 1.7  2005/10/10 12:14:25  rwiegand
 * Allow reference to observation matches.
 *
 * Revision 1.6  2005/08/27 12:52:34  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.5  2005/08/09 19:48:36  drhunter
 * Considering final.
 *
 * Revision 1.4  2005/08/09 17:44:19  drhunter
 * Update allows a single observation to be matched to multiple reference
 * objects in the first phase.
 *
 * Revision 1.3  2005/08/03 16:53:57  drhunter
 * Code changes to reflect new Java code. Still no many-to-many matching.
 * Significant differences in MAX_DELTA.
 *
 * Revision 1.2  2005/08/03 13:07:35  drhunter
 * Tristarid as of 9:00 AM, 8/3/05.
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#ifndef TRISTARID_SOURCE_H
#define TRISTARID_SOURCE_H

#include "List.h"


typedef struct Source_t
{
   char *id;
   double ra;
   double dec;
   float mag;
   char *type;
   int observation;

   double unit[3];
   List *proposed;
   int locked;

   struct Source_t *assigned;
} Source;


static const float MAG_NULL = 99;


Source * Source_create (char * id,
		double ra, double dec, float mag, char * type);

void Source_construct (Source * s , char * id,
		double ra, double dec, float mag, char * type);

void Source_deconstruct (Source * s);

void Source_deallocate (Source * s);

double Source_cosangle (const Source * s1, const Source * s2);

double Source_angle (const Source * s1, const Source * s2);
double Source_angle_arcsec (const Source * s1, const Source * s2);

void Source_correct (Source * s, double v[3]);

void Source_lock (Source * s);

int Source_assign (Source * s, Source * s1);

void Source_resign (Source * s);
int Source_reject (Source * s, Source * o);

int Source_isMagNull (Source * s);

int Source_compare (const void * v1, const void * v2);

int Source_propose (Source * s, Source * o);


#endif

