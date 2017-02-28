/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Match.h,v $
 * $Revision: 1.9 $
 * $Date: 2010/07/26 04:38:00 $
 * 
 * $Log: Match.h,v $
 * Revision 1.9  2010/07/26 04:38:00  rwiegand
 * The tool now keeps track of two sorts of ambiguous matches: those which
 * are slightly better or worse in terms of distance relative to the primary
 * group, and those which do not yield a unique mapping between observed and
 * reference objects.  During the match filtering process, the hope is that
 * some ambiguities will resolve themselves allowing unambiguous matches to
 * be identified.
 *
 * Revision 1.8  2010/07/22 21:12:18  rwiegand
 * Optionally direct match reference objects against detections.  Provide
 * alternate match filtering algorithm (filter.mode=BASE_COUNT_RANGE).
 * Make Group goodness depend on number, intensity of matches, and distance.
 *
 * Revision 1.7  2006/02/22 15:50:56  rwiegand
 * Reworked match disambiguation.  Reverted change that did not not identify
 * which was the observation and which was the reference in a Match.
 *
 * Revision 1.6  2005/10/10 12:14:25  rwiegand
 * Allow reference to observation matches.
 *
 * Revision 1.5  2005/10/09 22:44:31  rwiegand
 * Keep track of ambiguous matches and attempt to discard them if they
 * result in a contradiction.
 *
 * Revision 1.4  2005/08/27 12:51:39  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/09 17:41:38  drhunter
 * Update allows a single observation to be matched to multiple reference
 * objects in the first phase.
 *
 * Revision 1.1  2005/08/03 13:07:35  drhunter
 * Initial revision
 *
 */

#ifndef TRISTARID_MATCH_H
#define TRISTARID_MATCH_H

#include "Source.h"
#include "List.h"



typedef struct
{
   Source *obs;
   Source *ref;
   int count;
   float weight;
} Match;


Match * Match_create (Source * o, Source * r);
Match * Match_allocate ();
void Match_construct (Match * m, Source * o, Source * r);
Match * Match_copy (Match * m);

void Match_deconstruct (Match * m);

double Match_angle (const Match * m);
double Match_angle_arcsec (const Match * m);

int Match_compare (const void * v1, const void * v2);

void Source_proposeMatch (Source * s, Match * m);


typedef struct
{
   const char * tag;
   double angle;
   int first;
   int size;
   double meanCounts;
   double score;
} Group;

void Group_toString (const Group * group, const List * matches,
				char * s, int space);

int Group_select (Group * group, const List * matches, int start, int count);



#endif

