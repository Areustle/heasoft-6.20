/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Match.c,v $
 * $Revision: 1.10 $
 * $Date: 2010/07/26 04:38:00 $
 * 
 * $Log: Match.c,v $
 * Revision 1.10  2010/07/26 04:38:00  rwiegand
 * The tool now keeps track of two sorts of ambiguous matches: those which
 * are slightly better or worse in terms of distance relative to the primary
 * group, and those which do not yield a unique mapping between observed and
 * reference objects.  During the match filtering process, the hope is that
 * some ambiguities will resolve themselves allowing unambiguous matches to
 * be identified.
 *
 * Revision 1.9  2010/07/22 21:12:18  rwiegand
 * Optionally direct match reference objects against detections.  Provide
 * alternate match filtering algorithm (filter.mode=BASE_COUNT_RANGE).
 * Make Group goodness depend on number, intensity of matches, and distance.
 *
 * Revision 1.8  2006/02/22 15:50:56  rwiegand
 * Reworked match disambiguation.  Reverted change that did not not identify
 * which was the observation and which was the reference in a Match.
 *
 * Revision 1.7  2005/10/10 12:14:25  rwiegand
 * Allow reference to observation matches.
 *
 * Revision 1.6  2005/10/09 22:44:31  rwiegand
 * Keep track of ambiguous matches and attempt to discard them if they
 * result in a contradiction.
 *
 * Revision 1.5  2005/08/29 22:56:39  rwiegand
 * Mis-formatting group string.
 *
 * Revision 1.4  2005/08/27 12:51:37  wiegand
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
 * Revision 1.1  2005/08/03 13:05:57  drhunter
 * Initial revision
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Source.h"
#include "Match.h"
#include "MUtil.h"



Match * Match_allocate ()
{
   return calloc(1, sizeof(Match));
}


void Match_construct (Match * m, Source * o, Source * r)
{
   m->obs = o;
   m->ref = r;
   m->count = 1;
   m->weight = 1;
}


Match * Match_create (Source * o, Source * r)
{
   Match * m = Match_allocate();
   Match_construct(m, o, r);
   return m;
}


Match * Match_copy (Match * m)
{
    Match * copy = Match_create(m->obs, m->ref);
	copy->count = m->count;
	copy->weight = m->weight;
    return copy;
}


void Match_deconstruct (Match * m)
{
   m->obs = 0;
   m->ref = 0;
}


double Match_angle (const Match * m)
{
   return Source_angle(m->obs, m->ref);
}


double Match_angle_arcsec (const Match * m)
{
   return 3600 * Math_toDegrees(Match_angle(m));
}


double Match_angleAS (const Match * m)
{
   double angle = Match_angle(m);
   double as100 = 100 * 3600 * Math_toDegrees(angle);

   double rounded = floor(as100 + 0.5) / 100;

   return rounded;
}


int Match_compare (const void * v1, const void * v2)
{
   Match * m1 = *(Match **) v1;
   Match * m2 = *(Match **) v2;

   double angle = Match_angle(m1) - Match_angle(m2);

   return Math_signum(angle);
}


#define UPDATE_GROUP_STRING(i) { \
         Match * m = (Match *) List_get(matches, i); \
         sprintf(s, "%.2f ", Match_angleAS(m)); \
         strcpy(gs + pos, s); \
         pos += strlen(s); \
         }

void Group_toString (const Group * group, const List * matches,
                char * gs, int limit)
{
   int i, pos, longest;
   char s[32];
   Match * last;

   sprintf(gs, "Group [ ]");
   if (group->size == 0)
      return;

   sprintf(gs, "Group (x%.1f) [ ]", group->meanCounts);
   pos = strlen(gs) - 1;

   last = (Match *) List_get(matches, group->first + group->size - 1);
   sprintf(s, "%.2f ", Match_angleAS(last));
   longest = strlen(s);
   if (longest * group->size + 10 > limit) {
      /* can't fit them all, just include first and last n */
      int n = (limit - strlen("Group [ ... ]")) / longest / 2;
      for (i = 0; i < n; ++i)
         UPDATE_GROUP_STRING(group->first + i)
      strcpy(gs + pos, "... ");
      pos += 4;
      for (i = 0; i < n; ++i)
         UPDATE_GROUP_STRING(group->first + group->size - n + i)
   }
   else {
      for (i = 0; i < group->size; ++i)
         UPDATE_GROUP_STRING(group->first + i)
   }

   strcpy(gs + pos, "]");
}


int Group_select (Group * group, const List * matches, int start, int end)
{
   int i, j;
   Match * m0;
   Match * m1;
   double angle0, angle1;

   group->first = 0;
   group->size = 0;
   group->score = 0;

   for (i = start; i < end; ++i) {
      m0 = (Match *) List_get(matches, i);

      angle0 = Match_angle(m0);

      for (j = group->first + group->size + 1; j < end; ++j) {
         m1 = (Match *) List_get(matches, j);

         angle1 = Match_angle(m1);

         if (angle1 - angle0 > group->angle)
            break;
      }

      if (j - i > group->size) {
         group->size = j - i;
         group->first = i;
      }
   }

   group->meanCounts = 0;

   for (i = 0; i < group->size; ++i) {
         Match * m = (Match *) List_get(matches, group->first + i);
         group->meanCounts += m->count;
   }

   if (group->size > 0) {
      group->meanCounts /= group->size;

      m0 = (Match *) List_get(matches, group->first);
      angle0 = Match_angleAS(m0);
      group->score = (group->size + sqrt(group->meanCounts))
            / sqrt(1 + angle0);
   }

   return group->size;
}

