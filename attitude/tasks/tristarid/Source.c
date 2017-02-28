/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Source.c,v $
 * $Revision: 1.9 $
 * $Date: 2010/07/26 04:38:00 $
 * 
 *	Triplet based star identification algorithm
 *
 * $Log: Source.c,v $
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
 * Revision 1.6  2005/08/27 12:52:37  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.5  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.4  2005/08/09 17:43:56  drhunter
 * Update allows a single observation to be matched to multiple reference
 * objects in the first phase.
 *
 * Revision 1.3  2005/08/03 16:53:57  drhunter
 * Code changes to reflect new Java code. Still no many-to-many matching.
 * Significant differences in MAX_DELTA.
 *
 * Revision 1.2  2005/08/03 13:05:57  drhunter
 * Tristarid as of 9:00 AM, 8/3/05
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Source.h"
#include "Match.h"
#include "MUtil.h"
#include "report.h"


Source * Source_create (char * id, double ra, double dec, float mag, char * type)
{
	Source * s = calloc(1, sizeof(Source));
	Source_construct(s, id, ra, dec, mag, type);
	return s;
}


void Source_construct (Source * s, char * id,
		double ra, double dec, float mag, char * type)
{
	s->id = id;
	s->ra = ra;
	s->dec = dec;
	s->mag = mag;
	s->type = type;
	s->assigned = 0;
	s->locked = 0;
	s->proposed = 0;

	Math_rd2unit(s->ra, s->dec, s->unit);
}


void Source_deconstruct (Source * s)
{
	free(s->id);
	s->id = 0;
	free(s->type);
	s->type = 0;
	s->assigned = 0;
	if (s->proposed)
		List_deallocate(s->proposed);
	s->proposed = 0;
}


void Source_deallocate (Source * s)
{
	Source_deconstruct(s);
	free(s);
}


double Source_cosangle (const Source * s1, const Source * s2)
{
	return Math_u3cosangle(s1->unit, s2->unit);
}


double Source_angle (const Source * s1, const Source * s2)
{
	return Math_u3angle(s1->unit, s2->unit);
}


double Source_angle_arcsec (const Source * s1, const Source * s2)
{
	return 3600 * Math_toDegrees(Source_angle(s1, s2));
}


void Source_correct (Source * s, double v[3])
{
	s->unit[0] = v[0];
	s->unit[1] = v[1];
	s->unit[2] = v[2];
}


int Source_assign (Source * s, Source * o)
{
	s->assigned = o;
	s->locked = 1;
	return 1;
}


void Source_resign (Source * s)
{
	if (s->proposed)
		List_clear(s->proposed);
	s->assigned = 0;
	s->locked = 0;
}


int Source_isMagNull (Source * s)
{
	return s->mag == MAG_NULL;
}


int Source_compare (const void * v1, const void * v2)
{
	Source *s1 = *(Source **)v1;
	Source *s2 = *(Source **)v2;
	return Math_signum(s1->mag - s2->mag);
}


static Match * Source_propose0 (Source * s, Source * o)
{
	int i;
	Match * out = NULL;

	if (!s->proposed)
		s->proposed = List_create(4, 0);

	for (i = 0; i < List_size(s->proposed); ++i) {
		Match * m = (Match *) List_get(s->proposed, i);
		Source * t = s->observation ? m->ref : m->obs;
		if (t == o)
			out = m;
	}

	if (out != NULL)
		++out->count;
	else {
		out = s->observation ? Match_create(s, o) : Match_create(o, s);
		List_push(s->proposed, out);
	}

	return out;
}


int Source_propose (Source * s, Source * o)
{
	Match * m = Source_propose0(s, o);
	Source_propose0(o, s);
	return m->count == 1;
}


void Source_proposeMatch (Source * s, Match * m)
{
	List_push(s->proposed, m);
}


int Source_reject (Source * s, Source * o)
{
	int i;

	if (s->assigned == o) {
		s->assigned = 0;
		s->locked = 0;
		report_verbose("Source_reject: %s leaving %s\n", s->id, o->id);
	}
	else
		report_verbose("Source_reject: %s did not have %s\n", s->id, o->id);

	for (i = 0; i < List_size(s->proposed); ++i) {
		Match * m = (Match *) List_get(s->proposed, i);
		Source * t = s->observation ? m->ref : m->obs;
		if (t == o) {
			List_remove(s->proposed, m);
			return 1;
		}
	}

	return 0;
}

