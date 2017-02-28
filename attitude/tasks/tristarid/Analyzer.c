/* 
 * $Source: /headas/headas/attitude/tasks/tristarid/Analyzer.c,v $
 * $Revision: 1.11 $
 * $Date: 2010/07/26 21:29:23 $
 * 
 *	Triplet based star identification algorithm
 * 
 * $Log: Analyzer.c,v $
 * Revision 1.11  2010/07/26 21:29:23  rwiegand
 * Ugh- matches which did not pass the grouping phase were not being rejected.
 *
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
 * Revision 1.8  2006/02/22 15:46:18  rwiegand
 * Depend on Source's knowledge of what Matches they are part of instead
 * of SourceMap.
 *
 * Revision 1.7  2005/10/10 12:14:25  rwiegand
 * Allow reference to observation matches.
 *
 * Revision 1.6  2005/09/16 12:13:52  rwiegand
 * Tweaked some reported messages.
 *
 * Revision 1.5  2005/08/29 12:30:47  wiegand
 * Added Stats object.
 *
 * Revision 1.4  2005/08/27 12:49:54  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:05:57  drhunter
 * Tristarid as of 9:00 AM, 8/3/05
 * 
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 * 
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "report.h"
#include "Analyzer.h"
#include "Match.h"
#include "MUtil.h"
#include "SUtil.h"



static void Stats_zero (Stats * s)
{
	s->count = 0;
	s->sum = 0;
	s->sum2 = 0;

	s->min = 0;
	s->max = 0;
	s->median = 0;

	s->mean = 0;
	s->sigma = 0;
	s->rms = 0;
}


Analyzer * Analyzer_create (const char * tag)
{
	Analyzer * a = calloc(1, sizeof(Analyzer));

	Analyzer_reset(a, tag);

	return a;
}


void Analyzer_reset (Analyzer * a, const char * tag)
{
	if (a->tag)
		free(a->tag);

	a->tag = string_copy(tag);

	Stats_zero(&a->stats);
}


void Analyzer_deallocate (Analyzer * a)
{
	if (!a)
		return;
	free(a->tag);
	free(a);
}


static void update (Analyzer * a, Match * m)
{
	Stats * s = &a->stats;
	Source * obs = m->obs;
	Source * ref = m->ref;
	double d = 3600 * Math_toDegrees(Source_angle(obs, ref));
	++s->count;
	s->sum += d;
	s->sum2 += d * d;
	if (d < s->min)
		s->min = d;
	if (d > s->max)
		s->max = d;

	report_status("%s to %s x %d at %.3f [arcsec]\n",
			obs->id, ref->id, m->count, d);
}


int Analyzer_analyze (Analyzer * a, List * matches)
{
	int i, unique;
	Stats * s = &a->stats;

	Stats_zero(s);

	List_sort(matches, Match_compare);

	report_status("--- %s analysis\n", a->tag);
	unique = 1;

	for (i = 0; i < List_size(matches); ++i) {
		Match * m = (Match *) List_get(matches, i);
		update(a, m);
		if (m->obs->assigned != m->ref) {
			unique = 0;
			report_warning("%s is assigned to %s and %s\n",
					m->obs->id, m->obs->assigned->id, m->ref->id);
		}
		if (m->ref->assigned != m->obs) {
			report_warning("%s is assigned to %s and %s\n",
					m->ref->id, m->ref->assigned->id, m->obs->id);
		}
	}

	report_status("%s source map is%s unique\n",
			a->tag, (unique ? "" : " not"));

	if (s->count > 0) {
		s->mean = s->sum / s->count;
		s->rms = sqrt(s->sum2 / s->count);
		s->sigma = sqrt((s->sum2 - s->mean * s->sum) / s->count);
	}

	if (List_size(matches) > 0) {
		double radians;
		Match * median0 = (Match *) List_get(matches, List_size(matches) / 2);
		if (List_size(matches) % 2)
			radians = Match_angle(median0);
		else {
			Match * median1 = (Match *) List_get(matches, List_size(matches) / 2 - 1);
			radians = (Match_angle(median0) + Match_angle(median1)) / 2;
		}
		s->median = 3600 * Math_toDegrees(radians);
	}

	report_status("count %d\n", s->count);
	report_status("mean %s %.3f [arcsec]\n", a->tag, s->mean);
	report_status("rms %s %.3f [arcsec]\n", a->tag, s->rms);
	report_status("sigma %s %.3f [arcsec]\n", a->tag, s->sigma);

	report_verbose("median %s %.3f [arcsec]\n", a->tag, s->median);

	report_status("--- end\n");

	return unique && s->count > 0;
}

