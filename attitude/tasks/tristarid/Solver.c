/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Solver.c,v $
 * $Revision: 1.21 $
 * $Date: 2010/07/26 21:29:23 $
 * 
 *	Triplet based star identification algorithm
 *
 * $Log: Solver.c,v $
 * Revision 1.21  2010/07/26 21:29:23  rwiegand
 * Ugh- matches which did not pass the grouping phase were not being rejected.
 *
 * Revision 1.20  2010/07/26 16:24:03  rwiegand
 * Only display invalid triangle message at high chatter.
 *
 * Revision 1.19  2010/07/26 14:07:29  rwiegand
 * Updated version. Store match statistics even if there is no solution.
 *
 * Revision 1.18  2010/07/26 04:38:00  rwiegand
 * The tool now keeps track of two sorts of ambiguous matches: those which
 * are slightly better or worse in terms of distance relative to the primary
 * group, and those which do not yield a unique mapping between observed and
 * reference objects.  During the match filtering process, the hope is that
 * some ambiguities will resolve themselves allowing unambiguous matches to
 * be identified.
 *
 * Revision 1.17  2010/07/22 21:12:18  rwiegand
 * Optionally direct match reference objects against detections.  Provide
 * alternate match filtering algorithm (filter.mode=BASE_COUNT_RANGE).
 * Make Group goodness depend on number, intensity of matches, and distance.
 *
 * Revision 1.16  2007/10/05 19:17:49  rwiegand
 * Added parameters to constrain the separation between sources used for
 * matching and to filter matches.
 *
 * Revision 1.15  2006/03/06 19:04:54  rwiegand
 * Perform a second filtering pass.
 *
 * Revision 1.14  2006/03/06 16:11:39  rwiegand
 * Updated formatting of match list.
 *
 * Revision 1.13  2006/02/22 15:50:56  rwiegand
 * Reworked match disambiguation.  Reverted change that did not not identify
 * which was the observation and which was the reference in a Match.
 *
 * Revision 1.12  2005/11/10 18:40:09  rwiegand
 * Updated documentation.  Point users to the catspec help file.
 *
 * Revision 1.11  2005/10/10 12:14:25  rwiegand
 * Allow reference to observation matches.
 *
 * Revision 1.10  2005/10/09 22:44:31  rwiegand
 * Keep track of ambiguous matches and attempt to discard them if they
 * result in a contradiction.
 *
 * Revision 1.9  2005/08/31 13:26:27  rwiegand
 * misCount test was not accounting for possible equality.
 *
 * Revision 1.8  2005/08/29 22:56:13  rwiegand
 * Mis-reporting size of largest match group.
 *
 * Revision 1.7  2005/08/29 12:41:42  wiegand
 * Added a second round of match filtering after initial correction is
 * applied.  Hang on to Parameters object instead of having local copies
 * of all of them.
 *
 * Revision 1.6  2005/08/27 12:52:27  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.5  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.4  2005/08/09 17:42:59  drhunter
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
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "report.h"

#include "Analyzer.h"
#include "Catalog.h"
#include "Position.h"
#include "QMethod.h"
#include "Solver.h"
#include "Source.h"
#include "Triplet.h"
#include "MUtil.h"

#define DEBUG_TRIPLETS           (1<<7)
#define DEBUG_SOURCE_SELECTION   (1<<8)
#define DEBUG_MATCHING           (1<<9)

#define ASSIGN_AMBIGUOUS         (1<<0)
#define ASSIGN_PROPOSED          (1<<1)
#define ASSIGN_REPORT            (1<<2)
#define ASSIGN_DEFERRED          (1<<3)
#define ASSIGN_EXPAND            (1<<4)

#define EPSILON 1e-6


typedef struct
{
	int iteration;
	const Stats * stats;
	double limit;
} MatchFilter;


typedef struct
{
	int misCount;

/* cube of Triplet pointers (dimensions allocated in constructIndex)
	tripletArchive for easy deallocation of Triplets */
	List * tripletIndex;
	List * tripletArchive;

	int nMinMatches;

	double minDoublet;
	double maxDoublet;
	double minTriplet;
	double maxTriplet;

	double errDoublet;

	Parameters * par;

	double mincosRot;

	double dPrimary;
	double dSecondary;

	List * selected;
	List * proposed;
	List * deferred;

	double qOptimal[4];

	List * triangles;

} Task;


const char *ITERATION[] = {
	"alpha", "beta", "gamma", "delta", "epsilon",
	"zeta", "eta", "theta", "iota", "kappa"
};

static void initialize (Task * task, Parameters * p);
static List * getSources (List * list, int obs, const Parameters * p);

static void assignSource (List * matches, Source * obs, Source * ref, int flags);
static int groupMatches (Task * task);
static int filterMatches (Task * task, MatchFilter * filter);
static void reinstateMatches (Task * task, const MatchFilter * filter);

static void expandMatches (Task * task, List * ref, List * obs);
static void expandSimple (Task * task, Source * obs, List * candidates);
static void expandBest (Task * task, Source * obs, List * candidates);

static void qMethod (Task * task, List * obs);

static int isMagMatch (double tol, Source * a, Source * b);
static int isDoubletMatch (double tol , double a, double b);
static int isTripletMatch (Task * task, Triplet * r, Triplet * o);

static void iterateTriplets (Task * task, List * s,
		int (*apply)(Task * task, Triplet * t));
static void Solver_search (Task * task, List * obs);

static void constructIndex (Task * task);
static void Solver_analyzeIndex (Task * task);

static int addTriplet (Task * task, Triplet * t);
static int findTripletMatches (Task * task, Triplet * t);
static int disambiguateMatches (Task * task);




typedef struct
{
	Triplet * obs;
	Triplet * ref;
} TripletMatch;



void TripletMatch_construct (TripletMatch * tm, Triplet * o, Triplet * r)
{
	tm->obs = o;
	tm->ref = r;
}


TripletMatch * TripletMatch_create (Triplet * o, Triplet * r)
{
	 TripletMatch * tm = malloc(sizeof(TripletMatch));
	 TripletMatch_construct(tm, o, r);
	 return tm;
}


void initialize (Task * task, Parameters * p)
{
	task->par = p;
	task->qOptimal[0] = 0;

	task->minDoublet = Math_toRadians(p->asMinDoublet / 3600);
	task->maxDoublet = Math_toRadians(p->degFoV);

	task->errDoublet = Math_toRadians(p->asErrDoublet / 3600);

	task->minTriplet = Math_toRadians(p->degTriplet);
	task->maxTriplet = Math_toRadians(180 - p->degTriplet);

	task->dPrimary = (task->maxDoublet - task->minDoublet) / p->nPrimary;
	task->dSecondary = (task->maxTriplet - task->minTriplet) / p->nSecondary;

	constructIndex(task);

	task->mincosRot = p->degRotErr < 0 ? -2 : cos(Math_toRadians(p->degRotErr / 60));
}


static void dumpSourceList (const char * tag, const List * list)
{
	 int i;
	 if (!tag)
		tag = "tag";
	 report_debug("source list[%s] count=%d\n", tag, List_size(list));
	 for (i = 0; i < List_size(list); ++i) {
		Source * s = (Source *) List_get(list, i);
		report_debug("\tSource[id=%s, mag=%f]\n", s->id, s->mag);
	 }
}


static void reportMatch (const char * tag, const Match * m)
{
	double as = Match_angle_arcsec(m);
	report_status("%s%s to %s x %d at %.3f [arcsec]\n",
			tag, m->obs->id, m->ref->id, m->count, as);
}

static void dumpMatchList (const char * tag, const List * list, const char * indent)
{
	 int i;
	 if (!tag)
		tag = "tag";
	 if (!indent)
		indent = "\t";
	 report_status("%s [count=%d]\n", tag, List_size(list));
	 for (i = 0; i < List_size(list); ++i) {
		Match * m = (Match *) List_get(list, i);
		reportMatch(indent, m);
	 }
}


static void reportMatches (Task * task, const char * tag)
{
	dumpMatchList("selected", task->selected, 0);
	dumpMatchList("deferred", task->deferred, 0);
}


static void deferMatch (Task * task, const char * tag, Match * m)
{
	List_push(task->deferred, m);
	reportMatch(tag, m);
}


static void rejectMatch (const char * tag, Match * m)
{
	Source_reject(m->obs, m->ref);
	Source_reject(m->ref, m->obs);
	reportMatch(tag, m);
}


static void installMatch (Task * task, const char * tag, Match * m)
{
	Source * obs = m->obs;
	Source * ref = m->ref;

	if (obs->assigned) {
		if (obs->assigned == ref)
			return;
		report_warning("reassigning %s\n", obs->id);
	}

	if (ref->assigned)
		report_warning("reassigning %s\n", ref->id);

	Source_assign(obs, ref);
	Source_assign(ref, obs);

	List_push(task->selected, m);
	reportMatch(tag, m);
}


List * directReferences (Task * task, const List * ref, const List * obs)
{
	Parameters * par = task->par;
	int i, j;
	List * out;
	double mincos;

	out = List_create(List_size(ref), 0);
	mincos = cos(Math_toRadians(par->asDirectReferences / 3600));

	report_status("direct matching ref to within %.3f [arcsec] of obs\n",
			par->asDirectReferences);

	for (i = 0; i < List_size(obs); ++i) {
		Source * o = (Source *) List_get(obs, i);
		for (j = 0; j < List_size(ref); ++j) {
			Source * r = (Source *) List_get(ref, j);
			if (!r->locked && Source_cosangle(o, r) >= mincos) {
				report_verbose("%s satisfies initial direct match\n", r->id);
				r->locked = 1;
				List_push(out, r);
			}
		}
	}

	/* unlock sources */
	for (i = 0; i < List_size(out); ++i) {
		Source * r = (Source *) List_get(out, i);
		r->locked = 0;
	}

	return out;
}


int Solver_solve (Parameters * par, List * ref0, List * obs0)
{
	int i, count, done;
	Analyzer * a = 0;
	double q[4];
	List lists = { 0 };
	List * obs;
	List * ref;
	char buffer[256];
	MatchFilter filter = { 0 };
	Task task = { 0 };

	initialize(&task, par);
	report_status("initialized\n");

	List_construct(&lists, 10, 0);

	obs = getSources(obs0, 1, par);
	List_push(&lists, obs);

	report_status("selected %d observations\n", List_size(obs));
	report_status("starting with %d references\n", List_size(ref0));

	if (par->asDirectReferences > 0) {
		ref = directReferences(&task, ref0, obs);
		List_push(&lists, ref);
	}
	else
		ref = ref0;

	ref = getSources(ref, 0, par);
	List_push(&lists, ref);

	if (par->debug & DEBUG_SOURCE_SELECTION) {
		dumpSourceList("ref", ref);
		dumpSourceList("obs", obs);
	 }

	iterateTriplets(&task, ref, addTriplet);
	report_status("built triangle database\n");

	Solver_analyzeIndex(&task);

	Solver_search(&task, obs);
	dumpMatchList("triangle search complete", task.proposed, "proposed ");

	task.deferred = List_create(List_size(task.proposed), 0);
	List_push(&lists, task.deferred);

	task.selected = List_create(List_size(task.proposed), 0);
	List_push(&lists, task.selected);

	if (!groupMatches(&task))
		goto cleanup;
	report_status("matches grouped\n");

	disambiguateMatches(&task);

	a = Analyzer_create("raw");
	if (!Analyzer_analyze(a, task.selected))
		goto cleanup;

	qMethod(&task, obs);
	Analyzer_reset(a, "triangle");
	Analyzer_analyze(a, task.selected);

	done = 0;
	filter.stats = &a->stats;

	while (!done) {

		const char * tag = ITERATION[filter.iteration];

		++filter.iteration;

		done = 1;
		count = List_size(task.selected);

		filterMatches(&task, &filter);

		if (count != List_size(task.selected)) {
			done = 0;
			qMethod(&task, obs);
			sprintf(buffer, "filter %s", tag);
			Analyzer_reset(a, buffer);
			Analyzer_analyze(a, task.selected);
		}

		if (List_size(task.deferred)) {
			count = List_size(task.selected);
			reinstateMatches(&task, &filter);
			if (count != List_size(task.selected)) {
				done = 0;
				qMethod(&task, obs);
				sprintf(buffer, "undefer %s", tag);
				Analyzer_reset(a, buffer);
				Analyzer_analyze(a, task.selected);
			}
		}

		if (filter.iteration > sizeof(ITERATION)/sizeof(ITERATION[0])) {
			done = 1;
			report_warning("ran out of iterations\n");
		}
	}

	if (par->expansion != EXPAND_NONE) {
		expandMatches(&task, ref, obs);
		qMethod(&task, obs);
		Analyzer_reset(a, "expanded");
		Analyzer_analyze(a, task.selected);
	}

cleanup:
	if (a)
		Analyzer_deallocate(a);

	for (i = 0; i < List_size(&lists); ++i)
		List_deallocate((List *) List_get(&lists, i));

	return q != NULL;
}


int groupMatches (Task * task)
{
	int i;
	Group group[3] = { { 0 } };
	Group * alphaGroup = &group[0];
	Group * preGroup = &group[1];
	Group * postGroup = &group[2];
	Group * useGroup = 0;
	char grpStr[256];
	int minCount;
	Parameters * par = task->par;
	List * old;
	List * proposed;

	if (List_size(task->proposed) < 1) {
		report_warning("grouping failed (no matches)\n");
		return 0;
	}

	old = task->proposed;
	minCount = List_size(old) > 6;

	proposed = List_create(List_size(old), 0);

	for (i = 0; i < List_size(old); ++i) {
		Match * m = (Match *) List_get(old, i);
		if (m->count > minCount)
			List_push(proposed, m);
		else
			rejectMatch("ignoring trivial ", m);
	}

	List_sort(proposed, Match_compare);

	for (i = 0; i < 3; ++i)
		group[i].angle = Math_toRadians(par->asGroupAngle / 3600);

	Group_select(alphaGroup, proposed, 0, List_size(proposed));
	Group_toString(alphaGroup, proposed, grpStr, sizeof(grpStr));
	alphaGroup->tag = "alpha";
	report_status("alpha %s\n", grpStr);

	/* find the largest group before alpha */
	Group_select(preGroup, proposed, 0, alphaGroup->first);
	preGroup->tag = "pre";
	Group_toString(preGroup, proposed, grpStr, sizeof(grpStr));
	report_status("pre %s\n", grpStr);

	/* find the largest group after alpha */
	Group_select(postGroup, proposed,
			alphaGroup->first + alphaGroup->size, List_size(proposed));
	postGroup->tag = "post";
	Group_toString(postGroup, proposed, grpStr, sizeof(grpStr));
	report_status("post %s\n", grpStr);

	/* select the group with the highest score */
	for (i = 0; i < 3; ++i)
		if (!useGroup || group[i].score > useGroup->score)
			useGroup = &group[i];

	report_status("selected %s group\n", useGroup->tag);

	/* make sure it is enough better than alternatives */
	for (i = 0; i < 3; ++i) {
		Group * g = &group[i];
		if (g != useGroup && g->size > 0)
			if (useGroup->score < g->score + par->nGroupDelta) {
				report_error("match groups are ambiguous [%.1f vs %.1f (%s)]\n",
						useGroup->score, g->score, g->tag);
				return 0;
			}
	}

	if (par->asDeferAngle > 0) {
		int last = useGroup->first + useGroup->size - 1;
		Match * m0 = (Match *) List_get(proposed, useGroup->first);
		double m0_arcsec = Match_angle_arcsec(m0);
		for (i = 0; i < useGroup->first; ++i) {
			Match * m = (Match *) List_get(proposed, i);
			if (Match_angle_arcsec(m) >= m0_arcsec - par->asDeferAngle)
				deferMatch(task, "deferring pre ", m);
			else
				rejectMatch("reject pre ", m);
		}

		m0 = (Match *) List_get(proposed, last);
		m0_arcsec = Match_angle_arcsec(m0);
		for (i = last +1; i < List_size(proposed); ++i) {
			Match * m = (Match *) List_get(proposed, i);
			if (Match_angle_arcsec(m) <= m0_arcsec + par->asDeferAngle)
				deferMatch(task, "deferring post ", m);
			else
				rejectMatch("reject post ", m);
		}
	}

	old = proposed;
	task->proposed = proposed = List_create(List_size(old), 0);
	for (i = 0; i < useGroup->size; ++i) {
		Match * m = (Match *) List_get(old, useGroup->first + i);
		List_push(proposed, m);
	}

	return List_size(proposed) + List_size(task->deferred) >= 3;
}


static void filterBaseMedian (Task * task, MatchFilter * filter)
{
	int i;
	Parameters * par = task->par;
	List * selected = task->selected;
	const Stats * stats = filter->stats;
	double limit;
	int count = 0;

	if (!filter->iteration)
		limit = stats->median * par->kFilterMedian + par->kFilterBase;
	else
		limit = stats->median * par->kRefilterMedian + par->kRefilterBase;
	filter->limit = limit;

	report_status("filtering matches with limit %.3f [arcsec]\n", limit);

	/* perform filtering */
	for (i = List_size(selected) - 1; i >= 0; --i) {
		Match * m = (Match *) List_get(selected, i);
		double arcsec = Match_angle_arcsec(m);
		if (arcsec > limit) {
			++count;
			rejectMatch("filtered ", m);
			List_remove(selected, m);
		}
	}

	if (!count)
		report_status("no matches filtered\n");
}


static void filterBaseCountRange (Task * task, MatchFilter * filter)
{
	int i;
	Parameters * par = task->par;
	List * selected = task->selected;
	List * candidates;
	double limit;
	const Stats * stats = filter->stats;

	limit = stats->max - par->kFilterRange;
	if (limit < par->kFilterBase)
		limit = par->kFilterBase;
	filter->limit = limit;

	if (stats->max <= par->kFilterBase) {
		report_status("no matches filtered [all better than %.3f [arcsec]\n", par->kFilterBase);
		return;
	}
	
	report_status("finding up to %d filtering candidates worse than limit=%.3f [arcsec]\n", par->nFilterCount, limit);

	/* perform filtering */
	candidates = List_create(List_size(selected), 0);
	for (i = 0; i < List_size(selected); ++i) {
		Match * m = (Match *) List_get(selected, i);
		double as = Match_angle_arcsec(m);
		if (as > limit) {
			List_push(candidates, m);
			reportMatch("candidate ", m);
		}
	}

	i = 0;
	if (List_size(candidates) > par->nFilterCount) {
		report_status("found %d [>%d] candidates, ignoring the best %d\n",
				List_size(candidates), par->nFilterCount,
				List_size(candidates) - par->nFilterCount);
		List_sort(candidates, Match_compare);
		i = List_size(candidates) - par->nFilterCount;
	}

	for ( ; i < List_size(candidates); ++i) {
		Match * m = (Match *) List_get(candidates, i);
		rejectMatch("filtered ", m);
		List_remove(selected, m);
	}
}


int filterMatches (Task * task, MatchFilter * filter)
{
	Parameters * par = task->par;

	if (par->filterMode == FILTER_BASE_MEDIAN)
		filterBaseMedian(task, filter);
	else
		filterBaseCountRange(task, filter);

	if (par->debug & DEBUG_MATCHING)
		reportMatches(task, "after filtering");

	return List_size(task->selected) + List_size(task->deferred)
			>= par->nMinMatches;
}


void expandMatches (Task * task, List * ref, List * obs)
{
	Parameters * par = task->par;
	int i;
	List * candidates;
	Position pos;
	double radius;

	report_status("creating catalog\n");
	Catalog_initialize(obs, par->nCatalog);
	Catalog_index(ref);
	report_status("catalog indexed\n");

	candidates = LIST_CREATE(20, 0);

	radius = Math_toRadians(par->asExpandRadius / 3600);

	for (i = 0; i < List_size(obs); ++i) {

		Source * o = (Source *) List_get(obs, i);

		if (!o->locked) {
			Position_construct(&pos, o->unit, radius);
			List_clear(candidates);
			Catalog_query(&pos, candidates);
			if (par->expansion == EXPAND_SIMPLE)
				expandSimple(task, o, candidates);
			else if (par->expansion == EXPAND_BEST)
				expandBest(task, o, candidates);
		}
	}

	List_deallocate(candidates);

	Catalog_deconstruct();
}


void expandSimple (Task * task, Source * o, List * candidates)
{
	Parameters * par = task->par;

	int nCand = List_size(candidates);
	if (nCand == 1) {
		Source * c = (Source *) List_get(candidates, 0);
		if (fabs(c->mag - o->mag) > par->errMag)
			; /* magnitude mismatch */
		else if (c->locked)
			report_warning("attempted to map %s to [locked] %s\n", o->id, c->id);
		else
			assignSource(task->proposed, o, c, ASSIGN_REPORT|ASSIGN_EXPAND);
	}
	else if (nCand > 1)
		report_verbose("warning: %d candidates for %s\n", nCand, o->id);
	else
		report_verbose("warning: no candidates for %s\n", o->id);
}


static int selectStrongestMatches (Source * s, Match * strongest[2])
{
	int i;
	Match * alpha = 0;
	Match * beta = 0;
	int size = List_size(s->proposed);

	if (size < 1)
		return 0;

	alpha = (Match *) List_get(s->proposed, 0);
	if (size > 1)
		beta = (Match *) List_get(s->proposed, 1);

	if (beta && beta->count > alpha->count) {
		Match * m = alpha;
		alpha = beta;
		beta = m;
	}

	for (i = 2; i < size; ++i) {
		Match * m = (Match *) List_get(s->proposed, i);
		if (m->count > alpha->count) {
			beta = alpha;
			alpha = m;
		}
		else if (m->count > beta->count)
			beta = m;
	}

	strongest[0] = alpha;
	strongest[1] = beta;

	return beta ? 2 : 1;
}


static void dispatchMatch (Task * task, Match * m)
{
	int obsCount, refCount;
	Match * obsAB[2];
	Match * refAB[2];
	Source * obs = m->obs;
	Source * ref = m->ref;
	int misCount = task->misCount;

	obsCount = selectStrongestMatches(obs, obsAB);
	refCount = selectStrongestMatches(ref, refAB);

	if (obsCount == 1 && refCount == 1) {
		const char * tag = "unambiguous ";
		if (m->count <= misCount)
			tag = "unambiguous but weak ";
		installMatch(task, tag, m);
	}

	else if (obsCount == 1) {
		/* obs was matched to one reference,
		 * but ref has alternative match(es) */
		Match * alpha = refAB[0];
		Match * beta = refAB[1];
		if (alpha->obs == obs && beta->count <= misCount && m->count > misCount)
			installMatch(task, "ref dominating ", m);
		else
			deferMatch(task, "ambiguous ", m);
	}

	else if (refCount == 1) {
		/* ref was matched to one observation,
		 * but obs has alternative match(es) */
		Match * alpha = obsAB[0];
		Match * beta = obsAB[1];
		if (alpha->ref == ref && m->count > misCount
				&& beta->count <= misCount)
			installMatch(task, "obs dominating ", m);
		else
			deferMatch(task, "ambiguous ", m);
	}

	else {
		/* both the obs and ref for this match had alternative matches;
		 * if this match dominates the next best obs and ref matches, keep it */
		if (refAB[0]->obs == obs && obsAB[0]->ref == ref
				&& refAB[1]->count <= misCount && obsAB[1]->count <= misCount
				&& m->count > misCount) {
			installMatch(task, "obs+ref dominating ", m);
		}
		else
			deferMatch(task, "doubly ambiguous ", m);
	}
}


static int compareMatchCountsDescending (const void * p1, const void * p2)
{
	Match * m1 = *(Match **) p1;
	Match * m2 = *(Match **) p2;
	return m2->count - m1->count;
}


void assignSource (List * list, Source * obs, Source * ref, int flags)
{
	Match * m;
	if (obs->assigned) {
		if (obs->assigned == ref)
			return;
		report_warning("reassigning %s\n", obs->id);
	}

	if (ref->assigned)
		report_warning("reassigning %s\n", ref->id);

	Source_assign(obs, ref);
	Source_assign(ref, obs);

	if (list) {
		m = Match_create(obs, ref);
		List_push(list, m);
	}

	if (flags & ASSIGN_REPORT) {
		const char * assign = "assigned";
		if (flags & ASSIGN_DEFERRED)
			assign = "assigning deferred";
		else if (flags & ASSIGN_EXPAND)
			assign = "assigning expanded";
		 report_status("%s %s to %s at %.3f [arcsec]\n",
				 assign, obs->id, ref->id, Source_angle_arcsec(obs, ref));
	}
}


static int disambiguateMatches (Task * task)
{
	Parameters * par = task->par;
	List * proposed = task->proposed;
	int i;
	int maxCount;

	if (List_size(proposed) < 2)
		return 0;

	List_sort(proposed, compareMatchCountsDescending);
	if (par->debug & DEBUG_MATCHING)
		dumpMatchList("sorted descending", proposed, 0);

	maxCount = ((Match *) List_get(proposed, 0))->count;

	task->misCount = (int) (par->kFilterIgnore * maxCount);
	report_status("disambiguating matches [maxCount=%d misCount=%d proposed=%d]\n",
				maxCount, task->misCount, List_size(proposed));

	for (i = 0; i < List_size(proposed); ++i) {

		Match * m = (Match *) List_get(proposed, i);

		if (m->obs->locked) {
			/* report_status("%s has been locked\n", m->obs->id); */
			rejectMatch("discarding ", m);
			continue;
		}

		dispatchMatch(task, m);
	}

	if (par->debug & DEBUG_MATCHING)
		reportMatches(task, "after disambiguation");

	return 1;
}


static void reinstateMatches (Task * task, const MatchFilter * filter)
{
	Parameters * par = task->par;
	List * deferred = task->deferred;
	int i;
	Match * m;
	int obsCount, refCount;
	Match * obsAB[2];
	Match * refAB[2];
	Source * obs;
	Source * ref;
	int count = 0;

	/* purge bad matches */
	for (i = List_size(deferred) - 1; i >= 0; --i) {

		m = (Match *) List_get(deferred, i);

		if (Match_angle_arcsec(m) > filter->limit) {
			rejectMatch("rejecting deferred ", m);
			List_remove(deferred, m);
			++count;
		}
	}

	/* if any deferred matches are now unambiguous, install them */
	for (i = List_size(deferred) - 1; i >= 0; --i) {

		m = (Match *) List_get(deferred, i);

		obs = m->obs;
		ref = m->ref;

		if (obs->locked || ref->locked) {
			report_verbose("%s or %s locked [%d/%d]\n",
					obs->id, ref->id, obs->locked, ref->locked);
			continue;
		}

		obsCount = selectStrongestMatches(obs, obsAB);
		refCount = selectStrongestMatches(ref, refAB);

		if (obsCount == 1 && refCount == 1) {
			List_remove(deferred, m);
			installMatch(task, "undeferring ", m);
			++count;
		}
		else {
			report_verbose("%s to %s remains ambiguous o2r=%d r2o=%d\n",
					obs->id, ref->id, obsCount, refCount);
		}
	}

	if (count > 0) {
		if (par->debug & DEBUG_MATCHING)
			reportMatches(task, "after undeferring");
	}
	else
		report_status("nothing undeferred\n");
}


void expandBest (Task * task, Source * obs, List * candidates)
{
	Parameters * par = task->par;
	Source * best = NULL;
	double bestScore = par->kExpandWorst;

	int i;
	for (i = 0; i < List_size(candidates); ++i) {
		Source * c = (Source *) List_get(candidates, i);
		double arcsec, badness;
		float dmag;
		if (c->locked)
			continue;

		arcsec = Source_angle_arcsec(obs, c);
		dmag = fabs(obs->mag - c->mag);
		badness = arcsec * par->kExpandDist + dmag * par->kExpandMag;
		if (badness < bestScore) {
			bestScore = badness;
			best = c;
		}
	}

	if (best != NULL)
		assignSource(task->proposed, obs, best, ASSIGN_REPORT|ASSIGN_EXPAND);
}


void qMethod (Task * task, List * obs)
{
	int i;

	double q[4];
	double qinv[4];
	double maxDelta = 0;
	double *qOptimal = &task->qOptimal[0];

	if (QMethod_solve(task->selected, q))
		return;

	qinv[0] = -q[0];
	qinv[1] = -q[1];
	qinv[2] = -q[2];
	qinv[3] =  q[3];

	report_status("refining positions\n");
	for (i = 0; i < List_size(obs); ++i) {
		Source * o;
		Source * r;
		double prime[3];
		double radec[3];

		o = (Source *) List_get(obs, i);
		Math_rotate(o->unit, qinv, prime);
		Source_correct(o, prime);
		Math_v3rdl(prime, radec);
		radec[0] *= 180 / Math_PI;
		radec[1] *= 180 / Math_PI;
		r = o->assigned;
		if (r) {
			double as = Source_angle_arcsec(o, r);
			if (as > maxDelta)
				maxDelta = as;
		}
	}

	if (qOptimal[0] == 0) {
		qOptimal[0] = qinv[0];
		qOptimal[1] = qinv[1];
		qOptimal[2] = qinv[2];
		qOptimal[3] = qinv[3];
	}
	else
		Math_quatprod(qOptimal, qinv, qOptimal);

	Math_qNormalize(qOptimal);

	q[0] = qOptimal[0];
	q[1] = qOptimal[1];
	q[2] = qOptimal[2];
	q[3] = qOptimal[3];

#if 0
	report_status("optimal quaternion: %.16e %.16e %.16e %.16e\n",
			q[0], q[1], q[2], q[3]);
#else
	report_status("optimal quaternion:\n");
	for (i = 0; i < 4; ++i)
		report_status("\tQ%d %.16e\n", i, q[i]);
#endif
	report_status("match count: %d\n", List_size(task->selected));
	report_status("max separation: %.3f [arcsec]\n", maxDelta);
}


List * getSources (List * list, int obs, const Parameters * par)
{
	List * out;
	int i, j, withMag = 0;
	double maxcos;
	const char * tag = obs ? "obs" : "ref";
	int limit = obs ? par->nObservation : par->nReference;

	out = List_create(limit, 0);

	List_sort(list, Source_compare);

	if (par->asSeparateSources > 0) {
		maxcos = cos(Math_toRadians(par->asSeparateSources / 3600.));

		for (i = 0; i < List_size(list); ++i) {
			Source * s = (Source *) List_get(list, i);
			for (j = 0; j < i; ++j) {
				Source * o = (Source *) List_get(list, j);
				if (Source_cosangle(o, s) > maxcos) {
					s->locked = 1;
					o->locked = 1;
					report_verbose("%s is too close to %s\n", s->id, o->id);
				}
			}
		}
	}

	for (i = 0; List_size(out) < limit && i < List_size(list); ++i) {
		Source * s = (Source *) List_get(list, i);
		int reject = s->locked;

		if (reject)
			;
		else if (s->mag >= par->minMag && s->mag <= par->maxMag) {
			List_push(out, s);
			++withMag;
		}
		else if (Source_isMagNull(s))
			List_push(out, s);
		else if (par->debug & DEBUG_SOURCE_SELECTION)
			report_verbose("source %s is out of mag range\n", s->id);
	}

	for (i = 0; i < List_size(list); ++i) {
		Source * s = (Source *) List_get(list, i);
		s->locked = 0;
	}

	if (withMag == 0)
		report_warning("no %s sources have mag\n", tag);
	else if (withMag == List_size(out))
		report_status("all %d %s sources have mag\n", withMag, tag);
	else
		report_warning("%d of %d %s sources have mag\n", withMag, List_size(out), tag);

	return out;
}


int isMagMatch (double tol, Source * a, Source * b)
{
	if (Source_isMagNull(a) || Source_isMagNull(b))
		return 1;
	return fabs(a->mag - b->mag) <= tol;
}


int isDoubletMatch (double tol, double a, double b)
{
	return fabs(a - b) <= tol;
}


int Solver_isOrientationMatch (double minimum, Triplet * r, Triplet * o)
{
	double diffref[3];
	double diffobs[3];
	double cosangle;

	Math_v3sub(diffref, r->primary->unit, r->left->unit);
	Math_v3sub(diffobs, o->primary->unit, o->left->unit);
	cosangle = Math_v3cosangle(diffref, diffobs);
	return cosangle >= minimum;
}


int isTripletMatch (Task * task, Triplet * r, Triplet * o)
{
	Parameters * par = task->par;

	/* compare edges */
	if (!isDoubletMatch(task->errDoublet, r->angleAB, o->angleAB))
		return 0;
	if (!isDoubletMatch(task->errDoublet, r->angleAC, o->angleAC))
		return 0;
	if (!isDoubletMatch(task->errDoublet, r->angleBC, o->angleBC))
		return 0;

	/* compare magnitudes */
	if (!isMagMatch(par->errMag, r->primary, o->primary))
		return 0;
	if (!isMagMatch(par->errMag, r->left, o->left))
		return 0;
	if (!isMagMatch(par->errMag, r->right, o->right))
		return 0;

	/* compare overall */
	if (!Solver_isOrientationMatch(task->mincosRot, r, o))
		return 0;

	
	if (par->debug & DEBUG_TRIPLETS) {
		char r_string[512], o_string[512];

		Triplet_toString(r, r_string);
		Triplet_toString(o, o_string);

		report_verbose("matched %s to %s\n", r_string, o_string);
	}

	return 1;
}



void iterateTriplets (Task * task, List * s, int (*apply)(Task * task, Triplet *))
{
	int i, j, k;
	Triplet t;
	int size = List_size(s);
	double maxDoublet = task->maxDoublet;
	double minDoublet = task->minDoublet;
	double maxTriplet = task->maxTriplet;
	double minTriplet = task->minTriplet;

	for (i = 0; i < size; ++i) {
		Source * s1 = (Source *) List_get(s, i);

		for (j = 0; j < size; ++j) {
			Source * s2 = (Source *) List_get(s, j);
			if (s2 == s1)
				break;

			for (k = 0; k < size; ++k) {
				Source * s3 = (Source *) List_get(s, k);
				if (s3 == s1 || s3 == s2)
					break;

				Triplet_construct(&t, s3, s1, s2);

				if (!t.rightHanded) {
					Triplet_construct(&t, s3, s2, s1);
					if (!t.rightHanded)
						report_verbose("unable to make %s %s %s right-handed\n",
								s1->id, s2->id, s3->id);
				}

				/* could perform the initial checks on cosines
					then calculate the angles when indexing */
				if (!t.rightHanded ||
					 t.angleAB > maxDoublet ||
					 t.angleAC > maxDoublet ||
					 t.angleAB < minDoublet ||
					 t.angleAC < minDoublet ||
					 t.angleBAC < minTriplet ||
					 t.angleBAC > maxTriplet) {
					;
				}
				else if ((*apply)(task, &t))
					return;
			}
		}
	}
}


void Solver_analyzeIndex (Task * task)
{
	Parameters * par = task->par;
	int n, empty = 0;
	double sum = 0;
	double sum2 = 0;
	double mean, variance;

	int i, j;
	for (i = 0; i < par->nPrimary; ++i) {
		List * index0 = List_get(task->tripletIndex, i);
		for (j = 0; j < par->nSecondary; ++j) {
			List * index1 = List_get(index0, j);
			if (List_size(index1) == 0)
				++empty;
			else {
				sum += List_size(index1);
				sum2 += List_size(index1) * List_size(index1);
			}
		}
	}

	n = par->nPrimary * par->nSecondary - empty;
	mean = sum / n;
	variance = (sum2 - mean * sum) / n;
	report_verbose("empty cells in index %d [%.1f %%]\n",
			empty, 100. * empty / par->nPrimary / par->nSecondary);
	report_verbose("non-empty cell mean %.1f\n", mean);
	report_verbose("non-empty cell variance %.1f\n", variance);
}


static void proposeAlpha (List * obsProposed, Source * obs, Source * ref)
{
	if (!obs->proposed || List_size(obs->proposed) == 0) {
		List_push(obsProposed, obs);
	}

	Source_propose(obs, ref);
}


void Solver_search (Task * task, List * useObs)
{
	int i, j;
	List * obsProposed;
	List * triangles;

	task->triangles = triangles = List_create(20, free);
	iterateTriplets(task, useObs, findTripletMatches);

	obsProposed = List_create(List_size(triangles) * 3, 0);

	for (i = 0; i < List_size(triangles); ++i) {
		TripletMatch * tm;
		Triplet * r;
		Triplet * o;
		tm = (TripletMatch *) List_get(triangles, i);
		r = tm->ref;
		o = tm->obs;
		proposeAlpha(obsProposed, o->primary, r->primary);
		proposeAlpha(obsProposed, o->left, r->left);
		proposeAlpha(obsProposed, o->right, r->right);
	}

	task->proposed = List_create(List_size(obsProposed) * 3, free);

	for (i = 0; i < List_size(obsProposed); ++i) {
		Source * s = (Source *) List_get(obsProposed, i);
		for (j = 0; j < List_size(s->proposed); ++j) {
			Match * m = (Match *) List_get(s->proposed, j);
			List_push(task->proposed, m);
		}
	}

	List_deallocate(obsProposed);
}


static int getPrimary (Task * task, double x)
{
	return (int) ((x - task->minDoublet) / task->dPrimary);
}


static int getSecondary (Task * task, double x)
{
	return (int) ((x - task->minTriplet) / task->dSecondary);
}


int addTriplet (Task * task, Triplet * t)
{
	int i, j, added = 0;
	int i0, i1, j0, j1;
	double tmp;
	double errTriplet;

	tmp = Math_max(t->angleAB - task->errDoublet, task->minDoublet);
	i0 = getPrimary(task, tmp);

	tmp = Math_min(t->angleAB + task->errDoublet, task->maxDoublet - EPSILON);
	i1 = getPrimary(task, tmp);

	errTriplet = task->errDoublet / t->angleAB + task->errDoublet / t->angleAC;

	tmp = Math_max(t->angleBAC - errTriplet, task->minTriplet);
	j0 = getSecondary(task, tmp);
	tmp = Math_min(t->angleBAC + errTriplet, task->maxTriplet - EPSILON);
	j1 = getSecondary(task, tmp);

	t = Triplet_copy(t);

	for (i = i0; i <= i1; ++i) {
		List * index0 = (List *) List_get(task->tripletIndex, i);
		for (j = j0; j <= j1; ++j) {
			List * index1 = (List *) List_get(index0, j);
			List_push(index1, t);
			if (!added) {
				List_push(task->tripletArchive, t);
				added = 1;
			}
		}
	}

	return 0;
}


int findTripletMatches (Task * task, Triplet * t)
{
	int i, x, y;
	List * triplets;

	x = getPrimary(task, t->angleAB);
	y = getSecondary(task, t->angleBAC);

	triplets = List_get(List_get(task->tripletIndex, x), y);

	for (i = 0; i < List_size(triplets); ++i) {
		Triplet * t0 = List_get(triplets, i);
		if (isTripletMatch(task, t, t0)) {
			TripletMatch * tm = TripletMatch_create(Triplet_copy(t), t0);
			List_push(task->triangles, tm);
			List_push(task->tripletArchive, tm->obs);
		}
	}

	return 0;
}


void constructIndex (Task * task)
{
	Parameters * par = task->par;
	int x, y;

	task->tripletIndex = LIST_CREATE(par->nPrimary, List_deallocate);
	task->tripletArchive = LIST_CREATE(par->nPrimary * par->nSecondary, Triplet_deallocate);

	for (x = 0; x < par->nPrimary; ++x) {
		List * index0 = LIST_CREATE(par->nSecondary, List_deallocate);

		List_push(task->tripletIndex, index0);

		for (y = 0; y < par->nSecondary; ++y) {
			List * index1 = LIST_CREATE(20, Triplet_deconstruct);
			List_push(index0, index1);
		}
	}
}


void Solver_deconstruct (Task * task)
{
#if 0
	LIST_DEALLOCATE(task->proposed);
	LIST_DEALLOCATE(task->triangles);
	LIST_DEALLOCATE(task->tripletIndex);
	LIST_DEALLOCATE(task->tripletArchive);
#endif
}

