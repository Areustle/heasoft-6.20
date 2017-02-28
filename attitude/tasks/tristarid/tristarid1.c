/*
 * $Source: /headas/headas/attitude/tasks/tristarid/tristarid1.c,v $
 * $Revision: 1.10 $
 * $Date: 2010/07/26 04:38:00 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: tristarid1.c,v $
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
 * Revision 1.8  2007/10/05 19:17:49  rwiegand
 * Added parameters to constrain the separation between sources used for
 * matching and to filter matches.
 *
 * Revision 1.7  2006/02/22 15:50:57  rwiegand
 * Reworked match disambiguation.  Reverted change that did not not identify
 * which was the observation and which was the reference in a Match.
 *
 * Revision 1.6  2005/10/10 12:14:25  rwiegand
 * Allow reference to observation matches.
 *
 * Revision 1.5  2005/08/29 12:51:25  wiegand
 * Combined Driver and Parameter source.
 *
 * Revision 1.4  2005/08/27 12:50:48  wiegand
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

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "pil.h"
#include "report.h"
#include "headas.h"

#include "List.h"
#include "Loader.h"
#include "Parameters.h"
#include "Solver.h"


#define TOOLSUB tristarid1
#include "headas_main.c"



static int getParameters (Parameters *p)
{
   int code = 0;
   int pill = 0;

   char buffer[PIL_LINESIZE];
   char expandHow[PIL_LINESIZE];

   if (!pill)
      pill = PILGetFname("ref.path", p->refpath);

   if (!pill)
      pill = PILGetFname("obs.path", p->obspath);

   if (!pill)
      pill = PILGetFname("out.path", p->outpath);

   if (!pill)
      pill = PILGetBool("ref.deg", &p->refdeg);

   if (!pill)
      pill = PILGetBool("obs.deg", &p->obsdeg);

   if (!pill)
      pill = PILGetReal4("fov", &p->degFoV);

   if (!pill)
      pill = PILGetInt("n.reference", &p->nReference);

   if (!pill)
      pill = PILGetInt("n.observation", &p->nObservation);

   if (!pill)
      pill = PILGetReal4("direct.refs", &p->asDirectReferences);

   if (!pill)
      pill = PILGetInt("n.primary", &p->nPrimary);

   if (!pill)
      pill = PILGetInt("n.secondary", &p->nSecondary);

   if (!pill)
      pill = PILGetReal4("doublet.err", &p->asErrDoublet);

   if (!pill)
      pill = PILGetReal4("doublet.min", &p->asMinDoublet);

   if (!pill)
      pill = PILGetReal4("triplet.min", &p->degTriplet);

   if (!pill)
      pill = PILGetReal4("mag.min", &p->minMag);

   if (!pill)
      pill = PILGetReal4("mag.max", &p->maxMag);

   if (!pill)
      pill = PILGetReal4("mag.err", &p->errMag);

   if (!pill)
      pill = PILGetReal4("group.angle", &p->asGroupAngle);

   if (!pill)
      pill = PILGetInt("group.delta", &p->nGroupDelta);

   if (!pill)
      pill = PILGetReal4("defer.angle", &p->asDeferAngle);

   if (!pill)
      pill = PILGetInt("min.matches", &p->nMinMatches);

   if (!pill)
      pill = PILGetInt("n.catalog", &p->nCatalog);

   if (!pill)
      pill = PILGetReal4("rot.err", &p->degRotErr);

   p->filterMode = FILTER_BASE_MEDIAN;

   if (!pill) {
      pill = PILGetString("filter.mode", buffer);
      if (!pill) {
         if (!strcasecmp(buffer, "BASE_COUNT_RANGE"))
            p->filterMode = FILTER_BASE_COUNT_RANGE;
      }
   }

   if (!pill)
      pill = PILGetReal4("filter.ignore", &p->kFilterIgnore);

   if (!pill)
      pill = PILGetReal4("filter.base", &p->kFilterBase);

   if (!pill)
      pill = PILGetReal4("filter.median", &p->kFilterMedian);

   if (!pill)
      pill = PILGetReal4("refilter.base", &p->kRefilterBase);

   if (!pill)
      pill = PILGetReal4("refilter.median", &p->kRefilterMedian);

   if (!pill)
      pill = PILGetInt("filter.count", &p->nFilterCount);

   if (!pill)
      pill = PILGetReal4("filter.range", &p->kFilterRange);

#if 0
   if (!pill)
      pill = PILGetReal4("filter.doublet", &p->kFilterDoublet);

   if (!pill)
      pill = PILGetReal4("filter.mean", &p->kFilterMean);

   if (!pill)
      pill = PILGetReal4("filter.sigma", &p->kFilterSigma);

   if (!pill)
      pill = PILGetReal4("filter.rms", &p->kFilterRMS);
#endif

   if (!pill)
      pill = PILGetReal4("sep.sources", &p->asSeparateSources);

   if (!pill)
      pill = PILGetString("expand", expandHow);

   if (!pill)
      pill = PILGetReal4("expand.radius", &p->asExpandRadius);

   if (!pill)
      pill = PILGetReal4("expand.dist", &p->kExpandDist);

   if (!pill)
      pill = PILGetReal4("expand.mag", &p->kExpandMag);

   if (!pill)
      pill = PILGetReal4("expand.worst", &p->kExpandWorst);

   if (!pill) {
      if (strcasecmp(expandHow, "none") == 0)
         p->expansion = EXPAND_NONE;
      else if (strcasecmp(expandHow, "simple") == 0)
         p->expansion = EXPAND_SIMPLE;
      else if (strcasecmp(expandHow, "best") == 0)
         p->expansion = EXPAND_BEST;
      else
         report_warning("ignoring invalid expand value %s\n", expandHow);
   }

   if (pill) {
      code = 1;/*TASK_SETUP_ERROR;*/
      report_error("unable to load parameters\n");
   }
   else
      report_verbose("parameters loaded\n");

   return code;
}


static FILE * LOG = 0;
static int report_file (report_t * info)
{
   char buffer[32];

   const char * code = report_code_string(info->code, buffer);

   if (info->module)
      fprintf(LOG, "%s: %s: %s", code, info->module, info->string);
   else
      fprintf(LOG, "%s: %s", code, info->string);

   return 0;
}



int tristarid1 ()
{
   Parameters par = { 0 };
   List * references;
   List * observations;
   int flags;

   add_report_function(&report_headas);

   report_status("begin starid\n");

   if (getParameters(&par) != 0) {
      report_error("bad parameters\n");
      return 0;
   }

   if (par.outpath[0] && strcasecmp(par.outpath, "NONE")) {
      LOG = fopen(par.outpath, "w");
      if (!LOG)
         report_warning("unable to create %s [%s]\n",
               par.outpath, strerror(errno));
      else
         add_report_function(&report_file);
   }
   references = LIST_CREATE(50, Source_deallocate);
   observations = LIST_CREATE(50, Source_deallocate);

   flags = 0;
   if (par.refdeg) flags |= LOADER_DEGREES;
   if (!loadSources(references, par.refpath, flags)) {
      report_error("unable to load %s\n", par.refpath);
      return 0;
   }

   flags = LOADER_OBSERVATION;
   if (par.refdeg) flags |= LOADER_DEGREES;
   if (!loadSources(observations, par.obspath, flags)) {
      report_error("unable to load %s\n", par.obspath);
      return 0;
   }

   if (List_size(references) == 0) {
      report_error("no references read\n");
      return 0;
   }
   if (List_size(observations) == 0) {
      report_error("no observations read\n");
      return 0;
   }

   report_status("loaded source lists\n");

   Solver_solve(&par, references, observations);

   Solver_deconstruct();

   report_verbose("deallocating source lists\n");
   List_deallocate(references);
   List_deallocate(observations);

   if (LOG)
      fclose(LOG);

   return 0;
}

