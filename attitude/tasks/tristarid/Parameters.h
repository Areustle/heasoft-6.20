/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Parameters.h,v $
 * $Revision: 1.10 $
 * $Date: 2010/07/26 04:38:00 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Parameters.h,v $
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
 * Revision 1.7  2006/02/22 15:50:56  rwiegand
 * Reworked match disambiguation.  Reverted change that did not not identify
 * which was the observation and which was the reference in a Match.
 *
 * Revision 1.6  2005/08/29 12:38:12  wiegand
 * Added output path, renamed and stream-lined other parameters.
 *
 * Revision 1.5  2005/08/27 12:51:52  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.4  2005/08/09 18:57:19  drhunter
 * Considering final.
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

#ifndef STARID_PARAMETERS_H
#define STARID_PARAMETERS_H

#include "pil.h"


enum
{
	EXPAND_NONE,
	EXPAND_SIMPLE,
	EXPAND_BEST
};


enum
{
	FILTER_BASE_MEDIAN,
	FILTER_BASE_COUNT_RANGE,
	FILTER_LIMIT
};



typedef struct
{
   int debug;

   char refpath[PIL_LINESIZE];
   char obspath[PIL_LINESIZE];
   char outpath[PIL_LINESIZE];

   int refdeg;
   int obsdeg;

   int nReference;
   int nObservation;
   float asMinDoublet;
   float asErrDoublet;
   float asErrDirect;
   float asSeparateSources;
   float asDirectReferences;
   float degTriplet;
   float degFoV;
   float errMag;
   float minMag;
   float maxMag;
   int nPrimary;
   int nSecondary;
   float asGroupAngle;
   float asDeferAngle;
   int nGroupDelta;
   int nMinMatches;
   float degRotErr;

   int filterMode;

   float kFilterIgnore;
   float kFilterBase;
   float kFilterMedian;
   float kRefilterBase;
   float kRefilterMedian;

   int nFilterCount;
   float kFilterRange;

#if 0
   float kFilterDoublet;
   float kFilterMean;
   float kFilterSigma;
   float kFilterRMS;
#endif

   int iteration;

   int expansion;
   int nCatalog;
   float asExpandRadius;
   float kExpandDist;
   float kExpandMag;
   float kExpandWorst;

} Parameters;



#endif

