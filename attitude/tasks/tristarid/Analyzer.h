/* 
 * $Source: /headas/headas/attitude/tasks/tristarid/Analyzer.h,v $
 * $Revision: 1.7 $
 * $Date: 2010/07/26 04:38:00 $
 * 
 *   Triplet based star identification algorithm
 * 
 * $Log: Analyzer.h,v $
 * Revision 1.7  2010/07/26 04:38:00  rwiegand
 * The tool now keeps track of two sorts of ambiguous matches: those which
 * are slightly better or worse in terms of distance relative to the primary
 * group, and those which do not yield a unique mapping between observed and
 * reference objects.  During the match filtering process, the hope is that
 * some ambiguities will resolve themselves allowing unambiguous matches to
 * be identified.
 *
 * Revision 1.6  2006/02/22 15:46:18  rwiegand
 * Depend on Source's knowledge of what Matches they are part of instead
 * of SourceMap.
 *
 * Revision 1.5  2005/08/29 12:31:25  wiegand
 * Added Stats object.
 *
 * Revision 1.4  2005/08/27 12:48:23  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:07:35  drhunter
 *  Tristarid as of 9:00 AM, 8/3/05.
 * 
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 *  Initial revision
 * 
 */

#ifndef ANALYZER_H
#define ANALYZER_H

#include "List.h"
#include "Source.h"
#include "SourceMap.h"


typedef struct
{
   int count;
   double sum;
   double sum2;

   double min;
   double max;
   double median;

   double mean;
   double sigma;
   double rms;

} Stats;


typedef struct
{
   char * tag;

   Stats stats;

} Analyzer;


Analyzer * Analyzer_create (const char * tag);

void Analyzer_reset (Analyzer * a, const char * tag);

void Analyzer_deallocate (Analyzer * a);

int Analyzer_analyze (Analyzer * a, List * matches);


#endif

