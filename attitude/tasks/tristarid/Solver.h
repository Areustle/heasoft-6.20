/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Solver.h,v $
 * $Revision: 1.7 $
 * $Date: 2005/08/29 12:39:26 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Solver.h,v $
 * Revision 1.7  2005/08/29 12:39:26  wiegand
 * Removed internals from interface.
 *
 * Revision 1.6  2005/08/27 12:52:30  wiegand
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
 * Revision 1.2  2005/08/03 13:07:35  drhunter
 * Tristarid as of 9:00 AM, 8/3/05.
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#ifndef SOLVER_H
#define SOLVER_H

#include "Parameters.h"
#include "Source.h"
#include "Match.h"
#include "List.h"
#include "Triplet.h"



int Solver_solve (Parameters *p, List *ref, List *obs);

void Solver_deconstruct ();


#endif

