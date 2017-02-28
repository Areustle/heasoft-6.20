/*
 * $Source: /headas/headas/attitude/tasks/tristarid/SourceMap.h,v $
 * $Revision: 1.5 $
 * $Date: 2006/02/22 15:50:57 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: SourceMap.h,v $
 * Revision 1.5  2006/02/22 15:50:57  rwiegand
 * Reworked match disambiguation.  Reverted change that did not not identify
 * which was the observation and which was the reference in a Match.
 *
 * Revision 1.4  2005/08/27 12:52:43  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 19:49:28  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:07:35  drhunter
 * Tristarid as of 9:00 AM, 8/3/05.
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#ifndef SOURCEMAP_H
#define SOURCEMAP_H

#include <string.h>

#include "List.h"
#include "Source.h"


typedef struct
{
   List *keys;
   List *data;
} SourceMap;


SourceMap * SourceMap_allocate (int size0);

void SourceMap_deallocate (SourceMap * sm);

int SourceMap_add (SourceMap * sm, const char * key, Source * s);

List * SourceMap_get (SourceMap * sm, const char * key);


#endif

