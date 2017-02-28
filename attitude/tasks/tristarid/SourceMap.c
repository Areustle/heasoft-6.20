/*
 * $Source: /headas/headas/attitude/tasks/tristarid/SourceMap.c,v $
 * $Revision: 1.5 $
 * $Date: 2005/08/29 12:43:07 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: SourceMap.c,v $
 * Revision 1.5  2005/08/29 12:43:07  wiegand
 * Removed unnecessary macro.
 *
 * Revision 1.4  2005/08/27 12:52:41  wiegand
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
 */

#include <stdlib.h>

#include "SourceMap.h"
#include "SUtil.h"


SourceMap * SourceMap_allocate (int size0)
{
   const int MIN_SIZE = 30;
   SourceMap * sm = calloc(1, sizeof(SourceMap));

   if (size0 < MIN_SIZE)
      size0 = MIN_SIZE;

   sm->keys = LIST_CREATE(size0, free);
   sm->data = LIST_CREATE(size0, List_deallocate);

   return sm;
}


void SourceMap_deallocate (SourceMap * sm)
{
   List_deallocate(sm->keys);
   List_deallocate(sm->data);
   sm->keys = 0;
   sm->data = 0;
   free(sm);
}


int SourceMap_add (SourceMap * sm, const char * key, Source * s)
{
   int mapSize = List_size(sm->keys);
   int x;
   char *newKey;
   List *newSrcLst;

   for (x = 0; x < mapSize; x++)
      if (strcmp(key, List_get(sm->keys, x)) == 0)
         break;          /* Matching key */

   if (x != mapSize) {
      List *targets = (List *) List_get(sm->data, x);
      int numSrcs = List_size(targets);
      int y;
      for (y = 0; y < numSrcs; y++)
         if (((Source *) List_get(targets, y)) == s)
            break;         /* Matching source */

      if (y != numSrcs)
         return 1;         /* Repeat */

      List_push(targets, s);
      return 0;
   }

   newKey = string_copy(key);
   List_push(sm->keys, newKey);
   newSrcLst = List_create(20, 0);
   List_push(newSrcLst, s);
   List_push(sm->data, newSrcLst);

   return 0;
}


List * SourceMap_get (SourceMap * sm, const char * key)
{
   int x;
   for (x = 0; x < List_size(sm->keys); x++)
      if (strcmp(((char *) List_get(sm->keys, x)), key) == 0)
         return (List *) List_get(sm->data, x);
   return 0;
}

