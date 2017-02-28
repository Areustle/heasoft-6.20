/*
 * $Source: /headas/headas/attitude/tasks/tristarid/List.c,v $
 * $Revision: 1.4 $
 * $Date: 2006/02/22 15:45:07 $
 * 
 * $Log: List.c,v $
 * Revision 1.4  2006/02/22 15:45:07  rwiegand
 * Support removing objects from lists.
 *
 * Revision 1.3  2005/08/27 12:51:06  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.2  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.1  2005/08/03 13:05:57  drhunter
 * Initial revision
 */

#include <stdlib.h>
#include <stdio.h>

#include "List.h"



List * List_create (int size0, LIST_DESTRUCTOR release)
{
   List * list = List_allocate();
   List_construct(list, size0, release);
   return list;
}


List * List_allocate  ()
{
	return calloc(1, sizeof(List));
}


void List_construct (List * list, int size0, LIST_DESTRUCTOR release)
{
   list->data = calloc(size0, sizeof(void *));

   list->allocated = size0;
   list->elements = 0;

   list->release = release;
}


void List_deallocate (List * list)
{
   List_deconstruct(list);
   free(list);
}


void List_deconstruct (List * list)
{
   int i;

   for (i = 0; i < list->elements; i++) {
      if (list->release)
         list->release(list->data[i]);
      list->data[i] = 0;
   }

   if (list->data)
      free(list->data);
   list->data = 0;

   list->allocated = 0;
   list->elements = 0;
}


int List_size (const List *list)
{
   return list->elements;
}


void List_clear (List * list)
{
   int i;
   if (list->release)
      for (i = 0; i < list->elements; ++i) {
         (list->release)(list->data[i]);
         list->data[i] = 0;
      }

   list->elements = 0;
}


void * List_get (const List * list, int x)
{
   return list->data[x];
}


int List_push (List *list, void *p) {
   if (list->elements == list->allocated) {
      void * p = realloc(list->data, 2 * list->allocated * sizeof(void *));
      if (p) {
         list->data = p;
         list->allocated *= 2;
      }
      else {
         fprintf(stderr, "List_push: unable to allocate space\n");
         return 0;
      }
   }

   list->data[list->elements++] = p;
   return list->elements;
}


void List_alias (List * list, List * alias, int start, int size)
{
   alias->data = list->data + start;
   alias->elements = size < 0 ? list->elements - start : size;
   alias->release = 0;
   alias->allocated = -1;
}

   
void List_sort (List *list, int (*compare)(const void *, const void *))
{
#if 1
   qsort(list->data, list->elements, sizeof(void*), compare);
#else
   merge_sort(list->data, list->elements, compare);
#endif
}


/*
 * remove the first instance of p from list
 * returns: 1 if removed, 0 otherwise
 */
int List_remove (List * list, void *p)
{
   int i;
   int removed = 0;

   for (i = 0; i < list->elements; i++) {
      if (list->data[i] == p) {
         removed = 1;
         if (list->release)
            list->release(list->data[i]);
         --list->elements;
         while (i < list->elements) {
            list->data[i] = list->data[i+1];
			++i;
		 }
         list->data[list->elements] = 0;
         break;
      }
   }

   return removed;
}


