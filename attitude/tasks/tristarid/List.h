/*
 * $Source: /headas/headas/attitude/tasks/tristarid/List.h,v $
 * $Revision: 1.4 $
 * $Date: 2006/02/22 15:45:08 $
 * 
 * $Log: List.h,v $
 * Revision 1.4  2006/02/22 15:45:08  rwiegand
 * Support removing objects from lists.
 *
 * Revision 1.3  2005/08/27 12:51:08  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.2  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.1  2005/08/03 13:07:35  drhunter
 * Initial revision
 */

#ifndef TRISTARID_LIST_H
#define TRISTARID_LIST_H


typedef void (*LIST_DESTRUCTOR)(void*);

typedef struct
{
   int allocated;
   int elements;

   void **data;

   LIST_DESTRUCTOR release;

} List;



#define LIST_CREATE(s, d) List_create(s, (LIST_DESTRUCTOR) d)
#define LIST_DEALLOCATE(list) if (list) { List_deallocate(list); list = 0; }

List * List_create (int size0, LIST_DESTRUCTOR release);

List * List_allocate ();

void List_construct (List * list, int size0, LIST_DESTRUCTOR release);
void List_deconstruct (List * list);

void List_deallocate (List * list);

int List_size (const List * list);

void List_clear (List * list);

void * List_get (const List * list, int x);

void * List_put (List * list, int x, void * p);

int List_push (List * list, void * p);

void List_sort (List * list, int (*compare)(const void *, const void *));

void List_alias (List * list, List * out, int start, int size);

int List_remove (List * list, void * p);



#endif

