/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Catalog.h,v $
 * $Revision: 1.4 $
 * $Date: 2005/08/27 12:50:35 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Catalog.h,v $
 * Revision 1.4  2005/08/27 12:50:35  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:07:35  drhunter
 * Tristarid as of 9:00 AM, 8/3/05.
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 *
 */

#ifndef TRISTARID_CATALOG_H
#define TRISTARID_CATALOG_H

#include "List.h"
#include "Position.h"
#include "Source.h"


void Catalog_describe ();

void Catalog_initialize (List *objects, int _n);

void Catalog_deconstruct ();

void Catalog_query (Position *spec, List *out);

void Catalog_include (Position *spec, List *ref, List *out);

int Catalog_test (double x, double y);

void Catalog_index (List *sources);

void Catalog_place (Source *s);


#endif

