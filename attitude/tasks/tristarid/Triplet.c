/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Triplet.c,v $
 * $Revision: 1.4 $
 * $Date: 2005/08/27 12:52:49 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Triplet.c,v $
 * Revision 1.4  2005/08/27 12:52:49  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 19:50:32  drhunter
 * Considering final.
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

#include "Triplet.h"
#include "MUtil.h"


Triplet * Triplet_create (Source * a, Source * b, Source * c)
{
	Triplet * t = calloc(1, sizeof(Triplet));
	Triplet_construct(t, a, b, c);
	return t;
}


Triplet * Triplet_copy (Triplet * t)
{
	Triplet * copy = malloc(sizeof(Triplet));
	*copy = *t;
	return copy;
}


void Triplet_construct (Triplet *t, Source *a, Source *b, Source *c)
{
   t->primary = a;
   t->left = b;
   t->right = c;

   t->cosAB = Source_cosangle(a, b);
   t->cosAC = Source_cosangle(a, c);
   t->angleAB = acos(t->cosAB);
   t->angleAC = acos(t->cosAC);
   t->angleBC = acos(Source_cosangle(b, c));
   Math_v3sub(t->v1, b->unit, a->unit);
   Math_v3sub(t->v2, c->unit, a->unit);
   t->rightHanded = Triplet_isRightHanded(a->unit, t->v1, t->v2);
   t->cosBAC = Math_v3cosangle(t->v1, t->v2);
   t->angleBAC = acos(t->cosBAC);
}


void Triplet_deconstruct (Triplet * t)
{
   t->primary = 0;
   t->left = 0;
   t->right = 0;
}


void Triplet_deallocate(Triplet * t)
{
   Triplet_deconstruct(t);
   free(t);
}


void Triplet_toString(Triplet *t, char * s)
{
   sprintf(s, "Triplet[%s,%s,%s]", t->primary->id, t->left->id, t->right->id);
}


int Triplet_isRightHanded (double a[3], double b[3], double c[3])
{
   double tmp = a[0] * (b[1] * c[2] - b[2] * c[1])
		   + a[1] * (b[2] * c[0] - b[0] * c[2])
		   + a[2] * (b[0] * c[1] - b[1] * c[0]);

   return (tmp > 0);
}


