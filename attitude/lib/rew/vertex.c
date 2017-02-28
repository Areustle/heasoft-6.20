/*
 * $Source: /headas/headas/attitude/lib/rew/vertex.c,v $
 * $Revision: 1.1 $
 * $Date: 2003/09/04 17:41:21 $
 *
 *	Vertex implementation
 *
 * $Log: vertex.c,v $
 * Revision 1.1  2003/09/04 17:41:21  rwiegand
 * Added polygon utilities.  When iterating over images, allow specifying a
 * region (less than the whole image), or the step size (other than every pixel).
 * Allow resizing a FITSHeader memory buffer.
 *
 */


#include <stdio.h>

#include "polygon.h"


void
vertex_print (const Vertex * v)
{
  printf("V%d = ", v->i);
  point_print(v->p);
}


void
vertex_print_tag(const Vertex * v, int index)
{
  printf("V%d = ", index);
  point_print(v->p);
}

