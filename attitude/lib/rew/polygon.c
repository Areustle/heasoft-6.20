/*
 * $Source: /headas/headas/attitude/lib/rew/polygon.c,v $
 * $Revision: 1.1 $
 * $Date: 2003/09/04 17:41:21 $
 *
 *	Polygon structure based on O'Rourke's cPointi/cPointd
 *
 * $Log: polygon.c,v $
 * Revision 1.1  2003/09/04 17:41:21  rwiegand
 * Added polygon utilities.  When iterating over images, allow specifying a
 * region (less than the whole image), or the step size (other than every pixel).
 * Allow resizing a FITSHeader memory buffer.
 *
 * Revision 1.3  2003/04/02 15:57:16  rwiegand
 * Added functions to get previous and next vertices.  Grew PIState object
 * for sleeker advance implementation.
 *
 * Revision 1.2  2003/04/01 22:52:10  rwiegand
 * Maintain vertices as both linked list and array.  Implemented
 * polygon intersection.
 *
 * Revision 1.1  2003/04/01 00:36:50  rwiegand
 * Initial revision
 *
 */

#include <stdio.h>
#include <assert.h>

#include "polygon.h"



Vertex *
polygon_set_space (Polygon * p, Vertex * v, int n)
{
  Vertex * o = p->v;
  p->v = v;
  p->space = n;
  p->count = 0;
  return o;
}


Vertex *
polygon_vertex (const Polygon * p, int u)
{
  int i = u % p->count;
  Vertex * v = p->v + i;
  return v;
}


Vertex *
polygon_head (const Polygon * p)
{
  Vertex * head = polygon_vertex(p, 0);
  return head;
}


Vertex *
polygon_next (const Polygon * p, const Vertex * v)
{
  Vertex * next = polygon_vertex(p, v->i + 1);
  return next;
}


Vertex *
polygon_prev (const Polygon * p, const Vertex * v)
{
  Vertex * prev = polygon_vertex(p, v->i + p->count - 1);
  return prev;
}


void
polygon_clear (Polygon * p)
{
  p->count = 0;
}


void
polygon_add_vertex (Polygon * p, double x, double y)
{
  Vertex * v = p->v + p->count;

  assert(p->count < p->space);

  v->p = &v->d;
  v->p->x = x;
  v->p->y = y;
  v->i = p->count;

  ++p->count;
}



/* Reverses the vertices, in order to get a ccw orientation
 * 1234 becomes 1432
 */
void
polygon_reverse_orientation (Polygon * p)
{
  int i1 = 1;
  int i2 = p->count - 1;

  while (i1 < i2)
    {
      Point tmp;
      Vertex * v1, * v2;

      v1 = polygon_vertex(p, i1);
      v2 = polygon_vertex(p, i2);

      tmp = v1->d;
      v1->d = v2->d;
      v2->d = tmp;

      ++i1;
      --i2;
    }
}


/* Returns area of a polygon formed by the list of vertices
 */
double
polygon_area (const Polygon * p)
{
  double area;
  double sum = 0;
  Vertex * v0 = polygon_head(p);	/* fixed */
  int i;
  for (i = 1; i < p->count; ++i)
    {
      Vertex * a1 = polygon_vertex(p, i);
      Vertex * a2 = polygon_vertex(p, i + 1);
      sum += point_area2(v0->p, a1->p, a2->p);
    }

  area = sum / 2;
  return area;
}


/* Determine if the polygon/list is oriented counterclockwise (ccw).
 * (A more efficient method is possible, but here we use the available
 * area())
 */
int
polygon_is_ccw (const Polygon * p)
{
  double area = polygon_area(p);
  if (area > 0)
    return 1;
  else
    return 0;
}


int
polygon_is_convex (const Polygon * p)
{
  int flag = 1;
  int i;
  Vertex * v = polygon_head(p);

  for (i = 1; i < p->count; ++i)
    {
      Vertex * vp = polygon_vertex(p, i);
      Vertex * vq = polygon_vertex(p, i + 1);
      if (!point_is_leftOn(v->p, vp->p, vq->p))
        {
          flag = 0;
          break;
        }
      v = vp;
    }

  return flag;
}


void
polygon_print (const Polygon * p)
{
  int i;
  for (i = 0; i < p->count; ++i)
    {
      Vertex * v = polygon_vertex(p, i);
      vertex_print_tag(v, i);
      printf("\n");
    }
}


void
polygon_print_detailed (const Polygon * p)
{
  int i;
  for (i = 0; i < p->count; ++i)
    {
      Vertex * v = polygon_vertex(p, i);
      printf("V%d | vnum=%d\n", i + 1, v->i);
      point_print(v->p);
      printf("\n");
    }
}



/*
 * polygon intersection
 */

static int PI_CHATTER;

int
set_intersection_chatter (int i)
{
  int o = PI_CHATTER;
  PI_CHATTER = i;
  return o;
}



/* polygon intersection state */
typedef struct
{
  Polygon * p;
  Polygon * q;
  Polygon * i;
  int aa;
  int ba;
  int inFlag;                   /* inside/outside state */

} PIState;

enum
{
  COUNTER_AA,
  COUNTER_BA
};

enum
{
  FLAG_Unknown,
  FLAG_Pin,
  FLAG_Qin
};


/* Advances and prints out an inside vertex if appropriate.
 */

static Vertex *
advance (PIState * state, int counter, Vertex * v)
{
  Vertex * next;
  int inside = 0;

  if (counter == COUNTER_AA)
    {
      ++state->aa;
      next = polygon_next(state->p, v);
      inside = state->inFlag == FLAG_Pin;
    }
  else
    {
      ++state->ba;
      next = polygon_next(state->q, v);
      inside = state->inFlag == FLAG_Qin;
    }

  if (inside)
    polygon_add_vertex(state->i, v->p->x, v->p->y);

  return next;
}


/* sets c to a - b */
static void
point_subtract (const Point * a, const Point * b, Point * c)
{
  c->x = a->x - b->x;
  c->y = a->y - b->y;
}


/* Returns the dot product of the two input vectors.
 */
static double
point_dot (const Point * a, const Point * b)
{
  double sum = a->x * b->x + a->y * b->y;
  return sum;
}


static void
insert_shared_segment (Polygon * pgon, const Point * p, const Point * q)
{
  polygon_add_vertex(pgon, p->x, p->y);
  polygon_add_vertex(pgon, q->x, q->y);
}


static int
convex_intersection (Polygon * P, Polygon * Q, Polygon * I)
{
  int code = POLYGON_INT_VOID;

  int n = P->count;
  int m = Q->count;

  Vertex * a, * b;
  Vertex * a1, * b1;

  Point ORIGIN = { 0, 0 };
  Point A, B;                   /* directed edges on P and Q */
  Point p, q;                   /* the first point */
  Point p0;                     /* the first point */

  PIState state = { 0 };
  int aHB, bHA;
  int cross;

  int chatter = PI_CHATTER;
  int firstPoint = 1;
  int pitest;

  state.p = P;
  state.q = Q;
  state.i = I;
  state.inFlag = FLAG_Unknown;
  a = polygon_head(P);
  b = polygon_head(Q);

  do
    {
      if (chatter > 1)
        printf("\nBefore advance: a=%e,%e; b=%e,%e\n"
               "\taa=%d, ba=%d; inFlag=%d\n",
               a->p->x, a->p->y, b->p->x, b->p->y,
               state.aa, state.ba, state.inFlag);

      /*
       * Computations of key variables. 
       */
      a1 = polygon_prev(P, a);
      b1 = polygon_prev(Q, b);

      point_subtract(a->p, a1->p, &A);
      point_subtract(b->p, b1->p, &B);

      cross = point_areaSign(&ORIGIN, &A, &B);
      /*
       * sign of z-component of A x B 
       */

      aHB = point_areaSign(b1->p, b->p, a->p);
      bHA = point_areaSign(a1->p, a->p, b->p);

      if (chatter > 1)
        printf("cross=%d, aHB=%d, bHA=%d\n", cross, aHB, bHA);

      /*
       * If A & B intersect, update inflag. 
       */
      pitest = point_intersect(a1->p, a->p, b1->p, b->p, &p, &q);

      if (chatter > 1)
        printf("intersect: pitest = %d\n", pitest);

      if (pitest == POINT_INT_PROPER || pitest == POINT_INT_VERTEX)
        {
          if (state.inFlag == FLAG_Unknown && firstPoint)
            {
              state.aa = state.ba = 0;
              firstPoint = 0;
              p0.x = p.x;
              p0.y = p.y;
            }

          polygon_add_vertex(state.i, p.x, p.y);

          /*
           * Update inflag. 
           */
          if (aHB > 0)
              state.inFlag = FLAG_Pin;
          else if (bHA > 0)
              state.inFlag = FLAG_Qin;

          if (chatter > 1)
            printf("toggleInOut => inFlag=%d\n", state.inFlag);
        }

      /*-----Advance rules-----*/

      /*
       * Special case: A & B overlap and oppositely oriented. 
       */
      if ((pitest == POINT_INT_OVERLAP) && (point_dot(&A, &B) < 0))
        {
          insert_shared_segment(I, &p, &q);
          return POLYGON_INT_VOID;
        }

      /*
       * Special case: A & B parallel and separated. 
       */
      if ((cross == 0) && (aHB < 0) && (bHA < 0))
        {
          if (chatter)
            printf("P and Q are disjoint.\n");
          polygon_clear(I);
          return POLYGON_INT_DISJOINT;
        }

      /*
       * Special case: A & B collinear. 
       */
      else if ((cross == 0) && (aHB == 0) && (bHA == 0))
        {
          /*
           * Advance but do not output point. 
           */
          if (state.inFlag == FLAG_Pin)
            b = advance(&state, COUNTER_BA, b);
          else
            a = advance(&state, COUNTER_AA, a);
        }

      /*
       * Generic cases. 
       */
      else if (cross >= 0)
        {
          if (bHA > 0)
            a = advance(&state, COUNTER_AA, a);
          else
            b = advance(&state, COUNTER_BA, b);
        }

      else
        {
          /*
           * cross < 0 
           */
          if (aHB > 0)
            b = advance(&state, COUNTER_BA, b);
          else
            a = advance(&state, COUNTER_AA, a);
        }

      if (chatter > 1)
        printf("After advance: a=%e,%e; b=%e,%e\n"
               "\taa=%d, ba=%d, inFlag=%d\n",
               a->p->x, a->p->y, b->p->x, b->p->y,
               state.aa, state.ba, state.inFlag);

      /*
       * Quit when both adv. indices have cycled, or one has cycled twice. 
       */
    }
  while (((state.aa < n) || (state.ba < m)) && (state.aa < 2 * n)
         && (state.ba < 2 * m));

  /*
   * Deal with special cases: not implemented. 
   */
  if (state.inFlag == FLAG_Unknown)
    {
      code = POLYGON_INT_DISJOINT;
      polygon_clear(I);
      if (chatter)
        printf("The boundaries of P and Q do not cross.\n");
    }

  if (I->count == 0)
    {
      code = POLYGON_INT_VOID;
      if (chatter)
        printf("The intersection polygon is empty.\n");
    }
	else if (I->count > P->count + Q->count)
    {
	    printf("excessive intersections!\nP is\n");
	    polygon_print(P);
	    printf("Q is\n");
	    polygon_print(Q);
    }
	else if (I->count > 0)
		code = POLYGON_INT_PROPER;

  return code;
}


int
polygon_intersect (Polygon * p, Polygon * q, Polygon * i)
{
  if (!polygon_is_ccw(p))
    polygon_reverse_orientation(p);

  if (!polygon_is_ccw(q))
    polygon_reverse_orientation(q);

  if (!polygon_is_convex(p) || !polygon_is_convex(q))
    {
      if (PI_CHATTER)
        printf("polygons are not convex...\n");
      return POLYGON_INT_NOT_CONVEX;
    }

  polygon_clear(i);

  return convex_intersection(p, q, i);
}


