/*
 * $Source: /headas/headas/attitude/lib/rew/point.c,v $
 * $Revision: 1.2 $
 * $Date: 2004/09/20 18:29:33 $
 *
 *	Translation of O'Rourke's cPointi/cPointd
 *
 * $Log: point.c,v $
 * Revision 1.2  2004/09/20 18:29:33  rwiegand
 * Prematurely removed point/vertex/polygon from library.
 *
 * Revision 1.1  2003/09/04 17:41:21  rwiegand
 * Added polygon utilities.  When iterating over images, allow specifying a
 * region (less than the whole image), or the step size (other than every pixel).
 * Allow resizing a FITSHeader memory buffer.
 *
 */


#include <stdio.h>
#include <math.h>

#include "polygon.h"




#ifdef NOT_USED
/* Exclusive or: true iff exactly one argument is true.
 */
static int
xor (int x, int y)
{
  /* The arguments are negated to ensure that they are 0/1 values. */
  /* (Idea due to Michael Baldwin.) */
  return !x ^ !y;
}
#endif


static void
assign (Point * p, const Point * a)
{
  p->x = a->x;
  p->y = a->y;
}


double
point_distance (const Point * p, const Point * q)
{
  double d = hypot(p->x - q->x, p->y - q->y);
  return d;
}


#if 0
double
point_area2 (const Point * a, const Point * b, const Point * c)
{
  double area = ((c->x - b->x) * (a->y - b->y))
      - ((a->x - b->x) * (c->y - b->y));
  return area;
}
#endif


int
point_areaSign (const Point * a, const Point * b, const Point * c)
{
  int sign;

  double area2 = point_area2(a, b, c);

  if (area2 > 0)
    sign = 1;
  else if (area2 < 0)
    sign = -1;
  else
    sign = 0;

  return sign;
}


int
point_is_collinear (const Point * a, const Point * b, const Point * c)
{
  return point_areaSign(a, b, c) == 0;
}


int
point_is_left (const Point * a, const Point * b, const Point * c)
{
  return point_areaSign(a, b, c) > 0;
}


int
point_is_leftOn (const Point * a, const Point * b, const Point * c)
{
  return point_areaSign(a, b, c) >= 0;
}



/* Returns true iff point c lies on the closed segment ab.
 * First checks that c is collinear with a and b.
 */
int
point_is_between (const Point * a, const Point * b, const Point * c)
{
  int between;

  if (!point_is_collinear(a, b, c))
    {
      between = 0;
    }
  else if (a->x != b->x)
    {
      /* If ab not vertical, check betweenness on x; else on y. */
      between = ((a->x <= c->x) && (c->x <= b->x))
                 || ((a->x >= c->x) && (c->x >= b->x));
    }
  else
    {
      between = ((a->y <= c->y) && (c->y <= b->y))
                 || ((a->y >= c->y) && (c->y >= b->y));
    }

  return between;
}


#ifdef NOT_USED
static int
intersectProper (const Point * a, const Point * b,
		 const Point * c, const Point * d)
{
  int code;

  /* Eliminate improper cases. */
  if (point_is_collinear(a, b, c)
      || point_is_collinear(a, b, d)
      || point_is_collinear(c, d, a)
      || point_is_collinear(c, d, b))
    code = 0;

  else
    code = xor(point_is_left(a, b, c), point_is_left(a, b, d))
        && xor(point_is_left(c, d, a), point_is_left(c, d, b));

  return code;
}


/* Returns TRUE iff segments ab & cd intersect, properly or improperly.
 */
static int
intersectAny (const Point * a, const Point * b, const Point * c,
	      const Point * d)
{
  int code;

  if (intersectProper(a, b, c, d))
    code = 1;

  else if (point_is_between(a, b, c)
      || point_is_between(a, b, d)
      || point_is_between(c, d, a)
      || point_is_between(c, d, b))

    code = 1;

  else
    code = 0;

  return code;
}
#endif



/* Returns TRUE iff point c lies on the closed segement ab.
 * Assumes it is already known that abc are collinear.
 * (This is the only difference with isBetween().)
 */
static int
isBetween1 (const Point * a, const Point * b, const Point * c)
{
  int between;

  /* If ab not vertical, check betweenness on x; else on y. */
  if (a->x != b->x)
    between = ((a->x <= c->x) && (c->x <= b->x))
               || ((a->x >= c->x) && (c->x >= b->x));
  else
    between = ((a->y <= c->y) && (c->y <= b->y))
               || ((a->y >= c->y) && (c->y >= b->y));

  return between;
}


static int
intersectParallel (const Point * a, const Point * b, const Point * c,
		   const Point * d, Point * p, Point * q)
{
  int code;

  if (!point_is_collinear(a, b, c))
    code = POINT_INT_NONE;

  else if (isBetween1(a, b, c) && isBetween1(a, b, d))
    {
      assign(p, c);
      assign(q, d);
      code = POINT_INT_OVERLAP;
    }

  else if (isBetween1(c, d, a) && isBetween1(c, d, b))
    {
      assign(p, a);
      assign(q, b);
      code = POINT_INT_OVERLAP;
    }

  else if (isBetween1(a, b, c) && isBetween1(c, d, b))
    {
      assign(p, c);
      assign(q, b);
      code = POINT_INT_OVERLAP;
    }

  else if (isBetween1(a, b, c) && isBetween1(c, d, a))
    {
      assign(p, c);
      assign(q, a);
      code = POINT_INT_OVERLAP;
    }

  else if (isBetween1(a, b, d) && isBetween1(c, d, b))
    {
      assign(p, d);
      assign(q, b);
      code = POINT_INT_OVERLAP;
    }

  else if (isBetween1(a, b, d) && isBetween1(c, d, a))
    {
      assign(p, d);
      assign(q, a);
      code = POINT_INT_OVERLAP;
    }

  else
    code = POINT_INT_NONE;

  return code;
}


int
point_intersect (const Point * a, const Point * b, const Point * c,
		 const Point * d, Point * p, Point * q)
{
  double s, t;         /* The two parameters of the parametric eqns. */
  double num, denom;   /* Numerator and denoninator of equations. */
  int code = -1;       /* Return code characterizing intersection. */

  denom = a->x * (d->y - c->y) + b->x * (c->y - d->y)
          + d->x * (b->y - a->y) + c->x * (a->y - b->y);

  /* If denom is zero, then segments are parallel: handle separately. */
  if (denom == 0.0)
    return intersectParallel(a, b, c, d, p, q);

  num = a->x * (d->y - c->y) + c->x * (a->y - d->y) + d->x * (c->y - a->y);

  if ((num == 0.0) || (num == denom))
    code = POINT_INT_VERTEX;

  s = num / denom;

#ifdef POINT_DEBUG
  printf ("intersect: num=%e, denom=%e, s=%e\n", num, denom, s);
#endif

  num = -(a->x * (c->y - b->y) + b->x * (a->y - c->y) + c->x * (b->y - a->y));

  if ((num == 0.0) || (num == denom))
    code = POINT_INT_VERTEX;
  t = num / denom;
#ifdef POINT_DEBUG
  printf ("intersect: num=%e, denom=%e, t=%e\n", num, denom, t);
#endif

  if ((0.0 < s) && (s < 1.0) && (0.0 < t) && (t < 1.0))
    code = POINT_INT_PROPER;
  else if ((0.0 > s) || (s > 1.0) || (0.0 > t) || (t > 1.0))
    code = POINT_INT_NONE;

  p->x = a->x + s * (b->x - a->x);
  p->y = a->y + s * (b->y - a->y);

  return code;
}


void
point_print (const Point * p)
{
  printf("[%f, %f]", p->x, p->y);
}

