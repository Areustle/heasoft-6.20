#include <math.h>

#include "overlap.h"



typedef struct
{
  int xmin, xmax;
  int ymin, ymax;

} Bounds;



static int
find_bounds (const Quad * quad, Bounds * b)
{
  int code = 0;

  double xmin, xmax, ymin, ymax;        /* bounds of quadrilateral */

  xmin = xmax = quad->a.x;
  ymin = ymax = quad->a.y;

#define UPDATE_BOUNDS(p) { \
  if (quad->p.x < xmin) \
    xmin = quad->p.x; \
  else if (quad->p.x > xmax) \
    xmax = quad->p.x; \
  if (quad->p.y < ymin) \
    ymin = quad->p.y; \
  else if (quad->p.y > ymax) \
    ymax = quad->p.y; \
  }

  UPDATE_BOUNDS(b)
  UPDATE_BOUNDS(c)
  UPDATE_BOUNDS(d)

  b->xmin = (int) floor(xmin + 0.5);
  b->xmax = (int) floor(xmax + 0.5);

  b->ymin = (int) floor(ymin + 0.5);
  b->ymax = (int) floor(ymax + 0.5);

  return code;
}


double
point_area2 (const Point * a, const Point * b, const Point * c)
{
  double area = ((c->x - b->x) * (a->y - b->y))
              - ((a->x - b->x) * (c->y - b->y));
  return area;
}


double quad_area (const Quad * quad)
{
  double area;
  double sum;
  sum = point_area2(&quad->a, &quad->b, &quad->c);
  sum += point_area2(&quad->a, &quad->c, &quad->d);
  area = sum / 2;
  return area;
}


void boxer (int is, int js, double * x, double * y, double * darea);
double sgarea (double x1, double y1, double x2, double y2, int is, int js);


int
iterate_overlap (const Quad * quad, OverlapCallback callback, void * info)
{
  int code = 0;
  int i, j;
  Bounds bounds = { 0 };
  OverlapState state;
  double area, x[4], y[4];

  state.user = info;

  area = quad_area(quad);
  if (area == 0)
    return OVERLAP_EMPTY;

  /* boxer wants x[], y[] to be clockwise */
  x[0] = quad->a.x;
  x[1] = quad->b.x;
  x[2] = quad->c.x;
  x[3] = quad->d.x;

  y[0] = quad->a.y;
  y[1] = quad->b.y;
  y[2] = quad->c.y;
  y[3] = quad->d.y;

  if (area < 0)
    area = -area;
  else
   {
      /* swap b and d so clockwise */
      x[1] = quad->d.x;
      x[3] = quad->b.x;

      y[1] = quad->d.y;
      y[3] = quad->b.y;
   }

  find_bounds(quad, &bounds);

  /* iterate over pixels that could be overlapped */
  for (j = bounds.ymin; j <= bounds.ymax; ++j)
    {
      for (i = bounds.xmin; i <= bounds.xmax; ++i)
        {
          double overlap;
          boxer(i, j, x, y, &overlap);

          if (overlap > 0)
            {
              state.weight = overlap / area;
              state.output = overlap;
              (*callback) (i, j, &state);
            }
        }
    }

  return code;
}



/*
 *  From HST drizzle code:
 *  http://www.stsci.edu/instruments/wfpc2/Wfpc2_driz/wfpc2_driz.html
 *
 *  BOXER -- compute area of box overlap (originally in Fortran)
 *
 *  Calculate the area common to input clockwise polygon x(n), y(n)
 *  with square (is-0.5, js-0.5) to (is+0.5, js+0.5)
 *  Note: convention is for integer value to be at pixel center
 *
 *  (Original boxer last modified 24-APR-1996 Richard Hook ECF)
 */

void boxer(int is, int js, double *x, double *y, double *darea) {
/*
 *  Local variables
 */
   double px[4], py[4], sum;
   int i;
/*
 *  Set up coords relative to unit square at origin
 */
   for ( i = 0; i < 4; i++ ) {
      px[i] = x[i] - (is-0.5);
      py[i] = y[i] - (js-0.5);
   }
/*
 *  For each line in the quadrilateral, calculate the area common to the
 *  unit square (allow negative area for subsequent 'vector' addition
 *  of subarea)
 */
   sum = 0.0;
   for ( i = 0; i < 3; i++ ) {
      sum = sum + sgarea(px[i], py[i], px[i+1], py[i+1], is, js);
   }
   sum = sum + sgarea(px[3], py[3], px[0], py[0], is, js);
   *darea = sum;
}

/*
 *  Used by BOXER
 *  Calculate area under a line segment within unit square at origin
 */
double sgarea(double x1, double y1, double x2, double y2, int is, int js) {
/*
 *  Local variables
 */
   double m, c, dx;
   double xlo, xhi, ylo, yhi, xtop;
   int negdx;

   dx = x2 - x1;
/*
 *  Trap vertical line
 */
   if ( dx == 0.0 ) return 0.0;
/*
 *  Order the two input points in x
 */
   if ( x1 < x2 ) {
      xlo = x1;
      xhi = x2;
   } else {
      xlo = x2;
      xhi = x1;
   }
/*
 *  And determine the bounds ignoring y for now
 */
   if ( xlo >= 1.0 ) return 0.0;
   if ( xhi <= 0.0 ) return 0.0;
   xlo = xlo > 0.0 ? xlo : 0.0;
   xhi = xhi < 1.0 ? xhi : 1.0;
/*
 *  Now look at y, basic info about the line y = mx + c
 */
   negdx = 0;
   if ( dx < 0.0 ) negdx = 1;
   m = (y2 - y1) / dx;
   c = y1 - m * x1;
   ylo = m * xlo + c;
   yhi = m * xhi + c;
/*
 *  Trap segment entirely below axis
 */
   if ( ylo <= 0.0 && yhi <= 0.0 ) return 0.0;
/*
 *  Adjust bounds if segment crosses axis
 *  (to exclude anything below axis)
 */
   if ( ylo < 0.0 ) {
      ylo = 0.0;
      xlo = -c/m;
   }
   if ( yhi < 0.0 ) {
      yhi = 0.0;
      xhi = -c/m;
   }
/*
 *  There are four possibilities: both y below 1, both y above 1
 *  and one of each
 */
   if ( ylo >= 1.0 && yhi >= 1.0 ) {
      /* Line segment is entirely above square */
      if ( negdx ) return (xlo - xhi);
      return (xhi - xlo);
   }
   if ( ylo <= 1.0 && yhi <= 1.0 ) {
      /* Segment entirely within square */
      if ( negdx ) return (0.5 * (xlo-xhi) * (yhi+ylo));
      return (0.5 * (xhi-xlo) * (yhi+ylo));
   }
/*
 *  Otherwise it must cross the top of the square
 */
   xtop = (1.0 - c)/m;
/*
 *  Check for a problem - note that this now has a small error
 *  margin included to avoid errors coming from numerical inaccuracy
 */
   if ( xtop < (xlo - 1.0e-5) || xtop > (xhi + 1.0e-5) ) {
#if 0
      fprintf(stderr,  "Warning: box overlap calculation problem %d %d\n", is, js);
#endif
   }

   if ( ylo < 1.0 ) {
      if ( negdx ) return -(0.5 * (xtop-xlo) * (1.0+ylo) + xhi - xtop);
      return (0.5 * (xtop-xlo) * (1.0+ylo) + xhi - xtop);
   }
   if ( negdx ) return -(0.5 * (xhi-xtop) * (1.0+yhi) + xtop-xlo);
   return (0.5 * (xhi-xtop) * (1.0+yhi) + xtop-xlo);
}

