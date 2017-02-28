#ifndef POLYGON_H
#define POLYGON_H

/*
 * $Source: /headas/headas/attitude/lib/rew/polygon.h,v $
 * $Revision: 1.1 $
 * $Date: 2003/09/04 17:41:21 $
 *
 *	Structures for and interface to O'Rourke's polygon functions.
 *
 * $Log: polygon.h,v $
 * Revision 1.1  2003/09/04 17:41:21  rwiegand
 * Added polygon utilities.  When iterating over images, allow specifying a
 * region (less than the whole image), or the step size (other than every pixel).
 * Allow resizing a FITSHeader memory buffer.
 *
 */


typedef struct
{
  double x;
  double y;

} Point;



typedef struct
{
  int i;
  Point d;
  Point *p;
} Vertex;



typedef struct
{
  int count;                    /* 0 means empty; 1 means one vertex; etc. */
  Vertex *v;
  int space;                    /* number of Vertex's pointed to by v */

} Polygon;



double point_distance (const Point * p, const Point * q);


/* the signed area of the triangle abc, positive if ccw, negative if cw */
double point_area2 (const Point * a, const Point * b, const Point * c);
int point_areaSign (const Point * a, const Point * b, const Point * c);


/* Returns true iff c is strictly to the left of the directed
 * line through a to b.
 */
int point_is_left (const Point * a, const Point * b, const Point * c);
int point_is_leftOn (const Point * a, const Point * b, const Point * c);
int point_is_collinear (const Point * a, const Point * b, const Point * c);

/* Returns true iff point c lies on the closed segement ab.
 * First checks that c is collinear with a and b.
 */
int point_is_between (const Point * a, const Point * b, const Point * c);


/* Returns TRUE iff segments ab & cd intersect, properly or improperly.
 */
enum
{
  POINT_INT_NONE,
  POINT_INT_OVERLAP,
  POINT_INT_VERTEX,
  POINT_INT_PROPER
};

/* intersect: Finds the point(s) of intersection p between two closed
   segments ab and cd.  Returns p and one of the Intersect constants

	OVERLAP : The segments collinearly overlap, sharing a point.
	VERTEX: An endpoint (vertex) of one segment is on the other segment,
		but OVERLAP doesn't hold.
	PROPER: The segments intersect properly (i.e., they share a
		point and Intersect.neither OVERLAP nor VERTEX holds).
	NONE: The segments do not intersect (i.e., they share no points).

	Note that two collinear segments that share just one point, an endpoint
	of each, returns OVERLAP rather than VERTEX as one might expect.
 */

int point_intersect (const Point * a, const Point * b,
		     const Point * c, const Point * d, Point * p, Point * q);

void point_print (const Point * p);


void vertex_print (const Vertex * v);
void vertex_print_tag (const Vertex * v, int tag);


Vertex * polygon_set_space (Polygon * p, Vertex * v, int space);
void polygon_clear (Polygon * p);

Vertex * polygon_vertex (const Polygon * p, int i);

Vertex * polygon_head (const Polygon * p);
Vertex * polygon_next (const Polygon * p, const Vertex * v);
Vertex * polygon_prev (const Polygon * p, const Vertex * v);

void polygon_add_vertex (Polygon * p, double x, double y);

/* Determine if the polygon/list is oriented counterclockwise (ccw).
 */
int polygon_is_ccw (const Polygon * p);
int polygon_is_convex (const Polygon * p);
void polygon_reverse_orientation (Polygon * p);

double polygon_area (const Polygon * p);

void polygon_print (const Polygon * p);
void polygon_print_flags (const Polygon * p, int flags);


enum
{
  POLYGON_INT_VOID,
  POLYGON_INT_NOT_CONVEX,
  POLYGON_INT_PROPER,
  POLYGON_INT_DISJOINT
};

int polygon_intersect (Polygon * p, Polygon * q, Polygon * i);


#endif
