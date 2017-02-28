#ifndef OVERLAP_H
#define OVERLAP_H


enum
{
  OVERLAP_EMPTY = 1,
  OVERLAP_DUMMY
};


typedef struct
{
  double x, y;

} Point;


typedef struct
{
  Point a, b, c, d;

} Quad;


typedef struct
{
  void * user;

  double weight; /* fraction of the input pixel in this output pixel */
  double output; /* fraction of output pixel overlapped by input pixel */

} OverlapState;


typedef void (*OverlapCallback)(int x, int y, OverlapState * state);

double point_area2 (const Point * a, const Point * b, const Point * c);
double quad_area (const Quad * q);

int iterate_overlap (const Quad * q, OverlapCallback callback, void * info);


#endif
