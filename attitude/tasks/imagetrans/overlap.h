#ifndef OVERLAP_H
#define OVERLAP_H


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

  double weight;

} OverlapState;


typedef void (*OverlapCallback)(int x, int y, OverlapState * state);

int iterate_overlap (const Quad * q, OverlapCallback callback, void * info);


#endif
