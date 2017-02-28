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

/*
  Revision Log:
  $Log: overlap.h,v $
  Revision 1.1  2016/01/16 00:36:57  rshill
  Copied from attitude/tasks/imagetrans.  The only change
  is renaming the PARAM structure to IMAGETRANS.


*/
