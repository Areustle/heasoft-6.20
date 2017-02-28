/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotmodmap/vecmat.h,v $
 * $Revision: 1.2 $
 * $Date: 2002/08/05 13:20:02 $
 *
 * $Log: vecmat.h,v $
 * Revision 1.2  2002/08/05 13:20:02  rwiegand
 * Optimizations for IntMatrix.  Allow storing data as bytes instead of words.
 * Sped up int_matrix_section by accessing data directly instead of using
 * get_int_matrix/set_int_matrix.
 *
 * Revision 1.3  2002/08/05 12:52:52  wiegand
 * Added macro to store data in bytes instead of words.
 *
 * Revision 1.2  2002/08/02 19:06:53  wiegand
 * Cache count value
 *
 * Revision 1.1  2002/08/02 12:40:35  wiegand
 * Initial revision
 *
 */

#ifndef VECMAT_H
#define VECMAT_H


#define INT_MATRIX_WORD


typedef struct
{
  int iMin;
  int iMax;
  double *data;
} RealVector;


typedef unsigned char byte;

struct IntMatrix;
typedef struct IntMatrix IntMatrix;


struct IntMatrix
{
  int xMin;
  int xMax;
  int yMin;
  int yMax;

#ifdef INT_MATRIX_WORD
  int *data;
#elif defined(INT_MATRIX_BYTE)
  byte *data;
#else
  byte *data;
  int bytes;
#endif

  int xSize;
  int ySize;

  /* for aliased matrices */
  int xDimension;
  int yDimension;
  int xAlias;
  int yAlias;

  int count;
  IntMatrix *alias;

};


typedef struct
{
  int xMin;
  int xMax;
  int yMin;
  int yMax;
  double *data;

  int xSize;
  int ySize;

  /* for aliased matrices */
  int xDimension;
  int yDimension;
  int xAlias;
  int yAlias;

} RealMatrix;


typedef void IntFunctor (IntMatrix *, void *info, int x, int y);
typedef void RealFunctor (RealMatrix *, void *info, int x, int y);


IntMatrix * create_int_matrix (int xMin, int xMax, int yMin, int yMax);
void destroy_int_matrix (IntMatrix * m);
void set_int_matrix (IntMatrix * m, int x, int y, int value);
int get_int_matrix (const IntMatrix * m, int x, int y);

void alias_int_matrix (IntMatrix * alias, IntMatrix * m,
    int xMin, int xMax, int yMin, int yMax);


RealVector * create_real_vector (int iMin, int iMax);
void destroy_real_vector (RealVector * v);
void set_real_vector (RealVector * v, int i, double value);
double get_real_vector (const RealVector * v, int i);


void int_matrix_section (IntMatrix * m, IntMatrix * o, int xMin, int yMin);

RealMatrix * create_real_matrix (int xMin, int xMax, int yMin, int yMax);
void destroy_real_matrix (RealMatrix * m);

void set_real_matrix (RealMatrix * m, int x, int y, double value);
double get_real_matrix (const RealMatrix * m, int x, int y);

void real_matrix_section (RealMatrix * m, RealMatrix * o, int xMin, int yMin);


/* iteration */
void iterate_int_matrix (IntMatrix * m, IntFunctor * f, void *info);
void iterate_int_matrix_step (IntMatrix * m, IntFunctor * f, void *info,
    int dx, int dy);

void iterate_set_true (IntMatrix * m, void *info, int x, int y);
void iterate_set_false (IntMatrix * m, void *info, int x, int y);
void iterate_set_same (IntMatrix * m, void *info, int x, int y);
void iterate_set_constant (IntMatrix * m, void *info, int x, int y);

void iterate_count (IntMatrix * m, void *info, int x, int y);

int count (IntMatrix * m);
int countStep (IntMatrix * m, int dx, int dy);
int all (IntMatrix * m);


typedef struct
{
  double sum;
  RealMatrix *real;
} IterateSumMask;

void iterate_sum_mask (IntMatrix * m, void *info, int x, int y);
double sumMask (RealMatrix * r, IntMatrix * m);
double sumMaskStep (RealMatrix * r, IntMatrix * m, int dx, int dy);


void iterate_real_matrix (RealMatrix * m, RealFunctor * f, void *info);

void copy_real_matrix (RealMatrix * m, RealMatrix * o);

void iterate_sum (RealMatrix * m, void *info, int x, int y);

double sum (RealMatrix * m);


typedef struct
{
  double scale;
  int xMin;
  int yMin;
  int accumulate;
  RealMatrix *output;
} IterateScale;

void iterate_scale (RealMatrix * m, void *info, int x, int y);

double sum_range (RealMatrix * m, int xMin, int xMax, int yMin, int yMax);

void iterate_print (RealMatrix * m, void *info, int x, int y);


#define DESTROY_INT_MATRIX(m) { destroy_int_matrix(m); m = 0; }
#define DESTROY_REAL_VECTOR(v) { destroy_real_vector(v); v = 0; }
#define DESTROY_REAL_MATRIX(m) { destroy_real_matrix(m); m = 0; }


#endif

