/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotmodmap/vecmat.c,v $
 * $Revision: 1.7 $
 * $Date: 2004/10/18 15:45:28 $
 *
 * $Log: vecmat.c,v $
 * Revision 1.7  2004/10/18 15:45:28  rwiegand
 * Declare scratch variables static.
 *
 * Revision 1.6  2004/10/13 21:13:27  rwiegand
 * Committing Bob O'Brien's optimizations and sub-windowing support.
 *
 * Revision 1.4  2003/10/24 14:35:44  valett
 * initialized output image to input image to assure the intire output
 * image has meaningful data.  This corrected an error where areas lying
 * outside the input image size mod 4 where not being set.
 *
 * Revision 1.3  2002/08/07 18:07:29  valett
 * added NDEBUG macro definition for optimization.
 *
 * Revision 1.2  2002/08/05 13:20:02  rwiegand
 * Optimizations for IntMatrix.  Allow storing data as bytes instead of words.
 * Sped up int_matrix_section by accessing data directly instead of using
 * get_int_matrix/set_int_matrix.
 *
 * Revision 1.5  2002/08/05 12:53:53  wiegand
 * Partial implementation of bit matrices.  Unroll int_matrix_section loop
 *
 * Revision 1.4  2002/08/02 20:37:45  wiegand
 * Use macros for offsets.  Optimized int_matrix_section.
 *
 * Revision 1.3  2002/08/02 19:11:54  wiegand
 * Include [xy]Min in [xy]Alias.  Allow aliasing matrices with any indexing.
 *
 * Revision 1.1  2002/08/02 12:41:04  wiegand
 * Initial revision
 *
 */

#ifndef NDEBUG
#define NDEBUG
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "vecmat.h"

#define UNROLL_INT_MATRIX_SECTION 1
#define FAST_INT_MATRIX_SECTION 0

#define VECTOR_OFFSET(v, i) \
  (i - v->iMin)

#define MATRIX_OFFSET(m, x, y) \
  (m->xDimension * (y + m->yAlias) + x + m->xAlias)


static IntMatrix scratch;



/*
 * set the count to a value such that it will stay negative (which
 * indicates that it is unknown) during set/get
 */

static void
invalidate_count (IntMatrix *m)
{
  m->count = -(m->xSize * m->ySize + 1);
}


/*
 * IntMatrix implementation
 */

IntMatrix *
create_int_matrix (int xMin, int xMax, int yMin, int yMax)
{
  IntMatrix * m;
  int elements;

  assert(xMin >= 1);
  assert(yMin >= 1);
  assert(xMin < xMax);
  assert(yMin < yMax);
#ifdef MAX_DIMENSION
  assert(xMax <= MAX_DIMENSION);
  assert(yMax <= MAX_DIMENSION);
#endif

  m = (IntMatrix *) malloc(sizeof(IntMatrix));
  assert(m != 0);

  m->xMin = xMin;
  m->xMax = xMax;
  m->yMin = yMin;
  m->yMax = yMax;

  m->xSize = xMax - xMin + 1;
  m->ySize = yMax - yMin + 1;
  m->xDimension = m->xSize;
  m->yDimension = m->ySize;
  m->xAlias = -xMin;
  m->yAlias = -yMin;

#if defined(INT_MATRIX_WORD)
  elements = m->xSize * m->ySize;
  m->data = (int *) calloc(elements ,sizeof(int));
  m->alias = &scratch;
m->count = -(elements + 1);
return m;
#elif defined(INT_MATRIX_BYTE)
  elements = m->xSize * m->ySize;
  m->data = (byte *) malloc(elements);
#else
  elements = m->xSize * m->ySize / 8 + 1;
  m->data = malloc(elements);
#endif
  assert(m->data != 0);

#ifdef INT_MATRIX_ZERO_DATA
  {
    int i;
	int		limit = elements;		/* ROB */
    m->count = 0;
    for (i = 0; i < limit; ++i)
      m->data[i] = 0;
  }
#else
  invalidate_count(m);
#endif

  m->alias = &scratch;

  return m;
}


void
destroy_int_matrix (IntMatrix * m)
{
  assert(m != 0);
  assert(m->data != 0);
  free(m->data);
  free(m);
}


void
set_int_matrix (IntMatrix * m, int x, int y, int value)
{
  int offset;
  assert(x >= m->xMin);
  assert(x <= m->xMax);
  assert(y >= m->yMin);
  assert(y <= m->yMax);

  offset = MATRIX_OFFSET(m, x, y);

  if (m->data[offset])
    {
      if (!value)
        --m->count, --m->alias->count;
    }
  else if (value)
      ++m->count, ++m->alias->count;

  m->data[offset] = value;
}


int
get_int_matrix (const IntMatrix * m, int x, int y)
{
  int value, offset;

  assert(x >= m->xMin);
  assert(x <= m->xMax);
  assert(y >= m->yMin);
  assert(y <= m->yMax);

  offset = MATRIX_OFFSET(m, x, y);

  value = m->data[offset];

  return value;
}



/*
 * alias a section of a matrix
 */

void
alias_int_matrix (IntMatrix * alias, IntMatrix * m,
    int xMin, int xMax, int yMin, int yMax)
{
/* repeated from create_int_matrix */
  assert(xMin >= 1);
  assert(yMin >= 1);
  assert(xMin < xMax);
  assert(yMin < yMax);
#ifdef MAX_DIMENSION
  assert(xMax <= MAX_DIMENSION);
  assert(yMax <= MAX_DIMENSION);
#endif

/* ensure it is inside the parent's bounds */
  assert(xMin >= m->xMin);
  assert(xMax <= m->xMax);
  assert(yMin >= m->yMin);
  assert(yMax <= m->yMax);

  alias->xMin = xMin;
  alias->xMax = xMax;
  alias->yMin = yMin;
  alias->yMax = yMax;

  alias->xSize = xMax - xMin + 1;
  alias->ySize = yMax - yMin + 1;

  alias->xDimension = m->xDimension;
  alias->yDimension = m->yDimension;

  alias->xAlias = -m->xMin;
  alias->yAlias = -m->yMin;

  alias->data = m->data;
  invalidate_count(alias);

  alias->alias = m;
}


/*
 * copies a section of matrix m starting at xMin, yMin to matrix o
 * the amount copied fills o
 */
void
int_matrix_section (IntMatrix * m, IntMatrix * o, int xMin, int yMin)
{
  int x, y;

#if UNROLL_INT_MATRIX_SECTION

#define UNROLL_CHUNK 8

  int count = 0;
  for (y = 0; y < o->ySize; ++y)
    {
      int i, z, chunks;
      chunks = o->xSize / UNROLL_CHUNK;
      for (i = 0; i < chunks; ++i)
        {
          int mOffset, oOffset;

          x = i * UNROLL_CHUNK;
          mOffset = MATRIX_OFFSET(m, x + xMin, y + yMin);
          oOffset = MATRIX_OFFSET(o, x + o->xMin, y + o->yMin);

#define INT_MATRIX_SECTION_STEP \
   { z = m->data[mOffset++]; o->data[oOffset++] = z; count += z; }

          INT_MATRIX_SECTION_STEP;
          INT_MATRIX_SECTION_STEP;
          INT_MATRIX_SECTION_STEP;
          INT_MATRIX_SECTION_STEP;
          INT_MATRIX_SECTION_STEP;
          INT_MATRIX_SECTION_STEP;
          INT_MATRIX_SECTION_STEP;
          INT_MATRIX_SECTION_STEP;
        }

      /* finish up */
      for (x = chunks * UNROLL_CHUNK; x < o->xSize; ++x)
        {
          int z = m->data[MATRIX_OFFSET(m, x + xMin, y + yMin)];
          o->data[MATRIX_OFFSET(o, x + o->xMin, y + o->yMin)] = z;
          count += z;
        }
    }

  o->count = count;

#elif FAST_INT_MATRIX_SECTION

  int count = 0;
  for (y = 0; y < o->ySize; ++y)
    for (x = 0; x < o->xSize; ++x)
      {
        int z = m->data[MATRIX_OFFSET(m, x + xMin, y + yMin)];
        o->data[MATRIX_OFFSET(o, x + o->xMin, y + o->yMin)] = z;
        count += z;
      }

  o->count = count;

#else

  for (x = 0; x < o->xSize; ++x)
    for (y = 0; y < o->ySize; ++y)
      set_int_matrix(o, x + o->xMin, y + o->yMin,
          get_int_matrix(m, x + xMin, y + yMin));

  invalidate_count(o);

#endif
}


/*
 * RealVector implementation
 */

RealVector *
create_real_vector (int iMin, int iMax)
{
  RealVector *v;
  assert(iMin >= 1);
  assert(iMin < iMax);
#ifdef MAX_DIMENSION
  assert(iMax <= MAX_DIMENSION);
#endif
  v = (RealVector *) malloc(sizeof(RealVector));
  assert(v != 0);
  v->iMin = iMin;
  v->iMax = iMax;
  v->data = (double *) malloc((iMax - iMin + 1) * sizeof(double));
  assert(v->data != 0);
  return v;
}


void
destroy_real_vector (RealVector * v)
{
  assert(v != 0);
  assert(v->data != 0);
  free(v->data);
  free(v);
}


void
set_real_vector (RealVector * v, int i, double value)
{
  int offset;
  assert(i >= v->iMin);
  assert(i <= v->iMax);
  offset = VECTOR_OFFSET(v, i);
  v->data[offset] = value;
}


double
get_real_vector (const RealVector * v, int i)
{
  double value;
  int offset;
  assert(i >= v->iMin);
  assert(i <= v->iMax);
  offset = VECTOR_OFFSET(v, i);
  value = v->data[offset];
  return value;
}


/*
 * RealMatrix implementation
 */

RealMatrix *
create_real_matrix (int xMin, int xMax, int yMin, int yMax)
{
  RealMatrix *m;
  assert(xMin >= 1);
  assert(yMin >= 1);
  assert(xMin < xMax);
  assert(yMin < yMax);
#ifdef MAX_DIMENSION
  assert(xMax <= MAX_DIMENSION);
  assert(yMax <= MAX_DIMENSION);
#endif

  m = (RealMatrix *) malloc(sizeof(RealMatrix));
  assert(m != 0);
  m->xMin = xMin;
  m->xMax = xMax;
  m->yMin = yMin;
  m->yMax = yMax;

  m->xSize = xMax - xMin + 1;
  m->ySize = yMax - yMin + 1;
  m->xDimension = m->xSize;
  m->yDimension = m->ySize;
  m->xAlias = -xMin;
  m->yAlias = -yMin;

  m->data = (double *) calloc(m->xSize * m->ySize, sizeof(double));
  assert(m->data != 0);
  return m;
}


void
destroy_real_matrix (RealMatrix * m)
{
  assert(m != 0);
  assert(m->data != 0);
  free(m->data);
  free(m);
}


void
set_real_matrix (RealMatrix * m, int x, int y, double value)
{
  int offset;
  assert(x >= m->xMin);
  assert(x <= m->xMax);
  assert(y >= m->yMin);
  assert(y <= m->yMax);
  offset = MATRIX_OFFSET(m, x, y);
  m->data[offset] = value;
}


double
get_real_matrix (const RealMatrix * m, int x, int y)
{
  double value;
  int offset;
  assert(x >= m->xMin);
  assert(x <= m->xMax);
  assert(y >= m->yMin);
  assert(y <= m->yMax);
  offset = MATRIX_OFFSET(m, x, y);
  value = m->data[offset];
  return value;
}


void
real_matrix_section (RealMatrix * m, RealMatrix * o, int xMin, int yMin)
{
  int x, y;
  for (x = 0; x < o->xSize; ++x)
    for (y = 0; y < o->ySize; ++y)
      set_real_matrix(o, x + o->xMin, y + o->yMin,
        get_real_matrix(m, x + xMin, y + yMin));
}


void
iterate_int_matrix_step (IntMatrix * m, IntFunctor * f, void *info,
    int dx, int dy)
{
  int x, y;
assert(dx > 0);
assert(dy > 0);
  for (x = m->xMin; x <= m->xMax; x += dx)
    for (y = m->yMin; y <= m->yMax; y += dy)
      (*f)(m, info, x, y);
}


void
iterate_int_matrix (IntMatrix * m, IntFunctor * f, void *info)
{
  iterate_int_matrix_step(m, f, info, 1, 1);
}


void
iterate_set_true (IntMatrix * m, void *info, int x, int y)
{
  set_int_matrix(m, x, y, 1);
}


void
iterate_set_false (IntMatrix * m, void *info, int x, int y)
{
  set_int_matrix(m, x, y, 0);
}


void
iterate_set_constant (IntMatrix * m, void *info, int x, int y)
{
  int *p = (int *) info;
  set_int_matrix(m, x, y, *p);
}


void
iterate_set_same (IntMatrix * m, void *info, int x, int y)
{
  int q;
  IntMatrix *o = (IntMatrix *) info;
  q = get_int_matrix(m, x, y);
  set_int_matrix(o, x, y, q);
}


void
iterate_count (IntMatrix * m, void *info, int x, int y)
{
  int *pcount = (int *) info;
  if (get_int_matrix(m, x, y))
    ++*pcount;
}


int
count (IntMatrix * m)
{
  if (m->count < 0)
    {
      int count = 0;
      /* need to calculate count from scratch */
      iterate_int_matrix(m, &iterate_count, &count);
      m->count = count;
    }

  return m->count;
}


int
countStep (IntMatrix * m, int dx, int dy)
{
  int count = 0;
  iterate_int_matrix_step(m, &iterate_count, &count, dx, dy);
  return count;
}


int
all (IntMatrix * m)
{
  int all = (count(m) == m->xSize * m->ySize);
  return all;
}


/*	Calculate the sum of the matrix elements in raw which correspond to
	non-zero elements in m
*/
void
iterate_sum_mask (IntMatrix * m, void *raw, int x, int y)
{
  IterateSumMask *info = (IterateSumMask *) raw;
  if (get_int_matrix(m, x, y))
    info->sum += get_real_matrix(info->real, x, y);
}


double
sumMask (RealMatrix * r, IntMatrix * m)
{
  IterateSumMask info;
  info.sum = 0;
  info.real = r;
  iterate_int_matrix(m, iterate_sum_mask, &info);
  return info.sum;
}


double
sumMaskStep (RealMatrix * r, IntMatrix * m, int dx, int dy)
{
  IterateSumMask info;
  info.sum = 0;
  info.real = r;
  iterate_int_matrix_step(m, iterate_sum_mask, &info, dx, dy);
  return info.sum;
}


void
iterate_real_matrix (RealMatrix * m, RealFunctor * f, void *info)
{
  int x, y;
  for (x = m->xMin; x <= m->xMax; ++x)
    for (y = m->yMin; y <= m->yMax; ++y)
      (*f)(m, info, x, y);
}


void
copy_real_matrix (RealMatrix * m, RealMatrix * o)
{
  int x, y;
  for (x = m->xMin; x <= m->xMax; ++x)
    for (y = m->yMin; y <= m->yMax; ++y)
      set_real_matrix(o, x, y,
        get_real_matrix(m, x, y));
}


void
iterate_sum (RealMatrix * m, void *info, int x, int y)
{
  double *psum = (double *) info;
  *psum += get_real_matrix(m, x, y);
}


double
sum (RealMatrix * m)
{
  double sum = 0.0;
  iterate_real_matrix(m, iterate_sum, &sum);
  return sum;
}


void
iterate_scale (RealMatrix * m, void *raw, int x, int y)
{
  IterateScale *info = (IterateScale *) raw;
  int xp, yp;
  double q;
  q = get_real_matrix(m, x, y) * info->scale;
  xp = x + info->xMin - 1;
  yp = y + info->yMin - 1;
  if (info->accumulate)
    q += get_real_matrix(info->output, xp, yp);
  set_real_matrix(info->output, xp, yp, q);
}


double
sum_range (RealMatrix * m, int xMin, int xMax, int yMin, int yMax)
{
  int x, y;
  double sum = 0;
  for (x = xMin; x <= xMax; ++x)
    for (y = yMin; y <= yMax; ++y)
      sum += get_real_matrix(m, x, y);
  return sum;
}


void
iterate_print (RealMatrix * m, void *info, int x, int y)
{
  FILE *fp = (FILE *) info;
  if (!fp)
    fp = stdout;
  /*fprintf(fp, "x=%d, y=%d, value=%f\n",
      x, y, get_real_matrix(m, x, y));*/
}

