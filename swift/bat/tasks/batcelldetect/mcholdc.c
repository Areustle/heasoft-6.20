#include <fitsio.h>
#include <math.h>
#include "imageutils.h"

/*
 * MCHOLDC - Cholesky decomposition of square symmetric array
 * Perform a Cholesky decomposition of the image array A,
 * which must be a square symmetric positive definite
 * matrix.  
 *
 * The factorization is:
 *    A = L L^T
 * where L is lower triangular.
 * 
 * Only the upper triangle of A needs to be filled upon input
 * (the lower triangle is ignored).
 * 
 * Upon return, the lower triangle of A contains the factorization L.
 * The diagonal elements of the factorization are stored in 
 * the n+1 row of A.
 *
 * struct image_struct *a - pointer to NxN matrix, which should actually 
 *      be an Nx(N+1) image.  Upon input, only the upper triangle of
 *      a is used.  Upon output, the lower triangle of a contains
 *      the factorization, and a->data[N] contains the diagonal
 *      elements.
 * 
 * RETURNS: -1 if the matrix is not positive definite;
 *          -2 if the matrix is not Nx(N+1);
 *          0 for success.
 */
int mcholdc(int n, double *a, double *p)
{
  int i,j,k;
  double sum;
  double *ai, *aj;

  for (i=0; i<n; i++) {
    ai = a + i*n;
    for (j=i; j<n; j++) {
      aj = a + j*n;
      
      sum=ai[j];
      for (k=i-1; k>=0; k--) {
	sum -= ai[k]*aj[k];
      }
      if (i == j) {
	if (sum <= 0.0) return -1;
	p[i]=sqrt(sum);
      } else {
	aj[i] = sum/p[i];
      }
    }
  }
  return 0;
}

/*
 * MCHOLSOL - solve L x = b Cholesky equation
 *
 * struct image_struct *a - image a which has been factored
 *   with MCHOLDC (should be an Nx(N+1) image where N is 
 *   then number of variables).
 * double *b - pointer to an N-vector; the right hand side
 *   of the equation to be solved.
 * double *x - pointer to an N-vector; the solution to be
 *   returned.
 * NOTE: b and x can point to the same vector.
 *
 * RETURNS: -2 if a has the wrong dimensions
 *          0 upon success.
 */
int mcholsol(int n, double *a, double *p, 
	     double *b, double *x)
{
  int i, k;
  double *ai;
  double sum;
 
  /* Solve L   y = b, storing y in x. */

  for (i=0; i<n; i++) { 
    ai = a + i*n;
    sum = b[i];
    for (k=i-1; k>=0; k--) sum -= ai[k]*x[k];
    x[i] = sum/p[i];
  }

  return 0;
}
