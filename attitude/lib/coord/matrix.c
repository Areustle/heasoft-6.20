/*******************************************************************************
* This file contains some code for doing simple linear algebra. 
* It's very old code dating back to my thesis and based on stuff pirated
* from Numerical Recipies
* -ED 2003-06-03
*******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "matrix.h"

/*****************************************************************************
* create a new MATRIX structure
******************************************************************************/
MATRIX* allocateMatrix(int n) {

MATRIX* matrix;
double** a;
int i;

matrix = (MATRIX*)malloc(sizeof(MATRIX));

matrix->n = n;

a = (double**)malloc(sizeof(double*)*n);
*a = (double*)malloc(sizeof(double)*n*n);
for(i=1; i<n; ++i) {
    a[i] = a[i-1] + n;
}

matrix->a = a;

matrix->index = (int*)malloc(sizeof(int)*n);

return matrix;

} /* end of allocateMatrix function */

/*****************************************************************************
* make a new matrix with the same contents as the old one
*****************************************************************************/
MATRIX* cloneMatrix(MATRIX* matrix) {

int i,j;

MATRIX* clone;

clone = allocateMatrix(matrix->n);

for(j=0; j<matrix->n; ++j) {
    for(i=0; i<matrix->n; ++i) {
        clone->a[j][i] = matrix->a[j][i];
    }

    clone->index[j] = matrix->index[i];
}


return clone;

} /* end of cloneMatrix function */



/*****************************************************************************
* free the memory for a MATRIX structure
******************************************************************************/
void destroyMatrix(MATRIX* matrix) {

    free(matrix->index);
    free(*(matrix->a));
    free(matrix->a);
    
    free(matrix);
} /* end of destriyMatrix function */


/********************************************************************************
* perform L-U decomposition on a matrix
********************************************************************************/
void decomposeMatrix(MATRIX* matrix) {
int i,imax,j,k;
double big,dum,sum,temp;
double* vv;

double** a;
int n;
int* indx;

a = matrix->a;
n = matrix->n;
indx = matrix->index;

vv = (double*)malloc(sizeof(double)*n);

/****************************************
* find ther largest element in each row *
****************************************/
for (i=0;i<n;i++) {
     big=0.0;
     for (j=0;j<n;j++) {
          if ((temp=fabs(a[i][j])) > big) big=temp;
     }

     vv[i]=1.0/big;
}

for (j=0;j<n;j++) {
     for (i=0;i<j;i++) {
          sum=a[i][j];
          for (k=0;k<i;k++) sum -= a[i][k]*a[k][j];
          a[i][j]=sum;
     }

     big=0.0;
     imax=0; /* don't need this, but it keeps the compiler quiet */
     for (i=j;i<n;i++) {
          sum=a[i][j];
          for (k=0;k<j;k++) sum -= a[i][k]*a[k][j];
          a[i][j]=sum;

          if( (dum=vv[i]*fabs(sum)) >= big) {
               big=dum;
               imax=i;
          }
     }

     if (j != imax) {
          for (k=0;k<n;k++) {
               dum=a[imax][k];
               a[imax][k]=a[j][k];
               a[j][k]=dum;
          }
          vv[imax]=vv[j];
     }

     indx[j]=imax;
     
     if(a[j][j]==0.0) {
         fprintf(stderr, "Decomposing singular matrix\n");
     }

     dum=1.0/(a[j][j]);
     for (i=j+1;i<n;i++)  a[i][j]=a[i][j]*dum;

}

free(vv);

} /* end of DecomposeMatrix method */

/********************************************************************************
* Solve a set of linear equations given an L-U decomposed matrix and a vector
* giving the values on the "right hand side" of the equation.
* The solution is stored in the "right hand side"  array, obilterating the
* original contents
********************************************************************************/
void solveMatrix(MATRIX* matrix, double* b) {
int i,ii=-1,ip,j;
double sum;
double** a;
int n;
int* indx;

a = matrix->a;
n = matrix->n;
indx = matrix->index;

for (i=0;i<n;i++) {
     ip=indx[i];
     sum=b[ip];
     b[ip]=b[i];
     if (ii+1) {
          for (j=ii;j<=i-1;j++) sum -= a[i][j]*b[j];

     } else if (sum) {
         ii=i;
     }
     b[i]=sum;
}

for (i=n-1;i>=0;i--) {
     sum=b[i];
     for (j=i+1;j<n;j++) sum -= a[i][j]*b[j];
     b[i]=sum/a[i][i];
}
} /* end of solveMatrix function */

