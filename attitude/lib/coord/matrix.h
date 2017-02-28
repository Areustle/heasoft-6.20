#ifndef MATRIX_INCLUDED

typedef struct {

double** a;
int n;

int* index;


} MATRIX;

/*****************************************************************************
* create a new MATRIX structure
******************************************************************************/
MATRIX* allocateMatrix(int n);


/*****************************************************************************
* free the memory for a MATRIX structure
******************************************************************************/
void destroyMatrix(MATRIX* matrix);

/*****************************************************************************
* make a new matrix with the same contents as the old one
*****************************************************************************/
MATRIX* cloneMatrix(MATRIX* matrix);

/********************************************************************************
* perform L-U decomposition on a matrix
********************************************************************************/
void decomposeMatrix(MATRIX* matrix);

/********************************************************************************
* Solve a set of linear equations given an L-U decomposed matrix and a vector
* giving the values on the "right hand side" of the equation.
* The solution is stored in the "right hand side"  array, obilterating the
* original contents
********************************************************************************/
void solveMatrix(MATRIX* matrix, double* b);


#define MATRIX_INCLUDED
#endif /* MAXTRIX_INCLUDED */
