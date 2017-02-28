#ifndef ROTMATRIX_INCLUDED

/****************************************************************************
*****************************************************************************
* Data structure describing a 3x3 matrix which describes an arbitrary
* three-dimensional rotation.
****************************************************************************/
typedef struct {

double m[3][3];

} ROTMATRIX;

/****************************************************************************
*****************************************************************************
* allocate space for a rotation matrix structure
****************************************************************************/
ROTMATRIX* allocateRotMatrix();

/****************************************************************************
*****************************************************************************
* free storage for a rotation matrix structure
****************************************************************************/
void destroyRotMatrix(ROTMATRIX* rot);

/****************************************************************************
*****************************************************************************
* inconsistent values in rotation matrix
* This function prints a message to stderr and exits with status "1"
****************************************************************************/
void badRotMatrixError(ROTMATRIX* rot);

/****************************************************************************
*****************************************************************************
* set an existing rotation matrix structure to the identity matrix
****************************************************************************/
void setRotMatrixToIdentity(ROTMATRIX* rot);

/****************************************************************************
*****************************************************************************
* given a 3-vector vec, calculate the vector newvec after rotating the
* coordinate system by rot.
****************************************************************************/
void applyRotMatrixToVector(ROTMATRIX* rot, double newvec[3], double vec[3] );

#define ROTMATRIX_INCLUDED
#endif /* ROTMATRIX_INCLUDED */
