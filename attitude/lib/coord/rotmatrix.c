#include <stdlib.h>
#include <stdio.h>

#include "rotmatrix.h"

/****************************************************************************
*****************************************************************************
* allocate space for a rotation matrix structure
****************************************************************************/
ROTMATRIX* allocateRotMatrix()
{
ROTMATRIX* rot;

rot=(ROTMATRIX*)malloc(sizeof(ROTMATRIX));

return(rot);

}

/****************************************************************************
*****************************************************************************
* free storage for a rotation matrix structure
****************************************************************************/
void destroyRotMatrix(ROTMATRIX* rot)
{
free(rot);
}

/****************************************************************************
*****************************************************************************
* inconsistent values in rotation matrix
* This function prints a message to stderr and exits with status "1"
****************************************************************************/
void badRotMatrixError(ROTMATRIX* rot)
{
fprintf(stderr,"Ill formed rotation matrix:\n");

fprintf(stderr,"(%g %g %g)\n",rot->m[0][0],rot->m[0][1],rot->m[0][2]);
fprintf(stderr,"(%g %g %g)\n",rot->m[1][0],rot->m[1][1],rot->m[1][2]);
fprintf(stderr,"(%g %g %g)\n",rot->m[2][0],rot->m[2][1],rot->m[2][2]);

exit(1);
}


/****************************************************************************
*****************************************************************************
* set an existing rotation matrix structure to the identity matrix
****************************************************************************/
void setRotMatrixToIdentity(ROTMATRIX* rot) {

rot->m[0][0]=1.;
rot->m[1][1]=1.;
rot->m[2][2]=1.;

rot->m[0][1]=0.;
rot->m[0][2]=0.;
rot->m[1][2]=0.;

rot->m[1][0]=0.;
rot->m[2][0]=0.;
rot->m[2][1]=0.;

} /* end set RotMatrixToIdentity function */

/****************************************************************************
*****************************************************************************
* given a 3-vector vec, calculate the vector newvec after rotating the
* coordinate system by rot.
****************************************************************************/
void applyRotMatrixToVector(ROTMATRIX* rot, double newvec[3], double vec[3] ) {

newvec[0]=vec[0]*rot->m[0][0] + vec[1]*rot->m[0][1] + vec[2]*rot->m[0][2];
newvec[1]=vec[0]*rot->m[1][0] + vec[1]*rot->m[1][1] + vec[2]*rot->m[1][2];
newvec[2]=vec[0]*rot->m[2][0] + vec[1]*rot->m[2][1] + vec[2]*rot->m[2][2];

}

