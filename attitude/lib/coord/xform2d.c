#include <stdio.h>
#include <math.h>
#include "xform2d.h"
#include "matrix.h"

/*
#define DEBUG
*/

/*****************************************************************************
******************************************************************************
* allocate space for an XFORM2D structure
*****************************************************************************/
XFORM2D* allocateXform2d(void)
{
XFORM2D* xform;

xform=(XFORM2D*)malloc(sizeof(XFORM2D));

return(xform);
}

/*****************************************************************************
******************************************************************************
* free space for an XFORM2D structure
*****************************************************************************/
void destroyXform2d(XFORM2D* xform)
{
free(xform);
}

/*****************************************************************************
******************************************************************************
* copy one xform2d into another
*****************************************************************************/
void copyXform2d(XFORM2D* copy, XFORM2D* xform) {

    copy->rot[0][0] = xform->rot[0][0];
    copy->rot[0][1] = xform->rot[0][1];
    copy->rot[1][0] = xform->rot[1][0];
    copy->rot[1][1] = xform->rot[1][1];

    copy->xshift = xform->xshift;
    copy->yshift = xform->yshift;

} /* end of copyXform2d method */

/*****************************************************************************
******************************************************************************
* Perform a transformation on a pair of discrete pixel coordinates.
* dx and dy give the exact location of the event with respect to the 
* center of the pixel. Note that in general the exact location of an 
* event within a pixel is unknown, so the standard practice is to use
* random numbers between -5. and +.5 for dx and dy.
*****************************************************************************/
void applyXform2dToDiscreteCoords(XFORM2D* xform, double* x1, double* y1, 
                                  int i, int j, double dx, double dy)
{

applyXform2dToContinuousCoords(xform,x1,y1, (double)i+dx, (double)j+dy );

#ifdef DEBUG
printf("applyXform2dToDiscreteCoords: i=%d j=%d dx=%g dy=%g x1=%g y1=%g\n",
       i,j,dx,dy,x1,y1);
#endif /* DEBUG */

} /* end of applyXform2dToDiscreteCoords function */


/*****************************************************************************
******************************************************************************
* Perform a transformation on a pair of real-valued coordinates.
*****************************************************************************/
void applyXform2dToContinuousCoords(XFORM2D* xform, double* x1, double* y1,
                                    double x0, double y0 )
{

*x1 = xform->xshift + xform->rot[0][0]*x0 + xform->rot[0][1]*y0;
*y1 = xform->yshift + xform->rot[1][0]*x0 + xform->rot[1][1]*y0;

#ifdef DEBUG
printf("applyXform2dToContinuousCoords: x0=%g y0=%g x1=%g y1=%g\n",
        x0,y0,*x1,*y1);
#endif /* DEBUG */


} /* end of applyXform2dToContinuousCoords */

/*****************************************************************************
******************************************************************************
* transform the difference in position between two sets of coordinates
* This is equivalent to (x1,y1) = trans(x,y) - trans(0,0) .
*****************************************************************************/
void applyXform2dToOffset(XFORM2D* xform, double* x1, double* y1,
                                    double x0, double y0 )
{

*x1 = xform->rot[0][0]*x0 + xform->rot[0][1]*y0;
*y1 = xform->rot[1][0]*x0 + xform->rot[1][1]*y0;


} /* end of applyXform2dToOffset function */
/*****************************************************************************
******************************************************************************
* Find the transform (xform) equivalent to 
* first applying xform1 and then xform2
*****************************************************************************/
void combineXform2ds(XFORM2D* xform,XFORM2D* xform1, XFORM2D* xform2) 
{

xform->xshift = xform2->xshift + xform2->rot[0][0]*xform1->xshift +
                                 xform2->rot[0][1]*xform1->yshift;

xform->yshift = xform2->yshift + xform2->rot[1][0]*xform1->xshift +
                                 xform2->rot[1][1]*xform1->yshift;

xform->rot[0][0] = xform2->rot[0][0]*xform1->rot[0][0] +
                   xform2->rot[0][1]*xform1->rot[1][0];

xform->rot[0][1] = xform2->rot[0][0]*xform1->rot[0][1] +
                   xform2->rot[0][1]*xform1->rot[1][1];

xform->rot[1][0] = xform2->rot[1][0]*xform1->rot[0][0] +
                   xform2->rot[1][1]*xform1->rot[1][0];

xform->rot[1][1] = xform2->rot[1][0]*xform1->rot[0][1] +
                   xform2->rot[1][1]*xform1->rot[1][1];

}


/*****************************************************************************
******************************************************************************
* set the XFORM2D structure for a simple translation, such that the
* old origin will be at new coordinates (deltax, deltay)
*****************************************************************************/
void setXform2dToTranslation(XFORM2D* xform, double deltax, double deltay ) 
{

xform->xshift=deltax;
xform->yshift=deltay;

xform->rot[0][0]=1.;
xform->rot[1][1]=1.;

xform->rot[0][1]=0.;
xform->rot[1][0]=0.;


}

/*****************************************************************************
******************************************************************************
* set the XFORM2D structure for a simple scaling, such that the
* position of the point (x0,y0) remains fixed.
* Note: negative scale factors mean a coordinate inversion.
*****************************************************************************/
void setXform2dToScaling(XFORM2D* xform, double xscale, double yscale,
                         double x0, double y0 )
{
xform->xshift = x0 - xscale*x0;
xform->yshift = y0 - yscale*y0;

xform->rot[0][0]=xscale;
xform->rot[1][1]=yscale;

xform->rot[0][1]=0.;
xform->rot[1][0]=0.;

}

/*****************************************************************************
******************************************************************************
* set the XFORM2D structure for rotation about the point (x0,y0) in the
* old coordinate system. This is the same as translating by (-x0, -y0),
* doing a rotation about the origin, and then translating by (x0,y0).
*****************************************************************************/
void setXform2dToRotation(XFORM2D* xform, double sinang, double cosang, 
                          double x0, double y0)
{


xform->xshift = x0 - cosang*x0 - sinang*y0;
xform->yshift = y0 + sinang*x0 - cosang*y0;


xform->rot[0][0] = cosang;
xform->rot[0][1] = sinang;
xform->rot[1][0] =-sinang;
xform->rot[1][1] = cosang;

} /* end of setXform2dToRotation function */

/*****************************************************************************
******************************************************************************
* modify a transform to be the original followed by a translation.
* This is similar to setXform2dToTranslation followed by combineXform2ds,
* but can be much more efficient.
*****************************************************************************/
void applyTranslationToXform2d(XFORM2D* xform, double deltax, double deltay )
{
xform->xshift+=deltax;
xform->yshift+=deltay;
}

/*****************************************************************************
******************************************************************************
* Aberration is the pedestrian cousin to relativistic beaming.
* A moving observer will see a star in a position shifted away from the
* direction of motion.
* For the full relativistic equation see Rybicki and Lightman eq 4.8.
* The dominant component of the observer's velocity is the motion of the 
* Earth around the Sun which has v/c~1e-4, so in the following
* calculation we discard all terms of order higher than (v/c)^1, and
* are accurate to one part in a few x 1e-4. 
* With this approximation a star's position is shifted by an angle 
* (v/c)sin(theta),
* where theta is the angle between the position of the star and
* the direction of motion.
* This means that projected onto the tangent plane, the aberration shift
* in radians is just the velocity vector in units of c (also projected
* onto the plane).
*
* Aberration is at most ~20 arcsec.
* 
* This routine adds the affects of aberration to an existing
* detector to sky transformation.
*
* rot is the rotation matrix specifying the orientation of the tangent
* plane coordinates with respect to the celestial coordinates.
* Note that by convention the tangent plan X axis is inverted with respect to
* to the three dimensional X axis.
* v is the magnitude of the observer's (i.e. the Earth's) velocity
* and vhat is the three dimensional unit vector giving the
* direction of that velocity. in cartesian celestial coordinates.
* pix_per_radian gives the plate scale.
*****************************************************************************/
void addAberrationToXform2d(XFORM2D* xform, ROTMATRIX* rot,
                            double v, double vhat[3],
                            double pix_per_radian ) 
{
double deltax;
double deltay;

#ifdef DEBUG
printf("addAberrationToXform2d: vhat=(%g %g %g)\n",vhat[0],vhat[1],vhat[2]);
printf("addAberrationToXform2d: m00=%g m10=%g m20=%g\n",
       rot->m[0][0], rot->m[1][0], rot->m[2][0]);
#endif /* DEBUG */

deltax=-v*pix_per_radian*(vhat[0]*rot->m[0][0] + 
                          vhat[1]*rot->m[0][1] +
                          vhat[2]*rot->m[0][2]  );

deltay= v*pix_per_radian*(vhat[0]*rot->m[1][0] + 
                          vhat[1]*rot->m[1][1] +
                          vhat[2]*rot->m[1][2]  );


applyTranslationToXform2d(xform,deltax,deltay );

} /* end of addAberrationToXform2d function */



/*****************************************************************************
******************************************************************************
* Aberration is the pedestrian cousin to relativistic beaming.
* A moving observer will see a star in a position shifted away from the
* direction of motion.
* For the full relativistic equation see Rybicki and Lightman eq 4.8.
* The dominant component of the observer's velocity is the motion of the 
* Earth around the Sun which has v/c~1e-4, so in the following
* calculation we discard all terms of order higher than (v/c)^1, and
* are accurate to one part in a few x 1e-4. 
* With this approximation a star's position is shifted by an angle 
* (v/c)sin(theta),
* where theta is the angle between the position of the star and
* the direction of motion.
* This means that projected onto the tangent plane, the aberration shift
* in radians is just the velocity vector in units of c (also projected
* onto the plane).
*
* Aberration is at most ~20 arcsec.
* 
* This routine adds the affects of aberration to an existing
* DET -> SKY transformation.
*
* xhat and yhat are three dimensional unit vectors which define the
* SKYX and SKYY directions in the tangent plane projection.
* v is the magnitude of the observer's (i.e. the Earth's) velocity
* and vhat is the three dimensional unit vector giving the
* direction of that velocity.
* pix_per_radian gives the plate scale.
*****************************************************************************/
void old_addAberrationToXform2d(XFORM2D* xform, double xhat[3], double yhat[3],
                            double v, double vhat[3],
                            double pix_per_radian ) 
{
double deltax;
double deltay;

deltax=v*pix_per_radian*(xhat[0]*vhat[0] + xhat[1]*vhat[1] + xhat[2]*vhat[2]);
deltay=v*pix_per_radian*(yhat[0]*vhat[0] + yhat[1]*vhat[1] + yhat[2]*vhat[2]);


applyTranslationToXform2d(xform,deltax,deltay );

}



                            
/*****************************************************************************
*****************************************************************************
* This routine sets xform2 to the inverse of xform1
*****************************************************************************/
void invertXform2d(XFORM2D* xform2, XFORM2D* xform1) {

double det;


det=1./(xform1->rot[0][0]*xform1->rot[1][1] - 
        xform1->rot[1][0]*xform1->rot[0][1]  );

xform2->rot[0][0] =   xform1->rot[1][1] *det;
xform2->rot[0][1] = - xform1->rot[0][1] *det;
xform2->rot[1][0] = - xform1->rot[1][0] *det;
xform2->rot[1][1] =   xform1->rot[0][0] *det;

xform2->xshift = det*( xform1->rot[0][1] * xform1->yshift -
                       xform1->rot[1][1] * xform1->xshift   );

xform2->yshift = det*( xform1->rot[1][0] * xform1->xshift - 
                       xform1->rot[0][0] * xform1->yshift   );

}

/***************************************************************************
****************************************************************************
* Determine the best transformation from the four points
* (0,0) (1,0) (1,1) (0,1) to the four points specified by (x,y)
*
* Note three points define the transform, but we
* have four points. We are in effect finding the parallegram
* whose corners match the measured corners the best.
* "The best" is defined as having the smallest sum of
* the squares of distances between measured points and parallegram 
* corners.
* Chosing the four corners above simplifies the math considerably,
* and most of the least squares calculation is done analytically.
* to obtain the transformation from a different set of points to 
* the four given points, combine the output of this function
* with another transform using combineXform2ds.
*
* This function returns the R.M.S. error between the given points and the
* ones specified by the transformation.
****************************************************************************/
double setXform2dFromCornerPixels(XFORM2D* xform, double* x, double* y) {
double error;
double xx,yy;
double dx,dy;
double rawx[4]={0.,1.,1.,0.};
double rawy[4]={0.,0.,1.,1.};
int i;


/**********************
* calculate transform *
**********************/
xform->rot[0][0]=(  -x[0]+x[1]+x[2]-x[3])*.5 ;
xform->rot[0][1]=(  -x[0]-x[1]+x[2]+x[3])*.5 ;
xform->xshift   =(3.*x[0]+x[1]-x[2]+x[3])*.25;

xform->rot[1][0]=(  -y[0]+y[1]+y[2]-y[3])*.5 ;
xform->rot[1][1]=(  -y[0]-y[1]+y[2]+y[3])*.5 ;
xform->yshift   =(3.*y[0]+y[1]-y[2]+y[3])*.25;

/******************
* calculate error *
******************/
for(i=0,error=0.;i<4;++i) {

    applyXform2dToContinuousCoords(xform,&xx,&yy,rawx[i],rawy[i]);
    dx=xx-x[i];
    dy=yy-y[i];
    error+=dx*dx+dy*dy;
}

error=sqrt(error);

return(error);

} /* end of setXform2dFromCornerPixels function */



/*****************************************************************************
*****************************************************************************
* print the components of an XFORM2D structure to a particular stream.
* This is mostly useful for debugging.
*****************************************************************************/
void printXform2d(XFORM2D* xform, FILE* stream) {

fprintf(stream,"(%g %g) + %g\n",
        xform->rot[0][0],xform->rot[0][1], xform->xshift);

fprintf(stream,"(%g %g) + %g\n",
        xform->rot[1][0],xform->rot[1][1], xform->yshift);

}

/*****************************************************************************
******************************************************************************
* Determine the rigid transformation which takes the set of points
* (x0,y0) as close as possible to (x1,y1). A rigid transform consists
* of only rotations and translations. 
* "Closest" is defined as the weighted sum of the squares of the distances 
* from the transformed (x0,y0) to the (x1,y1). the "wgt" array gives
* this weighting. Usually wgt would be set to 1./(sigma*sigma), and the
* closeness criterion would be "chi-squared". If a null pointer
* is given for wgt, then all the weights are assumed to equal one.
******************************************************************************/
void findBestRigidXform2d(XFORM2D* xform, double x0[], double y0[], 
                                     double x1[], double y1[], 
                          double wgt[], int npoints) {
double sum=0.;
double sum_recip;

double sumx0=0.;
double sumy0=0.;
double sumx1=0.;
double sumy1=0.;

double sumx0x1=0.;
double sumy0y1=0.;
double sumx0y1=0.;
double sumy0x1=0.;

double norm;

int i;

double sintheta,costheta;
double deltax, deltay;

/********************************************
* calculate a bunch of sums over the points *
********************************************/
if(wgt!=NULL) {
    /********************
    * include weighting *
    ********************/
    for(i=0;i<npoints;++i) {

        sum += wgt[i];

        sumx0 += x0[i]*wgt[i];
        sumy0 += y0[i]*wgt[i];
        sumx1 += x1[i]*wgt[i];
        sumy1 += y1[i]*wgt[i];

        sumx0x1 += x0[i]*x1[i]*wgt[i];
        sumy0y1 += y0[i]*y1[i]*wgt[i];
        sumx0y1 += x0[i]*y1[i]*wgt[i];
        sumy0x1 += y0[i]*x1[i]*wgt[i];

    } /* end of loop over all points */
} else {
    /*************************
    * set all weights to one *
    *************************/
    sum=(double)npoints;
    for(i=0;i<npoints;++i) {

        sumx0 += x0[i];
        sumy0 += y0[i];
        sumx1 += x1[i];
        sumy1 += y1[i];

        sumx0x1 += x0[i]*x1[i];
        sumy0y1 += y0[i]*y1[i];
        sumx0y1 += x0[i]*y1[i];
        sumy0x1 += y0[i]*x1[i];

    } /* end of loop over all points */
} /* end if we are not including weighting */

/*******************************
* calculate the rotation angle *
*******************************/
sum_recip=1./sum;

costheta= (sumx0x1 - sumx0*sumx1*sum_recip) + (sumy0y1 - sumy0*sumy1*sum_recip);
sintheta= (sumy0x1 - sumy0*sumx1*sum_recip) - (sumx0y1 - sumx0*sumy1*sum_recip);

norm=sqrt(sintheta*sintheta + costheta*costheta);
if(norm==0.) {
    /*************************************************
    * this can happen if all the points are the same *
    *************************************************/
    setXform2dToTranslation(xform,0.,0.);
    return;
}

norm=1./norm;
sintheta*=norm;
costheta*=norm;

setXform2dToRotation(xform,sintheta,costheta,0.,0.);

/*******************************
* calculate the x and y shifts *
*******************************/
deltax=(-costheta*sumx0 - sintheta*sumy0 + sumx1)*sum_recip;
deltay=( sintheta*sumx0 - costheta*sumy0 + sumy1)*sum_recip;

applyTranslationToXform2d(xform,deltax, deltay);

}/* end of findBestRigidXform2d function*/

/********************************************************************************
* Similar to findBestRigidXform2d, but it sets trans to the least squares
* best linear transform without restricting to rotations and translations.
********************************************************************************/
void findBestXform2d(XFORM2D* trans, double* x0, double* y0, double* x1, double* y1,
                double* wgt, int npoints) {


double sumx2=0.0;
double sumx=0.0;
double sumxy=0.0;
double sumy2=0.0;
double sumy=0.0;

double sumxx1=0.0;
double sumyx1=0.0;
double sumxy1=0.0;
double sumyy1=0.0;

double sumx1=0.0;
double sumy1=0.0;

double sum=0.0;

double w;

int i;

MATRIX* matrix;
double rhs[3];

/*****************
* calculate sums *
*****************/
for(i=0; i<npoints; ++i) {

    /********************************
    * get the weight for this point *
    ********************************/
    if(wgt==NULL) w=1.;
    else          w=wgt[i];

    sumx2 += w*x0[i]*x0[i];
    sumx  += w*x0[i];

    sumxy += w*x0[i]*y0[i];

    sumy2 += w*y0[i]*y0[i];
    sumy  += w*y0[i];

    sumxx1 += w*x0[i]*x1[i];
    sumyx1 += w*y0[i]*x1[i];

    sumxy1 += w*x0[i]*y1[i];
    sumyy1 += w*y0[i]*y1[i];
    
    sumx1 += w*x1[i];
    sumy1 += w*y1[i];

    sum += w;

}


matrix = allocateMatrix(3);

/*********************
* solve the "X" part *
*********************/
matrix->a[0][0]=sumx2;
matrix->a[0][1]=sumxy;
matrix->a[0][2]=sumx;
rhs[0]=sumxx1;

matrix->a[1][0]=sumxy;
matrix->a[1][1]=sumy2;
matrix->a[1][2]=sumy;
rhs[1]=sumyx1;

matrix->a[2][0]=sumx;
matrix->a[2][1]=sumy;
matrix->a[2][2]=sum;
rhs[2]=sumx1;

decomposeMatrix(matrix);
solveMatrix(matrix, rhs);

trans->rot[0][0]=rhs[0];
trans->rot[0][1]=rhs[1];
trans->xshift   =rhs[2];

/*
printf("%g %g %g\n", rhs[0], rhs[1], rhs[2]);
*/
/*********************
* solve the "Y" part *
*********************/

rhs[0]=sumxy1;
rhs[1]=sumyy1;
rhs[2]=sumy1;

solveMatrix(matrix, rhs);

trans->rot[1][0]=rhs[0];
trans->rot[1][1]=rhs[1];
trans->yshift   =rhs[2];

} /* end of findBestXform2d function */
