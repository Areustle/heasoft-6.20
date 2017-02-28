#include <stdlib.h>
#include <math.h>
#include <stdio.h>

#include "quat.h"



static int WARNING_COUNT = 0;

void set_quat_warnings (int count)
{
	WARNING_COUNT = count;
}

static void maintain_quat (QUAT * q)
{
	double * p = q->p;
	double norm = sqrt(p[0] * p[0] + p[1] * p[1] + p[2] * p[2] + p[3] * p[3]);
	if (p[3] < 0 || fabs(norm - 1) > QUAT_ROUNDOFF_ERR / 10) {
		double invnorm = 1. / norm;
		double err = fabs(norm - 1);
		if (WARNING_COUNT > 0) {
			--WARNING_COUNT;
			printf("renormalizing QUAT %p with |1-norm|=%g\n", (void*) q, err);
		}
		if (p[3] < 0)
			invnorm = -invnorm;
		p[0] *= invnorm;
		p[1] *= invnorm;
		p[2] *= invnorm;
		p[3] *= invnorm;
	}
}


void forceRenormalizeQuat (QUAT * q)
{
  double * p = q->p;
  double norm = sqrt(p[0] * p[0] + p[1] * p[1] + p[2] * p[2] + p[3] * p[3]);
  double invnorm = 1. / norm;
  if (p[3] < 0)  invnorm = -invnorm;
  
  p[0] *= invnorm;
  p[1] *= invnorm;
  p[2] *= invnorm;
  p[3] *= invnorm;
}



/***************************************************************
****************************************************************
* allocate space for a new quaternion structure
****************************************************************/
QUAT* allocateQuat(void)
{
QUAT* q;

q=(QUAT*)malloc(sizeof(QUAT));

return(q);

}

/***************************************************************
****************************************************************
* allocate space for an array of quaternion structures
* note allocateQuatArray(1) is the same as allocateQuat
****************************************************************/
QUAT* allocateQuatArray(int dimen) {
QUAT* q;

q=(QUAT*)malloc(sizeof(QUAT)*dimen);

return(q);

}

/*********************************************************************
*********************************************************************
* Change the size of a previously allocated 
********************************************************************/
QUAT* changeQuatArraySize(QUAT* q, int newdimen) {

q=(QUAT*)realloc((void*)q,sizeof(QUAT)*newdimen);

return(q);

}



/***************************************************************
****************************************************************
* free storage for a quaternion structure
****************************************************************/
void destroyQuat(QUAT* q)
{
free(q);
}

/**************************************************************************
***************************************************************************
* Set the four components of a quaternion and makes sure they are normalized
***************************************************************************/
void setQuat(QUAT* q, double q0, double q1, double q2, double q3) 
{
q->p[0]=q0;
q->p[1]=q1;
q->p[2]=q2;
q->p[3]=q3;

maintain_quat(q);
}

/* Copy a quaternion. */
void copyQuat(QUAT* dest, QUAT* source)
{
  setQuat(dest, source->p[0], source->p[1], source->p[2], source->p[3]);
}


void renormalizeQuat (QUAT * q)
{
maintain_quat(q);
}



/**************************************************************************
***************************************************************************
* Set a quaternity to the identity quaternion (0,0,0,1)
***************************************************************************/
void setQuatToIdentity(QUAT* q) {

setQuat(q,0.,0.,0.,1.);

} /* end of setQuatToIdentity functiuon */



/**************************************************************************
***************************************************************************
* Report that there is an inconsistency in a quaternion
* This would usually be due to bad normalization
* The program prints a message to stderr and exits with status "1"
***************************************************************************/
void badQuatError(QUAT* q) 
{
fprintf(stderr,"Ill formed quaternion:\n");
fprintf(stderr,"(%g %g %g %g)\n",q->p[0],q->p[1],q->p[2],q->p[3]);
fprintf(stderr,"norm=%.14g\n",normOfQuat(q) );

exit(1);

}

/***************************************************************
****************************************************************
* give the magnitude of a quaternion
* attitude quaternions should be normalized to 1.
****************************************************************/
double normOfQuat(QUAT* q)
{
	double * p = q->p;
return(sqrt(p[0] * p[0] + 
            p[1] * p[1] +
            p[2] * p[2] + 
            p[3] * p[3]   ) );

}

/***************************************************************
****************************************************************
* Computes the quaternion product q = q1 q2
* This gives the result of sucessive rotations by q1 and then q2
* Note that the quaternion product does not commute.
****************************************************************/
void productOfQuats(QUAT* q, QUAT* q1, QUAT* q2)
{
double* p1;
double* p2;

p1=q1->p;
p2=q2->p;

q->p[0] =  p2[3]*p1[0] + p2[2]*p1[1] - p2[1]*p1[2] + p2[0]*p1[3];
q->p[1] = -p2[2]*p1[0] + p2[3]*p1[1] + p2[0]*p1[2] + p2[1]*p1[3];
q->p[2] =  p2[1]*p1[0] - p2[0]*p1[1] + p2[3]*p1[2] + p2[2]*p1[3];
q->p[3] = -p2[0]*p1[0] - p2[1]*p1[1] - p2[2]*p1[2] + p2[3]*p1[3];

maintain_quat(q);
}

/***************************************************************
****************************************************************
* determine the quaternion q such that q1*q=q2
* In terms of rotations, this gives the quaternion of the rotation
* between the orientations specified by q1 and q2.
* Intuitively it's the "difference" - though mathematically it's
* more like division: q=inv(q1)*q2.
* Note this can also be done using invertQuat and productofQuats,
* but this function is more efficient since it is coded directly 
* with the quaternion components.
****************************************************************/
void getQuatOfChange(QUAT* q, QUAT* q1, QUAT* q2)
{
double* p1;
double* p2;

p1=q1->p;
p2=q2->p;

q->p[0] = -p2[3]*p1[0] - p2[2]*p1[1] + p2[1]*p1[2] + p2[0]*p1[3];
q->p[1] =  p2[2]*p1[0] - p2[3]*p1[1] - p2[0]*p1[2] + p2[1]*p1[3];
q->p[2] = -p2[1]*p1[0] + p2[0]*p1[1] - p2[3]*p1[2] + p2[2]*p1[3];
q->p[3] =  p2[0]*p1[0] + p2[1]*p1[1] + p2[2]*p1[2] + p2[3]*p1[3];


maintain_quat(q);

#ifdef DEBUG
printf("getQuatOfChange: q1=(%g %g %g %g)\n",p1[0],p1[1],p1[2],p1[3]);
printf("getQuatOfChange: q2=(%g %g %g %g)\n",p2[0],p2[1],p2[2],p2[3]);
printf("getQuatOfChange: q =(%g %g %g %g)\n",q->p[0],q->p[1],q->p[2],q->p[3]);

printf("getQuatOfChange: norm(q1)-1 = %g norm(q2)-1 = %g\n",
       normOfQuat(q1)-1.0, normOfQuat(q2)-1.0 );
#endif

}


/***************************************************************
****************************************************************
* calculate the compliment of a quaternion. The compliment of a
* quaternion represents a rotation in the opposite direction.
****************************************************************/
void invertQuat(QUAT* q, QUAT* q1)
{
q->p[0]=-q1->p[0];
q->p[1]=-q1->p[1];
q->p[2]=-q1->p[2];
q->p[3]= q1->p[3];

}

/***************************************************************
****************************************************************
* calculate the compliment of a quaternion. The compliment of a 
* quaternion represents a rotation in the opposite direction.
****************************************************************/
void invertQuatInPlace(QUAT* q)
{
q->p[0]=-q->p[0];
q->p[1]=-q->p[1];
q->p[2]=-q->p[2];

}





/***************************************************************
****************************************************************
* Calculates the angle in radians of the rotation represented by the
* quaternion q divided by 2.
* This routine assumes the quaternion is properly normalized
* also note that by convention, the rotation angle is always
* positive.
****************************************************************/
double getQuatHalfRotation(QUAT* q)
{

double p3;

p3 = q->p[3];

if (fabs(p3) <= 1.) return( acos(p3) );

/************************************************************
* only get here due to round-off error or an unnormalized
* or ill-formed quaternion
*************************************************************/
if (fabs(p3) - 1 > QUAT_ROUNDOFF_ERR ) badQuatError(q);

/************************************************************
* if we get here, it's just roundoff error so we compensate *
************************************************************/
if (p3 > 1) return(0.);
else        return(M_PI);

}

/***************************************************************
****************************************************************
* q represents a rotation about the same axis as q1, but through 
* a rotation angle of a times the rotation angle of q1
* This is useful for interpolation and extrapolation.
* Note this routine assumes the quaternions are normalized 
* to within roundoff error. 
****************************************************************/
void multiplyQuatByScalar(QUAT* q, QUAT* q1,double a)
{
double * p = q->p;
double * p1 = q1->p;
double pi[4];
double err;

if (p1[3] < 0) {
	/* this keeps the function sane for angles near -pi */
	pi[0] = -p1[0];
	pi[1] = -p1[1];
	pi[2] = -p1[2];
	pi[3] = -p1[3];
	p1 = pi;
}

/****************************************
* dummy variable to avoid trouble with  
* the acos argument being out of bounds
****************************************/
err = p1[3] - 1;

/**************
* normal case *
**************/
if (err < 0) {

	double ratio;
    double halfangle = acos(p1[3]);

    ratio = sin(a * halfangle) / sin(halfangle);

    p[0] = p1[0] * ratio; 
    p[1] = p1[1] * ratio;
    p[2] = p1[2] * ratio; 
    p[3] = cos(a * halfangle);

} else if (err < QUAT_ROUNDOFF_ERR) {
    /*****************************************************************
    * assuming the quaternion is normalized, this must mean q1 has
    * halfangle=0 so just return the value of q1
    *****************************************************************/
    *q=*q1;

} else {
    /***************************************
    * something wrong with the quaternion *
    ***************************************/
    badQuatError(q);
}

maintain_quat(q);

} /* end of multiplyQuatByScalar function */

/***************************************************************************
****************************************************************************
* get the quaternion which will give the shortest "great circle" rotation
* which will transform unit vector hat1 into unit vector hat2
***************************************************************************/
void greatCircleQuat(QUAT* q, double hat1[3], double hat2[3]) {
double qq;

/*******************************************
* the vector part of the quaternion 
* (components 0, 1, 2) is just hat1 x hat2
*******************************************/
q->p[0]=  hat1[1]*hat2[2] - hat1[2]*hat2[1];
q->p[1]=-(hat1[0]*hat2[2] - hat1[2]*hat2[0]);
q->p[2]=  hat1[0]*hat2[1] - hat1[1]*hat2[0];

/********************************************
* then we set the scaler part of the quat to 
* make the whole thing normalized
*********************************************/
qq=1.- q->p[0]*q->p[0] - q->p[1]*q->p[1] - q->p[2]*q->p[2];
/*
printf("q0=%g q1=%g q2=%g qq=%g\n",q->p[0],q->p[1],q->p[2],qq);
*/

if     (qq>=0.               ) q->p[3]=sqrt(qq);
else if(qq>-QUAT_ROUNDOFF_ERR) q->p[3]=0.;
else {
    fprintf(stderr,"unit vectors not normalized in greatCircleQuat\n");

    fprintf(stderr,"|hat1| = |%g %g %g| = %g\n",
            hat1[0],hat1[1],hat1[2],
            sqrt(hat1[0]*hat1[0] + hat1[1]*hat1[1] + hat1[2]*hat1[2]) );

    fprintf(stderr,"|hat2| = |%g %g %g| = %g\n",
            hat2[0],hat2[1],hat2[2],
            sqrt(hat2[0]*hat2[0] + hat2[1]*hat2[1] + hat2[2]*hat2[2]) );

    exit(1);
}

maintain_quat(q);

} /* end of greatCircleQuat function */

/***************************************************************************
****************************************************************************
* convert a quaternion to a direction cosine rotation matrix
***************************************************************************/
void convertQuatToRotMatrix(ROTMATRIX* rot,QUAT* q)
{

double* p;
p=q->p;

/********************
* diagonal elements *
********************/
rot->m[0][0] =  p[0]*p[0] - p[1]*p[1] - p[2]*p[2] + p[3]*p[3];
rot->m[1][1] = -p[0]*p[0] + p[1]*p[1] - p[2]*p[2] + p[3]*p[3];
rot->m[2][2] = -p[0]*p[0] - p[1]*p[1] + p[2]*p[2] + p[3]*p[3];

/***************
* off diagonal *
***************/
rot->m[0][1] = 2.*(p[0]*p[1] + p[2]*p[3]);
rot->m[1][0] = 2.*(p[0]*p[1] - p[2]*p[3]);

rot->m[0][2] = 2.*(p[0]*p[2] - p[1]*p[3]);
rot->m[2][0] = 2.*(p[0]*p[2] + p[1]*p[3]);

rot->m[1][2] = 2.*(p[1]*p[2] + p[0]*p[3]);
rot->m[2][1] = 2.*(p[1]*p[2] - p[0]*p[3]);

}

/***************************************************************************
****************************************************************************
* convert  a direction cosine rotation matrix to a quaternion
***************************************************************************/
void convertRotMatrixToQuat(QUAT* q, ROTMATRIX* rot)
{

double diag_sum[4];
double recip;
int i,maxi;

/****************************************************************
* there are four equivalent ways of calculating this.
* we pick the one with the greatest numerical accuracy
* for this matrix
*****************************************************************/
diag_sum[0]=1+rot->m[0][0]-rot->m[1][1]-rot->m[2][2];
diag_sum[1]=1-rot->m[0][0]+rot->m[1][1]-rot->m[2][2];
diag_sum[2]=1-rot->m[0][0]-rot->m[1][1]+rot->m[2][2];
diag_sum[3]=1+rot->m[0][0]+rot->m[1][1]+rot->m[2][2];

#ifdef DEBUG
printf("convertRotMatrixToQuat: %d diag_sum=%g\n",0,diag_sum[0]);
#endif /* DEBUG */

maxi=0;
for(i=1;i<4;++i) {
    #ifdef DEBUG
    printf("convertRotMatrixToQuat: %d diag_sum=%g\n",i,diag_sum[i]);
    #endif /* DEBUG */
    if(diag_sum[i]>diag_sum[maxi]) maxi=i;
}

#ifdef DEBUG
printf("convertRotMatrixToQuat: maxi=%d\n",maxi);
#endif /* DEBUG */

q->p[maxi]=0.5*sqrt(diag_sum[maxi]);
recip=1./(4.*q->p[maxi]);

if(maxi==0) {
    q->p[1]=recip*(rot->m[0][1]+rot->m[1][0]);
    q->p[2]=recip*(rot->m[2][0]+rot->m[0][2]);
    q->p[3]=recip*(rot->m[1][2]-rot->m[2][1]);

} else if(maxi==1) {
    q->p[0]=recip*(rot->m[0][1]+rot->m[1][0]);
    q->p[2]=recip*(rot->m[1][2]+rot->m[2][1]);
    q->p[3]=recip*(rot->m[2][0]-rot->m[0][2]);

} else if(maxi==2) {
    q->p[0]=recip*(rot->m[2][0]+rot->m[0][2]);
    q->p[1]=recip*(rot->m[1][2]+rot->m[2][1]);
    q->p[3]=recip*(rot->m[0][1]-rot->m[1][0]);

} else if(maxi==3) {
    q->p[0]=recip*(rot->m[1][2]-rot->m[2][1]);
    q->p[1]=recip*(rot->m[2][0]-rot->m[0][2]);
    q->p[2]=recip*(rot->m[0][1]-rot->m[1][0]);
}

maintain_quat(q);

} /* end of convertQuatToRotMatrix function */


/***************************************************************************
****************************************************************************
* convert a quaternion to a set of euler angles
* This routine achieves the same result as first converting to a rotation
* matrix and then converting to euler angles, but is more efficient
* since not all the rotation matrix elements are needed.
* On the other hand if you already have the rotation matrix available
* it is more efficient to convert that into the euler angles.
***************************************************************************/
void convertQuatToEuler(EULER* e, QUAT* q)
{

double m22;

double* p;
p=q->p;

/*****************
* matrix element *
*****************/
m22=-p[0]*p[0] - p[1]*p[1] + p[2]*p[2] + p[3]*p[3];

if(fabs(m22)<1.-QUAT_ROUNDOFF_ERR ) {
    /**************
    * normal case *
    **************/
    double m20,m21;
    double m02,m12;

    e->theta=acos(m22);

    /************************************************************
    * more matrix elements - though we've dropped a factor of 2 *
    ************************************************************/
    m20=p[0]*p[2] + p[1]*p[3];
    m02=p[0]*p[2] - p[1]*p[3];

    m21=p[1]*p[2] - p[0]*p[3];
    m12=p[1]*p[2] + p[0]*p[3];

    if((m20==0. && m21==0.) || 
       (m02==0. && m12==0.)   ) badQuatError(q);

    e->phi=atan2(m21, m20);
    e->psi=atan2(m12,-m02);

} else if(fabs(m22) > 1.+EULER_ROUNDOFF_ERR) {
    /*************************
    * rotation matrix is bad *
    *************************/ 
    badQuatError(q);

} else {
    /**********************************
    * just a roundoff problem at pole *
    **********************************/
    double m00,m01;

    if(m22>0.) e->theta=0.;
    else       e->theta=M_PI;

    m00=p[0]*p[0] - p[1]*p[1] - p[2]*p[2] + p[3]*p[3];
    m01=2.*(p[0]*p[1] + p[2]*p[3]);
 
    if(m01==0. && m00==0.) badQuatError(q);

    if(m22>0.) e->phi=atan2( m01, m00);
    else       e->phi=atan2(-m01,-m00);

    e->psi = 0.;
   
}


} /* end of convertQuatToEuler function */

/***************************************************************************
****************************************************************************
* convert  a set of Euler angles to a quaternion
* This routine does exactly the same calculation as first converting
* to a rotation matrix and then to a quaternion, but it saves having
* to handle the intermediate storage.
* If you already have the rotation matrix, then it is more
* efficient to convert that to a quaternion.
*****************************************************************************/
void convertEulerToQuat(QUAT* q, EULER* e)
{

double sinphi, sintheta, sinpsi;
double cosphi, costheta, cospsi;

double m00,m01,m02;
double m10,m11,m12;
double m20,m21,m22;

double diag_sum[4];

int i,maxi;
double recip;

/***************************
* calculate trig functions *
***************************/
sinphi=sin(e->phi);
cosphi=cos(e->phi);

sintheta=sin(e->theta);
costheta=cos(e->theta);

sinpsi=sin(e->psi);
cospsi=cos(e->psi);

/****************************
* calculate matrix elements *
****************************/
m00= cospsi*costheta*cosphi - sinpsi*sinphi;
m01= cospsi*costheta*sinphi + sinpsi*cosphi;

m10=-sinpsi*costheta*cosphi - cospsi*sinphi;
m11=-sinpsi*costheta*sinphi + cospsi*cosphi;

m02=-cospsi*sintheta;      
m12= sinpsi*sintheta;

m20=sintheta*cosphi;
m21=sintheta*sinphi;

m22=costheta;

/***********************************************************
* now do the conversion from rotation matrix to quaternion *
***********************************************************/
diag_sum[0]=1+m00-m11-m22;
diag_sum[1]=1-m00+m11-m22;
diag_sum[2]=1-m00-m11+m22;
diag_sum[3]=1+m00+m11+m22;

maxi=0;
for(i=1;i<4;++i) {
    if(diag_sum[i]>diag_sum[maxi]) maxi=i;
}

q->p[maxi]=0.5*sqrt(diag_sum[maxi]);
recip=1./(4.*q->p[maxi]);

if(maxi==0) {
    q->p[1]=recip*(m01+m10);
    q->p[2]=recip*(m20+m02);
    q->p[3]=recip*(m12-m21);

} else if(maxi==1) {
    q->p[0]=recip*(m01+m10);
    q->p[2]=recip*(m12+m21);
    q->p[3]=recip*(m20-m02);

} else if(maxi==2) {
    q->p[0]=recip*(m20+m02);
    q->p[1]=recip*(m12+m21);
    q->p[3]=recip*(m01-m10);

} else if(maxi==3) {
    q->p[0]=recip*(m12-m21);
    q->p[1]=recip*(m20-m02);
    q->p[2]=recip*(m01-m10);
}


maintain_quat(q);

} /* end of convertEulerToQuat function */

/***************************************************************************
****************************************************************************
* Determine the two dimensional transformation from one tangent plane
* projection coordinate system to another, which has been rotated by
* a given quaternion from the first. This transformation can be used
* for detector to sky transformations. 
* Note that the exact transformation is non-linear but the linear 
* approximation is very good for small angles.
* (oldx0, oldy0) and (newx0, newy0) are the tangent points in the
* old and new projected coordinate systems. 
* pixels_per_radian is the scale of both new and old coordinate systems
* at the tangent point.
* deltaq is the rotation from the new coordinate system to the old one.
* note this may seem counter-intuitive at first, but the usual use of this
* is from a tilting coordinate system (detector) to a fixed one (sky).
* In this case deltaq is the relative rotation with respect to the
* fixed reference attitude
*
* The equations used in this function can be derived in the following way.
* First, calculate the three dimensional unit vector direction 
* corresponding to a set of coordinates in the tangent plane. 
* The trick here is that by convention the X axis in the tangent plane points 
* in the opposite direction of the X axis in 3-D space. 
* Next, rotate this vector by the inverse of
* deltaq and convert the resulting unit vector back to tangent plane
* coordinates. The resulting transformation becomes linear in the limit
* where -m20*(oldx-oldx0)/pixels_per_radian +
*        m21*(oldy-oldy0)/pixels_per_radian   << m22
* which is true if the sin of the "theta" Euler angle corresponding to deltaq
* is small or if the event is near the tangent point in radians.
* For typical applications both conditions are met. 
* 
* This function returns the cosine of the angle between the z axes of the 
* old and new coordinate systems. That value can be used to detect absurdly
* large tilt angles (e.g. >= 90 degrees ).
* 
****************************************************************************/
double convertQuatToXform2d(XFORM2D* xform, QUAT* deltaq,
                            double oldx0, double oldy0, 
                            double newx0, double newy0,
                            double pixels_per_radian) {

double* p;
double m00, m11, m22, m01, m10, m02, m12;


/*******************************************
* calculate some rotation matrix elements 
* of the inverse of deltaq
*******************************************/
p=deltaq->p;

m00 =  p[0]*p[0] - p[1]*p[1] - p[2]*p[2] + p[3]*p[3];
m11 = -p[0]*p[0] + p[1]*p[1] - p[2]*p[2] + p[3]*p[3];
m22 = -p[0]*p[0] - p[1]*p[1] + p[2]*p[2] + p[3]*p[3];

m01 = 2.*(p[0]*p[1] - p[2]*p[3]);
m10 = 2.*(p[0]*p[1] + p[2]*p[3]);

m02 = 2.*(p[0]*p[2] + p[1]*p[3]);
m12 = 2.*(p[1]*p[2] - p[0]*p[3]);

/********************************************************
* set the XFORM2D from the 3-D rotation matrix elements *
********************************************************/
xform->rot[0][0]= m00/m22;
xform->rot[0][1]=-m01/m22;
xform->rot[1][0]=-m10/m22;
xform->rot[1][1]= m11/m22;

xform->xshift=-m02/m22*pixels_per_radian;
xform->yshift= m12/m22*pixels_per_radian;

/*********************************************************
* apply the translations to line up the reference points *
*********************************************************/
applyTranslationToXform2d(xform, newx0 - xform->rot[0][0] * oldx0
                                       - xform->rot[0][1] * oldy0,
                                 newy0 - xform->rot[1][0] * oldx0
                                       - xform->rot[1][1] * oldy0 );


return m22;

} /* end of convertQuatToXform2d function */


/*****************************************************************************
******************************************************************************
* This is the inverse of convertQuatToXform2d. It takes an arbitrary
* two dimensional transform from one tangent plane coordinate
* system to another, and gives the quaternion representing the
* relative orientation of these two coordinate systems.
* Note that an arbitrary 2 dimensional transform may not correspond
* exactly to any quaternion. For instance xform may contain extra
* scaling. In this case the function does the best it can to
* return a quaternion which corresponds to a 2-d transform which is
* similar to xform. The resulting quaternion is guaranteed to be correctly
* normalized for any input xform.
******************************************************************************/
void convertXform2dToQuat(QUAT* q, XFORM2D* xform, double oldx0, double oldy0, 
                                                   double newx0, double newy0,
                          double pixels_per_radian) {

double tantheta, sintheta;
double m00,m01,m02;
double m10,m11,m12;
double m20,m21,m22;

double sinsum,cossum;

double determinant, norm;
double dx,dy;

double diag_sum[4];
int i,maxi;
double recip;


/*******************************************************
* get the theta euler angle and 3x3 matrix element m22 *
*******************************************************/
dx = ( xform->xshift - newx0 + xform->rot[0][0] * oldx0
                             + xform->rot[0][1] * oldy0 )/pixels_per_radian;
dy = ( xform->yshift - newy0 + xform->rot[1][0] * oldx0
                             + xform->rot[1][1] * oldy0 )/pixels_per_radian;

tantheta=sqrt(dx*dx+dy*dy);
if(tantheta>0.0) {
    sintheta=tantheta/sqrt(1+tantheta*tantheta);
    m22=sintheta/tantheta;
} else {
    sintheta=0.0;
    m22=1.0;
}

/**********************************************************
* check if the transform includes an inversion - if so
* theta>90 and we have to flip the sign of m22=cos(theta)
* note we first set the upper left hand corner of the 
* 3x3 matrix to be the 2x2 matrix but with some signs flipped 
* because the phi decreases along the tangent plane 
* X axis by convention. This is correct if the xform2d
* is an exact representation of the three dimensional 
* rotation, but only approximately true otherwise.
* we will recalculate these four elements below to ensure
* that the completed 3x3 matrix is a valid rotation matrix
*********************************************************/
m00= xform->rot[0][0];
m01=-xform->rot[0][1];
m10=-xform->rot[1][0];
m11= xform->rot[1][1];

determinant = m00*m11 - m01*m10;
if(determinant<0) m22= -m22;
if(determinant==0) {
    fprintf(stderr,"Can't convert the following transform to a quaternion:\n");
    printXform2d(xform,stderr);
    exit(1);
}

#ifdef DEBUG
printf("convertXform2dToQuat: m22=%g determinant=%g\n",m22,determinant);
#endif /* DEBUG */


/***************************************
* now determine the psi euler angle 
* and 3x3 matrix elements m02 and m12 
***************************************/
m02=-dx*m22; /* note phi decreases along x axis by convention */
m12= dy*m22;

/*********************************************
* now get the sin and cos of phi+psi
* and use this to derive phi and m20 and m21
*********************************************/
cossum=m00+m11;
sinsum=m01-m10;

norm=1./sqrt(sinsum*sinsum + cossum*cossum);
sinsum *= norm;
cossum *= norm;

m21=-(m12*cossum + m02*sinsum);
m20= (m12*sinsum - m02*cossum);



/*******************************************************************
* recalculate the upper left hand corner of the matrix to be sure
* we get a valid rotation matrix. 
*******************************************************************/
if(sintheta!=0.) {
    norm=1./(sintheta*sintheta);
    m00=(-m02*m22*m20-m12*m21)*norm;
    m01=(-m02*m22*m21+m12*m20)*norm;
    m10=(-m12*m22*m20+m02*m21)*norm;
    m11=(-m12*m22*m21-m02*m20)*norm;
} else {
    m00= cossum;
    m01= sinsum;
    m10=-sinsum;
    m11= cossum;
}

#ifdef DEBUG
printf("convertXform2dToQuat: %g %g %g\n",m00,m01,m02);
printf("convertXform2dToQuat: %g %g %g\n",m10,m11,m12);
printf("convertXform2dToQuat: %g %g %g\n",m20,m21,m22);
printf("convertXform2dToQuat: m21*m21+m20*m20=%g sintheta*sintheta=%g\n",
       m21*m21+m20*m20, sintheta*sintheta);
#endif /* DEBUG */


/**************************************************
* now convert the rotation matrix to a quaternion *
**************************************************/
diag_sum[0]=1+m00-m11-m22;
diag_sum[1]=1-m00+m11-m22;
diag_sum[2]=1-m00-m11+m22;
diag_sum[3]=1+m00+m11+m22;

maxi=0;
for(i=1;i<4;++i) {
    if(diag_sum[i]>diag_sum[maxi]) maxi=i;
}

q->p[maxi]=0.5*sqrt(diag_sum[maxi]);
recip=1./(4.*q->p[maxi]);

if(maxi==0) {
    q->p[1]=recip*(m01+m10);
    q->p[2]=recip*(m20+m02);
    q->p[3]=recip*(m12-m21);

} else if(maxi==1) {
    q->p[0]=recip*(m01+m10);
    q->p[2]=recip*(m12+m21);
    q->p[3]=recip*(m20-m02);

} else if(maxi==2) {
    q->p[0]=recip*(m20+m02);
    q->p[1]=recip*(m12+m21);
    q->p[3]=recip*(m01-m10);

} else if(maxi==3) {
    q->p[0]=recip*(m12-m21);
    q->p[1]=recip*(m20-m02);
    q->p[2]=recip*(m01-m10);
}


maintain_quat(q);

/*****************************************
* by convention we want the inverse of q *
*****************************************/
invertQuatInPlace(q);

#ifdef DEBUG
printf("convertXform2dToQuat: q=(%g %g %g %g) norm=%g\n",
       q->p[0], q->p[1], q->p[2], q->p[3],normOfQuat(q));
#endif /* DEBUG */

} /* end of convertXform2dtoQuat function */


/***************************************************************************
****************************************************************************
* for a pointing defined by the quaternion q, determine the three unit
* vectors, xhat, yhat, and zhat which define the plane tangent to the line 
* of sight.
* The vector xhat points in the negative "R.A." direction, yhat in the positive
* "Dec" direction, and zhat points along the line of sight.
* 
* At the pole xhat points along the meridian (R.A.=0).
***************************************************************************/
void getPlaneTangentToQuat(QUAT* q, 
                           double xhat[3], double yhat[3], double zhat[3] )
{

double m22;


double* p;
p=q->p;


/*****************
* matrix element *
*****************/
m22=-p[0]*p[0] - p[1]*p[1] + p[2]*p[2] + p[3]*p[3];

if(fabs(m22)<1.-QUAT_ROUNDOFF_ERR ) {
    /**************
    * normal case *
    **************/
    double m20,m21;
    double sinphi,sintheta;
    double cosphi,costheta;
    double norm;

    costheta=m22;

    /************************************************************
    * more matrix elements - though we've dropped a factor of 2 *
    ************************************************************/
    m20=p[0]*p[2] + p[1]*p[3];
    m21=p[1]*p[2] - p[0]*p[3];

    norm=sqrt(m20*m20+m21*m21);

    sinphi=m21/norm;
    cosphi=m20/norm;

    sintheta=2.*norm;

    /**************************
    * set the x and y vectors *
    **************************/
    xhat[0]=sinphi;
    xhat[1]=-cosphi;
    xhat[2]=0.;

    yhat[0]=-cosphi*costheta;
    yhat[1]=-sinphi*costheta;
    yhat[2]=sintheta;

    zhat[0]=cosphi*sintheta;
    zhat[1]=sinphi*sintheta;
    zhat[2]=costheta;

} else if(fabs(m22) > 1.+EULER_ROUNDOFF_ERR) {
    /*************************
    * rotation matrix is bad *
    *************************/ 
    badQuatError(q);

} else {
    /**********************************
    * just a roundoff problem at pole *
    **********************************/
    xhat[0]=1.;
    xhat[1]=0.;
    xhat[2]=0.;

    yhat[0]=0.;
    yhat[1]=1.;
    yhat[2]=0.;

    zhat[0]=0.;
    zhat[1]=0.;
    if(m22>0.) zhat[2]= 1.;
    else       zhat[2]=-1.;

}


} /* end of getPlaneTangentToQuat function */



/*****************************************************************************
******************************************************************************
* Given a set of points (x0,y0), find the linear transform which moves
* the points closest to (x1,y1). The linear transform is restricted to 
* be a transform corresponding to a three dimensional rotation between
* two tangent plane coordinate systems.
* "Closeness" is defined in the same way as for findBestRigidXform2d.
* And the linear transform is defined as in convertQuatToXform2d.
*
* The function returns the error in the transform, and sets q to the
* three dimensional rotation and xform to the corresponding 2 dimensional
* transformation. Using NULL for wgt is the same as setting all weights
* to unity.
*
* We use an iterative method where we find the closest rigid transform,
* then find the "quat" transform which approximates the rigid transform,
* and then repeat this proceedure on the residuals left from the
* previous approximation. The routine converges rapidly for small
* transforms. It begins to diverge if the Euler angles corresponding to
* q have theta > about 40 degrees, but even then it will give a reasonable
* approximation if you restrict max_iterations to only a few iterations.
* There is probably a way to solve for q directly without iterating, but
* this method works in practical cases and who needs all that algebra?
* 
******************************************************************************/
double findBestQuatXform2d(QUAT* q, XFORM2D* xform, double x0[], double y0[], 
                                                  double x1[], double y1[], 
                           double wgt[], int npoints,
                           double oldx0, double oldy0, 
                           double newx0, double newy0, 
                           double pixels_per_radian, 
                           double tolerance, int max_iterations) {

int iteration;
int i;

QUAT* deltaq;
QUAT* newq;

XFORM2D* rigid;
double *x;
double *y;

double dx, dy;
double error;
double norm;

/*****************************
* allocate temporary storage *
*****************************/
deltaq=allocateQuat();
newq=allocateQuat();

rigid=allocateXform2d();

x=(double*)malloc(sizeof(double)*npoints);
y=(double*)malloc(sizeof(double)*npoints);

/*******************************
* initialize temporary storage *
*******************************/
setQuatToIdentity(q);

for(i=0;i<npoints;++i) {
    x[i]=x0[i];
    y[i]=y0[i];
}

if(wgt == NULL) {
    norm=npoints;
} else {
    norm=0.;
    for(i=0; i<npoints; ++i) norm += wgt[i];
}


error=2.*tolerance+1;

/***********************************
* iterate until we have a solution *
***********************************/
for(iteration=0; error>tolerance && iteration<max_iterations; ++iteration) {

    /***********************************
    * approximate as a rigid transform *
    ***********************************/
    findBestRigidXform2d(rigid,x,y,x1,y1,wgt,npoints);

    /******************************************************
    * now approximate the rigid transform as a quaternion *
    ******************************************************/
    convertXform2dToQuat(deltaq,rigid, oldx0,oldy0, newx0,newy0, 
                         pixels_per_radian);

    /**********************************************************
    * update the quaternion by the piece we just approximated *
    **********************************************************/
    productOfQuats(newq,deltaq,q); 
    *q=*newq;

    /***********************************************************************
    * adjust the pre-transform points to take into
    * account for the parts of the quaternion we have already accumulated
    * and calculate the error
    ***********************************************************************/
    convertQuatToXform2d(xform, q, oldx0,oldy0, newx0,newy0, 
                         pixels_per_radian);

    error=0.;
    for(i=0;i<npoints;++i) {

        applyXform2dToContinuousCoords(xform, x+i,y+i, x0[i],y0[i]);

        dx=x1[i]-x[i];
        dy=y1[i]-y[i];
        if(wgt!=NULL) error += (dx*dx+dy*dy)*wgt[i];
        else          error +=  dx*dx+dy*dy;

    }

    error = sqrt(error/norm);

    #ifdef DEBUG
    {
    EULER* e;
    e=allocateEuler();

    convertQuatToEuler(e,q);
    printf("findBestQuatXform2d: it=%d (%g %g %g) error=%g\n",
           iteration, 
           e->phi*180./M_PI, e->theta*180./M_PI, e->psi*180./M_PI,
           error);
    }
    #endif /* DEBUG */

}

/*****************************
* free the temporary storage *
*****************************/
destroyQuat(deltaq);
destroyQuat(newq);
destroyXform2d(rigid);

free(x);
free(y);

return(error);

}/* end of findBestQuatXform2d function*/


/**************************
 * Print a Quat to a stream.
 **************************/
void printQuat(QUAT* q, FILE* stream)
{
  fprintf(stream, "[%.15g %.15g %.15g %.15g]", q->p[0], q->p[1], q->p[2], q->p[3]);
}


/*******************************************************
 * Set a null QUAT (all elements zero, not normalized) *
 *******************************************************/
void setNullQuat(QUAT* q)
{
  q->p[0] = 0.;
  q->p[1] = 0.;
  q->p[2] = 0.;
  q->p[3] = 0.;
}

/********************************************************************************
 * Determine if a QUAT is null (all elements essentially zero, not normalized). *
 ********************************************************************************/
int isQuatNull(QUAT* q)
{
  double tol = 1.0e-30;
  return (fabs(q->p[0]) < tol && fabs(q->p[1]) < tol && fabs(q->p[2]) < tol && fabs(q->p[3]) < tol ? 1 : 0);
}

/* Linearly interpolate the attitude quaternion given a pair of 
 * bracketing quaternions.  The independent variable (t in q(t))
 * is called time in this function, but it could be any 
 * floating-point quantity. The half-angle calculation is 
 * performed using its sine rather than its cosine because
 * the sine retains more numerical precision, and this is needed
 * for interpolating within small-angle rotations. */
void interpolateQuat
(
 QUAT* q,      /* Desired interpolated quaternion at a time t (output) */
 QUAT* q0,     /* Earlier bracketing quaternion at time t0 (input) */
 QUAT* q1,     /* Later bracketing quaternion at time t1 (input) */
 QUAT* dq,     /* Pre-calculated quaternion of change from q0 to q1 (input) */
 QUAT* dq_int, /* Quaternion of change from q0 to interpolated q (output) */
 double tfrac  /* Time fraction, tfrac = (t - t0)/(t1 - t0) */
 )
{
  double* dp_int = dq_int->p;
  double* dp = dq->p;
  double dpi[4];
  double err = 0.;
  double ratio = 1.;
  double halfangle = 0.;
  double sin_halfangle = 0.;

  if(dp[3] < 0)
    {
      /* This keeps the function sane for angles near -pi. */
	dpi[0] = -dp[0];
	dpi[1] = -dp[1];
	dpi[2] = -dp[2];
	dpi[3] = -dp[3];
	dp = dpi;
    }

  /* If the bracketing quats are identical, then interpolation is trivial. */

  if(fabs(q0->p[0] - q1->p[0]) < QUAT_ROUNDOFF_ERR &&
     fabs(q0->p[1] - q1->p[1]) < QUAT_ROUNDOFF_ERR &&
     fabs(q0->p[2] - q1->p[2]) < QUAT_ROUNDOFF_ERR &&
     fabs(q0->p[3] - q1->p[3]) < QUAT_ROUNDOFF_ERR)
    {
      setQuat(q, q0->p[0], q0->p[1], q0->p[2], q0->p[3]);
      return;
    }

  /* Calculate the difference from 1 in the real component of the quat of 
   * change to avoid an out-of-bounds cosine. */

  err = dp[3] - 1;

  /* If the real component is larger than 1 by more than some numerical 
   * imprecision, the quat cannot be used. */

  if(err > QUAT_ROUNDOFF_ERR)
    {
      badQuatError(q);
    }
  
  /* Calculate the rotation angle represented by dq from dq's imaginary
   * components. */

  sin_halfangle = sqrt(dp[0]*dp[0] + dp[1]*dp[1] + dp[2]*dp[2]);
  halfangle = asin(sin_halfangle);
  ratio = sin(tfrac * halfangle)/sin_halfangle;
  
  /* Calculate the interpolated quat of change. */

  dp_int[0] = dp[0] * ratio;
  dp_int[1] = dp[1] * ratio;
  dp_int[2] = dp[2] * ratio;
  dp_int[3] = cos(tfrac * halfangle);

  /* Apply the interpolated quat of change to the early quat to *
     calculate the quat q at the desired time. */

  productOfQuats(q, q0, dq_int);

}

