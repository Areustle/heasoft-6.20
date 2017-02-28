#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#include "euler.h"

/* 2013-03-07: T. Reichard - Added printEuler and printEulerDeg functions. */

/***************************************************************************
****************************************************************************
* allocate storage for a set of euler angles
***************************************************************************/
EULER* allocateEuler()
{
EULER* e;

e=(EULER*)malloc(sizeof(EULER));

return(e);
}

/***************************************************************************
****************************************************************************
* free storage for a set of euler angles
***************************************************************************/
void destroyEuler(EULER* e)
{
free(e);
}

/***************************************************************************
****************************************************************************
* set the phi, theta and psi values of euler angles.
***************************************************************************/
void setEulerAngles(EULER* e, double phi, double theta, double psi) 
{
e->phi=phi;
e->theta=theta;
e->psi=psi;
}

/***************************************************************************
****************************************************************************
* Convert a set of Euler angles to R.A. Dec. and Roll angle in decimal degrees
* Note that this is just another way of specifying the same coordinates,
* except for the units, 
* and 0<= RA <= 360. , -90 <= Dec <= 0. and 0. <= roll <= 360.
***************************************************************************/
/*
void convertEulerToRADecRoll(EULER* e, double* ra, double* dec, double* roll)
{

*ra=180./M_PI*e->phi;
while(*ra<0.) *ra+=360.;

*dec=90.-180./M_PI*e->theta;

*roll=180./M_PI*e->psi - 90.;
while(*roll<0.) *roll+=360.;

}
*/

/***************************************************************************
****************************************************************************
* Convert R.A., Dec. and Roll angles in decimal degrees
* tp a set of Euler angles.
* Note that this is just another way of specifying the same coordinates,
* except for the units, 
* and 0<= RA <= 360. , -90 <= Dec <= 0. and 0. <= roll <= 360.
***************************************************************************/
/*
void convertRADecRollToEuler(EULER* e, double ra, double dec, double roll)
{

setEulerAngles(e, ra*M_PI/180., (90.-dec)*M_PI/180., (roll+90)*M_PI/180. );

}
*/


/***************************************************************************
****************************************************************************
* convert a direction cosine rotation matrix to a set of euler angles
***************************************************************************/
void convertRotMatrixToEuler(EULER* e, ROTMATRIX* rot)
{

if(fabs(rot->m[2][2])<1.-EULER_ROUNDOFF_ERR) {
    /**************
    * normal case *
    **************/
    e->theta=acos(rot->m[2][2]);

    if((rot->m[2][0]==0. && rot->m[2][1]==0.) ||
       (rot->m[0][2]==0. && rot->m[1][2]==0.)   ) badRotMatrixError(rot);

    e->phi=atan2(rot->m[2][1], rot->m[2][0]);
    e->psi=atan2(rot->m[1][2],-rot->m[0][2]);
    
} else if(fabs(rot->m[2][2]) > 1.+EULER_ROUNDOFF_ERR) {
    /*************************
    * rotation matrix is bad *
    *************************/ 
    badRotMatrixError(rot);

} else  {
    /**********************************
    * just a roundoff problem at pole *
    **********************************/
    if(rot->m[2][2]>1.) e->theta=0.  ;
    else                e->theta=M_PI;

    if(rot->m[0][1]==0. && rot->m[0][0]==0.) badRotMatrixError(rot);

    if(rot->m[2][2]>1.) e->phi=atan2( rot->m[0][1], rot->m[0][0]);
    else                e->phi=atan2(-rot->m[0][1],-rot->m[0][0]);

    e->psi = 0.;
   
          
}


} /* end of convertRotMatrixToEuler function */



/*****************************************************************************
******************************************************************************
* determine the direction cosine rotation matrix corresponding to
* a given set of Euler angles.
******************************************************************************/
void convertEulerToRotMatrix(ROTMATRIX* rot, EULER* e) 
{
double sinphi, sintheta, sinpsi;
double cosphi, costheta, cospsi;

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
rot->m[0][0]= cospsi*costheta*cosphi - sinpsi*sinphi;
rot->m[0][1]= cospsi*costheta*sinphi + sinpsi*cosphi;

rot->m[1][0]=-sinpsi*costheta*cosphi - cospsi*sinphi;
rot->m[1][1]=-sinpsi*costheta*sinphi + cospsi*cosphi;

rot->m[0][2]=-cospsi*sintheta;      
rot->m[1][2]= sinpsi*sintheta;

rot->m[2][0]=sintheta*cosphi;
rot->m[2][1]=sintheta*sinphi;

rot->m[2][2]=costheta;


} /* end of convertEulerToRotMatrix function */


/***************************************************************************
****************************************************************************
* for a pointing defined by Euler angles, e, determine the three unit
* vectors, xhat, yhat, and zhat which define the plane tangent to the line 
* of sight.
* The vector xhat points in the negative "R.A." direction, yhat in the positive
* "Dec" direction, and zhat points along the line of sight.
* 
* At the pole xhat points along the meridian (R.A.=0).
***************************************************************************/
void getPlaneTangentToEuler(EULER* e, 
                           double xhat[3], double yhat[3], double zhat[3] )
{
double sintheta,costheta;
double sinphi,cosphi;
double sinpsi,cospsi;

/***************************
* calculate trig functions *
***************************/
sinphi=sin(e->phi);
cosphi=cos(e->phi);

sintheta=sin(e->theta);
costheta=cos(e->theta);

sinpsi=sin(e->psi);
cospsi=cos(e->psi);

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

} /* end of getPlaneTangentToEuler function */


/* Print an Euler angle trio in radians to a stream. */
void printEuler(EULER* e, FILE* stream)
{
  fprintf(stream, "(%.15g %.15g %.15g)", e->phi, e->theta, e->psi);
}


/* Print an Euler angle trio in degrees to a stream. */
void printEulerDeg(EULER* e, FILE* stream)
{
  fprintf(stream, "(%.15g %.15g %.15g)", e->phi*180./M_PI, e->theta*180./M_PI, e->psi*180./M_PI);
}
