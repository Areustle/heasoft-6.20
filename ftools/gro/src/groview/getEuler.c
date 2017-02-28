/*
C SUBROUTINE TO TRANSFORM BETWEEN ASTRONOMICAL COORDINATE SYSTEMS
C     AI,BI (INPUT) AND AO,BO (OUTPUT).
C
C
C     ANGLES ARE INPUT AND OUTPUT IN RADIANS
C	     AI,AO = AZIMUTHAL ANGLE (LONGITUDE) = 0 TO 360
C	     BI,BO = COPOLAR ANGLE (LATITUDE) = -90 TO +90
C
C     THE TRANSFORMATION IS ABOUT THE THREE ORTHONGONAL EULER ANGLES
C     PHI,THETA,PSI, PERFORMED AS A SINGLE ROTATION ABOUT THE LINE
C     OF NODES.
C
C     ***	     ***     ***		  *** ***	     ***
C     * C(BO)C(AO-PSI) *     *	 1	0	0   * * C(BI)C(AI-PHI) *
C     * 	       *     *			    * * 	       *
C     * C(BO)S(AO-PSI) *  =  *	 0   CTHETA  STHETA * * C(BI)S(AI-PHI) *
C     * 	       *     *			    * * 	       *
C     *     S(BO)      *     *	 0  -STHETA  CTHETA * *     S(BI)      *
C     ***	     ***     ***		  *** ***	     ***
C
C * * * * *
C
C
*/

#include "groview.h"
#define    TWOPI   6.2831853072
#define    FOURPI  12.566370614
#define    PITWO   1.5707963268
#define    DEGREE  57.295779513
#define    RADIAN  1.7453292520E-2

void getEuler(float **zLong, float **zLat, float **xLong, float **xLat, 
        float *AI, float *BI, float **AO, float **BO, long *num, long *npt)
{
     float PSI[18]={0.57595, 4.92619, 1.35524, 1.91898,
                    -0.99141335536E-3, 4.89763, 0.000000,  0.000000, 0.111290,
			   4.70053,0.,0.,0.,0.,0.,0.,0.,0.};
     float STHETA[18]={0.887815, -0.887815, -0.25943153617E-1,  
                              0.25943153617E-1, 0.885181, -0.885181, 0.397881,
                              -0.397881, 0.867661, -0.867661,0.,0.,0.,0.,0.,0.,
			      0.397777, -0.397777 };
     float  CTHETA[18]={ 0.460199, 0.460199, 0.999663, 0.999663,
                     0.465245, 0.465245, 0.917436, 0.917436, 0.497154, 
                     0.497154, 0.0,0.,0.,0.,0.,0.,0.917482,0.917482};
     float  PHI[18]={4.92619, 0.575958, 1.91898, 1.35524, 4.89763, 
                            -0.99141335536E-3,0.000000, 0.000000, 4.70053, 
                            0.111290,0.,0.,0.,0.,0.,0.,0.,0.};
     double A, B, SB, CB, CBSA, tmp1, tmp2, THETA;
     int i,j;

      *AO = (float *) malloc((long)(*npt) * sizeof(float));
      *BO = (float *) malloc((long)(*npt) * sizeof(float)); 
     for (j=0;j<*npt; j++)
       {
	 i = 15;
	 A = sin((*xLat)[j]);
	 B = cos((*xLat)[j])*cos((*zLat)[j])*sin((*xLong)[j]-(*zLong)[j]);
      
	PSI[i] = -atan(A/B);
	PHI[i+1] = PSI[i];
	THETA = PITWO-(*zLat)[j];   
	STHETA[i] = sin(THETA);
	STHETA[i+1] = -STHETA[i];
	CTHETA[i] = cos(THETA);
	CTHETA[i+1] = CTHETA[i];
	PHI[i] = (*zLong)[j] -T3PI2;
	PSI[i+1] = PHI[i];
        
	/*  TRANSFORMATION */
	A = *AI - PHI[*num];
	B = *BI;
	SB = sin(B);
	CB = cos(B);
	CBSA = CB*sin(A);
	B = -STHETA[*num]*CBSA+CTHETA[*num]*SB;
    
	if((fabs(B) - 1.0) > 0.0) 
	  { if (B >=0) B =1.0;
	  else B = - 1.0;
	  }
    
	(*BO)[j] = asin(B);
	A = atan( (CTHETA[*num]*CBSA+STHETA[*num]*SB)/(CB*cos(A)));
   	tmp1 = A+PSI[*num]+FOURPI;
	tmp2 = TWOPI;
	(*AO)[j] = tmp1 - ((int)(tmp1/tmp2)) * tmp2;
       
       }
     
}
     
