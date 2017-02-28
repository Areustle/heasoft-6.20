/* groview.h */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"
#include <math.h>

#define PI     3.1415926536
#define TWPI   6.2831853072
#define PITW   1.5707963268
#define T3PI2  4.7123889804
#define RAD    1.7453292520E-2
#define    TWOPI   6.2831853072
#define    FOURPI  12.566370614
#define    PITWO   1.5707963268
#define    DEGREE  57.295779513
   
   void getData( float **tjd, float **raZ, float **decZ,
	float **raX, float **decX, long *nrows, char *data_dir, long **vp,
        char *egretFlg[], char *primTarget[], char *secTarget[], char *all_VPs);  
   void getAdxyz(float **ra, float **dec, float **x,
		 float **y,float **z, long *npt);
   void getCross(float **scZx, float **scZy, float **scZz,
                 float **scXx, float **scXy, float **scXz,
                 float **scYx, float **scYy, float **scYz, long *nrows);
   void getXyzad(float **x, float **y, float **z,
		 float **ra, float **dec, long *npt);
   void getZlongLat(float **raY, float **decY, float **zLong, float **zLat, 
		    long *npt); 
   void getNorCord(float *ra, float *dec, float *x, float *y,float *z);
   void getEuler(float **zLong, float **zLat, float **xLong, float **xLat,
           float *AI, float *BI, float **AO, float **BO, long *num, long *npt);
   void getGroView(float **raZ, float **decZ,
    float *raSource, float *decSource, long *nrows, float **scLong,
    float **scLat, float **raX, float **decX, float **scAng, 
    float **sx, float **sy, float **sz);
   void  getBatseView(float **SpaceCor, float **B);
   float getDotProd( float **v1, float *v2, long *ndim );
   void  CmpView(float **SpaceCor,float **R);
   void EgretView(float **SpaceCor,float **R);
   void getSpaceCor(float **ra, float **dec, float **sx, float **sy,
        float **sz, long *npt);
   void rad2Deg(float **raY, float **decY, float **zLong, 
	       float **zLat, long *npt);
   void getAng(float *raSource, float *decSource, float **raZ, float **decZ, 
	       float **ang, long *npt);
   void getSource(float *raSource, float *decSource);
   void getAngThresh(float *incThresh);
   void getViewFlag(char *show_OSSE,char *show_EGRET,char *show_COMPTEL,
		    char *show_BATSE);
