/*
   gro_view:	input parameters are S/C Z-axis coordinates (ie COMPTEL,
c		& EGRET center of fov), offset from true sun of S/C XZ
c		plane,  instrument id ("EGRET", "COMPTEL", "OSSE"),
c		Julian Day #, Source coordinates; all are  epoch J2000
c		(code exists for 1950.0 as well);
c		returned arguments are the S/C coordiates
c		of the source in a long-lat system defined w/(0,0)
c		at the S/C +Z, eg. for  OSSE visibility, 270 < SC_long < 360 
c		or 0 < SC_long < 90 AND	|SC_lat| < ~~5; also, returns
c		the source poistion as a unit carteasian vector in SC
c		coordinates
c
c		internal variable definitions: 
c
c			alpha_z, delta_z	==  S/C Z-axis, clestial
c			alpha_s, delta_s	==  source, celestial
c			SC_long, SC_lat		==  S/ long lat system
c			alpha_So, delta_So	==  ra & dec of Sun @epoch
c			JD			==  Julian Day
c			LMDAS, DLMBDA		==  ecliptic long of sun, offset
c						    from optimum roll
c			SC_sun, SC_Z, etc.	==  S/C sun vector, S/C Z-axis ..
c			SX, SY, SZ = S(3)	==  cartesian source position
c						    in SC coordinates
c
C  DEFINE COORDINATE SYTEMS:
C
C                  SYSTEM A: Celestial epoch 1950.0
C                  SYSTEM B: S/C Long, Lat  w/(0,0) @Z-axis, 
C			     longitude in XZ plane, latitude
C			     about Z-axis
C History:
C	C.R. Shrader  GROSSC   GSFC Code 668.1		6/90
C	Chunhui Pan   GROSSC   GSFC Code 668.1		08/2002 convert to C
   */

#include "groview.h"

void getGroView(float **raZ, float **decZ,
     float *raSource, float *decSource, long *nrows, float **scLong,
     float **scLat, float **raX, float **decX, float **scAng,
     float **sx, float **sy, float **sz)
 {
   float jd;
   float scNorthx,scNorthy,scNorthz;
   float *scLongtmp=NULL, *scLattmp=NULL; 
   float *scZx=NULL,*scZy=NULL,*scZz=NULL;
   float *scYx=NULL,*scYy=NULL,*scYz=NULL;
   float *scXx=NULL,*scXy=NULL,*scXz=NULL; 
   float *raY=NULL, *decY=NULL, along, alat;
   float *zLong=NULL, *zLat=NULL, *xLong=NULL, *xLat=NULL;
   float *xLongD=NULL, *xLatD=NULL;
   long npt, num;
   int i=0;
   float raNorth=192.25, decNorth=27.40;

     getNorCord(&raNorth,&decNorth,&scNorthx,&scNorthy,&scNorthz);
    
     /* DEFINE  S/C Z-axis VECTOR:*/
        npt = *nrows;
        getAdxyz(raZ,decZ,&scZx,&scZy,&scZz,&npt);
    
     /* DEFINE  S/C X-axis VECTOR:*/ 
        getAdxyz(raX, decX,&scXx,&scXy,&scXz,&npt); 
    
     /* DEFINE  S/C Y-axis VECTOR:*/
        getCross(&scZx,&scZy,&scZz,&scXx,&scXy,&scXz,&scYx,
               &scYy,&scYz, &npt);

     /* get raY, decY */
        getXyzad( &scYx,&scYy,&scYz,&raY,&decY,&npt);
        
     /* DEFINE SYSB: this is related to the SC body system by a simple parity
        transformation; the sys-B longitude is then in the SC ZX
        plane, and sys-B latitude wrt/SC Z-axis. sys-B long
        lat = (0,0) at Z-axis  */

     /* get Z-axis long. and lat. in rad. */
        getZlongLat(&raY,&decY, &zLong, &zLat, &npt);
       
        getCross(&scZx,&scZy,&scZz,&scYx,
               &scYy,&scYz,&scXx,&scXy,&scXz, &npt);
        
      /* get X-axis long. and lat. */
         getXyzad(&scXx, &scXy, &scXz, &xLongD, &xLatD, &npt);
       
      /* convert deg to rad*/
         getZlongLat(&xLongD, &xLatD, &xLong, &xLat, &npt);
         
      /* defines transformation parameters:*/
         
      /* convert deg to rad*/
         along = (*raSource) *RAD;
         alat = (*decSource) *RAD;
        
      /* long, lat of S/C sys*/
         num = 15;
         getEuler(&zLong, &zLat, &xLong, &xLat,
                  &along, &alat, &scLongtmp, &scLattmp, &num, &npt);
       
	 /* Convert RAD to Deg */
         rad2Deg(&scLongtmp, &scLattmp, scLong, scLat, &npt);
 
      /* get source cartesian in sys-B, then make simple
         transformation to actual SC (x,y,z) */
         getSpaceCor(scLong, scLat,sx,sz,sy,&npt);
         
	 for (i=0; i<npt;i++)(*scLong)[i]=(*scLong)[i] -90;
    
 }
