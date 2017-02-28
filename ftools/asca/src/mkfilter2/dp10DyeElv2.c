/*
  dp10DyeElv2.c 	   return value : elevation in radian

        Originally coded                  by C.Otani   1993.2.12
        Modified to use for sisAdSet.c    by T.Dotani
	Modified to use for dp10DyeElv.c  by C.Otani
	Modified                          by C.Otani   1993.12.14
*/

#include <math.h>
#include <float.h>
#include        "mkf2_recd.h"
#include        "mkfilter.h"

#define DYEDIRECT -70.0
#define CANNOTSEE 120.0
#define DEBUG1

double dp10DyeElv
  (
   AtVect  vSat,      /*  (in) sidereal   */
   AtVect  vSun,      /*  (in) sidereal   */
   AtVect  vTgt       /*  (in) sidereal   */
   )
{
  int      i;
  AtVect   vEarth, vSat2, nvTgt, vTgt2, vD;
  AtRotMat RM;
  double   elv, angEarth, dAng, xDist, cross, hanteishiki, dot, satDist;
  double   xyradius;
  double   erad=EARTH_RADIUS;


/*   elevation angle   */
  atNormVect(vTgt,nvTgt);
  atInvVect(vSat,vEarth);          
  satDist = atNorm(vEarth);
  angEarth = asin(erad/satDist);
  atAngDistance(nvTgt, vEarth, &xDist);
  elv = xDist - angEarth;


/*   coordinate transformation   */
  atSetRotMatZX( vSun, vSat, RM);
  atRotVect( RM, vSat, vSat2);
  atRotVect( RM,nvTgt, vTgt2);


/*   Check data   */
/*
  if ( vSat2[0]<0.0 || fabs(vSat2[1])>1.0e-6 ){
    fprintf(stderr,"There is a bug in dp10DyeElv2.c.Call Otani (y=%le)\n",
	    vSat2[1]);
    exit(-1);
  }
  else{
    vSat2[1]=0.0;
  }
*/


/* If xyradius <= eradius, the result is very simple. */
  xyradius = vSat2[0];       /*  always vSat2[1]==0.0 */
  if ( xyradius <= erad ){
    if ( vSat2[2] <= 0.0  ){
#ifdef DEBUG
        fprintf ( stderr, "1:dp10DyeElv2:ret=%lf\n", CANNOTSEE*DEG2RAD );
#endif
        return CANNOTSEE*DEG2RAD;       /*  night  */
      }
    else {
#ifdef DEBUG
      if ( elv >= 0.0 ){
	  fprintf ( stderr, "2:dp10DyeElv2:ret=%lf\n", elv );
	}
      else {
	  fprintf ( stderr, "3:dp10DyeElv2:ret=%lf\n", DYEDIRECT*DEG2RAD );
	}
#endif
      return ( ( elv >= 0.0 ) ? elv : DYEDIRECT*DEG2RAD );
    }
  }


/* satellite is looking at th Earth */
  if( elv <= 0.0 ) {
    dot = atScalProd(vEarth,vTgt);
    cross = vSat2[2]
      + vTgt2[2]*(dot - sqrt(erad*erad-satDist*satDist+dot*dot));

    /* looking at the bright earth */
    if( cross >= 0.0 ) {
#ifdef DEBUG
      fprintf ( stderr, "4:dp10DyeElv2:ret=%lf\n", DYEDIRECT*DEG2RAD );
#endif
      return DYEDIRECT*DEG2RAD;
    }

    /* looking at the dark earth */
    else {   /* sat can see bright earth */
      NUMCAL2( vSat2, vTgt2, erad, dAng);
#ifdef DEBUG
      fprintf ( stderr, "5:dp10DyeElv2:ret=%lf\n", dAng );
#endif
      return dAng;           /* near bright earth */
    }
  }

/* looking at the space */
  else {
    dot = fabs(atScalProd(vSat2,vTgt2));
    for (i=0;i<3;i++)   vD[i] = vSat2[i]+vTgt2[i]*dot;
    cross = (vD[2]/atNorm(vD))*erad;

   /* near the bright earth */
    if( cross >= 0.0 ) {
#ifdef DEBUG
      fprintf ( stderr, "6:dp10DyeElv2:ret=%lf\n", elv );
#endif
      return elv;
    }

    /* near the dark earth */
    else {
      NUMCAL2( vSat2, vTgt2, erad, dAng);
#ifdef DEBUG
      fprintf ( stderr, "7:dp10DyeElv2:ret=%lf\n", dAng );
#endif
      return dAng;
    }
  }
}

