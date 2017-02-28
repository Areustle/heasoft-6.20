#include	<stdio.h>
#include	<string.h>
#include	<math.h>
#include        "atFunctions.h"

int
atQuatToEuler(
   AtQuat      quat,     /* typedef double AtQuat[4];                              */
   AtEulerAng  *eul      /* typedef struct {double phi; double theta; double psi;} */
)                        /*                      AtEulerAng;  Euler Angle (radian) */
{
    AtRotMat    rm;     /* typedef double AtRotMat[3][3];                         */
    int         iret;

    iret = atQuatToRM( quat, rm );
    if ( iret == 0 )  iret = atRMToEuler( rm, eul );
    return(iret);
}
