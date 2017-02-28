// xrrtflatmodel.cc
//
// Member definition for photon source (Flat model)
// Richard L Fink GSFC/631
// 1997/06/17


#include "xrrtflatmodel.hh"

XrrtFlatModel::XrrtFlatModel():
    flatRadiusInArcmin(0)
{
}

void
XrrtFlatModel::getFlatModelAngles(double& modelOffAxisAngleRadian, 
                                  double& modelRotationAngleRadian)
{
// Return a flat model photon assuming that the source is on-axis.
//
// CODE FROM NAGOYA UNIVERSITY RAY2AREAV6_0CAL FOR ASCA
//       if(diftyp.eq.0.0) then
//          bb       = Raytraceran()
//          bc       = Raytraceran()
//          tta1     = 0.0D0  * DEG2RAD
//          tta2     = difrad * DEG2RAD / 60.0d0
//          tta      = -dcos(tta1)+(-dcos(tta2)-(-dcos(tta1)))*bb
//          theed   = dacos(-tta)
//          phid      = 360.00D0*DEG2RAD*bc
//        endif

const double DEGREE2RADIAN = 0.017453292519943295769e0;
const double ARCMIN2DEGREE = 1.0e0/60.0e0;

     double fracOfRadius = xrrtrandom();
     double fracOfCircle = xrrtrandom();
     double tta2 = flatRadiusInArcmin*ARCMIN2DEGREE*DEGREE2RADIAN;
     double tta = -1.0e0+(1.0e0 - cos(tta2))*fracOfRadius;
     modelOffAxisAngleRadian = acos(-tta);
     modelRotationAngleRadian = 360.0e0*DEGREE2RADIAN*fracOfCircle;
}



void
XrrtFlatModel::setFlatRadius(double radiusInArcmin)
{
      flatRadiusInArcmin = radiusInArcmin;
}

