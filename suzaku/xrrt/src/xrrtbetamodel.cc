// xrrtbetamodel.cc
//
// Class definition for photon source (Beta model)
// Richard L Fink GSFC/631
// 1997/06/17


#include "xrrtbetamodel.hh"

XrrtBetaModel::XrrtBetaModel():
    coreRadiusArcMin(0),
    beta(0)
{
}

void
XrrtBetaModel::getBetaModelAngles(double& modelOffAxisAngleRadian, 
                                  double& modelRotationAngleRadian)
{
// Return a beta model photon assuming that the source is on-axis.
//
// CODE FROM NAGOYA UNIVERSITY RAY2AREAV6_0CAL FOR ASCA
//C
//C     subroutine raytracebetamdl(theeta,phi)
//C
//C     input     difrad : core radius of beta model
//C               difbet : beta of beta model
//C
//C     output    theeta : off-set angle of incident photon
//C               phi    : rooll angle of incident photon
//C
//C
//C                          '94.1.25
//C                          coded by Fumie Akimoto
//
//      subroutine raytracebetamdl(theeta,phi)
//
//      implicit double precision (a-h,o-z) 
//      double precision Raytraceran
//      common /angle/  angin,angphi,diftyp,difrad,difbet
//      data deg /1.745329252d-2/
//
// 222  continue
//      bb     = Raytraceran()
//      if (bb.le.0.0d0) goto 222
//      bc     =Raytraceran()
//      theeta =difrad*dsqrt(bb**(1/(1.5D0-3.0D0*difbet))-1)
//      theeta =theeta/60.0d0 * deg
//      phi    =360.00D0*deg*bc
//
//      if(theeta.gt.0.04) goto 222
//
//      return
//      end

const double DEGREE2RADIAN = 0.017453292519943295769e0;
const double ARCMIN2DEGREE = 1.0e0/60.0e0;

     double offsetAngle = 1.0e0;
     while (offsetAngle > 0.04e0)
         {
         double fracOfRadius = xrrtrandom();
         if (fracOfRadius == 0.0e0)
            {
            continue;
            }
         double fracOfCircle = xrrtrandom();
         double power = 1.0e0/(1.5e0 - 3.0e0*beta);
         offsetAngle = coreRadiusArcMin*sqrt(pow(fracOfRadius,power)-1.0e0);
         modelOffAxisAngleRadian = offsetAngle*ARCMIN2DEGREE*DEGREE2RADIAN;
         modelRotationAngleRadian = 360.0e0*DEGREE2RADIAN*fracOfCircle;
         }
}



void
XrrtBetaModel::setCoreRadius(double radiusInArcMin)
{
      coreRadiusArcMin = radiusInArcMin;
}


void
XrrtBetaModel::setBeta(double parameter)
{
      beta = parameter;
}


