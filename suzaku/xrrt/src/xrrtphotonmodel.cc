// xrrtphotonmodel.cc
//
// Member definition for photon sources
// Richard L Fink GSFC/631
// 1997/06/17
// 1997/09/24 Upgrade documentation. R. Fink
//
// 2006/06/28 Y.ISHISAKI	version 6.3.10
//    check if both arguments are 0.0 before atan()

//#include <iostream.h>
#include "xrrtphotonmodel.hh"

XrrtPhotonModel::XrrtPhotonModel():
    energy(0),
    betaModel(),
    flatModel(),
    fitsFileCurrentRow(0),
    skyModelOpen(false),
    groundModelOpen(false),
    skyModel(),
    groundModel()
{
// A simple constructor
}

string
XrrtPhotonModel::errorMessage(XrrtGroundModelErrorCode errorCode)
{
//
// A pass thru to the real source of the error code
//
string errorMessage;

     errorMessage = groundModel.errorMessage(errorCode);
     return errorMessage;
}
    
string
XrrtPhotonModel::errorMessage(XrrtSkyModelErrorCode errorCode)
{
//
// A pass thru to the real source of the error code
//
string errorMessage;

    errorMessage = skyModel.errorMessage(errorCode);
    return errorMessage;
}


void
XrrtPhotonModel::getPointSourcePhoton(
    const double& modelOffAxisAngleDegree,
    const double& modelRotationalAngleDegree,
          double& photonRhoDirection,
          double& photonPhiAngle,
          double& photonZDirection)
{
//
// Compute point source photons
//
const double DEGREE2RADIAN = 0.017453292519943295769e0;

     // Convert incoming angles to radians
     double modelOffAxisAngleRadian = modelOffAxisAngleDegree * 
                                      DEGREE2RADIAN;
     double modelRotationalAngleRadian = modelRotationalAngleDegree * 
                                         DEGREE2RADIAN;

     // Return the appropriate photon values

     // Spherical to cylindrical coords rho = sin(theta);
     photonRhoDirection = sin(modelOffAxisAngleRadian);

     // The convention from ray2areaV6_0cal is to keep the same quadrant
     photonPhiAngle     = modelRotationalAngleRadian;

     // Spherical to cylindrical z = cos(theta) and -ve since moving downwards
     photonZDirection   = -cos(modelOffAxisAngleRadian);

}


void
XrrtPhotonModel::getBetaModelPhoton(
    const double& modelOffAxisAngleDegree,
    const double& modelRotationalAngleDegree,
          double& photonRhoDirection,
          double& photonPhiAngle,
          double& photonZDirection)
{
// Code from NAGOYA UNIVERSITY RAY2AREAV6_0CAL
//CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
//C--------1---------2---------3---------4---------5---------6---------7--
//C SUBROUTINE : RAYTRACEPHOTONGEN
//C     photon generator                96.04.13 A. Furuzawa
//C
//      subroutine raytracephotongen(phth,phphi)
//C
//C     Input(Common)                            
//C       diftyp:R8  :source type(<0:point, =1:beta model, =0:flat diffuse
//C       angin :R8  :source position(off-axis angle;deg)
//C       angphi:R8  :source position(  phase angle ;deg)
//C       difrad:R8  :source radius
//C                     (flat diffuse:radius, core radius:beta model)
//C       difbet:R8  :beta(beta model only)
//C     Output
//C       phth  :R8  :direction of incident photon(radial;deg)
//C       phphi :R8  :direction of incident photon(phase;deg)
//CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
//      implicit none
//
//C     For PARAMETER
//      double precision MPI, M2PI, M90DEG, DEG2RAD
//      Parameter (MPI = 3.1415926535897932385d0)
//      Parameter (M2PI = 2.d0*MPI)
//      Parameter (M90DEG = MPI/2d0)
//      Parameter (DEG2RAD = 0.017453292519943295769d0)
//
//C     For OUTPUT
//      double precision phth,phphi
//
//C     For LOCAL
//      double precision theeta,phi
//      double precision bb,bc,tta1,tta2,tta,theed,phid,drayn(3),rayn(3)
//
//C     For COMMON
//C      integer diftyp
//      double precision angin,angphi,diftyp,difrad,difbet
//      common /angle/ angin,angphi,diftyp,difrad,difbet
//
//C     For EXTERNAL
//      double precision Raytraceran
//
//C=========
//C---------
//      theeta   = DEG2RAD * angin
//      phi      = DEG2RAD * angphi
//
//      if(diftyp.eq.0.0.or.diftyp.eq.1.0) then
//         if(diftyp.eq.0.0) then
//            bb       = Raytraceran()
//            bc       = Raytraceran() 
//            tta1     = 0.0D0  * DEG2RAD
//            tta2     = difrad * DEG2RAD / 60.0d0
//            tta      = -dcos(tta1)+(-dcos(tta2)-(-dcos(tta1)))*bb
//            theed   = dacos(-tta)
//            phid      = 360.00D0*DEG2RAD*bc
//         else
//            call raytracebetamdl(theed,phid)
//         endif
//         drayn(1)  = dsin(theed)*dcos(phid)
//         drayn(2)  = dsin(theed)*dsin(phid)
//         drayn(3)  = -dcos(theed)
//         call raytracerotate(drayn,theeta,phi,rayn)
//C
//         phth = dacos(-rayn(3))
//         if(rayn(1).gt.0.d0) then
//            phphi=datan(rayn(2)/rayn(1))+M2PI
//            if(phphi.ge.M2PI) then
//               phphi=phphi-M2PI
//            endif
//         else if(rayn(1).lt.0.d0) then
//            phphi=datan(rayn(2)/rayn(1))+MPI
//         else if(rayn(2).ge.0.d0) then
//            phphi=M90DEG
//         else
//            phphi=M90DEG*3.d0
//         endif
//         phth  = phth/DEG2RAD
//         phphi = phphi/DEG2RAD
//      else
//         phth  = angin
//         phphi = angphi
//      endif
//
//      return
//      end

const double DEGREE2RADIAN =  0.017453292519943295769e0;

double modelOffAxisAngleRadian;
double modelRotationalAngleRadian;
double betaTheta;
double betaPhi;

     // Convert incoming angles to radians
     modelOffAxisAngleRadian = modelOffAxisAngleDegree * DEGREE2RADIAN;
     modelRotationalAngleRadian = modelRotationalAngleDegree * DEGREE2RADIAN;

     // Get Beta model angles
     // the beta model returns angles assuming the source is on-axis
     betaModel.getBetaModelAngles(betaTheta, betaPhi);

     // adjust the model angles to account for the offset requested
     photonRhoDirection = sin(betaTheta+modelOffAxisAngleRadian);
     photonPhiAngle = betaPhi + modelRotationalAngleRadian;
     photonZDirection = -cos(betaTheta+modelOffAxisAngleRadian);

}

void
XrrtPhotonModel::getFlatModelPhoton(
    const double& modelOffAxisAngleDegree,
    const double& modelRotationalAngleDegree,
          double& photonRhoDirection,
          double& photonPhiAngle,
          double& photonZDirection)
{

//CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
//C--------1---------2---------3---------4---------5---------6---------7--
//C SUBROUTINE : RAYTRACEPHOTONGEN
//C     photon generator                96.04.13 A. Furuzawa
//C
//      subroutine raytracephotongen(phth,phphi)
//C
//C     Input(Common)                            
//C       diftyp:R8  :source type(<0:point, =1:beta model, =0:flat diffuse
//C       angin :R8  :source position(off-axis angle;deg)
//C       angphi:R8  :source position(  phase angle ;deg)
//C       difrad:R8  :source radius
//C                     (flat diffuse:radius, core radius:beta model)
//C       difbet:R8  :beta(beta model only)
//C     Output
//C       phth  :R8  :direction of incident photon(radial;deg)
//C       phphi :R8  :direction of incident photon(phase;deg)
//CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
//      implicit none
//
//C     For PARAMETER
//      double precision MPI, M2PI, M90DEG, DEG2RAD
//      Parameter (MPI = 3.1415926535897932385d0)
//      Parameter (M2PI = 2.d0*MPI)
//      Parameter (M90DEG = MPI/2d0)
//      Parameter (DEG2RAD = 0.017453292519943295769d0)
//
//C     For OUTPUT
//      double precision phth,phphi
//
//C     For LOCAL
//      double precision theeta,phi
//      double precision bb,bc,tta1,tta2,tta,theed,phid,drayn(3),rayn(3)
//
//C     For COMMON
//C      integer diftyp
//      double precision angin,angphi,diftyp,difrad,difbet
//      common /angle/ angin,angphi,diftyp,difrad,difbet
//
//C     For EXTERNAL
//      double precision Raytraceran
//
//C=========
//C---------
//      theeta   = DEG2RAD * angin
//      phi      = DEG2RAD * angphi
//
//      if(diftyp.eq.0.0.or.diftyp.eq.1.0) then
//         if(diftyp.eq.0.0) then
//            bb       = Raytraceran()
//            bc       = Raytraceran() 
//            tta1     = 0.0D0  * DEG2RAD
//            tta2     = difrad * DEG2RAD / 60.0d0
//            tta      = -dcos(tta1)+(-dcos(tta2)-(-dcos(tta1)))*bb
//            theed   = dacos(-tta)
//            phid      = 360.00D0*DEG2RAD*bc
//         else
//            call raytracebetamdl(theed,phid)
//         endif
//         drayn(1)  = dsin(theed)*dcos(phid)
//         drayn(2)  = dsin(theed)*dsin(phid)
//         drayn(3)  = -dcos(theed)
//         call raytracerotate(drayn,theeta,phi,rayn)
//C
//         phth = dacos(-rayn(3))
//         if(rayn(1).gt.0.d0) then
//            phphi=datan(rayn(2)/rayn(1))+M2PI
//            if(phphi.ge.M2PI) then
//               phphi=phphi-M2PI
//            endif
//         else if(rayn(1).lt.0.d0) then
//            phphi=datan(rayn(2)/rayn(1))+MPI
//         else if(rayn(2).ge.0.d0) then
//            phphi=M90DEG
//         else
//            phphi=M90DEG*3.d0
//         endif
//         phth  = phth/DEG2RAD
//         phphi = phphi/DEG2RAD
//      else
//         phth  = angin
//         phphi = angphi
//      endif
//
//      return
//      end

const double DEGREE2RADIAN =  0.017453292519943295769e0;

double modelOffAxisAngleRadian;
double modelRotationalAngleRadian;
double flatTheta;
double flatPhi;

     // Convert incoming angles to radians
     modelOffAxisAngleRadian = modelOffAxisAngleDegree * DEGREE2RADIAN;
     modelRotationalAngleRadian = modelRotationalAngleDegree * DEGREE2RADIAN;

     // Get Flat model angles
     // the flat model returns angles assuming the source is on-axis
     flatModel.getFlatModelAngles(flatTheta, flatPhi);

     // Transform the model angles to the shifted model center
     photonRhoDirection = sin(flatTheta+modelOffAxisAngleRadian);
     photonPhiAngle = flatPhi + modelRotationalAngleRadian;
     photonZDirection = -cos(flatTheta+modelOffAxisAngleRadian);
}

void
XrrtPhotonModel::openSkyModel(const string& fitsFileName, int& modelRowCount)
{
// 
// Open a Sky model FITS file for reading
//
    string extensionName = "sky";
    skyModel.openSkyFitsFile(fitsFileName, extensionName);
    modelRowCount = skyModel.getMaxFitsModelRow();
    skyModelOpen = true;
}

void
XrrtPhotonModel::openGroundModel(const string& fitsFileName, int& modelRowCount)
{
//
// Open a Ground model FITS file for reading
//
    string extensionName = "ground";
    groundModel.openGroundFitsFile(fitsFileName, extensionName);
    modelRowCount = groundModel.getMaxFitsModelRow();
    groundModelOpen = true;
}

void
XrrtPhotonModel::getSkyModelPhoton(double& energyInKeV,
                                   double& offAxisAngleDegree,
                                   double& rotationAngleDegree,
                                   double& photonUnitRadial,
                                   double& photonPhiAngle,
                                   double& photonUnitZ)
{
//
// Read a sky model photon from the FITS file
//
const double DEGREE2RADIAN =  0.017453292519943295769e0;
    double thetaRadian = 0;
    double phiRadian = 0;
    skyModel.getSkyModelPhoton(energyInKeV,
                               thetaRadian,
                               phiRadian);
     // Transform the model angles to the shifted model center
     double offAxisAngleRadian = offAxisAngleDegree * DEGREE2RADIAN;
     double rotationAngleRadian= rotationAngleDegree * DEGREE2RADIAN;
     photonUnitRadial = sin(thetaRadian+offAxisAngleRadian);
     photonPhiAngle   = phiRadian + rotationAngleRadian;
     photonUnitZ      = -cos(thetaRadian+offAxisAngleRadian);
}

void
XrrtPhotonModel::getGroundModelPhoton(double& energyInKeV,
                                      double& radiusInMM,
                                      double& telescopeRotationAngleRadian,
                                      double& zDistanceInMM,
                                      double& photonUnitRadial,
                                      double& photonPhiAngle,
                                      double& photonUnitZ)
{
//
// Read a Ground model photon from the FITS file
//
    double xInMM = 0;
    double yInMM = 0;
    double zInMM = 0;
    double unitX = 0;
    double unitY = 0;
    double unitZ = 0;
    groundModel.getGroundModelPhoton(energyInKeV,
                                     xInMM, yInMM, zInMM,
                                     unitX, unitY, unitZ);
    // 
    radiusInMM       = sqrt(xInMM*xInMM + yInMM*yInMM);
    telescopeRotationAngleRadian =
		( 0.0 == radiusInMM ) ? 0.0 : atan2(yInMM, xInMM);
    zDistanceInMM    = zInMM;
    photonUnitRadial = sqrt(unitX*unitX + unitY*unitY);
    photonPhiAngle   = ( 0.0 == photonUnitRadial ) ? 0.0 : atan2(unitY, unitX);
    photonUnitZ      = unitZ;
}


void 
XrrtPhotonModel::setEnergy (const EnergyInKev& parameter)
{
     energy = parameter;
}

EnergyInKev 
XrrtPhotonModel::getEnergy() const
{
     return energy;
}

void
XrrtPhotonModel::setBetaModelCoreRadius(double radius)
{
    betaModel.setCoreRadius(radius);
}

void
XrrtPhotonModel::setBetaModelBeta( double beta)
{
    betaModel.setBeta(beta);
}

void
XrrtPhotonModel::setFlatRadius(double radius)
{
    flatModel.setFlatRadius(radius);
}

long
XrrtPhotonModel::getFitsModelRow() const
{
    long currentRow = 0;
    if (skyModelOpen) currentRow = skyModel.getFitsModelRow();
    if (groundModelOpen) currentRow = groundModel.getFitsModelRow();
    return currentRow;
}


