// xrrtphotonmodel.hh
//
// Class definition for photon sources
// Richard L Fink GSFC/631
// 1997/06/17
// 1997/09/18 Moved error codes from xrrt_types.hh to here. R. Fink
// 1997/09/19 Upgraded documentation. R. Fink

#ifndef XRRTPHOTONMODEL_HH
#define XRRTPHOTONMODEL_HH

//
// System interfaces used
//
#include <exception>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"
#include "xrrtbetamodel.hh"
#include "xrrtflatmodel.hh"
#include "xrrtvector.hh"
#include "xrrtskymodel.hh"
#include "xrrtgroundmodel.hh"


//
// XrrtPhotonModel is a collection class of all different sources of photons
// for XRRT to ray trace. It exists to take the bookkeeping load off xrrtray.
//
class  XrrtPhotonModel
{
    public:
          // Constructor
          XrrtPhotonModel(); 
          // Copy Constructor
          XrrtPhotonModel( const XrrtPhotonModel& photonModel ); 
         
          // Accessor functions
          //
          // setEnergy sets the photon energy.
          void setEnergy (const EnergyInKev& parameter);
          //
          // getEnergy returns the photon energy.
	  EnergyInKev getEnergy() const;

          //
          // Pass thru to XrrtBetaModel
          void setBetaModelCoreRadius(double radius);
          void setBetaModelBeta( double beta);
          // Return Beta model photon 
          void getBetaModelPhoton(const double& modeloffAxisAngleDegree,
                                  const double& modelRotationalAngleDegree,
                                  double& photonRhoDirection, 
                                  double& photonPhiAngle, 
                                  double& photonZDirection);

          //
          // Pass thru to XrrtFlatModel
          void setFlatRadius(double radius);
          // Return Flat model photon 
          void getFlatModelPhoton(const double& modeloffAxisAngleDegree,
                                  const double& modelRotationalAngleDegree,
                                  double& photonRhoDirection, 
                                  double& photonPhiAngle, 
                                  double& photonZDirection);

         //
         // Return Point source photon
         void getPointSourcePhoton(const double& modeloffAxisAngleDegree,
                                   const double& modelRotationalAngleDegree,
                                   double& photonRhoDirection,
                                   double& photonPhiAngle,
                                   double& photonZDirection);
         //
         // FITS file based functions
         //
         // Set up the Sky Model so that it can be read
         void openSkyModel(const string& fitsFileName, int& modelRowCount);
         //
         // Set up the Ground Model so that it can be read
         void openGroundModel(const string& fitsFileName, int& modelRowCount);
         //
         // Return the active FITS file current row
         long getFitsModelRow() const;
         //
         // Return a Sky Model photon from the FITS file
         void getSkyModelPhoton(double& energyInKeV,
                                double& offAxisAngleDegree,
                                double& rotationAngleDegree,
                                double& photonUnitRadial,
                                double& photonPhiAngle,
                                double& photonUnitZ);
         //
         // Return a Ground Model photon from the FITS file
         void getGroundModelPhoton(double& energyInKeV,
                                   double& radiusInMM,
                                   double& telescopeRotationAngleRadian,
                                   double& zDistanceInMM,
                                   double& photonUnitRadial,
                                   double& photonPhiAngle,
                                   double& photonUnitZ);

         // Error messages
         string errorMessage(XrrtSkyModelErrorCode errorCode);
         string errorMessage(XrrtGroundModelErrorCode errorCode);

    private:
          //
          // Data members
          //
          // The last energy requested. All later photons continue
          // with this energy unti lit changed.
          EnergyInKev        energy;
          //
          // Class storage for a Beta Model photon source
          XrrtBetaModel      betaModel;
          //
          // Class storage for a Flat Model photon source
          XrrtFlatModel      flatModel;
          //
          // For models that input from a FITS file, the current table row.
          long               fitsFileCurrentRow;
          //
          // Whether a XrrtSkyModel is active
          bool               skyModelOpen;
          // 
          // Whether a XrrtGrounfModel is active
          bool               groundModelOpen;
          //
          // Class storage
          XrrtSkyModel       skyModel;
          //
          // Class storage
          XrrtGroundModel    groundModel;
          
}; 



#endif
