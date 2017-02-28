// Runs the APED interpolation
//      parameters (params):
//           0:      kT temperature in keV
//       1..13:      Metal abundances (H and He fixed at cosmic)
//          14:      redshift z
//           Norm = (4 * pi * 1e14)^-1 * Int(n^2)dV / D^2 where n is
//           in cm^-3 and D is the distance in cm.

#include "xsTypes.h"
#include <XSFunctions/functionMap.h>

// from calcMultiTempPlasma.cxx
int calcMultiTempPlasma(const RealArray& energyArray, const int plasmaType, 
			const IntegerArray& Zarray, const RealArray& abun, 
			const Real dens, const Real z, const Real T, 
			const Real DEM, const int ifl, const bool qtherm, 
			const Real velocity, RealArray& fluxArray, 
			RealArray& fluxErrArray);


void vapec(const RealArray& energyArray, const RealArray& params, 
 	   int spectrumNumber, RealArray& fluxArray, RealArray& fluxErrArray,
	   const string& initString)
{
  const size_t nAp(14);
  const int apelt[] = {1, 2, 6, 7, 8, 10, 12, 13, 14, 16, 18, 20, 26, 28};

  RealArray abun(nAp);
  IntegerArray Zarray(nAp);
  for (size_t i=0; i<nAp; i++) Zarray[i] = apelt[i];
  abun[0] = 1.0;
  for (size_t i=1; i<nAp; i++) abun[i] = params[i];

  Real T = params[0];
  Real z = params[14];
  calcMultiTempPlasma(energyArray, 6, Zarray, abun, (Real)1.0, z, T, 
		      (Real)1.0, spectrumNumber, false, (Real)0.0, 
		      fluxArray, fluxErrArray);
  return;
}