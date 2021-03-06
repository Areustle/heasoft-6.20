// XSPEC subroutine to calculate the new Mewe-Kaastra-Liedahl
// plasma emission spectrum.

// parameters :
//    0.........kT (keV)
//    1.........nH (cm^-3)           fixed at 1 for most applications
//    2.........He abundance
//    3.........C     "
//    4.........N     "
//    5.........O     "
//    6.........Ne    "
//    7.........Na    "
//    8.........Mg    "
//    9.........Al    "
//   10.........Si    "
//   11.........S     "
//   12.........Ar    "
//   13.........Ca    "
//   14.........Fe    "
//   15.........Ni    "
//   16.........Redshift
//   17.........Switch (0=calculate, 1=interpolate)


#include "xsTypes.h"
#include <XSFunctions/functionMap.h>

// from calcMultiTempPlasma.cxx
int calcMultiTempPlasma(const RealArray& energyArray, const int plasmaType, 
			const IntegerArray& Zarray, const RealArray& abun, 
			const Real dens, const Real z, const Real T, 
			const Real DEM, const int ifl, const bool qtherm, 
			const Real velocity, RealArray& fluxArray, 
			RealArray& fluxErrArray);


void vmekal(const RealArray& energyArray, const RealArray& params, 
	    int spectrumNumber, RealArray& fluxArray, RealArray& fluxErrArray,
	    const string& initString)
{
  size_t nMK(15);
  const int mkelt[] = {1, 2, 6, 7, 8, 10, 11, 12, 13, 14, 16, 18, 20, 26, 28};

  int swtch = round(params[17]);
  // if interpolating on the table then don't include the H abundance
  if ( swtch == 1 ) nMK--;

  RealArray abun(nMK);
  IntegerArray Zarray(nMK);
  if ( swtch == 0 ) {
    for(size_t i=0; i<nMK; i++) Zarray[i] = mkelt[i];
    abun[0] = 1.0;
    for(size_t i=0; i<nMK; i++) abun[i] = params[i+1];
  } else if ( swtch == 1 ) {
    for(size_t i=0; i<nMK; i++) Zarray[i] = mkelt[i+1];
    for(size_t i=0; i<nMK; i++) abun[i] = params[i+2];
  }

  int plasmaType = 3 + swtch;
  Real T = params[0];
  Real dens = params[1];
  Real z = params[16];
  calcMultiTempPlasma(energyArray, plasmaType, Zarray, abun, dens, z, T, 
		      (Real)1.0, spectrumNumber, false, (Real)0.0, fluxArray, 
		      fluxErrArray);
  return;
}
