// Generic code for calculating lines with various shapes

#define GAUSS 0
#define LORENTZ 1
#define VOIGT 2

#define SQRT2 1.4142135623730950488

#include <xsTypes.h>
#include <XSstreams.h>
#include <XSUtil/Numerics/BinarySearch.h>
#include <cmath>
#include <sstream>
#include <iostream>

Real lineFraction(const int lineShape, const Real energy, const Real ecenter, 
		  const RealArray& lineParams, const bool qspeedy);
void calcManyLines(const RealArray& energyArray, const RealArray& ecenterArray,
		   const std::vector<RealArray>& lineParamsArray,
		   const RealArray& linefluxArray, const Real crtLevel,
		   const int lineShape, const bool qspeedy, RealArray& fluxArray);
void calcLine(const RealArray& energyArray, const Real ecenter, 
	      const RealArray& lineParams, const Real lineflux, const Real crtLevel, 
	      const int lineShape, const bool qspeedy, RealArray& fluxArray);

Real gaussFraction(const Real deltasigma, const bool qspeedy);
Real lorentzFraction(const Real deltasigma, const bool qspeedy);
Real voigtFraction(const Real energy, const Real ecenter, const Real sigma, 
		   const Real gamma, const bool qspeedy);

// to calculate line shapes. If qspeedy is true then uses a tabulation
// otherwise does a proper calculation. Lines are calculated down to crtlevel*(flux
// in center bin)

void calcManyLines(const RealArray& energyArray, const RealArray& ecenterArray,
		   const std::vector<RealArray>& lineParamsArray,
		   const RealArray& linefluxArray, const Real crtLevel, 
		   const int lineShape, const bool qspeedy, RealArray& fluxArray)
{
  // Find the bins containing the line centers. This assumes that the ecenter
  // array is in increasing order of energy. If the center energy falls outside
  // the energy array then lineCenterBin will be set to -1. Otherwise
  // energyArray[lineCenterBin] < ecenterArray <= energyArray[lineCenterBin+1]

  IntegerArray lineCenterBin(ecenterArray.size());
  lineCenterBin = Numerics::BinarySearch(energyArray, ecenterArray);

  Real efirst = energyArray[0];
  int nE = energyArray.size();
  Real elast = energyArray[nE-1];

  // Loop around the lines

  for (size_t iline=0; iline<ecenterArray.size(); iline++) {

    int icen = lineCenterBin[iline];
    Real ecenter = ecenterArray[iline];
    const RealArray& lineParams = lineParamsArray[iline];
    Real lineflux = linefluxArray[iline];

    // first do case of zero width line. assume for the moment that this occurs
    // if all the lineParams are zero

    bool deltaFunction(true);
    for (size_t i=0; i<lineParams.size(); i++) {
      if (lineParams[i] != 0.0) {
	deltaFunction = false;
	break;
      }
    }

    if ( deltaFunction ) {
      if ( icen != -1 ) fluxArray[icen] += lineflux;
      continue;
    }

    // If the line center is below first bin then don't calculate the 
    // lower part of the line. If line center is above the first bin
    // then just calculate part of line within the energy range.

    if ( ecenter >= efirst ) {
      int ielow = icen;
      Real alow = 0.0;
      Real fractionInsideRange = lineFraction(lineShape, efirst, ecenter, lineParams, qspeedy)/2;
      if ( ecenter > elast ) {
	ielow = nE-2;
	alow = lineFraction(lineShape, elast, ecenter, lineParams, qspeedy);
	fractionInsideRange -= alow/2;
      }

      // Do the low energy part of the line

      Real lineSum = 0.0;
      Real ahi;
      while ( ielow >= 0 ) {
	ahi = lineFraction(lineShape, energyArray[ielow], ecenter, lineParams, qspeedy);
	Real fract = (ahi-alow)/2;

	fluxArray[ielow] += fract*lineflux;
	lineSum += fract;
	if ( (fractionInsideRange-lineSum) < crtLevel ) {
	  // Too many sigma away so stop now and add the rest of the line into this
	  // bin. Not strictly correct but shouldn't matter and ensures that the total
	  // flux is preserved.
	  fluxArray[ielow] += (fractionInsideRange-lineSum)*lineflux;
	  ielow = 0;
	}
	alow = ahi;
	ielow -= 1;
      }
    }

    // If the line center is above the last bin then don't calculate 
    // the upper part of the line. If line center is below first bin then 
    // just calculate the part of the line within energy range.

    if ( ecenter <= elast ) {
      Real alow = 0.0;
      int ielow = icen;
      Real fractionInsideRange = lineFraction(lineShape, elast, ecenter, lineParams, qspeedy)/2;
      if ( ecenter <  efirst ) {
	ielow = 0;
	alow = lineFraction(lineShape, energyArray[ielow], ecenter, lineParams, qspeedy);
	fractionInsideRange -= alow/2;
      }

      // Do the high energy part of the line

      Real lineSum = 0.0;
      Real ahi;
      while ( ielow < nE-1 ) {
	ahi = lineFraction(lineShape, energyArray[ielow+1], ecenter, lineParams, qspeedy);
	Real fract = (ahi-alow)/2;
	fluxArray[ielow] += fract*lineflux;
	lineSum += fract;
	if ( (fractionInsideRange-lineSum) < crtLevel ) {
	  // Too many sigma away so stop now and add the rest of the Gaussian into this
	  // bin. Not strictly correct but shouldn't matter and ensures that the total
	  // flux is preserved.
	  fluxArray[ielow] += (fractionInsideRange-lineSum)*lineflux;
	  ielow = nE;
	}
	alow = ahi;
	ielow += 1;
      }
    }
	  
  }

  return;
}

void calcLine(const RealArray& energyArray, const Real ecenter, 
	      const RealArray& lineParams, const Real lineflux, const Real crtLevel, 
	      const int lineShape, const bool qspeedy, RealArray& fluxArray)
{
  RealArray ecenterArray(ecenter,1);
  std::vector<RealArray> lineParamsArray(1,lineParams);
  RealArray linefluxArray(lineflux,1);
  calcManyLines(energyArray, ecenterArray, lineParamsArray, linefluxArray, 
		crtLevel, lineShape, qspeedy, fluxArray);
  return;
}

Real lineFraction(const int lineShape, const Real energy, const Real ecenter,
		  const RealArray& lineParams, const bool qspeedy)
{
  static bool first(true);
  static Real saveEcenter, saveWidth, lnorm;
  if ( lineShape == GAUSS ) {
    return gaussFraction(fabs(energy-ecenter)/lineParams[0], qspeedy);
  } else if ( lineShape == LORENTZ ) {
    if ( first || ecenter != saveEcenter || saveWidth != lineParams[0] ) {
      saveEcenter = ecenter;
      saveWidth = lineParams[0];
      lnorm = 1.0/(M_PI/2.0 - atan(-2.0*ecenter/lineParams[0]));
      first = false;
    }
    Real lfrac = lorentzFraction(fabs(energy-ecenter)/lineParams[0], qspeedy);
    return lfrac * lnorm;
  } else if ( lineShape == VOIGT ) {
    return voigtFraction(energy, ecenter, lineParams[0], lineParams[1], qspeedy);
  } else {
    return 0.0;
  }
}

Real gaussFraction(const Real deltasigma, const bool qspeedy)
{

  // Function to return the integral of a Gaussian(0,1) distribution from 
  // -deltasigma to +deltasigma
  // If qspeedy=true interpolates on a previously calculated grid of erf calculations
  // while if qspeedy=false then calls erf each time.

  static const size_t GaussMaxStep(1200);
  static const size_t GaussMax(6);
  static const Real GaussStep = (Real)GaussMax / GaussMaxStep;

  static RealArray tabErf(GaussMaxStep+1);
  static bool first(true);

  // if the first time through then calculate the erf grid on which we
  // will interpolate

  if ( qspeedy && first ) {
    for (size_t i=0; i<=GaussMaxStep; i++) {
      tabErf[i] = erf(i*GaussStep/SQRT2);
    }
    first = false;
  }
  
  // how many sigmas away we are
  Real x = fabs( deltasigma );

  if ( qspeedy ) {

    // Now interpolate from the table
    size_t index = (size_t)(x/GaussStep);

    // If we're past the edge of the tabulated data return 1.0
    if ( index >= GaussMaxStep ) return 1.0;

    Real remainder = (x - (Real)index*GaussStep) * (1.0/GaussStep);

    // Do the interpolation
    return (1.0-remainder)*tabErf[index] +
      remainder*tabErf[index+1];

  } else {

    return erf(x/SQRT2);

  }

}


Real lorentzFraction(const Real deltasigma, const bool qspeedy)
{

  // Function to return the integral of a Lorentzian(0,1) distribution from 
  // -deltasigma to +deltasigma. There is an additional normalization factor
  // which depends on the line energy and width of 1/(pi/2 - arctan(-2*E0/W))
  // and should be applied by the routine calling lorentzFraction.
  // If qspeedy=true interpolates on a previously calculated grid of calculations
  // while if qspeedy=false then does calculation each time.

  static const size_t LorentzMaxStep(1200);
  static const size_t LorentzMax(6);
  static const Real LorentzStep = (Real)LorentzMax / LorentzMaxStep;

  static RealArray tabLor(LorentzMaxStep+1);
  static bool first(true);

  // if the first time through then calculate the grid on which we
  // will interpolate

  if ( qspeedy && first ) {
    for (size_t i=0; i<=LorentzMaxStep; i++) {
      tabLor[i] = 2*atan(2*i*LorentzStep);
    }
    first = false;
  }
  
  // how many sigmas away we are
  Real x = fabs( deltasigma );

  if ( qspeedy ) {

    // Now interpolate from the table
    size_t index = (size_t)(x/LorentzStep);

    // If we're past the edge of the tabulated data return 1.0.
    if ( index >= LorentzMaxStep ) return 1.0;

    Real remainder = (x - (Real)index*LorentzStep) * (1.0/LorentzStep);

    // Do the interpolation
    return 2*(1.0-remainder)*tabLor[index] +
      remainder*tabLor[index+1];

  } else {

    return 2*atan(2*x);

  }

}

Real voigtFraction(const Real energy, const Real ecenter, const Real sigma, 
		   const Real gamma, const bool qspeedy)
{
  // Function to return the integral of a Voigt(ecenter,sigma,gamma) distribution 
  // from -energy to +energy.
  // If qspeedy=true interpolates on a previously calculated grid of calculations
  // while if qspeedy=false then does calculation each time.

  // for now use a pseudo-Voigt approximation (good to 1%) which is just a sum of a 
  // Gaussian and a Lorentzian. Reference is Ida, T, Ando, M and Toraya, H (2000), 
  // "Extended pseudo-Voigt function for approximating the Voigt profile",
  // Journal of Applied Crystallography 33 (6): 1311–1316.

  Real gaussValue, lorentzValue;
  if ( sigma > 0.0 ) gaussValue = gaussFraction(fabs(energy-ecenter)/sigma, qspeedy);
  if ( gamma > 0.0 ) lorentzValue = lorentzFraction(fabs(energy-ecenter)/gamma, qspeedy);

  if ( sigma == 0.0 ) return lorentzValue;
  if ( gamma == 0.0 ) return gaussValue;

  // 2sqrt(2ln(2)) = 2.35482

  Real fG = 2.35482 * sigma;
  Real fL = 2.0 * gamma;

  Real f = fG*fG*fG*fG*fG + 2.69269*fG*fG*fG*fG*fL + 2.42843*fG*fG*fG*fL*fL +
    4.47163*fG*fG*fL*fL*fL + 0.07842*fG*fL*fL*fL*fL + fL*fL*fL*fL*fL;
  f = pow(f,0.2);

  Real fLof = fL/f;
  Real eta = 1.36603*fLof - 0.47719*fLof*fLof + 0.11116*fLof*fLof*fLof;

  return eta*lorentzValue + (1.0-eta)*gaussValue;

}
