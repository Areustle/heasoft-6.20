#include <stlToCArrays.h>
#include <xsFortran.h>
#include <functionMap.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Numerics/Numerics.h>
#include <xsTypes.h>
#include <cmath>

void cxxsumdem(int itype, int swtch, const RealArray& energyArray, const RealArray& abundances, Real density, Real redshift, const RealArray& Tarray, const RealArray& demarray, int spectrumNumber, bool qtherm, Real velocity, RealArray& flux, RealArray& fluxErr, int& status);

// XSPEC model subroutine to calculate collisional plasma with a gaussian DEM
// 
// Parameters:
//    param(1) = Temperature mean
//    param(2) = Temperature sigma
//    param(3) = nH (cm^-3)  Fixed at 1 for most applications
//    param(4) = He abundance
//    param(5) = C   "
//    param(6) = N   "
//    param(7) = O   "
//    param(8) = Ne  "
//    param(9) = Na  "
//    param(10)= Mg  "
//    param(11)= Al  "
//    param(12)= Si  "
//    param(13)= S   "
//    param(14)= Ar  "
//    param(15)= Ca  "
//    param(16)= Fe  "
//    param(17)= Ni  " 
//    param(18)= redshift
//    param(19) = switch(0=calculate MEKAL model, 1=interpolate MEKAL model,
//                       2=AtomDB model)


void vgaussDem(const RealArray& energyArray, const RealArray& params,
	       int spectrumNumber, RealArray& flux, RealArray& fluxErr, 
	       const string& initString)
{


   using namespace XSutility;
   using namespace Numerics;

   const Real Tmean = params[0];
   const Real Tsigma = params[1];

   // *******************************************************************
   // set up arrays of temperature and DEM values
   // use nT temperature steps running from -nSig sigma to +nSig sigma

   int nT = 21;
   int nSig = 3.0;

   RealArray Tarray(nT);
   RealArray demarray(nT);

   Real Tmin = Tmean - nSig*Tsigma;
   if ( Tmin < 0.0 ) Tmin = 0.0;
   Real Tdelta = 2 * nSig * Tsigma / nT;

   for (int i=0; i<nT; i++) {
     Real T1 = Tmin + Tdelta * i;
     Real T2 = T1 + Tdelta;
     Tarray[i] = 0.5 * (T1 + T2);
     demarray[i] = erf((T2-Tmean)/Tsigma) - erf((T1-Tmean)/Tsigma);
   }

   // end of set up arrays of temperature and DEM values
   // *******************************************************************


   // set up all the variables to pass to sumdem

   int swtch = static_cast<int>(params[18]); 
   const int itype=2;
   const bool qtherm = false;
   const Real velocity = 0.0;
   const Real density = params[2];
   const Real redshift = params[17];
   RealArray abundances(14);
   for (size_t i=0; i<abundances.size(); i++) abundances[i] = params[i+3];

   int status=0;   

   cxxsumdem(itype, swtch, energyArray, abundances, density, redshift, Tarray, 
	     demarray, spectrumNumber, qtherm, velocity, flux, fluxErr, status);

   if (status != 0)
   {
      char msg[] = "***Error returned from SUMDEM called from vgaussDem";
      xs_write(msg, 10);
   }
}
