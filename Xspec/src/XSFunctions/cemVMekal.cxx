#include <stlToCArrays.h>
#include <xsFortran.h>
#include <functionMap.h>
#include <XSUtil/Utils/XSutility.h>
#include <xsTypes.h>
#include <cmath>

void cxxsumdem(int itype, int swtch, const RealArray& energyArray, const RealArray& abundances, Real density, Real redshift, const RealArray& Tarray, const RealArray& demarray, int spectrumNumber, bool qtherm, Real velocity, RealArray& flux, RealArray& fluxErr, int& status);

void cemVMekal(const RealArray& energyArray, const RealArray& params,
        int spectrumNumber, RealArray& flux, RealArray& fluxErr, 
        const string& initString)
{
   /*  
     c
     c XSPEC model subroutine to calculate Continous Emission
     c Measure of the form :
     c   Q(T) = Norm*(T/Tmax)^alpha  
     c See, for example, Schmitt et al. ApJ 365, 704 (1990), 
     c but note that this program yields Schmitt's alpha - 1.0.
     c
     c This program calls 'vmeka' thus allowing one to vary the elemental
     c abundances.
     c 
     c Parameters:
     c    param(1) = slope of CEM, alpha
     c    param(2) = maximum temperature, tmax
     c    param(3) = nH (cm^-3)  Fixed at 1 for most applications
     c    param(4) = He abundance
     c    param(5) = C   "
     c    param(6) = N   "
     c    param(7) = O   "
     c    param(8) = Ne  "
     c    param(9) = Na  "
     c    param(10)= Mg  "
     c    param(11)= Al  "
     c    param(12)= Si  "
     c    param(13)= S   "
     c    param(14)= Ar  "
     c    param(15)= Ca  "
     c    param(16)= Fe  "
     c    param(17)= Ni  " 
     c    param(18) = redshift used in the Mewe-Kaastra plasma model
     c    param(19) = switch(0=calculate MEKAL model, 1=interpolate MEKAL model)
     c
     c K. P. Singh    April 22, 1994
     c
     c Disclaimer: Any resemblance to a real program is purely
     c             coincidental
     c
       Translated from Fortran cevmkl.f by C. Gordon   Dec. 2007
   */

   using namespace XSutility;

   const Real k = 8.6171e-8;
   const Real alpha = params[0];
   const Real tMax = params[1];
   const Real max = log10(tMax/k);
   int nt = static_cast<int>((max - 5.5)*10.0 + 1.0);

   RealArray abundances(14);
   for (size_t i=0; i<abundances.size(); i++) abundances[i] = params[i+3];

   RealArray Tarray(nt);
   RealArray demarray(nt);

   /*
      c Integrate contributions in form:
      c    f = (sum over i) Wi * Fi * logdeltT
      c where
      c   Wi= (Ti/Tmax)^alpha and Fi is F(Ti) from meka:
      c
      c it is important to do this in uniform Log(T) steps.
      c
      c I am shuffling the definitions of Fi and photar
      c temporarily for ease.
      c
      c Note: Doing the stepping from logT=log(Tmax) in nt 0.1 steps.
      c       This suggestion from Chris Done to ensure that changing
      c       Tmax by less than 0.1 gives a change in chi-squared.
   */

   for (int i=nt-1; i>=0; --i)
   {
      Real logTemp = max - 0.1*i;
      Tarray[i] = k*pow(10.0, logTemp);
      demarray[i] = 0.1*pow(Tarray[i]/tMax, alpha);      
   }

   int swtch = static_cast<int>(params[18]); 
   const int itype=2;
   const bool qtherm = false;
   const Real velocity = 0.0;
   const Real density = params[2];
   const Real redshift = params[17];
   int status=0;   

   cxxsumdem(itype, swtch, energyArray, abundances, density, redshift, Tarray, 
	     demarray, spectrumNumber, qtherm, velocity, flux, fluxErr, status);

   if (status != 0)
   {
      char msg[] = "***Error returned from SUMDEM called from cevmkl";
      xs_write(msg, 10);
   }
}

/* C++ wrapper for the Fortran sumdem routine */

void sumdem(int itype, int swtch, float* ear, int ne, float* abun,
            float dens, float z, int ninputt, float* inputt, float* dem,
            int ifl, bool qtherm, float velocity, float* photar, 
	    float* photer, int* status);

void cxxsumdem(int itype, int swtch, const RealArray& energyArray, const RealArray& abundances, Real density, Real redshift, const RealArray& Tarray, const RealArray& demarray, int spectrumNumber, bool qtherm, Real velocity, RealArray& flux, RealArray& fluxErr, int& status)
{
   using namespace XSutility;

   float *ear=0, *abund=0, *photar=0, *photer=0;
   XSFunctions::stlToFloatArrays<float>(energyArray, abundances, flux, fluxErr,
        ear, abund, photar, photer);
   auto_array_ptr<float> apEar(ear);
   auto_array_ptr<float> apPars(abund);
   auto_array_ptr<float> apPhotar(photar);
   auto_array_ptr<float> apPhoter(photer);

   int ne = energyArray.size()-1;
   float dens = (float)density;
   float z = (float)redshift;
   float vel = (float)velocity;

   int nt = Tarray.size();

   auto_array_ptr<float> apTarr(new float[nt]);
   auto_array_ptr<float> apDem(new float[nt]);
   float* tarr = apTarr.get();
   float* dem = apDem.get();
   for (int i=0; i<nt; i++) tarr[i] = Tarray[i];
   for (int i=0; i<nt; i++) dem[i] = demarray[i];

   sumdem(itype, swtch, ear, ne, abund, dens, z, nt, 
          tarr, dem, spectrumNumber, qtherm, vel, photar, photer,
	  &status);

   XSFunctions::floatFluxToStl<float>(photar, photer, ne, false, flux, fluxErr);

   return;

}
