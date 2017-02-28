#include <functionMap.h>
#include <xsTypes.h>
#include <stlToCArrays.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSUtil/FunctionUtils/xsFortran.h>
#include <XSUtil/Utils/XSutility.h>
#include <sstream>
#include <cfortran.h>

PROTOCCALLSFSUB20(XSVCFL,xsvcfl,FLOATV,INT,FLOAT,FLOAT,FLOAT,FLOATV,FLOAT,FLOAT,INT,INT,INT,INT,INT,INT,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV)
#define XSVCFL(ear,ne,tlow,thigh,slope,abun,dens,z,itype,switch,ifl,cflver,nsteps,nmtval,bolo,ttab,tval,dem,photar,photer) \
  CCALLSFSUB20(XSVCFL,xsvcfl,FLOATV,INT,FLOAT,FLOAT,FLOAT,FLOATV,FLOAT,FLOAT,INT,INT,INT,INT,INT,INT,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV, \
	       ear,ne,tlow,thigh,slope,abun,dens,z,itype,switch,ifl,cflver,nsteps,nmtval,bolo,ttab,tval,dem,photar,photer)

void pfltmp(int itype, float** tval, int* nmtval, int* status);
void pflbol(int itype, const float* abun, float* bolo, int* status);

void xsvmcf(const RealArray& energyArray, const RealArray& params,
        int spectrumNumber, RealArray& flux, RealArray& fluxErr, 
        const string& initString)
{

   //  XSPEC model subroutine to calculate cooling flow spectrum
   //  from sum of MEKAL spectra. Variable abundances.
   //  Parameters :
   //        1..................low temperature
   //        2..................high temperature
   //        3..................He abundance
   //        4..................C     "
   //        5..................N     "
   //        6..................O     "
   //        7..................Ne    "
   //        8..................Na    "
   //        9..................Mg    "
   //       10..................Al    "
   //       11..................Si    "
   //       12..................S     "
   //       13..................Ar    "
   //       14..................Ca    "
   //       15..................Fe    "
   //       16..................Ni    "
   //       17..................redshift
   //       18..................switch(0=calculate MEKAL model, 
   //                                  1=interpolate MEKAL model)

   //  Norm is mass accretion rate in units of Msun/yr

   //  kaa 8/3/93      based on XSCFLW.
   //      10/4/96     added in slow and fast options

   //  cg    2/6/09    Front end and dynamic memory allocation translated
   //                  to C++.

   using namespace XSutility;
   const size_t nE = energyArray.size()-1;
   flux.resize(nE);
   fluxErr.resize(0);


   // On initialization find the tabulated temperatures and calculate the
   // bolometric luminosities for each temperature.

   static bool isFirst = true;
   static float *ttab=0; // This memory is owned in ldpfil
   static int nmtval=0;
   static auto_array_ptr<float> apBolo(0);
   int status = 0;   
   int itype = 2;
   if (isFirst)
   {
      pfltmp(itype, &ttab, &nmtval, &status);
      if (status)
         return;      
      float *oldF = apBolo.reset(new float[nmtval]);
      delete [] oldF;
   }

   // Calculate the bolometric luminosities
   float *ear=0, *pars=0, *photar=0, *photer=0;
   XSFunctions::stlToFloatArrays<float>(energyArray, params, flux, fluxErr,
        ear, pars, photar, photer);
   auto_array_ptr<float> apEar(ear);
   auto_array_ptr<float> apPars(pars);
   auto_array_ptr<float> apPhotar(photar);
   auto_array_ptr<float> apPhoter(photer);
   pflbol(itype, pars+2, apBolo.get(), &status);
   if (status)
      return;

   // Check for a version number

   int cflver = 2;
   const string& verStr(FunctionUtility::getModelString("CFLOW_VERSION"));
   if (verStr.length() && verStr != FunctionUtility::NOT_A_KEY())
   {
      std::istringstream iss(verStr);
      int test=0;
      if (!(iss >> test) || !iss.eof())
      {
         std::ostringstream err;
         err << "Invalid CFLOW_VERSION value: " << verStr
             <<"\n will use version = " << cflver;
         xs_write(const_cast<char*>(err.str().c_str()), 10);
      }
      else
      {
         cflver = test;
      }
   }
   std::ostringstream oss;
   oss << "Cooling flow model version " << cflver;
   xs_write(const_cast<char*>(oss.str().c_str()), 25);

   // Get the number of temperature steps. For the new version use 
   // either 10 or the value set by CFLOW_NTEMPS

   int nsteps = 0;
   if (cflver == 1)
      nsteps = nmtval;
   else if (cflver == 2)
   {
      nsteps = 10;
      const string& nTempsStr(FunctionUtility::getModelString("CFLOW_NTEMPS"));
      if (nTempsStr.length() && nTempsStr != FunctionUtility::NOT_A_KEY())
      {
         std::istringstream iss(nTempsStr);
         int test=0;
         if (!(iss >> test) || !iss.eof())
         {
            std::ostringstream err;
            err << "Invalid CFLOW_NTEMPS value: " << nTempsStr
                <<"\n will use ntemps = " << nsteps;
            xs_write(const_cast<char*>(err.str().c_str()), 10);
         }
         else
            nsteps = test;
      }
   }
   else
   {
      std::ostringstream err;
      err << "Invalid CFLOW version number: " << cflver;
      xs_write(const_cast<char*>(err.str().c_str()), 10);
      return;
   }

   // Get the memory for the temperature and DEM arrays

   auto_array_ptr<float> apTval(new float[nsteps]);
   auto_array_ptr<float> apDem(new float[nsteps]);

   int ne = static_cast<const int>(nE);

   // Call the routine to calculate the CF
   int swtch = static_cast<int>(pars[17]);
   XSVCFL(ear, ne, pars[0], pars[1], 0.0, pars+2, 1.0, pars[16], itype, swtch, 
        spectrumNumber, cflver, nsteps, nmtval, apBolo.get(), ttab, apTval.get(), 
	  apDem.get(), photar, photer);

   XSFunctions::floatFluxToStl<float>(photar, photer, nE, false, flux, fluxErr);    

   isFirst = false;
}