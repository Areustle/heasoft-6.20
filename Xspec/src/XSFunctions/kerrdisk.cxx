#include <xsTypes.h>
#include <stlToCArrays.h>
#include <functionMap.h>
#include <XSUtil/Utils/XSutility.h>

void rdlaor(int* nRad, float** rad, int* nteta, float** incl, int* nenrgy,
                float** ebin, float** trs);

extern "C" void dokdisk_(float* ear, int& ne, float* param, int& nenrgy, int& nrad,
                int& nteta, float* efil, float* ebin, float* rad, float* incl, 
                float* trs, float* bin, float* start, float* end, float* fstart, 
                float* fend, float* photloc, float* photlocinner, 
                float* inner, float* outer, float* photar);

void kerrdisk(const RealArray& energyArray, const RealArray& params,
        int spectrumNumber, RealArray& flux, RealArray& fluxErr, 
        const string& initString)
{
   using namespace XSutility;

//    1. - distance (kpc)
//    2. - spectral hardening factor (Tcol/Teff)
//    3. - mass of the central object (solar mass unit)
//    4. - mass accretion rate in 1e18 erg/s
//    5. - disk inclinatino angle (degree, 0 for face-on)
//    6. - disk inner radius in unit of GM/c^2 (>1.235)
//    7. - disk outer radius 

   static bool isFirst = true;
   static int nrad=0;
   static int nteta=0;
   static int nenrgy=0;
   static int nESave=0;
   static auto_array_ptr<float> apRad(0);
   static auto_array_ptr<float> apIncl(0);
   static auto_array_ptr<float> apEbin(0);
   static auto_array_ptr<float> apTrs(0);
   static auto_array_ptr<float> apEfil(0);
   static auto_array_ptr<float> apBin(0);
   static auto_array_ptr<float> apInner(0);

   int nE = static_cast<int>(energyArray.size()) - 1;
   if (isFirst)
   {
      float *rad=0, *incl=0, *ebin=0, *trs=0;
      try
      {
         rdlaor(&nrad, &rad, &nteta, &incl, &nenrgy, &ebin, &trs);
      }
      catch (...)
      {
         return;
      }
      apRad.reset(rad);
      apIncl.reset(incl);
      apEbin.reset(ebin);
      apTrs.reset(trs);

      // Get the memory for the efilpt array
      apEfil.reset(new float[nenrgy+3]);
      // Get the memory for the bin array
      apBin.reset(new float[nenrgy+2]);
      // Get memory for work arrays
      apInner.reset(new float[nenrgy+2]);      
   }

   static auto_array_ptr<float> apPhotl(0);
   static auto_array_ptr<float> apInphtl(0);
   static auto_array_ptr<float> apOuter(0);
   static auto_array_ptr<float> apStart(0);
   static auto_array_ptr<float> apEnd(0);
   static auto_array_ptr<float> apFstart(0);
   static auto_array_ptr<float> apFend(0);
   if (isFirst || nE != nESave)
   {
      float *old = apPhotl.reset(new float[nE]);
      delete [] old;
      old = apInphtl.reset(new float[nE]);
      delete [] old;
      old = apOuter.reset(new float[nE]);
      delete [] old;
      old = apStart.reset(new float[nE]);
      delete [] old;
      old = apEnd.reset(new float[nE]);
      delete [] old;
      old = apFstart.reset(new float[nE]);
      delete [] old;
      old = apFend.reset(new float[nE]);
      delete [] old;
      nESave = nE;
   }

   float *ear=0, *pars=0, *photar=0, *photer=0;
   XSFunctions::stlToFloatArrays<float>(energyArray, params, flux, fluxErr,
        ear, pars, photar, photer);
   auto_array_ptr<float> apEar(ear);
   auto_array_ptr<float> apPars(pars);
   auto_array_ptr<float> apPhotar(photar);
   auto_array_ptr<float> apPhoter(photer);   

   dokdisk_(ear, nE, pars, nenrgy, nrad, nteta, apEfil.get(), apEbin.get(), 
          apRad.get(), apIncl.get(), apTrs.get(), apBin.get(), apStart.get(), 
          apEnd.get(), apFstart.get(), apFend.get(), apPhotl.get(),
          apInphtl.get(), apInner.get(), apOuter.get(), photar);
   XSFunctions::floatFluxToStl<float>(photar, photer, nE, false, flux, fluxErr);

   isFirst = false;
}
