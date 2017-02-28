#include <xsTypes.h>
#include <stlToCArrays.h>
#include <functionMap.h>
#include <XSUtil/Utils/XSutility.h>

extern "C" void kdblur2i_(float* ear, int& nE, float* pars, int& spectrumNumber,
                float* photar, float* workArray);

void kdblur2(const RealArray& energyArray, const RealArray& params,
        int spectrumNumber, RealArray& flux, RealArray& fluxErr, 
        const string& initString)
{
   using namespace XSutility;

   int nE = static_cast<int>(energyArray.size()) - 1;
   auto_array_ptr<float> apWork(new float[nE]);
   float* workArray = apWork.get();
   for (int i=0; i<nE; ++i)
      workArray[i] = 0.0;

   float *ear=0, *pars=0, *photar=0, *photer=0;
   XSFunctions::stlToFloatArrays(energyArray, params, flux, fluxErr,
           ear, pars, photar, photer);
   auto_array_ptr<float> apEar(ear);
   auto_array_ptr<float> apPars(pars);
   auto_array_ptr<float> apPhotar(photar);
   auto_array_ptr<float> apPhoter(photer);

   kdblur2i_(ear, nE, pars, spectrumNumber, photar, workArray);
   XSFunctions::floatFluxToStl<float>(photar, photer, nE, false, flux, fluxErr);

}
