#include <xsTypes.h>
#include <stlToCArrays.h>
#include <functionMap.h>
#include <XSUtil/Utils/XSutility.h>

extern "C" void runkbb_(float* ear, int& nE, float* pars, int& spectrumNumber,
                float* photar, float* photer, int& nS, int& nTh, int& nG,
                int& nEner, float* flux0, float* fflux, float* e, float* gI,
                float* aI, float* thetaI, float* fluxE, float* ear0, 
                int* nex1, int* nex2);

void kerrbb(const RealArray& energyArray, const RealArray& params,
        int spectrumNumber, RealArray& flux, RealArray& fluxErr, 
        const string& initString)
{
   using namespace XSutility;

   // Memory allocation wrapper function for Li-Xin Li's runkbb routine.

   int nG = 6, nS = 46, nTh = 18, nEner = 601;
   int nE = static_cast<int>(energyArray.size()) - 1;
   static bool isFirst = true;

   // Grab the memory for the arrays that are read in from the model files

   static auto_array_ptr<float> apFlux0(0), apFflux(0), apE(0), apGI(0);
   static auto_array_ptr<float>  apAI(0), apThetaI(0), apEar0(0);
   if (isFirst)
   {
      apFlux0.reset(new float[nS*nTh*nG*nEner]);
      apFflux.reset(new float[nEner]);
      apE.reset(new float[nEner]);
      apGI.reset(new float[nG]);
      apAI.reset(new float[nS]);
      apThetaI.reset(new float[nTh]);
      apEar0.reset(new float[nEner]);
      isFirst = false;
   }

   // Now the memory for temporary arrays

   auto_array_ptr<float> apFluxE(new float[nE*2]);
   auto_array_ptr<int> apNex1(new int[nE*2]);
   auto_array_ptr<int> apNex2(new int[nE*2]);

   float *ear=0, *pars=0, *photar=0, *photer=0;
   XSFunctions::stlToFloatArrays<float>(energyArray, params, flux, fluxErr,
           ear, pars, photar, photer);
   auto_array_ptr<float> apEar(ear);
   auto_array_ptr<float> apPars(pars);
   auto_array_ptr<float> apPhotar(photar);
   auto_array_ptr<float> apPhoter(photer);

   // Call the main routine

   runkbb_(ear, nE, pars, spectrumNumber, photar, photer, nS, nTh, nG,
        nEner, apFlux0.get(), apFflux.get(), apE.get(), apGI.get(),
        apAI.get(), apThetaI.get(), apFluxE.get(), apEar0.get(),
        apNex1.get(), apNex2.get());
   XSFunctions::floatFluxToStl<float>(photar, photer, nE, false, flux, fluxErr);
}
