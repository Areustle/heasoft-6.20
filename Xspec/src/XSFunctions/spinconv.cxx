#include <xsTypes.h>
#include <functionMap.h>
#include <stlToCArrays.h>
#include <XSUtil/Utils/XSutility.h>
#include <cfortran.h>

// Functions called FROM here to Fortran:

PROTOCCALLSFSUB6(DOSPINCONV,dospinconv,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)
#define DOSPINCONV(ear,ne,param,ifl,photar,tmp) \
           CCALLSFSUB6(DOSPINCONV,dospinconv,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
             ear,ne,param,ifl,photar,tmp)



void spinconv(const RealArray& energyArray, const RealArray& params,
        int spectrumNumber, RealArray& flux, RealArray& fluxErr, 
        const string& initString)
{
   // Wrapper to handle dynamic memory allocation.  Actual work
   // is done in dospinconv

   using namespace XSutility;

   int ne = static_cast<int>(energyArray.size()) - 1;
   auto_array_ptr<float> apTmp(new float[ne]);

   float *ear=0, *pars=0, *photar=0, *photer=0;
   XSFunctions::stlToFloatArrays<float>(energyArray, params, flux, fluxErr,
           ear, pars, photar, photer);
   auto_array_ptr<float> apEar(ear);
   auto_array_ptr<float> apPars(pars);
   auto_array_ptr<float> apPhotar(photar);
   auto_array_ptr<float> apPhoter(photer);

   DOSPINCONV(ear, ne, pars, spectrumNumber, photar, apTmp.get());

   XSFunctions::floatFluxToStl<float>(photar, photer, ne, false, flux, fluxErr);       
}
