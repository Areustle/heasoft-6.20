#include <xsTypes.h>
#include <stlToCArrays.h>
#include <functionMap.h>
#include <FunctionUtility.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSutility.h>
#include <fitsio.h>
#include <sstream>

void rdlaor(int* nRad, float** rad, int* nteta, float** incl, int* nenrgy,
                float** ebin, float** trs);

extern "C" void laorsb_(float* ear, int& ne, float* param, int& nenrgy, int& nrad, 
             int& nteta, float* efil, float* ebin, float* rad, float* incl,
             float* trs, float* bin, float* start, float* end, float* fstart,
             float* fend, float* photar);

void xslaor(const RealArray& energyArray, const RealArray& params,
        int spectrumNumber, RealArray& flux, RealArray& fluxErr, 
        const string& initString)
{
   using namespace XSutility;

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
   }

   static auto_array_ptr<float> apStart(0);
   static auto_array_ptr<float> apEnd(0);
   static auto_array_ptr<float> apFstart(0);
   static auto_array_ptr<float> apFend(0);
   if (isFirst || nE != nESave)
   {
      float *old = apStart.reset(new float[nE]);
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

   laorsb_(ear,nE,pars,nenrgy,nrad,nteta,apEfil.get(),apEbin.get(),apRad.get(),
        apIncl.get(),apTrs.get(),apBin.get(),apStart.get(),apEnd.get(),
        apFstart.get(),apFend.get(),photar);
   isFirst = false;
   XSFunctions::floatFluxToStl<float>(photar, photer, nE, false, flux, fluxErr);
} 
// ******* end xslaor *********

void rdlaor(int* nrad, float** rad, int* nteta, float** incl, int* nenrgy,
                float** ebin, float** trs)
{
   using namespace XSutility;

   int status=0;
   string modFileName(FunctionUtility::modelDataPath());
   modFileName += "ari.mod";

   // It's OK to do this as long as NOTHING actually modifies
   // the file name string.
   char *cFileName = const_cast<char*>(modFileName.c_str());

   std::ostringstream errMsg;
   errMsg << "In RDLAOR, FITSIO error = ";
   fitsfile *fptr=0;

   // Open the ari.mod file
   fits_open_file(&fptr,cFileName, 0, &status);
   if (status)
   {
      errMsg << status << "\nFailed to open " << modFileName << "\n";
      throw YellowAlert(errMsg.str());
   }

   auto_array_ptr<float> apRad(0);
   auto_array_ptr<float> apIncl(0);
   auto_array_ptr<float> apEbin(0);
   auto_array_ptr<float> apTrs(0);
   try
   {   
      // Go to the second extension to get the radii
      fits_movabs_hdu(fptr, 2, 0, &status);
      if (status)
      {
         errMsg << status << "\nFailed to open 2nd extension in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }
      fits_read_key(fptr, TINT, "NAXIS2", nrad, 0, &status);
      if (status)
      {
         errMsg << status << "\nFailed to get size of RADIUS data in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }
      apRad.reset(new float[*nrad]);
      fits_read_col(fptr, TFLOAT, 1, 1, 1, *nrad, 0, apRad.get(), 0,&status);
      if (status)
      {
         errMsg << status << "\nFailed read RADIUS data in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }

      // Go to the third extension to get the angles
      fits_movabs_hdu(fptr, 3, 0, &status);
      if (status)
      {
         errMsg << status << "\nFailed to open 3nd extension in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }
      fits_read_key(fptr, TINT, "NAXIS2", nteta, 0, &status);
      if (status)
      {
         errMsg << status << "\nFailed to get size of ANGLE data in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }
      apIncl.reset(new float[*nteta]);
      fits_read_col(fptr, TFLOAT, 1, 1, 1, *nteta, 0, apIncl.get(), 0,&status);
      if (status)
      {
         errMsg << status << "\nFailed read ANGLE data in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }

      // Go to the fourth extension to get the energies
      fits_movabs_hdu(fptr, 4, 0, &status);
      if (status)
      {
         errMsg << status << "\nFailed to open 4th extension in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }
      fits_read_key(fptr, TINT, "NAXIS2", nenrgy, 0, &status);
      if (status)
      {
         errMsg << status << "\nFailed to get size of ENERGY data in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }
      apEbin.reset(new float[*nenrgy]);
      fits_read_col(fptr, TFLOAT, 1, 1, 1, *nenrgy, 0, apEbin.get(), 0,&status);
      if (status)
      {
         errMsg << status << "\nFailed read ENERGY data in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }

      // Go to the fifth extension to read the transfer function
      fits_movabs_hdu(fptr, 5, 0, &status);
      if (status)
      {
         errMsg << status << "\nFailed to open 5th extension in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }
      long nElem = static_cast<long>(*nrad)*(*nteta)*(*nenrgy);
      apTrs.reset(new float[nElem]);
      fits_read_col(fptr, TFLOAT, 1, 1, 1, nElem, 0, apTrs.get(), 0,&status);
      if (status)
      {
         errMsg << status << "\nFailed read transfer function in "
            << modFileName << "\n";
         throw YellowAlert(errMsg.str());
      }
   }
   catch (...)
   {
      fits_close_file(fptr,&status);
      throw;
   }
   fits_close_file(fptr,&status);
   *rad = apRad.release();
   *incl = apIncl.release();
   *ebin = apEbin.release();
   *trs = apTrs.release();

}
