#include <xsTypes.h>
#include <stlToCArrays.h>
#include <functionMap.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSUtil/FunctionUtils/xsFortran.h>
#include <XSUtil/Utils/XSutility.h>
#include <fitsio.h>
#include <sstream>
#include <cfortran.h>

PROTOCCALLSFSUB17(LAORSB2,laorsb2,FLOATV,INT,FLOATV,INT,INT,INT,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV)

#define LAORSB2(ear,ne,param,nenrgy,nrad,nteta,efil,ebin,rad,incl,itrs,bin,start,end,fstart,fend,photar) \
           CCALLSFSUB17(LAORSB2,laorsb2,FLOATV,INT,FLOATV,INT,INT,INT,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV,FLOATV, \
           ear,ne,param,nenrgy,nrad,nteta,efil,ebin,rad,incl,itrs,bin,start,end,fstart,fend,photar)

namespace {
   void errReturn(fitsfile* pFile, const string& context, int status);
}

void laor2(const RealArray& energyArray, const RealArray& params,
        int spectrumNumber, RealArray& flux, RealArray& fluxErr, 
        const string& initString)
{

   // ACF version of laor model with broken power law of emissivity


   // This subroutine calculates line profiles from an accretion disk around
   // a Kerr BH.

   // Parameters :
   //    1:    Rest frame energy of line
   //    2:    Power-law dependence of emission
   //    3:    Inner radius of disk (in gravl radii)  >= 1.235
   //    4:    Outer radius of disk (in gravl radii)  =< 400
   //    5:    Inclination angle of disk (degrees, 0=face on)
   //    6:    Break radius
   //    7:    outer power-law dependence

   // The transfer function is read from the file ari.bin
   // The Radii are in the range 1.23 to 400, inclinations are from 0 to 1.
   // There are 31 tabulated inclinations in steps of 0.033 from 0 to 1.

   // kaa 1/17/95 modified to use FITS file.
   //  kaa 9/6/96  added dynamic memory

   // Tabulated data:
   //    nrad         Number of radii
   //    nteta        Number of inclinations
   //    NENRGY       Number of energies
   //    radpt        Radii (in gravl radii)
   //    inclpt       cos(inclination)
   //    ebin         Energies (units of rest energy of line)
   //    trspt        Transfer function

   // Translated from laor2.f Jan 2009 (CG)

   using namespace XSutility;

   static auto_array_ptr<float> apRad(0);
   static auto_array_ptr<float> apIncl(0);
   static auto_array_ptr<float> apEbin(0);
   static auto_array_ptr<float> apEfil(0);
   static auto_array_ptr<float> apTrs(0);
   static auto_array_ptr<float> apBin(0);

   static bool isFirst = true;
   static int nrad=0;
   static int nenergy=0;
   static int nteta=0;

   // If the first time round then read in the data 

   if (isFirst)
   {
      string laorFile(FunctionUtility::modelDataPath());
      const string fName("ari.mod");
      laorFile += "ari.mod";
      fitsfile *pFile=0;
      int status=0;
      if (fits_open_file(&pFile, const_cast<char*>(laorFile.c_str()), 0, &status))
      {
         std::ostringstream err;
         err << "Failed to open " << laorFile << "\nFITSIO error = "
            << status << " in LAOR2\n";
         xs_write(const_cast<char*>(err.str().c_str()), 10);
         return;
      }

      // Go to the second extension to get the radii

      if (fits_movabs_hdu(pFile, 2, 0, &status))
      {
         string context("Failed to open second extension in " + fName);
         errReturn(pFile, context, status);
         return;
      }

      long lnrad=0;
      if (fits_read_key_lng(pFile, "NAXIS2", &lnrad, 0, &status))
      {
         string context("Failed to get size of RADIUS data");
         errReturn(pFile, context, status);
         return;
      }

      int anynul=0;
      float* oldF = apRad.reset(new float[lnrad]);
      delete [] oldF;
      if (fits_read_col_flt(pFile, 1, 1, 1, lnrad, 0., apRad.get(),
                &anynul, &status))
      {
         string context("Failed to read RADIUS data");
         errReturn(pFile, context, status);
         return;
      }

      // Go to the third extension to get the angles

      if (fits_movabs_hdu(pFile, 3, 0, &status))
      {
         string context("Failed to open third extension in " + fName);
         errReturn(pFile, context, status);
         return;
      }

      long lnteta=0; 
      if (fits_read_key_lng(pFile, "NAXIS2", &lnteta, 0, &status))
      {
         string context("Failed to get size of ANGLE data");
         errReturn(pFile, context, status);
         return;
      }

      oldF = apIncl.reset(new float[lnteta]);
      delete [] oldF;
      if (fits_read_col_flt(pFile, 1, 1, 1, lnteta, 0., apIncl.get(),
                &anynul, &status))
      {
         string context("Failed to read ANGLE data");
         errReturn(pFile, context, status);
         return;
      }

      // Go to the fourth extension to get the energies

      if (fits_movabs_hdu(pFile, 4, 0, &status))
      {
         string context("Failed to open fourth extension in " + fName);
         errReturn(pFile, context, status);
         return;
      }

      long lnenergy=0;
      if (fits_read_key_lng(pFile, "NAXIS2", &lnenergy, 0, &status))
      {
         string context("Failed to get size of ENERGY data");
         errReturn(pFile, context, status);
         return;
      }

      oldF = apEbin.reset(new float[lnenergy]);
      delete [] oldF;
      if (fits_read_col_flt(pFile, 1, 1, 1, lnenergy, 0., apEbin.get(),
                &anynul, &status))
      {
         string context("Failed to read ENERGY data");
         errReturn(pFile, context, status);
         return;
      }

      // Get the memory for the efilpt array
      oldF = apEfil.reset(new float[lnenergy+3]);
      delete [] oldF;

      // Go to the fifth extension to read the transfer function

      if (fits_movabs_hdu(pFile, 5, 0, &status))
      {
         string context("Failed to open fifth extension in " + fName);
         errReturn(pFile, context, status);
         return;
      }

      oldF = apTrs.reset(new float[lnrad*lnteta*lnenergy]);
      delete [] oldF;
      if (fits_read_col_flt(pFile, 1, 1, 1, lnrad*lnteta*lnenergy, 0., 
                apTrs.get(), &anynul, &status))
      {
         string context("Failed to read transfer function");
         errReturn(pFile, context, status);
         return;
      }

      fits_close_file(pFile, &status);

      oldF = apBin.reset(new float[lnenergy+2]);
      delete [] oldF;

      nrad = static_cast<int>(lnrad);
      nteta = static_cast<int>(lnteta);
      nenergy = static_cast<int>(lnenergy);

   } // end if isFirst

   // allocate memory for inibin/erebin work arrays
   int nE = static_cast<int>(energyArray.size()) - 1;
   auto_array_ptr<float> apStart(new float[nE]);
   auto_array_ptr<float> apEnd(new float[nE]);
   auto_array_ptr<float> apFstart(new float[nE]);
   auto_array_ptr<float> apFend(new float[nE]);

   // call the subroutine to do the actual calculation

   float *ear=0, *pars=0, *photar=0, *photer=0;
   XSFunctions::stlToFloatArrays<float>(energyArray, params, flux, fluxErr,
           ear, pars, photar, photer);
   auto_array_ptr<float> apEar(ear);
   auto_array_ptr<float> apPars(pars);
   auto_array_ptr<float> apPhotar(photar);
   auto_array_ptr<float> apPhoter(photer);

   LAORSB2(ear, nE, pars, nenergy, nrad, nteta, apEfil.get(), apEbin.get(),
        apRad.get(), apIncl.get(), apTrs.get(), apBin.get(), apStart.get(),
        apEnd.get(), apFstart.get(), apFend.get(), photar);

   XSFunctions::floatFluxToStl<float>(photar, photer, nE, false, flux, fluxErr);       

   isFirst = false;      
}

namespace {
   void errReturn(fitsfile* pFile, const string& context, int status)
   {
      std::ostringstream oss;
      oss << context << "\nError in LAOR2 : status = " << status;
      string fullMsg(oss.str());
      xs_write(const_cast<char*>(fullMsg.c_str()), 10);
      int status2=0;
      fits_close_file(pFile, &status2);
   }
}
