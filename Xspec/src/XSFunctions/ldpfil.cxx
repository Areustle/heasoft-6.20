#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSUtil/FunctionUtils/xsFortran.h>
#include <XSUtil/Utils/XSutility.h>
#include <fitsio.h>
#include <sstream>

namespace {
   void errReturn(fitsfile* pFile, const string& context, int status);
}

void ldpfil(int itype, float** tval, int* nmtval, float** efil, int* nin,
                float** tabs, int* ncomp, int* status)
{

   // Subroutine to read the R-S or Mekal tables into memory. Returns the pointers
   // to the arrays and sizes of arrays

   // Parameters :
   //     itype     I         i: type of plasma emission file
   //                            1 = R-S
   //                            2 = Mekal
   //     tval      R         r: Pointer to array of tabulated temperatures
   //     nmtval    I         r: Number of temperatures
   //     efil      R         r: Pointer to array of tabulated energies
   //     nin       I         r: Number of energy ranges
   //     tabs      R         r: Pointer to array of model spectra
   //     ncomp     I         r: Number of spectra for each temperature

   // Translated from ldpfil.f Feb. 2009 (C.Gordon)


   using namespace XSutility;

   // *** IMPORTANT: The code ASSUMES that allowed itypes will be 
   // *** numbered sequentially from 1.  If this should change,
   // *** an indirection array will need to be added for converting
   // *** itypes to array indices.

   if (itype < 1 || itype > 2)
   {
      std::ostringstream err;
      err << "***Error: itype " << itype << " not allowed in LDPFIL."
        << "\n   No plasma emission file loaded.";
      xs_write(const_cast<char*>(err.str().c_str()),10);
      *status = -1;
      return; 
   }
   const int idx = itype-1;
   const int NTYPE=2;

   static auto_array_ptr<float> apTval[NTYPE];
   static auto_array_ptr<float> apEfil[NTYPE];
   static auto_array_ptr<float> apTabs[NTYPE];
   static int snmtval[NTYPE]={0,0};
   static int snin[NTYPE]={0,0};
   static int sncomp[NTYPE]={0,0};

   static bool isFirst[NTYPE] = {true,true};

   *status = 0;

   // if this the first time through then read in the data

   if (isFirst[idx])
   {
      string filenm(FunctionUtility::modelDataPath());
      if (itype == 1)
         filenm += "raysmith.mod";
      else if (itype == 2)
         filenm += "mekal.mod"; 

      // open input FITS file

      fitsfile *pFile=0;
      fits_open_file(&pFile, const_cast<char*>(filenm.c_str()), 0, status);
      if (*status)
      {
         std::ostringstream err;
         err << "Failed to open " << filenm <<"\nFITSIO error " << *status
            << " in LDPFIL";
         xs_write(const_cast<char*>(err.str().c_str()), 10);
         return;
      }

      // go to the second extension to get the number of additional parameters
      // and the tabulated temperatures

      fits_movabs_hdu(pFile, 2, 0, status);
      if (*status)
      {
         string context("Failed to go to second extension");
         errReturn(pFile, context, *status);
         return; 
      }

      // Get the number of additional parameters - ncomp is set to this + 1

      long tmpLong=0;
      fits_read_key_lng(pFile, "NADDPARM", &tmpLong, 0, status);
      if (*status)
      {
         string context("Failed to read NADDPARM keyword in plasma emission file.");
         errReturn(pFile, context, *status);
         return;
      }
      *ncomp = static_cast<int>(tmpLong) + 1;

      // Get the number of tabulated temperatures;

      int icol=0;
      fits_get_colnum(pFile, CASESEN, const_cast<char*>("NUMBVALS"), &icol, status);
      if (*status)
      {
         string context("Failed to find NUMBVALS column.");
         errReturn(pFile, context, *status);
         return;
      }
      int anynul=0;
      fits_read_col_lng(pFile, icol, 1, 1, 1, 0, &tmpLong, &anynul, status);
      if (*status)
      {
         string context("Failed to read the NUMBVALS column.");
         errReturn(pFile, context, *status);
         return;
      }
      *nmtval = static_cast<int>(tmpLong);

      std::ostringstream oss;
      oss << *nmtval << " tabulated temperatures";
      xs_write(const_cast<char*>(oss.str().c_str()), 15);

      // get the memory for the tabulated temperatures

      float* oldF = apTval[idx].reset(new float[*nmtval]);
      delete [] oldF;

      // get the tabulated temperatures

      *tval = apTval[idx].get();
      fits_get_colnum(pFile, CASESEN, const_cast<char*>("VALUE"), &icol, status);
      if (*status)
      {
         string context("Failed to find VALUE column.");
         errReturn(pFile, context, *status);
         return;
      }
      fits_read_col_flt(pFile, icol, 1, 1, *nmtval, 0, *tval, &anynul, status);
      if (*status)
      {
         string context("Failed to read the VALUE column.");
         errReturn(pFile, context, *status);
         return;
      }

      // go to the energies (third) extension

      fits_movabs_hdu(pFile, 3, 0, status);
      if (*status)
      {
         string context("Failed to go to third extension.");
         errReturn(pFile, context, *status);
         return;
      }

      // get the number of energies, note that nin is set to be the 
      // number of bins so we end up with nin+1 energies.

      fits_read_key_lng(pFile, "NAXIS2", &tmpLong, 0, status);
      if (*status)
      {
         string context("Failed to read NAXIS2 keyword.");
         errReturn(pFile, context, *status);
         return;
      }
      *nin = static_cast<int>(tmpLong);

      // get the memory for the tabulated energies

      oldF = apEfil[idx].reset(new float[*nin+1]);
      delete [] oldF;

      // load the energies

      *efil = apEfil[idx].get();
      fits_read_col_flt(pFile, 1, 1, 1, *nin, 0, *efil, &anynul, status);
      if (*status)
      {
         string context("Failed to read the ENERG_LO column.");
         errReturn(pFile, context, *status);
         return;
      }
      fits_read_col_flt(pFile, 2, *nin, 1, 1, 0, *efil+*nin, &anynul, status);
      if (*status)
      {
         string context("Failed to read the ENERG_HI column.");
         errReturn(pFile, context, *status);
         return;
      }

      // move to the fourth extension to read the model spectra

      fits_movabs_hdu(pFile, 4, 0, status);
      if (*status)
      {
         string context("Failed to go to fourth extension.");
         errReturn(pFile, context, *status);
         return;
      }

      // get the memory for the model spectra

      oldF = apTabs[idx].reset(new float[(*nin)*(*ncomp)*(*nmtval)]);
      delete [] oldF;

      // read in the model spectra

      int ioff = 0;
      *tabs = apTabs[idx].get();
      for (int j=1; j<=*nmtval; ++j)
      {
         fits_read_col_flt(pFile, 2, j, 1, *nin, 0, *tabs+ioff, &anynul, status);
         if (*status)
         {
            string context("Failed to read second column in model spectra.");
            errReturn(pFile, context, *status);
            return;
         }
         ioff += *nin;
         for (int i=1; i<*ncomp; ++i)
         {
            fits_read_col_flt(pFile, i+2, j, 1, *nin, 0, *tabs+ioff, &anynul, status);
            if (*status)
            {
               std::ostringstream oss;
               oss <<"Failed to read column " << i+2 << " in model spectra.";
               string context(oss.str());
               errReturn(pFile, context, *status);
               return;
            }
            ioff += *nin;         
         }
      }

      fits_close_file(pFile, status);
      if (*status)
      {
         std::ostringstream oss;
         oss << "Failed to close FITS file: Error in LDPFIL: "
            << "status = " << *status;
         string context(oss.str());
         xs_write(const_cast<char*>(context.c_str()), 10);
         return;
      }

      // save the sizes

      snmtval[idx] = *nmtval;
      snin[idx] = *nin;
      sncomp[idx] = *ncomp;

      isFirst[idx] = false;

   } // end if first time for idx
   else
   {
      // If not the first time into this routine then just return the 
      // pointers and sizes that have been SAVED

      *tval = apTval[idx].get();
      *nmtval = snmtval[idx];
      *efil = apEfil[idx].get();
      *nin = snin[idx];
      *tabs = apTabs[idx].get();
      *ncomp = sncomp[idx];
      *status = 0;
   }   
}


namespace {
   void errReturn(fitsfile* pFile, const string& context, int status)
   {
      std::ostringstream oss;
      oss << context << "\nFITSIO error " << status << " in LDPFIL";
      string fullMsg(oss.str());
      xs_write(const_cast<char*>(fullMsg.c_str()), 10);
      int status2=0;
      fits_close_file(pFile, &status2);
   }
}
