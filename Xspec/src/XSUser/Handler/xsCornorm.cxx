//
//  XSPEC12  November 2003
//
//
#include <XSUser/Global/Global.h>        
#include <XSUser/Global/XSGlobal.h>        
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSFit/Fit/Fit.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>        
#include <XSstreams.h>
#include <xsTypes.h>

namespace
{
   void setCornorms(const std::vector<RangePair>& ranges, Real norm, 
                std::vector<SpectralData*>& specsChanged);
}

int
XSGlobal::xsCornorm(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doCornorm(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doCornorm(const StringArray& rawArgs)
{
   using  XSContainer::datasets;
   static RangePair lastRange(1,1);
   static Real lastNorm = 1.0;
   const char* cmd = "cornorm";
   const size_t nArgs = rawArgs.size();
   std::vector<SpectralData*> specsChanged;
   int status = 0;
   if (nArgs == 1)
   {
       printDocs(cmd,"?");
   }
   else
   {
       try
       {
          IntegerArray iParams;
          StringArray xsParams;
          StringArray args(nArgs-1,"");
          for (size_t i=1; i<nArgs; ++i) args[i-1] = rawArgs[i];
          XSparse::collectParams(args, iParams, xsParams);
          size_t nPars = xsParams.size();
          if (nPars > 0)
          {
           // Commas used as arg placeholders (rather than mere delimiters) 
           // make no sense for this command, aside from perhaps the first
           // arg.  Check that iParams are numbered consecutively from 0 or 1.
             int iPrev = iParams[0];
             bool isBadSyntax = false;
             if (iPrev != 0 && iPrev != 1)
             {
                isBadSyntax = true;
             }
             for (size_t i=1; !isBadSyntax && i<nPars; ++i)
             {
                if (iParams[i] != (iPrev + 1))
                {
                   isBadSyntax = true;
                }
                iPrev = iParams[i];
             }
             if (isBadSyntax)
             {
                tcerr << "***Error: Improper syntax for cornorm command.\n\n"
                   <<"Cornorm command summary:" << std::endl;
                printDocs(cmd,"?");
                return -1;
             }
             if (iParams[0] == 0 && xsParams[0] == string("?"))
             {
                printDocs(cmd,"?");
                return 0;
             }

             const size_t nAllSpectra = datasets->numberOfSpectra();
             if (!nAllSpectra)
             {
                tcout << "Cannot set cornorm values, no spectra loaded."
                  <<std::endl;
                return 0;
             }
             const RangePair maxRange(1, nAllSpectra);
             std::vector<RangePair> pendingRanges; 
             for (size_t i=0; i<nPars; ++i)
             {
                string inArg = xsParams[i];
                Real normTest = .0;
                bool entryIsValue = true;
                tperr << xsverbose(0);
                try
                {
                   // If wildRange succeeds, lastRange will be
                   // automatically updated with newest range.
                   XSparse::wildRange(inArg, maxRange, lastRange);
                   entryIsValue = false;
                   pendingRanges.push_back(lastRange);
                }
                catch (XSparse::InvalidRange)
                {
                   // OK, so this is not a range entry.  Then it better
                   // be a real.
                   if (!XSutility::isReal(inArg,normTest))
                   {
                      tperr << xsverbose();
                      string msg ("cornorm takes spectral ranges and real values");
                      throw XSparse::InputError(msg);                      
                   }
                   else
                      lastNorm = normTest;
                }
                catch (YellowAlert&)
                {
                   tperr << xsverbose();
                   throw;
                }
                tperr << xsverbose();
                if (entryIsValue)
                {
                   if (pendingRanges.empty())
                   {
                      // This is for case where the first arg is a value.
                      pendingRanges.push_back(lastRange);
                   }
                   setCornorms(pendingRanges, lastNorm, specsChanged);
                   pendingRanges.clear();
                }                
             } // end arg loop
             if (pendingRanges.size())
             {
                setCornorms(pendingRanges, lastNorm, specsChanged);
             }
          } // end if nPars > 0
       }
       catch (YellowAlert&)
       {
          status = -1;
       }
       if (specsChanged.size())
       {
          std::vector<SpectralData*>::iterator itSp = specsChanged.begin();
          std::vector<SpectralData*>::iterator itSpEnd = specsChanged.end();
          while (itSp != itSpEnd)
          {
             (*itSp)->computeTotals();
             ++itSp;
          }
          XSContainer::fit->Update();
       }
   }
   return status;        
}

namespace
{
   void setCornorms(const std::vector<RangePair>& ranges, Real norm,
                std::vector<SpectralData*>& specsChanged)
   {
      const size_t nAllSpectra = XSContainer::datasets->numberOfSpectra();
      for (size_t i=0; i<ranges.size(); ++i)
      {
         const RangePair& range = ranges[i];
         for (size_t j=range.first; j<=range.second; ++j)
         {
            if (j > nAllSpectra)
            {
               tcout << " Spectrum " << j << " not loaded." <<std::endl;
               break;
            }
            SpectralData* sd = XSContainer::datasets->lookup(j);
            if (sd)
            {
               sd->correctionScale(norm);
               sd->correctionChanged(true);
               specsChanged.push_back(sd);
               tcout << " Spectrum " << j << " correction norm set to "
                  << norm << std::endl;
            }                     
         }
      }
      return;
   }
}
