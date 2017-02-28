//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/XSGlobal.h>  
#include <XSUser/Global/Global.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSstreams.h>
#include <XSContainer.h>

int
XSGlobal::xsStatistic(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doStatistic(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doStatistic(const StringArray& rawArgs)
{
   using namespace XSContainer;

   const size_t nArgs = rawArgs.size();
   const char* cmd = "statistic";
   StatMethod* defaultStat = fit->statManager()->defaultStat();
   StatMethod* defaultTestStat = fit->statManager()->defaultTestStat();
   static RangePair prevRange(0,0);
   bool isTest(false);
   const string tt = "tT";
   size_t iArgs(1);

   if (nArgs == 1 || (rawArgs[1] == "?") || (nArgs == 2 && tt.find(rawArgs[iArgs][0]) != string::npos))
   {
       printDocs(cmd,"?");
       tcout <<"(Current default statistic: " 
             << defaultStat->fullName() <<")" <<std::endl;
       tcout <<"(Current default test statistic: " 
             << defaultTestStat->fullName() <<")" <<std::endl;
       tcout << std::flush;
       return 0;
   } 

   // check for the test statistic being specified. note that this assumes
   // none of the statistic names start with t.

   if ( tt.find(rawArgs[iArgs][0]) != string::npos )
   {
     isTest = true;
     iArgs++;
   }

   try
   {
     StatManager* manager = fit->statManager();
     StatMethod* newStatMethod;
     if ( isTest ) {
       newStatMethod = manager->getTestStatMethod(rawArgs[iArgs]);
     } else {
       newStatMethod = manager->getStatMethod(rawArgs[iArgs]);
     }
     if ( newStatMethod ) 
     {
       bool isSettingDefault = false;
       const size_t nSpectra = datasets->numberOfSpectra();
       RangePair limits(1,nSpectra);
       // Note that nSpectra may be 0
       if (nArgs == iArgs+1)
       {
	 if ( isTest ) {
	   manager->setDefaultTestStat(newStatMethod->name());
	 } else {
	   manager->setDefaultStat(newStatMethod->name());
	 }
	 isSettingDefault = true;
       }
       else
       {
	 // When a range is supplied, don't make the StatMethod the
	 // new default.
	 if (!nSpectra)
	 {
	   tcout << "***No spectra currently loaded - cannot change statistics."<<std::endl;
	   return -1;
	 }
	 if (!prevRange.first) prevRange.first = 1;
	 if (!prevRange.second) prevRange.second = nSpectra;
	 StringArray rangeArgs(nArgs-2);
	 for (size_t i=iArgs+1; i<nArgs; ++i)
	   rangeArgs[i-2] = rawArgs[i];
	 // This may throw
	 IntegerArray selectedSpecs = XSparse::getRanges(rangeArgs,
							 prevRange, limits);
	 for (size_t i=0; i<selectedSpecs.size(); ++i)
	 {
	   const size_t specNum = static_cast<size_t>(selectedSpecs[i]);
	   SpectralData* spec = datasets->lookup(specNum);
	   if ( isTest ) {
	     spec->testStatName(newStatMethod->name());
	   } else {
	     spec->statName(newStatMethod->name());
	   }
	 }
       }

       if (isSettingDefault)
       {
	 if ( isTest ) {
	   tcout << "Default test statistic is set to: " <<newStatMethod->fullName() 
		 << "\n   This will apply to all current and newly loaded spectra."<<std::endl;
	 } else {
	   tcout << "Default fit statistic is set to: " <<newStatMethod->fullName() 
		 << "\n   This will apply to all current and newly loaded spectra."<<std::endl;
	 }
       }
       else
       {
	 if ( isTest ) {
	   tcout << "The "<<newStatMethod->fullName()<<" test statistic will be applied to "
		 << "the specified spectra.\n" << "   The default test statistic is: " 
		 << manager->defaultStat()->fullName()<<std::endl;
	 } else {
	   tcout << "The "<<newStatMethod->fullName()<<" fit statistic will be applied to "
		 << "the specified spectra.\n" << "   The default fit statistic is: " 
		 << manager->defaultStat()->fullName()<<std::endl;
	 }
       }

       const bool wasStillValid = fit->isStillValid();
       fit->Update();
       // Update will automatically mark fit as invalid, but if
       // it was previously valid and all we did is change the
       // stat method, let's still call it valid.
       fit->isStillValid(wasStillValid);
     } // end if found new StatMethod  
     else throw Fit::NoSuchStatMethod(rawArgs[iArgs]);
   }
   catch ( YellowAlert& )
   {
     printDocs(cmd,"?");
     return -1;   
   }

   tcout << std::flush;
   return 0;
}





