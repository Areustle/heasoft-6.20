//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>  
#include <XSUser/Global/XSGlobal.h>  
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSFit/Fit/Fit.h>
#include <XSUtil/Parse/XSparse.h>      
#include <XSUtil/Utils/XSutility.h>

#include <XSContainer.h>
#include <XSstreams.h>
#include <sstream>
#include <iomanip>

int
XSGlobal::xsGoodness(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doGoodness(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doGoodness(const StringArray& rawArgs)
{
   using XSContainer::fit;

   const char* cmd = "goodness";
   const size_t nArgs = rawArgs.size();
   if (nArgs == 2 && rawArgs[1] == "?") 
   {
           printDocs(cmd,"?");
           return 0;
   }

   //   if (!fit->isStillValid())
   //   {
   //      tcerr << "A valid fit is first required in order to run goodness command."
   //           <<std::endl;
   //      return -1;
   //   }
   const bool isOrigFitValid = fit->isStillValid();
   
   
   // get the default setting from the Fit class. Idea is to allow
   // this to be set in the init file.
   int NRealizations = Fit::MCrealizations();
   static bool isSim = false;
   static bool isFit = false;

   if ( nArgs > 1 )
   {
      std::istringstream iss(rawArgs[1]);
      int testNR=0;
      if (!(iss >> testNR) || !iss.eof() || testNR < 1)
      {
         tcerr << "Invalid argument for <# of realizations>"<<std::endl;
         return -1;
      }
      NRealizations = testNR;
   
      const string NOSIM("nosim");
      const string SIM("sim");
      const string NOFIT("nofit");
      const string FIT("fit");

      if ( nArgs > 2 )
      {
         string simParams(XSutility::lowerCase(rawArgs[2]));
         if (SIM.find(simParams) == 0)
            isSim = true;
         else if (NOSIM.find(simParams) == 0)
            isSim = false;
         else if (FIT.find(simParams) == 0)
            isFit = true;
         else if (NOFIT.find(simParams) == 0)
            isFit = false;
         else
         {
            tcerr << "Unrecognized third argument.  Valid args: sim | nosim | fit | nofit ."
               << std::endl;
            return -1;
         }

      }

      if ( nArgs > 3 )
      {
         string simParams(XSutility::lowerCase(rawArgs[3]));
         if (SIM.find(simParams) == 0)
            isSim = true;
         else if (NOSIM.find(simParams) == 0)
            isSim = false;
         else if (FIT.find(simParams) == 0)
            isFit = true;
         else if (NOFIT.find(simParams) == 0)
            isFit = false;
         else
         {
            tcerr << "Unrecognized fourth argument.  Valid args: sim | nosim | fit | nofit ."
               << std::endl;
            return -1;
         }

      }


   }

   // ensure that the models and the data are defined and stored
   // in the Fit object.

   try
   {
      fit->reinitialize(false);
      fit->goodness(NRealizations,isSim,isFit);
      fit->isStillValid(isOrigFitValid);
   }
   catch ( YellowAlert& )
   {
      return -1;       
   }
   tcout << std::flush;
   return 0;        
}
