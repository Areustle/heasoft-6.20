//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Signals/SignalHandler.h>
#include <XSFit/Fit/Fit.h>        
#include <XSFit/Fit/FitMethod.h>        
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>

int
XSGlobal::xsFit(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doFit(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doFit(const StringArray& rawArgs)
{
   using namespace XSContainer;

   const size_t nArgs = rawArgs.size();
   const char* cmd = "fit";
   if (nArgs >= 2 &&  (rawArgs[1] == "?"))
   {
       printDocs(cmd,"?");
   } 
   else
   {
      StringArray params;
      IntegerArray iParams;
      XSparse::collectParams(rawArgs, iParams, params); 
      try
      {
         if (iParams.size()>1)
         {
            fit->fitMethod()->processMethodString(iParams, params);
         }
      }
      catch (YellowAlert&)
      {
         return -1;
      }

      SIGINT_Handler intHandler;
      SignalHandler* sigContainer = SignalHandler::instance();
      EventHandler* oldHandler = 0;
      try
      {
         bool saveParameterValues = true;
         // fit->reinitialize and fit->initializeStatistic
         // will both throw Fit::CantInitialize 
         // if there is no model or no data.
         fit->reinitialize(saveParameterValues);

         fit->initializeStatistic();

         oldHandler = sigContainer->registerHandler(SIGINT,
                              &intHandler);
         fit->perform();
         sigContainer->registerHandler(SIGINT,oldHandler);
         fit->report();
      }
      catch ( FitMethod::FitError )
      {
         if (oldHandler)
         {
            sigContainer->registerHandler(SIGINT,oldHandler);
         }    
         fit->resetParameters();
         return -1;       
      }
      catch (Fit::FitInterrupt)
      {
         sigContainer->registerHandler(SIGINT,oldHandler);
         return -1;
      }
      catch ( YellowAlert& )
      {
          // should actually write some code in NoSuchFitMethod that lists
          // the options.
          // output stream seems in bad state if interrupted. Attempt to correct...
          if (oldHandler)
          {
             sigContainer->registerHandler(SIGINT,oldHandler);
          }    
          tcout.exceptions(std::ios_base::goodbit);
          return -1;   
      }
   }
   if (!fit->errorCalc()) tcout << std::flush;
   return 0;
}




