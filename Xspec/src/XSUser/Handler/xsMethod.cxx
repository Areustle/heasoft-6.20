//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSFit/Fit/Fit.h>        
#include <XSFit/Fit/FitMethod.h>        
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>
#include <XSUtil/Parse/XSparse.h>
#include <memory>

int
XSGlobal::xsMethod(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doMethod(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doMethod(const StringArray& rawArgs)
{
   using namespace XSContainer;
   const char* cmd = "method";
   const size_t nArgs = rawArgs.size();
   string arg;
   if (nArgs == 1 || (arg = rawArgs[1]) == "?")
   {
       printDocs(cmd,"?");
   } 
   else
   {
      StringArray inArgs;
      StringArray params;
      IntegerArray iParams; 
      for (size_t i=1; i<nArgs; ++i)
      {
         inArgs.push_back(rawArgs[i]);
      }
      XSparse::collectParams(inArgs, iParams, params); 

      try
      {
         if (iParams.size())
         {
            if (iParams[0] == 0)
            {
               std::auto_ptr<FitMethod> newFitMethod(FitMethod::get(params[0]));
               if (newFitMethod.get())
               {
                  newFitMethod->processMethodString(iParams, params);
                  // This will automatically call fit update and reset
                  // the isStillValid flag if necessary.
                  fit->fitMethod(newFitMethod.release());
               }
               else throw Fit::NoSuchFitMethod(params[0]);
            }
            else 
            {
               fit->fitMethod()->processMethodString(iParams, params);
            }

         }
      }
      catch ( YellowAlert& )
      {
          // should actually write some code in NoSuchFitMethod that lists
          // the options.
          return -1;   
      }
   }
   tcout << std::flush;
   return 0;
}
