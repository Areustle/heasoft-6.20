//
//  XSPEC12  November 2003
//
//

#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>
#include <XSFit/Fit/Fit.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>

int
XSGlobal::xsXsect(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doXsect(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doXsect(const StringArray& rawArgs)
{
   using namespace XSContainer;
   const char* cmd = "xsect";
   const size_t nArgs = rawArgs.size();
   try
   {

      if (nArgs == 1)
      {
         // report current setting
         tcout << FunctionUtility::XSECT() << ": "
               << FunctionUtility::crossSections(FunctionUtility::XSECT()) 
               << std::endl;
      }
      else if (nArgs >= 2)
      {
         string arg = rawArgs[1]; 
         if ( arg[0] == '?')
         {
            // query options/syntax
            printDocs(cmd,"?");
         }   
         else
         {
            FunctionUtility::crossSections(arg);
            FunctionUtility::XSECT(arg);

            // Unfortunately we have no way of knowing at the moment if
            // a model contains a component that is affected by the xsect
            // change.  Therefore we'll just have to recalculate all the
            // models.  Active/On models will be calculated in fit->Update.
            // The rest we'll do now.
            ModelMap::const_iterator itMod = models->modelSet().begin();
            ModelMap::const_iterator itEnd = models->modelSet().end();
            while (itMod != itEnd)
            {
               Model* mod = itMod->second;
               mod->setComputeFlag(true);
               if (!mod->isActive())
               {
                  models->calculate(mod->name());
               }
               ++itMod;
            }
            fit->Update();
         }   
      }
   }
   catch ( YellowAlert&)
   {
      // catch instances of NoInitializer or InvalidAbundanceTable
      // and return an error.
      return -1;
   }
   return 0;
}
