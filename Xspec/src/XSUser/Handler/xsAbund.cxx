//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>

int
XSGlobal::xsAbund(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doAbund(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doAbund(const StringArray& rawArgs)
{
   using namespace XSContainer;
   const char* cmd = "abund";
   const size_t nArgs = rawArgs.size();
   try
   {

      if (nArgs == 1)
      {
         // report current setting
         tcout << FunctionUtility::ABUND() << ": "
               << FunctionUtility::abundDoc(FunctionUtility::ABUND()) 
               << std::endl;
         return 0;
      }
      else 
      {
         string arg = XSutility::lowerCase(rawArgs[1]); 
         if ( arg[0] == '?')
         {
                 // query options/syntax
                 printDocs(cmd,"?");
                 return 0;       
         } 
         else if (arg == "file")
         {
            if (nArgs < 3)
            {
               tcerr << "File name must be specified as 3rd argument." <<std::endl;
               return -1;
            }   
            // read the file, and set the abundance
            // vector if successful.
            string file(rawArgs[2]);
            FunctionUtility::readNewAbundances(file);
            // success.
         }  
         else
         {
            if (FunctionUtility::checkAbund(arg))
            {
               FunctionUtility::ABUND(arg);
            }
            else
            {
               // no filename specified so rethrow
               // exception with a message.
               string diag(" solar abundance table ");
               diag += arg;
               diag += ", table not changed.";

               throw FunctionUtility::NoInitializer(diag);
            }
         }
         // Unfortunately we have no way of knowing at the moment if
         // a model contains a component that is affected by the abund
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
   catch ( YellowAlert&)
   {
           // catch instances of NoInitializer or InvalidAbundanceTable
           // and return an error.
           return -1;
   }

   return 0;
}
