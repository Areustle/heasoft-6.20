//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSFit/Fit/Fit.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>

int
XSGlobal::xsCosmo(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doCosmo(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doCosmo(const StringArray& rawArgs)
{
   using namespace XSContainer;
   const char* cmd = "cosmo";
   const size_t nArgs = rawArgs.size();
   if (nArgs == 1)
   {
      tcout << "\n Cosmology parameters: H0 = " << models->cosmo().H0 
            << " q0 = " << models->cosmo().q0
            <<  " Lambda0 = " << models->cosmo().lambda0 << std::endl; 
   }
   else
   {
      string arg(rawArgs[1]);  
      if (arg.size() && arg[0] == '?')
      {
         XSGlobal::printDocs(cmd,"?");      
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
         bool isChanged = false;
         for (size_t i=0; i<iParams.size(); ++i)
         {
            Real value=0.0;
            int iPar = iParams[i];
            if (iPar >= 0 && iPar < 3)
            {
               if (XSutility::isReal(params[i], value))
               {
                  switch (iPar)
                  {
                     case 0:
                        models->cosmo().H0 = value; 
                        FunctionUtility::setH0(value);
                        isChanged = true;
                        break;
                     case 1:
                        models->cosmo().q0 = value; 
                        FunctionUtility::setq0(value);
                        isChanged = true;
                        break;
                     case 2:
                        models->cosmo().lambda0 = value;
                        FunctionUtility::setlambda0(value); 
                        isChanged = true;
                        break;
                     default:
                        break;
                  }
               }
               else 
               {
                  string errPar;
                  switch (iPar)
                  {
                     case 0:
                        errPar = "H0";
                        break;
                     case 1:
                        errPar = "q0";
                        break;
                     case 2:
                        errPar = "lambda0";
                        break;
                     default:
                        break;
                  }
                  tcout << "\n***Invalid input ignored for cosmo parameter "
                        << errPar << ": " <<params[i]
                        << std::endl;
               } 
            } // end if iPar in valid range
         } // end params loop
         if (isChanged)
         {
            // Unfortunately we have no way of knowing at the moment if
            // a model contains a component that is affected by the cosmo
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
      } // end if not '?'   
   }
   return 0;
}
