//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSUtil/Utils/XSutility.h>     
#include <XSContainer.h>
#include <XSstreams.h>

int
XSGlobal::xsSystematic(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doSystematic(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doSystematic(const StringArray& rawArgs)
{
   using XSContainer::models;
   const char* cmd = "systematic";
   const size_t nArgs = rawArgs.size();
   string arg;
   if (nArgs == 1 || (arg = rawArgs[1]) == "?")
   {
       printDocs(cmd,"?");
   } 
   else
   {
       Real syst(0.);
       if (XSutility::isReal(arg,syst))
       {
           models->modelSystematicError(syst);   
           if (syst < 0 || syst >= 1)
           {
               tcout << "*** Warning: model systematic error value set to " << syst
                   << ": \n*** value is expected to be a fraction of model count rate"
                   << "\n*** to be included in error estimates\n";    
           }
           else 
           {
               tcout << " Model systematic error set to " << syst << '\n';
           }
       }
       else
       {
           tcout << " systematic: require real value as input \n";
           return -1;
       }
   }
   tcout << std::flush;
   return 0;
}
