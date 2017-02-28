//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>        
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>

int
XSGlobal::xsRenorm(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doRenorm(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doRenorm(const StringArray& rawArgs)
{
   using XSContainer::fit;
   const char* cmd = "renorm";
   const size_t nArgs = rawArgs.size();
   if (nArgs == 1)
   {
      if (!fit->statManager()->usingSingleStat())
      {
         tcout <<"***Renormalization cannot be performed when multiple fit statistics are in use.\n"
            << std::endl;
      }
      else
      {
         try
         {
            fit->renormalize();      
            fit->Update();                
         }
         catch (Fit::CantInitialize)
         {
            return -1;
         }
      }
   }
   else // ? | NONE | AUTO | PREFIT
   {

      string arg = rawArgs[1];
      arg = XSutility::lowerCase(arg);
      static const string onlyExplicit ("none");
      static const string automatic ("auto");
      static const string beforeFit ("prefit");

      // these are diagnostics, not error messages.
      if (automatic.find(arg) != XSparse::NOTFOUND() )
      {
         fit->renormType(Fit::AUTO); 
         tcout << " Renormalization will be performed automatically\n";     
      }
      else if (onlyExplicit.find(arg) !=  XSparse::NOTFOUND() )
      {
         fit->renormType(Fit::NONE); 
         tcout << " Implicit renormalizations disabled - use renorm command\n";                          
      }
      else if (beforeFit.find(arg) !=  XSparse::NOTFOUND() )
      {
         fit->renormType(Fit::PREFIT); 
         tcout << " Renormalizations only performed before fitting\n" ;                          
      }
      else
      {
         printDocs(cmd,"?");
         if ( arg[0] != '?' ) return -1;          
      }

      if (!fit->statManager()->usingSingleStat())
      {
         tcout <<"\n***Warning: Renormalizations will not be performed while multiple"
               <<"\n        fit statistics are in use.\n" << std::endl;
      }                
   }
   tcout << std::flush;
   return 0;
}
