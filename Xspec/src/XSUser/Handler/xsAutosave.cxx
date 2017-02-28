//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/XSGlobal.h>  
#include <XSUser/Global/Global.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSstreams.h>      

int
XSGlobal::xsAutosave(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doAutosave(rawArgs);
   if (!status)
      return TCL_OK;
   else
      return TCL_ERROR;
}

int
XSGlobal::doAutosave(const StringArray& rawArgs)
{
   const char* cmd = "autosave";
   const size_t nArgs = rawArgs.size();
   const string OFF ("off");
   if (nArgs == 1 ) 
   {
      printDocs(cmd,"?");
      return 0;       
   }
   else 
   {
      string arg(rawArgs[1]);
      if ( arg[0] == '?')
      {
         printDocs(cmd,"?");
         return 0;      
      }   
      else
      {
         size_t f=0;
         if ((f = XSutility::isInteger(arg)) != XSparse::NOTFOUND())
         {    
            tcout << "Auto-save frequency set to " << f << std::endl;
            globalData->autoSaveFrequency(f);     
         }    
         else 
         {
            arg = XSutility::lowerCase(arg);
            if ( arg.compare(0,std::min(arg.length(),OFF.length()),OFF) == 0)
            {
               tcout << "Auto-save disabled " << std::endl;
               globalData->autoSaveFrequency(XSparse::NOTFOUND());
            }
            else
            {
               printDocs(cmd,"?");
               return -1;
            }

         }

      }
   }
   return 0;
}
