//
//  XSPEC12  November 2003
//
//
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSutility.h>
#include <xsTypes.h>
#include <XSstreams.h>


int 
XSGlobal::xsInitpackage(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doInitpackage(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doInitpackage(const StringArray& rawArgs)
{
   const char* cmd = "initpackage";
   const size_t nArgs = rawArgs.size();
   string shscript = "xspackage";
   if ( nArgs < 3 )
   {
      XSGlobal::printDocs(cmd,"?");      
      return -1;
   }
   else
   {
      string lc(rawArgs[1]);
      lc = XSutility::lowerCase(lc);
      if (lc == string("static_local_mods"))
      {
         tcerr << "\n***Error: Initpackage will only build static libraries\n"
               << "   when run as a stand-alone application outside of XSPEC."
               << std::endl;
         return -1;
      }
      string initCmd(shscript);
      initCmd +=  string(" ") + rawArgs[1];
      initCmd +=  string(" ") + rawArgs[2] + string(" ");
      string buildDir;
      const string UDMGET("-udmget");
      bool isUdmget = false;
      if ( nArgs == 3 )
      {
         buildDir = globalData->defaultLocalModelDirectory();       
      }
      else
      {
         // 3rd argument may be the local model dir or the "-udmget" flag
         // If it's the latter, we must insert the default directory 
         // prior to it.
         lc = string(rawArgs[3]);
         lc = XSutility::lowerCase(lc);
         if (lc.length() > 1 && UDMGET.find(lc) == 0)
         {
            buildDir = globalData->defaultLocalModelDirectory();
            isUdmget = true;
         }
         else 
            buildDir =  rawArgs[3]; 

      }
      initCmd += buildDir;
      if (isUdmget)
      {
         initCmd += string(" ") + UDMGET;
      }

      // The "-udmget" flag may also appear as an optional 4th argument
      if (nArgs > 4)
      {
         // Not going to check what the 4th arg actual is.  Simply 
         // append it and pass along to xspackage.
         initCmd += string(" ") + rawArgs[4];
      }

      if (!system(initCmd.c_str())) 
      {
         tcout <<"\nLocal model library has been built from model definition and code files in:\n"
            << buildDir <<"\n" << std::endl;
         return 0;
      }
      else return -1;
   }
}
