//
//  XSPEC12  November 2003
//
//
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Parse/XSparse.h>
#include <unistd.h> 
#include <XSstreams.h>
#include <XSContainer.h>
#include <xsTypes.h>
#include <cstring>


int 
XSGlobal::xsLmod(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doLmod(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int 
XSGlobal::doLmod(const StringArray& rawArgs)
{
        static const string LAPPEND("lappend ");
        static const string AUTOPATH("auto_path ");
        const char* cmd = "lmod";
        const size_t nArgs = rawArgs.size();
        if (nArgs == 1 )
        {
                XSGlobal::printDocs(cmd,"?");      
        }
        else
        {
           string arg(rawArgs[1]);
           if ( arg[0] == '?') 
           {
                   XSGlobal::printDocs(cmd,"?");   
                   return 0;
           }
           else  
           {
              // simple case: use default local model library, and 
              // the model description has filename packageName.dat
              const char* pkgName = rawArgs[1].c_str();
              if (nArgs == 2 )
              {
                 if ( globalData->defaultLocalModelDirectory().length())
                 {
                    string sCmd = LAPPEND;
                    sCmd += AUTOPATH;
                    sCmd += globalData->defaultLocalModelDirectory();
                    // To avoid passing const char* to Tcl_Eval:
                    char* c_sCmd(new char[sCmd.length() + 1]);
                    sCmd.copy(c_sCmd, string::npos);
                    c_sCmd[sCmd.length()] = 0;
                    Tcl_Eval(interp,c_sCmd);
                    delete [] c_sCmd;
                    const char* pkgTest =
                                    Tcl_PkgRequire(interp,pkgName,NULL,0);
                    if (!pkgTest)
                    {
                       // This is a hack to fix the case where lmod is being
                       //  called from within a start-up .rc script.  It fails
                       //  on the first call but succeeds on a second call.
                       //  (This begain with version 12.9.0 once the pkg_mkIndex
                       //  stage was removed from lmod.)
                       pkgTest = Tcl_PkgRequire(interp,pkgName,NULL,0);
                    }
                    Tcl_ResetResult(interp);
                    if (!pkgTest)
                    {
                       tcerr << "\n***Error: Xspec was unable to load the model package: "
                             << pkgName << "\n   Either it could not find the model library file "
                             <<"in the directory:\n"<<globalData->defaultLocalModelDirectory()
                             <<"\n   or the file contains errors."
                             <<"\n   (try \"load (path)/(lib filename)\" for more error info)"
                             <<std::endl;
                       return -1;

                    }
                    else
                    {
                       tcout << "Model package " << pkgName << " successfully loaded."
                             << std::endl;
                    }
                    return 0;  
                 }  
                 else
                 {
                         tcerr << " No valid setting for the default local model "
                               << " directory. \nSet a path for the model library or "
                               << " modify the default setting in Xspec.init\n";
                         return -1;
                 }  
              }
              else if (nArgs >= 3 )
              {
                 string directory =  rawArgs[2];
                 string fullDirectory("");
                 if ( !strchr("/$~.",directory[0]) )
                 {
                         tcerr << "*** Error: local model directory must either be"
                               << " an absolute path \n*** or begin with a defined "
                               << " environment variable, or shell character "
                               << " {~,./,../}\n";
                         return -1;
                 } 
                 else
                 {
                    if (XSparse::addToLibraryPath(directory,fullDirectory,
                                                    F_OK|R_OK|W_OK|X_OK))
                    {
                       string sCmd = LAPPEND;
                       sCmd += AUTOPATH;
                       sCmd += fullDirectory;
                       char* c_sCmd(new char[sCmd.length() + 1]);
                       sCmd.copy(c_sCmd, string::npos);
                       c_sCmd[sCmd.length()] = 0;
                       Tcl_Eval(interp,c_sCmd);
                       delete [] c_sCmd;

                       // nothing in here can throw.
                       const char* pkgTest 
                               = Tcl_PkgRequire(interp,pkgName,NULL,0);
                       if (!pkgTest)
                       {
                          // This is a hack to fix the case where lmod is being
                          //  called from within a start-up .rc script.  It fails
                          //  on the first call but succeeds on a second call.
                          //  (This begain with version 12.9.0 once the pkg_mkIndex
                          //  stage was removed from lmod.)
                          pkgTest = Tcl_PkgRequire(interp,pkgName,NULL,0);
                       }
                       Tcl_ResetResult(interp);
                       if (!pkgTest)
                       {
                          tcerr << "\n***Error: Xspec was unable to load the model package: "
                                << pkgName << "\n   Either it could not find the model library file "
                                <<"in the directory:\n"<<fullDirectory
                                <<"\n   or the file contains errors."
                                <<"\n   (try \"load (path)/(lib filename)\" for more error info)"
                                <<std::endl;
                          return -1;

                       }
                       else
                       {
                          tcout << "Model package " << pkgName << " successfully loaded."
                                << std::endl;
                       }
                    }
                    else
                    {
                            tcerr << "*** Error: local model directory \n"
                               << "*** " << fullDirectory 
                               << "\n*** cannot be added to the load path. "
                               << "\n*** most likely it has an undefined "
                               << "environment variable "
                               << "\n*** or insufficient access permissions "
                               << "(The directory must exist and be writable)"
                               << "\n";      
                            return -1; 
                    } 

                 }      
              }
           }
        }



        return 0;

}

int 
XSGlobal::doLoad(const StringArray& rawArgs)
{
   // This function is intended to be accessible only from PyXspec.
   // It provides a way for Python users to by-pass lmod and directly
   // call Tcl's 'load' function.  It is mostly useful for diagnostic purposes.
   if (rawArgs.size() < 2)
      return -1;
   if (rawArgs[1].empty())
      return -1;
   
   int status=0;
   // rawArgs[1] should contain the full path to the local mod lib.
   string sCmd("load ");
   const string fullPath(rawArgs[1]);
   // perform some validation
   for (size_t i=0; i<fullPath.length(); ++i)
   {
      int asc = static_cast<int>(fullPath[i]);
      if (asc < 32 || asc > 126)
      {
         tcerr << "Invalid string for path to local library"<<std::endl;
         return -1;
      }
   }
   
   sCmd += fullPath;   
   char* c_sCmd(new char[sCmd.length() + 1]);
   sCmd.copy(c_sCmd, sCmd.length());
   c_sCmd[sCmd.length()] = 0;
   status = Tcl_Eval(interp,c_sCmd);
   delete [] c_sCmd;
   
   if (status != TCL_OK)
   {
      tcerr << Tcl_GetStringResult(interp) << std::endl;
      Tcl_ResetResult(interp);
      return -1;
   }
    
   return 0;
}
