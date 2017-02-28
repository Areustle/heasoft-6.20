//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSstreams.h>
#include <xsTypes.h>

int
XSGlobal::xsVersion(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doVersion(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doVersion(const StringArray& rawArgs)
{
        string version, buildDate;
	XSutility::XSVersionString(version, buildDate);
        tcout << '\n' << version << std::endl;
        return 0;

}         
