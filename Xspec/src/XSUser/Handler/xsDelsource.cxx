#include <xsTypes.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <XSContainer.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h> 

int
XSGlobal::xsDelsource(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   try
   {
   }
   catch (YellowAlert&)
   {
      return globalData->autoSave(TCL_ERROR);
   }
   return globalData->autoSave(TCL_OK);
}
