//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUtil/Error/Error.h>
#include <xsTypes.h>

int
XSGlobal::xsPlot(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   int status = TCL_OK;
   try
   {
        string cmd = string(static_cast<char*>(cdata));
        if (objc > 1 && string(Tcl_GetString(objv[1])) == "?")
        {
            printDocs(cmd.c_str(),"?");
        } 
	else
	{
                StringArray args(std::max(1,objc-1),"");
                for ( int j = 1; j < objc; ++j)
                {
                        args[j-1] = Tcl_GetString(objv[j]);              
                }

	        XSGlobal::commonPlotHandler(args, false);
	}
   }
   catch (YellowAlert&)
   {
      status = TCL_ERROR;
   }		
   return globalData->autoSave(status);
}
