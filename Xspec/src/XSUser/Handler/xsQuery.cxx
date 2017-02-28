//
//  XSPEC12  November 2003
//
//

#include "XSUser/Global/Global.h"
#include "XSUser/Global/XSGlobal.h"
#include "XSFit/Fit/Fit.h"
#include "XSContainer.h"
#include "XSstreams.h"
#include "xsTypes.h"


int
XSGlobal::xsQuery(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
        string cmd = string(static_cast<char*>(cdata));
        string arg("");
        // query on, yes, no  are the possibilities.
        static const string ALLOWED = "oyn?";
        if (objc == 1)
        {
                printDocs(cmd.c_str(),"?");
        } 
        else
        {       
                char* a = Tcl_GetString(objv[1]);
                size_t f (ALLOWED.find(tolower(a[0])));
                if ( f <= 2 )
                {
                        XSContainer::fit->queryMode(Fit::querySetting(f));
                }
                else
                {
                        printDocs(cmd.c_str(),"?");                       
                }
        }
        tcout << std::flush;
        return globalData->autoSave(TCL_OK);

}
