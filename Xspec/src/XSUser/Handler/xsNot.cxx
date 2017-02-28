//
//  XSPEC12  November 2003
//
//
#include "XSUser/Global/XSGlobal.h"  
#include "XSstreams.h"      

void
XSGlobal::xsNotImplemented(const string& cmd)
{
        tcerr << "Command not yet implemented: " << cmd << "\n";
}           


int
XSGlobal::xsGenetic(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
        string cmd = string(static_cast<char*>(cdata));
        xsNotImplemented(cmd);
        return TCL_OK;
}



int
XSGlobal::xsRecornrm(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
        tcerr << "The recornrm command has been replaced by the recorn model.\n";
        return TCL_OK;
}



int
XSGlobal::xsThleqw(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
        string cmd = string(static_cast<char*>(cdata));
        xsNotImplemented(cmd);
        return TCL_OK;
}
