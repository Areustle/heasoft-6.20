#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/UserInterface/xstcl.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>

int
XSGlobal::xsUndo(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
    using namespace XSContainer;

    //datasets->clear();
    models->clearLists();
    models->Notify();

    string arg = "@";
    arg += globalData->undoFile();

    xstcl::xs_execute_script(tclInterp, const_cast<char*>(arg.c_str()));

    std::ofstream out(globalData->undoFile().c_str(), std::ios::trunc);

    out.close();

    return TCL_OK;
}
