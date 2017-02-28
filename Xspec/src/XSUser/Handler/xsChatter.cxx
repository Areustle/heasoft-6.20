//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>       
#include <XSUser/Global/XSGlobal.h>        
#include <XSUtil/Utils/XSstream.h>
#include <XSstreams.h>
#include <CCfits/CCfits>


int
XSGlobal::xsChatter(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
        using std::endl;
        string cmd = string(static_cast<char*>(cdata));
        if (objc == 1)
        {
                tcout << " Console Chatter Level " << tpout.consoleChatterLevel()
                      << " Log Chatter Level " << tpout.logChatterLevel() << endl;
        }
        else 
        {
                if (objc >= 2)
                {
                        int value(0);
                        // if the value is garbage do nothing.
                        if (Tcl_GetIntFromObj(tclInterp,objv[1],&value) == globalData->autoSave(TCL_OK))
                        {
                                tpout.consoleChatterLevel(value); 
                                bool fitsVerbose = (value >= 25);
                                CCfits::FITS::setVerboseMode(fitsVerbose);
                        }
                        else
                        {
                                int length = 0;
                                char* input= Tcl_GetStringFromObj(objv[1],&length);
                                if (length > 1 || input[0] != '?') tcout << " Error: ";
                                tcout << "chatter syntax is " 
                                        << "chatter consoleChatterLevel logChatterLevel"
                                        << endl;
                        }
                        if (objc >= 3)
                        {
                                if (Tcl_GetIntFromObj(tclInterp,objv[2],&value) == globalData->autoSave(TCL_OK))
                                        tpout.logChatterLevel(value); 
                                else
                                {
                                        int length = 0;
                                        char* input= Tcl_GetStringFromObj(objv[2],&length);
                                        if (length > 1 || input[0] != '?') tcout << " Error: ";
                                        tcout << "chatter syntax is " 
                                                << "chatter consoleChatterLevel logChatterLevel"
                                                << endl;
                                }

                        }     

                }
        }
        return globalData->autoSave(TCL_OK);
}
