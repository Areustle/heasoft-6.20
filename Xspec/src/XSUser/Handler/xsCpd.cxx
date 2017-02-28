//
//  XSPEC12  November 2003
//
//
#include <XSContainer.h> 
#include <XSstreams.h> 
#include <XSPlot/Plot/PlotDirector.h> 
#include <XSUser/Global/Global.h>  
#include <XSUser/Global/XSGlobal.h>  
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h> 

int
XSGlobal::xsCpd(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doCpd(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doCpd(const StringArray& rawArgs)
{
        using XSContainer::plot;
        // can also get here with "setplot device"
        const size_t nArgs = rawArgs.size();
        string commandName(rawArgs[0]);
        size_t argShift = 0;
        if (commandName[0] != 'c' && commandName[0] != 'C')
        {
           argShift = 1;
        }

        if (nArgs  >= 2 + argShift)
        {

           string firstArg (rawArgs[1 + argShift]);
           if ( firstArg[0] == '?')
           {
                printDocs(commandName.c_str(),"?");  
           }
           else
           {           
	           try
	           {
	                plot->setPlottingDevice(firstArg, plot->setplot().splashPage());
                   }
	           catch (YellowAlert &)
	           {
                        return -1;
	           }
           }
        }
        else
        {
                string dev (plot->getPlottingDeviceName());
                tcout << "Current Plotting Device: " ;
                Tcl_ResetResult(interp);
                Tcl_Obj* out (Tcl_NewStringObj(dev.c_str(),dev.size()));
                Tcl_SetObjResult(interp,out);
                if (dev.empty()) 
                {
                        tcout << "none\n";      
                }
                tcout << std::flush;
        }
        return 0;
}
