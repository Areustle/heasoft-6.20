//
//  XSPEC12  November 2003
//
//
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSUser/Global/Global.h>        
#include <XSUser/Global/XSGlobal.h>        
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>        
#include <XSstreams.h>
#include <xsTypes.h>

int
XSGlobal::xsNotice(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doNotice(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doNotice(const StringArray& rawArgs)
{
    using namespace XSContainer;

    const size_t nArgs = rawArgs.size();
    const char* cmd = "notice";
    bool isChanged = false;
    bool isRespChanged = false;
    try
    {
	if (nArgs == 1)
	{
	    printDocs(cmd,"?");
	}
	else
	{
	    string arg(rawArgs[1]);
	    if (arg == "all")
	    {
		// go through DataContainer datasets and set all 
		// noticedArrayelements
		IntegerArray dummy(2, -2), dummy2(2, -2);
		datasets->setChannels(true, dummy, dummy2);
		tcout << "\nAll channels noticed";  
		isChanged = true;
		IntegerArray tmp(0);
		isRespChanged = datasets->resetDetectors(tmp);                              
	    }
	    else
	    {
                IntegerArray iParams(0);
                StringArray specifiers(0);
                StringArray args(nArgs-1);
		for(size_t i=1; i < nArgs; ++i)
		    args[i-1] = rawArgs[i];
                // iParams is just a dummy array in this context.
                XSparse::collectParams(args, iParams, specifiers);

		static std::vector<Real> prevRanges;
                if (prevRanges.empty())
                {
                   prevRanges.resize(4);
                   prevRanges[0] = prevRanges[1] = 1;
                   prevRanges[2] = prevRanges[3] = -2;
                }
		static bool isReal = false;
		HandlerUtils::IgnoreNoticeParse(specifiers, prevRanges, isReal, isRespChanged, isChanged, true);
	    }
	}
	if (isChanged)
	{
	    datasets->Notify();
	}
	if (isRespChanged)
	{
	    tcout << "Existing dummy responses have been removed from all spectra."<<std::endl;
	    tcout << "Original responses (if any) have been restored."<<std::endl;	   
	}		
	tcout << std::endl;
	return 0;
    }
    catch (YellowAlert&)
    {
       // Still need to check isChanged since modifications may
       // have been made to spectra prior to arg that caused error.
       if (isChanged)
       {
          datasets->Notify();
       }
       if (isRespChanged)
       {
	   tcout << "Existing dummy responses have been removed from all spectra."<<std::endl;
	   tcout << "Original responses (if any) have been restored."<<std::endl;	   
       }
       tcout << std::endl;
       return -1;
    }
}
