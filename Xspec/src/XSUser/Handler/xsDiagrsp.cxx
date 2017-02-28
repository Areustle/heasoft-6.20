//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/Data/DataSet.h>
#include <XSModel/Data/DataInputRecord.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/UserDummyResponse.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>


#include <XSContainer.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <xsTypes.h>

int
XSGlobal::xsDiagrsp(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doDiagrsp(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doDiagrsp(const StringArray& rawArgs)
{
   using XSContainer::datasets;

   bool allOk (true);
   const char* cmd = "diagrsp";
   const size_t nArgs = rawArgs.size();
   if (nArgs > 1)
   {
      printDocs(cmd,"?");
   }
   else
   {
      const size_t nSpec = datasets->numberOfSpectra();
      if (!nSpec)
      {
         tcout << "\nData must be loaded to use diagrsp command." <<std::endl;
	 return -1;
      }
      const size_t nSources = datasets->numSourcesForSpectra();
      for (size_t i=1; i<=nSpec; ++i)
      {
         size_t row=0;
         DataSet* ds = datasets->dataSetLookup(i, row);
         size_t dGroup = ds->dataGroup();
         SpectralData* sd = ds->sourceData(row);
	 for (size_t j=0; j<nSources; ++j)
	 {
	    // NOTE: To create a UserDummyResponse from this command 
	    // (unlike from dummyrsp) a real response already needs to
	    // exist.
	    if (!sd->detector(j))
	    {
	       // No response exists, real or otherwise.  Do nothing.
	       allOk = false;
	    }
	    else
	    {
	       // If a user dummy response is already in place, this will
	       // remove it and put back the real response.  If no real
	       // response exists, it puts back a null.  If no user dummy
	       // response is there, this does nothing and returns false.
	       if (sd->removeUserDummy(j))
	       {
	          if (!sd->detector(j))
		  {
		     allOk = false;
		  }
		  else
		  {
                     UserDummyResponse *newUd = new UserDummyResponse(sd, j);
                     newUd->dataGroup(dGroup);
		     sd->attachUserDummy(newUd, j);
		  }
	       }
	       else
	       {
	          // A real response exists in the current detector position.
                  UserDummyResponse *newUd = new UserDummyResponse(sd, j);
                  newUd->dataGroup(dGroup);
		  sd->attachUserDummy(newUd, j);
	       }
	    }
	 } // end nSources loop
      } // end spectra loop
      if (!allOk)
      {
         tcerr << "\n***Warning!  1 or more spectra/sources have no real response."<<std::endl;
	 tcerr << "   No diagonal response will be associated with them." <<std::endl;
      }
      datasets->Notify();
   }
   if (!allOk) return -1;
   else return 0;
}
