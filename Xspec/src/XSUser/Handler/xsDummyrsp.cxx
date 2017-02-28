//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>        
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/Data/DataSet.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/Data/DataInputRecord.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/DummyResponse.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>


#include <XSContainer.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <xsTypes.h>

#include <sstream>


int
XSGlobal::xsDummyrsp(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doDummyrsp(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doDummyrsp(const StringArray& rawArgs)
{
   const char* cmd = "dummyrsp";
   const size_t nArgs = rawArgs.size();
   if (nArgs == 2 && rawArgs[1] == "?")
   {
           printDocs(cmd,"?");
   }
   else
   {
      using XSContainer::datasets; 
      static bool s_isLog = true;
      static Real s_offset = .0;
      static const Real s_df_channelWidth = .0;

      // This is a non-const in order to use the resetEnergies much further below.
      DummyResponse* dummy = dynamic_cast<DummyResponse*>
      		(XSContainer::responses->responseList(DUMMY_RSP,1));
      if (!dummy)
      {
	 throw RedAlert("Cannot find dummy response in global container");
      }
      // Initially, these 3 parameters should share the same values as the 
      // DummyResponse singleton in the global ResponseContainer. 
      static int s_nE = dummy->numEnergies();
      const DummyResponse* cdummy = const_cast<const DummyResponse*>(dummy);
      static Real s_lowE = cdummy->energies()[0];
      static Real s_highE = cdummy->energies()[s_nE];

      IntegerArray iParams(0);
      StringArray xsParams(0);

      std::vector<string> args(nArgs-1,"");
      for (size_t j = 1; j < nArgs; ++j) args[j-1] = rawArgs[j];

      XSparse::collectParams(args, iParams, xsParams);
      int nP = iParams.size();

      const int nSpec (datasets->numberOfSpectra());
      const int nSources (datasets->numSourcesForSpectra());

      // Before making any changes to static parameters, or constructing
      // any objects, first validate ALL input parameters.
      bool allOk = true;
      bool logLinEntered = false;
      bool isLog = false;
      Real offset;
      Real channelWidth = s_df_channelWidth;
      string logLin;
      int nE;
      int specNum = 0, sourceNum = 0;
      Real lowE = -1.;
      Real highE = -1.;
      // First test - check that each entered parameter is individually valid.
      for (int i=0; i<nP && allOk; ++i)
      {
         std::istringstream iss(xsParams[i]);
         switch (iParams[i])
	 {
	    case 0: // low energy
	       if (!(iss >> lowE) || !iss.eof() || lowE <.0)
	       {
	          allOk = false;
	       }
	       break;
	    case 1:  // high energy
	       if (!(iss >> highE) || !iss.eof() || highE <.0)
	       {
	          allOk = false;
	       }
	       break;
	    case 2:  // Num energy ranges
	       if (!(iss >> nE) || !iss.eof() || nE < 1)
	       {
	          allOk = false;
	       }
	       break;
	    case 3:  // log or linear
	       logLin = XSutility::lowerCase(xsParams[i]);
	       if (logLin != "log" && logLin != "lin" && logLin != "linear")
	       {
	          allOk = false;
	       }
	       else
	       {
	          logLinEntered = true;
		  isLog = (logLin == "log");
	       }
	       break;
	    case 4: // channel offset
	       if (!(iss >> offset) || !iss.eof() || offset < .0)
	       {
	          allOk = false;
	       }
	       break;
	    case 5: // channel width
	       if (!(iss >> channelWidth) || !iss.eof() || channelWidth < .0)
	       {
	          allOk = false;
	       }
	       break;
	    case 6: // Source num and Spectrum num (n:m)
	       if (!XSparse::integerPair(xsParams[i], sourceNum, specNum)
	       		|| specNum > nSpec || sourceNum < 0 || specNum < 0)
	       {
	          allOk = false;
	       }
	       break;
	    default:
	       break;
	 }
      } // end for loop - initial parameter checking

      if (allOk)
      {
         // 2nd test - check for conflicts between the entered parameters.
	 Real relevantLowE = (lowE < .0) ? s_lowE : lowE;
	 Real relevantHighE = (highE < .0) ? s_highE : highE;
	 bool relevantIsLog = logLinEntered ? isLog : s_isLog;
	 if ((relevantHighE <= relevantLowE) || (relevantIsLog && 
	 		!(relevantLowE > .0)))
	 {
	    allOk = false;
	 } 
      }

      if (allOk)
      {
         // All user input is now valid, proceed to update static params and
	 // build the UserDiagResp objects.
	 for (int i=0; i<nP; ++i)
	 {
	    switch (iParams[i])
	    {
	       case 0:  // low energy
	          s_lowE = lowE;
	          break;
	       case 1:  // high energy
	          s_highE = highE;
		  break;
	       case 2:  // num energy ranges
	          s_nE = nE;
		  break;
	       case 3:  // log or linear
	          s_isLog = isLog;
	          break;  
	       case 4:  // channel offset
	          s_offset = offset;
	          break;
	       case 5:  // channel width
	          break;
	       case 6:  // sourceNum:specNum - no static variables for this
	          break;
	       default:
	          break;
	    }
	 }
	 if (!sourceNum)
	 {
	    // Then, specNum must also = 0
	    if (!nSpec)
	    {
	       // There are no spectra currently loaded.  The user is
	       // then only modifying the default singleton dummy response
	       // that always resides in the response container.	       
	       dummy->resetEnergies(s_lowE, s_highE, s_nE, s_isLog);
	    }
	    else
	    {
	       for (size_t i=1; i<=(size_t)nSpec; ++i)
	       {
                  size_t row = 0;
                  DataSet* ds = datasets->dataSetLookup(i, row);
                  size_t dGroup = ds->dataGroup();
		  SpectralData* sd = ds->sourceData(row);
		  for (size_t j=0; j<(size_t)nSources; ++j)
		  {
                     UserDummyResponse *newUd = new UserDummyResponse(s_lowE, 
                                s_highE, s_nE, s_isLog, sd, s_offset,  
				channelWidth,j);
                     newUd->dataGroup(dGroup);
	             sd->attachUserDummy(newUd,j);
		  }
	       }
	    }
	 }
	 else
	 {
	    if (!specNum)
	    {
	       for (size_t i=1; i<=(size_t)nSpec; ++i)
	       {
                  size_t row = 0;
                  DataSet* ds = datasets->dataSetLookup(i, row);
                  size_t dGroup = ds->dataGroup();
		  SpectralData* sd = ds->sourceData(row);
                  UserDummyResponse *newUd = new UserDummyResponse(s_lowE, s_highE, 
	 			  s_nE,s_isLog, sd, s_offset, channelWidth,
				  (size_t)(sourceNum-1));
                  newUd->dataGroup(dGroup);
                  datasets->adjustNumSources(sourceNum);
	          sd->attachUserDummy(newUd, (size_t)(sourceNum-1));
	       }
	    }
	    else
	    {
               size_t row = 0;
               DataSet* ds = datasets->dataSetLookup(specNum, row);
               size_t dGroup = ds->dataGroup();
	       SpectralData* sd = ds->sourceData(row);
               UserDummyResponse *newUd = new UserDummyResponse(s_lowE, s_highE, s_nE,
	 			  s_isLog, sd, s_offset, channelWidth,
				  (size_t)(sourceNum-1));
               newUd->dataGroup(dGroup);
               datasets->adjustNumSources(sourceNum);
	       sd->attachUserDummy(newUd, (size_t)(sourceNum-1));
	    }
	 }
         datasets->determineDgSourceRelations();
	 datasets->Notify();
      }
      else
      {
         tcout << "\n***Error or incompatibility in one or more parameters" <<std::endl;
         return -1;
      }
   }
   return 0;
}
