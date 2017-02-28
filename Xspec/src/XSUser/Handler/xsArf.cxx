//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>        
#include <XSUser/Global/XSGlobal.h>        
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>       
#include <XSModel/Data/DataSet.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/Data/DataInputRecord.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/MultiResponse.h>
#include <XSModel/Data/Detector/RealResponse.h>
#include <XSModel/Data/Detector/UserDummyResponse.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>


#include <XSContainer.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <xsTypes.h>

int
XSGlobal::xsArf(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doArf(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doArf(const StringArray& rawArgs)
{
   bool isAnyChanged=false;
   string cmd("arf");
   const size_t nArgs = rawArgs.size();
   if (nArgs == 1 || (nArgs == 2 && rawArgs[1] == "?"))
   {
      printDocs(cmd.c_str(),"?");
   }

   else
   {
      StringArray argArray(rawArgs);
      DataUtility::recordList inputData;
      try
      {
         inputData = DataUtility::parseDataCommandString(argArray, XSutility::ARF);
      }
      catch (YellowAlert&)
      {
         return -1;
      }

      DataUtility::recordListIter recIter(inputData.begin());
      DataUtility::recordListIter last(inputData.end());

      while (recIter != last)
      {
	 string &arfName = recIter->fileName();
         size_t sourceNum = recIter->groupNumber();
	 const IntegerArray &specNum = recIter->spectrumNumber();
	 const IntegerArray &rowNums = recIter->spectrumRange();
	 size_t numArfRows(rowNums.size());
	 size_t row(0);
	 size_t arfRow(0);
	 for (size_t i=0; i<numArfRows; ++i)
	 {
	    arfRow = rowNums[i];
	    if (DataSet* dSet = XSContainer::datasets->dataSetLookup(specNum[i], row))
	    {
	       SpectralData *spec = dSet->sourceData(row);
	       if (sourceNum <= static_cast<size_t>
                           (XSContainer::datasets->numSourcesForSpectra()))
	       {
		  Response* resp = spec->detector(sourceNum-1);
		  if (!resp)
		  {
		     tcout <<"\nXSPEC: Attempt to modify ARF for non-existing\n";
		     tcout <<"       source ... skipped\n";
		  }
		  else if (resp->toUserDummyResponse())
		  {
		     tcout <<"\nARF command not applicable to dummy/diag responses\n";
		  }
		  else if (resp->toMultiResponse())
		  {
		     tcout <<"\nARF command not yet implemented for multiple-RMF responses\n";		       
		  }
		  else if (RealResponse *realResp = dynamic_cast<RealResponse*>(resp))
		  {
		     if (XSutility::lowerCase(arfName.substr(0,4)) == XSparse::NONE())
		     {
			if (realResp->arfName().length())
			{
		           // An Arf exists and must be removed
			   realResp->setEffectiveArea(realResp->rmfData()->normFactor());
			   realResp->arfName("");
			   realResp->arfRow(0);
			   realResp->arfRunPath("");
                           // this does nothing unless gain has been called at some
                           // point on this response
                           realResp->setZeroGainEffectiveArea(realResp->rmfData()->normFactor());
                           if (realResp->getConstGain() || realResp->getLinearGain() 
                                        || realResp->isGainApplied())
                           {
                              realResp->removeGain();
                              tcout << "Warning: This operation has removed the associated response's gain."
                                 << std::endl;
                           }

		           isAnyChanged = true;
                           spec->arfChanged(sourceNum-1,true);
			}
		     }
		     else
		     {
			string tmpName = realResp->arfName();
			size_t tmpRow = realResp->arfRow();
			realResp->arfName(arfName);
			try
			{
		           if (realResp->readAuxResponse(arfRow))
			   {
                              realResp->setZeroGainEffectiveArea(realResp->effectiveArea());
                              if (realResp->getConstGain() || realResp->getLinearGain() 
                                           || realResp->isGainApplied())
                              {
                                 realResp->removeGain();
                                 tcout << "Warning: This operation has removed the associated response's gain."
                                    << std::endl;
                              }

			      isAnyChanged = true;
                              spec->arfChanged(sourceNum-1, true);
			      realResp->arfRunPath(XSutility::getRunPath());
			      tcout << "Arf successfully loaded." << std::endl;
			   }
			   else
			   { // If readAuxResponse failed, effectiveArea
			     // array will be unchanged.  Only the arfName
			     // needs to be restored.
			      realResp->arfName(tmpName);
			      realResp->arfRow(tmpRow);
			   }
			}
			catch (YellowAlert &)
			{
		           realResp->arfName(tmpName);
			   realResp->arfRow(tmpRow);
                           // Remove the rest of the input arguments
                           // and stop processing this loop.
                           while (recIter != inputData.end()) 
			   {
			      recIter = inputData.erase(recIter); 
			   }      
                           break; 
			}
		     }
		  } // end if RealResponse
	       } // end if sourceNum exists
	       else
	       {
		  tcout <<"\nXSPEC: Request to modify ARF for non-existing\n";
		  tcout <<"       source ... skipped\n";
	       }
	    } // end if spectrum exists
	    else
	    {
	       tcout << "\nXSPEC: Request to modify ARF for spectra not\n";
	       tcout << "       yet loaded ... skipped\n";
	    }
	 } // end for i loop
	 ++recIter;
      }
      if (isAnyChanged)
	 XSContainer::datasets->Notify();
   }
   tcout << std::flush;
   return 0;
}
