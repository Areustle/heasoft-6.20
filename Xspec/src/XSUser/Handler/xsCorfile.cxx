//
//  XSPEC12  November 2003
//
//
#include <XSUser/Global/Global.h>        
#include <XSUser/Global/XSGlobal.h>        
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/Data/DataSet.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/Data/DataInputRecord.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSFit/Fit/Fit.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>        
#include <XSstreams.h>
#include <xsTypes.h>

namespace
{
   void doUpdates(const std::vector<SpectralData*>& specsChanged);
}

int
XSGlobal::xsCorfile(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doCorfile(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doCorfile(const StringArray& rawArgs)
{
   using namespace XSContainer;
   bool isChanged=false;
   const char* cmd = "corfile";
   const size_t nArgs = rawArgs.size();
   if (nArgs == 1 || (nArgs == 2 && rawArgs[1] == "?"))
   {
      printDocs(cmd,"?");
   }

   else
   {
      StringArray argArray(nArgs);
      for (size_t j = 0; j < nArgs; ++j)
      {
         argArray[j] = rawArgs[j];
      }
      DataUtility::recordList inputData;
      try
      {
         inputData = DataUtility::parseDataCommandString(argArray,XSutility::COR);
      }
      catch (YellowAlert&)
      {
         return -1;
      }

      DataUtility::recordListIter recIter(inputData.begin());
      DataUtility::recordListIter last(inputData.end());

      // store processed pointers to be able to update counts after the fact.
      std::vector<SpectralData*> specProcessed;
      while (recIter != last)
      {
	 string &corrName = recIter->fileName();
	 const IntegerArray &specNum = recIter->spectrumNumber();
	 const IntegerArray &rowNums = recIter->spectrumRange();
	 size_t numCorrRows(rowNums.size());
	 size_t row(0);
	 size_t corrRow(0);
	 for (size_t i=0; i<numCorrRows; ++i)
	 {
	    corrRow = rowNums[i];
	    if (DataSet* dSet=datasets->dataSetLookup(specNum[i], row))
	    {
	       SpectralData *spec = dSet->sourceData(row);
	       if (XSutility::lowerCase(corrName.substr(0,4)) == XSparse::NONE())
	       {
		  if (spec->correction())
		  {
		     // A correction obj exists and must be removed
		     spec->correction(0);
		     spec->correctionFile("");
		     isChanged = true;
                     specProcessed.push_back(spec);
		  }
	       }
	       else
	       {
		  string tmpName = spec->correctionFile();
		  spec->correctionFile(corrName);
		  try
		  {
		     if (dSet->setCorrectionData(row, corrRow))
		     {
			isChanged = true;
	                specProcessed.push_back(spec);
		     }
		     else
		     { 
			spec->correctionFile(tmpName);
		     }
		  }
		  catch (YellowAlert &)
		  {
		     spec->correctionFile(tmpName);
                     // Remove the rest of the input arguments
                     // and stop processing this loop.
                     while (recIter != inputData.end()) 
		     {
			recIter = inputData.erase(recIter); 
		     }   
                     if (isChanged) 
                        doUpdates(specProcessed);
                     tcout << std::flush;
                     return -1;
                     break; 
		  }
	       }
	       if(isChanged)
		   spec->correctionChanged(true);
	    } // end if spectrum exists
	    else
	    {
	       tcout << "XSPEC: Request to modify correction for spectra not\n"
		     << "       yet loaded ... skipped" << std::endl;
	    }
	 } // end for i loop
	 ++recIter;
      }
      if (isChanged)
      {
         doUpdates(specProcessed);
      }	
   }
   tcout << std::flush;
   return 0;
}

namespace
{
   void doUpdates(const std::vector<SpectralData*>& specsChanged)
   {
      std::vector<SpectralData*>::const_iterator s (specsChanged.begin());
      std::vector<SpectralData*>::const_iterator sEnd (specsChanged.end ());
      while ( s != sEnd)
      {
         (*s)->computeTotals();
         (*s)->reportRates();
         // May have gotten here because a Correction was removed.
         // Hence the test below.
         if ((*s)->correction())
         {
            const RealArray& corrSpec = (*s)->correction()->spectrum();
            tcout << "Net correction flux: " << corrSpec.sum() << std::endl;
         }
         tcout << std::endl;
         ++s;
      }
      XSContainer::fit->Update();
   }
}
