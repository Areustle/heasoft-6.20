//
//  XSPEC12  November 2003
//
//
#include <XSUser/Global/Global.h>        
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>       
#include <XSFit/Fit/Fit.h>
#include <XSModel/Data/DataSet.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/Data/DataInputRecord.h>
#include <XSModel/GlobalContainer/DataContainer.h>
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
XSGlobal::xsBackgrnd(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doBackgrnd(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doBackgrnd(const StringArray& rawArgs)
{
        using namespace XSContainer;
        bool isChanged(false);
        const char* cmd = "backgrnd";
        const size_t nArgs = rawArgs.size();
        if (nArgs == 1 || (nArgs == 2 && rawArgs[1] == "?"))
        {
                printDocs(cmd,"?");
        }

        else
        {
           DataUtility::recordList inputData;
           try
           {
              // parseDataCommandString may change args, so need a copy of rawArgs vector.
              StringArray inArgs(rawArgs);
              inputData = DataUtility::parseDataCommandString(inArgs,XSutility::PHA);
	   }
           catch (YellowAlert&)
           {
              return -1;
           }
	   DataUtility::recordListIter recIter(inputData.begin());
	   DataUtility::recordListIter last(inputData.end());

           std::vector<SpectralData*> specProcessed;
	   while (recIter != last)
	   {
	      string &bckName = recIter->fileName();
	      const IntegerArray &specNum = recIter->spectrumNumber();
	      const IntegerArray &rowNums = recIter->spectrumRange();
	      size_t numBckRows(rowNums.size());
	      size_t row(0);
	      size_t bckRow(0);
	      for (size_t i=0; i<numBckRows; ++i)
	      {
	         bckRow = rowNums[i];
		 if (DataSet* dSet=XSContainer::datasets->dataSetLookup(specNum[i], row))
		 {
	            SpectralData *spec = dSet->sourceData(row);
		    if (XSutility::lowerCase(bckName.substr(0,4)) == XSparse::NONE())
		    {
		        if (spec->background())
		        {
		                // A background exists and must be removed
			        spec->background(0);
			        spec->backgroundFile("");
		                isChanged = true;
                                specProcessed.push_back(spec);
		        }
		    }
		    else
		    {
		       string tmpName = spec->backgroundFile();
		       // if the background filename is the same as the source filename
		       // then auto-look for the background extension
                       // (but don't this if it's a type-II file)
		       if ( bckName == dSet->dataName() && !bckRow) {
			 spec->backgroundFile((bckName+"[back]"));
		       } else {
			 spec->backgroundFile(bckName);
		       }

		       try
		       {
		          if (dSet->setBackgroundData(row, bckRow))
			  {
			     isChanged = true;
	                     specProcessed.push_back(spec);
		          }
			  else
			  { 
			     spec->backgroundFile(tmpName);
			  }
		       }
		       catch (YellowAlert &)
		       {
		          spec->backgroundFile(tmpName);
                          // Remove the rest of the input arguments
                          // and stop processing this loop.
                          while (recIter != inputData.end()) 
			  {
			        recIter = inputData.erase(recIter); 
			  }   
                          if (isChanged)
                             doUpdates(specProcessed);
                          tcout << std::flush;
                          return   -1; 
		       }
		    }
		    if(isChanged)
			spec->backgroundChanged(true);
		 } // end if spectrum exists
		 else
		 {
		    tcout << "XSPEC: Request to modify background for spectra not\n"
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
         ++s;
      }
      XSContainer::fit->Update();
   }
}
