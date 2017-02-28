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
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>


#include <XSContainer.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <sstream>

int
XSGlobal::xsResponse(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doResponse(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doResponse(const StringArray& rawArgs)
{
    using XSContainer::datasets;

    static bool sourceWarningIssued = false;    
    bool isChanged=false;
    string cmd("response");
    const size_t nArgs = rawArgs.size();
    if (nArgs == 1)
    {
       IntegerArray dummy;
       isChanged = datasets->resetDetectors(dummy);
       if (isChanged)
       {
	   tcout << "\nExisting dummy responses have been removed from all spectra."<<std::endl;
	   tcout << "Original responses (if any) have been restored."<<std::endl;	   
       }
    }
    else
    {
	if (nArgs == 2)
	{
            const string& firstArg = rawArgs[1];
	    int tmp1, tmp2;
	    if (firstArg == "?" || XSparse::integerPair(firstArg, tmp1, tmp2))
	    {
		printDocs(cmd.c_str(),"?");
		return 0;
	    }
	}
	StringArray argArray(rawArgs);
	DataUtility::recordList inputData;
	try
	{
	    inputData = DataUtility::parseDataCommandString(argArray,XSutility::RSP);
	}
	catch (YellowAlert&)
	{
	    return -1;
	}

	DataUtility::recordListIter recIter(inputData.begin());
	DataUtility::recordListIter last(inputData.end());

	while (recIter != last)
	{
	    string& rspName = recIter->fileName();
	    size_t sourceNum = recIter->groupNumber();
	    size_t specNum = recIter->spectrumNumber(0);

            int extVers = recIter->spectrumRange(0);
            if (extVers < 0)
            {
               tcout << "\n***Error: Response extension number " << extVers 
                  << " is not allowed.....skipped" << std::endl;
               ++recIter;
               continue; 
            }
            else if (extVers == 0)
               extVers = 1;
            else
            {
               // parseDataCommandString took away any curly braces from
               // rspName, we're going to put them back for the setResponse 
               // function.  This is the price we pay for reusing code that's 
               // truly meant for data command parsing.
               std::ostringstream oss;
               oss << rspName << '{' << extVers << '}';
               rspName = oss.str();
            }


	    size_t row(0);
            if (!sourceNum)
            {
               tcout << "\n***Error: Source number = 0 not allowed.....skipped" << std::endl;
               ++recIter;
               continue;
            }
	    if (DataSet* dSet=datasets->dataSetLookup(specNum, row))
	    {
		//check this for exception...?
		SpectralData* pSourceData = dSet->sourceData(row);

		if ( XSutility::lowerCase(rspName.substr(0,4)) == XSparse::NONE()) 
		{
                   if (sourceNum > datasets->numSourcesForSpectra())
                   {
		       tcout <<"\nXSPEC: Request to remove response for non-existing\n"
		          <<"       source ... skipped" <<std::endl;
                   }
                   else
                   {
		       pSourceData->removeResponses(sourceNum);
                       // if response is set to "none", make sure all
                       // corresponding arfChanged flags are turned off.
                       pSourceData->arfChanged(sourceNum-1, false);
		       isChanged = true;
                       datasets->adjustNumSources(0);
                       datasets->determineDgSourceRelations();
                   }
		}
		else 
		{
		    try 
		    {
                        datasets->adjustNumSources(sourceNum);
			if (dSet->setResponse(pSourceData, specNum, sourceNum, rspName, string("")))
			{
			    tcout << "Response successfully loaded." << std::endl;
			    isChanged = true;
                            datasets->determineDgSourceRelations();
                           if (sourceNum > 1 && !sourceWarningIssued)
                           {
                              // Issue warning if this spectrum has no response
                              // for source 1.  It's possible the user did something
                              // like "resp 2:2" when they meant "1:2" or "2".
                              SpectralData* sp = datasets->lookup(specNum);
                              if (!sp->responseLoaded(0))
                              {
                                 tcout << "***Warning: This assigns the response to SOURCE " << sourceNum <<" of"
                                     << "\n      spectrum " << specNum <<".  If you intended to assign to"
                                     << "\n      SOURCE 1 of spectrum "<< specNum<<", remove with \"resp "<<sourceNum
                                     <<":"<< specNum << " none\""
                                     << "\n      and just enter \"resp " << specNum
                                     << "\" (or \"resp 1:" << specNum << "\").\n" << std::endl;
                                 sourceWarningIssued = true;
                              }
                           } // end if sourceNum > 1 && !sourceWarningIssued
			}
		    }
		    catch (YellowAlert &)
		    {
                        datasets->adjustNumSources(0);
                        // Remove the rest of the input arguments
                        // and stop processing this loop.
                        while (recIter != inputData.end()) 
			{
			    recIter = inputData.erase(recIter); 
			}      
                        break; 
		    }                                       
		}

		//user entered sourceNum's are 1 based, hence the '-1'
		if(isChanged)
		    pSourceData->responseChanged(sourceNum - 1, true);
	    }
	    else
	    {
		tcout << "\nXSPEC: Request to modify response for spectra not\n";
		tcout << "       yet loaded ... skipped\n";
	    }
	    ++recIter;
	}
    }
    if (isChanged)
    {
	datasets->Notify();
    }	
    tcout << std::flush;
    return 0;
} 
