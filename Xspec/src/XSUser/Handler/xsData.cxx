//
//  XSPEC12  November 2003
//
//

#include <XSstreams.h>
#include <XSContainer.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>     
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSModel/DataFactory/DataFactory.h>
#include <XSModel/DataFactory/XspecRegistry.h>
#include <XSModel/Data/DataSet.h>
#include <XSModel/Data/DataInputRecord.h>
#include <XSModel/Data/FakeDataInputRecord.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/DataSetTypes.h>
#include <XSModel/GlobalContainer/Memento.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSUtil/Signals/SignalHandler.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>

#include <algorithm>
#include <functional>
#include <list>
#include <utility>
#include <sstream>

using namespace XSContainer;

int
XSGlobal::xsData(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doData(rawArgs, true); 
   if (status == 0)
      return globalData->autoSave(TCL_OK, 0);
   else if (status == 1)
      return globalData->autoSave(TCL_OK);
   else if (status == 2)
      return TCL_OK;
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doData(const StringArray& rawArgs, const bool handleUndo)
{
    using namespace std;
    const char* cmd = "data";
    const size_t nArgs = rawArgs.size();
    int nSpectra (datasets->numberOfSpectra());
    int status = 0;

    //static Memento* dataMemento = 0;

    if ( nArgs == 1)
    {
	XSGlobal::printDocs(cmd,"?");
        status = 1;
	return status;
    }
    else if (nArgs >= 2)
    {
	string firstArg(rawArgs[1]);
        if (firstArg == "?")
        {
           XSGlobal::printDocs(cmd,"?");
           status = 1;
	   return status;
        }

        if (handleUndo)
        {
	   if(firstArg != "%MEMENTO")
	       globalData->memento(datasets->CreateMemento());
	   else
	   {
	       datasets->SetMemento(globalData->memento());
	       datasets->Notify();
	       datasets->emptyTrash();
	       globalData->memento(0);
	       //We don't want to write an autosave script here - at the
	       //moment undoing an undo operation is unimplemented.
	       //just save the autosave file.
	       XSGlobal::saveAll(globalData->autoSaveFile());
               status = 2;
	       return status;
	       //return globalData->autoSave(TCL_OK);
	   }
        }

	if(nArgs == 2)
	{
	    // if data none was typed, clear the data structure and return.
	    if ( XSutility::lowerCase(firstArg) == XSparse::NONE())
	    {
                datasets->dgHistory().initOldArray(datasets->numberOfGroups());
		datasets->clear();
		datasets->Notify();
		return status;
	    }

	    size_t numericArg  = XSutility::isInteger(firstArg);

	    // delete all datasets after the number supplied and return.

	    if (numericArg != XSparse::NOTFOUND() )
	    {
		if (numericArg >= static_cast<size_t>(nSpectra))
		{
		    tcerr << "XSPEC: Request to delete spectra not yet loaded ... skipped \n";
                    status = -1;
		    return status;      
		}
		else
		{
                    datasets->dgHistory().initOldArray(datasets->numberOfGroups());
		    datasets->deleteRange(numericArg+1,nSpectra);
		    datasets->Notify();
		    return status;
		}
	    }
	}
    }

    // pull out the case where we have a trailing slash. 
    // but, you can't modify rawArgs array...
    StringArray argArray(nArgs);
    for (size_t j = 0; j < nArgs; ++j)
    {
	argArray[j] = rawArgs[j];
	string arg = argArray[j];
	size_t len = arg.length();
	// Check that slash does not appear at the end of
	// any arg except the last.  Also check that slash
	// is not immediately followed by a comma anywhere.
        if (len)
        {
	   for (size_t i=0; i<len-1; ++i)
	   {
	       if (arg[i] == '/')
	       {
		   if (arg[i+1] == ',')
		   {
		       tcerr <<"***Error: Slash (/) can only be entered for the last entry"<<std::endl;
                       status = -1;
		       return status;
		   }

	       }
	   }
	   if (arg[len-1] == '/' && j != nArgs-1)
	   {
	       tcerr <<"***Error: Slash (/) can only be entered for the last entry"<<std::endl;
               status = -1;
	       return status;
	   }
        }
    }

    const string& testForPreserve = argArray[nArgs-1];

    bool preserve(testForPreserve[testForPreserve.length()-1] == '/');
    if (preserve)
    {
	argArray[nArgs - 1] = testForPreserve.substr(0,testForPreserve.length()-1);
    }

    DataUtility::recordList inputData;
    try
    {
	inputData = DataUtility::parseDataCommandString(argArray,XSutility::PHA);
    }
    catch (YellowAlert&)
    {
       status = -1;
       return status;
    }
    if (inputData.empty())
    {
       // This can happen if user entered a blank string in quotes.
       return status;
    }

    // mark datasets as preserved or deleted.
    // for those spectra with numbers higher than those deleted 
    // (e.g. with data <n> none), renumber.


    //std::vector<bool> markForDeletion(nSpectra,false);
    // conditions for *removal* of previously read spectra:
    // a) highest spectrum number requested in the data command
    //    is lower than the current number of spectra, and
    //    i) dataSetNames arguments ending in a '/' character
    //       preserve the datasets with following spectrum numbers.
    //    ii) dataSetNames arguments ending in one or more ',' characters
    //       preserve that number of spectra with following spectrum numbers.
    // b) a dataSetNames argument is "none" or "none/" .
    //    case (i) "none" - remove all datasets from the current spectrumNumber
    //                      to the end of the spectrumNumber array.
    //    case (ii) "none/" - remove the numbered spectrum and decrement the 
    //                        spectrumNumber of spectra with higher spectrumNumber
    //                        values by one.

    // first pass: correct spectrumNumbers and remove commas.
    // post-processing of dataSetNames arguments.
    // correct spectrumNumbers according to commas.          


    // try doing this in two passes. First, read the data from disk.
    // process the spectrumNumbers and data groups, and then 
    // add correct information to the DataContainer.

    size_t numSpectraRead = 0;
    SIGINT_Handler intHandler;
    SignalHandler* sigContainer = SignalHandler::instance();
    EventHandler* oldHandler = sigContainer->registerHandler(SIGINT, &intHandler);

    DataUtility::recordListIter record = inputData.begin();
    DataUtility::recordListIter dataEnd = inputData.end();

    // this first loop determines the total number of spectra to be read.
    while (record !=  dataEnd )
    {
	// got to fix commas before doing this.

	string& dsName = record->fileName();

	// any records after 'none' will be ignored
	if ( XSutility::lowerCase(dsName) == XSparse::NONE()) 
	{
	    ++record;
	    // Note that dataEnd is no longer valid after this,
	    // but we are exiting the loop so it's OK.
	    inputData.erase(record, dataEnd);
	    break;
	}

	DataPrototype* p = 0;
	try
	{
	    p = XSContainer::xsRegistry->returnPrototype(dsName);
	}
	catch (XspecRegistry::UnrecognizedFormat&)
	{
	    record = inputData.erase(record);    
	    continue;  
	}
	catch (XspecDataIO::CannotOpen&)
	{
	    string replacement("");

	    //returns true if a "terminate" string (none, /*) supplied
	    // by user.

	    try
	    {
		XSparse::getFileNameFromUser(dsName,replacement, XSutility::PHA);

		if (replacement.length() != 0)
		{
		    dsName = replacement;
		    p = XSContainer::xsRegistry->returnPrototype(dsName);
		}
		else 
		{
		    record = inputData.erase(record);
		    continue;
		}        

	    }
	    catch (XspecDataIO::CannotOpen&)
	    {
		record = inputData.erase(record);
		continue;
	    }
	    catch (XSparse::SkipThis&)
	    {
		record = inputData.erase(record);
		continue;                                
	    }
	    catch (XSparse::AbortLoop&)
	    {
		while (record != dataEnd) record = inputData.erase(record);       
		break;
	    }
            catch (YellowAlert&)
            {
               record = inputData.erase(record);
               continue;
            }

	}



	try
	{
	    std::auto_ptr<DataSet> dSet(p->MakeDataSet());
	    dSet->initialize(p,*record);
	    size_t group = record->groupNumber();
	    const IntegerArray& SN = record->spectrumNumber();
	    const IntegerArray& dataRow = record->spectrumRange();
	    size_t ns = SN.size();

	    dSet->dataGroup(group);
	    for (size_t j = 0; j < ns; ++j)
	    {
		dSet->setData(SN[j],dataRow[j]);
		if (SN[j] <= (int)datasets->numberOfSpectra())
		{
		    int n(0);
		    SpectralData* sd=datasets->lookup(SN[j]);
		    int pg=sd->plotGroup();
#ifndef STD_COUNT_DEFECT
		    n = std::count(datasets->plotGroupNums().begin(), 
				   datasets->plotGroupNums().end(), pg);
#else
		    std::count(datasets->plotGroupNums().begin(), 
			       datasets->plotGroupNums().end(), pg, n);
#endif
		    if ((n>1) && !dSet->sourceData(dataRow[j])->
			energiesEqual(sd))
		    {
			std::ostringstream os;
			os << record->fileName() << " skipped.\n"
			   << "Spectrum " << SN[j] << 
			    " is in a plot group with other spectra.\n"
			   << "It cannot be replaced with a spectrum with different"
			   <<" energy bins and/or channel numbers.\n";
			throw(XSparse::SkipThis(os.str()));
		    }
		}
		++numSpectraRead;
		if (numSpectraRead >= 5)
		{
		    // The backspace characters mess up the log file,
		    // so make sure log output is turned off.
                    tpout.setVerbose(10, 999);
		    std:: ostringstream os;
		    os << "Number of spectra read ..... " << numSpectraRead;                           
		    string backup(os.str().length(), '\b');
		    tcout << backup << os.str() << std::flush;
                    tpout.setVerbose();
		}
		if (intHandler.interrupted())
		{
		    dSet->closeSourceFiles();
		    string msg("\n***Data loading interrupted by user.");
		    msg += string("\n***All spectra loading from currently open file will be lost.\n");
		    throw XSparse::AbortLoop(msg);
		}
	    }
	    dSet->closeSourceFiles();

            // New spectra should be assigned the current default fit and test statistic.
            // Can't do this at the libXSModel level though, so must do it up
            // here in the handler.
            const string& currentStatName = 
                        fit->statManager()->defaultStat()->name();
            const string& currentTestStatName = 
                        fit->statManager()->defaultTestStat()->name();
            if (dSet->isMultiple())
            {
               SpectralDataMapConstIt itSpec = dSet->multiSpectralData().begin();
               SpectralDataMapConstIt itSpecEnd = dSet->multiSpectralData().end();
               while (itSpec != itSpecEnd)
               {
                  itSpec->second->statName(currentStatName);
                  itSpec->second->testStatName(currentTestStatName);
                  ++itSpec;
               }
            }
            else
            {
               dSet->spectralData()->statName(currentStatName);
               dSet->spectralData()->testStatName(currentTestStatName);
            }

	    record->data(dSet.release());                 
	    ++record;
	}
	catch (XSparse::AbortLoop)
	{
	    // /* was entered, remove the rest of the input arguments
	    // and stop processing this loop.
	    tcout << std::endl;
	    while (record != inputData.end()) record = inputData.erase(record);       
	    break;                                        
	}
	catch (YellowAlert&)
	{
	    // problem processing data: remove current iteration
	    // and continue.
	    record = inputData.erase(record);       
	    continue;                                        
	}


    }  // end record list loop

    sigContainer->registerHandler(SIGINT, oldHandler);


    DataUtility::fixSequence(inputData,nSpectra);

    for (DataUtility::recordListIter r = inputData.begin(); r != inputData.end(); ++r)
    {
	// diagnostic information for DataInputRecord.
	r->report(); 
    }

    std::vector<bool> marked = datasets->insertAndDelete(inputData,nSpectra,preserve);

    DataUtility::recordListIter dataStep = inputData.begin();

    datasets->dgHistory().initOldArray(datasets->numberOfGroups());
    while (dataStep != dataEnd )
    {
	// all successful, add processed data to global container                
	datasets->addToList(dataStep->data());
	++dataStep; 
    }                
    datasets->deleteRange(marked, preserve);

    // If any dummy responses existed anywhere, replace them.
    IntegerArray all(0);
    datasets->resetDetectors(all);

    dataStep = inputData.begin();        
    while (dataStep != dataEnd )
    {
	// Report and clean up the corresponding inputData record.
	dataStep->data()->reportAll();                                     
	dataStep = inputData.erase(dataStep);
    }
    datasets->Notify();

    return status;
}
