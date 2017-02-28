//
//  XSPEC12  November 2003
//
//

#include <CCfits/CCfits>        
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h> 
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/DataFactory/DataFactory.h>
#include <XSModel/DataFactory/XspecRegistry.h>
#include <XSModel/DataFactory/OGIP-92aIO.h>
#include <XSModel/Data/DataSet.h>
#include <XSModel/Data/DataInputRecord.h>
#include <XSModel/Data/FakeDataInputRecord.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Error/Error.h>       
#include <XSstreams.h>
#include <XSContainer.h>
#include <XSsymbol.h>

#include <set>

using namespace XSContainer;

class FakeitError : public YellowAlert
{
   public:
      FakeitError(const string& msg);
};
FakeitError::FakeitError(const string& msg)
   : YellowAlert("Fakeit Error: ")
{
   tcerr << msg << std::endl;
}

class FakeitHandler
{
   public: 
      typedef FakeDataInputRecord::BackLocator BackLocator;

      FakeitHandler(const StringArray& rawArgs, std::vector<DataSet*>& fakeDataSets);
      ~FakeitHandler() {}
      bool perform();

   private:
      FakeitHandler(const FakeitHandler&);
      void operator=(const FakeitHandler&);
      void collateArgs();
      void determineNSpectra(const StringArray& argArray, const string& lastArg);
      void determineDataSetFormats();
      void earlyResponsePrompt();
      void getAuxFileInfo();
      void oneTimePrompts();
      void getAdditionalFDataSetInfo();
      static Real getBackTimeFromFile(const BackLocator& bckLoc);
      void constructFakeDataSets();
      // This checks for cmdStr beginning with case-insensitive "nowrite".
      //  Returns true if found, and removes it along with leading and trailing
      //  whitespace (and a trailing single comma if it exists).
      static bool checkForNoWrite(string& cmdStr);

      size_t m_nOrigSpectra;
      size_t m_nOrigDataSets;
      size_t m_nSpectra;
      size_t m_nDataSets;
      bool m_doWrite;
      bool m_type2Flag;
      bool m_isModular;
      bool m_useCountStat;
      string m_prefix;
      const StringArray m_inArgs;
      std::deque<string> m_batchPrompts;
      DataUtility::recordList m_inputData;
      string m_earlyResponseName;
      BackLocator m_earlyArfLoc;
      std::vector<FakeDataInputRecord> m_fDataRecs;
      DataPrototype* m_prototype;
      std::vector<DataSet*>& m_fakeDataSets;
};

void XSGlobal::fakeitDataSetOrder(std::vector<DataSet*>& origDataSets)
{
   // The supplied origDataSets vector will be filled with pointers to
   // the pre-existing DataSets, in the order for which they are to be
   // processed.

   // This is necessitated entirely by the fact that the spectra within a 
   // type2 DataSet may be NON-CONSECUTIVELY numbered.  Since certain fakeit
   // parameters apply to DataSets and not spectra, (ie. file names), it
   // must prompt by set rather than spectrum number.  The ordering is
   // determined by the lowest spectrum number within each set.

   origDataSets.clear();
   const size_t nOrigSets = datasets->dataArray().size();
   const size_t nOrigSpec = datasets->numberOfSpectra();
   size_t nProcessed=0;
   std::set<size_t> origSpecNumsRemaining;
   for (size_t i=1; i<=nOrigSpec; ++i)
      origSpecNumsRemaining.insert(i);
   while (nProcessed < nOrigSets)
   {
      // Find the DataSet which contains the lowest remaining spectrum
      // number.
      const size_t lowestSpec = *origSpecNumsRemaining.begin();
      size_t row=0;
      DataSet* origDset = datasets->dataSetLookup(lowestSpec, row);
      IntegerArray rowNums;
      if (!origDset->specNumOrder(rowNums))
      {
	 tcout << "\n***Warning:  Gaps exist in the spectrum numbers currently";
	 tcout << "\n             assigned to the spectra in file ";
	 tcout << "\n" <<  origDset->dataName();
	 tcout << "\nFake spectra will be processed in the order of the spectra";
	 tcout <<"\nin this file, not in the order of increasing spectrum number."<<std::endl;
      }
      const size_t nSpec = rowNums.size();
      for (size_t i=0; i<nSpec; ++i)
      {
         const SpectralData* origSd = origDset->sourceData(rowNums[i]);
         origSpecNumsRemaining.erase(origSd->spectrumNumber());
      }
      origDataSets.push_back(origDset);	    
      ++nProcessed;
   }
}

int
XSGlobal::xsFakeit(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doFakeit(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doFakeit(const StringArray& rawArgs)
{
   const size_t nArgs = rawArgs.size();
   const char* cmd = "fakeit";
   if (nArgs == 2 && rawArgs[1] == "?")
   {
      printDocs(cmd,"?");
   }
   else
   {
      // If no models are active, there is no reason to proceed.
      IntegerArray activeSources;
      models->explActiveSources(activeSources);
      if (!activeSources.size())
      {
         tcout << "\n***Error: Cannot create fake data.  There are no active models." 
                <<std::endl;
         return -1;
      }
      std::vector<DataSet*> dataSetHooks(0);
      bool doWriteOutputFiles = true;
      try
      {
         doWriteOutputFiles = FakeitHandler(rawArgs, dataSetHooks).perform();
      }
      catch (YellowAlert&)
      {
         for (size_t i=0; i<dataSetHooks.size(); ++i)
         {
            delete dataSetHooks[i];
         }
         return -1;
      }
      // If we reached here, assume all fake data sets are in condition
      // to be generated.  Any pre-existing data sets must be cleared
      // out at this point.
      datasets->dgHistory().initOldArray(datasets->numberOfGroups());
      datasets->clear();
      for (size_t i=0; i<dataSetHooks.size(); ++i)
      {
         datasets->addToList(dataSetHooks[i]);
      }
      // This is a patch hack.  Want to really just get at datasets'
      // enumerateGroups function, but it is private.  So get
      // at through the deleteRange function as in xsData, but
      // don't actually delete anything.
      // This must be at least size 1 inside deleteRange:
      std::vector<bool> dummy(1, false); 
      datasets->deleteRange(dummy, false);

      datasets->Notify();
      models->fold();
      for (size_t i=0; i<dataSetHooks.size(); ++i)
      {
         DataSet* fakeDs = dataSetHooks[i];
         models->fillFakeData(fakeDs);
         fakeDs->generateFake();
         if (doWriteOutputFiles)
            fakeDs->outputData();
      }
      models->Notify();       	 
   } 
   return 0;
}

FakeitHandler::FakeitHandler(const StringArray& rawArgs, std::vector<DataSet*>& fakeDataSets)
   : m_nOrigSpectra(datasets->numberOfSpectra()),
     m_nOrigDataSets(datasets->dataArray().size()),
     m_nSpectra(0),
     m_nDataSets(0),
     m_doWrite(true),
     m_type2Flag(false),
     m_isModular(false),
     m_useCountStat(true),
     m_prefix(),
     m_inArgs(rawArgs),
     m_batchPrompts(),
     m_inputData(),
     m_earlyResponseName(),
     m_earlyArfLoc(),
     m_fDataRecs(),
     m_prototype(0),
     m_fakeDataSets(fakeDataSets)
{
}

bool FakeitHandler::perform()
{
   collateArgs();
   determineDataSetFormats();
   getAuxFileInfo();
   oneTimePrompts();
   getAdditionalFDataSetInfo();
   constructFakeDataSets();
   return m_doWrite;
}

void FakeitHandler::collateArgs()
{
   string cmdLine;
   string cmdStr;
   const size_t nArgs = m_inArgs.size();
   if (nArgs > 1)
   {
      // check for parameters from batch file.
      for (size_t j=1; j<nArgs; ++j)
      {
         cmdLine += m_inArgs[j];
         cmdLine += " ";
      }
      XSparse::findBatchString(cmdLine, cmdStr, m_batchPrompts);
   }
   m_doWrite = !checkForNoWrite(cmdStr);
    
   StringArray argArray;     
   argArray.push_back(m_inArgs[0]);
   while (cmdStr.length())
   {
      string cmd = XSparse::returnDelimitedArgument(cmdStr, string(" "));
      if (cmd.length())
      {
         argArray.push_back(cmd);
      }
   }

   // For case of 'fakeit' with no arguments, the inputData list will be empty.
   // Call to parseDataCommandString below can modify argArray.
   // If the last arg is a bare integer, we'll need to know that.
   string saveLastArg = argArray[argArray.size()-1];
   m_inputData = DataUtility::parseDataCommandString(argArray,XSutility::PHA);
   DataUtility::fixOverlap(m_inputData);
   determineNSpectra(argArray, saveLastArg);      
}

void FakeitHandler::determineNSpectra(const StringArray& argArray, const string& lastArg)
{

   size_t tmp;
   if (argArray.size() == 1)
   {
      m_nSpectra = m_nOrigSpectra;
   }
   else if ((tmp = XSutility::isInteger(lastArg)) != std::string::npos)
   {
      // This is the case where the last input argument is simply an integer,
      // which specifies the number of fake spectra to make.  A bare 
      // integer will not appear in the inputData list.
      m_nSpectra = tmp;
   }
   else if (m_inputData.empty())
   {
      throw FakeitError("Improper input to fakeit");
   }
   else
   {
      m_nSpectra = m_inputData.back().spectrumNumber().back();
   }

   if (!m_nSpectra)  
   {
      // Zero spectra requested, simply exit.
      throw YellowAlert();
   }

}

void FakeitHandler::determineDataSetFormats()
{
   // By the end of this function, WILL KNOW: m_type2Flag (if needed),
   // m_nDataSets, m_isModular, m_prototype.  MAY KNOW:  first response/arf.
   if (m_nSpectra <= m_nOrigSpectra)
   {
      // All fake data will be based on existing data, and ALL 
      // existing data will be replaced by fake data.
      m_nSpectra = m_nOrigSpectra;
      m_nDataSets = m_nOrigDataSets;
      // ASSUMPTION: All data sets are based on the same prototype.
      // Therefore, if first existing set is modular, then all are.
      // If we're here, at least 1 data set must exist, so...
      const DataSet* origDset = datasets->dataArray().begin()->second;
      m_prototype = XSContainer::xsRegistry->returnPrototype(typeid(*origDset));
      m_isModular = origDset->isModular();
      // isModular now known, type2Flag not relevant here since new sets 
      // will just be same type as orig sets.
   }
   else
   {
      // Fake data will be created in addition to replacing whatever
      // data is currently loaded.
      if (m_nOrigSpectra)
      {
         DataArrayConstIt dsIt = datasets->dataArray().begin();
         DataArrayConstIt dsItEnd = datasets->dataArray().end();
         const DataSet* origDset = dsIt->second;
      // ASSUMPTION: Prototype and hence isModular is same for all.
         m_prototype = XSContainer::xsRegistry->returnPrototype(typeid(*origDset));
         m_isModular = origDset->isModular();
         while (dsIt != dsItEnd)
         {
	    // If any loaded data is Type 2, then make the extra
	    // fake data Type 2.
	    if (dsIt->second->isMultiple())
	    {
	       m_type2Flag = true;
	       break;
	    }
	    ++dsIt;
         }
         m_nDataSets = m_nOrigDataSets + (m_type2Flag ? 1 : m_nSpectra-m_nOrigSpectra);
         // type2Flag and isModular now known.
      }
      else
      {
         // No loaded data with which to base Type of output.  Instead,
         // check all input records and look for any evidence of Type2 
         // background files.  If any, then additional data will be
         // placed in 1 Type2 file.  However, if no evidence of Type2,
         // then we are still undetermined and final test will come
         // from response prompting. (ie. a SPI-type response will tell 
         // us to use type2)
         DataUtility::recordListConstIter recIter(m_inputData.begin());
         const DataUtility::recordListConstIter last(m_inputData.end());
         while (recIter != last)
         {
	    if (recIter->spectrumRange(0) != 0)
	    {
	       m_type2Flag = true;
	       m_nDataSets = 1;
               // isModular not known.
	       break;
	    }
	    ++recIter;
         }
	 // OK, from the user input we still don't know if new data
         // sets are modular format, and perhaps not even type I
         // vs type II.  So, jump ahead and do the first response 
         // prompt now.
         earlyResponsePrompt();
      }
   }
}

void FakeitHandler::earlyResponsePrompt()
{
   // To be used when there is no original data loaded from which to 
   // determine isModular, and perhaps m_type2Flag as well.
   XSparse::promptResponseArf(m_earlyResponseName, DUMMY_RSP, m_earlyArfLoc.first, 
         m_earlyArfLoc.second, 1, false, true, &m_batchPrompts);
   if (XSutility::lowerCase(m_earlyArfLoc.first) == XSparse::NONE()) 
        m_earlyArfLoc.first = string("");
   m_prototype = DataUtility::prototypeFromResponse(m_earlyResponseName);
   // This will just be used briefly, hence no auto ptr.
   DataSet *tmpDset = m_prototype->MakeDataSet();
   // if type2Flag has already been determined to be true, keep it that way.
   if (!m_type2Flag)  m_type2Flag = tmpDset->isMultiple();
   m_isModular = tmpDset->isModular();
   m_nDataSets = m_type2Flag ? 1 : m_nSpectra;
   delete tmpDset;
}

void FakeitHandler::getAuxFileInfo()
{
   // Aside from getting aux file info, function also creates all 
   // fDataInputRecs, and fills in whatever it can from original data.
   if (m_nOrigDataSets)
   {
      std::vector<DataSet*> origDataSets;
      XSGlobal::fakeitDataSetOrder(origDataSets);

      // First process spectra based on original data.
      for (size_t iSet=0; iSet<origDataSets.size(); ++iSet)
      {
	 DataSet* origDset = origDataSets[iSet];
	 IntegerArray rowNums;
	 origDset->specNumOrder(rowNums);
	 size_t nSpec = rowNums.size();
	 FakeDataInputRecord fRec(nSpec);
         // The following parameter is for the benefit of 
         // OGIP_92aData's initializeFake.  For data based on existing 
         // sets, set it to the max num of sources in the originally 
         // loaded data, otherwise just set to 1.
         fRec.numSourcesForSpectra(datasets->numSourcesForSpectra());
	 fRec.setOrigRowNums(rowNums);
	 fRec.groupNumber(origDset->dataGroup());
	 fRec.isType2(origDset->isMultiple());
	 fRec.data(origDset);
         DataUtility::recordListConstIter recIter(m_inputData.begin());
         for (size_t i=0; i<nSpec; ++i)
         {
	    const SpectralData* origSd = origDset->sourceData(rowNums[i]);
	    size_t specNum = origSd->spectrumNumber();         
	    // Fill FakeDataRecord with info 
	    fRec.spectrumNumber(i, specNum);
	    // This may be changed later, but for now just number rows consecutively
	    // from 1.
	    fRec.spectrumRange(i, i+1);

	    size_t index=0;
            FakeDataInputRecord::Detectors responseNames;
	    FakeDataInputRecord::Arfs arfLocs;
	    // Did the user enter a background file or "none" on the 
	    // command line for this particular spectrum?  The following
	    // function may increment recIter.
	    if (DataUtility::searchRecordListSpecNums(m_inputData, specNum, recIter, index))
	    {
	       if (XSutility::lowerCase(recIter->fileName()) == XSparse::NONE())
	       {
                  fRec.enteredNone(true);
	          fRec.inputBackgrounds(i, BackLocator("",0));
	       }
	       else
	       {
	          responseNames =  origDset->getResponseName(rowNums[i]);
	          arfLocs = origDset->getAncillaryLocation(rowNums[i],responseNames);
	          BackLocator bckLoc(recIter->fileName(),
			          recIter->spectrumRange(index));
	          fRec.inputBackgrounds(i, bckLoc);
	       }
	    }
	    else
	    {
               // No command line arg for this spectrum.  Take everything
               // from the original spectrum.
	       responseNames =  origDset->getResponseName(rowNums[i]); 
	       arfLocs = origDset->getAncillaryLocation(rowNums[i],responseNames);
	       fRec.inputBackgrounds(i, 
		          origDset->getBackCorrLocation(rowNums[i]));
               // While we're here, lets get the default background exposure
               // time from the original background spectrum (if it exists).
               // This will save us a read from file later on in
               // getBackTimeFromFile.
               if (fRec.inputBackgrounds(i).first.length())
               {
                  fRec.backExposureTime(origSd->background()->data()->exposureTime());
               }
	    }
            fRec.inputCorrFiles(i, 
                          origDset->getBackCorrLocation(rowNums[i],true));

	    if (!responseNames.size()) 
	    {
              // Can get here if the user typed "none" for this
              // spectrum, or if it had no response to begin with.
               FakeDataInputRecord::ResponseID respID;
               FakeDataInputRecord::ArfID arfID;
               if (m_isModular)
               {
                  // Do not prompt for every spectrum, only the first
                  // in the set.  Also, no need to prompt for Arf.
                  if (i == 0)
                  {
		     XSparse::promptResponseArf(respID.first, DUMMY_RSP,arfID.first.first, 
		     		   arfID.first.second, specNum, true, false, &m_batchPrompts);
                  }
               }
               else
               {
	          XSparse::promptResponseArf(respID.first, DUMMY_RSP, arfID.first.first, 
		     		   arfID.first.second, specNum, true, true, &m_batchPrompts);
               }
               // Will just have to assume this is for source 1
               respID.second = 0;
               arfID.second = 0;
               if (XSutility::lowerCase(arfID.first.first) == XSparse::NONE()) 
                           arfID.first.first = string("");
               fRec.inputResponses(i, FakeDataInputRecord::Detectors(1,respID));
               fRec.inputArfs(i, FakeDataInputRecord::Arfs(1,arfID));
	    }
            else
            {
	       fRec.inputResponses(i, responseNames);
	       fRec.inputArfs(i, arfLocs);
            }

         }  // end spectrum loop
         m_fDataRecs.push_back(fRec);
      }
   }  // end if original data sets

   if (m_nDataSets > m_nOrigDataSets)
   {
      // In current implementation, responses and arfs which come
      // from prompts (which is everything in this section) can 
      // only be assigned to source 1.  If this constraint is lifted,
      // be sure to also modify the DataSet initializeFake functions.
      // (This assumption is also used in getAdditionalFDataSetInfo)
      size_t specNum = m_nOrigSpectra + 1;
      size_t index = 0;
      FakeDataInputRecord::Detectors respIDs(1);
      FakeDataInputRecord::Arfs arfIDs(1);
      FakeDataInputRecord::ResponseID& respID0 = respIDs[0];
      FakeDataInputRecord::ArfID& arfID0 = arfIDs[0];
      respID0.second = 0;
      arfID0.second = 0;
      if (m_type2Flag)
      {
         // Make just 1 type 2 data set containing all extra spectra.
         size_t nSpec = m_nSpectra-m_nOrigSpectra;
         FakeDataInputRecord fRec(nSpec);
         fRec.numSourcesForSpectra(1); // See note in original data section.
         fRec.isType2(true);
         fRec.groupNumber(1);
         fRec.data(0);
         fRec.statName(fit->statManager()->defaultStat()->name());
         fRec.testStatName(fit->statManager()->defaultTestStat()->name());
         DataUtility::recordListConstIter recIter = m_inputData.begin();
         for (size_t i=0; i<nSpec; ++i)
         {
            fRec.spectrumNumber(i, specNum);
            fRec.spectrumRange(i, i+1);
            if (i==0)
            {
               if (m_earlyResponseName.length())
               {
                  respID0.first = m_earlyResponseName;
	          fRec.inputResponses(0, respIDs);
                  arfID0.first = m_earlyArfLoc;
	          fRec.inputArfs(0, arfIDs);
               }
               else
               {
	          XSparse::promptResponseArf(respID0.first, DUMMY_RSP, arfID0.first.first, 
		  	     arfID0.first.second, specNum, false, !m_isModular, 
                             &m_batchPrompts);
	          fRec.inputResponses(0, respIDs);
                  if (XSutility::lowerCase(arfID0.first.first) == XSparse::NONE()) 
                        arfID0.first.first = string("");
	          fRec.inputArfs(0, arfIDs);
               }
	       if (DataUtility::searchRecordListSpecNums(m_inputData,
	             specNum, recIter, index))
	       {
	          // For this case 'none' is meaningless, simply skip it.
	          if (XSutility::lowerCase(recIter->fileName()) != XSparse::NONE())
	          {
	             BackLocator bckLoc(recIter->fileName(),
			             recIter->spectrumRange(index));
	             fRec.inputBackgrounds(i, bckLoc);		        
	          }
	       }
            } // end if i == 0
	    else if (!m_isModular)
	    {

	       XSparse::promptResponseArf(respID0.first, DUMMY_RSP, arfID0.first.first, 
		  	     arfID0.first.second, specNum, false, true, &m_batchPrompts);
	       fRec.inputResponses(i, respIDs);
               if (XSutility::lowerCase(arfID0.first.first) == XSparse::NONE()) 
                        arfID0.first.first = string("");
	       fRec.inputArfs(i, arfIDs);
	       if (DataUtility::searchRecordListSpecNums(m_inputData,
		     specNum, recIter, index))
	       {
		  // For this case 'none' is meaningless, simply skip it.
		  if (XSutility::lowerCase(recIter->fileName()) != XSparse::NONE())
		  {
		     BackLocator bckLoc(recIter->fileName(),
				     recIter->spectrumRange(index));
		     fRec.inputBackgrounds(i, bckLoc);		        
		  }
	       }
            }
	    ++specNum;	  
         } // end spectra loop
         m_fDataRecs.push_back(fRec);
      }  // end if type2Flag
      else // Extra data sets are type 1.  
      {
         DataUtility::recordListConstIter recIter = m_inputData.begin();
         for (size_t i=0; i<(m_nDataSets-m_nOrigDataSets); ++i)
         {
	    // Make type 1 data sets for each spectrum
	    FakeDataInputRecord fRec(1);
	    fRec.isType2(false);
            fRec.numSourcesForSpectra(1); // See note in original data section.
	    fRec.groupNumber(1);
            fRec.data(0);
	    fRec.spectrumNumber(0, specNum);
	    fRec.spectrumRange(0,0);
            fRec.statName(fit->statManager()->defaultStat()->name());
            fRec.testStatName(fit->statManager()->defaultTestStat()->name());
	    if (i==0 && m_earlyResponseName.length())
	    {
               respID0.first = m_earlyResponseName;
	       fRec.inputResponses(0, respIDs);
               arfID0.first = m_earlyArfLoc;
	       fRec.inputArfs(0, arfIDs);
	    }
	    else
	    {
	       XSparse::promptResponseArf(respID0.first, DUMMY_RSP, arfID0.first.first, 
		  	     arfID0.first.second, specNum, false, true, &m_batchPrompts);
	       fRec.inputResponses(0, respIDs);
               if (XSutility::lowerCase(arfID0.first.first) == XSparse::NONE()) 
                        arfID0.first.first = string("");
	       fRec.inputArfs(0, arfIDs);
	    }
	    if (DataUtility::searchRecordListSpecNums(m_inputData,
	          specNum, recIter, index))
	    {
	       // For this case 'none' is meaningless, simply skip it.
	       if (XSutility::lowerCase(recIter->fileName()) != XSparse::NONE())
	       {
	          BackLocator bckLoc(recIter->fileName(),
			          recIter->spectrumRange(index));
	          fRec.inputBackgrounds(0, bckLoc);
	       }
	    }
            ++specNum;
            m_fDataRecs.push_back(fRec);
         } // end spectra loop
      }  // end type I
   } // end if nDataSets > nOrigDataSets
} // end getAuxFileInfo

void FakeitHandler::oneTimePrompts()
{
   // Helper function for user input that should be prompted for
   // once and only once per each fakeit command.  
   while (1)
   {
      string userInput("");
      XSparse::basicPrompt(" Use counting statistics in creating fake data? (y): ",
	 		     userInput, &m_batchPrompts);
      userInput = XSutility::lowerCase(userInput);
      if (userInput.length())
      {
         if (userInput[0] == 'n')
         {
	    m_useCountStat = false;
	    break;
         }
         else if (userInput[0] == 'y' || userInput[0] == '/')
         {
	    m_useCountStat = true;
	    break;
         }
      }
      else
      {
         m_useCountStat = true;
         break;
      }
   }
   DataSet::useFakeCountingStat(m_useCountStat);

   XSparse::basicPrompt(" Input optional fake file prefix: ", m_prefix, &m_batchPrompts);
   if (m_prefix == "/")  m_prefix = "";
} // end oneTimePrompts

void FakeitHandler::getAdditionalFDataSetInfo()
{
   if (m_nDataSets != m_fDataRecs.size())
   {
      throw RedAlert("Internal fakeit error: data sets vs. data records size mismatch.");
   }
   string defOutFile("");
   Real defExpTime=1.0;
   Real defCorScale=1.0;
   Real defBckExpTime=1.0;
   const string defExt(".fak");
   const string bkgExt("_bkg");
   const string expCorPrompt(" Exposure time, correction norm, bkg exposure time");
   for (size_t i=0; i<m_nDataSets; ++i)
   {
      FakeDataInputRecord& fRec = m_fDataRecs[i];
      fRec.useCountingStat(m_useCountStat);
      size_t nSpec = fRec.inputBackgrounds().size();
      if (i < m_nOrigDataSets)
      {
         const DataSet* origDset = fRec.data();
         if (!origDset)
         {
            throw RedAlert("Internal fakeit error: missing data set pointer.");
         }
         // dataName is what the user entered from the data command.  It could
         // contain a leading relative or absolute path.
         string::size_type slashLoc = origDset->dataName().rfind('/');
         if (slashLoc != string::npos)
         {
            defOutFile = origDset->dataName();
            defOutFile.insert(slashLoc+1, m_prefix);
         }
         else 
            defOutFile = m_prefix + origDset->dataName();
         // Get default exposure and corr scaling from only the
         // first spectrum (irrel. if this is type 1).  For fake
         // data, all spectra will have the same exposure and
         // corrScale.  There won't be prompting for each individual
         // spectrum.
         if (!fRec.enteredNone())
         {
            size_t firstSpecNum = static_cast<size_t>(fRec.origRowNums(0));
            const SpectralData* origSd = origDset->sourceData(firstSpecNum);
            defExpTime = origSd->exposureTime();
            defCorScale = origSd->correctionScale();
            // If using background from original data rather than command-line
            // arg, getAuxFileInfo should already have placed its exposure
            // time in fRec, as indicated by a non-negative value.  This will 
            // spare us from having to do a read from disk later on in 
            // getBackTimeFromFile.
            if (fRec.backExposureTime() < .0)
            {
               Real tmpBckExpTime = 
                  getBackTimeFromFile(fRec.inputBackgrounds(0));
               if (tmpBckExpTime >= .0)
                  defBckExpTime = tmpBckExpTime;
            }
            else
               defBckExpTime = fRec.backExposureTime();
         }

      }
      else
      {
         // Not based on original data.  Check for background file
         // for any of the spectra.  If any, use first background
         // file's exposure time as the default for the whole set.
         // ASSUME resp must only be in source 1 if not based on 
         // original data.
         defOutFile = (fRec.inputResponses(0)[0].first == DUMMY_RSP) ?
                  m_prefix + "dummy_rsp" : m_prefix + fRec.inputResponses(0)[0].first;
         size_t nAttempts = m_isModular ? 1: nSpec;
         Real tmpBckExpTime = -1.0;
         for (size_t j=0; j<nAttempts && tmpBckExpTime<.0; ++j)
         {
            tmpBckExpTime = getBackTimeFromFile(fRec.inputBackgrounds(j));
         }
         if (tmpBckExpTime >= .0)
         {
            defBckExpTime = tmpBckExpTime;
            defExpTime = tmpBckExpTime;
         }
      }
      XSparse::changeExtension(defOutFile, defExt);
      // default filename should be in current working directory so strip off
      // any directory specification
      fRec.fileName() = XSparse::promptFilename(XSparse::stripDirectoryPath(defOutFile), &m_batchPrompts);

      std::vector<Real> tmpExpCor(3);
      tmpExpCor[0] = defExpTime;
      tmpExpCor[1] = defCorScale;
      tmpExpCor[2] = defBckExpTime;
      while (!XSparse::promptRealTuple(expCorPrompt, tmpExpCor, &m_batchPrompts))
      {
         tmpExpCor[0] = defExpTime;
         tmpExpCor[1] = defCorScale;
         tmpExpCor[2] = defBckExpTime;
         tcout << "\n***Improper input" << std::endl;
      }
      fRec.exposureTime(tmpExpCor[0]);
      fRec.correctionNorm(tmpExpCor[1]);
      fRec.backExposureTime(tmpExpCor[2]);

      // If any spectra have a bkg file name attached (or
      // just the first for modular case), then
      // make an output bkg file name.
      for (size_t j=0; j<(m_isModular ? 1 : nSpec); ++j)
      {
	 if (fRec.inputBackgrounds(j).first.length())
	 {
	    string tmp = fRec.fileName();
	    string::size_type period = tmp.rfind('.');
	    if (period == string::npos)
	    {
	       fRec.backgndFile(tmp + bkgExt);
	    }
	    else
	    {
	       tmp.insert(period, bkgExt);
	       fRec.backgndFile(tmp);
	    }
	    break;
	 }
      }
   } // end data sets loop
} // end getAdditionalFDataSetInfo

Real FakeitHandler::getBackTimeFromFile(const BackLocator& bckLoc)
{
   // This is something of a hack in that we're directly accessing a 
   // specific data format type (OGIP-92) from a handler file.  But at least
   // we're only calling static functions. I don't seea way around this 
   // short of going through the whole process of prototyping a new DataSet
   // object for the background file, which is just not worth it when all
   // we want here is the exposure time value.  If the background  file 
   // format is not OGIP, then do nothing and no harm done (no exceptions 
   // will leave this block).
   Real bckExpTime = -1.0;
   const string bckName = bckLoc.first;
   if (bckName.length())
   {
      const string exposureStr("EXPOSURE");
      bool isRead = false;
      try
      {
         std::auto_ptr<CCfits::FITS> pFits(OGIP_92aIO::openFitsExtension
               (bckName, XspecDataIO::SpectrumType));                 
         // First, try for keyword.
         float keyVal = .0;
         pFits->currentExtension().readKey(exposureStr, keyVal);
         bckExpTime = static_cast<Real>(keyVal);
         isRead = true;                 
      }
      catch (...)
      {
      }
      if (!isRead)
      {
         // Try for column.
         try
         {
            std::auto_ptr<CCfits::FITS> pFits(OGIP_92aIO::openFitsExtension
                  (bckName, XspecDataIO::SpectrumType)); 
            RealArray colVals;
            size_t row = bckLoc.second;
            pFits->currentExtension().column(exposureStr).read(colVals, row, row);
            bckExpTime = colVals[0];
         }
         catch (...)
         {
         }
      }      
   }
   return bckExpTime;
}

void FakeitHandler::constructFakeDataSets()
{
   for (size_t i=0; i<m_nDataSets; ++i)
   {
      FakeDataInputRecord& fRec = m_fDataRecs[i];
      std::auto_ptr<DataSet> dSet(m_prototype->MakeDataSet());
      dSet->initializeFake(m_prototype, fRec);
      DataSet* validDs(dSet.release());
      m_fakeDataSets.push_back(validDs);
   }
}

bool FakeitHandler::checkForNoWrite(string& cmdStr)
{
   const string WS(" \t");
   const string::size_type inLen = cmdStr.length();
   bool isFound = false;
   // Look for leading "nowrite", case-insensitive.  If it's there,
   //  remove it along with leading/trailing whitespace (and remove
   //  a single trailing comma if it exists).
   const string::size_type startPos = cmdStr.find_first_not_of(WS);
   if (startPos != string::npos)
   {
      const string::size_type endPos = cmdStr.find_first_of(", \t", startPos);
      const string::size_type nTest = (endPos == string::npos) ? 
                   string::npos : (endPos - startPos);
      string testStr(cmdStr.substr(startPos, nTest));
      if (testStr.length() && XSutility::lowerCase(testStr) == string("nowrite"))
      {
         isFound = true;
         bool isDone = false;
         string::size_type newStart = endPos;
         while (!isDone)
         {
            if (newStart < inLen)
            {
               if (WS.find_first_of(cmdStr[newStart]) != string::npos)
               {
                  newStart = cmdStr.find_first_not_of(WS, newStart);
               }
               else
               {
                  isDone = true;
                  if (cmdStr[newStart] == ',')
                     ++newStart;
               }
            }
            else
               isDone = true;
         }
         if (newStart < inLen)
            cmdStr = cmdStr.substr(newStart);
         else
            cmdStr.clear();
      }
   }
   return isFound;
}
