#include <XSUser/Python/xspec/PySpectrum.h>
#include <XSUser/Python/xspec/PyXSutils.h>

#include <XSContainer.h>
#include <XSstreams.h>
#include <XSModel/Data/DataInputRecord.h>
#include <XSModel/Data/DataSet.h>
#include <XSModel/Data/FakeDataInputRecord.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/DataFactory/DataFactory.h>
#include <XSModel/DataFactory/XspecRegistry.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Parse/XSparse.h>
#include <memory>
#include <sstream>

namespace {
   // fileName is passed as input since it can't be retrieved from the
   // same place, depending on whether this is a Spectrum or Background.
   PyObject* createSpecTupleInv(const SpectralData* spec, const string& fileName,const BackCorr* bckCor=0);
   SpectralData* getSpectrumFromIndex(const size_t specNum);
   
}

PyObject*
_pyXspec_dataCmd(PyObject *self, PyObject *args)
{
   StringArray rawArgs;
   rawArgs.push_back(string("data"));
   if (!PyXSutils::PyListToXSArgs(args, rawArgs))
      return NULL;
   if (XSGlobal::doData(rawArgs, false) < 0)
   {
      XSContainer::datasets->emptyTrash();
      PyErr_SetString(PyExc_Exception, "Data Command Error");
      return NULL;
   }
   // Not using undo memento so we don't need to keep anything in the
   // trash can.  In normal tclsh Xspec, the trash is emptied from
   // globalData's autoSave.
   XSContainer::datasets->emptyTrash();
   return Py_BuildValue("i",1);
}

PyObject* _pyXspec_doFakeit(PyObject *self, PyObject *args)
{
   using namespace XSContainer;

   PyObject* inList=0;
   int applyStats=0;
   const char* cfilePrefix=0;
   int noWrite=0;
   if (!PyArg_ParseTuple(args, "Oisi", &inList, &applyStats, &cfilePrefix, &noWrite))
   {
      string msg("Programmer Error: Parsing fakeit command.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const string filePrefix(cfilePrefix);

   const size_t nOrigSpec = datasets->numberOfSpectra();
   const size_t nRecs = static_cast<size_t>(PyList_Size(inList));
   StringArray respNames(nRecs);
   StringArray arfNames(nRecs);
   StringArray bkgNames(nRecs);
   StringArray fileNames(nRecs);
   StringArray exposures(nRecs);
   StringArray corrections(nRecs);
   StringArray backExposures(nRecs);
   for (PY_SZ_TYPE iRec=0; iRec<static_cast<PY_SZ_TYPE>(nRecs); ++iRec)
   {
      // record is a borrowed reference (as is inList)
      PyObject* record = PyList_GetItem(inList, iRec);
      // ...but these are new references
      PyObject* respObj = PyObject_GetAttrString(record, "response");
      respNames[iRec] = string(PYXSSTRING_ASSTRING(respObj));
      PyObject* arfObj = PyObject_GetAttrString(record, "arf");
      arfNames[iRec] = string(PYXSSTRING_ASSTRING(arfObj));
      PyObject* bkgObj = PyObject_GetAttrString(record, "background");
      bkgNames[iRec] = string(PYXSSTRING_ASSTRING(bkgObj));
      PyObject* fileNameObj = PyObject_GetAttrString(record, "fileName");
      fileNames[iRec] = string(PYXSSTRING_ASSTRING(fileNameObj));
      PyObject* exposureObj = PyObject_GetAttrString(record, "exposure");
      exposures[iRec] = string(PYXSSTRING_ASSTRING(exposureObj));
      PyObject* correctionObj = PyObject_GetAttrString(record, "correction");
      corrections[iRec] = string(PYXSSTRING_ASSTRING(correctionObj));
      PyObject* backExposureObj = PyObject_GetAttrString(record, "backExposure");
      backExposures[iRec] = string(PYXSSTRING_ASSTRING(backExposureObj));
      Py_DECREF(respObj);
      Py_DECREF(arfObj);
      Py_DECREF(bkgObj);
      Py_DECREF(fileNameObj);
      Py_DECREF(exposureObj);
      Py_DECREF(correctionObj);      
      Py_DECREF(backExposureObj);
   }

   const string SKIP("/");
   StringArray cmdArgs(1,"fakeit");
   if (noWrite)
      cmdArgs.push_back("nowrite");
   
   for (size_t iRec=0; iRec<nRecs; ++iRec)
   {
      if (bkgNames[iRec].length())
      {
         std::ostringstream oss;
         oss << iRec+1;
         cmdArgs.push_back(oss.str());
         cmdArgs.push_back(bkgNames[iRec]);
      }
      else if (iRec+1 == nRecs && nRecs > nOrigSpec)
      {
         // This is the case where the command-line args must have a trailing
         // integer to specify the total number of spectra to make.
         std::ostringstream oss;
         oss << iRec+1;
         cmdArgs.push_back(oss.str());
      }
   } // end background loop

   std::vector<DataSet*> origDataSets;
   XSGlobal::fakeitDataSetOrder(origDataSets);

   // This is confusing, but xsFakeit prompts for resp, fileNames and 
   // exp/corr by data set, which is not necessarily the same order
   // as spectrum index.  Must do likewise here.  This is
   // complicated by the fact that pre-existing spectra may bundled 
   // NON-CONSECUTIVELY into data sets. ugh.

   // Response prompting for pre-existing spectra:
   size_t iRec=0;
   for (size_t iSet=0; iSet<origDataSets.size(); ++iSet)
   {
      const DataSet* origDset = origDataSets[iSet];
      IntegerArray rowNums;
      origDset->specNumOrder(rowNums);
      for (size_t iSpec=0; iSpec<rowNums.size(); ++iSpec)
      {
         // If the spectrum has no response, fakeit handler is going to 
         // prompt user and insist on getting a file name.  If it has a 
         // response, then it won't prompt user at all (or for an arf).

         // This roundabout way is necessary to match the criteria which
         // the fakeit handler uses to determine prompt/no-prompt.
         FakeDataInputRecord::Detectors respRecs =
                origDset->getResponseName(rowNums[iSpec]);
         if (!respRecs.size())
         {
            const string& respName = respNames[iRec];
            if (!respName.length() || respName == SKIP)
            {
               std::ostringstream oss;
               oss << "A response is needed for spectrum " <<iRec+1;
               PyErr_SetString(PyExc_Exception, oss.str().c_str());
               return NULL;                     
            }
            else
            {
               cmdArgs.push_back("&");
               cmdArgs.push_back(respNames[iRec]);
               cmdArgs.push_back("&");
               cmdArgs.push_back(arfNames[iRec]);
            }
         } // end if original data has no response
         ++iRec;
      } // end spectra in data set loop     
   }  // end original data set loop for response prompting

   while (iRec < nRecs)
   {
      // Not based on original data, it's OK if user hasn't filled
      // in the response prompt.  In that case the fakeit handler
      // will use the singleton dummyresp (and it won't prompt for arf).
      const string& respName = respNames[iRec];
      cmdArgs.push_back("&");
      cmdArgs.push_back(respName);
      if (respName.length() && respName != SKIP)
      {
         cmdArgs.push_back("&");
         cmdArgs.push_back(arfNames[iRec]);            
      }
      ++iRec;
   } // end response loop, not based on original data

   cmdArgs.push_back("&");
   string doRand = applyStats ? string("y") : string ("n");
   cmdArgs.push_back(doRand);
   cmdArgs.push_back("&");
   filePrefix.length() ? cmdArgs.push_back(filePrefix) :
                         cmdArgs.push_back(SKIP);
   cmdArgs.push_back("&");

   iRec = 0;
   for (size_t iSet=0; iSet<origDataSets.size(); ++iSet)
   {
      const DataSet* origDset = origDataSets[iSet];
      fileNames[iRec].length() ? cmdArgs.push_back(fileNames[iRec]) :
                                 cmdArgs.push_back(SKIP);
      cmdArgs.push_back("&");
      // Since exp,corr,backExp may be the last things entered, we can't get away
      // with entering a blank.  XSparse::findBatchString ignores whitespace
      // following the last '&'.  Therefore make sure to add a '/' if necessary.
      if (!exposures[iRec].length() && !corrections[iRec].length()
                && !backExposures[iRec].length())
         cmdArgs.push_back(SKIP);
      else
      {
         // These are all eventually parsed with a collectParams call.
         // Therefore must make sure commas are inserted in case any 
         // preceding args are blank.  It's OK if this prints out
         // unnecessary trailing commas.  They will be ignored.
         cmdArgs.push_back(exposures[iRec]+",");
         cmdArgs.push_back(corrections[iRec]+",");
         cmdArgs.push_back(backExposures[iRec]);
      }
      iRec += static_cast<size_t>(origDset->numSpectra());

      if (iRec < nRecs)
         cmdArgs.push_back("&");      
   } // end filename, exp/corr/backExp for orig data sets

   // The trouble is that it is difficult to know from here whether 
   // fakeit spectra NOT based on existing spectra are to be placed 
   // in type1 or type2 output. (This would take either a redesign
   // and global accessibility of xsFakeit's FakeitHandler class, 
   // or a LOT of duplicate code pasted into here.) But since these are
   // the final prompts, we can get away with over-filling cmdArgs.
   // If it turns out that only a single type2 file is to be created,
   // the remaining cmdArgs will simply be ignored.

   while (iRec < nRecs)
   {
      fileNames[iRec].length() ? cmdArgs.push_back(fileNames[iRec]) :
                                 cmdArgs.push_back(SKIP);
      cmdArgs.push_back("&");
      if (!exposures[iRec].length() && !corrections[iRec].length()
                && !backExposures[iRec].length())
         cmdArgs.push_back(SKIP);
      else
      {
         cmdArgs.push_back(exposures[iRec]+",");
         cmdArgs.push_back(corrections[iRec]+",");
         cmdArgs.push_back(backExposures[iRec]);
      }
      ++iRec;      
      if (iRec < nRecs)
         cmdArgs.push_back("&");
   } // end filename, exp/corr/backExp for non-orig data sets

   // Intended for DEBUGGING purposes:
   tcout << "xsFakeit cmd string:"<<std::endl;
   string fakeitStr;
   for (size_t i=0; i< cmdArgs.size(); ++i)
   {
      fakeitStr += cmdArgs[i];
      fakeitStr += " ";
   }   
   tcout << fakeitStr <<'\n'<<std::endl; 

   if (XSGlobal::doFakeit(cmdArgs))
   {
      string msg("Fakeit error");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }   
   return Py_BuildValue("i",1);
} // end doFakeit


PyObject* _pyXspec_getBackgrnd(PyObject *self, PyObject *args)
{
   size_t specNum=0;
   size_t isCorr=0;
   if (!PyArg_ParseTuple(args, "II", &specNum, &isCorr))
   {
      string msg("Error: Cannot parse getBackgrnd arguments.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const SpectralData* spec = getSpectrumFromIndex(specNum);
   if (!spec)
   {
      return NULL;
   }
   const BackCorr* bckCor = isCorr ? spec->correction() : spec->background();
   if (bckCor)
   {
      const string& fileName = isCorr ? spec->correctionFile() :
                                        spec->backgroundFile();
      PyObject* backTuple = createSpecTupleInv(bckCor->data(), fileName);
      return backTuple;
   }
   else
   {
      const string fileDescr = isCorr ? string("correction.") : 
                                  string("background.");
      string msg("Error: Spectrum has no ");
      msg += fileDescr;
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }  
}
// end getBackgrnd

PyObject* _pyXspec_getCornorm(PyObject *self, PyObject *args)
{
   PyObject* handle=0;
   if (!PyArg_ParseTuple(args, "O", &handle))
   {
      string msg("Programmer Error: Spectrum handle conversion in getCornorm.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const SpectralData* spec = PyXSutils::verifySpectrumHandle(handle);
   if (!spec)
      return NULL;
   PyObject* cornorm = PyFloat_FromDouble(spec->correctionScale());
   return cornorm;
}


PyObject* _pyXspec_getFluxLuminCalc(PyObject *self, PyObject *args)
{
   size_t specNum=0;
   int isFlux=0;
   if (!PyArg_ParseTuple(args, "Ii", &specNum, &isFlux))
   {
      string msg("Programmer Error: Parsing getFluxLuminCalc arg.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   const SpectralData* spec = XSContainer::datasets->lookup(specNum);
   // 1 set of flux/lumin calculations exist for each model associated
   // with spectrum.
   const std::vector<SpectralData::FluxCalc>& fluxHolder = isFlux ?
                spec->lastModelFluxCalc() : spec->lastModelLuminCalc();
   const size_t nFlux = fluxHolder.size();
   PyObject* fluxTuple = PyTuple_New(nFlux ? 6*nFlux : 6);
   for (size_t i=0; i<nFlux; ++i)
   {
      const size_t offset = i*6;
      const SpectralData::FluxCalc& fluxCalc = fluxHolder[i];
      PyTuple_SetItem(fluxTuple, offset, PyFloat_FromDouble(fluxCalc.value));
      PyTuple_SetItem(fluxTuple, offset+1, PyFloat_FromDouble(fluxCalc.errLow));
      PyTuple_SetItem(fluxTuple, offset+2, PyFloat_FromDouble(fluxCalc.errHigh));
      PyTuple_SetItem(fluxTuple, offset+3, PyFloat_FromDouble(fluxCalc.photonValue));
      PyTuple_SetItem(fluxTuple, offset+4, PyFloat_FromDouble(fluxCalc.photonLow));
      PyTuple_SetItem(fluxTuple, offset+5, PyFloat_FromDouble(fluxCalc.photonHigh));       
   }
   if (!nFlux)
   {
      for (size_t j=0; j<6; ++j)
         PyTuple_SetItem(fluxTuple, j, PyFloat_FromDouble(0.0));
   }
   return fluxTuple;
}
// end getFluxLuminCalc

PyObject* _pyXspec_getIgnoredChannels(PyObject *self, PyObject *args)
{
   size_t specNum=0;
   if (!PyArg_ParseTuple(args, "I", &specNum))
   {
      string msg("Programmer Error: Parsing getIgnoredChannels arg.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   const SpectralData* sd = XSContainer::datasets->lookup(specNum);
   const BoolArray& noticedChans = sd->noticedChannels();
   const size_t offset = sd->startChan() - sd->firstChan();
   const size_t nChans = sd->channels();
   std::vector<size_t> ignoredChans;
   for (size_t i=0; i<nChans; ++i)
   {
      if (!noticedChans[i+offset])
         ignoredChans.push_back(i+1);
   }
   PY_SZ_TYPE nIgnored = static_cast<PY_SZ_TYPE>(ignoredChans.size());
   PyObject* list = PyList_New(nIgnored);
   for (PY_SZ_TYPE i=0; i<nIgnored; ++i)
   {
      PyObject* val = PYXSINT_FROMLONG(static_cast<long>(ignoredChans[i]));
      PyList_SetItem(list, i, val);
   }
   return list;   
}

PyObject* _pyXspec_getIndexFromHandle(PyObject *self, PyObject* args)
{
   using namespace XSContainer;

   PyObject* handle=0;
   if (!PyArg_ParseTuple(args, "O", &handle))
   {
      string msg("Programmer Error: Cannot get spectrum object handle.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   SpectralData* spec = PyXSutils::verifySpectrumHandle(handle);
   if (!spec)
      return NULL;
   
   int iSpec = static_cast<int>(spec->spectrumNumber());
   // The "I" format doesn't seem to work in Python 2.3
   return Py_BuildValue("i",iSpec);
}
// end getIndexFromHandle

PyObject* _pyXspec_getNoticedChannels(PyObject *self, PyObject *args)
{
   size_t specNum=0;
   if (!PyArg_ParseTuple(args, "I", &specNum))
   {
      string msg("Programmer Error: Parsing getNoticedChannels arg.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   const SpectralData* spec = XSContainer::datasets->lookup(specNum);
   std::valarray<size_t> indirectNotice;
   spec->buildIndirectNotice(indirectNotice);
   const PY_SZ_TYPE nChan = indirectNotice.size();
   PyObject* list = PyList_New(nChan);
   for (PY_SZ_TYPE i=0; i<nChan; ++i)
   {
      PyObject* val = PYXSINT_FROMLONG(static_cast<long>(indirectNotice[i])+1);
      PyList_SetItem(list, i, val);
   }
   return list;   
}
// end getNoticedChannels

PyObject* _pyXspec_getNSpectra(PyObject *self, PyObject *args)
{
   long nSpec = static_cast<long>(XSContainer::datasets->numberOfSpectra());
   return PYXSINT_FROMLONG(nSpec);
}

PyObject* _pyXspec_getRate(PyObject *self, PyObject *args)
{
   size_t specNum=0;
   if (!PyArg_ParseTuple(args, "I", &specNum))
   {
      string msg("Programmer Error: Parsing getRate arg.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   const SpectralData* spec = XSContainer::datasets->lookup(specNum);
   const Real modelRate = XSContainer::models->countRate(specNum);
   PyObject* rateTuple = PyTuple_New(4);
   PyTuple_SetItem(rateTuple, 0, PyFloat_FromDouble(spec->netFlux()));
   PyTuple_SetItem(rateTuple, 1, PyFloat_FromDouble(spec->netVariance()));
   PyTuple_SetItem(rateTuple, 2, PyFloat_FromDouble(spec->totalFlux()));
   PyTuple_SetItem(rateTuple, 3, PyFloat_FromDouble(modelRate));
   return rateTuple;
}



PyObject*
_pyXspec_getSpectrum(PyObject *self, PyObject *args)
{
   size_t specNum=0;
   if (!PyArg_ParseTuple(args, "I", &specNum))
   {
      string msg("Error: Spectrum index must be a positive integer.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   SpectralData* spec = getSpectrumFromIndex(specNum);
   if (!spec)
      return NULL;
   void* handle = static_cast<void*>(spec);
   return PYXSCOBJECT_FROMVOIDPTR(handle,NULL,NULL);
}

PyObject* _pyXspec_getSpectrumInvariants(PyObject *self, PyObject *args)
{
   size_t specNum=0;
   if (!PyArg_ParseTuple(args, "I", &specNum))
   {
      string msg("Error: Spectrum index must be a positive integer.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   SpectralData* spec = getSpectrumFromIndex(specNum);
   if (!spec)
      return NULL;
   const string& fileName = spec->parent()->dataName();
   return createSpecTupleInv(spec, fileName);
}

PyObject*
_pyXspec_readSpectrum(PyObject *self, PyObject *args)
{
   using namespace std;
   using namespace XSContainer;

   const char *dataFile=0;
   if (!PyArg_ParseTuple(args,"z", &dataFile))
   {
      string msg("Filename parsing error");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   if (XSparse::isBlank(dataFile))
   {
      string msg("Error: Data file name is required.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }

   void *handleToSpec=0;
   const int specNum = static_cast<int>(datasets->numberOfSpectra()) + 1;
   StringArray rawArgs;
   rawArgs.push_back(string("data"));
   ostringstream oss;
   oss << specNum;
   rawArgs.push_back(oss.str());
   rawArgs.push_back(string(dataFile));
   if (XSGlobal::doData(rawArgs, false) < 0)
   {
      string msg("Error while attempting to load data from ");
      msg += string(dataFile);
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   handleToSpec = static_cast<void*>(datasets->lookup((size_t)specNum));

   // Don't need to empty trash here since this function can't delete
   // existing datasets.

   return PYXSCOBJECT_FROMVOIDPTR(handleToSpec,NULL,NULL);
}  // end readSpectrum



PyObject* _pyXspec_setBackgrnd(PyObject *self, PyObject *args)
{
   const char *backFile=0;
   size_t specNum=0;
   size_t isCorr=0;
   if (!PyArg_ParseTuple(args,"IzI", &specNum, &backFile, &isCorr))
   {
      string msg("Back/corr filename parsing error");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   StringArray rawArgs(3);
   rawArgs[0] = isCorr ? string("corfile") : string("backgrnd");
   std::ostringstream oss;
   oss << specNum;
   rawArgs[1] = oss.str();
   // If backFile == 0, caller is requesting that back/corr be removed.
   rawArgs[2] = backFile ? string(backFile) : string("none");
   const BackCorr* bckCor=0;
   string fileName;
   if (isCorr)
   {
      if (XSGlobal::doCorfile(rawArgs))
      {
         string msg("Correction file setting error");
         PyErr_SetString(PyExc_Exception, msg.c_str());
         return NULL;      
      }
      const SpectralData* spec = XSContainer::datasets->lookup(specNum);
      bckCor = spec->correction();
      fileName = spec->correctionFile();
   }
   else
   {
      if (XSGlobal::doBackgrnd(rawArgs))
      {
         string msg("Background file setting error");
         PyErr_SetString(PyExc_Exception, msg.c_str());
         return NULL;      
      }
      const SpectralData* spec = XSContainer::datasets->lookup(specNum);
      bckCor = spec->background();
      fileName = spec->backgroundFile();
   }   
   if (bckCor)
   {
      PyObject* backTuple = createSpecTupleInv(bckCor->data(), fileName);
      return backTuple;
   }
   else
   {
      Py_INCREF(Py_None);
      return Py_None; 
   }  
} // end setBackgrnd


PyObject* _pyXspec_showAllData(PyObject *self, PyObject *args)
{
   StringArray rawArgs(2);
   rawArgs[0] = string("show");
   rawArgs[1] = string("data");
   string dummy;
   XSGlobal::doShow(rawArgs, dummy);
   return Py_BuildValue("i",1);
}

PyObject* _pyXspec_showData(PyObject *self, PyObject *args)
{
   // Don't just convert and use handle directly.  Must first
   // verify that handle still points to a valid SpectralData
   // object.  User could accidentally hang on to a Python
   // Spectrum object after the C++ object is gone, and we don't
   // want an innocuous call to a show function to crash
   // the session.
   PyObject* specIdx = _pyXspec_getIndexFromHandle(self, args);
   if (!specIdx)
   {
      // PyErr string will already have been set.
      return NULL;
   }
   size_t specNum = static_cast<size_t>(PYXSINT_ASLONG(specIdx));
   const SpectralData* sd = XSContainer::datasets->lookup(specNum);               
   sd->report(false);
   tcout << " Spectral data counts: " 
         << sd->totalFlux()*sd->exposureTime() << std::endl;
   XSContainer::models->reportModelRate(sd);
   tcout << std::endl;

   return Py_BuildValue("i",1);
}

namespace {

PyObject* createSpecTupleInv(const SpectralData* spec, const string& fileName, const BackCorr* bckCor)
{
   // Collect information that will remain invariant throughout the Python
   // object's lifetime.
   PyObject* pyVals=0;
   pyVals = PyTuple_New(7);
   PyTuple_SetItem(pyVals, 0, PYXSSTRING_FROMSTRING(fileName.c_str()));

   // Now make a nested tuple to hold the spectrum array values in pyVals[1].
   // A list is a more natural structure for this, but we can't
   // enforce invariance with a list.
   const RealArray& specVals = spec->spectrum();
   const PY_SZ_TYPE nChans = static_cast<PY_SZ_TYPE>(specVals.size());
   PyObject* arrayTuple = PyTuple_New(nChans);
   for (PY_SZ_TYPE i=0; i<nChans; ++i)
      PyTuple_SetItem(arrayTuple, i, PyFloat_FromDouble(specVals[i]));
   PyTuple_SetItem(pyVals, 1, arrayTuple);

   const RealArray& variance = spec->variance();
   // Don't assume variance is size nChans.  This could be a correction
   //  file with a zero-sized variance array.
   const PY_SZ_TYPE nVarChans = static_cast<PY_SZ_TYPE>(variance.size());
   PyObject* varTuple = PyTuple_New(nVarChans);
   for (PY_SZ_TYPE i=0; i<nVarChans; ++i)
      PyTuple_SetItem(varTuple, i, PyFloat_FromDouble(variance[i]));
   PyTuple_SetItem(pyVals, 2, varTuple);

   long isPoiss = static_cast<long>(spec->isPoisson());
   PyTuple_SetItem(pyVals, 3, PyBool_FromLong(isPoiss));

   PyTuple_SetItem(pyVals, 4, PyFloat_FromDouble(spec->exposureTime()));

   // If areascale is just a keyword in the original file, store a single
   // float.  Otherwise we'll need to add another nested tuple array.
   const bool isAScaleKey = bckCor ? bckCor->aScaleIsKeyword() :
                                spec->parent()->aScaleIsKeyword();
   if (isAScaleKey)
      PyTuple_SetItem(pyVals, 5, PyFloat_FromDouble(spec->areaScale(0)));
   else
   {
      const RealArray& aScale = spec->areaScale();
      PyObject* scaleTuple = PyTuple_New(nChans);
      for (PY_SZ_TYPE i=0; i<nChans; ++i)
         PyTuple_SetItem(scaleTuple, i, PyFloat_FromDouble(aScale[i]));
      PyTuple_SetItem(pyVals, 5, scaleTuple);
   }

   // Now do same for backscale.
   const bool isBScaleKey = bckCor ? bckCor->bScaleIsKeyword() :
                                spec->parent()->bScaleIsKeyword();
   if (isBScaleKey)
      PyTuple_SetItem(pyVals, 6, PyFloat_FromDouble(spec->backgroundScale(0)));
   else
   {
      const RealArray& bScale = spec->backgroundScale();
      PyObject* scaleTuple = PyTuple_New(nChans);
      for (PY_SZ_TYPE i=0; i<nChans; ++i)
         PyTuple_SetItem(scaleTuple, i, PyFloat_FromDouble(bScale[i]));
      PyTuple_SetItem(pyVals, 6, scaleTuple);
   }
   
   return pyVals;
}

SpectralData* getSpectrumFromIndex(const size_t specNum)
{
   if (specNum < 1 || specNum > XSContainer::datasets->numberOfSpectra())
   {
      std::ostringstream oss;
      oss << "Error: Spectrum index number is out of range: "
         << specNum;
      PyErr_SetString(PyExc_Exception, oss.str().c_str());
      return NULL;
   }
   SpectralData* spec = XSContainer::datasets->lookup(specNum);
   return spec;
}

}
