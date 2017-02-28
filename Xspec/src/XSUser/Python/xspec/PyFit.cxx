#include <XSUser/Python/xspec/PyFit.h>
#include <XSUser/Python/xspec/PyXSutils.h>

#include <XSContainer.h>
#include <XSstreams.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSFit/MCMC/Chain.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSUser/Handler/XSinterface.h>
#include <sstream>

PyObject* _pyXspec_getFitSettings(PyObject *self, PyObject *args)
{
   using namespace XSContainer;
   
   PyObject* isBayesOn = 0;
   PyObject* settings = PyTuple_New(6);
   PyTuple_SetItem(settings, 0, 
        PYXSSTRING_FROMSTRING(fit->statManager()->defaultStat()->name().c_str()));
   PyTuple_SetItem(settings, 1,
        PYXSSTRING_FROMSTRING(fit->fitMethod()->selectedSubMethod().c_str()));
   PyTuple_SetItem(settings, 2, 
        PYXSINT_FROMLONG(static_cast<long>(fit->fitMethod()->numberOfTrials())));
   PyTuple_SetItem(settings, 3, PyFloat_FromDouble(fit->fitMethod()->deltaCrit()));
   if (fit->statManager()->getBayes())
   {
      Py_INCREF(Py_True);
      isBayesOn = Py_True;
   }
   else
   {
      Py_INCREF(Py_False);
      isBayesOn = Py_False;
   }
   PyTuple_SetItem(settings, 4, isBayesOn);
   PyTuple_SetItem(settings, 5,
        PYXSSTRING_FROMSTRING(fit->statManager()->defaultTestStat()->name().c_str()));
   
   return settings;
}

PyObject* _pyXspec_getStatistic(PyObject *self, PyObject *args)
{
  double statVal = XSContainer::fit->statistic();
  return Py_BuildValue("d",statVal);
}

PyObject* _pyXspec_getTestStatistic(PyObject *self, PyObject *args)
{
  double statVal = XSContainer::fit->statManager()->totalTestStatistic();
  return Py_BuildValue("d",statVal);
}

PyObject* _pyXspec_setQuery(PyObject *self, PyObject *args)
{
   using namespace XSContainer;

   const char* query=0;
   if (!PyArg_ParseTuple(args, "s", &query))
   {
      string msg("Programmer Error: Parsing query setting.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   string queryStr(query);
   if (queryStr[0]=='o')
      fit->queryMode(Fit::ON);
   else if (queryStr[0]=='y')
      fit->queryMode(Fit::YES);
   else if (queryStr[0]=='n')
      fit->queryMode(Fit::NO);
   return Py_BuildValue("i",0);
}

PyObject* _pyXspec_showFit(PyObject *self, PyObject *args)
{
   StringArray rawArgs(2);
   rawArgs[0] = string("show");
   rawArgs[1] = string("fit");
   string dummy;
   XSGlobal::doShow(rawArgs, dummy);
   return Py_BuildValue("i",0);
}

PyObject* _pyXspec_getChainByIndex(PyObject *self, PyObject *args)
{
   using namespace XSContainer;
   
   PyObject* pyChain=0;
   size_t chainIdx=0;
   if (!PyArg_ParseTuple(args, "I", &chainIdx))
   {
      string msg("Programmer Error:  Chain index must be a positive int");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const ChainManager::ChainContainer& loadedChains = 
                fit->chainManager()->chains();
   if (!chainIdx || chainIdx > loadedChains.size())
   {
      std::ostringstream oss;
      oss << "Error: Chain index number: " << chainIdx << " is out  range.\n"
          << "  " << loadedChains.size()  << " chains are currently loaded.";
      PyErr_SetString(PyExc_Exception, oss.str().c_str());
      return NULL;
   }

   size_t idx=1;
   const Chain* chain=0;
   ChainManager::ChainContainer::const_iterator it = loadedChains.begin();
   ChainManager::ChainContainer::const_iterator itEnd = loadedChains.end();
   while (!chain && it != itEnd)
   {
      if (chainIdx == idx)
      {
         chain = it->second;
      }
      ++it;
      ++idx;
   }
   // Can't see how this could happen:
   if (!chain)
   {
      string msg("Programmer Error:  Unable to recover chain.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   
   // No more throwing, OK to allocate.
   pyChain = PyTuple_New(4);
   PyTuple_SetItem(pyChain, 0, PYXSSTRING_FROMSTRING(chain->getFileName().c_str()));
   PyTuple_SetItem(pyChain, 1, PYXSSTRING_FROMSTRING(chain->format().c_str()));
   PyTuple_SetItem(pyChain, 2, PYXSINT_FROMLONG(static_cast<long>(chain->length())));
   PyTuple_SetItem(pyChain, 3, PYXSINT_FROMLONG(static_cast<long>(chain->width())));
   
   return pyChain;
} // end getChainByIndex

PyObject* _pyXspec_getChainManagerInfo(PyObject *self, PyObject *args)
{
   using namespace XSContainer;
   
   const ChainManager::ChainContainer& loadedChains = fit->chainManager()->chains();
   const std::map<int,ModParam*>& varPars = fit->variableParameters();
   PY_SZ_TYPE nChains = static_cast<PY_SZ_TYPE>(loadedChains.size());
   PY_SZ_TYPE nPars = static_cast<PY_SZ_TYPE>(varPars.size());
   PyObject* chainList = PyList_New(nChains);
   PyObject* parList = PyList_New(nPars);
   PY_SZ_TYPE idx=0;
   ChainManager::ChainContainer::const_iterator itChains = loadedChains.begin();
   ChainManager::ChainContainer::const_iterator itChainsEnd = loadedChains.end();
   while (itChains != itChainsEnd)
   {
      PyList_SetItem(chainList, idx, PYXSSTRING_FROMSTRING(itChains->first.c_str()));
      ++itChains;
      ++idx;
   }
   idx = 0;
   std::map<int,ModParam*>::const_iterator itPar = varPars.begin();
   std::map<int,ModParam*>::const_iterator itParEnd = varPars.end();
   while (itPar != itParEnd)
   {
      PyList_SetItem(parList, idx, 
                PYXSSTRING_FROMSTRING(itPar->second->getParameterLabel().c_str()));
      ++itPar;
      ++idx;
   }
   PyObject* containerInfo=PyTuple_New(2);
   PyTuple_SetItem(containerInfo, 0, chainList);
   PyTuple_SetItem(containerInfo, 1, parList);
      
   return containerInfo;
} // end getChainManagerInfo

PyObject* _pyXspec_removeChainByName(PyObject *self, PyObject *args)
{
   using namespace XSContainer;

   const char* cfileName=0;
   if (!PyArg_ParseTuple(args, "s", &cfileName))
   {
      string msg("Programmer Error: Parsing getChainIndex.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const string fileName(cfileName);
   int removalDone = fit->chainManager()->removeChain(fileName) ?
                        0 : -1;
   
   return Py_BuildValue("i", removalDone);
}

PyObject* _pyXspec_showChain(PyObject *self, PyObject *args)
{
   PyObject* chain=0;
   if (!PyArg_ParseTuple(args, "O", &chain))
   {
      string msg("Programmer Error: Chain object conversion error.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   PyObject* algorithmObj = PyObject_GetAttrString(chain, "algorithm");
   PyObject* burnObj = PyObject_GetAttrString(chain, "burn");
   PyObject* fileNameObj = PyObject_GetAttrString(chain, "fileName");
   PyObject* fileTypeObj = PyObject_GetAttrString(chain, "fileType");
   PyObject* runLengthObj = PyObject_GetAttrString(chain, "runLength");
   PyObject* totalLengthObj = PyObject_GetAttrString(chain, "totalLength");
   PyObject* proposalObj = PyObject_GetAttrString(chain, "proposal");
   PyObject* randObj = PyObject_GetAttrString(chain, "rand");
   PyObject* temperatureObj = PyObject_GetAttrString(chain, "temperature");
   PyObject* walkersObj = PyObject_GetAttrString(chain, "walkers");
   
   const char* algorithm = PYXSSTRING_ASSTRING(algorithmObj);
   const long  burn = PYXSINT_ASLONG(burnObj);
   const char* fileName = PYXSSTRING_ASSTRING(fileNameObj);
   const char* fileType = PYXSSTRING_ASSTRING(fileTypeObj);
   const long  runLength = PYXSINT_ASLONG(runLengthObj);
   const long  totalLength = PYXSINT_ASLONG(totalLengthObj);
   const char* proposal = PYXSSTRING_ASSTRING(proposalObj);
   const char* rand = (randObj == Py_True) ? "True" : "False"; 
   const double temperature = PyFloat_AsDouble(temperatureObj);
   const long walkers = PYXSINT_ASLONG(walkersObj);
   
   tcout << "Current settings for Chain object:\n\n"
         << "   Non-modifiable attributes:\n"
         << "     fileName    = " << fileName << "\n"
         << "     fileType    = " << fileType << "\n"
         << "     totalLength = " << totalLength << "\n\n"
         << "   These attribute settings will apply to the NEXT run for this chain.\n"
         << "   The algorithm, burn, rand, and walkers settings are irrelevant if run is appending.\n"
         << "     runLength   = " << runLength << "\n"
         << "     proposal    = " << proposal << "\n"
         << "     temperature = " << temperature << "\n"
         << "     burn        = " << burn << "\n"
         << "     rand        = " << rand << "\n"
         << "     algorithm   = " << algorithm << "\n"
         << "     walkers     = " << walkers << std::endl;
   
   Py_DECREF(algorithmObj);
   Py_DECREF(burnObj);
   Py_DECREF(fileNameObj);
   Py_DECREF(fileTypeObj);
   Py_DECREF(runLengthObj);
   Py_DECREF(totalLengthObj);
   Py_DECREF(proposalObj);
   Py_DECREF(randObj);
   Py_DECREF(temperatureObj);
   Py_DECREF(walkersObj);
   
   return Py_BuildValue("i", 0);
} // end showChain

PyObject* _pyXspec_showChainContainer(PyObject *self, PyObject *args)
{
   using namespace XSContainer;
   
   PyObject* container=0;
   if (!PyArg_ParseTuple(args, "O", &container))
   {
      string msg("Programmer Error: ChainContainer object conversion error.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   PyObject* algorithmObj = PyObject_GetAttrString(container, "defAlgorithm");
   PyObject* burnObj = PyObject_GetAttrString(container, "defBurn");
   PyObject* fileTypeObj = PyObject_GetAttrString(container, "defFileType");
   PyObject* lengthObj = PyObject_GetAttrString(container, "defLength");
   PyObject* proposalObj = PyObject_GetAttrString(container, "defProposal");
   PyObject* randObj = PyObject_GetAttrString(container, "defRand");
   PyObject* temperatureObj = PyObject_GetAttrString(container, "defTemperature");
   PyObject* walkersObj = PyObject_GetAttrString(container, "defWalkers");

   const char* algorithm = PYXSSTRING_ASSTRING(algorithmObj);
   const long  burn = PYXSINT_ASLONG(burnObj);
   const char* fileType = PYXSSTRING_ASSTRING(fileTypeObj);
   const long  length = PYXSINT_ASLONG(lengthObj);
   const char* proposal = PYXSSTRING_ASSTRING(proposalObj);
   const char* rand = (randObj == Py_True) ? "True" : "False"; 
   const double temperature = PyFloat_AsDouble(temperatureObj);
   const long walkers = PYXSINT_ASLONG(walkersObj);
   
   tcout << "Current settings for AllChains container:\n\n"
         << "   Default values when constructing new Chain objects:\n"
         << "     defAlgorithm = " << algorithm << "\n"
         << "     defBurn      = " << burn << "\n"
         << "     defFileType  = " << fileType << "\n"
         << "     defLength    = " << length << "\n"
         << "     defProposal  = " << proposal << "\n"
         << "     defRand      = " << rand << "\n"
         << "     defTemperature = " << temperature << "\n"
         << "     defWalkers   = " << walkers << std::endl;
   
    tcout << "\n(Model parameters";
    const std::map<int,ModParam*>& varPars = fit->variableParameters();
    std::map<int,ModParam*>::const_iterator it = varPars.begin();
    std::map<int,ModParam*>::const_iterator itEnd = varPars.end();
    while (it != itEnd)
    {
       tcout << "   " << it->second->getParameterLabel();
       ++it;
    }
    tcout << ")" << std::endl; 
   
   const ChainManager::ChainContainer& loadedChains = fit->chainManager()->chains();
   if (!loadedChains.size())
      tcout << "No loaded chains" << std::endl;
   else
   {
      tcout << "Loaded chains:   length:" <<std::endl;
      ChainManager::ChainContainer::const_iterator it = loadedChains.begin();
      ChainManager::ChainContainer::const_iterator itEnd = loadedChains.end();
      size_t index=1;
      while (it != itEnd)
      {
         tcout << "    " << index << ".  " << it->first 
            << "  " << it->second->length() << std::endl;
         ++index;
         ++it;
      }   
   }
   
   Py_DECREF(algorithmObj);
   Py_DECREF(burnObj);
   Py_DECREF(fileTypeObj);
   Py_DECREF(lengthObj);
   Py_DECREF(proposalObj);
   Py_DECREF(randObj);
   Py_DECREF(temperatureObj);
   Py_DECREF(walkersObj);
   
   return Py_BuildValue("i", 0);
} // end showChainContainer
