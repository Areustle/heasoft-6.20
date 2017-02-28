#include <Python.h>

#include <XSsymbol.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <xsTypes.h>
#include <XSFit/Fit/Fit.h>
#include <XSFunctions/functionMap.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Model/MixFunction/xsmixFunctionMap.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUser/Python/xspec/PyFit.h>
#include <XSUser/Python/xspec/PyModel.h>
#include <XSUser/Python/xspec/PyParameter.h>
#include <XSUser/Python/xspec/PyPlot.h>
#include <XSUser/Python/xspec/PyPymod.h>
#include <XSUser/Python/xspec/PyResponse.h>
#include <XSUser/Python/xspec/PySpectrum.h>
#include <XSUser/Python/xspec/PyxsIO.h>
#include <XSUser/Python/xspec/PyXset.h>
#include <XSUser/Python/xspec/PyXSutils.h>
#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSUtil/Numerics/RandomGenerator.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/IosHolder.h>
#include <XSUtil/Utils/ProcessManager.h>
#include <XSUtil/Utils/XSstream.h>

void xspecLibInit();
void exitXspec();

PyMethodDef _PyXspecMethods[] = {
   {"getFitSettings",_pyXspec_getFitSettings, METH_VARARGS,"Get the current fit settings."},
   {"getStatistic",_pyXspec_getStatistic, METH_VARARGS,"Get the fit statistic value."},
   {"getTestStatistic",_pyXspec_getTestStatistic, METH_VARARGS,"Get the test statistic value."},
   {"setQuery",_pyXspec_setQuery, METH_VARARGS,"Set the fit query value."},
   {"showFit",_pyXspec_showFit, METH_VARARGS,"Call the Xspec \"show fit\" function."},
   {"getChainByIndex",_pyXspec_getChainByIndex, METH_VARARGS,"Get attributes of a loaded chain."},
   {"getChainManagerInfo",_pyXspec_getChainManagerInfo, METH_VARARGS,"Get info from C++ ChainManager."},
   {"removeChainByName",_pyXspec_removeChainByName, METH_VARARGS,"Unload chain identified by its filename."},
   {"showChain",_pyXspec_showChain, METH_VARARGS,"Display current chain attributes."},
   {"showChainContainer",_pyXspec_showChainContainer, METH_VARARGS,"Display info for AllChains."},

   {"createModel",_pyXspec_createModel, METH_VARARGS,"Create a new model."},
   {"fluxCmd",_pyXspec_fluxCmd, METH_VARARGS,"Xspec flux command wrapper."},
   {"getArray",_pyXspec_getArray, METH_VARARGS,"Get a model's flux or folded array for a given spectrum."},
   {"getComponentPars",_pyXspec_getComponentPars, METH_VARARGS,"Return a parameter dictionary for a model component."},
   {"getModelFluxLuminCalc",_pyXspec_getModelFluxLuminCalc,METH_VARARGS,"Get a model's most recent flux or lumin calculation."},
   {"getModelFromNameAndGroup",_pyXspec_getModelFromNameAndGroup,
        METH_VARARGS, "Return a handle for a currently existing Xspec Model object."},
   {"getModelSourceAssignments",_pyXspec_getModelSourceAssignments,METH_VARARGS,"Get all active model/source assignments."},
   {"getModelTuple",_pyXspec_getModelTuple, METH_VARARGS,"Get info from a currently existing Model object."},
   {"getSpectraForModel",_pyXspec_getSpectraForModel, METH_VARARGS,"Get Model's associated spectrum numbers."},
   {"localModel",_pyXspec_localModel, METH_VARARGS,"Load a local model library."},
   {"removeModels",_pyXspec_removeModels, METH_VARARGS,"Remove one or all models."},
   {"showModel",_pyXspec_showModel, METH_VARARGS, "Display information for a single Model object."},
   {"addPyComp",_pyXspec_addPyComp, METH_VARARGS, "Add an Xspec model component written in Python."},

   {"getParTuple",_pyXspec_getParTuple, METH_VARARGS,"Return a tuple from a single parameter."},
   {"newparCmd",_pyXspec_newparCmd, METH_VARARGS,"Wrapper for calling doNewpar handler."},
   {"setParBayes",_pyXspec_setParBayes, METH_VARARGS, "Set Bayes prior types and values."},
   {"setParFreeze",_pyXspec_setParFreeze, METH_VARARGS, "Freeze or thaw a single parameter."},
   {"setParLink",_pyXspec_setParLink, METH_VARARGS, "Set or remove a parameter link."},
   {"setPars",_pyXspec_setPars, METH_VARARGS, "Set multiple parameters at a time."},
   {"setParsGlobal",_pyXspec_setParsGlobal, METH_VARARGS,"Set multiple pars for multiple model objects."},
   {"setRespPars",_pyXspec_setRespPars, METH_VARARGS, "Set multiple response parameters at a time."},
   {"showPar",_pyXspec_showPar, METH_VARARGS, "Show all or a subset of parameters."},

   {"getArf",_pyXspec_getArf, METH_VARARGS, "Get arf for response"},
   {"getChannelEnergies",_pyXspec_getChannelEnergies, METH_VARARGS, "Get spectrum's energies from response."},
   {"getResponse",_pyXspec_getResponse, METH_VARARGS, "Get a response for spectrum and source"},
   {"hasGainPars",_pyXspec_hasGainPars, METH_VARARGS, "Return True if response has gain parameters."},
   {"setArf",_pyXspec_setArf, METH_VARARGS,"Set or remove a response's arf"},
   {"setResponse",_pyXspec_setResponse, METH_VARARGS, "Set a response for spectrum."},
   {"showResponse",_pyXspec_showResponse, METH_VARARGS, "Display response information."},

   {"dataCmd",_pyXspec_dataCmd, METH_VARARGS, "Pass string directly to Xspec's data command handler."},
   {"doFakeit",_pyXspec_doFakeit, METH_VARARGS, "Parse fakeit args and pass to Xspec."},
   {"getBackgrnd",_pyXspec_getBackgrnd, METH_VARARGS, "Return background information for a given spectrum."},
   {"getCornorm",_pyXspec_getCornorm, METH_VARARGS, "Get the spectrum's correction scale"},
   {"getFluxLuminCalc",_pyXspec_getFluxLuminCalc, METH_VARARGS, "Get spectrum's most recent flux or lumin calc."},
   {"getIgnoredChannels",_pyXspec_getIgnoredChannels, METH_VARARGS, "Get the ignored channels array."},
   {"getIndexFromHandle",_pyXspec_getIndexFromHandle, METH_VARARGS, "Get a spectrum's index number from its handle."},
   {"getNoticedChannels",_pyXspec_getNoticedChannels, METH_VARARGS, "Get the indirect noticed array."},
   {"getNSpectra",_pyXspec_getNSpectra, METH_VARARGS, "Get the number of loaded spectra."},
   {"getRate",_pyXspec_getRate, METH_VARARGS, "Get the spectrum's net and total rates"},
   {"getSpectrum",_pyXspec_getSpectrum, METH_VARARGS,"Get a spectrum from index number."},
   {"getSpectrumInvariants",_pyXspec_getSpectrumInvariants, METH_VARARGS,"Collect invariant spectrum information."},
   {"readSpectrum",_pyXspec_readSpectrum, METH_VARARGS,"Read a spectral data file."},
   {"setBackgrnd",_pyXspec_setBackgrnd, METH_VARARGS, "Set a spectrum object's background."},
   {"showAllData",_pyXspec_showAllData, METH_VARARGS, "Call the Xspec \"show data\" function."},
   {"showData",_pyXspec_showData, METH_VARARGS, "Show a single Spectrum object."},

   {"clearCommands",_pyXspec_clearCommands,METH_VARARGS,"Remove all user-entered plot commands."},
   {"getplotSettings",_pyXspec_getplotSettings, METH_VARARGS,"Get a particular setplot attribute."},
   {"getplotValues",_pyXspec_getplotValues, METH_VARARGS,"Get array of X or Y plotted values."},
   {"identifyUnits",_pyXspec_identifyUnits,METH_VARARGS,"Determine whether string is a valid energy or wavelength unit."},
   {"setPerHz",_pyXspec_setPerHz,METH_VARARGS,"Set the isWavePerHz flag for y-axis display."},
   {"setplotCmd",_pyXspec_setplotCmd, METH_VARARGS,"Xspec setplot command wrapper."},
   {"showPlot",_pyXspec_showPlot, METH_VARARGS,"Wrapper for Xspec show plot."},
   {"doPlot",_pyXspec_doPlot, METH_VARARGS,"Display the plot."},

   {"allowPrompting",_pyXspec_allowPrompting, METH_VARARGS,"Toggle whether user prompting occurs."},
   {"closeLog",_pyXspec_closeLog, METH_VARARGS,"Close the Xspec log file."},
   {"getChatter",_pyXspec_getChatter, METH_VARARGS,"Get the console or log chatter level."},
   {"setChatter",_pyXspec_setChatter, METH_VARARGS,"Set the console or log chatter level."},
   {"getLog",_pyXspec_getLog, METH_VARARGS,"Get the currently opened log file."},
   {"setLog",_pyXspec_setLog, METH_VARARGS, "Open and set an Xspec log file."},

   {"doTclout",_pyXspec_doTclout, METH_VARARGS,"Call the tclout command handler."},
   {"getAbund",_pyXspec_getAbund, METH_VARARGS,"Get abundance table setting."},
   {"getModelStringValues",_pyXspec_getModelStringValues, METH_VARARGS,"Get the entire model string database."},
   {"getParallel",_pyXspec_getParallel, METH_VARARGS,"Get the number of parallel processes."},
   {"getPropDelta",_pyXspec_getPropDelta, METH_VARARGS,"Get the proportional fit delta value."},
   {"setModelStringValues",_pyXspec_setModelStringValues, METH_VARARGS,"Set the entire mdoel string database"},
   {"getXsect",_pyXspec_getXsect, METH_VARARGS,"Get the name of the photoelectric absorption cross-section"},
   {"showXset",_pyXspec_showXset, METH_VARARGS,"Show Xset settings."},

   {"doXspecCmd",_pyXspec_doXspecCmd, METH_VARARGS,"Generic Xspec handler function wrapper"},

   {NULL, NULL, 0, NULL}
};

#ifdef PYXSPEC3
   static struct PyModuleDef pyXspecmodule = {
      PyModuleDef_HEAD_INIT,
      "_pyXspec",
      NULL,
      -1,
      _PyXspecMethods
   };
   PyMODINIT_FUNC
   PyInit__pyXspec(void)
   {
      xspecLibInit();
      PyObject* m = PyModule_Create(&pyXspecmodule);
      Py_AtExit(exitXspec);
      return m;
   }
#else
   PyMODINIT_FUNC
   init_pyXspec(void)
   {
      (void) Py_InitModule("_pyXspec", _PyXspecMethods);
      // Anything that throws from here should cause immediate program termination.
      xspecLibInit();
      Py_AtExit(exitXspec);
   }
#endif

void xspecLibInit()
{
   using namespace XSGlobal;

   globalData = GlobalData::Instance();
   globalData->readSettings();

   XSstream::defineChannel(tcin, new PyxsIO(&std::cin,&std::cout), -32768);
   tcin.tie(&tcout);
   XSstream::defineChannel(tcout, new PyxsIO(0,&std::cout), bufSize);
   XSstream::defineChannel(tcerr, new PyxsIO(0,&std::cerr), 0);
   IosHolder::setStreams(&tcin, &tcout, &tcerr);

   createFunctionMap();
   createStdInterfaceCmdMap();

// create model & response containers. This will eventually be in the equivalent call to xs_start
// will also need to create data container.


   Parameter::initializeLinks();

   const string& datDir = globalData->managerDir();
   string modelDat = datDir + "/";
   modelDat += globalData->modDescripFile();

   XSModelFunction::updateComponentList(modelDat, true);

   // Now add mix models
   createxsmixFunctionMap();     
   XSModelFunction::updateComponentList(datDir + "/mixmodel.dat", true);

   registerFunctionUtility();

   DummyResponse* defaultEnergyArrayResponse (globalData->getDummyResponse());

   XSContainer::xsRegistry = XspecRegistry::Instance();

   XSContainer::datasets  = XSContainer::DataContainer::Instance();

   XSContainer::models    = XSContainer::ModelContainer::Instance();

   XSContainer::responses = XSContainer::ResponseContainer::Instance(defaultEnergyArrayResponse);

   // Provide a default initial seed to RandomGenerator singleton based on clock time.
   Numerics::DefaultRandomGenerator& randGen = 
           Numerics::DefaultRandomGenerator::instance();
   randGen.seed(time(0));
   randGen.initialize();

   // need to register Fit, Statistic and Weightings so we can check  
   // whether settings file makes any sense.
   registerNativeFitMethods();

   // also registers Statistical Weighting methods.
   registerNativeStatMethods();

   //registers all known table formats
   registerNativeTables();        

   // This is deliberately not a reference.  Want to store this 
   // even after globalData's method may be changed by init file.
   const string defaultMethod = globalData->method();      
   XSContainer::fit = Fit::Instance(defaultMethod);

   registerNativeRandomizingStrategies(XSContainer::fit);

   registerNativeLineLists();
   
   ProcessManager::initMaxProcsMap();

   // initialize user settings and interface
   // Even though version string won't be printed, we still want
   // to collect the version information.  Therefore set XSPEC_CURRENT
   // which is used by the XSutility function.
   string binaryLoc(getenv("HEADAS"));
   binaryLoc += "/bin/xspec";
   static string XSPEC_CURRENT("XSPEC_CURRENT=");
   XSPEC_CURRENT += binaryLoc;
   putenv(const_cast<char*>(XSPEC_CURRENT.c_str()));

   XSGlobal::startUp(false);

   return;
}

void exitXspec()
{
   XSGlobal::cleanUp();
   XSGlobal::nonTclAppClean();
}
