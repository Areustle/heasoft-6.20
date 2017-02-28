#include <XSUser/Python/xspec/PyModel.h>
#include <XSUser/Python/xspec/PyXSutils.h>

#include <XSContainer.h>
#include <XSstreams.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Parameter/Parameter.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>
#include <sstream>
#include <cstring>

namespace {
   PyObject* createModelTuple(const Model*);
   PyObject* getModelComps(const Model*);
}

PyObject*
_pyXspec_createModel(PyObject *self, PyObject *args)
{
   using namespace XSContainer;

   const char *exprStr=0;
   const char *modName=0;
   size_t sourceNum=0;
   if (!PyArg_ParseTuple(args,"ssI", &exprStr, &modName, &sourceNum))
   {
      string msg("Programmer Error: Invalid args in createModel function.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   if (XSparse::isBlank(exprStr))
   {
      string msg("Error: Model definition string is required.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   StringArray rawArgs;
   rawArgs.push_back(string("model"));
   string modNameStr;
   if (XSparse::isBlank(modName))
   {
      if (sourceNum != 1)
      {
         string msg("Error: When assigning to source > 1, model must be given a name.");
         PyErr_SetString(PyExc_Exception, msg.c_str());
         return NULL;
      }
      else
         modNameStr = Model::DEFAULT();
   }
   else
   {
      modNameStr = string(modName);
      std::ostringstream oss;
      oss << sourceNum << ":" << modNameStr;
      rawArgs.push_back(oss.str());
   }
   rawArgs.push_back(string(exprStr));
   rawArgs.push_back("& /*");
   if (XSGlobal::doModel(rawArgs) < 0)
   {
      PyErr_SetString(PyExc_Exception, "Model Command Error");
      return NULL;
   }

   // The lowest dg copy is all we need.
   Model* newModel = models->lookup(modNameStr);
   if (!newModel)
   {
      string msg("Programmer Error: Cannot access new model object");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   return createModelTuple(newModel);
}  // end createModel

PyObject* _pyXspec_fluxCmd(PyObject *self, PyObject *args)
{
   int isFlux=0;
   PyObject* inList=0;
   if (!PyArg_ParseTuple(args, "iO", &isFlux, &inList))
   {
      string msg("Programmer Error: Parsing flux command");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   StringArray rawArgs;
   if (isFlux)
      rawArgs.push_back("flux");
   else
      rawArgs.push_back("lumin");
   PY_SZ_TYPE nArgs = PyList_Size(inList);
   for (PY_SZ_TYPE i=0; i<nArgs; ++i)
      rawArgs.push_back(PYXSSTRING_ASSTRING(PyList_GetItem(inList, i)));
   if (XSGlobal::doFlux(rawArgs) < 0)
   {
      PyErr_SetString(PyExc_Exception, "Flux Command Error");
      return NULL;
   }
   return Py_BuildValue("i",0);
}
// end fluxCmd

PyObject* _pyXspec_getArray(PyObject *self, PyObject *args)
{
   PyObject* handle=0;
   size_t specNum=0;
   int isFolded=0;
   if (!PyArg_ParseTuple(args, "OIi", &handle, &specNum, &isFolded))
   {
      string msg("Programmer Error: Cannot parse getFlux arguments.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   Model* mod = PyXSutils::verifyModelHandle(handle);
   if (!mod)
      return NULL;
   PyObject* list=0;
   try
   {
      RealArray noticedFolded;
      if (isFolded)
      {
         // This throws if no folded model for specNum.
         const RealArray& folded = mod->foldedModel(specNum);
         const SpectralData* spec = XSContainer::datasets->lookup(specNum);
         if (!spec)
         {
            string msg("Programmer Error: Can't find spectrum in _getArray.");
            PyErr_SetString(PyExc_Exception, msg.c_str());
            return NULL;
         }
         // If/when SpectralData's indirectNotice array gets automatically
         // updated when channels change, won't need to do this here.
         std::valarray<size_t> indirectNotice;
         spec->buildIndirectNotice(indirectNotice);
         noticedFolded.resize(indirectNotice.size());
         noticedFolded = folded[indirectNotice];
      }
      const RealArray& array = isFolded ? noticedFolded :
                                          mod->modelFlux(specNum);
      list = PyList_New(array.size());
      for (PY_SZ_TYPE i=0; i<static_cast<PY_SZ_TYPE>(array.size()); ++i)
      {
         PyObject* val = PyFloat_FromDouble(array[i]);
         PyList_SetItem(list, i, val);
      }
   }
   catch (...)
   {
      string msg("Error: Cannot retrieve array for this model and spectrum number.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   return list;
}
// end getArray

PyObject*
_pyXspec_getComponentPars(PyObject *self, PyObject *args)
{
   const char* modName=0;
   int iComp=0;  
   PyObject* paramList=0;

   try
   {
      if (!PyArg_ParseTuple(args, "si", &modName, &iComp))
         throw YellowAlert();
      string modNameStr = XSparse::isBlank(modName) ? 
                   string("_DEFAULT") : string(modName);

      // The lowest dg copy will suffice.
      const Model* mod = XSContainer::models->lookup(modNameStr);
      if (!mod)
         throw YellowAlert();
      std::vector<Component*> modComps;
      mod->bundleComponents(modComps);
      if (iComp < 1 || iComp > (int)modComps.size())
         throw YellowAlert();
      const Component* comp = modComps[iComp-1];
      const std::vector<Parameter*>& params = comp->parameterSet();
      paramList = PyList_New(params.size());
      for (PY_SZ_TYPE i=0; i<static_cast<PY_SZ_TYPE>(params.size()); ++i)
      {
         const Parameter* par = params[i];
         const string& nameStr = par->name();
         PyObject* pyNameStr = PYXSSTRING_FROMSTRING(nameStr.c_str());
         PyList_SetItem(paramList, i, pyNameStr);
      }

   }
   catch (...)
   {
      string msg("Programmer Error: Failed to locate parameters for model in getComponentPars.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }

   return paramList;
} 
// end getComponentPars

PyObject* _pyXspec_getModelFluxLuminCalc(PyObject *self, PyObject *args)
{
   PyObject* handle=0;
   int isFlux=0;
   if (!PyArg_ParseTuple(args, "Oi", &handle, &isFlux))
   {
      string msg("Programmer Error: Cannot parse getModelFluxLuminCalc arguments.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   Model* mod = PyXSutils::verifyModelHandle(handle);
   if (!mod)
      return NULL;
   const SpectralData::FluxCalc& fluxCalc = isFlux ?
                mod->lastModelFluxCalc() : mod->lastModelLuminCalc();
   PyObject* fluxTuple = PyTuple_New(6);
   PyTuple_SetItem(fluxTuple, 0, PyFloat_FromDouble(fluxCalc.value));
   PyTuple_SetItem(fluxTuple, 1, PyFloat_FromDouble(fluxCalc.errLow));
   PyTuple_SetItem(fluxTuple, 2, PyFloat_FromDouble(fluxCalc.errHigh));
   PyTuple_SetItem(fluxTuple, 3, PyFloat_FromDouble(fluxCalc.photonValue));
   PyTuple_SetItem(fluxTuple, 4, PyFloat_FromDouble(fluxCalc.photonLow));
   PyTuple_SetItem(fluxTuple, 5, PyFloat_FromDouble(fluxCalc.photonHigh));       
   return fluxTuple;
}
// end getModelFluxLuminCalc

PyObject*
_pyXspec_getModelFromNameAndGroup(PyObject *self, PyObject *args)
{
   const char* modName=0;
   size_t groupNum=0;
   if (!PyArg_ParseTuple(args, "sI", &modName, &groupNum))
   {
      string msg("Programmer Error: Parsing getModel args.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   // We could do lookup with just one function call to ModelContainer,
   // but let's do it in 2 stages to provide better error messages
   // in case model isn't there.
   Model* mod=0;
   string modNameStr(modName);
   std::vector<Model*> modGroups = 
        XSContainer::models->lookupModelGroup(modNameStr);
   if (!modGroups.size())
   {
      string msg("Error: ");
      if (modNameStr.empty())
         msg += "Model is not currently loaded in XSPEC.";
      else
         msg += "Model with name: " + modNameStr
                + " is not currently loaded in XSPEC.";
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   else
   {
      if (modNameStr.empty())
         modNameStr = "_DEFAULT";
      mod = XSContainer::models->lookup(modNameStr, groupNum);
   }   
   if (!mod)
   {
      const string errName = (modNameStr == string("_DEFAULT")) ?
              string("unnamed model") : string("model name: ")+modNameStr;
      std::ostringstream oss;
      oss <<"Error: No model object exists for " << errName 
          << ", data group " << groupNum;
      PyErr_SetString(PyExc_Exception, oss.str().c_str());
      return NULL;
   }
   void* handle = static_cast<void*>(mod);
   return PYXSCOBJECT_FROMVOIDPTR(handle,NULL,NULL);
} // end getModelFromNameAndGroup

PyObject* _pyXspec_getModelSourceAssignments(PyObject *self, PyObject *args)
{
   using namespace XSContainer;
   
   PyObject* dict=PyDict_New();
   const size_t allSources = datasets->numSourcesForSpectra();
   for (size_t i=0; i<allSources; ++i)
   {
      string modName(models->lookupModelForSource(i+1));
      if (modName.length())
      {
         if (modName == Model::DEFAULT())
            modName = "";
         const long iSource = static_cast<long>(i)+1;
         PyObject* pySourceNum = PYXSINT_FROMLONG(iSource);
         PyObject* pyModName = PYXSSTRING_FROMSTRING(modName.c_str());
         PyDict_SetItem(dict, pySourceNum, pyModName);
      }
   }
   
   return dict;
}

PyObject* _pyXspec_getModelTuple(PyObject *self, PyObject *args)
{
   PyObject* handle=0;
   if (!PyArg_ParseTuple(args, "O", &handle))
   {
      string msg("Programmer Error: Cannot convert model handle.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   Model* mod = PyXSutils::verifyModelHandle(handle);
   if (!mod)
      return NULL;
   return createModelTuple(mod);
} // end getModelTuple

PyObject* _pyXspec_getSpectraForModel(PyObject *self, PyObject *args)
{
   PyObject* indexTuple=0;
   PyObject* handle=0;
   if (!PyArg_ParseTuple(args, "O", &handle))
   {
      string msg("Programmer Error: Cannot convert model handle.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const Model* mod = PyXSutils::verifyModelHandle(handle);
   if (!mod)
      return NULL;
   const size_t nSpecs = mod->modelFlux().size();
   if (nSpecs)
   {
      size_t i=0;
      indexTuple = PyTuple_New(nSpecs);
      ArrayContainer::const_iterator itFlux = mod->modelFlux().begin();
      ArrayContainer::const_iterator itEnd = mod->modelFlux().end();
      while (itFlux != itEnd)
      {
         const long specIdx = static_cast<long>(itFlux->first);
         PyTuple_SetItem(indexTuple, i, PYXSINT_FROMLONG(specIdx));
         ++itFlux, ++i;      
      }
   }
   return indexTuple;
}
// end getSpectraForModel

PyObject* _pyXspec_localModel(PyObject *self, PyObject *args)
{
   const char* packageName=0;
   const char* dirPath=0;
   if (!PyArg_ParseTuple(args, "ss", &packageName, &dirPath))
   {
      string errMsg("Programmer Error: Parsing localModel");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   if (!strlen(packageName))
   {
      // Assume user wants to by-pass lmod mechanism and call 'load'.
      // In this case dirPath should be the full path including the
      //   local model library name.
      StringArray rawArgs(2);
      rawArgs[0] = string("load");
      rawArgs[1] = string(dirPath);
      if (XSGlobal::doLoad(rawArgs))
      {
         string errMsg("Error attempting to load local model library.");
         PyErr_SetString(PyExc_Exception, errMsg.c_str());
         return NULL;
      }
   }
   else
   {
      StringArray rawArgs(2);
      rawArgs[0] = string("lmod");
      rawArgs[1] = string(packageName);
      string dirPathStr(dirPath);
      if (dirPathStr.length())
         rawArgs.push_back(dirPathStr);
      if (XSGlobal::doLmod(rawArgs))
      {
         string errMsg("Error attempting to load local model library.");
         PyErr_SetString(PyExc_Exception, errMsg.c_str());
         return NULL;
      }
   }
   return Py_BuildValue("i",0);
}
// end localModel

PyObject* _pyXspec_removeModels(PyObject *self, PyObject *args)
{
   const char* modName=0;
   if (!PyArg_ParseTuple(args, "s", &modName))
   {
      string errMsg("Programmer Error: Cannot parse removeModel");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   const string modNameStr(modName);
   StringArray rawArgs;
   rawArgs.push_back("model");
   if (modNameStr.empty())
      rawArgs.push_back("clear");
   else
   {
      rawArgs.push_back(modNameStr);
      rawArgs.push_back("none");
   }
   if (XSGlobal::doModel(rawArgs) < 0)
   {
      PyErr_SetString(PyExc_Exception, "Model removal error.");
      return NULL;
   }

   return Py_BuildValue("i",1);
}
// end removeModels



PyObject* _pyXspec_showModel(PyObject *self, PyObject *args)
{
   PyObject* handle=0;
   if (!PyArg_ParseTuple(args, "O", &handle))
   {
      string msg("Programmer Error: Cannot get model object handle.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const Model* mod = PyXSutils::verifyModelHandle(handle);
   if (!mod)
      return NULL;
   mod->printHeading();
   tcout << *mod;
   mod->printMixComp();
   tcout << string(72,'_') << '\n' << std::endl;

   return Py_BuildValue("i",1);
}


namespace {

PyObject* createModelTuple(const Model* mod)
{
   PyObject* pyVals = PyTuple_New(7);
   Model* ncMod = const_cast<Model*>(mod);
   PyObject* handle = PYXSCOBJECT_FROMVOIDPTR(static_cast<void*>(ncMod),NULL,NULL);
   PyTuple_SetItem(pyVals, 0, handle);
   PyTuple_SetItem(pyVals, 1, getModelComps(mod));
   long dgNumber = static_cast<long>(mod->dataGroupNumber());
   long sourceNumber = static_cast<long>(mod->sourceNumber());
   long startParIdx = 1 + static_cast<long>(mod->parameterIndexBase());
   long numberOfPars = static_cast<long>(mod->numberOfParameters());
   PyTuple_SetItem(pyVals, 2, PYXSINT_FROMLONG(dgNumber));
   PyTuple_SetItem(pyVals, 3, PYXSINT_FROMLONG(sourceNumber));
   PyTuple_SetItem(pyVals, 4, PYXSINT_FROMLONG(startParIdx));
   PyTuple_SetItem(pyVals, 5, PYXSINT_FROMLONG(numberOfPars));
   PyTuple_SetItem(pyVals, 6, PYXSSTRING_FROMSTRING(mod->fullExpression().c_str()));
   return pyVals;
}


PyObject* getModelComps(const Model* mod)
{
   std::vector<Component*> comps;
   mod->bundleComponents(comps);
   PY_SZ_TYPE nComps = static_cast<PY_SZ_TYPE>(comps.size());
   PyObject* compList = PyList_New(nComps);
   for (PY_SZ_TYPE i=0; i<nComps; ++i)
   {
      PyObject* pyStr = PYXSSTRING_FROMSTRING(comps[i]->name().c_str());
      // This function transfers ownership to the list container.
      PyList_SetItem(compList, i, pyStr);
   }
   return compList;
} // end getModelComps

} // end unnamed namespace
