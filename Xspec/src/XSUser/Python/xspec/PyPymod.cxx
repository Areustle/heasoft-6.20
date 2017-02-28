#include <XSUser/Python/xspec/PyPymod.h>
#include <XSUser/Python/xspec/PyXSutils.h>
#include <XSUtil/Error/Error.h>
#include <XSstreams.h>
#include <limits>


PyObject* _pyXspec_addPyComp(PyObject *self, PyObject *args)
{
   PyObject *pyfunc=0, *parInfoTuple=0, *doErr=0, *specDep=0;
   const char* funcName=0, *compType=0;
   if (!PyArg_ParseTuple(args,"OOsOOs",&pyfunc,&parInfoTuple,&compType,&doErr,&specDep,&funcName))
   {
      string msg("Programmer Error: Parsing addPyComp command");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   // Assume args have already been checked for proper types in the callling
   //  Python function.
   const PY_SZ_TYPE nPars = PyTuple_Size(parInfoTuple);
   std::vector<string> parStrings(nPars);
   for (PY_SZ_TYPE iPar=0; iPar<nPars; ++iPar)
   {
      // GetItem returns a borrowed reference.
      PyObject* pyParInfo = PyTuple_GetItem(parInfoTuple, iPar);
      parStrings[iPar] = string(PYXSSTRING_ASSTRING(pyParInfo));
   }
   bool isErrCalc = (doErr == Py_True);
   bool isSpecDep = (specDep == Py_True);
   string funcNameStr=string(funcName);
   
   // If model with same name exists, replace it.
   ModelFunctionMap::iterator itFuncMap = XSFunctionMap.find(funcNameStr);
   if (itFuncMap != XSFunctionMap.end())
   {
      tcout <<"\n...Replacing previous model component which had name: "
         << funcNameStr <<std::endl<<std::endl;
      delete itFuncMap->second;
      XSFunctionMap.erase(itFuncMap);
      // If function already existed in function map, it better
      //   also have a counterpart in the name cache.
      NameCacheType::iterator itNameCache = 
        XSModelFunction::exactMatch(funcNameStr);
      if (itNameCache == XSModelFunction::nameCache().end())
      {
         throw RedAlert("Corrupted XSPEC model name data base.");
      }
      XSModelFunction::nameCache().erase(itNameCache);
      
      // Only pre-existing models that are also Python models will
      //  have an entry in parInfoCache.
      ParInfoContainer& parCache = XSModelFunction::parInfoCache();
      ParInfoContainer::iterator itParCache = parCache.find(funcNameStr);
      if (itParCache != parCache.end())
      {
         parCache.erase(itParCache);
      }
   }
      
   XSFunctionMap[funcNameStr] = new XSCall<PyObject>(pyfunc);
   ComponentInfo compInfo(funcNameStr,string(""),string::npos,string(compType),
                isErrCalc, string(""), false);
   compInfo.isPythonModel = true;
   compInfo.isSpecDependent = isSpecDep;
   XSModelFunction::nameCache(XSutility::lowerCase(funcNameStr.substr(0,2)),
        compInfo);
                
   XSModelFunction::parInfoCache()[funcNameStr] = parStrings;
   
   return Py_BuildValue("i",0);
}

template <>
void XSCall<PyObject>::operator() (const RealArray& energyArray, const RealArray& params,
                        int spectrumNumber, RealArray& fluxArray, RealArray& fluxErrArray, 
                        const string& initString) const
{
   PyObject *pFuncArgs = PyObject_GetAttrString(PyFunction_GetCode(m_generator), "co_argcount");
   const long nFuncArgs = PYXSINT_ASLONG(pFuncArgs);
   const long NMANDATORY_ARGS = 3;
   Py_DECREF(pFuncArgs);
   
   // Assume user's function has been checked (during the addPyComp) stage
   //  to have AT LEAST 3 input args, for the mandatory engs, params, and flux.
   PyObject *pArgs=PyTuple_New(nFuncArgs);
   const PY_SZ_TYPE nEngs = energyArray.size();
   const PY_SZ_TYPE nPars = params.size();
   PyObject* engTuple = PyTuple_New(nEngs);
   for (PY_SZ_TYPE i=0; i<nEngs; ++i)
      PyTuple_SetItem(engTuple, i, PyFloat_FromDouble(energyArray[i]));
   PyTuple_SetItem(pArgs, 0, engTuple);
   PyObject* paramTuple = PyTuple_New(nPars);
   for (PY_SZ_TYPE i=0; i<nPars; ++i)
      PyTuple_SetItem(paramTuple, i, PyFloat_FromDouble(params[i]));
   PyTuple_SetItem(pArgs, 1, paramTuple);
   
   const size_t nBins = nEngs-1;
   // Can't assume size of incoming fluxArray.  It may be zero or nEngs-1
   //  depending on the context.  If it is for a 'con' or 'mix' component, 
   //  it definitely won't be size zero.
   PyObject *fluxList = 0;
   if (!fluxArray.size() || fluxArray.size() == nBins)
   {
      fluxList = PyList_New(nBins);
      if (fluxArray.size())
      {
         for (size_t i=0; i<nBins; ++i)
         {
            PyObject* flux = PyFloat_FromDouble(fluxArray[i]);
            PyList_SetItem(fluxList, (PY_SZ_TYPE)i, flux);
         }
      }
      else
      {
         for (size_t i=0; i<nBins; ++i)
         {
            PyObject* flux = PyFloat_FromDouble(0.0);
            PyList_SetItem(fluxList, (PY_SZ_TYPE)i, flux);            
         }
      }
   }
   else
   {
      throw RedAlert("Programmer Error: Improper sized flux array passed to PyXspec");
   }
   PyTuple_SetItem(pArgs, 2, fluxList);
   // End building tuple for mandatory arguments.
   
   PyObject *fluxErrList=0;
   if (nFuncArgs > NMANDATORY_ARGS)
   {
      // Send an empty fluxErr array.  If it comes back filled, we'll use it.
      fluxErrList = PyList_New(0);
      PyTuple_SetItem(pArgs, 3, fluxErrList);
      if (nFuncArgs > NMANDATORY_ARGS+1)
      {
         PyObject *pySpecNum = PYXSINT_FROMLONG(static_cast<long>(spectrumNumber));
         PyTuple_SetItem(pArgs, 4, pySpecNum);
      }
   }
      
   bool isFuncOK=true;
   string errMsg;
   fluxArray.resize(nBins);
   fluxErrArray.resize(0);
   PY_SZ_TYPE nFluxErr=0;
   PyObject* status=PyObject_CallObject(m_generator, pArgs);
   if (!status)
   {
      errMsg="***Error: Cannot perform model calculation in user-defined Python model.";
      isFuncOK=false;
   }
   else
   {
      if (PySequence_Size(fluxList) != static_cast<PY_SZ_TYPE>(nBins))
      {
         errMsg="***Error: User-defined Python model did not fill flux array to proper size.";
         isFuncOK=false;
      }
      if (fluxErrList)
      {
         // If here, users' function includes a fluxerr arg.
         // However that doesn't necessarily mean they've used it.
         // It could just be a dummy place holder to allow use
         // of later args (ie. specNum). If size=nEngs-1, assume
         // user has properly filled it.  If size=0, assume it's a
         // dummy.  Any other size is an error.
         nFluxErr=PySequence_Size(fluxErrList);
         if (nFluxErr && nFluxErr != static_cast<PY_SZ_TYPE>(nBins))
         {
            errMsg="***Error: User-defined Python model fills fluxError array to wrong size.";
            isFuncOK=false;
         }
      }
   }   
   if (isFuncOK)
   {
      for (PY_SZ_TYPE i=0; i<static_cast<PY_SZ_TYPE>(nBins); ++i)
      {
         PyObject* flux = PyList_GetItem(fluxList, i);
         fluxArray[i] = PyFloat_AsDouble(flux);
      }
      if (nFluxErr)
      {
         fluxErrArray.resize(nBins);
         for (PY_SZ_TYPE i=0; i<static_cast<PY_SZ_TYPE>(nBins); ++i)
         {
            PyObject* fluxErr = PyList_GetItem(fluxErrList, i);
            fluxErrArray[i] = PyFloat_AsDouble(fluxErr);
         }
      }
   }
   else
   {
      fluxArray = std::numeric_limits<Real>::quiet_NaN();
      tcerr<<errMsg<<std::endl;
   }    
   Py_DECREF(pArgs);
   if (status)
   {
      Py_DECREF(status);
   }
}
