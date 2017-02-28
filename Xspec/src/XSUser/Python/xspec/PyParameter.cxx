#include <XSUser/Python/xspec/PyParameter.h>
#include <XSUser/Python/xspec/PyXSutils.h>

#include <XSContainer.h>
#include <XSFit/Fit/Fit.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSModel/Parameter/Parameter.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Error/Error.h>
#include <sstream>
#include <set>

namespace {
   PyObject* createParTuple(const Parameter*);

   // This returns the standard Xspec parID string for a python Parameter
   // object, so that it may pass things to a standard Xspec command handler.
   // Note that this will also VERIFY that the parent python object (and the
   // python Parameter object itself if this is a response parameter) still
   // corresponds to a stored C++ object.  If not, this returns an EMPTY string.
   string getParIDString(PyObject* parent, const int relativeIdx, const int isMod);
   
   // This returns an error string if it can't find one or more parameters,
   // if it has a problem setting ANY of the parameter values, or if
   // for some reason it can't temporarily save the current values. 
   // It returns an empty string if all goes well.  If an error has occurred,
   // all parameters will be restored to their original values.
   string commonSetPars(const std::vector<string>& modNames,
                     const std::vector<int>& parIndices,
                     const std::vector<string>& values);
}

PyObject*
_pyXspec_getParTuple(PyObject *self, PyObject *args)
{
   PyObject* handle=0;
   int iPar=0;
   int iType=0;
   PyObject* paramTuple=0;
   string parentType("Model");
   if (!PyArg_ParseTuple(args, "Oii", &handle, &iPar, &iType))
   {
      string msg("Programmer Error: Cannot parse getParTuple.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   try
   {
      if (iType == 1)
      {
         parentType = "Response";
         const Response* resp = PyXSutils::verifyResponseHandle(handle);
         if (!resp)
            return NULL;
         // Unlike for model parameters, we can't assume the response
         // parameter exists for the lifetime of the response object.
         
         // This is hardcoded specifically for gain functions:
         //   slope: iPar=1, offset: iPar=2
         //   These values must be consistent with indices used during
         //   Python response parameter construction.
         const ResponseParam* rpar = (iPar==2) ? resp->getConstGain() :
                                       resp->getLinearGain();
         if (!rpar)
         {
            string err("Error: Response currently has no gain parameters");
            PyErr_SetString(PyExc_Exception, err.c_str());
            return NULL;
         }
         paramTuple = createParTuple(rpar);
      }
      else
      {
         const Model* mod = PyXSutils::verifyModelHandle(handle);
         if (!mod)
            return NULL;
         iPar += static_cast<int>(mod->parameterIndexBase());
         const Parameter* par = XSContainer::models->lookupParameter(iPar, mod->name());
         if (!par)
            throw YellowAlert();
         paramTuple = createParTuple(par);
      }
   }
   catch (...)
   {
      std::ostringstream oss;
      oss << "Programmer Error: Cannot retrieve parameter " << iPar 
          << " from " << parentType << " object."
          << std::endl; 
      PyErr_SetString(PyExc_Exception, oss.str().c_str());
      return NULL;
   }
   return paramTuple;
} 
// end getParTuple

PyObject* _pyXspec_newparCmd(PyObject *self, PyObject *args)
{
   const char* expression=0;
   int iCmd=0;
   if (!PyArg_ParseTuple(args, "si", &expression, &iCmd))
   {
      string msg("Programmer Error: Cannot parse newparCmd input.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const string expressionStr(expression);
   const bool isRespPar = static_cast<bool>(iCmd);
   string dummy1;
   IntegerArray dummy2;
   try
   {
      XSGlobal::doNewpar(expressionStr, isRespPar, dummy1, dummy2);
   }
   catch (...)
   {
      string msg("Error: Cannot set newpar command: ");
      msg += expressionStr;
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   
   return Py_BuildValue("i",0);
}

PyObject* _pyXspec_setParBayes(PyObject *self, PyObject *args)
{
   PyObject* objHandle=0;
   int iPar=0;
   const char* type=0;
   PyObject* hyperList=0;
   int isMod=0;
   if (!PyArg_ParseTuple(args, "OisOi", &objHandle, &iPar, &type,
                &hyperList, &isMod))
   {
      string msg("Programmer Error: Cannot parse setParBayes input.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }

   const string idxStr(getParIDString(objHandle, iPar, isMod));
   if (!idxStr.length())
   {
      string errMsg("Error: Parameter object is no longer valid");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   
   const string priorType(type);
   StringArray rawArgs(3);
   rawArgs[0] = "bayes";
   rawArgs[1] = idxStr;
   rawArgs[2] = priorType;
   
   if (!isMod)
   {
      // Bayes command uses [<sourceNum>:]r<parNum> notation for resp pars.
      string::size_type colonPos = idxStr.find(":");
      if (colonPos == string::npos)
         rawArgs[1].insert(0, 1, 'r');
      else
         rawArgs[1].insert(colonPos+1, 1, 'r');       
   }
   PY_SZ_TYPE nHyper = PyList_Size(hyperList);
   for (PY_SZ_TYPE i=0; i<nHyper; ++i)
   {
      // Borrowed reference
      PyObject* hypObj = PyList_GetItem(hyperList, i);
      rawArgs.push_back(string(PYXSSTRING_ASSTRING(hypObj)));
   }
   if (XSGlobal::doBayes(rawArgs))
   {
      string msg("Error executing bayes command");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }   
   return Py_BuildValue("i",1);
}

PyObject* _pyXspec_setParFreeze(PyObject *self, PyObject *args)
{
   PyObject* objHandle=0;
   int iPar=0;
   int input=0;
   int isMod=0; 
   if (!PyArg_ParseTuple(args, "Oiii", &objHandle, &iPar, &input, &isMod))
   {
      string errMsg("Programmer Error: Cannot parse setParFreeze input");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   
   const string idxStr(getParIDString(objHandle, iPar, isMod));
   if (!idxStr.length())
   {
      string errMsg("Error: Parameter object is no longer valid");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   
   StringArray cmdArgs(2);
   cmdArgs[1] = idxStr;
   int status=0;
   if (input == 1)
   {
      const string xsCmd = isMod ? string("freeze") : string("rfreeze");
      cmdArgs[0] = xsCmd;
      status = XSGlobal::doFreeze(cmdArgs);
   }
   else
   {
      const string xsCmd = isMod ? string("thaw") : string("rthaw");
      cmdArgs[0] = xsCmd;
      status = XSGlobal::doThaw(cmdArgs);
   }
   if (status)
   {
      string errMsg("Error while attempting to change frozen/thaw status of parameter");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;   
   }    
   return Py_BuildValue("i",1);
}
// end setParFreeze

PyObject* _pyXspec_setParLink(PyObject *self, PyObject *args)
{
   PyObject* objHandle=0;
   const char* input=0;
   int iPar=0;
   int isMod=0;
   
   if (!PyArg_ParseTuple(args, "Oisi", &objHandle, &iPar, &input, &isMod))
   {
      string errMsg("Programmer Error: Cannot parse setParLink input");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   
   const string idxStr(getParIDString(objHandle, iPar, isMod));
   if (!idxStr.length())
   {
      string errMsg("Error: Parameter object is no longer valid");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
      
   const string inputStr(input);
   if (!inputStr.length())
   {
      // Remove link (if any)
      StringArray cmdArgs(2);
      cmdArgs[0] = isMod ? string("untie") : string("runtie");
      cmdArgs[1] = idxStr;
      int status = XSGlobal::doUntie(cmdArgs);
      if (status)
      {
         string errMsg("Error while attempting to untie parameter");
         PyErr_SetString(PyExc_Exception, errMsg.c_str());
         return NULL;   
      }
   }
   else
   {
      string newparStr(idxStr + " " + inputStr);
      // These variables are only needed in the context of the xsNewpar handler.
      string dummy1;
      IntegerArray dummy2;
      try
      {
         XSGlobal::doNewpar(newparStr, !isMod, dummy1, dummy2);
      }
      catch (...)
      {
         string msg("Error: Cannot set parameter ");
         msg += idxStr;
         msg += " with string: ";
         msg += input;
         PyErr_SetString(PyExc_Exception, msg.c_str());
         return NULL;
      }
   }
   return Py_BuildValue("i",1);
}
// end setParLink

PyObject* _pyXspec_setPars(PyObject *self, PyObject *args)
{
   PyObject* modHandle=0;
   PyObject* parIdxTuple=0;
   PyObject* valsTuple=0;
   int areLocal=0;
   
   if (!PyArg_ParseTuple(args, "OOOi", &modHandle, &parIdxTuple,
                &valsTuple, &areLocal))
   {
      string errMsg("Programmer Error: Cannot parse setPars input");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   Model* mod = PyXSutils::verifyModelHandle(modHandle);
   if (!mod)
      return NULL;
      
   const PY_SZ_TYPE nPars = PySequence_Size(parIdxTuple);
   const string modName = (mod->name() == Model::DEFAULT()) ?
                  string("") : mod->name();
   const std::vector<string> modNames(nPars,modName);
   std::vector<int> parIndices;
   std::vector<string> values;
   for (PY_SZ_TYPE i=0; i<nPars; ++i)
   {
      int iPar = static_cast<int>(PyLong_AsLong(PyTuple_GetItem(parIdxTuple, i)));
      if (areLocal)
      {
         // Convert parIdx from local to global.
         // parameterIndexBase is 0-based while iPar is 1-based.
         iPar += mod->parameterIndexBase();
      }
      parIndices.push_back(iPar);
      const string valString(PYXSSTRING_ASSTRING(PyTuple_GetItem(valsTuple, i)));
      values.push_back(valString);
   }
   string status = commonSetPars(modNames, parIndices, values);
   if (status.length())
   {
      PyErr_SetString(PyExc_Exception, status.c_str());
      return NULL;
   }
   return Py_BuildValue("i",1);
}
// end setPars

PyObject* _pyXspec_setParsGlobal(PyObject *self, PyObject *args)
{
   PyObject* namesTuple=0;
   PyObject* parIdxTuple=0;
   PyObject* valsTuple=0;
   
   if (!PyArg_ParseTuple(args, "OOO", &namesTuple, &parIdxTuple,
                &valsTuple))
   {
      string errMsg("Programmer Error: Cannot parse setParsGlobal input");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }   
   std::vector<string> modNames;
   std::vector<int> parIndices;
   std::vector<string> values;
   const PY_SZ_TYPE nPars = PySequence_Size(namesTuple);
   for (PY_SZ_TYPE i=0; i<nPars; ++i)
   {
      modNames.push_back(PYXSSTRING_ASSTRING(PyTuple_GetItem(namesTuple,i)));
      parIndices.push_back(static_cast<int>(PyLong_AsLong(PyTuple_GetItem(parIdxTuple,i))));
      values.push_back(PYXSSTRING_ASSTRING(PyTuple_GetItem(valsTuple,i)));
   }
   
   string status = commonSetPars(modNames, parIndices, values);
   if (status.length())
   {
      PyErr_SetString(PyExc_Exception, status.c_str());
      return NULL;
   }
   return Py_BuildValue("i",1);
}
// setParsGlobal

PyObject* _pyXspec_setRespPars(PyObject *self, PyObject *args)
{
   PyObject* respHandle=0;
   PyObject* parIdxTuple=0;
   PyObject* valsTuple=0;
   
   if (!PyArg_ParseTuple(args, "OOO", &respHandle, &parIdxTuple,
                &valsTuple))
   {
      string errMsg("Programmer Error: Cannot parse setRespPars input");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   Response* resp = PyXSutils::verifyResponseHandle(respHandle);
   if (!resp)
      return NULL;
   const size_t sourceNum = resp->sourceNumber();
   std::ostringstream oss;
   oss << sourceNum <<":";
      
   // Until the C++ Response class is able to handle more general response
   // functions, we'll have to assume it is using 'gain'. (It has no way
   // to tell us otherwise.)  And since we're assuming that, we may as well
   // assume that multiple pars must be consecutive (there are only 2 after
   // all), and so we can go ahead and use 'rnewpar'.
   
   const PY_SZ_TYPE nPars = PySequence_Size(parIdxTuple);
   if (nPars < 1 || nPars > 2)
   {
      string errMsg("Programmer Error: Only 1 or 2 pars allowed in setRespPars");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   
   // parIdxTuple val is 1-based and relative to the particular Response object
   // (ie. it's either 1 (slope) or 2 (offset)).  We need its global index here.
   if (!resp->getLinearGain())
   {
      // User may be holding a reference to an obsolete parameter, even though
      // its parent Response is still valid.
      string errMsg("Error: Response parameter object is no longer valid.");
      PyErr_SetString(PyExc_Exception, errMsg.c_str());
      return NULL;
   }
   const size_t iOffset = resp->getLinearGain()->index();
   const size_t iPar = iOffset - 1
          + static_cast<size_t>(PyLong_AsLong(PyTuple_GetItem(parIdxTuple, 0)));
   oss << iPar;
   if (nPars > 1)
   {
      oss << "-" << iPar+1;
   }      
   string valString(PYXSSTRING_ASSTRING(PyTuple_GetItem(valsTuple, 0)));
   oss << " " << valString;
   if (nPars > 1)
   {
      oss << " & ";
      valString = PYXSSTRING_ASSTRING(PyTuple_GetItem(valsTuple, 1));
      oss << valString;
   }
   string rnewparStr(oss.str());
   const bool isRespPar = true;
   // These variables are only needed in the context of the xsNewpar handler.
   string dummy1;
   IntegerArray dummy2;
   try
   {
      XSGlobal::doNewpar(rnewparStr, isRespPar, dummy1, dummy2);
   }
   catch (...)
   {
      string msg("Error: Unable to set response parameter(s).");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   
   return Py_BuildValue("i",1); 
}
// end setRespPars

PyObject* _pyXspec_showPar(PyObject *self, PyObject *args)
{
   StringArray rawArgs(2);
   rawArgs[0] = string("show");
   rawArgs[1] = string("par");
   // args(0) list can contain pieces of paramIDs including range
   // specifiers (with the commas and hyphens that implies).
   if (!PyXSutils::PyListToXSArgs(args, rawArgs))
      return NULL;
   string dummy;
   XSGlobal::doShow(rawArgs, dummy);
   return Py_BuildValue("i",1);
}


namespace {

PyObject* createParTuple(const Parameter* par)
{
   PyObject* pyVal = PyTuple_New(11);
   PyObject* valList = PyList_New(0);
   PyObject* modVal = PyFloat_FromDouble(par->value());
   PyList_Append(valList,modVal);
   Py_DECREF(modVal);
   double sigma=-1.0;
   string units;
   double errLow=0.0, errHigh=0.0;
   string errCmdCode;
   string priorTypeStr;
   RealArray hyperPars;
   PyObject* hyperList = PyList_New(0);
   if (const ModParam* modPar = dynamic_cast<const ModParam*>(par))
   {
      const char valTypes[]={'d','l','b','t','h'};
      const size_t nTypes=5;
      for (size_t iType=0; iType<nTypes; ++iType)
      {
         double dval = modPar->value(valTypes[iType]);
         if (valTypes[iType] == 'd')
         {
            // Xspec stores a positive delta even when par is frozen,
            // but the user should see it as negative.
            if (modPar->isFrozen() && dval > 0.0)
               dval *= -1.0;
         }
         PyObject* subVal = PyFloat_FromDouble(dval);
         PyList_Append(valList, subVal);
         Py_DECREF(subVal);
      }
      sigma = modPar->sigma();
      // Only modPars have a units string
      units = modPar->unit();

      // Only modPars can have error command output or
      // Bayesian priors.
      errLow = modPar->emn();
      errHigh = modPar->epe();
      errCmdCode = modPar->lastErrorStatus();
      switch (modPar->priorType())
      {
         case ModParam::CONS:
            priorTypeStr = "CONS";
            break;
         case ModParam::EXP:
            priorTypeStr = "EXP";
            break;
         case ModParam::JEFFREYS:
            priorTypeStr = "JEFFREYS";
            break;
         case ModParam::GAUSS:
         default:
            priorTypeStr = "GAUSS";
            break;
      }
      hyperPars.resize(modPar->hyperParam().size());
      hyperPars = modPar->hyperParam();
      for (size_t i=0; i<hyperPars.size(); ++i)
      {
         PyObject* hyperPar = PyFloat_FromDouble(hyperPars[i]);
         PyList_Append(hyperList, hyperPar);
         Py_DECREF(hyperPar);
      }
   }

   // This function "steals" the reference to valList.
   PyTuple_SetItem(pyVal, 0, valList);
   PyObject* sigmaVal = PyFloat_FromDouble(sigma);
   // Now steal a reference to sigmaVal
   PyTuple_SetItem(pyVal, 1, sigmaVal);
   PyObject* frozenVal = 0;
   if (par->isFrozen())
   {
      Py_INCREF(Py_True);
      frozenVal = Py_True;
   }
   else
   {
      Py_INCREF(Py_False);
      frozenVal = Py_False;
   }
   PyTuple_SetItem(pyVal, 2, frozenVal);

   string linkStr;
   if (par->isLinked())
      linkStr = par->parameterSetting();
   PyTuple_SetItem(pyVal, 3, PYXSSTRING_FROMSTRING(linkStr.c_str()));   
   PyTuple_SetItem(pyVal, 4, PYXSSTRING_FROMSTRING(units.c_str()));
   PyTuple_SetItem(pyVal, 5, PYXSSTRING_FROMSTRING(par->name().c_str()));
   PyTuple_SetItem(pyVal, 6, PyFloat_FromDouble(errLow));
   PyTuple_SetItem(pyVal, 7, PyFloat_FromDouble(errHigh));
   PyTuple_SetItem(pyVal, 8, PYXSSTRING_FROMSTRING(errCmdCode.c_str()));
   PyTuple_SetItem(pyVal, 9, PYXSSTRING_FROMSTRING(priorTypeStr.c_str()));
   PyTuple_SetItem(pyVal, 10, hyperList);

   return pyVal;
} // end createParTuple

string getParIDString(PyObject* parent, const int relativeIdx, const int isMod)
{
   std::ostringstream oss;
   int iPar=0;
   if (isMod)
   {
      const Model* mod = PyXSutils::verifyModelHandle(parent);
      if (!mod)
         return string("");
      if (mod->name() != Model::DEFAULT())
         oss << mod->name() << ":";
      iPar = relativeIdx + static_cast<int>(mod->parameterIndexBase());
   }
   else
   {
      const Response* resp = PyXSutils::verifyResponseHandle(parent);
      if (!resp)
         return string("");
      // Must hardcode this to gain-specific until Response interface
      //  is generalized.
      const Parameter* rpar = (relativeIdx == 1) ? resp->getLinearGain() :
                resp->getConstGain();
      if (!rpar)
      {
         return string("");
      }
      const size_t iSource = resp->sourceNumber();
      if (iSource > 1)
         oss << iSource << ":";
      iPar = rpar->index();
   }
   oss << iPar;
   
   return oss.str();
} // end getParIDString

string commonSetPars(const std::vector<string>& modNames,
                  const std::vector<int>& parIndices,
                  const std::vector<string>& values)
{
   using namespace XSContainer;
   string status;
   const size_t nPars = modNames.size();
   
   // First just get the requested pars and save the original values
   //   in case of input error.
   std::vector<Parameter*> pars;
   std::vector<string> origValues;
   // Also need to keep track of changed inactive models so that we
   // can force a recalculation at the end of this function.
   std::set<Model*> modifiedInactive;
   for (size_t i=0; i<nPars; ++i)
   {
      string modName = modNames[i].length() ? modNames[i] : Model::DEFAULT();
      Parameter* par = models->lookupParameter(parIndices[i],modName);
      if (!par)
      {
          std::ostringstream oss;
          oss << "Error: Unable to find parameter: ";
          if (modNames[i].length())
             oss << modNames[i] << ":";
          oss << parIndices[i];
          status = oss.str();
          return status;
      }
      pars.push_back(par);      
      if (par->isLinked())
      {
         origValues.push_back(par->parameterSetting());
      }
      else
      {
         std::ostringstream parID;
         if (modNames[i].length())
            parID << modNames[i] << ":";
         parID << parIndices[i];      
         std::vector<string> cmdArgs(3);
         cmdArgs[0] = "tclout";
         cmdArgs[1] = "param";
         cmdArgs[2] = parID.str();
         bool dummy=false;
         string results;
         if (XSGlobal::doTclout(cmdArgs, dummy, results))
         {
            status="Error: Cannot retrieve original parameter value string.";
            return status;
         }
         origValues.push_back(results);
      }
      
      // Just grab the lowest dg object, since if model is inactive
      // this will be its only object anyway.
      Model* mod = models->lookup(modName);
      if (!mod->isActive())
         modifiedInactive.insert(mod);
      
   } // end pars loop
   
   // Now proceed to make parameter changes.  Errors can still occur
   // due to invalid values strings.
   bool allOK=true;
   for (size_t i=0; i<nPars && allOK; ++i)
   {
      Parameter* par = pars[i];
      try
      {
         par->modify(values[i]);
      }
      catch(YellowAlert&)
      {
         allOK = false;
      }
   }
   if (!allOK)
   {
      for (size_t i=0; i<nPars; ++i)
      {
         Parameter* par = pars[i];
         // Assume it can't throw when restoring original values.
         par->modify(origValues[i]);
      }
      status = "Error: Cannot apply new parameter settings.";
   }
      
   // Do recalculation even if !allOK and original pars were restored.
   // Their 'calculate' flags would have been set to true, so might as
   // well do a recalc to keep things consistent.  This should only be
   // a corner case anyhow.
   
   // It may be redundant in most cases to explicitly call setCompute, 
   // but if user sent a link string, the earlier call to par->modify 
   // would not have already set the Component's recompute flag.
   for (size_t i=0; i<nPars; ++i)
   {
      string modName = modNames[i].length() ? modNames[i] : Model::DEFAULT();
      models->setCompute(modName);
   }
   std::set<Model*>::const_iterator itMod = modifiedInactive.begin();
   std::set<Model*>::const_iterator itModEnd = modifiedInactive.end();
   while (itMod != itModEnd)
   {
      Model* mod = *itMod;
      if (!mod->isActive())
      {
         // If active, this gets done in Fit Update.
         models->calculate(mod->name());
      }
      ++itMod;
   } 
   fit->Update();
   
   return status;
} // end commonSetPars


} // end unnamed namespace
