#include <XSUser/Python/xspec/PyXset.h>
#include <XSUser/Python/xspec/PyXSutils.h>

#include <XSContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSUtil/Utils/ProcessManager.h>

PyObject* _pyXspec_doTclout(PyObject *self, PyObject *args)
{
   StringArray rawArgs;
   rawArgs.push_back(string("tclout"));
   if (!PyXSutils::PyListToXSArgs(args, rawArgs))
      return NULL;
   string results;
   bool resultsEntered=false;
   // The resultsEntered flag is not relevant here.  It exists to let us
   // know of the case where doTclout returns without error, yet no results
   // are retrieved.  This can happen with "tclout ?" during an interactive
   // session.
   if (XSGlobal::doTclout(rawArgs, resultsEntered, results))
   {
      string msg("Error executing xset/tclout command.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   return Py_BuildValue("s",results.c_str());
}

PyObject* _pyXspec_getAbund(PyObject *self, PyObject *args)
{
   const string& abund = FunctionUtility::ABUND();
   return Py_BuildValue("s", abund.c_str());
}

PyObject* _pyXspec_getModelStringValues(PyObject *self, PyObject *args)
{
   PyObject* dBase = PyDict_New();
   std::map<string,string>::const_iterator 
                   itMS (FunctionUtility::modelStringDataBase().begin());
   std::map<string,string>::const_iterator 
                   itEnd (FunctionUtility::modelStringDataBase().end());
   while (itMS != itEnd)
   {
      PyObject* val = PYXSSTRING_FROMSTRING(itMS->second.c_str());
      PyDict_SetItemString(dBase, itMS->first.c_str(), val);
      Py_DECREF(val);
      ++itMS;
   }
   
   return dBase;
}

PyObject* _pyXspec_getParallel(PyObject *self, PyObject *args)
{
   const char* cName=0;
   if (!PyArg_ParseTuple(args, "s", &cName))
   {
      string msg("Programmer Error: Parsing getParallel.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const string contextName(cName);
   const std::map<string,int>& procsMap = ProcessManager::maxProcs();
   std::map<string,int>::const_iterator itProcs = procsMap.find(contextName);
   if (itProcs == procsMap.end())
   {
      string msg("Programmer Error: Accessing process map.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   
   long nProcs=static_cast<long>(itProcs->second);
   return PYXSINT_FROMLONG(nProcs);
}

PyObject* _pyXspec_getPropDelta(PyObject *self, PyObject *args)
{
   Real delta = XSContainer::models->proportionalDelta();
   if (delta <= 0.0)
      delta = 0.0;
   return Py_BuildValue("d",delta);
}

PyObject* _pyXspec_getXsect(PyObject *self, PyObject *args)
{
   const string& xsect = FunctionUtility::XSECT();
   return Py_BuildValue("s",xsect.c_str());
}

PyObject* _pyXspec_setModelStringValues(PyObject *self, PyObject *args)
{
   // Replace ENTIRE string database.  Note that this does not add new
   // database by re-using xset command code (via multiple calls of
   // "xset <key> <val>".  This is to avoid doing multiple calls to
   // Notify/Update chain.
   
   PyObject* dict=0;
   if (!PyArg_ParseTuple(args, "O", &dict))
   {
      string msg("Programmer Error: parsing setModelStringValues");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   FunctionUtility::eraseModelStringDataBase();
   
   PyObject* pKey=0;
   PyObject* pVal=0;
   PY_SZ_TYPE pos=0;
   while (PyDict_Next(dict, &pos, &pKey, &pVal))
   {
       string keyStr(PYXSSTRING_ASSTRING(pKey));
       string valStr(PYXSSTRING_ASSTRING(pVal));
       FunctionUtility::setModelString(keyStr, valStr);
   }
   XSContainer::datasets->Notify();
   return Py_BuildValue("i",0);
}

PyObject* _pyXspec_showXset(PyObject *self, PyObject *args)
{
   StringArray rawArgs(2);
   rawArgs[0] = string("show");
   rawArgs[1] = string("control");
   string dummy;
   XSGlobal::doShow(rawArgs, dummy);

   if (FunctionUtility::modelStringDataBase().size())
   {
      rawArgs.resize(1);
      rawArgs[0] = string("xset");
      XSGlobal::doXset(rawArgs);
   }
   return Py_BuildValue("i",0);
}
