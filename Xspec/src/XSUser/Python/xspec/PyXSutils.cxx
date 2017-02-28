#include <XSUser/Python/xspec/PyXSutils.h>
#include <XSUser/Handler/XSinterface.h>

#include <XSContainer.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Model/Model.h>

PyObject* _pyXspec_doXspecCmd(PyObject* self, PyObject* args)
{
   using namespace XSGlobal;
   StringArray rawArgs;
   if (!PyXSutils::PyListToXSArgs(args, rawArgs))
      return NULL;
   const string& xsCmdName = rawArgs[0];
   std::map<string,stdInterface>::const_iterator itFunc = 
                stdInterfaceCmdMap.find(xsCmdName);
   if (itFunc == stdInterfaceCmdMap.end())
   {
      string msg("Error: Unable to make function call for ");
      msg += xsCmdName;
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   stdInterface xsFunc = itFunc->second;
   int status = (*xsFunc)(rawArgs);
   if (status)
   {
      string msg("Error executing command: ");
      msg += xsCmdName;
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   return Py_BuildValue("i",0);
}

namespace PyXSutils {

int PyListToXSArgs(PyObject* args, StringArray& rawArgs)
{
   PyObject* inList=0;
   if (!PyArg_ParseTuple(args, "O", &inList))
   {
      string msg("Programmer Error: converting Python list to Xspec args.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return 0;
   }
   PY_SZ_TYPE nArgs = PyList_Size(inList);
   for (PY_SZ_TYPE i=0; i<nArgs; ++i)
      rawArgs.push_back(PYXSSTRING_ASSTRING(PyList_GetItem(inList, i)));

   return 1;
}

Model* verifyModelHandle(PyObject* handle)
{
   using namespace XSContainer;

   Model* mod = static_cast<Model*>(PYXSCOBJECT_ASVOIDPTR(handle,NULL));
   bool isFound = false;
   // Unfortunately this is O(N) lookup:
   ModelMap::const_iterator itMap = models->modelSet().begin();
   ModelMap::const_iterator itMapEnd = models->modelSet().end();
   while (!isFound && itMap != itMapEnd)
   {
      if (mod == itMap->second)
         isFound = true;
      ++itMap;
   }
   if (!isFound)
   {
      string msg("Error: Python Model object reference no longer corresponds to\n");
      msg +=     "          an actual XSPEC model.";
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   return mod;
}

Response* verifyResponseHandle(PyObject* handle)
{
   using namespace XSContainer;
   
   Response* resp = static_cast<Response*>(PYXSCOBJECT_ASVOIDPTR(handle,NULL));
   bool isFound = false;
   // O(N) lookup again:
   ResponseMap::const_iterator itMap = responses->responseList().begin();
   ResponseMap::const_iterator itMapEnd = responses->responseList().end();
   while (!isFound && itMap != itMapEnd)
   {
      if (resp == itMap->second)
         isFound = true;
      ++itMap;
   }
   if (!isFound)
   {
      string msg("Error: Python Response object reference no longer corresponds to\n");
      msg +=     "          an actual XSPEC response.";
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   
   return resp;
}

SpectralData* verifySpectrumHandle(PyObject* handle)
{
   using namespace XSContainer;
   
   SpectralData* spec = static_cast<SpectralData*>(PYXSCOBJECT_ASVOIDPTR(handle,NULL));
   bool isFound=false;
   const size_t nSpec = datasets->numberOfSpectra();
   size_t iSpec=1;
   while (iSpec<=nSpec && !isFound)
   {
      if (datasets->lookup(iSpec) == spec)
         isFound = true;
      else
         ++iSpec;
   }
   if (!isFound)
   {
      string msg("Error: Unable to locate Spectrum object.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   return spec;   
}

} // end PyXSutils namespace.
