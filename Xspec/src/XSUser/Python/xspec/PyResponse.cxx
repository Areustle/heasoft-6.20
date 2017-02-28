#include <XSUser/Python/xspec/PyResponse.h>
#include <XSUser/Python/xspec/PyXSutils.h>

#include <XSContainer.h>
#include <XSstreams.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/Data/Detector/RealResponse.h>
#include <XSModel/Data/Detector/UserDummyResponse.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSUser/Handler/XSinterface.h>
#include <sstream>

namespace {
   PyObject* createRespTuple(const Response* resp);
}

PyObject* _pyXspec_getArf(PyObject* self, PyObject *args)
{
   PyObject* handle=0;
   if (!PyArg_ParseTuple(args, "O", &handle))
   {
      string msg("Programmer Error: Response handle conversion in getArf.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const Response* resp = static_cast<Response*>(PYXSCOBJECT_ASVOIDPTR(handle,NULL));
   
   PyObject* pyArf=0;
   if (const RealResponse* realResp = resp->toRealResponse())
   {
      if (realResp->arfName().size())
      {
         pyArf = PYXSSTRING_FROMSTRING(realResp->arfName().c_str());
      }
      else
      {
         string msg("Error: Response has no Arf.");
         PyErr_SetString(PyExc_Exception, msg.c_str());
         return NULL;
      }
   }
   else if (resp->toUserDummyResponse())
   {
      string msg("Error: Dummy response has no Arf.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   else
   {
      string msg("Error: Type of response not supported in Python interface.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   return pyArf;
} // end getArf

PyObject* _pyXspec_getChannelEnergies(PyObject *self, PyObject *args)
{
   size_t specNum=0;
   if (!PyArg_ParseTuple(args, "I", &specNum))
   {
      string msg("Programmer Error: Parsing getNoticedChannels arg.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   const SpectralData* spec = XSContainer::datasets->lookup(specNum);
   std::vector<Response*>::const_iterator itDet = spec->detector().begin();
   std::vector<Response*>::const_iterator itDetEnd = spec->detector().end();
   const Response* firstResp=0;
   while (!firstResp && itDet != itDetEnd)
   {
      if (*itDet)
         firstResp = *itDet;
      ++itDet;
   }
   if (!firstResp || !firstResp->eboundsMin().size())
   {
      string msg("No energies are currently assigned to the spectrum.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   PyObject* respTuple = createRespTuple(firstResp);
   PyObject* chanEnergies = PyTuple_GetItem(respTuple, 1);
   Py_INCREF(chanEnergies);   
   Py_DECREF(respTuple);
   return chanEnergies;   
}
// end getChannelEnergies

PyObject* _pyXspec_getResponse(PyObject *self, PyObject *args)
{
   PyObject* handle=0;
   // sourceNum is 0-based
   size_t sourceNum=0;
   if (!PyArg_ParseTuple(args, "OI", &handle, &sourceNum))
   {
      string msg("Programmer Error: Spectrum handle conversion in getResponses.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const SpectralData* spec = PyXSutils::verifySpectrumHandle(handle);
   if (!spec)
      return NULL;
   const std::vector<Response*>& dets = spec->detector();
   if (sourceNum >= dets.size())
   {
      std::ostringstream oss;
      oss <<"Error: Response array element '" << sourceNum 
                << "' is out of bounds.";
      PyErr_SetString(PyExc_Exception, oss.str().c_str());
      return NULL;
   }
   const Response* resp=dets[sourceNum];
   if (!resp)
   {
      tcout << "Spectrum " << spec->spectrumNumber() <<" has no response for "
         << "source " << sourceNum+1 << std::endl;
      Py_INCREF(Py_None);
      return Py_None;
      
   }
   
   return createRespTuple(resp);
} // end getResponse


PyObject* _pyXspec_hasGainPars(PyObject *self, PyObject *args)
{
   PyObject* handle=0;
   if (!PyArg_ParseTuple(args, "O", &handle))
   {
      string msg("Programmer Error: Cannot get response object handle.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const Response* resp = static_cast<Response*>(PYXSCOBJECT_ASVOIDPTR(handle,NULL));
   PyObject* hasPars=0;
   if (resp->getConstGain() || resp->getLinearGain())
   {
      Py_INCREF(Py_True);
      hasPars = Py_True;
   }
   else
   {
      Py_INCREF(Py_False);
      hasPars = Py_False;
   }
   
   return hasPars;   
}

PyObject* _pyXspec_setArf(PyObject *self, PyObject *args)
{
   const char* fileName=0;
   PyObject* handle=0;
   if (!PyArg_ParseTuple(args, "Oz", &handle, &fileName))
   {
      string msg("Programmer Error: Response handle conversion in setArf.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const Response* resp = static_cast<Response*>(PYXSCOBJECT_ASVOIDPTR(handle,NULL));
   const size_t sourceNum = resp->sourceNumber();
   const size_t specNum=resp->source()->spectrumNumber();
   StringArray rawArgs(3);
   rawArgs[0] = "arf";
   std::ostringstream oss;
   oss << sourceNum << ":" << specNum;
   rawArgs[1] = oss.str();
   // If fileName == 0, caller is requesting that arf be removed.
   rawArgs[2] = fileName ? string(fileName) : string("none");
   if (XSGlobal::doArf(rawArgs))
   {
      string msg("Arf file setting error");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
      
   return Py_BuildValue("i",1);
}

PyObject* _pyXspec_setResponse(PyObject *self, PyObject *args)
{
   PyObject* handle=0;
   // This is 0-based.
   size_t sourceNum=0;
   char* fileName=0;
   if (!PyArg_ParseTuple(args, "OIz", &handle, &sourceNum, &fileName))
   {
      string msg("Programmer Error: Spectrum handle conversion in setResponse.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const SpectralData* spec = PyXSutils::verifySpectrumHandle(handle);
   if (!spec)
      return NULL;
   const size_t specNum = spec->spectrumNumber();
   StringArray rawArgs(3);
   rawArgs[0] = "response";
   std::ostringstream oss;
   oss << sourceNum+1 << ":" << specNum;
   rawArgs[1] = oss.str();
   rawArgs[2] = fileName ? string(fileName) : string("none");
   if (XSGlobal::doResponse(rawArgs))
   {
      string msg("Response file setting error");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;      
   }
   
   return Py_BuildValue("i",1);
}

PyObject* _pyXspec_showResponse(PyObject* self, PyObject *args)
{
   PyObject* handle=0;
   if (!PyArg_ParseTuple(args, "O", &handle))
   {
      string msg("Programmer Error: Cannot get response object handle.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const Response* resp = PyXSutils::verifyResponseHandle(handle);
   if (!resp)
      return NULL;
   
   string respName;
   string arfName;
   if (resp->toRealResponse())
   {
      const RealResponse* rresp = resp->toRealResponse();
      respName = rresp->rmfName();
      arfName = rresp->arfName();
   }
   else if (resp->toUserDummyResponse())
   {
      respName = "Dummy response";
   }
   else
   {
      string msg("Error: Type of response not supported in Python interface.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   tcout << "\n"<< respName << " associated with spectrum " << resp->spectrumNumber()
         << " source " << resp->sourceNumber() << std::endl;
   tcout << "   energies: " << resp->numEnergies() << " channels: "
         << resp->numChannels() << std::endl;
   if (!arfName.length())
      arfName = "None";
   tcout << "Using ARF: " << arfName << "\n"<<std::endl;
   // Hardcoded for 'gain' response model only.
   if (resp->getLinearGain() || resp->getConstGain())
   {
      const size_t linelen(72);
      tcout << "Using response model:\n" << string(linelen,'=')
            << "\nRpar Spectrum Rmodel   Rpar_name  Unit   Value\n" <<std::endl; 
      tcout << *resp->getLinearGain();
      tcout << *resp->getConstGain();
      tcout << string(linelen,'_') <<"\n"<<std::endl;
   }
   
   return Py_BuildValue("i",1);
} // end showResponse

namespace {

PyObject* createRespTuple(const Response* resp)
{
   PyObject* pyVals=PyTuple_New(5);
   if (const RealResponse* realResp = resp->toRealResponse())
   {
      PyTuple_SetItem(pyVals, 0,
                PYXSSTRING_FROMSTRING(realResp->rmfName().c_str()));
   }
   else if (resp->toUserDummyResponse())
   {
      PyTuple_SetItem(pyVals, 0, PYXSSTRING_FROMSTRING("dummy"));
   }
   else
   {
      string msg("Error: Type of response not supported in Python interface.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      Py_DECREF(pyVals);
      return NULL;
   }

   // Not every Response will have channel energies (ie. UserDummyResp
   // without channels).  In that case channel tuple should be size 0.
   RealArray channelEnergies(resp->eboundsMin());
   const size_t nChanEngs = channelEnergies.size() ? 
                                channelEnergies.size()+1 : 0;
   PyObject* chanEngTuple = PyTuple_New(nChanEngs);
   for (PY_SZ_TYPE i=0; i<static_cast<PY_SZ_TYPE>(nChanEngs)-1; ++i)
      PyTuple_SetItem(chanEngTuple, i, PyFloat_FromDouble(channelEnergies[i]));
   if (nChanEngs)
      PyTuple_SetItem(chanEngTuple, nChanEngs-1,
                PyFloat_FromDouble(resp->eboundsMax()[nChanEngs-2]));
   PyTuple_SetItem(pyVals, 1, chanEngTuple);

   const RealArray& energies = resp->energies();
   const size_t nEngs = energies.size();
   PyObject* engTuple = PyTuple_New(nEngs);
   for (PY_SZ_TYPE i=0; i<static_cast<PY_SZ_TYPE>(nEngs); ++i)
      PyTuple_SetItem(engTuple, i, PyFloat_FromDouble(energies[i]));
   PyTuple_SetItem(pyVals, 2, engTuple);
   const void* cHandle = static_cast<const void*>(resp);
   void* handle = const_cast<void*>(cHandle);
   PyTuple_SetItem(pyVals, 3, PYXSCOBJECT_FROMVOIDPTR(handle,NULL,NULL));
   PyTuple_SetItem(pyVals, 4, 
                PYXSINT_FROMLONG(static_cast<long>(resp->sourceNumber())));
   
   return pyVals;
}

} // end unnamed namespace
