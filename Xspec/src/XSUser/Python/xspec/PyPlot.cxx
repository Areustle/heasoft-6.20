#include <XSUser/Python/xspec/PyPlot.h>
#include <XSUser/Python/xspec/PyXSutils.h>

#include <XSContainer.h>
#include <XSPlot/Plot/PlotDirector.h>
#include <XSPlot/Plot/PlotSettings.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h>


PyObject* _pyXspec_clearCommands(PyObject *self, PyObject *args)
{
   PlotSettings& settings = XSContainer::plot->setplot();
   const size_t nComs = settings.userCommands().size();
   // Just erase the first command each time.  It's quicker
   // when deleting from a list.
   for (size_t i=0; i<nComs; ++i)
      settings.removeUserCommand(1);
   return Py_BuildValue("i",0);
}

PyObject* _pyXspec_doPlot(PyObject *self, PyObject *args)
{
   StringArray plotComs;
   if (!PyXSutils::PyListToXSArgs(args, plotComs))
      return NULL;

   try
   {
      // Set this flag to tell Xspec PlotDirector "do NOT delete
      // PlotGroup objects after executing the plot".  Instead leave
      // these around until the next time plot is called.  This makes
      // it possible for Python PlotManager class to retrieve the 
      // various plot arrays AFTER a plot has been displayed.
      // If first arg is ALL_AR, the plot group num is irrelevant.
      std::pair<PlotSettings::SaveArrayOption,int> saveSetting(PlotSettings::ALL_AR,1);
      XSContainer::plot->setplot().saveArrayInfo(saveSetting);
      
      // A hack to distinguish the case of iplot.  Normally this would
      // be done by passing a bool flag through doPlot, but then we
      // couldn't use the generic PyListToXSArgs function (without making
      // a hack to that).  So instead we'll flag 'iplot' by sending a 
      // special string as the last arg to doPlot, and make it something 
      // the user is unlikely to duplicate.
      const string iplotID("h49U^#iplot");
      bool doIplot = false;
      if (plotComs.size())
      {
         const string& lastStr = plotComs[plotComs.size()-1];
         if (lastStr == iplotID)
         {
            doIplot = true;
            plotComs.pop_back();
            // Better NOT use lastStr again after above pop_back operation.
         }
      }
      
      // commonPlotHandler expects plotComs to always have
      // at least 1 string, even if that string is empty.
      if (plotComs.empty())
         plotComs.push_back("");
      XSGlobal::commonPlotHandler(plotComs, doIplot);
   }
   catch (...)
   {
      PyErr_SetString(PyExc_Exception, "");
      return NULL;
   }
   return Py_BuildValue("i",0);
}
// end doPlot

PyObject* _pyXspec_getplotSettings(PyObject *self, PyObject *args)
{
   const PlotSettings& settings = XSContainer::plot->setplot();
   const char* attribute=0;
   PyObject* pyVals=0;
   if (!PyArg_ParseTuple(args, "s", &attribute))
   {
      string msg("Programmer Error: Parsing getplotSettings");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const string attrStr(attribute);
   if (attrStr == "add")
   {
      if (settings.getShowAddComponent())
      {
         Py_INCREF(Py_True);
         pyVals = Py_True;
      }
      else
      {
         Py_INCREF(Py_False);
         pyVals = Py_False;
      }
   }
   else if (attrStr == "area")
   {
      if (settings.getDivideByArea())
      {
         Py_INCREF(Py_True);
         pyVals = Py_True;
      }
      else
      {
         Py_INCREF(Py_False);
         pyVals = Py_False;
      }      
   }
   else if (attrStr == "background")
   {
      if (settings.getShowBackground())
      {
         Py_INCREF(Py_True);
         pyVals = Py_True;
      }
      else
      {
         Py_INCREF(Py_False);
         pyVals = Py_False;
      }      
   }
   else if (attrStr == "device")
   {
      pyVals = PYXSSTRING_FROMSTRING(XSContainer::plot->getPlottingDeviceName().c_str()); 
   }
   else if (attrStr == "id")
   {
      pyVals = PyTuple_New(3);
      PyTuple_SetItem(pyVals, 0, 
                PyFloat_FromDouble(settings.temperature()));
      PyTuple_SetItem(pyVals, 1, 
                PyFloat_FromDouble(settings.emisLimit()));
      PyTuple_SetItem(pyVals, 2, 
                PyFloat_FromDouble(settings.redshiftLinesToObs()));
   }
   else if (attrStr == "lastrebin")
   {
      PlotSettings& ncSettings = XSContainer::plot->setplot();      
      const PlotSettings::RebinInfo& rbInfo = ncSettings.lastRebinEntry().second;      
      pyVals = PyTuple_New(4);
      PyTuple_SetItem(pyVals, 0, PyFloat_FromDouble(rbInfo.sigma));
      PyTuple_SetItem(pyVals, 1, PYXSINT_FROMLONG(static_cast<long>(rbInfo.maxBins)));
      PyTuple_SetItem(pyVals, 2, 
                PYXSINT_FROMLONG(static_cast<long>(ncSettings.lastRebinEntry().first)));
      string errModeStr;
      switch (rbInfo.mode)
      {
         case PlotSettings::STD:
            errModeStr = "quad";
            break;
         case PlotSettings::ROOTN:
            errModeStr = "sqrt";
            break;
         case PlotSettings::GEHRELS1:
            errModeStr = "poiss-1";
            break;
         case PlotSettings::GEHRELS2:
            errModeStr = "poiss-2";
            break;
         case PlotSettings::GEHRELSM:
            errModeStr = "poiss-3";
            break;
         default:
            break;
      }
      PyTuple_SetItem(pyVals, 3, PYXSSTRING_FROMSTRING(errModeStr.c_str()));
   }
   else if (attrStr == "perhz")
   {
      if (settings.isWavePerHz())
      {
         Py_INCREF(Py_True);
         pyVals = Py_True;
      }
      else
      {
         Py_INCREF(Py_False);
         pyVals = Py_False;
      }
   }
   else if (attrStr == "redshift")
   {
      pyVals = PyFloat_FromDouble(settings.redshiftToSource());
   }
   else if (attrStr == "splashpage")
   {
      if (settings.splashPage())
      {
         Py_INCREF(Py_True);
         pyVals = Py_True;
      }
      else
      {
         Py_INCREF(Py_False);
         pyVals = Py_False;
      }
   }
   else if (attrStr == "xaxis")
   {
      string xAxis("channel");
      if (settings.xOption() != CHANNELS)
         xAxis = settings.getUnitID(settings.xOption());
      pyVals = PYXSSTRING_FROMSTRING(xAxis.c_str());
   }
   else if (attrStr == "xlog" || attrStr == "ylog")
   {
      const bool val = (attrStr == "xlog") ? settings.xLog() :
                             settings.yLog();   
      if (val)
      {
         Py_INCREF(Py_True);
         pyVals = Py_True;
      }
      else
      {
         Py_INCREF(Py_False);
         pyVals = Py_False;
      }
   }
   else
   {
      string msg("Non-implemented get settings option.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   return pyVals;
}
// end getplotSettings

PyObject* _pyXspec_getplotValues(PyObject *self, PyObject *args)
{
   using namespace XSContainer;
   
   size_t iGroup=0;
   size_t iPane=0;
   const char* arrayID=0;
   if (!PyArg_ParseTuple(args, "IIs", &iGroup, &iPane, &arrayID))
   {
      string msg("Programmer Error: Invalid args in getplotValues.");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   const string arrType(arrayID);
   const std::vector<Real>* pArray=0;
   size_t nPts=0;
   try
   {
      // getPlotArray should either retrieve a valid reference to a vector,
      //   or throw.
      if (arrType == "x")
         pArray = &plot->getPlotArray(iPane, iGroup, PlotSettings::X_AR, nPts);
      else if (arrType == "xerr")
         pArray = &plot->getPlotArray(iPane, iGroup, PlotSettings::XERR_AR, nPts);
      else if (arrType == "y")
         pArray = &plot->getPlotArray(iPane, iGroup, PlotSettings::Y_AR, nPts);
      else if (arrType == "yerr")
         pArray = &plot->getPlotArray(iPane, iGroup, PlotSettings::YERR_AR, nPts);
      else if (arrType == "model")
         pArray = &plot->getPlotArray(iPane, iGroup, PlotSettings::MODEL_AR, nPts);
      else if (arrType == "background")
         pArray = &plot->getPlotArray(iPane, iGroup, PlotSettings::BACK_AR, nPts);
      else
      {
         string msg("Programmer Error: Invalid plot array string.");
         PyErr_SetString(PyExc_Exception, msg.c_str());
         return NULL;
      }
   } 
   catch (YellowAlert&)
   {
      string msg("Error: Unable to retrieve requested plot array");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   
   const std::vector<Real>& plotArray = *pArray;
   PyObject* plotVals = PyList_New(static_cast<const PY_SZ_TYPE>(nPts));
   for (size_t i=0; i<nPts; ++i)
   {
      PyObject* pyVal = PyFloat_FromDouble(plotArray[i]);
      PyList_SetItem(plotVals, static_cast<PY_SZ_TYPE>(i), pyVal);
   }
   return plotVals;
}
// end getplotValues

PyObject* _pyXspec_identifyUnits(PyObject *self, PyObject *args)
{
   const char* units=0;
   if (!PyArg_ParseTuple(args, "s", &units))
   {
      string msg("Programmer Error: Parsing identifyUnits");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }

   int unitsCategory = -1;
   const string unitsStr(XSutility::lowerCase(string(units)));
   const PlotDirector::UnitsContainer& availableEngUnits = 
           XSContainer::plot->energyUnitsInformation();
   const PlotDirector::UnitsContainer&  availableWaveUnits = 
           XSContainer::plot->waveUnitsInformation();
   PlotDirector::UnitsContainer::const_iterator itUnit =
           availableEngUnits.lower_bound(unitsStr);
   if (itUnit != availableEngUnits.end())
   {
      const string lcKey(XSutility::lowerCase(itUnit->first));
      if (lcKey.find(unitsStr) == 0)
         unitsCategory = 0;
   }
   if (unitsCategory < 0)
   {
      itUnit = availableWaveUnits.lower_bound(unitsStr);
      if (itUnit != availableWaveUnits.end())
      {
         const string lcKey(XSutility::lowerCase(itUnit->first));
         if (lcKey.find(unitsStr) == 0)
            unitsCategory = 1;
      }
   }

   return Py_BuildValue("i",unitsCategory);
} // end indentifyUnits

PyObject* _pyXspec_setPerHz(PyObject *self, PyObject *args)
{
   PlotSettings& settings = XSContainer::plot->setplot();
   PyObject* flag=0;
   if (!PyArg_ParseTuple(args, "O", &flag))
   {
      string msg("Programmer Error: Parsing setPerHz command");
      PyErr_SetString(PyExc_Exception, msg.c_str());
      return NULL;
   }
   flag == Py_True ? settings.isWavePerHz(true):
                     settings.isWavePerHz(false);
   return Py_BuildValue("i",0);
}

PyObject* _pyXspec_setplotCmd(PyObject *self, PyObject *args)
{
   StringArray rawArgs;
   rawArgs.push_back("setplot");
   if (!PyXSutils::PyListToXSArgs(args, rawArgs))
      return NULL;
   if (XSGlobal::doSetplot(rawArgs) < 0)
   {
      PyErr_SetString(PyExc_Exception, "Setplot Command Error");
      return NULL;
   }

   return Py_BuildValue("i",0);
}

PyObject* _pyXspec_showPlot(PyObject *self, PyObject *args)
{
   StringArray rawArgs(2);
   rawArgs[0] = string("show");
   rawArgs[1] = string("plot");
   string dummy;
   XSGlobal::doShow(rawArgs, dummy);
   return Py_BuildValue("i",0);
}
