"""
PyXspec
=======

An object oriented Python interface to the XSPEC spectral-fitting
program.

Getting Help
------------

Help is available by accessing class docstrings with Python's help 
function, ie.:

>>> import xspec
>>> help(xspec.Spectrum)

Other xspec classes are:  Background, Chain, Component, FakeitSettings, 
Model, Parameter, Response, RModel, and the 6 manager classes mentioned
below.
   
When the xspec package is imported, it immediately instantiates a global 
object for each of the 6 manager classes:

   Object Name     Class
   
   AllData         DataManager
   AllModels       ModelManager
   Fit             FitManager
   Plot            PlotManager
   Xset            XspecSettings
   AllChains       ChainManager
   
You may enter the object name or class name to get help for these:

>>> help(xspec.AllData)
          or
>>> help(xspec.DataManager)

Help is also available in a manual distributed with the code and installed
at $HEADAS/spectral/help/python, in PDF and HTML formats (the HTML home
page is at html/index.html).  The manual includes 2 tutorial sections
and a Doxygen-generated PyXspec class reference guide.  The "Quick
Tutorial" section is the recommended starting point for all new
users.

Package Modules
---------------

chain.py
data.py
fit.py
model.py
parameter.py
plot.py
response.py
spectrum.py
xset.py

When the xspec package is imported, all relevant names (of classes, 
functions, global objects) defined in these modules are also imported
into the xspec namespace.

"""
from __future__ import absolute_import

from .chain import ChainManager, AllChains, Chain
from .fit import FitManager, Fit
from .data import FakeitSettings, DataManager, AllData
from .model import AllModels, ModelManager, Model, Component
from .parameter import Parameter
from .plot import PlotManager, Plot
from .response import Response, RModel
from .spectrum import Spectrum, Background
from .xset import XspecSettings, Xset
