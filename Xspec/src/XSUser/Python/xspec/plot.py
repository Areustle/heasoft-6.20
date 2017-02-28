from __future__ import print_function
from __future__ import absolute_import

from . import _pyXspec
from .xset import _AttrRestrictor
      
class PlotManager(_AttrRestrictor):
    """**Xspec plotting class.**
    
    PyXspec automatically creates a single object of this class,
    named *Plot*.
    
    **Methods**
    
    .. hlist::
       :columns: 2
    
       - __call__ (the '()' operator)
       - addCommand
       - backgroundVals
       - delCommand
       - iplot
       - model
       - noID
       - setGroup
       - setID
       - setRebin
       - show
       - x
       - xErr
       - y
       - yErr
          
    **Attributes**
    
    .. hlist::
       :columns: 2
    
       - add                
       - area               
       - background         
       - commands                              
       - device             
       - perHz              
       - redshift                     
       - splashPage         
       - xAxis              
       - xLog        
       - yLog        
    
    """    
    __single = None
    def __init__(self, deviceStr):
        if PlotManager.__single:
            raise Exception("Error: Only 1 instance of PlotManager is allowed.")
        PlotManager.__single = self
        self._turnRestrictOff()
        self.__commands = []
        self._resetRestrict()
           
    def __call__(self, *panes):
        """Display the plot.
        
        Input 1 or more plot command strings.
        
        Examples::
        
           # Single Plots: 
           Plot("data")
           Plot("model")
           Plot("ufspec")
              
           # Multiple Plots (or single plots taking additional arguments):
           Plot("data","model","resid")
           Plot("data model resid")
           Plot("data,model,resid")
           Plot("data","model m1")  # Plots data and a model named "m1".
              
           # To repeat a plot using the previously entered arguments, simply do: 
           Plot()
                     
        """
        plotArgs = []
        for s in panes:
            plotArgs += s.split()
        _pyXspec.doPlot(plotArgs)
    
    def _getAdd(self):
        return _pyXspec.getplotSettings("add")
    def _setAdd(self, value):
        if value == True:
            _pyXspec.setplotCmd(["add"])
        else:
            _pyXspec.setplotCmd(["noadd"])
    add = property(_getAdd, _setAdd, 
                doc="""Turn on/off the display of individual additive 
                       components [bool].""")
    
    def _getArea(self):
        return _pyXspec.getplotSettings("area")
    def _setArea(self, value):
        if value == True:
            _pyXspec.setplotCmd(["area"])
        else:
            _pyXspec.setplotCmd(["noarea"])
    area = property(_getArea, _setArea, 
           doc="""Toggle displaying the data divided by the response
                  effective area for each channel [bool].""")
    
    def _getBackground(self):
        return _pyXspec.getplotSettings("background")
    def _setBackground(self, value):
        if value == True:
            _pyXspec.setplotCmd(["background"])
        else:
            _pyXspec.setplotCmd(["nobackground"])
    background = property(_getBackground, _setBackground, 
         doc="""Toggle displaying the background spectrum (if any)
                when plotting data [bool].""")
    
    def _getCommands(self):
        return tuple(self.__commands)
    def _setCommands(self, cmds):
        if isinstance(cmds, tuple):
            _pyXspec.clearCommands()
            for com in cmds:
                _pyXspec.setplotCmd(["command"] + com.split())
            if len(cmds):
                _pyXspec.setplotCmd(["list"])
            else:
                print("Plot command list is now empty")
            self.__commands = list(cmds)
        else:
            raise Exception("Error: Must set commands to a tuple of strings.")
    commands = property(_getCommands, _setCommands,
      doc="""Custom plot commands to be appended to Xspec-generated 
                        commands.
             
             Get: Returns a tuple of the currently entered command 
             strings.
             
             Set: Replaces all commands with the new tuple of 
             strings.

             To remove ALL plot commands, set to an empty tuple, ie:             
                >>> Plot.commands = ()

             For inserting and deleting individual commands, use
             *addCommand* and *delCommand* functions.""")
    
    def _getDevice(self):
        return _pyXspec.getplotSettings("device")
    def _setDevice(self, value):
        setCmd = ["device"] + value.split()
        _pyXspec.setplotCmd(setCmd)
    device = property(_getDevice, _setDevice, 
                doc="The plotting device name [string].")
    
    def _getPerHz(self):
        return _pyXspec.getplotSettings("perhz")
    def _setPerHz(self, val):
        if val:
            _pyXspec.setPerHz(True)
        else:
            _pyXspec.setPerHz(False)
    perHz = property(_getPerHz, _setPerHz, 
      doc="""Toggle displaying Y-axis units per Hz when using 
             wavelength units for X-axis [bool].""")
        
    def _getRedshift(self):
        return _pyXspec.getplotSettings("redshift")
    def _setRedshift(self, value):
        _pyXspec.setplotCmd(["redshift"] + [str(value)])
    redshift = property(_getRedshift, _setRedshift,
        doc="""Apply a redshift to the X-axis energy or wavelength
               values [float].
               
               This will multiply X-axis energies by a factor of (1+z)
               to allow for viewing in the source frame.  Y-axis values
               will be equally affected in plots which are normalized
               by energy or wavelength.  Note that this is not
               connected in any way to redshift parameters in the model
               (or the setplot id redshift parameter) and should only
               be used for illustrative purposes.""")
    
    def _getSplashPage(self):
        return _pyXspec.getplotSettings("splashpage")
    def _setSplashPage(self, value):
        if value:
            _pyXspec.setplotCmd(["splashpage"] + ["on"])
        else:
            _pyXspec.setplotCmd(["splashpage"] + ["off"])
    splashPage = property(_getSplashPage, _setSplashPage,
          doc="""When set to False, the usual XSPEC version and build data
                 information will not be printed to the screen when the 
                 first plot window is initially opened [bool].""")
    
    def _getXAxis(self):
        return _pyXspec.getplotSettings("xaxis")
    def _setXAxis(self, value):
        if not isinstance(value, str):
            raise Exception("Error: Argument must be a string")
        lcValue = value.lower()
        if "channel".find(lcValue) == 0:
            _pyXspec.setplotCmd(["channel"])
        else:
            unitsCat = _pyXspec.identifyUnits(value)
            if unitsCat < 0:
                raise Exception("Error: Invalid units string")
            elif unitsCat == 0:
                _pyXspec.setplotCmd(["energy"] + [value])
            else:
                _pyXspec.setplotCmd(["wave"] + [value])               
    xAxis = property(_getXAxis, _setXAxis,
                doc="""X-Axis Units [string].
                               
                       Valid options are:   "channel",                       
                       (energies)         "keV", "MeV", "GeV", "Hz",
                       (wavelengths)      "angstrom", "cm", "micron", nm"

                       These are case-insensitive and may be abbreviated.
                
                       This setting also affects the ignore/notice range
                       interpretation.""")  
    
    def _getXLog(self):
        return _pyXspec.getplotSettings("xlog")
    def _setXLog(self, value):
        if value:
            _pyXspec.setplotCmd(["xlog"] + ["on"])
        else:
            _pyXspec.setplotCmd(["xlog"] + ["off"])
    xLog = property(_getXLog, _setXLog,
         doc="""Set the x-axis to logarithmic or linear for energy or
                wavelength plots [bool].
                 
                *xLog* has no effect on plots in channel space.  *xLog*
                and *yLog* will not work for model-related plots 
                (eg. *model*, *ufspec*, and their variants) as their axes
                are always set to log scale.""")
    
    def _getYLog(self):
        return _pyXspec.getplotSettings("ylog")
    def _setYLog(self, value):
        if value:
            _pyXspec.setplotCmd(["ylog"] + ["on"])
        else:
            _pyXspec.setplotCmd(["ylog"] + ["off"])
    yLog = property(_getYLog, _setYLog,
                doc="""See xLog.""")
    
    
    def addCommand(self, cmd):
        """Add a plot command [string] to the end of the plot commands list."""
        if isinstance(cmd,str):
            _pyXspec.setplotCmd(["command"] + cmd.split())
            self.__commands.append(cmd)
        else:
            raise Exception("Error: Input argument must be a string.")
    
    def delCommand(self, num):
        """Remove a plot command by (1-based) number [int].
        
        This is intended for removal of single commands.  To remove ALL
        commands, set the *Plot.commands* attribute to an empty tuple, ie:
        
           >>> Plot.commands = ()
           
        """
        if isinstance(num, int):
            if num < 1 or num > len(self.__commands):
                errMsg="Error: Delete command number is out of range"
                raise Exception(errMsg)
            delcom = ["delete"]
            delcom.append(str(num))
            _pyXspec.setplotCmd(delcom)
            del self.__commands[num-1]                 
        else:
            raise Exception("Error: Input argument must be an int")
    
    def iplot(self, *panes):
        """Display the plot and leave it in interactive plotting mode.
        
           This function takes the same arguments and syntax as when
           displaying plots in the regular mode (through Plot's
           *__call__* method).  Examples::
           
              Plot.iplot("data")   # 1-panel data plot
              Plot.iplot("data model")    # 2-panel data and model
              Plot.iplot()                # Repeats the previous plot.
              
        """
        plotArgs = []
        for s in panes:
            plotArgs += s.split()
        # Add the mangled iplot identifier string (defined in doPlot).
        plotArgs.append("h49U^#iplot")
        _pyXspec.doPlot(plotArgs)
        
    def noID(self):
        """Turn off the plotting of line IDs."""
        _pyXspec.setplotCmd(["noid"])
    
    def setGroup(self,groupStr):
        """Define a range of spectra to be in the same plot group.
        
        Input argument is a string specifying one or more ranges, delimited
        by commas and/or spaces.  Examples::
        
           # Spectra 1-3 in plot group 1, 4-6 in group 2.
           Plot.setGroup("1-3  4-6")
           
           # Spectra 1, 2, and 4 are each now in their own group.           
           Plot.setGroup("1,2 4")
           
           # All spectra are in a single plot group. 
           Plot.setGroup("1-**")
           
           # If input argument is Python's 'None' variable, all
           # plot grouping will be removed.  
           Plot.setGroup(None)    
        
        """        
        if isinstance(groupStr,str):
            _pyXspec.setplotCmd(["group"] + [groupStr])
        elif groupStr is None:
            _pyXspec.setplotCmd(["ungroup"])
        else:
            raise Exception("Error: Input argument must be a string or None.")
    
    def setID(self, temperature=None, emissivity=None, redshift=None):
        """Switch on plotting of line IDs.
        
        All input arguments are floats and are optional.  If they are omitted 
        they will retain their previous values.
        
           - *temperature*:  Selects the temperature of the APEC line list.
           
           - *emissitivity*: Only lines with emissivities above this setting 
             will be displayed.
             
           - *redshift*:  Line display will be redshifted by this amount.
           
        To turn off plotting of line IDs, use the *noID()* function.  
        
        """
        idTuple = _pyXspec.getplotSettings("id")
        if temperature is None:
            temperature = idTuple[0]
        if emissivity is None:
            emissivity = idTuple[1]
        if redshift is None:
            redshift = idTuple[2]
        idCmds = ["id"]
        idCmds.append(str(temperature))
        idCmds.append(str(emissivity))
        idCmds.append(str(redshift))
        _pyXspec.setplotCmd(idCmds)     
    
    def setRebin(self, minSig=None, maxBins=None, groupNum=None, errType=None):
        """Define characteristics used in rebinning the data (for plotting
        purposes ONLY).
        
        All input arguments are optional.  If they are omitted they will retain
        their previous values.
           
           - *minSig*:    Bins will be combined until this minimum significance
             is reached (in units of sigma).  [float]
             
           - *maxBins*:   The maximum number of bins to combine in attempt to
             reach *minSig*.  [int]
             
           - *groupNum*:  The plot group number to which this setting applies.
             If number is negative, it will apply to ALL plot groups.  [int]
             
           - *errType*:   Specifies how to calculate the error bars on the new
             bins.  Valid entries are "quad", "sqrt", "poiss-1","poiss-2", 
             "poiss-3".  [string]  See the "setplot" description in the XSPEC 
             manual for more information.
                   
        """
        rebinTuple = _pyXspec.getplotSettings("lastrebin")
        if minSig is None:
            minSig = rebinTuple[0]
        if maxBins is None:
            maxBins = rebinTuple[1]
        if groupNum is None:
            groupNum = rebinTuple[2]
        if errType is None:
            errType = rebinTuple[3]
        rebinCmds = ["rebin"]
        rebinCmds.append(str(minSig))
        rebinCmds.append(str(maxBins))
        rebinCmds.append(str(groupNum))
        rebinCmds.append(errType)
        _pyXspec.setplotCmd(rebinCmds)    

    def show(self):
        """Display current plot settings"""
        _pyXspec.showPlot()
        print()
        print("User entered plot commands:")
        _pyXspec.setplotCmd(["list"])
    
    def x(self, plotGroup=1, plotWindow=1):
        """Return a list of X-coordinate data values for a plot group and plot window"""
        if not isinstance(plotGroup, int) or plotGroup < 1:
            errMsg = "Error: plotGroup number must be an int >= 1"
            raise Exception(errMsg)
        if not isinstance(plotWindow, int) or plotWindow < 1:
            errMsg = "Error: plotWindow number must be an int >= 1"
            raise Exception(errMsg)
        return _pyXspec.getplotValues(plotGroup-1, plotWindow-1,"x")

    def xErr(self, plotGroup=1, plotWindow=1):
        """Return a list of X-coordinate errors for a plot group and plot window"""
        if not isinstance(plotGroup, int) or plotGroup < 1:
            errMsg = "Error: plotGroup number must be an int >= 1"
            raise Exception(errMsg)
        if not isinstance(plotWindow, int) or plotWindow < 1:
            errMsg = "Error: plotWindow number must be an int >= 1"
            raise Exception(errMsg)
        return _pyXspec.getplotValues(plotGroup-1, plotWindow-1, "xerr")
        
    def y(self, plotGroup=1, plotWindow=1):
        """Return a list of Y-coordinate data values for a plot group and plot window"""
        if not isinstance(plotGroup, int) or plotGroup < 1:
            errMsg = "Error: plotGroup number must be an int >= 1"
            raise Exception(errMsg)
        if not isinstance(plotWindow, int) or plotWindow < 1:
            errMsg = "Error: plotWindow number must be an int >= 1"
            raise Exception(errMsg)
        return _pyXspec.getplotValues(plotGroup-1, plotWindow-1, "y")
    
    def yErr(self, plotGroup=1, plotWindow=1):
        """Return a list of Y-coordinate errors for a plot group and plot window"""
        if not isinstance(plotGroup, int) or plotGroup < 1:
            errMsg = "Error: plotGroup number must be an int >= 1"
            raise Exception(errMsg)
        if not isinstance(plotWindow, int) or plotWindow < 1:
            errMsg = "Error: plotWindow number must be an int >= 1"
            raise Exception(errMsg)
        return _pyXspec.getplotValues(plotGroup-1, plotWindow-1, "yerr")
        
    def model(self, plotGroup=1, plotWindow=1):
        """Return a list of Y-coordinate model values for a plot group and plot window"""
        if not isinstance(plotGroup, int) or plotGroup < 1:
            errMsg = "Error: plotGroup number must be an int >= 1"
            raise Exception(errMsg)
        if not isinstance(plotWindow, int) or plotWindow < 1:
            errMsg = "Error: plotWindow number must be an int >= 1"
            raise Exception(errMsg)
        return _pyXspec.getplotValues(plotGroup-1, plotWindow-1, "model")
         
    def backgroundVals(self, plotGroup=1, plotWindow=1):
        """Return a list of background data values for a plot group and plot window
        
        Background value arrays only exist for data plots when the
        *Plot.background* flag is set to True. 
        """
        if not isinstance(plotGroup, int) or plotGroup < 1:
            errMsg = "Error: plotGroup number must be an int >= 1"
            raise Exception(errMsg)
        if not isinstance(plotWindow, int) or plotWindow < 1:
            errMsg = "Error: plotWindow number must be an int >= 1"
            raise Exception(errMsg)
        return _pyXspec.getplotValues(plotGroup-1, plotWindow-1, "background")
    
Plot = PlotManager("/null")


