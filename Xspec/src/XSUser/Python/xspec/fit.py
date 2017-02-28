from __future__ import print_function
from __future__ import absolute_import

from . import _pyXspec
from .xset import _AttrRestrictor

class FitManager(_AttrRestrictor):
    """**Xspec fitting class.**
    
    PyXspec automatically creates a single object of this class,
    named *Fit*.
    
       **Methods**
       
       .. hlist::
          :columns: 2
       
          - error
          - ftest
          - goodness
          - improve
          - perform
          - renorm
          - show
          - steppar
          - stepparResults
       
       **Attributes**  ((*) = get-only)
       
       .. hlist::
          :columns: 2
       
          - bayes       
          - covariance* 
          - criticalDelta       
          - delta       
          - dof*                  
          - method   
          - nIterations  
          - query              
          - statMethod
          - statTest
          - statistic*     
          - testStatistic* 
          - weight         
                      
    """    
    __single = None
    def __init__(self):
        if FitManager.__single:
            raise Exception("Error: Only 1 instance of FitManager is allowed.")
        FitManager.__single = self
        self._turnRestrictOff()
        self.__query = "on"
        self._resetRestrict()
    
    def _getBayes(self):
        if _pyXspec.getFitSettings()[4]:
            return "on"
        else:
            return "off"
    def _setBayes(self, val):
        # Just make sure this is a string.  Let Xspec handler check if
        #   it's actually a valid entry.
        if not isinstance(val, str):
            err="Error: bayes should be set to 'on'|'off'|'cons'"
            raise Exception(err)
        cmdArgs = ["bayes"] + val.split()
        _pyXspec.doXspecCmd(cmdArgs)
    bayes = property(_getBayes,_setBayes,
             doc="""Turn Bayesian inference on or off [string].
       
                    Valid settings are 'on', 'off' (default), or 'cons'. 
                    'cons' turns Bayesian inference on AND gives ALL
                    parameters a constant prior.  Priors can be set for
                    parameters individually through the Parameter object's
                    *prior* attribute.""")
    
    def _getCovariance(self):
        covarStr = _pyXspec.doTclout(["covariance"])
        valList = covarStr.split()
        for i in range(len(valList)):
            valList[i] = float(valList[i])
        return tuple(valList)
    def _setCovariance(self,val):
        raise Exception("Error: Cannot rebind Fit.covariance attribute")
    covariance = property(_getCovariance,_setCovariance,
                   doc="""The covariance matrix from the most recent fit [tuple
                          of floats] (GET only).
                        
                          As with standard XSPEC's "tclout covar", this only
                          returns the diagonal and below-diagonal matrix
                          elements.""")
    
    def _getCriticalDelta(self):
        settings = _pyXspec.getFitSettings()
        return settings[3]    
    def _setCriticalDelta(self, val):
        if isinstance(val, float):
            if val <= .0:
                raise Exception("Error: Critical delta must be > 0.0")
            else:
                #Critical delta is the 3rd arg sent to the method command.
                methodArgs = ["method"]
                critStr = ",," + str(val)
                methodArgs.append(critStr)
                _pyXspec.doXspecCmd(methodArgs)                
        else:
            raise Exception("Error: Critical delta argument must be a float.")
    criticalDelta = property(_getCriticalDelta, _setCriticalDelta,
                   doc="""Critical delta for fit statistic convergence [float].
                   
                          The absolute change in the fit statistic between
                          iterations, less than which the fit is deemed to 
                          have converged.""")
    
    def _getDelta(self):
        return _pyXspec.getPropDelta()
    def _setDelta(self, value):
        if isinstance(value, float) or isinstance(value,int):
            deltaArgs = ["xset","delta"]
            deltaArgs.append(str(value))
            _pyXspec.doXspecCmd(deltaArgs)
        else:
            raise Exception("Error: Input argument must be of numeric type")        
    delta = property(_getDelta, _setDelta,
        doc="""Set fit delta values to be proportional to the parameter value [float].
        
               Get: 
                    Returns the current proportional setting, or 0.0 if
                    currently using the fixed fit delta values.

               Set: 
                    Enter the constant factor which will multiply the 
                    parameter value to produce a fit delta.  A constant
                    factor of 0.0 or negative will turn off the use of
                    proportional fit deltas.""")

    def _getDof(self):
        dofStr = _pyXspec.doTclout(["dof"])
        return int(dofStr.split()[0])
    def _setDof(self, val):
        raise Exception("Error: Cannot rebind Fit.dof attribute")
    dof = property(_getDof, _setDof, 
         doc="The degrees of freedom for the fit [int] (GET only).")
    
    def _getMethod(self):
        settings = _pyXspec.getFitSettings()
        return settings[1] 
    def _setMethod(self, args):
        methodArgs = ["method"]
        if isinstance(args, str):
            methodArgs += args.split()
            _pyXspec.doXspecCmd(methodArgs)
        elif isinstance(args, list):
            for i in range(len(args)):
                # Do a str conversion in case some vals were entered as 
                #   floats or ints.
                arg = str(args[i])
                methodArgs += arg.split()
            _pyXspec.doXspecCmd(methodArgs)
        else:
            raise Exception("Error: method argument must be a string or list.")
    method = property(_getMethod, _setMethod, 
        doc="""The fitting algorithm to use [string]. 

               Choices are: 'leven', 'migrad', 'minimize', 'monte',
               'simplex'.  The default is 'leven'.

               When setting the method, additional arguments for 
               <nFitIterations> and <fit critical delta> may also be
               entered.  Valid formats for entering multiple 
               arguments are::

                  # Single string
                  Fit.method = "migrad  100 .05"
                  # List of strings
                  Fit.method = ["migrad","100",".05"]
                  # List of strings and numbers
                  Fit.method = ["migrad", 100, .05]
               """)
    
    def _getNIterations(self):
        settings = _pyXspec.getFitSettings()
        return settings[2]         
    def _setNIterations(self, val):
        if isinstance(val, int):
            if val <= 0:
                raise Exception("Error: nIterations value must be > 0")
            else:
                #nIterations is the 2nd arg sent to the method command.
                methodArgs = ["method"]
                nIterStr = "," + str(val)
                methodArgs.append(nIterStr)
                _pyXspec.doXspecCmd(methodArgs)                                
        else:
            raise Exception("Error: nIteration argument must be an integer > 0")
    nIterations = property(_getNIterations, _setNIterations,
        doc="""The maximum number of fit iterations prior to query [int].""")
    
    def _getQuery(self):
        return self.__query
    def _setQuery(self, val):
        if not isinstance(val, str) or len(val) == 0:
            raise Exception("Error: Query argument must be a string.")
        else:
            firstChar = val.lower()[0]
            if firstChar == 'o':
                self.__query = "on"
            elif firstChar == 'y':
                self.__query = "yes"
            elif firstChar == 'n':
                self.__query = "no"
            else:
               raise Exception("Error: Valid query settings are 'yes' | 'no' | 'on'")
            _pyXspec.setQuery(firstChar)
    query = property(_getQuery, _setQuery, 
                        doc="""The fit query setting [string].
                        
                           - 'yes': Fit will continue through query.
                           - 'no' : Fit will end at query.
                           - 'on' : User will be prompted for "y/n" response.""")
           
    def _getStatistic(self):
        return _pyXspec.getStatistic()
    def _setStatistic(self, value):
        raise Exception("Error: Cannot rebind the Fit statistic value.")
    statistic = property(_getStatistic, _setStatistic, 
        doc="""Fit statistic value from the most recent fit [float] (GET only).""")
    
    def _getStatMethod(self):
        settings = _pyXspec.getFitSettings()
        return settings[0]    
    def _setStatMethod(self, args):
        statArgs = ["statistic"]
        if isinstance(args, str):
            statArgs += args.split()
            _pyXspec.doXspecCmd(statArgs)
        elif isinstance(args, list):
            for i in range(len(args)):
                # Do a str conversion in case some vals were entered as 
                #   floats or ints.
                arg = str(args[i])
                statArgs += arg.split()
            _pyXspec.doXspecCmd(statArgs)
        else:
            raise Exception("Error: statMethod argument must be a string or list")
    statMethod = property(_getStatMethod, _setStatMethod, 
           doc="""The type of fit statistic in use [string].
           
                  Valid names: 'chi' | 'cstat' | 'lstat' | 'pgstat' | 
                  'pstat' | 'whittle'.  To set for individual spectra, 
                  add a spectrum number (or range) to the string: ie.
                   
                  >>> Fit.statMethod = "cstat 2"
               
               """)
    
    def _getStatTest(self):
        settings = _pyXspec.getFitSettings()
        return settings[5]    
    def _setStatTest(self, args):
        statArgs = ["statistic"]
        statArgs.append("test")
        if isinstance(args, str):
            statArgs += args.split()
            _pyXspec.doXspecCmd(statArgs)
        elif isinstance(args, list):
            for i in range(len(args)):
                # Do a str conversion in case some vals were entered as 
                #   floats or ints.
                arg = str(args[i])
                statArgs += arg.split()
            _pyXspec.doXspecCmd(statArgs)
        else:
            raise Exception("Error: statTest argument must be a string or list")
    statTest = property(_getStatTest, _setStatTest, 
         doc="""The type of test statistic in use [string].
         
                Valid names: 'ad' | 'chi' | 'cvm' | 'ks' | 'pchi' |
                'runs'.  To set for individual spectra, add a spectrum 
                number (or range) to the string: ie. 
                
                >>> Fit.statTest = "ad 2"
                
                """)
    
    def _getTestStatistic(self):
        return _pyXspec.getTestStatistic()
    def _setTestStatistic(self, value):
        raise Exception("Error: Cannot rebind the test statistic value.")
    testStatistic = property(_getTestStatistic, _setTestStatistic, 
        doc="""Test statistic value from the most recent fit [float] (GET only).""")
    
    def _getWeight(self):
        tcloutArgs = ["weight"]
        return _pyXspec.doTclout(tcloutArgs)
    def _setWeight(self, value):
        if isinstance(value,str):
            weightArgs = ["xset","weight"]
            # Use split() rather than append() to prevent any whitespace
            # user may have entered from getting through to PyListToXSArgs.
            weightArgs += value.split()
            _pyXspec.doXspecCmd(weightArgs)
        else:
            raise Exception("Error: Weight argument must be a string")       
    weight = property(_getWeight,_setWeight,
        doc="""Change the weighting function used in the calculation of chi-sq [string].
        
               Available functions: 'standard', 'gehrels', 'churazov', 'model'
               """)
    
    def error(self, argString):
        """Determine confidence intervals of a fit.
        
           Args:
              *argString*: 
                    A string with identical syntax to the standard
                    interactive XSPEC error command.
                           
                    "[[stopat <ntrial> <toler>] [maximum <redchi>]
                    [<delta fit statistic>] [<model param range>...]]"
                           
                    where <model param range> =::[<modelName>:]<first param> -
                                                 <last param>
                                                 
                    See the XSPEC manual for a more detailed description.
                   
           The results of the error command are stored in the *error* attributes
           of the individual Parameter objects.        
                   
           Examples::
           
              # Estimate the 90% confidence ranges for parameters 1-3
              Fit.error("1-3")
              # Repeat but with delta fit statistic = 9.0, equivalent to the
              # 3 sigma range.
              Fit.error("9.0")
              # Estimate for parameter 3 after setting the number of trials to 20.
              # Note that the tolerance field has to be included (or skipped over).
              Fit.error("stop 20,,3")
                      
        """
        if isinstance(argString, str):
            errorArgs = ["error"]
            errorArgs += argString.split()
            _pyXspec.doXspecCmd(errorArgs)
        else:
            raise Exception("Error: error function argument must be a string")
    
    def ftest(self, chisq2, dof2, chisq1, dof1):
        """Calculate the F-statistic and its probability given new and old
           values of chisq and number of degrees of freedom (DOF).
           
           Args:   
             *chisq2*: [float]
             
             *dof2*: [int]
             
             *chisq1*:  [float]
             
             *dof1*: [int]
                   
           *chisq2* and *dof2* should come from a new fit, in which an extra model
           component was added to (or a frozen parameter thawed from) the
           model which gave *chisq1* and *dof1*.  If the F-test probability is
           low then it is reasonable to add the extra model component.
           
           .. warning:: 
             It is not correct to use the F-test statistic to test
             for the presence of a line (see Protassov et al 2002, ApJ 571,
             545).

           Returns: The F-test probability [float].
            
        """
        ftestArgs = ["ftest"]
        badVar = ""
        badType = ""
        if not isinstance(chisq2, float) and not isinstance(chisq2, int):
            badVar = "chisq2"
            badType = "float"
        if not len(badVar) and not isinstance(dof2, int):
            badVar = "dof2"
            badType = "int"
        if not len(badVar) and (not isinstance(chisq1, float) and
                not isinstance(chisq1, int)):
            badVar = "chisq1"
            badType = "float"
        if not len(badVar) and not isinstance(dof1, int):
            badVar = "dof1"
            badType = "int"
        if len(badVar):
            err = "Error: Argument " + badVar + " must be of type " + badType
            raise Exception(err)
            
        ftestArgs.append(str(chisq2))
        ftestArgs.append(str(dof2))
        ftestArgs.append(str(chisq1))
        ftestArgs.append(str(dof1))
        _pyXspec.doXspecCmd(ftestArgs)
        return float(_pyXspec.doTclout(["ftest"]))
    
    def goodness(self, nRealizations=100, sim=False):
        """Perform a Monte Carlo calculation of the goodness-of-fit.
        
           Args:
             *nRealizations*: Number of spectra to simulate [int].
             
             *sim*: flag [bool]
             
                    If False (default), all simulations are drawn from
                    the best fit model parameter values.  If True,
                    parameters will be drawn from a Gaussian centered
                    on the best fit.
        
        """
        if not isinstance(nRealizations, int) or nRealizations < 1:
            err = "Error: nRealizations arg must be an int, and > 0"
            raise Exception(err)
        if not isinstance(sim, bool):
            raise Exception("Error: sim argument must be a bool")
        goodnessArgs = ["goodness"]
        goodnessArgs.append(str(nRealizations))
        if sim:
            goodnessArgs.append("sim")
        else:
            goodnessArgs.append("nosim")
        _pyXspec.doXspecCmd(goodnessArgs)   
        return float(_pyXspec.doTclout(["goodness"]))    
    
    def improve(self):
        """Try to find a new minimum.
        
           When *Fit.method* is set to one of the MINUIT algorithms, this
           will run the MINUIT 'improve' command.  This does nothing when
           Fit.method is set to Levenberg-Marquardt.
           
        """
        improveArgs = ["improve"]
        _pyXspec.doXspecCmd(improveArgs)
        
    def perform(self):
        """Perform a fit."""
        
        fitArgs = ["fit"]
        _pyXspec.doXspecCmd(fitArgs)
    
    def renorm(self, setting=None):
        """Renormalize the model to minimize statistic with current parameters.
        
        Args:
          *setting*: [string]
           
                     If ``None``, this will perform an explicit immediate 
                     renormalization.  Other options determine when
                     renormalization will be performed automatically.  They
                     are the following strings:
                   
                     - 'auto': Renormalize after a model command or parameter
                       change, and at the beginning of a fit.
                     
                     - 'prefit': Renormalize only at the beginning of a fit.
                     
                     - 'none': Perform no automatic renormalizations.
        
        """
        renormArgs = ["renorm"]
        if setting:
            # Only test for string type here.  The command handler naturally 
            # does more rigorous testing.
            if not isinstance(setting, str):
                err="Error: renorm argument must either be None, "
                err += "or one of the strings:\n"
                err+= "      'auto', 'prefit', or 'none'"
                raise Exception(err)
            renormArgs += setting.split()
        _pyXspec.doXspecCmd(renormArgs)
        
    def show(self):
        """Show fit information."""        
        _pyXspec.showFit()
        
    def steppar(self, argString):
        """Perform a steppar run.
        
           Generate the statistic "surface" for 1 or more parameters.
           
           Args:
             *argString*: [string]
             
              This uses identical syntax to the standard
              interactive XSPEC steppar command.
              "<step spec> [<step spec> ...]" where:

              <step spec> ::= [<log|nolog>] [<current|best>]
                    [<modName>:]<param index> <low value> <high value> <# steps>
                    
              See the XSPEC manual for a more detailed description of specs.
              
            Examples::
             
               # Step parameter 3 from 1.5 to 2.5 in 10 linear steps
               Fit.steppar("3 1.5 2.5 10")
               # Repeat the above but with logarithmic steps
               Fit.steppar("log")
               # Step parameter 2 linearly from -.2 to .2 in steps of .02
               Fit.steppar("nolog 2 -.2 .2 20")
              
        """
        if isinstance(argString, str):
            stepparArgs = ["steppar"]
            stepparArgs += argString.split()
            _pyXspec.doXspecCmd(stepparArgs)
        else:
            raise Exception("Error: steppar argument must be a string")
            
    def stepparResults(self, arg):
        """Retrieve values from the most recent steppar run.
        
           Args:
             *arg*: 
               
                argument should either be 'statistic', 'delstat',
                or a parameter specifier.  A parameter specifier
                should be a string of the form:
                
                '[<modName>:]<parNum>' 
                
                or simply an integer <parNum>.
           
           Returns the requested values as a list of floats.
        """
        tcloutArgs = ["steppar"]
        if isinstance(arg, str):
            tcloutArgs.append(arg)
        elif isinstance(arg, int):
            tcloutArgs.append(str(arg))
        else:
            raise Exception("Error: Invalid argument for steppar results.") 
        sl = _pyXspec.doTclout(tcloutArgs).split()
        fl = list()
        for s in sl:
            fl.append(float(s))
        return fl
         

Fit = FitManager()
