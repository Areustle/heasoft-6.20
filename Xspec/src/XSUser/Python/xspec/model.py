from __future__ import print_function
from __future__ import absolute_import

from . import _pyXspec
from .xset import _AttrRestrictor, Xset
from .parameter import Parameter, _ModParam
import inspect

class Component(_AttrRestrictor):
    """**Model component class**.
    
      **Attributes**
    
        - name
                The full name of the Component (get only).
       
        - <parameters>    
                Component contains an attribute of type **Parameter** for
                every parameter in the component.  The attribute
                name is the same as the parameter name in xspec.
                            
        - parameterNames
                List of Component's parameter names (get only).
    
    """             
    def __init__(self, compName, parNames):
        """Component constructor.
        
        Intended for creation by Model objects only.
        
        compName   -- The full xspec component name.  This will also
                        be the name of the attribute in the model object
        parNames   -- List containing component's parameter names.
                                          
        """
        self._turnRestrictOff()
        self.name = compName
        for parName in parNames:
           setattr(self, parName, Parameter(parName, _ModParam()))
        # After we set this attribute, __setattr__ will know which attributes
        # are Parameter objects and will handle them differently.
        self.parameterNames = parNames
        self._resetRestrict()
        
    def __setattr__(self, attrName, value):
        # If parameterNames list has been added, we know we've already
        # gone through the __init__ function for this object.
        if hasattr(self, 'parameterNames'):
            if attrName in self.parameterNames:
                # OK, this is subtle.  When user does something like
                # "m.powerlaw.norm += <val>", it will end up here due to
                # an internal call of the kind "comp.__setattr__('norm',x)
                # where x is a temp parameter object containing the post
                # += results.  We do NOT want to try to set par.values
                # equal to this in the code block below -- x is not a float.
                # So, ignore it altogether.  
                
                # Note that this is OK to do only because we've modified
                # the parameter object in-place in the += handler (ie. it
                # returns "self"). Therefore the call to __setattr__ is
                # redundant: the 'norm' parameter already IS x. 
                if not isinstance(value, Parameter):
                    # Treat as if setting parameter's val.
                    par = getattr(self, attrName)
                    par.values = value
            elif attrName == 'name':
                raise Exception("Error: Cannot rebind a Component object's name")
            elif attrName == 'parameterNames':
                raise Exception("Error: Cannot rebind a Component object's parameter names list")
            else:
                super(Component,self).__setattr__(attrName, value)                
        else:
            super(Component,self).__setattr__(attrName, value)      
                          
                        

class Model(_AttrRestrictor):
    """**Xspec model class.**
    
    **Methods**
    
      - __init__
      - __call__ (the '()' operator)
      - energies
      - folded
      - setPars
      - show
      - showList
      - untie
      - values
    
    **Attributes** (all are get-only)
        
       - expression  
              The model expression string, using full component names.
    
       - name   
              The model name, optional in Xspec.
              This is an empty string for un-named models.
                          
       - <components>    
              Model includes an attribute of type **Component** for every
              Xspec component in the model.  The attribute name is
              the same as the full name of the Xspec component 
              (ie. m=Model("po") produces an m.powerlaw 
              attribute).

       - componentNames
              List of component name strings.
       
       - flux           
              A tuple containing the results of the most recent flux
              calculation for this model.
                            
              The tuple values are: (*value*, *errLow*, *errHigh* (in 
              ergs/cm^2), *value*, *errLow*, *errHigh* (in photons)).  
              This will be filled in after an *AllModels.calcFlux()*
              call ONLY when no spectra are loaded.  Otherwise 
              results are stored in the Spectrum objects.
                            
       - lumin 
              Same as *flux* but for luminosity calculations.
                             
       - nParameters  
              Number of parameters in Model object [int].
       
       - startParIndex  
              Global index of the first parameter in this Model
              object [int].
    
    """    
    def __init__(self, exprString, modName="", sourceNum=1, setPars=None):
        """Model constructor.
       
        New model is automatically added to the *AllModels* container, with
        one Model object constructed (internally) for each data group to 
        which the model applies.  This function returns the Model object
        corresponding to the lowest numbered data group.
       
        Args:
           *exprString*: The model expression string.
           
                         Component names may be abbreviated.
                         
           *modName*:  Optional name assigned to model.  
           
                       Any whitespace in string will be removed.  This is 
                       required if souce number is > 1.
                         
           *sourceNum*:  Optional integer for model's source number.
                        
           *setPars*:  Optional initial values for the model's parameters.
           
                       These may be sent in a tuple, list, or dictionary
                       (or as a single float or string if only setting the
                       first parameter).  Examples::
                       
                         # Create a model with all default parameter settings:
                         m1 = Model("gauss")

                         # Create wabs*powerlaw and initialize pars 1 and 3 to
                         #   something other than their default values.
                         m2 = Model("wa*po", setPars={1:5.5, 3:".18,,.01,.02"})

                         # Create another model named 'b', and reset par 2 to 5.0:
                         m3 = Model("wa*bbody", "b", setPars={2:5.0})
                       
                       If any mistakes are made with the optional *setPars* 
                       parameter arguments, the model will be created using 
                       all default values.
                       
                       You can always reset the parameters later with the
                       *Model.setPars()* method, or directly through the Parameter
                       object's *values* attribute.
                                               
        """
        if (type(exprString).__name__ == 'PyCObject' or
                 type(exprString).__name__ == 'PyCapsule'):
            # Private use only: If exprString is actually a C-pointer,
            # treat it as a handle to an already existing Model in Xspec.
            modTuple = _pyXspec.getModelTuple(exprString)
        else:
            # Syntax requirements: modName string must contain NO whitespace,
            # exprString must not begin with "clear", "none", "act", or "inact" 
            # (case-insensitive). Any of these cases will foul up the lower level
            # parsing code.
            if not isinstance(modName,str):
                msg="Error: If 2nd argument is supplied, it must be a model name string."
                raise Exception(msg)
            if not isinstance(sourceNum,int):
                msg="Error: If 3rd argument is supplied, it must be an integer for source number."
                raise Exception(msg)
            
            modName = ''.join(modName.split())
            testExpr = exprString.lower().lstrip()
            if (not testExpr.find("none") or not testExpr.find("clear")
                  or not testExpr.find("act") or not testExpr.find("inact")):
                raise Exception("Error: 'clear','none','act', and 'inact' forbidden in this context")
            modTuple = _pyXspec.createModel(exprString, modName, sourceNum)
        
        self._turnRestrictOff()
        self.name = modName
        self._handle = modTuple[0]
        compNames = modTuple[1]
        # While no built-in Xspec component names have whitespace, table
        # model names might.  (These come from strings in the table model's
        # primary header.)  This replaces whitespace with underscores:
        _replaceWhitespace(compNames, '_')
        self.nParameters = modTuple[5]
        self.expression = modTuple[6]
        # Search for duplicate Components and append '_<n>' in order to
        # distinguish their attribute names.
        _appendCompName(compNames)
        parIdx = 1
        # Convert component names into model attributes
        for i in range(len(compNames)):
            # This gets its info from the lowest dg copy.
            parList = _pyXspec.getComponentPars(self.name,i+1)
            setattr(self, compNames[i], Component(compNames[i], parList))
            # Now reach in and set new parameters' storage of index number
            # and parent handle.  The index number is 1-based and relative
            # to the start of the particular Model object.
            # Only the Model and Parameter classes need know about these
            # two attributes.
            compAttr = getattr(self, compNames[i])
            for parName in parList:
                parAttr = getattr(compAttr, parName)
                parAttr._Parameter__index = parIdx
                parAttr._Parameter__parent = self._handle
                parIdx += 1
        # After we set this attribute, __setattr__ will know which attributes
        # are Component objects and will handle them differently.
        self.componentNames = compNames
        self._resetRestrict()
        
        if setPars is not None:
            try:           
                indAndVals = _collateSetPars(self, setPars)
                _pyXspec.setPars(self._handle, tuple(indAndVals[0]), 
                                tuple(indAndVals[1]), 0)

            except Exception as e:
                msgStr = str(e)
                msgStr += "\nError: Model parameters will retain their default values."
                print(msgStr)
                if Xset.log is not None:
                    print(msgStr,file=Xset.log)
                return
            self.show()
        
    def __setattr__(self, attrName, value):
        # If componentNames list has been added, we know we've already
        # gone through the __init__ function for this object.
        rebindErr = "Error: Cannot rebind a Model object's "
        if hasattr(self, 'componentNames'):
            if attrName in self.componentNames:
                raise Exception(rebindErr + "component")
            elif attrName == 'expression':
                raise Exception(rebindErr + "expression")
            elif attrName == 'name':
                raise Exception(rebindErr + "name")
            elif attrName == 'componentNames':
                raise Exception(rebindErr + "component names list")
            elif attrName == 'nParameters':
                raise Exception(rebindErr + "nParameters")
            elif attrName == 'startParIndex':
                raise Exception(rebindErr + "startParIndex")
            elif attrName == 'flux':
                raise Exception(rebindErr + "flux")
            elif attrName == 'lumin':
                raise Exception(rebindErr + "lumin")
            else:
                super(Model,self).__setattr__(attrName, value)                
        else:
            super(Model,self).__setattr__(attrName, value)      

    def __call__(self, parIdx):
        """Get a Parameter object from the Model.
        
        Args:
           *parIdx*:  The parameter index number.  
           
                      Regardless of the data group to which the Model object 
                      belongs, its parameters are numbered from 1 to 
                      *nParameters*.  
                  
        Returns the specified Parameter object.
        
        """
        if not isinstance(parIdx, int):
            raise Exception("Error: Argument must be an integer.")
        if parIdx < 1 or parIdx > self.nParameters:
            indexErr = "Error: Valid range of parameters for this object: "
            indexErr += "1-" + str(self.nParameters)
            raise Exception(indexErr)
            
        for compName in self.componentNames:
            compAttr = getattr(self, compName)
            for parName in compAttr.parameterNames:
                parAttr = getattr(compAttr, parName)
                if parIdx == parAttr._Parameter__index:
                    return parAttr
         
        raise Exception("Error: Unable to locate Parameter in Model")  
             
    
    def energies(self, spectrumIndex):
        """Get the Model object's energies array for a given spectrum.
        
        Args:
           *spectrumIndex*: The spectrum index number.  
           
                            If this is 0, it will return the energies array 
                            used by the default dummy response.
                        
        Returns a list of energy array elements, the size will be 1 larger than
        the corresponding flux array.
           
        This will return the energies array as specified by the
        *AllModels.setEnergies* function if that has been used to override
        the response energies array.
        
        """
        if not isinstance(spectrumIndex, int):
            raise Exception("Error: Argument must be a spectrum number (int).")
        tclArgs = ["energies"]
        # The tclout energies function doesn't care which model object is
        # assigned to the spectrum number, but in this context it matters.
        # So we must check here.
        if spectrumIndex != 0:
            specNums = set(_pyXspec.getSpectraForModel(self._handle))
            if spectrumIndex not in specNums:
                errMsg = "Error: model object is not used with spectrum "
                errMsg += str(spectrumIndex)
                raise Exception(errMsg)
        tclArgs.append(str(spectrumIndex))
        s = _pyXspec.doTclout(tclArgs)
        return s.split()
        
    def folded(self, spectrumIndex):
        """Get the Model object's folded flux array for a given spectrum.
        
        Args:
           *spectrumIndex*:  The spectrum index number.  
           
                             This number should be 0 if model is not presently 
                             applied to any spectra (ie. in the "off" state).
                        
        Returns a list of folded flux array elements.
        
        """
        if not isinstance(spectrumIndex, int):
            raise Exception("Error: Argument must be a spectrum number (int).")
        return _pyXspec.getArray(self._handle, spectrumIndex, 1)
    
    def setPars(self, *parVals):
        """Change the value of multiple parameters in a single function call.
        
        This is a quick way to change multiple parameter values at a time
        since only a SINGLE model recalculation will be performed at the end.
        In contrast, when parameter values are changed through the individual 
        parameter objects, the model is recalculated after EACH parameter
        change.  (See also *AllModels.setPars()*, for changing multiple parameters
        belonging to multiple model objects.)
        
        Args:
           *parVals*:  An arbitrary number of parameter values.  
           
                       These may be listed singly (as floats or strings), or 
                       collected into tuple, list or dictionary containers.
                       Dictionaries must be used if parameters are not in
                       consecutive order, in which case the parameter index
                       number is the dictionary key.
                                            
        Examples:  Assume we have a model object *m1* with 5 parameters.
           
          Simplest case: change only the parameter values (and not the
          auxiliary values, 'sigma', 'min', 'bot', etc.), and change
          them in consecutive order::
           
             # Pass in 1 or more floats
             m1.setPars(5.5, 7.83, 4.1e2)  # changes pars 1-3
             m1.setPars(2.0, 1.3e-5, -.05, 6.34, 9.2)  # changes all 5 pars
             
          Still changing only the parameter values, but skipping over some::
          
             m1.setPars(.02, 4.4, {5:3.2e5})  # changes pars 1-2, 5
             m1.setPars({2:3.0, 4:-1.2})  # changes pars 2, 4
             m1.setPars({2:1.8}, 9.3, 5.32)  # changes pars 2, 3, 4
             
          Now also change the auxiliary values for some of the parameters.
          Pass in a STRING containing "<val>,<sigma>,<min>,<bottom>,<top>,
          <max>"  This uses the same syntax as Standard XSPEC's "newpar"
          command.  Aux values can be skipped by using multiple commas::
          
             # This sets a new <val>, <sigma>, and <max> for parameter 1, and 
             # a new <val> of 5.3 for parameter 2.
             m1.setPars(".3,.01,,,,100", 5.3)
             
             # This sets all new auxiliary values for parameter 3.
             m1.setPars({3:".8 -.01 1e-4 1e-3 1e5 1e6"})
             
        """
        indAndVals = _collateSetPars(self, *parVals)
        _pyXspec.setPars(self._handle, tuple(indAndVals[0]), tuple(indAndVals[1]), 0)
    
    def show(self):
        """Display information for a single Model object."""
        _pyXspec.showModel(self._handle)
    
    def showList():
        """Show the list of all available XSPEC model components."""
        _pyXspec.doXspecCmd(["model","?"])
    showList = staticmethod(showList)
    
    def untie(self):
        """Remove links for all parameters in Model object"""
        untieArgs=["untie"]
        parRange = ""
        if len(self.name):
            parRange += self.name + ":"
        startIdx = self.startParIndex
        endIdx = startIdx + self.nParameters - 1
        parRange += str(startIdx)
        if endIdx > startIdx:
            parRange += "-" + str(endIdx)
        untieArgs.append(parRange)
        _pyXspec.doXspecCmd(untieArgs)
    
    def values(self, spectrumIndex):
        """Get the Model object's values array for a given spectrum.
        
        Args:
           *spectrumIndex*:  The spectrum index number.  
           
                             This number should be 0 if model is not presently 
                             applied to any spectra (ie. in the "off" state).
                        
        Returns the values array as a list.
        
        """
        if not isinstance(spectrumIndex, int):
            raise Exception("Error: Argument must be a spectrum number (int).")
        return _pyXspec.getArray(self._handle, spectrumIndex, 0)
        
    # Any property set functions would be hidden by __setattr__
    
    def _getFlux(self):
        return _pyXspec.getModelFluxLuminCalc(self._handle, 1)   
    flux = property(_getFlux) 

    def _getLumin(self):
        return _pyXspec.getModelFluxLuminCalc(self._handle, 0)
    lumin = property(_getLumin) 
    
    def _getStartParIndex(self):
        return _pyXspec.getModelTuple(self._handle)[4]
    startParIndex = property(_getStartParIndex)
    
    # End Model class
            
        
class ModelManager(_AttrRestrictor):
    """**Models container.**
    
    PyXspec automatically creates a single object of this class,
    named *AllModels*.
    
      **Methods**
      
      .. hlist::
         :columns: 2
      
         - __call__ (the '()' operator)
         - __iadd__ (the '+=' operator)
         - __isub__ (the '-=' operator)
         - addPyMod
         - calcFlux
         - calcLumin
         - clear
         - eqwidth
         - initpackage
         - lmod
         - setEnergies
         - setPars
         - show
         - simpars
         - tclLoad
      
      **Attributes**
          
      - sources (get-only)                                  
      - systematic
    
    """
    __single = None
    def __init__(self):
        if ModelManager.__single:
            raise Exception("Error: Only 1 instance of ModelManager is allowed.")
        ModelManager.__single = self
        self._turnRestrictOff()
        self.__systematic = 0.0
        self._resetRestrict()
    
    def _getSources(self):
        return _pyXspec.getModelSourceAssignments()
    def _setSources(self, val):
        raise Exception("Error: Cannot rebind AllModels.sources attribute") 
    sources = property(_getSources, _setSources,
         doc="""A dictionary containing the currently active <source number>:<model name> assignments.

                If the model has no name, <model name> will
                be an empty string.  (GET only)   """) 
        
    def _getSystematic(self):
        return self.__systematic
    def _setSystematic(self, val):
        if not isinstance(val, float) and not isinstance(val, int):
            raise Exception("Error: Systematic error must be numeric type.")
        if val < .0 or val >= 1.0:
            raise Exception("Error: Systematic error must be .0 <= syst < 1.0")
        systArgs = ["systematic"]
        systArgs.append(str(val))
        _pyXspec.doXspecCmd(systArgs)
        self.__systematic = val
    systematic = property(_getSystematic, _setSystematic,
                doc="""The fractional model systematic error.
                 
                       This will be added in quadrature to the error
                       on the data when evaluating chi-squared.  The
                       default value is zero.""")

    def __call__(self, groupNum, modName=""):
        """Get Model objects from the AllModels container.
        
        Args:
           *groupNum*: The data group number to which the Model object corresponds.
        
           *modName*: Optional string containing the Model's name (if any).
        
        Returns the Model object.
        
        """
        if not isinstance(groupNum, int) or groupNum < 1:
            raise Exception("Error: Must enter integer >= 1 for data group number.")
        if modName.isspace():
            modName = ""
        return Model(_pyXspec.getModelFromNameAndGroup(modName, groupNum),modName)
    
    def __iadd__(self, modelInfo):
        """Define a new model and add it to the *AllModels* container.
        
        This operation is equivalent to the Model class constructor,
        except that it does not return a Model object.
        
        Args:
           *modelInfo*: 
                        A string containing the model expression
                        (component names may be abbreviated).   The model will 
                        be unnamed and assigned to source number = 1.
                        
                        OR
                     
                        If supplying a model name and a source number, this
                        should be a tuple with:
                     
                        modelInfo[0] = model expression string
                        
                        modelInfo[1] = model name string
                        
                        modelInfo[2] = source number
                        
        """
        # Syntax requirements: modName string must contain NO whitespace,
        # exprString must not begin with "clear", "none", "act", or "inact" 
        # (case-insensitive). Any of these cases will foul up the lower level
        # parsing code.
        modName = str("")
        sourceNum = 1
        if isinstance(modelInfo, str):
            exprString = modelInfo
        else:
            exprString = modelInfo[0]
            modName = modelInfo[1]
            if len(modelInfo) > 2:
                sourceNum = modelInfo[2]
        
        modName = ''.join(modName.split())
        testExpr = exprString.lower().lstrip()
        if (not testExpr.find("none") or not testExpr.find("clear")
              or not testExpr.find("act") or not testExpr.find("inact")):
            raise Exception("Error: 'clear','none','act', and 'inact' forbidden in this context")
        _pyXspec.createModel(exprString, modName, sourceNum)
        return self
        
    def __isub__(self, modName):
        """Remove all copies of the given model from the AllModels container.
        
        Args:
           *modName*: 
                      The name of the model to be removed, or an empty string if
                      the model has no name.   If set to "*", this will behave
                      like the *clear()* function and remove all models.
                      
        """
        if len(modName) == 0 or modName.isspace():
            modName = "unnamed"
        elif modName == '*':
            modName = ""
        _pyXspec.removeModels(modName)
        return self
    
    def addPyMod(self, func, parInfo, compType, calcsErrors=False, spectrumDependent=False):
        """Add a user-defined Python model function to XSPEC's models library.
        
           This provides a way to add to XSPEC local models written in Python.  
           It performs the same role as the combination of *initpackage*/*lmod* 
           commands do for C/C++/Fortran local models.  The first 3 arguments 
           (*func*, *parInfo*, and *compType*) are mandatory.
           
           Args:
              *func*:  The user-defined model function (Python type = 'function').
              
                       Function must define at least 3 arguments for energies,
                       parameters, and flux.
                            
                       A optional fourth argument may be added if your model
                       calculates flux errors, and a fifth if your model
                       requires that XSPEC pass it the spectrum number.
                            
              *parInfo*:  A tuple of strings.  
              
                          One string for each parameter your model requires.  
                          The format of these strings is identicalto what is 
                          placed in a 'model.dat' file (see Appendix C of
                          the XSPEC manual).
          
              *compType*: A string telling XSPEC the type of your model.
              
                          Currently allowed types:  'add', 'mul', 'con'
                            
              *calcsErrors*:  OPTIONAL bool flag. 
              
                              If your model function also calculates model errors,
                              set this to True.
                             
              *spectrumDependent*:  OPTIONAL bool flag.  
              
                                    Set this to True only if your model function
                                    has an explicit dependence on the spectrum.
                                     
           Example usage:  A local additive model written in Python, named 'myModel', 
                which takes parameters named 'par1' and 'par2'.
                
                .. code-block:: python
                
                   def myModel(engs, pars, flux):
                      #    [... your model code, fill in
                      #      flux array based on input
                      #      engs and pars arrays ...]

                   myModelParInfo=("par1  \"\" 2.0  -10.0  -9.0  9.0  10.0  0.01",
                                   "par2  keV  1e-3 1e-5  1e-5  100. 200. .01" )

                   AllModels.addPyMod(myModel, myModelParInfo, 'add')
                                                                                        
        """
        if not inspect.isfunction(func):
            raise Exception("Error: 1st argument must be a callable function.")
        # pre 2.6 returns argument information in a plain tuple, not an ArgSpec tuple. 
        # So for compatibility, avoid access of the inspect.ArgSpec.args member.   
        nFuncArgs = len(inspect.getargspec(func)[0])
        if nFuncArgs < 3:
            err="Error: Model function " + func.__name__ +" does not have the required arguments.\n"
            err+= "    Needs: engs, params, flux."
            raise Exception(err)   
        if nFuncArgs > 5:
            err="Error: Model function " + func.__name__+" has too many arguments.\n"
            err+= "    Maximum allowed:  engs, params, flux, fluxerr, specnum."
            raise Exception(err)
        
        isParInfoOK=False
        if isinstance(parInfo, tuple):
            allStrings=True
            for val in parInfo:
                if not isinstance(val, str):
                    allStrings=False
            if allStrings:
                isParInfoOK=True
        if not isParInfoOK:
            err="Error: 2nd argument must be a tuple of strings,\n"
            err +="    one string for each parameter in model."
            raise Exception(err)
        isCompTypeOK=False
        allowedCompTypes=["add","mul","con"]
        if isinstance(compType, str):
            if compType in allowedCompTypes:
                isCompTypeOK=True
        if not isCompTypeOK:
            err="Error: 3rd argument must be a valid component type string: "
            for s in allowedCompTypes:
                err+= s+"  "
            err+="\n"
            raise Exception(err)       
        _pyXspec.addPyComp(func, parInfo, compType, calcsErrors, spectrumDependent, func.__name__)
        
    def calcFlux(self, cmdStr):
        """Calculate the model flux for a given energy range.
        
        Args:
           *cmdStr*: string
           
                     Should contain the energy limit values and
                     optional error specifiers. This follows the same 
                     syntax rules as the standard XSPEC 'flux' command.
        
        The flux will be calculated for all loaded spectra, and the results
        will be stored in the Spectrum objects' *flux* attribute.  If no
        spectra are loaded, the flux will be stored in the Model objects'
        *flux* attribute.
        
        """
        if isinstance(cmdStr, str):
            _pyXspec.fluxCmd(1, cmdStr.split())
        else:
            raise Exception("Error: Argument to calcFlux must be a string")
        
    def calcLumin(self, cmdStr):
        """Calculate the model luminosity for a given energy range and redshift.
        
        Args:
           *cmdStr*: string 
        
                     Should contain the energy limit values and
                     optional error specifiers.  This follows the same
                     syntax rules as the standard XSPEC 'lumin' command.
        
        The lumin will be calculated for all loaded spectra, and the results
        will be stored in the Spectrum objects' *lumin* attribute.  If no
        spectra are loaded, the flux will be stored in the Model objects'
        *lumin* attribute.
        
        """
        if isinstance(cmdStr, str):
            _pyXspec.fluxCmd(0, cmdStr.split())
        else:
            raise Exception("Error: Argument to calcLumin must be a string")
        
    def clear(self):
        """Remove all models."""
        _pyXspec.removeModels("")
    
    def eqwidth(self, component, rangeFrac=None, err=False, number=None, 
                        level=None):
        """Calculate the equivalent width of a model component.
        
        Please see the Standard XSPEC Manual for a discussion on how the eqwidth
        of a component is calculated.
        
        Args:
           *component*:
                     An integer specifying the model component number for 
                     which to calculate the eqwidth (left-most component 
                     is 1).  If the component belongs to a NAMED model, 
                     then this must be a **string** of the form 
                     "<modelName>:<compNumber>".
                       
           *rangeFrac*:  Determines the energy range for the continuum calculation.
           
                      Range will be from E(1-<rangeFrac>) to E(1+<rangeFrac>) 
                      where E is the location of the peak of the photon 
                      spectrum.  The initial default rangeFrac is 0.05.  
                      Setting this will change the future default value.
                       
           *err*:  Bool flag.
           
                      If set to True, errors will be estimated on the equivalent
                      width calculation.  This will also require the setting of
                      the "number" and "level" arguments.
                       
           *number*:  Only set this if *err* = True.  
           
                      This determines the number of sets of randomized 
                      parameter values to draw to make the error estimation. 
                      [int]
                       
           *level*:  Only set this if *err* = True.  
           
                      The error algorithm will order the equivalent widths of 
                      the *number* sets of parameter values, and the central 
                      *level* percent will determine the error range.  [float]
                        
        The results of the most recent eqwidth calculation are stored as
        attributes of the currently loaded Spectrum objects.
        
        """
        eqwArgs = ["eqwidth"]
        if rangeFrac:
            if not isinstance(rangeFrac, float):
                err="Error: rangeFrac must be a floating-point value."
                raise Exception(err)
            eqwArgs.append("range")
            eqwArgs.append(str(rangeFrac))
        if isinstance(component, str):
            eqwArgs += component.split()
        elif isinstance(component, int):
            eqwArgs.append(str(component))
        else:
            err="Error: First arg must be a component number (integer), or a \n"
            err += "   modelName:compNum string for named models."
            raise Exception(err)
        if err:
            eqwArgs.append("err")
            if not number or not isinstance(number, int):
                err="Error: When calling eqwidth with errors, must set number\n"
                err +="   of trials to a positive integer."  
                raise Exception(err)
            eqwArgs.append(str(number))
            if not level or (not isinstance(level, float) and not
                        isinstance(level, int)):
               err="Error: When calling eqwidth with errors, must set confidence\n"
               err +="level to a floating-point value"
               raise Exception(err)
            eqwArgs.append(str(level))
        _pyXspec.doXspecCmd(eqwArgs) 
    
    def setEnergies(self, arg1, arg2=None):
        """Specify new energy binning for model fluxes.
        
        Supply an energy binning array to be used in model evalutations in place
        of the associated response energies, or add an extension to the response
        energies.
        
        Args:
           *arg1*:  A string containing either:
           
                    "<range specifier> [<additional range specifiers>...]"
                    
                    "<name of input ascii file>"
                    
                    "extend"  [This option also uses *arg2*]
                    
                    "reset"
                  
                where the first <range specifier> ::= <lowE> <highE> <nBins> log|lin
                
                <additional range specifier> ::= <highE> <nBins> log|lin
                
                This uses the same syntax as standard XSPEC's 'energies'
                command.  Values can be delimited by spaces or commas.
                
           *arg2*:  String only needed when *arg1* is "extend".
           
                    This requires an extension specifier string of the form:
                    "low|high <energy> <nBins> log|lin"
                  
        All energies are in keV.  Multiple ranges may be specified to allow for
        varied binning in different segments of the array, but note that no gaps
        are allowed in the overall array.  Therefore only the first range 
        specifier accepts a <lowE> parameter.  Additional ranges will
        automatically begin at the <highE> value of the previous range.
        
        With the "extend" option, the specifier string supplied to *arg2* will
        extend the existing response energy array by an additional <nBins> to
        the new <energy>, in either the high or low direction.
        
        Once an energy array is specified, it will apply to all models and will
        be used in place of any response energy array (from actual or dummy
        responses) for calculating and binning the model flux.  It will also
        apply to any models that are created after it is specified.  To turn off
        this behavior and return all models back to using their response
        energies, set *arg1* to "reset".
                  
        *arg1* can also be the name of an ascii text file containing a custom
        energy array.  To see the proper file format, and for more details in
        general about the 'energies' command, please see the standard XSPEC
        manual.
        
        Examples::
        
          # Create an array of 1000 logarithmic-spaced bins, from .1 to 50. keV
          AllModels.setEnergies(".1 50. 1000 log")
          # Change it to 500 bins
          AllModels.setEnergies(",,500")
          # Now restore original response energies, but with an extension of the
          #   high end to 75.0 keV with 100 additional linear bins.
          AllModels.setEnergies("extend","high,75.,100 lin")
          # Return to using original response energies with no extensions.
          AllModels.setEnergies("reset")
        
        """
        energiesArgs = ["energies"]
        if isinstance(arg1, str):
            if arg1.lower() == "extend":
                if isinstance(arg2, str):
                    energiesArgs.append("extend")
                    energiesArgs += arg2.split()
                else:
                    msg="Error: setEnergies extend option requires a string argument."
                    raise Exception(msg)    
            else:
                energiesArgs += arg1.split()
        else:
            raise Exception("Error: setEnergies arguments must be strings")
        _pyXspec.doXspecCmd(energiesArgs)
    
    def initpackage(self, packageName, modDescrFile, dirPath=None, udmget=False):
        """Initialize a package of local models.
        
        Use this method to compile your local model source code and build
        a library, which can then be loaded into XSPEC with the *lmod* method.
        
        Args:
           *packageName*:  The name of the model package [string].
           
                           The name should be all lower-case and contain NO
                           numerals or spaces.  The local models library file
                           will be based upon this name, and this is also the 
                           name you will use when loading the library with the 
                           *lmod* method.
                         
           *modDescrFile*:  Name of your local model description file [string].
           
                            This file is typically named 'lmodel.dat', but 
                            you're free to name it something else.
                          
           *dirPath*:  Optional directory path to your local models [string].
           
                       This may be an absolute or relative path.  If you 
                       don't enter this argument, XSPEC will look in the
                       directory given by the LOCAL_MODEL_DIRECTORY in your
                       Xspec.init start-up file.
                          
           *udmget*:  Optional bool flag. 
           
                      Set this True only when your models need to call XSPEC's
                      udmget function.  Udmget is a function for 
                      allocating dynamic memory in Fortran routines, and
                      is no longer used within XSPEC itself.  If this
                      flag is set to 'True', initpackage will copy the
                      necessary files and build the udmget function within
                      your local models directory.
        
        """
        if not isinstance(packageName, str):
            msg="Error: packageName argument must be a string"
            raise Exception(msg)
        packageName = packageName.strip()
        if not len(packageName):
            msg="Error: Cannot enter a blank for packageName"
            raise Exception(msg)
        if not isinstance(modDescrFile, str):
            msg="Error: modDescrFile argument must be a string"
            raise Exception(msg)
        modDescrFile = modDescrFile.strip()
        if not len(modDescrFile):
            msg="Error: Cannot enter a blank for modDescrFile"
            raise Exception(msg)
        initArgs = ["initpackage", packageName, modDescrFile]
        
        if dirPath is not None:
            if not isinstance(dirPath,str):
                msg="Error: dirPath argument must be a string"
                raise Exception(msg)
            initArgs.append(dirPath)
        if not isinstance(udmget, bool):
            msg="Error: udmget must be set to a bool (True | False)"
            raise Exception(msg)
        if udmget == True:
            initArgs.append("-udmget")
            
            
        _pyXspec.doXspecCmd(initArgs)
        
    def lmod(self, packageName, dirPath=None):
        """Load a local models library.
        
        Args:
           *packageName*:  The name of the model package to be loaded.  
           
                           This is the same name that is the first argument 
                           in the *initpackage* command.
                         
           *dirPath*:  An optional string argument. 
           
                       This should specify the (absolute or relative) path to 
                       the local model directory.  If this argument is not 
                       entered, XSPEC will look in the directory given by the 
                       LOCAL_MODEL_DIRECTORY in the Xspec.init start-up file.
                         
        """
        if not isinstance(packageName, str):
            raise Exception("Error: Package name argument must be a string")
        if len(packageName) == 0:
            raise Exception("Error: Package name string is empty")
        if dirPath and not isinstance(dirPath, str):
            raise Exception("Error: dirPath argument must be a string")
        
        dPath = dirPath
        if dPath is None:
           dPath = ""
        _pyXspec.localModel(packageName, dPath)
    
    def setPars(self, *args):
        """Change the value of multiple parameters from multiple 
              model objects with a single function call.
        
        This is a quick way to change multiple parameter values at a time
        since only a SINGLE recalculation will be performed at the end.
        In contrast, when parameter values are changed through the individual 
        parameter objects, the model is recalculated after EACH parameter
        change.  (If all the parameters belong to a single model object,
        you can also use the *Model.setPars()* function.)
        
        *args*:  An arbitrary number model objects and parameter values.
         
                 The first argument must be model object, followed by 
                 one or more of its new parameter values.  Additional
                 groups of model objects and parameter values may follow.
                     
                 The parameter values follow the same syntax rules as with
                 the single *Model.setPars()* function.  They can be listed
                 singly (as floats or strings), or collected into tuple,
                 list, or dictionary containers.  Dictionaries must be used
                 when parameters are not in consecutive order, in which case
                 the parameter index number is the dictionary key.
                     
                 Parameter indices are local to each model object.  That is,
                 they are always numbered from 1 to N where N is the number
                 of parameters in the model object.
                    
        Examples::
        
            # Assume we've already assigned a 3 parameter model to 2 data groups:
            m1 = AllModels(1)
            m2 = AllModels(2)
                    
            # Various ways of changing parameters in consecutive order.
            
            # This changes pars 1-2 in m1 and 1-3 in m2:
            AllModels.setPars(m1, .4, "1.3 -.01", m2, "5.3 ,,3.0e-4", 2.2, 1.9)
            #   ...and these 2 examples do the exact same thing as above:
            valList = [.4, "1.3 -.01"]
            valTuple = ("5.3 ,,3.0e-4", 2.2, 1.9)
            AllModels.setPars(m1, valList, m2, valTuple)
            AllModels.setPars(m1, valList, m2,"5.3 ,,3.0e-4", [2.2, 1.9])
            
            # Parameters in non-consecutive order, must use Python
            #   dictionaries:
            
            # Change parameter 2 in m1, parameter 1 and 3 in m2:
            AllModels.setPars(m1, {2:8.3}, m2, {1:0.99, 3:"7.15 -.01"})
            # ...same thing as above:
            AllModels.setPars(m1, {2:8.3}, m2, 0.99, {3:"7.15 -.01"})
            
            # Note that identical syntax is used for model objects belonging
            # to different sources.  All of the above examples are still valid
            # had we obtained m1 and m2 like this:
              
            m1 = Model("wabs*pow", "firstMod", 1)
            m2 = Model("gauss", "secondMod", 2)
                 
        """
        rawValArgs = []
        allModNames = []
        allIndices = []
        allVals = []
        iArg=0
        mod = None
        while iArg <= len(args):
            if (iArg == len(args) or
                  isinstance(args[iArg], Model)):
                if len(rawValArgs):
                    # These args belong to the previous model.
                    # It's time to collate them.
                    indAndVals = _collateSetPars(mod, *rawValArgs)
                    nPars = len(indAndVals[0])
                    allModNames += nPars*[mod.name]
                    allIndices += indAndVals[0]
                    allVals += indAndVals[1]
                    rawValArgs = []
                if iArg < len(args):
                    mod = args[iArg]
            else:
                if iArg == 0:
                    error="Error: First argument must be a model object."
                    raise Exception(error)
                rawValArgs.append(args[iArg])
            iArg += 1
            
        _pyXspec.setParsGlobal(tuple(allModNames),tuple(allIndices),
                        tuple(allVals))
            
    def show(self, parIDs=None):
        """Show all or a subset of Xspec model parameters.

        Args:
           *parIDs*:
                    An optional string specifying a range of parameters as
                    with Xspec's "show parameter" function.  If no string is
                    supplied, this will show all parameters in all models.

        """
        if parIDs is not None:
            _pyXspec.showPar(parIDs.split())
        else:
            _pyXspec.showPar([])    

    def simpars(self):
        """Create a list of simulated parameter values.
        
        Values are drawn from a multivariate normal distribution based on the
        covariance matrix from the last fit, or from Monte Carlo Markov chains
        if they are loaded.  This method is identical to doing 'tclout simpars'
        in standard XSPEC.
        
        Returns a tuple of the simulated parameter values.
        
        """
        parValStr = _pyXspec.doTclout(["simpars"])
        valList = parValStr.split()
        for i in range(len(valList)):
            valList[i] = float(valList[i])
        return tuple(valList)

    def tclLoad(self, fullLibPath):
        """Load a local model library by calling Tcl's 'load' command.
        
        This by-passes *lmod* (with its pkgIndex.tcl requirements) and
        allows the user to load a local model by directly calling
        Tcl's lower level 'load' command.  May also be useful for
        error diagnostics when *lmod* has failed.
        
        Args:
           *fullLibPath*: The full local model library path and filename.
        
        """        
        if not isinstance(fullLibPath, str):
            raise Exception("Error: fullLibPath argument must be a string")
        if len(fullLibPath) == 0:
            raise Exception("Error: fullLibPath string is empty")
        _pyXspec.localModel("", fullLibPath)
        
            
# End class ModelManager

AllModels = ModelManager()

    
    
def _appendCompName(compNames):
    """Append '_<n>' to duplicate Component names.
    
    Search left-to-right for duplicate component names in a model, and for
    all duplicates encountered after the first of their name, append '_<n>' 
    where <n> is the component's index number.  Note that this does NOT mean
    the nth copy of the component.
    
    Input:  compNames -- List of Component names of a model (l to r).
    Output: compNames -- Same list but with '_<n>' appended to names
                            where necessary.
                            
    """
    # Really want compsFound to be a set and not a list, but can't for
    # compatibility with Python 2.3.  (Sets weren't introduced till 2.4)
    compsFound = []
    for i in range(len(compNames)):
        name = compNames[i]
        if name in compsFound:
            compNames[i] = name + '_' + str(i+1)
        else:
            compsFound.append(name)

def _collateSetPars(mod, *argsTuple):
    """Organize the user's various input args into containers compatible with
          the C++ setPars and setGlobalPars functions.
       
    Internal use only
    
    mod       -- The Python model object to which the args are applied.
    argsTuple -- Arbitrary numbers of float, strings, lists, tuples, and 
                   dictionaries containing new parameter values.  The keys in 
                   the dictionaries are to be 1-based LOCAL parameter indices.
                   
    Returns a tuple of size 2, containing a list of the 1-based GLOBAL
      parameter indices and a list of parameter value strings.  The 2
      lists will be of the same size.   
       
    """
    # iPar is the 1-based GLOBAL index.
    startIdx = mod.startParIndex
    iPar=startIdx
    # First gather the indices and vals in a dictionary to prevent
    #  multiple appearances of a single par index in the final output
    #  lists.  When multiple entries are found, the later value will step
    #  on the earlier one and the user will be given a warning.
    # Following any dictionary entry, the iPar count will be set to the
    #   last iPar key in the dictionary PLUS ONE. 
    valsDict={}
    nPars = mod.nParameters
    for arg in argsTuple:
        if (isinstance(arg, float) or isinstance(arg, int) or
                isinstance(arg, str)):
            _collateCheckPar(valsDict, iPar, startIdx, nPars) 
            valsDict[iPar] = arg
            iPar += 1
        elif isinstance(arg, tuple) or isinstance(arg, list):
            for singleArg in arg:
                if (isinstance(singleArg, float) or isinstance(singleArg, int)
                        or isinstance(singleArg, str)):
                    _collateCheckPar(valsDict, iPar, startIdx, nPars) 
                    valsDict[iPar] = singleArg
                    iPar += 1
                else:
                    err="Error: Sequence contains invalid argument type: "
                    err += type(singleArg).__name__
                    raise Exception(err)
        elif isinstance(arg, dict):
            # Use of iteritems() vs items() is for Python 2 and 3 compatibility.
            try:
                keyvals = arg.iteritems()
            except AttributeError:
                keyvals = arg.items()
            for k, v in keyvals:
                if not isinstance(k, int):
                    err="Error: Dict key must be a parameter index integer."
                    raise Exception(err)
                if (k < 1):
                    err="Error: " + str(k) + " is not a valid parameter index."
                    raise Exception(err)
                k += (startIdx-1)
                if (isinstance(v, float) or isinstance(v, int) or
                        isinstance(v, str)):
                    _collateCheckPar(valsDict, k, startIdx, nPars)
                    valsDict[k] = v
                else:
                    err="Error: Dictionary entry contains invalid argument type: "
                    err += type(v).__name__
                    raise Exception(err)
                iPar = k+1        
        else:
            err="Error: Invalid parameter argument type: "
            err += type(arg).__name__
            raise Exception(err)

    indices = valsDict.keys()
           
    # By this point vals must be floats, ints, or strings.  Need to
    # convert all of them to strings.
    vals = list(valsDict.values())
    for i in range(len(vals)):
        val = vals[i]
        if not isinstance(val, str):
            vals[i] = str(val)
    return (indices, vals)
        
def _collateCheckPar(valsDict, iPar, startPar, nPar):
    """Utility function for _collateSetPars utility function.

    Internal use only.

    Issue warning for duplicate par and raise Exception for
      iPar > (startPar + nPar - 1)
      
      iPar is a GLOBAL index.

    """
    localIdx = iPar - startPar + 1
    if localIdx > nPar:
        error = "Error: Attempting to set par " + str(localIdx)
        error += " in a model with only " + str(nPar) + " parameters.\n"
        raise Exception(error)
    if iPar in valsDict:
        warn=("***Warning: Duplicate setPars entries for par " + 
          str(localIdx) + ".  Will use most recent.\n")
        print(warn)
        if Xset.log is not None:
            print(warn,file=Xset.log)
            
def _replaceWhitespace(stringList, repl):
    """Replace whitespace for every string in a list.
    
    Internal use only.
    
    For each string, leading and trailing whitespace will be removed
      entirely.  Internal whitespace will be replaced with the string
      given by 'repl'.  This was originally intended for replacing
      whitespace in Component names prior to using them to create 
      Model attributes.
    """
    for i in range(len(stringList)):
        modifiedName = repl.join(stringList[i].split())
        stringList[i] = modifiedName
