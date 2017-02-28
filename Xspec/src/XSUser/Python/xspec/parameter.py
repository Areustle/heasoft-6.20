from __future__ import print_function
from __future__ import absolute_import

from . import _pyXspec
from .xset import _AttrRestrictor

class Parameter(_AttrRestrictor):
    """**Model or response parameter class.**
    
       **Methods**
       
         - untie
       
       **Attributes**  ((*) = get-only)
       
       .. hlist::
          :columns: 2 
    
          - error*   
          - frozen  
          - index*    
          - link    
          - name*    
          - prior     
          - sigma*   
          - unit*    
          - values  
       
    """
    def __init__(self, parName, parStrategy):
        """Parameter constructor.
        
        Not intended for stand-alone creation.  This should only be called
        from within Model, Component, or Response classes.
        
        parName -- Parameter name
        
        """
        self._turnRestrictOff()
        self.__name = parName
        self.__strategy = parStrategy
        if isinstance(parStrategy, _ModParam):
            self.__iType = 0
        else:
            self.__iType = 1 
        # The following 2 attributes are needed to identify this object
        #  within Xspec.  These are private, but the Model object will
        #  still reach in and set these during its initialization.
        self.__index = 0
        self.__parent = 0
        self._resetRestrict()
    
    def _getError(self):
        paramTuple = _pyXspec.getParTuple(self.__parent, self.__index, 
                        self.__iType)
        errInfo = (paramTuple[6], paramTuple[7], paramTuple[8])
        return errInfo
    def _setError(self, value):
        raise Exception("Error: Cannot rebind a Parameter object's error attribute") 
    error = property(_getError, _setError, 
              doc="""A tuple containing the results of the most recent fit *error*
                     command performed on the parameter (GET only).
                     The tuple values are: (<error low bound>, <error high bound>,
                     <error status code string>)""") 
        
    def _getIndex(self): return self.__index
    def _setIndex(self, value):
        raise Exception("Error: Cannot rebind a Parameter object's index")
    index = property(_getIndex, _setIndex,
               doc="""Position of the parameter within the Model object.
               
                      (The first parameter has *index* = 1)  Note that this is the 
                      same value that would be used to obtain a Parameter object
                      from its Model, ie: par = mod(<index>)  (GET only).""")
                        
    def _getName(self): return self.__name
    def _setName(self, value):
        if hasattr(self, '_Parameter__name'):
            raise Exception("Error: Cannot rebind a Parameter object's name")
        else:
            self.__name = value
    name = property(_getName, _setName,
                        doc="Name of Parameter (GET only).")
    
    def _getPrior(self):
        paramTuple = _pyXspec.getParTuple(self.__parent, self.__index,
                        self.__iType)
        priorList = [paramTuple[9]] + paramTuple[10]
        return tuple(priorList)
    def _setPrior(self, vals):
        hyperPars = []
        if isinstance(vals, str):
            priorType = vals
        elif isinstance(vals, tuple) and len(vals):
            priorType = vals[0]
            if not isinstance(priorType, str):
                raise Exception("Error: priorType must be a string")
            for i in range(1, len(vals)):
                hyperPars.append(str(vals[i]))
        else:
            err="Error: prior must be set to one of the following:\n\
        <priorType>  # string\n\
        (<priorType>,[<hyper 1>],[<hyper 2>]...)  # tuple of string and floats"
            raise Exception(err)
                
        self.__strategy.setBayes(self.__parent, self.__index, priorType,
                        hyperPars)   
    prior = property(_getPrior, _setPrior,
              doc="""A tuple containing the settings for the prior used when
                     Bayesian inference is turned on.

                     Get: Returns a tuple containing:
                       (<priorType>, <optional hyperparameters>)

                     Set with:
                        string:   <priorType>
                        
                        or tuple: (<priorType>, <optional hyperparameters>)
                        
                        Valid priorTypes are 'cons', 'exp', 'jeffreys', 'gauss'.
                        Hyperparameters should be entered as floats.""")
    
    def _getSigma(self):
        paramTuple = _pyXspec.getParTuple(self.__parent, self.__index,
                        self.__iType)
        return paramTuple[1]
    def _setSigma(self, value):
        raise Exception("Error: Cannot rebind a Parameter object's sigma") 
    sigma = property(_getSigma, _setSigma,
              doc="The Parameter fit sigma (-1.0 when not applicable) (GET only).")
    
    def _getValues(self):
        paramTuple = _pyXspec.getParTuple(self.__parent, self.__index,
                        self.__iType)
        return paramTuple[0]
    def _setValues(self, value):
        if not isinstance(value, str):
            value = str(value)
            # If value was originally a tuple or list, remove
            # the enclosing brackets.
            value = value.lstrip('([')
            value = value.rstrip(')]')
        self.__strategy.setPar(self.__parent,self.__index,value)
    values = property(_getValues, _setValues,
              doc="""List of value floats [val,delta,min,bot,top,max].
              
                     This may be set with: 
                       string:        x.values = "3.2,,,,1e2, 1e3"
                       
                       single float:  x.values = 4.1  (sets 'val' only)
                       
                       tuple:         x.values = 8.2,.02, -10.
                       
                       list:          x.values = [8.2,.02, -10.]
                       
                     Note that Tuple and List input do not allow the use
                     of consecutive commas for argument spacing.""")
    
    def _getFrozen(self):
        return _pyXspec.getParTuple(self.__parent, self.__index,
                        self.__iType)[2]
    def _setFrozen(self, value):
        if bool(value):
            self.__strategy.setFreeze(self.__parent,self.__index,1)
        else:
            self.__strategy.setFreeze(self.__parent,self.__index,0)
    frozen = property(_getFrozen, _setFrozen,
                        doc="Bool, if True then parameter is frozen.")
    
    def _getUnit(self):
        paramTuple = _pyXspec.getParTuple(self.__parent, self.__index,
                        self.__iType)
        return paramTuple[4]
    def _setUnit(self, value):
        raise Exception("Error: Cannot rebind a Parameter object's unit string")
    unit = property(_getUnit, _setUnit,
             doc="An optional string for the parameter's units (GET only).")
    
    def _getLink(self):
        paramTuple = _pyXspec.getParTuple(self.__parent, self.__index,
                        self.__iType)
        return paramTuple[3]
    def _setLink(self,value):
        # Newpar handler expects link strings to begin with a '='.
        # If user enters an empty (or blank) string, pass an empty
        # string to Xspec indicating that link should be removed.
        if len(value)==0 or value.isspace():
            value = ""
        else: 
            value = '=' + value
        self.__strategy.setLink(self.__parent,self.__index,value)
    link = property(_getLink, _setLink, 
                doc="Parameter link expression string (empty if not linked).")
    
    def untie(self):
        """Remove parameter link (if any)"""
        self.link = ""
    
    def __float__(self):
        return self.values[0]
        
    def __add__(self, other):
        if type(other) == Parameter:
            return self.values[0] + other.values[0]
        else:
            return self.values[0] + other
    def __radd__(self, other):
        return self.__add__(other)
    def __iadd__(self, other):
        self.values = self.__add__(other)
        return self
                
    def __mul__(self, other):
        if type(other) == Parameter:
            return self.values[0]*other.values[0]
        else:
            return self.values[0]*other
    def __rmul__(self, other):
        return self.__mul__(other)
    def __imul__(self, other):
        self.values = self.__mul__(other)
        return self


class _ModParam(object):
    """Strategy class for model parameters
    
       For internal use only
       
    """
    def setPar(self, modHandle, parIndex, valueStr):
        _pyXspec.setPars(modHandle,(parIndex,),(valueStr,),1)
    
    def setFreeze(self, modHandle, parIndex, isFreeze):
        _pyXspec.setParFreeze(modHandle, parIndex, isFreeze, 1)
    
    def setLink(self, modHandle, parIndex, valueStr):
        _pyXspec.setParLink(modHandle, parIndex, valueStr, 1)

    def setBayes(self, modHandle, parIndex, priorType, hyperPars):
        _pyXspec.setParBayes(modHandle, parIndex, priorType, hyperPars, 1)

            
class _RespParam(object):
    """Strategy class for response parameters
    
       For internal use only
    
    """
    def setPar(self, respHandle, parIndex, valueStr):
        _pyXspec.setRespPars(respHandle, (parIndex,),(valueStr,))
    
    def setFreeze(self, respHandle, parIndex, isFreeze):
        _pyXspec.setParFreeze(respHandle, parIndex, isFreeze, 0)

    def setLink(self, respHandle, parIndex, valueStr):
        _pyXspec.setParLink(respHandle, parIndex, valueStr, 0)

    def setBayes(self, respHandle, parIndex, priorType, hyperPars):
        _pyXspec.setParBayes(respHandle, parIndex, priorType, hyperPars, 0)
