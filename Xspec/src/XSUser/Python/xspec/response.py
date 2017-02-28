from __future__ import print_function
from __future__ import absolute_import

from . import _pyXspec
from .xset import _AttrRestrictor, Xset
from .parameter import Parameter, _RespParam

class _DetArrayEmulator(object):
    """For internal use only

    This defines __get/setitem__ functions to give Spectrum.response property
    the look and feel of an array.

    """
    def __init__(self, parent):
        self._parent = parent
    def __getitem__(self, sourceNum):
        if not isinstance(sourceNum, int) or sourceNum < 0:
            err="Error: Multiresponse array index must be an int for source number"
            raise Exception(err)
        resp = _pyXspec.getResponse(self._parent, sourceNum)
        if not resp:
            err="Error: No response is assigned to source " + str(sourceNum+1)
            err+=" for this spectrum"
            raise Exception(err)
        return Response(self._parent,resp)           
    def __setitem__(self, sourceNum, fname):
        if not isinstance(sourceNum, int) or sourceNum < 0:
            err="Error: Multiresponse array index must be an int for source number"
            raise Exception(err)
        if fname is not None and not isinstance(fname, str):
            err="Error: Multiresponse setting must be a filename string or None"
            raise Exception(err)
        fileName= None
        if fname and not fname.isspace():
            fileName = fname
        _pyXspec.setResponse(self._parent, sourceNum, fileName)
        
class Response(_AttrRestrictor):
    """Detector response class.
    
    **Methods**
    
      - setPars
      - show
        
    **Attributes** (get-only unless stated otherwise)
           
       - arf
               Get/Set the arf filename string.
               Enter ``None`` or empty string to remove an existing arf.
    
       - chanEnergies 
               Tuple of floats, the detector channel energies in keV.
               These are the energies normally stored in the 
               EBOUNDS extension.
       
       - energies
               Tuple of floats, the photon energies in keV.
               These are the energies normally stored in the MATRIX extension.
       
       - gain  
               A response model object (class RModel) for applying
               a shift in response file gain.
                          
               (Also see *Response.setPars()* for setting multiple
               gain parameters at a time.)

               When gain is turned ON, it creates two variable
               fit Parameter object members:
                                         
                  >>> gain.slope   # (default = 1.0)
                  >>> gain.offset  # (default = 0.0)

               To turn gain ON simply assign a value to EITHER
               parameter, ie.:  
                                                      
                  >>> gain.slope = 1.05

               This automatically also creates a *gain.offset* 
               parameter with default value 0.0, which you may 
               want to re-adjust. Examples:
               
                  >>> gain.offset = .02
                  >>> gain.offset.values = ".015,.001,,,,0.1"

               *slope* and *offset* are of the same type as regular
               model parameters, and therefore have the same
               functions, attributes, and syntax rules for setting
               values.  (See the Parameter class help for more 
               details.)

               To turn gain OFF, call its *off()* method:
               
                  >>> gain.off()

               *gain.off()* restores the response to its original state,
               and renders the *slope* and *offset* parameters 
               inaccessible.
                                                          
       - rmf   
               The response file name string.
       
       - sourceNumber 
               The 1-based source number for which the response is 
               assigned. This is normally always 1 unless multiple sources
               are loaded for multiple-model evaluation.                 
    
    """
    def __init__(self, parent, respTuple):
        """Construct a Response object.
        
        Intended for creation by a Spectrum object only.
        
        """
        self._turnRestrictOff() 
        # These are invariants:
        self.__parent = parent         
        self.__fileName = respTuple[0]
        self.__chanEnergies = respTuple[1]
        self.__energies = respTuple[2]
        self.__handle = respTuple[3]
        self.__sourceNum = respTuple[4]
        
        # Gain - a built in response function
        respParNames = ["slope","offset"]
        self.gain = RModel(self,respParNames, "gain")

        self._resetRestrict()
    
    def setPars(self, *seqPars):
        """Set multiple response parameters with a single function call.
        
        Similar to the *Model.setPars()* function, this allows multiple 
        response parameters to be changed with just a SINGLE recalculation
        performed at the end.
        
        Args:
          *seqPars*:  
                      An arbitrary number of CONSECUTIVE parameter values
                      to be matched 1-to-1 with the response model's
                      parameters.
                      
        Currently just 1 response model is available (*gain*), which has
        2 response parameters (*slope* and *offset*).
        
        Examples::
        
           s = Spectrum("file1")
           resp = s.response
           
           # 'gain' is off by default and response parameters don't yet exist.
           #   The following call automatically turns 'gain' on and creates
           #   both 'slope' and 'offset' parameters even though it is only
           #   assigning to 'slope'.  'offset' will retain its default value
           #   of 0.0.
           resp.setPars(1.05)  # Equivalent to doing: resp.gain.slope = 1.05
           
           # This is equivalent to: resp.gain.slope = .995
           #                        resp.gain.offset = .08
           #   except that the recalculation is only performed at the end
           #   rather than after each parameter is changed:
           resp.setPars(.995, .08)
           
           # Can also assign auxiliary values by passing 1 or 2 string
           #   arguments.
           resp.setPars("1.1,,.02,.02,1.8,1.8","-.05,,-2,-2")
           
           # Remove gain and restore response to original state:
           resp.gain.off()
                                 
        """
        # Must assume the only RModel is 'gain', with its 2 parameters.
        nPar = 2
        valsList=[]
        idxList=[]
        for i in range(len(seqPars)):
            if i >= nPar:
                warn = ("***Warning: 'gain' only has 2 parameters."
                   + "  Extra arguments are ignored.\n")
                print(warn)
                if Xset.log is not None:
                    print(warn,file=Xset.log)
                break
            arg = seqPars[i]
            if (isinstance(arg,float) or isinstance(arg,int)):
                valsList.append(str(arg))
                idxList.append(i+1)
            elif (isinstance(arg,str)):
                valsList.append(arg)
                idxList.append(i+1)
            else:
                err="Error: Argument must be a number or a string type"
                raise Exception(err)
                
        self.gain._RModel__turnRModelOn()
        _pyXspec.setRespPars(self.__handle, tuple(idxList), tuple(valsList))    
        
    def show(self):
        """Display response information including (optional) response parameters."""
        _pyXspec.showResponse(self.__handle)
    
    def _getArf(self):
        return _pyXspec.getArf(self.__handle)
    def _setArf(self, fileName):
        if fileName is None or isinstance(fileName, str):
           if not fileName or fileName.isspace():
               _pyXspec.setArf(self.__handle,None)
           else:
               _pyXspec.setArf(self.__handle, fileName)
        else:
            errMsg = "Error: Filename string is required to set Arf"
            raise Exception(errMsg)
    arf = property(_getArf, _setArf)
        
    def _getChanEnergies(self): return self.__chanEnergies
    def _setChanEnergies(self, value):
        raise Exception("Error: Cannot rebind a Response object's chanEnergies")
    chanEnergies = property(_getChanEnergies, _setChanEnergies)
    
    def _getEnergies(self): return self.__energies
    def _setEnergies(self, value):
        raise Exception("Error: Cannot rebind a Response object's energies")
    energies = property(_getEnergies, _setEnergies)
    
    def _getRmf(self): return self.__fileName
    def _setRmf(self, value):
        raise Exception("Error: Cannot rebind a Response object's rmf fileName")
    rmf = property(_getRmf, _setRmf)

    def _getSourceNumber(self): return self.__sourceNum
    def _setSourceNumber(self, value):
        raise Exception("Error: Cannot rebind a Response object's sourceNumber")
    sourceNumber = property(_getSourceNumber, _setSourceNumber)


class RModel(_AttrRestrictor):
    """**Response Model class.**
    
    Response models are functions which act upon the detector RMF.   XSPEC
    currently has just one response model: *gain*, which is a built-in attribute
    of the Response class.  RModel objects are not intended for stand-alone
    creation: its __init__ function should be considered private.
    
    **Attributes**
    
       - <parameters>
                 When RModel is ON, it contains an attribute of type
                 Parameter for every parameter in the model.  An
                 RModel is turned ON by a 'set' operation on ANY
                 of its parameters.  For example with the *gain*
                 RModel:
                 
                 >>> resp.gain.offset = .03
                           
                 automatically creates *offset* AND *slope* 
                 parameters if they don't already exist (*slope* 
                 would be initialized to its default value of 1.0). 
                 The shift is then applied immediately to the 
                 Response object *resp*.

                 When RModel is OFF (see the *RModel.off()* method),
                 the parameters are not accessible.  
       
       - isOn   
                 Boolean flag showing the On/Off status of the RModel
                 (get only).
                           
       - parameterNames 
                 List of the response model's parameter names
                 (get only).
                               
    
    """
    def __init__(self, resp, parNames, rmodName):
        """RModel constructor.
        
           Intended for internal use only.
        """
        self._turnRestrictOff()
        # These are invariants
        self.__pyParent = resp
        self.__name = rmodName
        parIdx=1
        for parName in parNames:
            newPar = Parameter(parName, _RespParam())
            newPar._Parameter__index = parIdx
            newPar._Parameter__parent = self.__pyParent._Response__handle
            super(RModel,self).__setattr__(parName, newPar)
            parIdx += 1
        self.parameterNames = tuple(parNames)
        self._resetRestrict()
                    
    def __getattribute__(self, name):
        # Must not call hasattr to determine if 'parameterNames'
        #  exists.  That would send things into an infinite recursive loop
        #  since hasattr indirectly relies on __getattribute__.
        try:
            super(RModel,self).__getattribute__('parameterNames')
        except:
            return super(RModel,self).__getattribute__(name)
            
        if name in super(RModel,self).__getattribute__('parameterNames'):
            # Must not directly access self.isOn in here.  Again,
            #   that will call __getattribute__ leading to another
            #   infinite loop
            if super(RModel,self).__getattribute__('isOn'):
                return super(RModel,self).__getattribute__(name)
            else:
                err="Error: Gain is currently off for this response"
                raise Exception(err)
        else:
           return super(RModel,self).__getattribute__(name)
                                
    def __setattr__(self, attrName, value):
        if hasattr(self, 'parameterNames'):
            if attrName in self.parameterNames:
                self.__turnRModelOn()
                # See comments in Component.__setattr__ for why we
                # first check for case of Parameter type.
                if not isinstance(value, Parameter):
                    # Treat as if setting parameter's val.
                    par = getattr(self, attrName)
                    par.values = value
            elif attrName == 'parameterNames':
                raise Exception("Error: Cannot rebind RModel's parameterNames")
            elif attrName == 'isOn':
                raise Exception("Error: Cannot rebind RModel's isOn flag")
            else:
                super(RModel,self).__setattr__(attrName, value)
        else:
            super(RModel,self).__setattr__(attrName, value)
    
    def __turnRModelOn(self):
        """Activate the RModel by creating its parameter attributes.

        This should be called any time a parameter 'set' operation is
        invoked.  If model is not already on, this will call the
        underlying Xspec rmodel function.  If already on, do nothing.

        For private use only.
        """
        if not self.isOn:
            rmodArgs=["rmodel"]
            specHandle = self.__pyParent._Response__parent
            sourceNum = self.__pyParent.sourceNumber
            specNum = _pyXspec.getIndexFromHandle(specHandle)
            rmodArgs.append(str(sourceNum) + ':' + str(specNum))
            rmodArgs.append(self.__name)
            for parName in self.parameterNames:
                rmodArgs.append("&")            
            _pyXspec.doXspecCmd(rmodArgs)
            
    def off(self):
        """Remove response parameters and turn the model OFF.
        
        The Response is restored to its original state.
        
        """
        if self.isOn:
            rmodArgs = ["rmodel"]
            specHandle = self.__pyParent._Response__parent
            sourceNum = self.__pyParent.sourceNumber
            specNum = _pyXspec.getIndexFromHandle(specHandle)
            rmodArgs.append(str(sourceNum) + ':' + str(specNum))
            rmodArgs.append("none")
            _pyXspec.doXspecCmd(rmodArgs)

    # A property set function here would be hidden by __setattr__.
    def _getIsOn(self):
        return _pyXspec.hasGainPars(self.__pyParent._Response__handle)
    isOn = property(_getIsOn, doc="On/Off indicator for RModel object.")
