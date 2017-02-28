from __future__ import print_function
from __future__ import absolute_import

from . import _pyXspec
from .xset import _AttrRestrictor

class ChainManager(_AttrRestrictor):
    """**Monte Carlo Markov Chain container.**
    
    PyXspec automatically creates a single object of this class,
    named *AllChains*.
    
       **Methods**

       - __call__ (the '()' operator)
       - __iadd__ (the '+=' operator)
       - __isub__ (the '-=' operator)
       - clear
       - show
       - stat

       **Attributes**

       These are the values which will be used when creating new Chain objects,
       unless they are explicitly overridden as arguments to the Chain class
       constructor.  For more detail, see the descriptions for the
       corresponding attributes in the Chain class doc.

       - defAlgorithm 
       - defBurn       
       - defFileType     
       - defLength     
       - defProposal    
       - defRand         
       - defTemperature  
       - defWalkers   
    
    """
    __single = None
    def __init__(self):
        if ChainManager.__single:
            raise Exception("Error: Only 1 instance of ChainManager is allowed.")
        ChainManager.__single = self
        self._turnRestrictOff()
        self.__defAlgorithm = "gw"
        self.__defBurn = 0
        self.__defFileType = "fits"
        self.__defLength = 100
        self.__defProposal = "gaussian fit"
        self.__defRand = False
        self.__defTemperature = 1.0
        self.__defWalkers = 10
        self._resetRestrict()

    def __call__(self, index):
        """Get a Chain object from the *AllChains* container.
        
        Args:
           *index*:  The index of a currently loaded chain file.  The list
                     of currently loaded chains can be seen with the
                     AllChains.show() method.  The valid range is:
                     1 <= *index* <= nLoadedChains.
                      
        Note that the returned Chain object's modifiable attributes will
        be initialized with the current AllChains *def<attribute>* settings::
         
            # Example: Load 2 chains from pre-existing files:
            AllChains += "chain1.fits"
            AllChains += "chain2.fits"
            # and use the __call__ method to retrieve a Chain object 
            #   for the 2nd chain:
            c2 = AllChains(2)
                         
        """
        if not isinstance(index, int) or index < 1:
            raise Exception("Error: Argument must be an integer > 0")
        # This will throw if index is out of range of loaded chains.
        return Chain(index, "__internal_ctor_only") 
        
    def __iadd__(self, chain):
        """Load a pre-existing chain into the *AllChains* container.
        
        Args:
           *chain*:  may be a currently existing chain object which had been
                     unloaded earlier:
                     
                     >>> AllChains += myChain1
                      
                     the filename of an existing chain file:
                     
                     >>> AllChains += "chainFile.fits"
                     
                     or the filename of a new chain:
                     
                     >>> AllChains += "newChainFile.fits"
                     
                     The last example will also perform a chain
                     run using the default settings.
                        
        """
        if isinstance(chain, Chain):
            chainArgs=["chain","load"]
            chainArgs.append(chain.fileName)
            _pyXspec.doXspecCmd(chainArgs)
        elif isinstance(chain, str):
            # Must determine if file already exists.  Just do a simple
            # read test.
            alreadyExists = True
            try:
                testFile = open(chain, 'r')
                testFile.close()
            except Exception:
                alreadyExists = False
            
            if alreadyExists:
                chainArgs=["chain","load"]
                chainArgs.append(chain)
                _pyXspec.doXspecCmd(chainArgs)
            else:
                Chain(chain)
            
        else:
            raise Exception("Error: Invalid type for += operator")
        return self
        
    def __isub__(self, chain):
        """Unload one or more chain objects from container.
       
        Args:
           *chain*:  may either be a chain object:
           
                     >>> AllChains -= myChain1
                       
                     a filename:
                     
                     >>> AllChains -= "chainFile.fits"
                                
                     the chain's current index [int] in the *AllChains* container:
                     
                     >>> AllChains -= 2
                                
                     or a '*' to unload ALL chains (equivalent to AllChains.clear()):
                     
                     >>> AllChains -= '*'
        
        """
        if isinstance(chain, Chain) or isinstance(chain, str):
            # Unload by fileName (unless str = '*')
            if isinstance (chain, Chain):
                fileName = chain.fileName
            else:
                fileName = chain
            if len(fileName) and fileName[0] == '*':
                chainArgs = ["chain","clear"]
                _pyXspec.doXspecCmd(chainArgs)
            else:
                result = _pyXspec.removeChainByName(fileName)
                if result == -1:
                    msg = "***Error: Chain with fileName: " + fileName
                    msg += " is not currently loaded."
                    print(msg)                        
        elif isinstance(chain, int):
            chainArgs = ["chain","unload"]
            chainArgs.append(str(chain))
            _pyXspec.doXspecCmd(chainArgs)
        else:
            raise Exception("Error: Invalid type for -= operator")
        return self    
    
    def clear(self):
        """Unload all chains from container"""
        chainArgs = ["chain","clear"]
        _pyXspec.doXspecCmd(chainArgs)
        
    def show(self):
        """ Display information for current attributes and loaded chains.
        """
        _pyXspec.showChainContainer(self)
        
    def stat(self, parIdx):
        """Display statistical information on a particular chain parameter.
        
           Args:
              *parIdx*:  The parameter index number, including optional model
                         name: [<modName>:]<idx>.  May be entered as a string
                         or int (if no model name).            
             
        """
        chainArgs = ["chain","stat"]
        if isinstance(parIdx, int):
            chainArgs.append(str(parIdx))
        elif isinstance(parIdx, str):
            chainArgs.append(parIdx)
        else:
            raise Exception("Error: Argument must be an int or a string")
        _pyXspec.doXspecCmd(chainArgs)

    def _getDefBurn(self):  return self.__defBurn
    def _setDefBurn(self, value):
        Chain._Chain__validateBurn(value)
        self.__defBurn = value
    defBurn = property(_getDefBurn, _setDefBurn,
                doc="Default burn length for new Chain objects (orig = 0).")
    
    def _getDefFileType(self):  return self.__defFileType
    def _setDefFileType(self, value):
        self.__defFileType = Chain._Chain__validateFileType(value)
    defFileType = property(_getDefFileType, _setDefFileType,
                        doc="Default output file format (orig = 'fits').") 
                        
    def _getDefLength(self):  return self.__defLength
    def _setDefLength(self, value):
        Chain._Chain__validateLength(value)
        self.__defLength = value
    defLength = property(_getDefLength, _setDefLength, 
                        doc="Default chain length (orig = 100).") 

    def _getDefProposal(self):  return self.__defProposal
    def _setDefProposal(self, value):
        Chain._Chain__validateProposal(value)
        self.__defProposal = value
    defProposal = property(_getDefProposal, _setDefProposal,
                 doc="Default chain proposal (orig = 'gaussian fit').")
                        
    def _getDefRand(self): return self.__defRand
    def _setDefRand(self, value):
        Chain._Chain__validateRand(value)
        self.__defRand = value
    defRand = property(_getDefRand, _setDefRand, 
                doc="Default randomization setting (orig = False).")
    
    def _getDefTemperature(self): return self.__defTemperature
    def _setDefTemperature(self, value):
        Chain._Chain__validateTemperature(value)
        self.__defTemperature = value
    defTemperature = property(_getDefTemperature, _setDefTemperature,
                        doc="Default chain temperature (orig = 1.0).")

    def _getDefAlgorithm(self): return self.__defAlgorithm
    def _setDefAlgorithm(self, value):
        Chain._Chain__validateAlgorithm(value)
        self.__defAlgorithm = value
    defAlgorithm = property(_getDefAlgorithm, _setDefAlgorithm,
                        doc="Default chain algorithm (orig = 'gw').")
                        
    def _getDefWalkers(self): return self.__defWalkers
    def _setDefWalkers(self, value):
        Chain._Chain__validateWalkers(value)
        self.__defWalkers = value
    defWalkers = property(_getDefWalkers, _setDefWalkers,
                        doc="Default walkers parameter for 'gw' chains (orig = 10).")
                        
# End class ChainManager
    
AllChains = ChainManager()

    
class Chain(_AttrRestrictor):
    """**Monte Carlo Markov Chain class.**
    
          **Methods**                    
          
          - __init__
          - run
          - show
          
          **Attributes**  ((*) = get-only)      

          .. hlist::
             :columns: 2
             
             - algorithm 
             - burn      
             - fileName* 
             - fileType*
             - proposal
             - rand
             - runLength
             - temperature
             - totalLength*
             - walkers
          
          
       The following attribute settings will apply to the NEXT run for this 
       chain: *algorithm*, *runLength*, *proposal*, *temperature*, 
       *burn*, *rand*, *walkers*.  
              
       The *algorithm*, *burn*, *rand*, and *walkers* settings are irrelevant 
       if run is performing an appending operation.
       
    """    
    def __init__(self, fileName, fileType=None, burn=None, runLength=None,
                   proposal=None, rand=None, temperature=None, algorithm=None,
                   walkers=None):
        """Construct a chain object, perform a run, and load into AllChains
              container.
        
        The only required argument is *fileName*.  All other arguments will
        take their default values from the current settings in the *AllChains* 
        container.
        
        """
        self._turnRestrictOff()
        
        if (isinstance(fileName, int) and isinstance(fileType,str) and
                fileType == "__internal_ctor_only"):
            # Construct object from an already loaded chain, and do NOT run.
            chainInfo = _pyXspec.getChainByIndex(fileName)
            self.__fileName = chainInfo[0]
            self.__fileType = chainInfo[1]
            self.__totalLength = chainInfo[2]
            self.algorithm = AllChains.defAlgorithm
            self.burn = AllChains.defBurn
            self.runLength = AllChains.defLength
            self.proposal = AllChains.defProposal
            self.rand = AllChains.defRand
            self.temperature = AllChains.defTemperature
            self.walkers = AllChains.defWalkers
            
        else:        
            self.__fileName = fileName.split()[0]
            self.__totalLength = 0

            # Conditional expressions don't exist prior to v2.5
            if burn is not None:
                self.burn = burn
            else:
                self.burn = AllChains.defBurn

            if fileType is not None:
                self.__fileType = Chain.__validateFileType(fileType)
            else:
                self.__fileType = AllChains.defFileType

            if runLength is not None:
                self.runLength = runLength
            else:
                self.runLength = AllChains.defLength

            if proposal is not None:
                self.proposal = proposal
            else:
                self.proposal = AllChains.defProposal

            if rand is not None:
                self.rand = rand
            else:
                self.rand = AllChains.defRand

            if temperature is not None:
                self.temperature = temperature
            else:
                self.temperature = AllChains.defTemperature

            if algorithm is not None:
                self.algorithm = algorithm
            else:
                self.algorithm = AllChains.defAlgorithm

            if walkers is not None:
                self.walkers = walkers
            else:
                self.walkers = AllChains.defWalkers

            self._resetRestrict()

            self.run(False)

    def run(self, append=True):
       """Perform a new chain run, either appending to or overwriting an
             existing chain.
       
       Args:
         *append*:  If this is set to True the new run will be appended.  
                    If False, the new run will overwrite.  Note that the *algorithm*,
                    *burn*, *rand*, and *walkers* settings do not apply when appending.
                        
       """
       savConsChatter = _pyXspec.getChatter(0)
       savLogChatter = _pyXspec.getChatter(1)
       _pyXspec.setChatter(0,1)
       _pyXspec.setChatter(1,1)
       try:
           fileTypeArgs = ["chain","filetype"]
           fileTypeArgs.append(self.fileType)
           _pyXspec.doXspecCmd(fileTypeArgs)
           lengthArgs = ["chain","length"]
           lengthArgs.append(str(self.runLength))
           _pyXspec.doXspecCmd(lengthArgs)
           propArgs = ["chain","proposal"]
           propArgs += self.__proposal.split()
           _pyXspec.doXspecCmd(propArgs)
           tempArgs = ["chain","temperature"]
           tempArgs.append(str(self.temperature))
           _pyXspec.doXspecCmd(tempArgs)
           if self.__totalLength and append:
               outFile = '>' + self.__fileName
           else:
               burnArgs = ["chain","burn"]
               burnArgs.append(str(self.burn))
               _pyXspec.doXspecCmd(burnArgs)
               randArgs = ["chain","rand"]
               if self.rand:
                   randArgs.append("on")
               else:
                   randArgs.append("off")
               _pyXspec.doXspecCmd(randArgs)
               algorithmArgs = ["chain","type"]
               algorithmArgs.append(self.algorithm)
               _pyXspec.doXspecCmd(algorithmArgs)
               walkerArgs = ["chain","walkers"]
               walkerArgs.append(str(self.walkers))
               _pyXspec.doXspecCmd(walkerArgs)
               outFile = self.__fileName
       except Exception:
           _pyXspec.setChatter(0,savConsChatter)
           _pyXspec.setChatter(1,savLogChatter)
           raise Exception
       _pyXspec.setChatter(0,savConsChatter)
       _pyXspec.setChatter(1,savLogChatter)
       runArgs = ["chain","run"]
       runArgs.append(outFile)
       _pyXspec.doXspecCmd(runArgs)
       self.__totalLength += self.__runLength
    
    def show(self):
        """ Display current settings of Chain object's attributes.
        """
        _pyXspec.showChain(self)

    
    def _getBurn(self):  return self.__burn
    def _setBurn(self, value):
        Chain.__validateBurn(value)
        self.__burn = value
    burn = property(_getBurn, _setBurn,doc="""The number of steps that will be thrown 
                                           away prior to storing the chain [int].""")
        
    def _getFileName(self): return self.__fileName
    def _setFileName(self, value):
        raise Exception("Error: Cannot rebind a Chain object's file name")
    fileName = property(_getFileName, _setFileName,doc="Chain output file name.")

    def _getFileType(self): return self.__fileType
    def _setFileType(self, value):
        raise Exception("Error: Cannot rebind a Chain object's file type")
    fileType = property(_getFileType, _setFileType,
                        doc="""Output format of the chain file [string].                        
                            Will be either 'fits' (the default), or 'ascii'.""")

    def _getRunLength(self): return self.__runLength
    def _setRunLength(self, value):
        Chain.__validateLength(value)
        self.__runLength = value
    runLength = property(_getRunLength, _setRunLength, 
                doc="The length of chain to be added during the next run [int].")
    
    def _getTotalLength(self): return self.__totalLength
    def _setTotalLength(self, value):
        raise Exception("Error: Cannot rebind a Chain object's total length value.")
    totalLength = property(_getTotalLength, _setTotalLength, 
                doc="""The cumulative length of the chain [int].
                       This will increase every time a run is performed.""")
    
    def _getProposal(self): return self.__proposal
    def _setProposal(self, value):
        Chain.__validateProposal(value)
        self.__proposal = value
    proposal = property(_getProposal, _setProposal,
                doc="""The proposal distribution and source of covariance
                         information to be used for the next run [string].
                         
                         Examples: 'gaussian fit', 'cauchy fit','gaussian chain', etc.
                                   
                         See the 'chain' command in the standard XSPEC manual
                         for more information.""")

    def _getRand(self): return self.__rand
    def _setRand(self, value):
        Chain.__validateRand(value)
        self.__rand = value
    rand = property(_getRand, _setRand, doc="""Determines whether chain start point will be randomized
                         (True) or taken from the current parameters (False). """)
    
    def _getTemperature(self): return self.__temperature
    def _setTemperature(self, value):
        Chain.__validateTemperature(value)
        self.__temperature = value
    temperature = property(_getTemperature, _setTemperature, 
                        doc="""The temperature parameter used in the Metropolis-Hastings
                         algorithm for the proposal acceptance or rejection
                         [float]. """)

    def _getAlgorithm(self): return self.__algorithm
    def _setAlgorithm(self, value):
        Chain.__validateAlgorithm(value)
        self.__algorithm = value
    algorithm = property(_getAlgorithm, _setAlgorithm,
                   doc="""The current chain algorithm.  Valid settings are 'gw'
                         (Goodman-Weare) or 'mh' (Metropolis-Hastings).""")
    
    def _getWalkers(self): return self.__walkers
    def _setWalkers(self, value):
        Chain.__validateWalkers(value)
        self.__walkers = value
    walkers = property(_getWalkers, _setWalkers, 
               doc="The number of walkers to be used for 'gw' chains [int].")
    
    # Chain attribute validation functions. These are performed here rather
    # than in property set functions so that ChainManager can also use them.
    def __validateBurn(val):
        """Check burn for int"""
        if not isinstance(val, int):
            raise Exception("Error: burn argument must be an int")
    __validateBurn = staticmethod(__validateBurn)

    def __validateFileType(val):
        """Check for fits or ascii"""
        isOK = False
        if isinstance(val, str):
            val = val.strip()
            val = val.lower()
            if val == "fits" or val == "ascii":
                isOK = True
        if not isOK:
            raise Exception("Error: fileType must be \"fits\" or \"ascii\"")
        return val
    __validateFileType = staticmethod(__validateFileType)

    def __validateLength(val):
        """Check length for int"""
        if not isinstance(val, int):
            raise Exception("Error: length argument must be an int")
    __validateLength = staticmethod(__validateLength)

    def __validateProposal(val):
        """Check for string, whitespace is allowed"""
        if not isinstance(val, str):
            raise Exception("Error: proposal argument must be a string")
    __validateProposal = staticmethod(__validateProposal)
    
    def __validateRand(val):
        """Check for bool"""
        if not isinstance(val, bool):
            raise Exception("Error: rand argument must be a bool")
    __validateRand = staticmethod(__validateRand)
    
    def __validateTemperature(val):
        """Check for float (also allow int)"""
        if not isinstance(val, float) and not isinstance(val, int):
            raise Exception("Error: temperature argument must be a float or int")
    __validateTemperature = staticmethod(__validateTemperature)
    
    def __validateAlgorithm(val):
        """Check for type of algorithm (gw|mh)"""
        isOK = False
        if isinstance(val,str):
            val = val.strip()
            val = val.lower()
            if val == "gw" or val == "mh":
                isOK = True
        if not isOK:
            raise Exception("Error: algorithm must be \"gw\" or \"mh\"")
        return val
    __validateAlgorithm = staticmethod(__validateAlgorithm)
    
    def __validateWalkers(val):
        """Check for even int > 0"""
        if not isinstance(val, int):
            raise Exception("Error: walkers value must be an even integer")
        if val <= 0:
            raise Exception("Error: walkers value must be > 0")
        if val % 2:
            raise Exception("Error: walkers value must be an even integer")
        return val
    __validateWalkers = staticmethod(__validateWalkers)
