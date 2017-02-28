from __future__ import print_function
from __future__ import absolute_import

from . import _pyXspec
from .spectrum import Spectrum
from .xset import _AttrRestrictor

class FakeitSettings(_AttrRestrictor):
    """**Fakeit command settings class.**
    
    The *AllData.fakeit* function will apply 1 FakeitSettings object to every
    fake spectrum that is to be created.  If the user does not explicitly
    supply their own FakeitSettings objects, *AllData.fakeit* will create its
    own as necessary, with default settings.
    
       **Methods**
       
       - __init__
       
       **Attributes**
    
       - response   
                     Name of detector response file to use for creating the
                     fake spectrum.
       
                     When a fake spectrum is based on a pre-existing spectrum
                     which already has a response, this should be left empty.
                     If a name is given it will be IGNORED.  However if the
                     pre-existing spectrum has no response, then this MUST be
                     filled.
                     
                     If the fake spectrum is not based on an existing spectrum,
                     this may be filled or left empty.  If it is empty,
                     XSPEC will just use its built-in dummy response.
                     
       - arf         
                     Name of optional arf to use with the response.  This is
                     ignored if no response is given.
                     
       - background  
                     Name of optional background file to use when creating the
                     fake spectrum.
                     
                     If based on an original spectrum, leave this empty to use
                     the original spectrum's background settings.
                     
       - exposure   
                     The fake spectrum exposure time.
       
       - correction  
                     Optional correction norm factor.
       
       - backExposure 
                     Optional background exposure time modifier.
       
                     For *exposure* and *correction*, if left empty fakeit will use
                     the values from the original spectrum, or 1.0 if not
                     based on an original spectrum.  Each of these may be
                     entered as a string or float.
                     
       - fileName   
                     Optional fake spectrum output file name.
       
                     If left empty, fakeit will create a default file name
                     based on the original spectrum, or the response name
                     if no original spectrum.  In the latter case, the
                     default names will also have an incremented suffix to
                     prevent file overwriting.
    
      When writing to a multiple-spectrum output file (OGIP type-2), *exposure*,
      *correction*, *backExposure*, and *fileName* are applied to the entire file
      rather than a single spectrum.  Therefore entries for these attributes
      will be IGNORED for all but the first fake spectrum in a type-2 output
      file.       
    
    """
    def __init__(self,response='',arf='',background='',exposure='',
                correction='',backExposure='',fileName=''):
        """Create a FakeitSettings object.
        
        All arguments are optional, and all may be entered as strings.
        The *exposure* and *correction* arguments may also be entered as floats.
        
        This can also create a new copy of a pre-existing FakeitSettings
        object, in which case the pre-existing object should be the only
        argument entered.
        
        Examples::
        
           fs1 = FakeitSettings("resp1.pha", exposure=1500.0)
           # Reuse fs1's settings, but with a new fileName attribute:
           fs2 = FakeitSettings(fs1)
           fs2.fileName = "fakeit2.pha"
           # Now generate 2 fake spectra
           AllData.fakeit(2, [fs1, fs2])
           
        """
        self._turnRestrictOff()
        if isinstance(response, FakeitSettings):
            # Copy constructor.
            self.response = response.response
            self.arf = response.arf
            self.background = response.background
            self.exposure = response.exposure
            self.correction = response.correction
            self.backExposure = response.backExposure
            self.fileName = response.fileName
        else:
            self.response = response
            self.arf = arf
            self.background = background
            # exposure/correction can be entered as floats or strings
            self.exposure = str(exposure)
            self.correction = str(correction)
            self.backExposure = str(backExposure)
            self.fileName = fileName                        
        self._resetRestrict()
        
    def _stripAll(self):
        """Strip leading and trailing whitespace from all string attrs
        
        Private use: This is to allow underlying C++ parsing functions to 
        assume no blank strings of non-zero length.
        
        """
        self.response = self.response.strip()
        self.arf = self.arf.strip()
        self.background = self.background.strip()
        self.exposure = self.exposure.strip()
        self.correction = self.correction.strip()
        self.backExposure = self.backExposure.strip()
        self.fileName = self.fileName.strip()
            
    def _preventNone(self):
        """Disallow any usage of the fakeit 'none' option.
        
        For reasons of interface consistency and to prevent the need for
        prompting, PyXspec does not allow the equivalent of standard XSPEC
        fakeit's 'none' option.  For pre-existing spectra the auxiliary file 
        names may be changed but not removed, and when based on non-existing 
        spectra, empty quotes signify 'none'.
        
        This assumes attributes have already been filtered through _stripAll.
        
        """
        usesNone=False
        for i in range(1):
           if self.response.lower() == "none":
               usesNone=True
               break
           if self.arf.lower() == "none":
               usesNone=True
               break
           if self.background.lower() == "none":
               usesNone=True
               break
           if self.fileName.lower() == "none":
               usesNone=True
               break
        if usesNone is True:
            errMsg="Error: \"none\" is not a valid option in PyXspec fakeit."
            errMsg+="\n   When fakeit sets are based on pre-existing spectra,"
            errMsg+="\n   remove any unwanted auxiliary files prior to running "
            errMsg+="fakeit."
            raise Exception(errMsg)
        
            
class DataManager(_AttrRestrictor):
    """**Spectral data container.**
    
    PyXspec automatically creates a single object of this class,
    named *AllData*.
    
       **Methods**
       
       - __call__ (the '()' operator)
       - __iadd__ (the '+=' operator)
       - __isub__ (the '-=' operator)
       - clear
       - diagrsp
       - dummyrsp
       - fakeit
       - ignore
       - notice
       - removeDummyrsp
       - show
       
       **Attributes** (get-only)
    
       - nGroups  
       - nSpectra
    
    """
    __single = None
    def __init__(self):
        if DataManager.__single:
            raise Exception("Error: Only 1 instance of DataManager is allowed.")
        DataManager.__single = self
    
    def _getNGroups(self):
        nGroupStr = _pyXspec.doTclout(["datagrp"])
        return int(nGroupStr)
    def _setNGroups(self, value):
        raise Exception("Error: Cannot rebind AllData's nGroups attribute")
    nGroups = property(_getNGroups, _setNGroups, doc="The number of data groups [int].")
    
    def _getNSpectra(self):
        nSpecStr = _pyXspec.doTclout(["datasets"])
        return int(nSpecStr)
    def _setNSpectra(self, value):
        raise Exception("Error: Cannot rebind AllData's nSpectra attribute")
    nSpectra = property(_getNSpectra, _setNSpectra, 
                        doc="The number of loaded spectra [int].")
        
    def __call__(self, expr):
        """DataManager get or set spectra.
        
        Args:
           *expr*:
               Get: 
                  An integer referring to the spectrum index number.  Returns
                  the spectrum, or raises an Exception if the integer is out
                  of range.
               Set:
                  A string following the same syntax rules as Xspec's
                  traditional "data" command handler. 

        """
        if isinstance(expr, int):
            spec = _pyXspec.getSpectrum(expr)
            return Spectrum(spec)
        
        elif expr is not None:
            _pyXspec.dataCmd(expr.split())
        else:
            _pyXspec.dataCmd([])
        
    def __isub__(self, spectra):
        """Remove 1 or all spectra from the data container.
        
        Args:
           *spectra*:  Either a single spectrum index number [int], a single
                       Spectrum object, or the string '*' to remove all. 
        
        """
        if isinstance(spectra, int):
            _pyXspec.dataCmd([str(spectra), "none/"])
        elif type(spectra).__name__ == 'Spectrum':
            iSpec = _pyXspec.getIndexFromHandle(spectra._Spectrum__handle)
            _pyXspec.dataCmd([str(iSpec), "none/"])                    
        elif spectra == '*':
            _pyXspec.dataCmd(["none"])
        else:
            raise Exception("Error: Invalid type for -= operator")
        return self

    def __iadd__(self, spectra):
        """Add 1 spectrum to the data container.
        
        Args:
           *spectra*:  The data filename string.
        
        """
        _pyXspec.readSpectrum(spectra)
        return self
        
    def clear(self):
        """Remove all spectra from the data container."""        
        _pyXspec.dataCmd(["none"])
    
    def diagrsp(self):
        """Diagonalize the current response matrix for ideal response.
        
        All currently loaded responses will be replaced with diagonal
        response matrices.  The energy range and channel binning
        information are retained from the original response, as is the
        effective area.  The channel values are mapped directly into the
        corresponding energy ranges to simulate a detector with perfect
        spectral resolution.
        
        To remove diagonal responses and restore the originals, call the
        *AllData.removeDummyrsp()* method.
        
        """
        _pyXspec.doXspecCmd(["diagrsp"])
    
    def dummyrsp(self, lowE=None, highE=None, nBins=None, scaleType=None,
                chanOffset=None, chanWidth=None):
        """Create a dummy response and apply it to all spectra.
        
           Args: (all are optional)                      
             *lowE*:  Input response energy lower bound, in keV. [float]
             
             *highE*:  Input response energy higher bound, in keV. [float]
             
             *nBins*:  Number of bins into which the energy range is 
                       divided [int].
             
             *scaleType*:  'log' or 'lin' [string]
             
             *chanOffset*:  Starting value of dummy channel 
                            energies. [float]
             
             *chanWidth*:   Energy width of the channel bins.  [float]
             
                            If this is set to 0, the dummy response
                            can only be used for evaluating model arrays,
                            and not for fitting to spectra.

           Examples::
           
                # All values are optional, use keywords to enter values
                # non-consecutively.  Unspecified values revert to the
                # current defaults.
                AllData.dummyrsp(.3, 30., 100, chanWidth=.5)
                AllData.dummyrsp(highE = 50.)
                AllData.dummyrsp(.1,10.,100,"lin",.0, 1.0)

        Initial defaults:  *lowE* = .1, *highE* = 50., *nBins* = 50, 
        *scaleType* = 'log', *chanOffset* = .0, *chanWidth* = .0.
                     
        The defaults for *lowE*, *highE*, *nBins*, *scaleType*, and 
        *chanOffset* will be modified for each explicit new entry.  
        *chanWidth* always defaults to 0.
        
        To remove dummy responses and restore actual responses (if any), call
        the *removeDummyrsp()* method.
        
        To apply a dummy response to just a single spectrum, use the 
        *Spectrum.dummyrsp* method.
        
        """
        dummyrspArgs = ["dummyrsp"]
        # Not performing a rigorous type check of each arg.  Instead rely
        # on XSPEC's dummyrsp parser to throw if it finds any problems.
        # We do however want to prevent strings with whitespace from
        # getting through (if for some reason the user decides to enter
        # vals as strings rather than ints or floats), hence all the calls
        # to split().
        if lowE is not None:
           dummyrspArgs += str(lowE).split()
        dummyrspArgs.append(',')
        if highE is not None:
            dummyrspArgs += str(highE).split()
        dummyrspArgs.append(',')
        if nBins is not None:
            dummyrspArgs += str(nBins).split()
        dummyrspArgs.append(',')
        if scaleType is not None:
            dummyrspArgs += scaleType.split()
        dummyrspArgs.append(',')
        if chanOffset is not None:
            dummyrspArgs += str(chanOffset).split()
        dummyrspArgs.append(',')
        if chanWidth is not None:
            dummyrspArgs += str(chanWidth).split()
        _pyXspec.doXspecCmd(dummyrspArgs)    
            
    def fakeit(self, nSpectra=1, settings=None, applyStats=True, filePrefix='', noWrite=False):
        """Produce spectra with simulated data using XSPEC's fakeit command.
        
        Note that if this method is run when spectra are currently loaded, it
        will follow the same rule as the standard XSPEC fakeit function:
        It will REMOVE ALL pre-existing spectra and replace each one with
        a simulated spectrum (even if *nSpectra* is less than the number 
        originally loaded).  
        
        Args: (all are optional)        
          *nSpectra*:  The number of fake spectra to produce.  [int]
           
                       If there are nOrig pre-existing spectra loaded at the
                       time this function is called and nSpectra < nOrig,
                       nSpectra will be RESET to nOrig (see note above).

                       If nSpectra == nOrig, then each of the fake spectra 
                       will use the settings from the respective original
                       spectra for their defaults (see the FakeitSettings 
                       class description).

                       If nSpectra > nOrig, then settings for the fake spectra 
                       numbered above nOrig will not be based on pre-existing
                       spectra (if any).
                        
          *settings*:   A collection of 0 to nSpectra FakeitSettings objects.
          
                        This may be entered as a list, a dictionary, a
                        single FakeitSettings object, or ``None``.          
                        If *settings* is a dictionary, the key,value pairs should
                        be the spectrum index number (1 is lowest) and the
                        FakeitSettings object.
                        
                        This function will match up FakeitSettings objects
                        1-to-1 with the nSpectra fake spectra to be created.
                        
                        If user provides FEWER than nSpectra FakeitSettings 
                        objects, fakeit will generate the necessary additional
                        objects with their default settings.  
                        
                        If MORE than nSpectra FakeitSettings objects are
                        provided, the extra objects will be ignored.
                        
          *applyStats*:  If set to True, statistical fluctuations will be
                         included in the generation of fake spectra.  [bool]
                        
          *filePrefix*:  Optional string to attach as a prefix to default fakeit
                         output file names.
                         
                         Note that this only applies when using the default 
                         file names.  If a file name is explicitly entered 
                         in the FakeitSettings.fileName attribute, it will not 
                         make use of this.
                        
          *noWrite*:  If set to True, no fakeit output files will be generated.
                      Default is False.  [bool]
                        
        Examples::
        
           # Assume no data is loaded, but a model is defined:
           
           AllData.fakeit()
           # Creates 1 fake spectrum using the default FakeitSettings object,
           # which has all input strings empty.  So it will use XSPEC's internal
           # dummy response and its output file name will be dummy_rsp_1.fak.
           
           # Now assume AllData contains 2 spectra PRIOR to running EACH of the
           # following commands, then:
           
           AllData.fakeit()
           # Creates 2 fake spectra with all settings (response, arf,
           # background, exposure, corrscale, backExposure, filenames) based
           # on the original spectra.  The original 2 spectra are removed from
           # AllData.
           
           AllData.fakeit(3)
           # Creates the first 2 spectra as above.  The 3rd fake spectrum is
           # based on the default FakeitSettings object and its output filename
           # will be dummy_rsp_3.fak
                                        
           fs = FakeitSettings(background="back1.pha", exposure=2000.0)
           sl = 3*[fs]
           AllData.fakeit(3, sl)
           # Same as above, but all 3 fake spectra will have a background file
           # based on back1.pha, and exposure time = 2000.0 sec.
           
           AllData.fakeit(3, sl, False, "my_fake_")
           # Same as above, but no statistical fluctuations will be applied to
           # fake spectra, and all output files will have the "my_fake_"
           # prefix attached.
           
           fs1 = FakeitSettings("resp1.rmf","arf1.pha",exposure=1500.)
           fs2 = FakeitSettings(fs1)
           fs2.response = "resp2.rmf"
           sd = {3:fs1, 5:fs2}
           AllData.fakeit(5, sd)
           # Creates 5 fake spectra.  The first 2 use the settings from the
           # originally loaded data.  Spectra 3 and 5 use the settings from
           # the fs1 and fs2 FakeitSettings objects, which differ only in their
           # response names.  Spectrum 4 uses the default FakeitSettings object.
           
        """
        nLoaded = _pyXspec.getNSpectra()
        if nSpectra < nLoaded:
            nSpectra = nLoaded
         
        # We want deep copies of all FakeitSettings objects.  The user can
        # send in shallow copies, ie.
        #   fs1=FakeitSettings()
        #   l1=3*[fs1]
        #   AllData.fakeit(3,l1)
        # but we may want to change attributes of objects INDIVIDUALLY, such
        # as the fileName.
        intSettings = []
        if isinstance(settings, FakeitSettings):
            intSettings = [FakeitSettings(settings)]
        elif isinstance(settings, list):
            # Using copy.deepcopy(l) isn't good enough if the original
            # list contains multiple references to the same object (as
            # in the 3*[fs1] example above).
            for i in range(len(settings)):
                intSettings += [FakeitSettings(settings[i])]                           
        elif isinstance(settings, dict):
            # Convert this to a list pronto.
            maxInDict = max(settings)
            if maxInDict > nSpectra:
                msg = "Error: Settings entry for spectrum " + str(maxInDict)
                msg += " is beyond the number of fake spectra requested."
                raise Exception(msg)
            for i in range(maxInDict):
                intSettings += [FakeitSettings()]
            # Use of iteritems() vs items() is for Python 2 and 3 compatibility.
            try:
                keyvals = settings.iteritems()
            except AttributeError:
                keyvals = settings.items()
            for k,v in keyvals:
                intSettings[k-1] = FakeitSettings(v)
                 
        nSettings = len(intSettings)
        # Remove unnecessary whitespace to make C++ parsing easier,
        # and disallow usage of fakeit 'none' option.
        for i in range(nSettings):
            intSettings[i]._stripAll()
            intSettings[i]._preventNone()
        
        # Now add or delete to list to have it match the number of fake spectra.
        # By this point, nSpectra >= nLoaded.
        if nSettings > nSpectra:
            del intSettings[nSpectra:]
            nSettings = nSpectra
        elif nSettings < nSpectra:
            # This may need some rethinking: what exactly should go
            #  into these additional records for which the user doesn't
            #  provide input?  For now just use empty (default) FakeitSettings
            #  objects, though fileName attributes may be individually
            #  modified.
            for i in range(nSettings, nSpectra):
                # Still making deep copies
                intSettings += [FakeitSettings()]
                
        # By this point must have len(intSettings) == nSpectra, where
        #  nSpectra is the number of fake sets that will be created.
        
        # For fake spectra not based on originals, let's reduce the risk of
        # having duplicate output file names.  In standard xspec, the default
        # filename is simply based on the response, even though the response
        # may be the same for all spectra (particularly if the user just takes
        # the default response  -- the dummy_rsp).  Instead, we'll add an
        # incremented suffix string to the root of the filename.
        for i in range(nLoaded, nSpectra):
            rootName=""
            curRecord = intSettings[i]
            if len(curRecord.fileName) is 0:
                if len(curRecord.response) is not 0:
                    rootName = curRecord.response
                else:
                    rootName = "dummy_rsp"
                # Remove extension (if any) from rootName
                extPos = rootName.rfind('.')
                if extPos != -1:
                    rootName = rootName[:extPos]
                # i is 0-based, make name 1-based.
                insertion = "_" + str(i+1)
                # filePrefix may be empty here, but that's OK.  If it exists
                # it must be inserted now, since the xsFakeit handler won't
                # do it when the fileName member is filled in.
                curRecord.fileName = filePrefix + rootName + insertion + ".fak"   
                                
        _pyXspec.doFakeit(intSettings, int(applyStats), filePrefix, int(noWrite))
                
    def ignore(self, ignoreRange):
        """Apply an ingore channels range to multiple loaded spectra.
        
        Args:
          *ignoreRange*:  String specifying the spectra ranges and/or
                          channel ranges to ignore, or "bad".
                          
                          This follows the same syntax as used in the standard
                          Xspec "ignore" command, except that the spectrum range
                          always defaults to ALL spectra.
                         
                          If the channel ranges are floats rather than ints, 
                          they will be treated as energies or wavelengths 
                          (depending on the *Plot* settings).
                                                  
        """
        if isinstance(ignoreRange, str):
            ignoreArgs = ["ignore"]
            colonPos = ignoreRange.find(":")
            if colonPos == -1 and ignoreRange.strip() != "bad":
                # No range specifier, want Xspec to make this apply to 
                #   ALL spectra.
                ignoreArgs += ["**:"]
            ignoreArgs += ignoreRange.split()
            _pyXspec.doXspecCmd(ignoreArgs)
        else:
            raise Exception("Error: Argument to ignore function must be a string.")
        
    def notice(self, noticeRange):
        """Apply a notice channels range to multiple loaded spectra.
        
        Args:
          *noticeRange*:  String specifying the spectra ranges and/or 
                          channel ranges to notice.  
                          
                          This follows the same syntax as used in the standard 
                          Xspec "notice" command, except that the spectrum 
                          range always defaults to ALL spectra.

                          If the numbers are floats rather than ints, they will 
                          be treated as energies or wavelengths (depending on 
                          the Plot settings).  If the string is 'all', it will 
                          notice all channels in all spectra.
        
        """
        if isinstance(noticeRange, str):
            noticeArgs = ["notice"]
            colonPos = noticeRange.find(":")
            if colonPos == -1 and noticeRange.strip() != "all":
                # No range specifier, want Xspec to make this apply to 
                #   ALL spectra.
                noticeArgs += ["**:"]
            noticeArgs += noticeRange.split()
            _pyXspec.doXspecCmd(noticeArgs)
        else:
            raise Exception("Error: Argument to notice function must be a string.")
        
    
    def removeDummyrsp(self):
        """Remove all dummy responses, restore original responses (if any)."""
        respArgs = ["response"]
        _pyXspec.doXspecCmd(respArgs)
            
    def show(self):
        """Display information for all loaded spectra."""
        _pyXspec.showAllData()
        
# End class DataManager

AllData = DataManager()


