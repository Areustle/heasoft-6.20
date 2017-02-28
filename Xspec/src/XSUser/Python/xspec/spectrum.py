from __future__ import print_function
from __future__ import absolute_import

from . import _pyXspec
from .response import _DetArrayEmulator, Response
from .xset import _AttrRestrictor

class Background(_AttrRestrictor):
    """**Background spectral data class.**
    
       **Attributes**  (all are get-only)
    
         - areaScale                            
         - backScale                               
         - exposure             
         - fileName             
         - isPoisson            
         - values                               
         - variance      
    
    """
    def __init__(self, backTuple, parent):
        """Construct a Background object.
        
        Intended for creation by a Spectrum object only.
        The parent arg should be the Spectrum object's self pointer.
        
        """
        self._turnRestrictOff()  
        self.__fileName = backTuple[0]
        self.__values = backTuple[1]
        self.__variance = backTuple[2]
        self.__isPoisson = backTuple[3]
        self.__exposure = backTuple[4]
        self.__areaScale = backTuple[5]
        self.__backScale = backTuple[6]
        self.__parent = parent
        self._resetRestrict()
        
    def _getAreaScale(self): return self.__areaScale
    def _setAreaScale(self, value):
        raise Exception("Error: Cannot rebind a Background object's area scale")
    areaScale = property(_getAreaScale, _setAreaScale, 
             doc="""The Background area scaling factor (GET only).
             
                    This is either a single float (if file stores it as a 
                    keyword), or a tuple of floats (if file stores column).""")
    
    def _getBackScale(self): return self.__backScale
    def _setBackScale(self, value):
        raise Exception("Error: Cannot rebind a Background object's back scale")
    backScale = property(_getBackScale, _setBackScale, 
           doc="""The Background back scaling factor (GET only).
           
            This is either a single float (if file stores it as a keyword),
            or a tuple of floats (if file stores column).""")
    
    def _getexposure(self): return self.__exposure
    def _setexposure(self, value):
        raise Exception("Error: Cannot rebind a Background object's exposure time")
    exposure = property(_getexposure, _setexposure, 
         doc="The exposure time keyword value [float] (GET only).")
    
    def _getFileName(self): return self.__fileName
    def _setFileName(self, value):
        raise Exception("Error: Cannot rebind a Background object's fileName")
    fileName = property(_getFileName, _setFileName, 
          doc="The spectrum's file name [string] (GET only).")
    
    def _getPoisson(self): return self.__isPoisson
    def _setPoisson(self, value):
        raise Exception("Cannot rebind a Background object's Poisson flag")
    isPoisson = property(_getPoisson, _setPoisson, 
         doc="""Boolean flag, True if spectrum has Poisson errors (GET only).""")
    
    def _getValues(self):
        noticedChans = self.__parent.noticed
        tmpVals = []
        for i in range(len(noticedChans)):
            tmpVals.append(self.__values[noticedChans[i]-1])
        # Convert tmp list to tuple upon return
        return tuple(tmpVals)            
    def _setValues(self, value):
        raise Exception("Error: Cannot rebind a Background object's values array")
    values = property(_getValues, _setValues,
              doc="""Tuple of floats containing the background rates array in 
                     counts/cm^2-sec (GET only).""")
    
    def _getVariance(self):
        noticedChans = self.__parent.noticed
        tmpVals = []
        # If this is a correction file, it won't have variance.
        if (len(self.__variance)):
            for i in range(len(noticedChans)):
                tmpVals.append(self.__variance[noticedChans[i]-1])
        # Convert tmp list to tuple upon return
        return tuple(tmpVals)            
    def _setVariance(self, value):
        raise Exception("Error: Cannot rebind a Background object's variance array")
    variance = property(_getVariance, _setVariance,
           doc="""Tuple of floats containing the variance of each 
                  channel (GET only).""")

    
class Spectrum(_AttrRestrictor):
    """**Spectral data class.**
    
       **Methods**
       
       .. hlist::
          :columns: 2
          
          - __init__
          - dummyrsp
          - ignore
          - ignoredString
          - notice
          - noticedString
          - show
       
       **Attributes**  ((*) = get-only)
       
       .. hlist::
          :columns: 2
       
          - areaScale*  
          - background   
          - backScale*  
          - cornorm     
          - correction  
          - dataGroup*  
          - energies*
          - eqwidth*    
          - exposure*   
          - fileName*   
          - flux*      
          - ignored*
          - index*    
          - isPoisson*  
          - lumin*      
          - multiresponse  
          - noticed*    
          - rate*                 
          - response   
          - values*     
          - variance*   
    
    """        
    def __init__(self, dataFile):
        """Construct a Spectrum object.
        
        Read in a spectrum and any associated background, response and
        arf files.  Spectrum is automatically added to the *AllData* container.    
        
        Args:
           *dataFile*:  Spectral data filename [string].
                   
        """
        # Private use only: If dataFile input is actually a C-pointer, 
        # then treat it as a handle to an already existing SpectralData object.
        self._turnRestrictOff()
        if (type(dataFile).__name__ == 'PyCObject' or 
                 type(dataFile).__name__ == 'PyCapsule'):
            self.__handle = dataFile
        else:
            self.__handle = _pyXspec.readSpectrum(dataFile)
        self.__detArray = _DetArrayEmulator(self.__handle)
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        specTuple = _pyXspec.getSpectrumInvariants(specIdx)
        self.__fileName = specTuple[0]
        self.__values = specTuple[1]
        self.__variance = specTuple[2]
        self.__isPoisson = specTuple[3]
        self.__exposure = specTuple[4]
        self.__areaScale = specTuple[5]
        self.__backScale = specTuple[6]
        self._resetRestrict() 
                   
    def _getAreaScale(self): return self.__areaScale
    def _setAreaScale(self, value):
        raise Exception("Error: Cannot rebind a Spectrum object's area scale")
    areaScale = property(_getAreaScale, _setAreaScale,
          doc="""The Spectrum area scaling factor.
                 
                 This is either a single float (if file stores it as a keyword),
                 or a tuple of floats (if file stores column).""")
        
    def _getbackground(self):
        specNum = _pyXspec.getIndexFromHandle(self.__handle)
        # This throws if no background.
        backTuple = _pyXspec.getBackgrnd(specNum, 0)
        return Background(backTuple, self)        
    def _setbackground(self, fileName):
        specNum = _pyXspec.getIndexFromHandle(self.__handle)
        if not fileName or fileName.isspace():
            _pyXspec.setBackgrnd(specNum, None, 0)
        else:
            _pyXspec.setBackgrnd(specNum, fileName, 0)
    background = property(_getbackground, _setbackground,
          doc="""Get/Set the spectrum's background.

                 Get: 
                   Returns the Background object associated with the
                   Spectrum.  If Spectrum has no background object,
                   this will raise an Exception.

                 Set: 
                   Supply a background filename [string].
                   This will become the new background to the Spectrum
                   object, and any previously existing background will
                   be removed.  If string is empty, all whitespace,
                   or the Python ``None`` variable, the background (if
                   any) will be removed.""")
    
    def _getBackScale(self): return self.__backScale
    def _setBackScale(self, value):
        raise Exception("Error: Cannot rebind a Spectrum object's back scale")
    backScale = property(_getBackScale, _setBackScale,
           doc="""The Spectrum background scaling factor.
           
                  This is either a single float (if file stores it as a keyword),
                  or a tuple of floats (if file stores column).""")
        
    def _getCornorm(self):
        return _pyXspec.getCornorm(self.__handle)
    def _setCornorm(self, value):
        if not isinstance(value,int) and not isinstance(value,float):
            raise Exception("Error: cornorm argument must be a numeric value")
        cornArgs = ["cornorm"]
        specNum = _pyXspec.getIndexFromHandle(self.__handle)
        cornArgs.append(str(specNum))
        cornArgs.append(str(value))
        _pyXspec.doXspecCmd(cornArgs)
    cornorm = property(_getCornorm, _setCornorm, 
      doc="""Get/Set the normalization of a spectrum's correction file. [float]""")
    
    def _getCorrection(self):
        specNum = _pyXspec.getIndexFromHandle(self.__handle)
        # This throws if no correction.
        corTuple = _pyXspec.getBackgrnd(specNum, 1)
        return Background(corTuple, self)
    def _setCorrection(self, corFile):
        specNum = _pyXspec.getIndexFromHandle(self.__handle)
        if not corFile or corFile == "none" or corFile.isspace():
            return _pyXspec.setBackgrnd(specNum, None, 1)
        else:
            corTuple = _pyXspec.setBackgrnd(specNum, corFile, 1)
            return Background(corTuple, self)
    correction = property(_getCorrection, _setCorrection,
           doc="""Get/Set the correction file.

                  Get: 
                    Returns the Spectrum's current correction information
                    as an object of class Background.  This raises an
                    Exception if Spectrum has no correction.

                  Set: 
                    Enter the filename string for the new correction.
                    This will remove any previously existing
                    correction.  Returns the new correction info
                    as an object of class Background.
                    If string is "none", empty, or all whitespace,
                    the current correction will be removed and this
                    will return ``None``.""")
    
    def _getDataGroup(self):
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        iGroupStr = _pyXspec.doTclout(["datagrp",str(specIdx)])
        return int(iGroupStr)
    def _setDataGroup(self, value):
        raise Exception("Error: Cannot rebind a spectrum's data group number")
    dataGroup = property(_getDataGroup, _setDataGroup,
             doc="The data group to which the spectrum belongs [int].")
    
    def _getEnergies(self):
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        allChanEngs = _pyXspec.getChannelEnergies(specIdx)
        noticedChans = self.noticed
        tmpVals = []
        for i in range(len(noticedChans)):
            tmpVals.append((allChanEngs[noticedChans[i]-1],
                                        allChanEngs[noticedChans[i]]))
        return tuple(tmpVals)
    def _setEnergies(self,value):
        raise Exception("Error: Cannot rebind a Spectrum object's energies")
    energies = property(_getEnergies, _setEnergies,
          doc="""Tuple of pairs of floats (also implemented as tuples)
                 giving the E_Min and E_Max of each noticed channel.""")
    
    def _getEqwidth(self):
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        eqwStr = _pyXspec.doTclout(["eqwidth",str(specIdx)])
        eqwList = eqwStr.split()
        return (float(eqwList[0]),float(eqwList[1]),float(eqwList[2]))
    def _setEqwidth(self, val):
        raise Exception("Error: Cannot rebind a Spectrum's eqwidth result")
    eqwidth = property(_getEqwidth, _setEqwidth,
          doc="""Tuple of 3 floats containing the results of the most recent
                 eqwidth calculation for this spectrum (performed with the
                 *AllModels.eqwidth* method).

                 The results are stored as:
                      [0] - eqwidth calculation
                      
                      [1] - eqwidth error lower bound
                      
                      [2] - eqwidth error upper bound
                      
                 The error bounds will be 0.0 if no error calculation was
                 performed, and all will be 0.0 if eqwidth wasn't
                 performed for this spectrum.  """)
    
    def _getexposure(self): return self.__exposure
    def _setexposure(self, value):
        raise Exception("Error: Cannot rebind a Spectrum object's exposure time")
    exposure = property(_getexposure, _setexposure,
                        doc="The exposure time keyword value [float].")
    
    def _getFileName(self): return self.__fileName
    def _setFileName(self,value):
        raise Exception("Error: Cannot rebind a Spectrum object's file name")
    fileName = property(_getFileName, _setFileName,
                        doc="The spectrum's file name [string].")
    
    def _getFlux(self):
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        return _pyXspec.getFluxLuminCalc(specIdx, 1)
    def _setFlux(self,value):
        raise Exception("Error: Cannot rebind a flux calculation value")
    flux = property(_getFlux, _setFlux,
            doc="""A tuple containing the results of the most recent flux 
                calculation for this spectrum.

                The tuple values are:
                (value, errLow, errHigh (in ergs/cm^2), value, errLow,
                errHigh (in photons)) for each model applied to the
                spectrum.""")
    
    def _getIgnored(self):
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        chanNums = _pyXspec.getIgnoredChannels(specIdx)
        return chanNums
    def _setIgnored(self, value):
        raise Exception("Error: Cannot rebind a Spectrum object's ignored array.")
    ignored = property(_getIgnored, _setIgnored,
           doc="A list of the currently ignored (1-based) channel numbers.")
    
    def _getIndex(self):
        return _pyXspec.getIndexFromHandle(self.__handle)
    def _setIndex(self, value):
        raise Exception("Error: Cannot rebind a spectrum's index number")
    index = property(_getIndex, _setIndex,
             doc="""The spectrum's current index number within the AllData
                    container [int].""")
    
    def _getPoisson(self): return self.__isPoisson
    def _setPoisson(self, value):
        raise Exception("Error: Cannot rebind a Spectrum object's Poisson flag")
    isPoisson = property(_getPoisson, _setPoisson,
         doc="Boolean flag, true if spectrum has Poisson errors.")
    
    def _getLumin(self):
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        return _pyXspec.getFluxLuminCalc(specIdx, 0)
    def _setLumin(self,value):
        raise Exception("Error: Cannot rebind a lumin calculation value")
    lumin = property(_getLumin, _setLumin,
          doc="""Similar to flux, the results of the most recent luminosity
                 calculation.""")
    
    def _getMultiresponse(self):
        return self.__detArray
    def _setMultiresponse(self, val):
        err="Error: Must specify array element for source number when setting \
multiresponse"
        raise Exception(err)
    multiresponse = property(_getMultiresponse, _setMultiresponse,
        doc="""Get/Set detector response ARRAY elements when using multiple 
               sources.

               This is for use only when assigning multiple responses
               to a spectrum, for multi-source/multi-model analysis.
               For standard single-source analysis, use the
               *response* attribute instead.

               You must provide an array index for all *multiresponse*
               get/set operations. Note that array indices ARE 0-BASED,
               so multiresponse[0] corresponds to source 1. Examples::

                  # Get the response assigned to source 1.
                  # This particular call is the same as doing
                  # "r1 = s.response"
                  r1 = spec.multiresponse[0]

                  # Get the response for the second source.
                  # Can only do this with multiresponse.
                  r2 = spec.multiresponse[1]

                  # Define a third source by adding a new response:
                  spec.multiresponse[2] = "myResp3.pha"

                  # Now remove the response for the second source:
                  spec.multiresponse[1] = None""")
    
    def _getNoticed(self):
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        chanNums = _pyXspec.getNoticedChannels(specIdx)
        return chanNums
    def _setNoticed(self, value):
        raise Exception("Error: Cannot rebind a Spectrum object's noticed array.")
    noticed = property(_getNoticed, _setNoticed,
           doc="A list of the currently noticed (1-based) channel numbers.")
    
    def _getRate(self):
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        return _pyXspec.getRate(specIdx)
    def _setRate(self, value):
        raise Exception("Error: Cannot rebind a Spectrum object's rate value")
    rate = property(_getRate, _setRate, 
        doc="""A tuple containing the total Spectrum rates in counts/sec.

               The tuple consists of:
                  [0] - current net rate (w/ background subtracted),
                  
                  [1] - net rate variance, 
                  
                  [2] - total rate (without background), 
                   
                  [3] - predicted model rate
            """)
    
    def _getResponse(self):
        resp = _pyXspec.getResponse(self.__handle, 0)
        if not resp:
            specNum = _pyXspec.getIndexFromHandle(self.__handle)
            err="Error: No response is assigned to source 1 for spectrum "
            err += str(specNum)
            raise Exception(err)
        return Response(self.__handle,resp)        
    def _setResponse(self, value):
        if value is not None and not isinstance(value, str):
            err="Error: Response setting must be a filename string or None."
            raise Exception(err)           
        fileName = None
        if value and not value.isspace():
            fileName = value
        _pyXspec.setResponse(self.__handle, 0, fileName)           
    response = property(_getResponse, _setResponse,
        doc="""Get/Set the detector response.

               Use this for standard SINGLE-SOURCE analysis.
               To add other responses for multi-source and multi-model
               analysis, use the *multiresponse* attribute.

               Get: 
                  Returns a Response object, or raises an
                  Exception if none exists

               Set: 
                  Supply a response filename string.  To remove
                  a response, supply an empty string or ``None``.""")
    
    def _getValues(self):
        noticedChans = self.noticed
        tmpVals = []
        for i in range(len(noticedChans)):
            tmpVals.append(self.__values[noticedChans[i]-1])
        # Convert tmp list to tuple upon return
        return tuple(tmpVals)            
    def _setValues(self, value):
        raise Exception("Error: Cannot rebind a Spectrum object's values array")
    values = property(_getValues, _setValues,
          doc="""Tuple of floats containing the spectrum rates for noticed
                 channels in counts/cm^2-sec.""")
    
    def _getVariance(self):
        noticedChans = self.noticed
        tmpVals = []
        for i in range(len(noticedChans)):
            tmpVals.append(self.__variance[noticedChans[i]-1])
        # Convert tmp list to tuple upon return
        return tuple(tmpVals)            
    def _setVariance(self, value):
        raise Exception("Error: Cannot rebind a Spectrum object's variance array")
    variance = property(_getVariance, _setVariance,
          doc="""Tuple of floats containing the variance of each noticed
                 channel.""")
    
    
    def dummyrsp(self, lowE=None, highE=None, nBins=None, scaleType=None,
                chanOffset=None, chanWidth=None, sourceNum=1):
        """Create a dummy response for this spectrum only.
        
           Args (all are optional):           
             *lowE*:  Input response energy lower bound, in keV. [float]
             
             *highE*: Input response energy higher bound, in keV. [float]
             
             *nBins*: Number of bins into which the energy range is divided. [int]
             
             *scaleType*:  'log' or 'lin'. [string]
             
             *chanOffset*:  Starting value of dummy channel energies. [float]
             
             *chanWidth*:  Energy width of the channel bins.  [float]
             
                           If *chanWidth* is set to 0, the dummy response
                           can only be used for evaluating model arrays,
                           and not for fitting to spectra.
                             
             *sourceNum*:  Optional source number for the dummy response. [int]

           Examples::
           
                # All values are optional, use keywords to enter values
                # non-consecutively.  Unspecified values revert to the
                # current defaults.
                s = Spectrum("dataFile.pha")
                s.dummyrsp(.3, 30., 100, chanWidth=.5)
                s.dummyrsp(highE = 50., sourceNum = 2)
                s.dummyrsp(.1,10.,100,"lin",.0, 1.0, 1)

        Initial defaults:  lowE = .1, highE = 50., nBins = 50, scaleType = "log"
        chanOffset = .0, chanWidth = .0, sourceNum = 1 
                   
        The defaults for *lowE*, *highE*, *nBins*, *scaleType*, and *chanOffset*
        will be modified for each explicit new entry.  *chanWidth* always 
        defaults to 0 and *sourceNum* always defaults to 1.
        
        To remove the spectrum's dummy response(s) and restore actual 
        responses (if any), call *AllData.removeDummyrsp()*.
        
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
        specIdx = _pyXspec.getIndexFromHandle(self.__handle)
        specSourceStr = "," + str(specIdx) + ":" + str(sourceNum)
        dummyrspArgs.append(specSourceStr)
        _pyXspec.doXspecCmd(dummyrspArgs)    
            
            
    def ignore (self, ignoreRange):
        """Ignore a range of the spectrum by channels or energy/wavelengths.
        
        Args:
          *ignoreRange*:  String specifying the channel range to ignore.
          
                          This follows the same syntax as used in the standard
                          Xspec "ignore" command. If the numbers are floats 
                          rather than ints, they will be treated as energies or 
                          wavelengths (depending on the *Plot* settings).

                          Note that "bad" will not work from here, as it can
                          only be applied to ALL of the loaded spectra.

                          To apply range(s) to multiple spectra, use the 
                          *AllData* ignore function.
        
        """
        specNum = _pyXspec.getIndexFromHandle(self.__handle)
        if isinstance(ignoreRange, str):
            rangeList = ["ignore"]
            rangeList += [str(specNum)+":"]
            rangeList += ignoreRange.split()            
            _pyXspec.doXspecCmd(rangeList)
        else:
            raise Exception("Error: Argument to ignore function must be a string.")
    def ignoredString(self):
        """Return a string of ignored channel ranges.
        
        This produces a string in compact (hyphenated) form, which can be
        used as input to a subsequent 'ignore' command.  Example: 
        
           If ignored channels are [1,3,4,5,7], 
           this function will output "1 3-5 7".
             
        """
        specNum = _pyXspec.getIndexFromHandle(self.__handle)
        ignoredStr=_pyXspec.doTclout(["ignore", str(specNum)])
        # For backwards compatibility, delimit with spaces instead of commas
        ignoredStr = ignoredStr.replace(',',' ')
        return ignoredStr
        
    def notice (self, noticeRange):
        """Notice a range of the spectrum by channels or energy/wavelengths.
        
        Args:
          *noticeRange*:  String specifying the channel range to notice.
          
                          This follows the same syntax as used in the standard
                          Xspec "notice" command. If the numbers are floats
                          rather than ints, they will be treated as energies or 
                          wavelengths (depending on the *Plot* settings).  If the 
                          string is "all", it will notice all channels in 
                          spectrum.

                          To apply range(s) to multiple spectra, use the 
                          *AllData* notice function.
        
        """
        specNum = _pyXspec.getIndexFromHandle(self.__handle)
        if isinstance(noticeRange, str):
            rangeList = ["notice"]
            rangeList += [str(specNum)+":"]
            # Cannot send an "all" specifier with a specNum, xspec lower-level
            # isn't set up for that.  So convert "all" to a range of "**".
            lcTest = noticeRange.lower()
            #  Just look for it anywhere in string.
            if lcTest.find("all") >= 0:
                rangeList.append("**")
            else:
                rangeList += noticeRange.split()
            _pyXspec.doXspecCmd(rangeList)
        else:
            raise Exception("Error: Argument to notice function must be a string.")
        
    def noticedString(self):
        """Return a string of noticed channel ranges.
        
        This produces a string in compact (hyphenated) form, which can be
        used as input to a subsequent 'notice' command.  Example: 
        
           If noticed channels are [1,3,4,5,7], 
           this function will output "1 3-5 7".
             
        """
        specNum = _pyXspec.getIndexFromHandle(self.__handle)
        noticedStr=_pyXspec.doTclout(["notice", str(specNum)])
        # For backwards compatibility, delimit with spaces instead of commas
        noticedStr = noticedStr.replace(',',' ')
        return noticedStr
                
    def show(self):
        """Display information for this Spectrum object"""
        
        _pyXspec.showData(self.__handle)
