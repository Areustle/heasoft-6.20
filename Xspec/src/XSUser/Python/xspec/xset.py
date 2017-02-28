from __future__ import print_function
from __future__ import absolute_import

from . import _pyXspec
import os.path
import fileinput

class _AttrRestrictor(object):
    """A mixin to prevent adding new attributes to a class instance.
    
    This is based on a design by Python guru Alex Martelli, and is
    intended to prevent the case where misspellings during a set attribute
    operation go undetected. For example without this restriction, in the
    second case below:
    
        p1.link = "a:3"  # correct
        p1.lnk = "a:3"   # Wrong
        
    Python will simply add a new attribute to the p1 Parameter object.
    This is probably not what the user intended and is very easy to miss.
    
    """
    def __setattr__(self, name, value):
        isOK = False
        #    First check if name exists as a PROPERTY.  We could just use
        # the same hasattr test that we do for regular attributes below, 
        # BUT hasattr is implemented by calling the getattr() function, 
        # and when the attribute is a property this means its 'get' method
        # will be executed.
        #    We'd rather not have to go through the get method every time
        # we call the property set method.  After all, the get method
        # can perform arbitrarily expensive operations.  Also if the method
        # returns NULL for any reason, hasattr will misleadingly return False.
        # Therefore we'll do the property search by looking in the CLASS
        # __dict__.
        classDict = self.__class__.__dict__
        if name in classDict:
            classVal = classDict.get(name)
            if isinstance(classVal, property):
                isOK = True
        
        if not isOK:            
            # This one's tricky.  We need a way to introduce the tmpRemoveRestrict
            # instance attribute without triggering the Exception.
            # Hence the ugly string comparison test.
            if (hasattr(self, name) or Xset.allowNewAttributes or
                name == "_AttrRestrictor__tmpRemoveRestrict" or
                (hasattr(self,"_AttrRestrictor__tmpRemoveRestrict") and
                 self.__tmpRemoveRestrict)):  isOK = True
                
        if not isOK:
            errMsg = "***Error: Attribute " + name + " does not exist in class " 
            errMsg += type(self).__name__ + "  Did you misspell?\n"
            errMsg += "If you would like to add new attributes, "
            errMsg += "set Xset.allowNewAttributes to True.\n"
            raise Exception(errMsg)
        # This goes directly to base class ('object') __setattr__
        super(_AttrRestrictor,self).__setattr__(name, value)
        

    def _turnRestrictOff(self):
        """Allow setting of new attributes regardless of the flag in Xset.
        
        This is for internal use only, to allow PyXspec classes to override
        the Xset.allowNewAttributes flag and add new instance attributes
        at strategic moments.  The most likely use of this is for adding
        attributes during __init__ functions.
        
        """
        self.__tmpRemoveRestrict = True
    
    def _resetRestrict(self):
        """Remove the turnRestrictOff override, return control to the Xset flag.        
        """
        self.__tmpRemoveRestrict = False

class _ParallelHandler(_AttrRestrictor):
    """An interface to standard XSPEC's 'parallel' command.
    
    This is intended for use only as a member of the XspecSettings class,
    which is where the one instance of this class will be created.
    
    Public instance attributes (implemented as properties):
    
       error         -- Get/Set the number of processes to use during
                           a Fit.error() command run [int].
                           
       leven         -- Get/Set the number of processes to use during
                           a Levenberg-Marquardt fit [int].
    
    """
    __single = None
    def __init__(self):
        if _ParallelHandler.__single:
            raise Exception("Error: Only 1 instance of ParallelHandler is allowed.")
        _ParallelHandler.__single = self
    
    def _getLeven(self):
        return _pyXspec.getParallel("leven")
    def _setLeven(self, val):
        cmdArgs = ["parallel", "leven"]
        cmdArgs.append(str(val))
        _pyXspec.doXspecCmd(cmdArgs)
    leven = property(_getLeven, _setLeven,
                doc="Set parallel processes for Lev-Marq algorithm.")
                
    def _getError(self):
        return _pyXspec.getParallel("error")
    def _setError(self, val):
        cmdArgs = ["parallel", "error"]
        cmdArgs.append(str(val))
        _pyXspec.doXspecCmd(cmdArgs)
    error = property(_getError, _setError,
                doc="Set parallel processes for error command.")
                
    def reset(self):
        """Restore all XSPEC contexts to single-process execution."""
        self.leven = 1
        self.error = 1
        msgStr = "All parallel contexts are now reset to single-process \
execution."
        print(msgStr)
        if Xset.log is not None:
            print(msgStr,file=Xset.log)

class XspecSettings(_AttrRestrictor):
    """**Storage class for Xspec settings.**
    
    PyXspec automatically creates a single object of this class,
    named *Xset*.
    
       **Methods**
       
         - addModelString
         - closeLog
         - delModelString
         - openLog
         - restore
         - save
       
       **Attributes**  ((*) = get-only)

       .. hlist::
          :columns: 2
          
          - abund
          - allowNewAttributes       
          - allowPrompting                                  
          - chatter     
          - logChatter  
          - cosmo
          - log*         
          - modelStrings  
          - parallel                         
          - seed        
          - version*     
          - xsect       

    """
    __single = None
    __allowNewAttributes = False
    __allowPrompting = True
    # Deliberately making __parallel a class rather than object attribute.
    #   If we create it in __init__, it leads to a chicken-and-egg problem
    #   in _AttrRestrictor.__setattr__ (since the latter attempts to use a 
    #   fully created Xset object).
    __parallel = _ParallelHandler()
    
    def __init__(self):
        if XspecSettings.__single:
            raise Exception("Error: Only 1 instance of XspecSettings is allowed.")
        XspecSettings.__single = self
    
    def _getAbund(self):
        return _pyXspec.getAbund()
    def _setAbund(self, table):
        #In most cases table arg will simply contain 1 word.
        abundArgs = ["xset","abund"]
        abundArgs += table.split()
        _pyXspec.doXspecCmd(abundArgs)
    abund = property(_getAbund, _setAbund, 
       doc="""Get/Set the abundance table used in the plasma emission and
              photoelectric absorption models [string].

              Valid tables: 'angr', 'aspl', 'feld', 'aneb', 'grsa', 'wilm', 
              'lodd', 'file <filename>' """) 
    
    def _getAllowNewAttributes(self):
        return XspecSettings.__allowNewAttributes
    def _setAllowNewAttributes(self, value):
        if isinstance(value, bool):
            XspecSettings.__allowNewAttributes = value
        else:
            raise Exception("Error: Argument must be a bool.")    
    allowNewAttributes = property(_getAllowNewAttributes,_setAllowNewAttributes,
         doc="""Get/Set the flag which allows the setting of new
                instance attributes for ALL PyXspec classes [bool].

                This is False by default, and is intended to catch the
                user's attention if they misspell an attribute name
                when attempting to set it.  Under normal Python 
                behavior, a misspelling would simply create a new 
                attribute and issue no warnings or errors.

                You must make sure this flag is set to True if you
                genuinely wish to add new attributes.
             """)
    
    def _getAllowPrompting(self):
        return XspecSettings.__allowPrompting
    def _setAllowPrompting(self, val):
        if isinstance(val, bool):
            if val:
                _pyXspec.allowPrompting(1)
                XspecSettings.__allowPrompting = True
            else:
                _pyXspec.allowPrompting(0)
                XspecSettings.__allowPrompting = False
        else:
            raise Exception("Error: allowPrompting argument must be a bool.")        
    allowPrompting = property(_getAllowPrompting, _setAllowPrompting,
          doc="""Get/Set flag determining whether user prompting occurs.
       
                 Get/Set whether user will be prompted in situations
                 where XSPEC may require additional information
                 to complete a task.  Default is 'True'. [bool]""")
    
    def _getChatter(self):
        return _pyXspec.getChatter(0)
    def _setChatter(self, val):
        if isinstance(val, int):
            _pyXspec.setChatter(0, val)
        else:
            raise Exception("Error: Chatter argument must be an integer.")        
    chatter = property(_getChatter, _setChatter,
                        doc="Get/Set the console chatter level [int].")
    
    def _getLogChatter(self):
        return _pyXspec.getChatter(1)
    def _setLogChatter(self, val):
        if isinstance(val, int):
            _pyXspec.setChatter(1, val)
        else:
            raise Exception("Error: Log chatter argument must be an integer.")       
    logChatter = property(_getLogChatter, _setLogChatter,
                        doc="Get/Set the log chatter level [int].")
    
    def _getCosmo(self):
        tcloutArgs = ["cosmo"]
        valStr = _pyXspec.doTclout(tcloutArgs)
        return tuple(valStr.split())
    def _setCosmo(self,values):
        cosmoArgs = ["xset","cosmo"]
        cosmoArgs += values.split()
        _pyXspec.doXspecCmd(cosmoArgs)        
    cosmo = property(_getCosmo, _setCosmo,
        doc="""Get/Set the cosmology values.

            Get: Returns a tuple of floats containing (H0, q0, l0), where
                   - H0 is the Hubble constant in km/(s-Mpc),
                   - q0 is the deceleration parameter, and
                   - l0 is the cosmological constant.

            Set: Enter a single string containing one or more of H0, q0, l0.  
            
            Examples::
            
               Xset.cosmo = "100"  # sets H0 to 100.0
               Xset.cosmo = ",0"   # sets q0 to 0.0
               Xset.cosmo = ",,0.7"  # sets l0 to 0.7
               Xset.cosmo = "50 .5 0." # sets H0=50.0, q0=0.5, l0=0.0""")

    def _getLog(self):
        return _pyXspec.getLog()
    def _setLog(self, val):
        raise Exception("Error: Cannot rebind Xset.log attribute")
    log = property(_getLog, _setLog, 
           doc="""Get only: Returns the currently opened log file object,
                  or ``None`` if no log file is open (also see the *openLog* 
                  and *closeLog* methods).""")

    def _getModelStrings(self):
        modStrDict = _pyXspec.getModelStringValues()
        # Convert dictionary to non-mutable return type (ie. a tuple).
        #   Don't want to give user the impression that modifications
        #   made to the returned argument will be mirrored in Xspec,
        #   cause they won't. 
        tmpList = []
        # Use of iteritems() vs items() is for Python 2 and 3 compatibility.
        try:
            keyvals = modStrDict.iteritems()
        except AttributeError:
            keyvals = modStrDict.items()
        for k,v in keyvals:
            entryTup = k, v
            tmpList.append(entryTup)
        return tuple(tmpList)
    def _setModelStrings(self, val):
        if isinstance(val, dict):
            try:
                keyvals = val.iteritems()
            except AttributeError:
                keyvals = val.items()
            for k,v in keyvals:
                if not isinstance(k, str) or not isinstance(v, str):
                    err="Error: All names and value entries must be strings."
                    raise Exception(err)
            _pyXspec.setModelStringValues(val)
        elif isinstance(val, tuple):
            valDict = {}
            for i in range(len(val)):
                entry = val[i]
                if len(entry) != 2:
                    err="Error: Individual entries should be tuples containing "
                    err += "<name> and <value> strings"
                    raise Exception(err)
                if not isinstance(entry[0],str) or not isinstance(entry[1],str):
                    err="Error: Inner tuple must contain only <name>,<value> strings"
                    raise Exception(err)
                valDict[entry[0]] = entry[1]
            _pyXspec.setModelStringValues(valDict)
        else:
            err="Error: modelStrings set requires a dictionary of string pairs "
            err +="or a tuple of string pairs (tuples)"
            raise Exception(err)
    modelStrings = property(_getModelStrings, _setModelStrings,
          doc="""XSPEC's internal database of <string_name>,
                 <string_value> pairs for settings which may be 
                 accessed by model functions.

                 Get: 
                      Returns a tuple of tuples, the inner tuples
                      being composed of <string_name>,<string_value> 
                      string pairs.

                 Set: 
                      Replaces ENTIRE database with user-supplied
                      new database.  Input may be a dictionary of
                      <string_name>:<string_value> entries, or a tuple
                      of (<string_name>,<string_value>) tuples.

                 For inserting and deleting INDIVIDUAL string 
                 name and value pairs, use the *addModelString* and
                 *delModelString* methods.""")
    
    def _getParallel(self):
        return XspecSettings.__parallel
    def _setParallel(self, val):
        raise Exception("Error: Cannot rebind Xset.parallel attribute")
    parallel = property(_getParallel, _setParallel,
        doc="""An attribute for controlling the number of parallel
               processes in use during various XSPEC contexts.
               
               Examples::
               
                  # Use up to 4 parallel processes during
                  #   Levenberg-Marquardt fitting.   
                  Xset.parallel.leven = 4

                  # Use up to 4 parallel processes during
                  #   Fit.error() command runs.
                  Xset.parallel.error = 4

                  # Reset all contexts to single process usage.
                  Xset.parallel.reset()""")
    
    def _getSeed(self):
        pass
    def _setSeed(self, value):
        if isinstance(value,int):
            seedArgs = ["xset","seed"]
            seedArgs.append(str(value))
            _pyXspec.doXspecCmd(seedArgs)
        else:
            raise Exception("Error: Seed argument must be an integer")
    seed = property(_getSeed,_setSeed,
        doc="""Re-seed and re-initialize XSPEC's random-number generator 
               with the supplied integer value (SET only).""")
    
    def _getVersion(self):
        pyXSvers = "2.0.0"
        stdXSvers = _pyXspec.doTclout(["version"])
        return (pyXSvers,stdXSvers)
    def _setVersion(self, val):
        raise Exception("Error:  Cannot change the version information")
    version = property(_getVersion, _setVersion,
        doc="""The version strings for PyXspec and standard XSPEC.
                                
               GET only, this returns a tuple containing:
               
               (<PyXspec version string>,<standard XSPEC version string>)
            """)
    
    def _getXsect(self):
        return _pyXspec.getXsect()
    def _setXsect(self, value):
        if isinstance(value,str):
            xsectArgs = ["xset","xsect"]
            xsectArgs += value.split()
            _pyXspec.doXspecCmd(xsectArgs)
        else:
            raise Exception("Error: xsect argument must be a string")
    xsect = property(_getXsect,_setXsect,
        doc="""Get/set the photoelectric absorption cross-sections in use 
               [string].

               Available options: 'bcmc', 'obcm', 'vern' """)
    
    def addModelString(self, key, value):
        """Add a key,value pair of strings to XSPEC's internal database.
        
           This database provides a way to pass string values to certain 
           model functions which are hardcoded to search for "key". 
           (See the XSPEC manual description for the "xset" command for a
           table showing model/key usage.)
           
           If the key,value pair already exists, it will be replaced with
           the new entries.
           
        """
        if isinstance(key,str) and isinstance(value,str):
            # User should not have entered whitespace in key or value,
            # but use split() to be sure.
            modStringArgs = ["xset"]
            modStringArgs += key.split()
            modStringArgs += value.split()
            _pyXspec.doXspecCmd(modStringArgs)    
        else:
            raise Exception("Error: addModelString requires 2 string arguments.")
    
    def delModelString(self, key):
        """Remove a key,value pair from XSPEC's internal string database.
        
        """
        if isinstance(key,str):
            modStringArgs = ["xset"]
            modStringArgs += key.split()
            _pyXspec.doXspecCmd(modStringArgs)
        else:
            raise Exception("Error: delModelString requires a string argument.")
    
    def closeLog(self):
        """Close XSPEC's current log file."""
        _pyXspec.closeLog()
        
    def openLog(self, fileName):
        """Open a file and set it to be XSPEC's log file.

           Args:
             *fileName*: The name of the log file.

           If Xspec already has an open log file, it will close it.
           Returns a Python file object for the new log file.
           
           Once opened, the log file object is also stored as the
           *Xset.log* attribute.
           
           .. note::
              To ensure proper cleanup and file flushing, it is recommended
              that you call *Xset.closeLog(fileName)* before exiting PyXspec.

        """
        if isinstance(fileName, str):
            return _pyXspec.setLog(fileName)
        else:
            raise Exception("Error: setLog argument must be a file name string.")
      
        
    def show(self):
        _pyXspec.showXset()
        
    def restore(self, fileName):
        """Restore the data/model configuration and settings.
        
           This will restore the data, models, and settings from a previous
           PyXspec session, as saved in file generated by the
           *Xset.save()* function.
        
           Args:
             *fileName*: The output file from a previous *Xset.save* command.
           
        """
        if isinstance(fileName, str):
            savedFile = open(fileName,'r')
            # check for PyXspec indicator in first line
            isPyXspecSave = savedFile.readline().startswith('#PyXspec')
            warningMsg ="\n***Warning: The file sent to Xset.restore(): "+fileName
            warningMsg += "\n    is not detected to be a file generated from Xset.save()."
            warningMsg += "\n    Xset.restore() usage is only intended for Xset.save() output."
            warningMsg += "\n    General XSPEC/Tcl scripts may not fully execute in PyXspec.\n\n"
            # reset back to start of file
            savedFile.seek(0)
            processingModelCmd = False
            try:
                for line in savedFile:
                    if line.strip() and line.lstrip()[0] != '#':
                        startNextCmd = False
                        lineArgs = line.split()
                        if processingModelCmd:
                            isNum = False
                            try:
                                float(lineArgs[0])
                                isNum = True
                            except Exception:
                                pass
                            if (isNum or lineArgs[0][0] == '='or lineArgs[0][0] == '/'): 
                                fullModelCmd.append('&')
                                fullModelCmd += lineArgs
                            else:
                                # Finished gathering args for previous model command
                                _pyXspec.doXspecCmd(fullModelCmd)
                                processingModelCmd = False
                                startNextCmd = True
                        else:
                            startNextCmd = True

                        if startNextCmd:
                            if lineArgs[0] in 'model':
                                processingModelCmd = True
                                fullModelCmd = lineArgs
                            elif lineArgs[0] in 'data':
                                _pyXspec.dataCmd(lineArgs[1:])
                            elif (lineArgs[0] in 'newpar' or lineArgs[0] in 'rnewpar'):
                                isRespPar = 1 if lineArgs[0][0] == 'r' else 0
                                singleStrArgs = str()
                                for arg in lineArgs[1:]:
                                    singleStrArgs += arg
                                    singleStrArgs += ' '
                                singleStrArgs = singleStrArgs.rstrip() 
                                _pyXspec.newparCmd(singleStrArgs, isRespPar)
                            else:
                                _pyXspec.doXspecCmd(lineArgs)
                # Check if we've reached the end with a model command still pending.
                if processingModelCmd:
                    _pyXspec.doXspecCmd(fullModelCmd)
                if not isPyXspecSave:
                    print(warningMsg)
                    if Xset.log is not None:
                        print(warningMsg,file=Xset.log)
            except Exception:
                savedFile.close()
                if not isPyXspecSave:
                    print(warningMsg)
                    if Xset.log is not None:
                        print(warningMsg,file=Xset.log)
                raise
                
            savedFile.close()
        else:
            raise Exception("Error: restore argument must be a file name string.")
                  
    def save(self, fileName, info='a'):
        """Save the data and model configuration and XSPEC settings
        
           Args:
             *fileName*: The name of the output file.
             
                         If the file name has no extension, '.xcm' will be 
                         appended.
           
             *info*:     A flag specifying which information to save:
             
                         - 'a' = save all (the default)
                         - 'f' = only save the data files information
                         - 'm' = only save the model information
           
        """
        if isinstance(fileName, str):
            fileName = fileName.strip()
            if len(fileName) == 0:
                raise Exception("Error: Must provide a file name argument.")
            if (not isinstance(info,str) or len(info)==0 or
               (info[0] !='a' and info[0] !='f' and info[0] !='m') ):
                raise Exception("Error: 2nd argument to 'save' should be 'a', 'f', or 'm'")
            else:
                saveArgs = ['save']
                if info[0] == 'a':
                    saveArgs.append('all')
                elif info[0] == 'f':
                    saveArgs.append('files')
                elif info[0] == 'm':
                     saveArgs.append('model')
                     
                # Append .xcm if no extension detected.  Note that this is
                # redundant since the Xspec xsSave handler would do this 
                # same operation below.  However we need to know at this level
                # what the ultimate file name will be in order to reopen it
                # and insert PyXspec-specific information.
                extension = os.path.splitext(fileName)[1]
                if len(extension) == 0:
                    fileName += ".xcm"
                saveArgs.append(fileName)
                
                _pyXspec.doXspecCmd(saveArgs)
                
                # If doXspecCmd didn't raise an exception, assume save file has
                # been written to disk.  Add a PyXspec identifier to top of file.
                
                # The key here is the 'inplace' specifier.  While iterating through
                # the FileInput object, standard output is automatically rerouted
                # to the file given by 'fileName'.  
                saveFile = fileinput.input(fileName, inplace=1)
                for line in saveFile:
                    if saveFile.isfirstline():
                        print('#PyXspec: Output generated from Xset.save().  DO NOT MODIFY.\n')
                        print(line.rstrip())
                    else:
                        print(line.rstrip())    
                
        else:
            raise Exception("Error: The first argument to 'save' must be a file name string.")
    
    
Xset = XspecSettings()


