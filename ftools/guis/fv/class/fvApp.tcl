itcl::class fvApp {
   constructor {args} {}
   destructor {}

   public  method processOptions { opts }
   public method _setupFont { }

   private method _loadLibs { }
   private method _setupColormap { }
   private method _setupOptions { }
   private method _setupBindings { }
   private method _setupBackup { }
   private method _correctPath { path }

   private variable preSetExpireTime [expr 2 * 24 * 60 * 60] 
   private variable _cmap 0
   private variable _isAborting 0
   private variable _modules {}
   private variable moduleExceptionList [list [list "hera" "tcl" "hera" "NONE"] \
                                              [list "download" "runtask" "runtask" "-download"]]
}

itcl::body fvApp::constructor {args} {
   global g_fvVersion
   global g_fvHera
   global tcl_version
   global isWin isMac g_isQuiet
   global g_openDir g_isExitting g_plugin
   global tcl_platform env
   global g_htmlViewer fvPrefObj

   if { $tcl_platform(platform) == "windows" } {
      if [info exists env(PATH)] {
         set origPath $env(PATH)
      }
      set env(PATH) "$origPath;$env(FITSVIEWER_LIBRARY)/../../gs6.52/bin;$env(FITSVIEWER_LIBRARY)/../../gs6.52/fonts"
   }
   
   if { ![info exists g_plugin] } {
      wm withdraw .
   }
 
   set g_fvVersion 5.4
   _setupOptions

   # Initialize preferences before searching through command-line options

   set fvPrefObj [fvPreferences ::fvPref]
   _setupFont

   _loadLibs
   set unusedArgs [processOptions $args]
   if { $_isAborting } {
      exitCmd
      return
   }

   _setupColormap

   if { $isMac } {
      BuildMacMenus
   }
   
   ###
   #   Build some global objects...
   ###

   FitsClipBoard ::fvClipBoard
   fvWinKeeper ::.fvwinkeeper
   FitsFileselectionbox ::.fD -title "fv: File Dialog"
   
   _setupBackup
   _setupBindings

   #####################

   # Initialize _modules before opening any files

   foreach module $_modules {
      set unusedArgs [eval ${module}::init $unusedArgs]
   }

   #####################
   #
   #  Reset Window Keepers visibility... _modules may deactivate
   #
   #####################

   .fvwinkeeper updateVisibility

   #####################
   #
   # unused Args should be nothing but file/directory names
   #
   #####################

   set optIdx [lsearch -glob $unusedArgs "-*"]
   if { $optIdx!=-1 } {
      puts "Unrecognized option [lindex $unusedArgs $optIdx]"
      processOptions -help
      set _isAborting 0
      exitCmd
      return
   }
   set g_openDir {}

   if { [llength $unusedArgs] == 0 || ([info exists g_fvHera] && $g_fvHera > 0) } {
      if { !$g_isQuiet } {
         selFile
      }
   } else {
      foreach filename $unusedArgs {
         if { $filename != {} } {
            openFile $filename
         }
      }
      if { $g_openDir != {} } {
         selFile $g_openDir
      }
   }

   # catch { set g_htmlViewer [gHtmlViewer] } err
   # catch { $g_htmlViewer deactivate } err
}

itcl::body fvApp::destructor {} { 
   if { !$_isAborting } {
      foreach module $_modules {
         ${module}::release
      }
      itcl::delete object fvClipBoard
      itcl::delete object fvPref
      itcl::delete object .fvwinkeeper
      if { [winfo exists .fD] } {
         itcl::delete object .fD
      }

## these deletes/destroys are bad programming
##   we should be calling the various objects and asking them to destro
##   themselves
## .dummy for example is created inside powScript.tcl
##    it is, though, also created in _setupColormap, presumably overriding
##    pow's creation?

      destroy .dummy
      if { [winfo exists .pow] } {
         destroy .pow
      }
   }
   destroy .
   after idle exit
}

itcl::body fvApp::_correctPath { path } { 
     set newStr ""
     for {set i 0} {$i < [string length $path]} {incr i} {
         if { [string range $path $i $i] == "\\" } {
            set newStr [format "%s/" $newStr]
         } else {
            set newStr [format "%s%s" $newStr [string range $path $i $i]]
         }
     }
     return $newStr
}

itcl::body fvApp::_loadLibs { } {
   global isWin isMac
   global env g_plugin
   global FITSVIEWER_LIBRARY
   global xpaFlag

   set FITSVIEWER_LIBRARY "$env(FITSVIEWER_LIBRARY)"

   if [info exists g_plugin] { 
      load "" fits
      load "" pow
   } elseif { $isWin } {
      set env(POW_LIBRARY) "$FITSVIEWER_LIBRARY/../pow"
#error "$FITSVIEWER_LIBRARY; $env(POW_LIBRARY)"
      set env(POW_HELPDIR) $env(POW_LIBRARY)
      load fitstcl fits
      load powtcl  pow
   } elseif { $isMac } {
      set FITSVIEWER_LIBRARY "$env(FITSVIEWER_LIBRARY)"
      set env(POW_LIBRARY) "${FITSVIEWER_LIBRARY}::pow Sources"
      set env(POW_HELPDIR)  $env(POW_LIBRARY)
      load "" fits
      load "" pow
      console hide
      bind all <Command-Option-KeyPress-0> {console show}
      # Implement Drag-n-Drop file opens
      proc ::tkOpenDocument {args} {
         foreach elem $args {
            openFile $elem
         }
         if { [winfo ismapped .fD] } {
            wm withdraw .fD
         }
      }
   } else {
      if ![ info exist env(FV_ISEXEC) ] {
          set env(POW_LIBRARY) "$FITSVIEWER_LIBRARY/../pow"
          set  env(POW_HELPDIR) $env(POW_LIBRARY)
          load [glob $FITSVIEWER_LIBRARY/../libfitstcl.{s\[ol\]*,dylib,dll}]
          load [glob $FITSVIEWER_LIBRARY/../libpow.{s\[ol\]*,dylib,dll}]
      } else {
	  if ![info exist env(FV_HELPDIR)] {
	     set fullname [info nameofexecutable]
	     set abspath [file dirname $fullname]
	     set env(FV_HELPDIR) [file join $abspath doc]
          }
	  set env(POW_LIBRARY) "/"
          set  env(POW_HELPDIR) $env(FV_HELPDIR)
          #Initialize the pow (script part)
	  # see pow/pow.tcl
	  powInitGlobals
      }
   }

   # Turn on fitsTcl's WCS swap flag, which adds a parameter to 'get wcs'
   # indicating whether the RA/Dec keywords are swapped with X/Y
   fits option wcsSwap 1

   # Besides trying to load XPA, this forces the scripting code to be loaded
   # (commented out until we have a need for XPA again)

# Pan Chai: this function will not be called until XPA is available in Windows
   if { $isWin } {
      if { [info exists ::fvPref::ds9LibPath] && [file exists ${::fvPref::ds9LibPath}/ds9.exe] } {
         set origPath $env(PATH)
         set env(PATH) "$origPath;${::fvPref::ds9LibPath};${::fvPref::xpaLibPath}"
      } else {
         set ::fvPref::imgDisplayer "POW"
         fvPref save
      }
   }
 
   catch { exec xpaset -h } err 
   set idx [string first "usage:" $err]
   
   set ::env(XPA_VERBOSITY) off
   fvXPA::init
   set xpaFlag AVAILABLE
   if { $idx >= 0 } {
      # ::powXPA::init
   } else {
      set xpaFlag NOT_AVAILABLE
   }
}

itcl::body fvApp::_setupColormap { } {
   global g_plugin
   global g_forceCMAP

   if { [info exists g_plugin] } {
      set _cmap 0
   } else {
      wm withdraw .
   }
   
   update idletask

   set screendepth [winfo screendepth .]

   # try to avoid the flash. 
   if ![info exists g_forceCMAP] {
      if { $screendepth >= 24 } {
         # true color
         set _cmap 2
      }
   } else {
      if { $screendepth >= 24 && $_cmap < 2 } {
         # true color
         set ans [tk_messageBox -icon warning -type okcancel \
                       -message "Colormap option does not match the number of bits per pixel of the screen.\n\nPlease select a value either 2 or 3 to run fv.\n\nOr would you like the system to run fv with appropriate value?"]
         if { $ans == "ok" } {
            set _cmap 2
         } else {
            exit
         }
 
      }
   }

   powSetupColormap .dummy 6 $_cmap
   wm withdraw .dummy
}

itcl::body fvApp::_setupOptions { } {

# these values are never used
#   global g_ftpID
#   global g_httpID
#   set g_ftpID         0
#   set g_httpID        0
    global g_titleFont
    global g_entryFont

   global env
   global g_hasSAOtng g_hasDS9
   global g_listObjs
   global g_histoFileID
   global g_histoPlotId
   global g_isNewFile
   global lastEventWndw
   global fileselect
   global g_fitsFileMode
   global g_isExitting g_isQuiet
   global g_isScript
   global isWin isMac tcl_platform

   # setup the options
   option add *borderWidth 1
   option add *Button.takeFocus      0 startupFile
   option add *Checkbutton.takeFocus 0 startupFile
   option add *Scrollbar.takeFocus   0 startupFile

   
   #  Initialize Some Globals
   
   set g_listObjs      {}
   set g_histoFileID   0
   set g_histoPlotId   0
   set g_isNewFile     0
   set lastEventWndw ""
   set fileselect    ""
   set g_fitsFileMode  0
   set g_isExitting    0
   set g_isQuiet       0
   set g_isScript      0
   
   if { $tcl_platform(platform) == "windows" } {
      set isWin 1
      set isMac 0
   } elseif { $tcl_platform(platform) == "macintosh" } {
      set isWin 0
      set isMac 1
   } else {
      set isWin 0
      set isMac 0
   }   

   #
   # check to see if there is SAOtng or DS9 and xpaset (with SAOtng)
   #
   
   #  Try to run saotng with a bogus flag and see if we get the expected
   #  error message including the substring "command-line"
   
   set g_hasSAOtng 0

   set g_hasDS9 1
   catch {exec ds9 -help} result

   if { [string first " option" $result] != -1 } {

      #  Check for xpaset by seeing if it will dump the expected
      #  help text containing the substring "usage"
      catch {exec xpaset -h} result
      if {[string first usage $result] != -1} {
         set g_hasDS9 1
      }
   }
}

itcl::body fvApp::_setupBindings { } {
   global isMac

   ##################################################
   #
   #     Create application-level bindings
   #
   ##################################################
   
   if { !$isMac } {
      # Implement all the necessary menu shortcuts
      
      bind all <Alt-KeyPress> {}
      event add <<Undo>>   <Control-Key-z>
      
      # File Menu
      event add <<NewFile>>     <Alt-Key-n>
      event add <<OpenFile>>    <Alt-Key-o>
      event add <<SaveFile>>    <Alt-Key-s>
      event add <<Export>>      <Alt-Key-h>
      event add <<RevertFile>>  <Alt-Key-r>
      event add <<CloseWindow>> <Alt-Key-w>
      event add <<Quit>>        <Alt-Key-q>
      event add <<Insert>>      <Alt-Key-i>
      event add <<Delete>>      <Alt-Key-d>
      bind Toplevel <<NewFile>>  createNewFITS
      bind Toplevel <<OpenFile>> selFile
      
      # Edit Menu
      event add <<Undo>>        <Alt-Key-z>
      event add <<Cut>>         <Alt-Key-x>
      event add <<Copy>>        <Alt-Key-c>
      event add <<Paste>>       <Alt-Key-v>
      
      event add <<AppendHDU>>   <Alt-Key-e>
   }
   
   bind . <<Quit>>      exitCmd
   bind . <<CheckExit>> checkForExit
}

itcl::body fvApp::_setupFont { } {
    global g_charPix
    global isWin isMac

    catch { font delete g_titleFont g_notebookTitleFont g_entryFont }
    catch { destroy .dummyFnt1 }
    catch { destroy .dummyFnt2 }
    catch { destroy .dummyFnt3 }
    catch { destroy .dummyFnt4 }

    if { $isWin } {
       font create g_titleFont -family Arial             -size -$fvPref::FontSize
       font create g_notebookTitleFont -family Arial     -size -[expr $fvPref::FontSize + 2]
    } elseif { $isMac } {
       font create g_titleFont -family system    -size -$fvPref::FontSize
       font create g_notebookTitleFont -family system -size -[expr $fvPref::FontSize + 2]
    } else {
       font create g_titleFont -family Helvetica -size -$fvPref::FontSize -weight bold
       font create g_notebookTitleFont -family Helvetica -size -[expr $fvPref::FontSize + 2] -weight bold
    }

    font create g_entryFont -family Courier -size -$fvPref::FontSize
    set g_charPix      [font measure g_entryFont m]

    # These lines guarantee that the default fonts are always loaded.
    entry .dummyFnt1
    entry .dummyFnt2 -font g_entryFont
    entry .dummyFnt3 -font g_titleFont
    entry .dummyFnt4 -font g_notebookTitleFont
    option add *FitsFileselectionbox.font g_titleFont
}

itcl::body fvApp::_setupBackup { } {
   global g_backupDir env
   global isWin isMac
   global fvHOME fvTmp

   # check if the g_backupDir exist 
   # read/write make a back up in ~/.fv (or fv_tmp for win32) dir or FVTMP dir
   
   if { $isWin } {
      set fvBackupDir "fv_tmp"
      set fvHOME FV_HOME
   } elseif { $isMac } {
      set fvBackupDir "fv_temp_folder"
      set fvHOME HOME
   } else {
      set fvBackupDir "fvtmp"
      set fvHOME HOME
   }
     
   set currentTime [clock seconds]
   set currentPid  [pid]

   set fvTmp "$fvBackupDir/fvTmp_${currentPid}_$currentTime"

   if { ![info exist env(FVTMP)] } {
      if { ![info exist env($fvHOME)] } {
         error "Please set up your $fvHOME enviroment first"
         exit 
      } else {
         if { [file exist [file join $env($fvHOME) $fvTmp]] } {
	    file stat [file join $env($fvHOME) $fvTmp] fstat
	    if { $fstat(type) != "directory"} {
               puts "~/$fvTmp is not a directory, can not create back up files"
               exit
	    } 
         } else {
	    file mkdir [file join $env($fvHOME) $fvTmp]
         }	       
         set g_backupDir [file join $env($fvHOME) $fvTmp]
      }
   } else {
      set g_backupDir $env(FVTMP)/$fvTmp
      if ![file exists $g_backupDir] {
	 file mkdir $g_backupDir
      }
   }

   # cleanup previous temp directory
   set backupList [eval glob -directory {[file dirname $g_backupDir]} -nocomplain "*"]

   foreach dirname $backupList {
       set token [split [file tail $dirname] "_"]
       if { [llength $token] == 3 } {
          set previousTime [lindex $token end]
          regsub -all {[0-9]} $previousTime {} result
          if { $result == "" } {
             # it is a time string
             set diffTime [expr $currentTime - $previousTime]
             if { $diffTime >= $preSetExpireTime } {
                file delete -force $dirname
             }
          }
       }
   }

   if { ![catch {glob [file join $g_backupDir *]}] } {	
      promptMsg "There are files left in $g_backupDir \n \
                  (probably from a terminated fv session).\n Do you want to delete them ?"  \
                  cleanBackupDir Yes No       
   }

   if { $isWin } {
      # Must change all \'s to /'s to avoid problems with escape chars
      regsub -all "\\\\" $g_backupDir "/" g_backupDir
   }
}


itcl::body fvApp::processOptions { opts } {
   global g_isQuiet
# note g_fvHera never used anywhere
   global g_fvHera
   global g_forceCMAP
   global g_fvVersion
   global g_userID
   global isWin

   set argc [llength $opts]
   set unusedArgs {}
   for {set i 0} {$i<$argc} {incr i} {
      switch -glob -- [lindex $opts $i] {
         "--help" -
          "-help" {
            puts "Usage: fv ?-cmap mode? ?-winmanager BOOL? ?-quiet? ?file?"
            puts "   -cmap <0-3>        Sets the colormap mode"
            puts "   -winmanager <1/0>  Turn Window manager On and Off"
            puts "   -quiet             Startup fv without open dialog and"
            puts "                      stay open even if all files are closed"
            puts "   -module modName    Load a web module into fv"
            puts "    or -modName       Load a web module into fv"
            puts "   -version           display fv Version"
            puts "   -user              logon with user ID"
            set _isAborting 1
         }
         "-cmap" {
            incr i
            if { $i==$argc } {
               # flag an "error" so that help message gets printed
               set _cmap -1
            } else {
               set _cmap [lindex $opts $i]
               set g_forceCMAP true
            }
            if { $_cmap < 0 || $_cmap > 3 } {
               puts "Usage: fv ?-cmap code? ?file?"
               puts "  code: 2 and 3 might eliminate color flash when\
                     you run out of color"
               puts "  0: Chose the best colormap"
               puts "  1: Force to install private pseudo colormap"
               puts "  2: Force to use truecolor colormap\
                     (if you don't have it will crash!)"
               puts "  3: Force to use screen default colormap"
               set _isAborting 1
            }
   
         }
         "-winmanager" {
            incr i
            if { $i==$argc || ![string is boolean -strict [lindex $opts $i]] } {
               puts "Usage: fv ?-winmanager BOOL? ?file?"
               set _isAborting 1
            } else {
               set ::fvPref::ifUseManager [lindex $opts $i]
            }
         }
         "-version" {
            tk_messageBox -icon info -type ok -message "FV version: $g_fvVersion"
            set _isAborting 1
         }
         "-quiet" {
            set g_isQuiet 1
         }
         "-user" {
            set g_fvHera 1
            incr i
            if { $i==$argc } {
               puts "Usage: fv ?-user userID?"
               set _isAborting 1
            } else {
               set _isWebModule 1
               set g_userID [lindex $opts 1]
               set webModule nAnonHera

               set url ${::fvPref::webModulesURLsHead}/$webModule

               set errorFlag [ catch {
                   set result [loadTclCode $url tclIndex]
               } err ]

               if { $errorFlag } {
                  puts "Unrecognized Anonymous Hera User name"
                  exit
               } else {
                  if { $result == "FAILED" } exit
               }
               lappend _modules $webModule
            }
         }
         "-mod*" {
            set g_fvHera 1
            incr i
            if { $i==$argc } {
               puts "Usage: fv ?-module moduleName?"
               set _isAborting 1
            } else {
               set _isWebModule 1
               set module [lindex $opts 1]
               set webModule [lindex $opts 1]
               if { [string tolower $module] == "hera"} {
                  set webModule tcl
               }

               set top .hmsg
               toplevel .hmsg
               set screenX [expr [winfo screenwidth .] / 2 - 100]
               set screenY [expr [winfo screenheight .] / 2 - 100]
               wm geometry $top 200x100+$screenX+$screenY
               wm title .hmsg "Starting up Hera"

               message $top.text -justify center \
                                 -text "\nStarting up Hera\n\nStand by..." \
                                 -cursor watch -font g_titleFont

               pack $top.text -fill x
               update idletask

               set url ${::fvPref::webModulesURLsHead}/$webModule
               set errorFlag [ catch {
                   set result [loadTclCode $url tclIndex]
               } err ]

               after 2000

               if { $errorFlag || [string trim $err] == "MODULE_NOT_FOUND" } {
                  tk_messageBox -icon error -type ok \
                                -message "Can't load Hera module $webModule, exiting now."
                  exit
               } else {
                  if { $result == "FAILED" } exit
               }
               destroy $top
               lappend _modules $module
            }
         }
         default {
            if { [string range [lindex $opts $i] 0 0] == "-" } {
               set testModule [string range [lindex $opts $i] 1 end]
               # test to see if it is part of exception list

               set exceptionArgs ""

               set idx [lsearch -glob $moduleExceptionList [list $testModule * * *]]
               if { $idx >= 0 } {
                  set loadModule [lindex [lindex $moduleExceptionList $idx] 1]
                  set runModule [lindex [lindex $moduleExceptionList $idx] 2]
                  set exceptionArgs [lindex [lindex $moduleExceptionList $idx] 3]
               } else {
                  set loadModule $testModule
                  set runModule  $testModule
               }

               set url ${::fvPref::webModulesURLsHead}/$loadModule
               set errorFlag [ catch {
                   set result [loadTclCode $url tclIndex]
               } err ]

               if { !$errorFlag && [string trim $err] != "FAILED" && [string trim $err] != "MODULE_NOT_FOUND" } {
#puts "result: $result"
                  if { $result != "SUCCESSFUL" } {
                     set loadModule $result
                     set runModule $result
                     set webModule $result
                  }
                  set url ${::fvPref::webModulesURLsHead}/$webModule
                  set g_fvHera 1
                  set _isWebModule 1
                  lappend _modules $runModule
                  if { $exceptionArgs != "" && $exceptionArgs != "NONE" } {
                     lappend unusedArgs $exceptionArgs
                  }
                  lappend unusedArgs [lrange $opts [expr $i + 1] end]
                  set i $argc
                  break
               } else {
                  if { [string trim $err] == "MODULE_NOT_FOUND" } {
                     tk_messageBox -icon error -type ok -message "Module [lindex $opts $i] does not exist"
                  }
                  exit
               }
            }
            if ![file exist "[lindex $opts $i]"] {
               tk_messageBox -icon error -type ok -message "File [lindex $opts $i] does not exist"
               return
            }
            set isfits [isFits "[lindex $opts $i]"]
            if { $isfits == 0 } {
               if [file isdirectory [lindex $opts $i]] {
                  lappend unusedArgs [lindex $opts $i]
               } else {
                  # test to see if it is AnonHera configure file
                  set testFile [lindex $opts $i]
                  set f [open $testFile r]
                  set data [split [read $f [file size $testFile]] \n]
                  close $f
   
                  set tokenCnt 0
                  regsub -all " " [lindex $data 0] "" result
                  set token [split $result "="]
                  if { [llength $token] == 2 } {
                     if { [string match "userid" [string tolower [lindex $token 0]]] == 1 } {
                        set g_fvHera 1
                        set _isWebModule 1
                        set g_userID [lindex $token 1]
                        set useridToken [split $g_userID "/"]
                        set webModule nAnonHera
                        set url ${::fvPref::webModulesURLsHead}/$webModule
                        if { [llength $useridToken] < 2 } {
                           set top .hmsg
                           toplevel .hmsg
                           set screenX [expr [winfo screenwidth .] / 2 - 100]
                           set screenY [expr [winfo screenheight .] / 2 - 100]
                           wm geometry $top 200x100+$screenX+$screenY
                           wm title .hmsg "Starting up Hera"
   
                           message $top.text -justify center \
                                             -text "\nStarting up Hera\n\nStand by..." \
                                             -cursor watch -font g_titleFont
   
                           pack $top.text -fill x
                           update idletask
                        }
   
                        set errorFlag [ catch {
                            set result [loadTclCode $url tclIndex]
                        } err ]
   
                        after 2000
                        if { $errorFlag || [string trim $err] == "MODULE_NOT_FOUND" } {
                           tk_messageBox -icon error -type ok \
                                         -message "Can't load Hera module: $webModule, exiting now."
                           exit
                        }

                        if [info exists top] {
                           destroy $top
                        }
   
                        lappend _modules $webModule
                     } elseif { [string match "duserid" [string tolower [lindex $token 0]]] == 1 } {
                        set g_fvHera 1
                        set _isWebModule 1
                        set g_userID [lindex $token 1]
                        set useridToken [split $g_userID "/"]
                        set webModule nAnonHeraD
                        set url ${::fvPref::webModulesURLsHead}/$webModule
                        if { [llength $useridToken] < 2 } {
                           set top .hmsg
                           toplevel .hmsg
                           set screenX [expr [winfo screenwidth .] / 2 - 100]
                           set screenY [expr [winfo screenheight .] / 2 - 100]
                           wm geometry $top 200x100+$screenX+$screenY
                           wm title .hmsg "Starting up Hera"
   
                           message $top.text -justify center \
                                             -text "\nStarting up Hera\n\nStand by..." \
                                             -cursor watch -font g_titleFont
   
                           pack $top.text -fill x
                           update idletask
                        }
   
                        set errorFlag [ catch {
                            set result [loadTclCode $url tclIndex]
                        } err ]
   
                        after 2000
                        if { $errorFlag || [string trim $err] == "MODULE_NOT_FOUND" } {
                           tk_messageBox -icon error -type ok \
                                         -message "Can't load Hera module nAnonHera, exiting now."
                           exit
                        }

                        if [info exists top] {
                           destroy $top
                        }
   
                        lappend _modules $webModule
                     } elseif { [string match "module" [string tolower [lindex $token 0]]] == 1 } {
                        set g_fvHera 1
                        set _isWebModule 1
                        set webModule [lindex $token 1]
                        set url ${::fvPref::webModulesURLsHead}/$webModule
      
                        set top .hmsg
                        toplevel .hmsg
                        set screenX [expr [winfo screenwidth .] / 2 - 100]
                        set screenY [expr [winfo screenheight .] / 2 - 100]
                        wm geometry $top 200x100+$screenX+$screenY
                        wm title .hmsg "Starting up Hera"
   
                        message $top.text -justify center \
                                          -text "\nStarting up Hera\n\nStand by..." \
                                          -cursor watch -font g_titleFont
   
                        pack $top.text -fill x
                        update idletask
   
                        set errorFlag [ catch {
                            set result [loadTclCode $url tclIndex]
                        } err ]
   
                        after 2000
                        if { $errorFlag || [string trim $err] == "MODULE_NOT_FOUND" } {
                           tk_messageBox -icon error -type ok \
                                         -message "Can't load Hera module [lindex $token 1], exiting now."
                           exit
                        }
                        destroy $top
   
                        lappend _modules $webModule
                     } else {
                        lappend unusedArgs [lindex $opts $i]
                     }
                  } else {
                     lappend unusedArgs [lindex $opts $i]
                  }
               }
               
            } else {
               lappend unusedArgs [lindex $opts $i]
            }
         }
      }
   }
   return $unusedArgs
}
