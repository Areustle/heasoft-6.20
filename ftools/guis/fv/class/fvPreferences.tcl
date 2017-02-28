# When this class is initialized in fvApp, the constructor will create a
# ::fvPref namespace with various variable names

itcl::class fvPreferences {

   # Create and initialize fvPref namespace
   constructor { args } {}
   destructor { }

   public {
      method save  { }
      method edit  { {page "fv"} }
      method close { }
      method raisePage { page }
      method _setGraphSize { menu {val ""} }
      method _askDirectorySetup {}
      method getCurrentGraphSize {} { return $fvPref::graphDispSize }
      method setNewGraphSize { size } { set ::fvPref::graphDispSize $size ; fvPref save }
   }

   private {
      method _read {}
      method _browseDirectory { win }
      method _selectDirectory { dir }

      method _buildPage_App      { w }
      method _buildPage_Keywords { w }
      method _buildPage_Tables   { w }
      method _buildPage_Graphs   { w }
      method _buildPage_Colors   { w }

      method _getGraphSize { }
      method _correctPath { path }
      method _changeColor  { type }
      method _changeWindowColors { win type }
      method _toggleChoice {}

      method _toggleCheckedSizeState { dim }
      method _deleteCheckedSize { {op "delete"} }
      method _addNewSize {}

      variable note
      variable _FVRC
      variable _padXAmnt 10
      variable _padYAmnt 3
      variable _tabLabels [list App Keywords Tables Graphs Colors]
      variable _selectedDirectoryName
      variable _currentBrowseDirectoryEntry
      variable _previousDirectory ""
      variable viewerWindow ""
      variable previousImgDisplayer ""

      variable sizeCheckList {}
      variable _graphDefaultWndw ""
   }
}

itcl::body fvPreferences::constructor { args } {
   global tcl_platform env FITSVIEWER_LIBRARY
   global g_titleFont

   switch $tcl_platform(platform) {
      "macintosh" {
         set _FVRC [file join $env(PREF_FOLDER) "fv Preferences"]
      }
      "windows" {
         set  FITSVIEWER_LIBRARY $env(FITSVIEWER_LIBRARY)
         set _FVRC [file join $FITSVIEWER_LIBRARY "fv.ini"]
         # Pan Chai: uncomment when XPA is available in Windows
         # lappend _tabLabels Library
      }
      default {
         set _FVRC "~/.fvrc"
      }
   }

   option add *fvPreference.font g_titleFont
   # Create and initialize fvPref namespace

   namespace eval ::fvPref { 
      variable globalBgColor        #cccccc
      variable globalFgColor        black  
      variable activeBgColor        #eeeeee
      variable activeFgColor        black  
      variable checkBBgColor        red
      variable imgDisplayer         POW
      variable ifWriteHisKey        1
      variable ifDispFitsOnly       1
      variable ifWCSInfo            1
      variable ifProtectedKeys      1
      variable ifAutoReformatKeys   1
      variable ifLeftJustifyString  1
      variable ifAutoUpdateChecksum 0
      variable ifUseManager         1
      variable ifAutoPlotPrimary    0
      variable keepFileDialogVisible 0
      variable xpaLibPath           
      variable ds9LibPath           
      variable winManagement        Keep
      variable graphDispSize        [list 300 300]
      variable FontSize             12
      variable webModulesURLsHead   "http://heasarc.gsfc.nasa.gov/hera/"
      variable BrowserSetting       "None Defined"
      variable GraphDefaultSizeList [list 100x100 120x120 160x160 200x200 256x256 300x300 400x400 500x500 600x600 700x700 300x400 400x300] 
   }

   # Override some defaults based on enviroment

   if { [winfo screenheight .]>850 } {
      set fvPref::graphDispSize [list 500 500]
   }

   set tmpBg [option get . background Background]
   if { $tmpBg != ""} {
      set fvPref::globalBgColor $tmpBg
   }
   set tmpFg [option get . foreground Foreground]
   if { $tmpFg != ""} {
      set fvPref::globalFgColor $tmpFg
   }

   if { $tcl_platform(platform)=="macintosh" } {
      set fvPref::ifUseManager 0
      set fvPref::globalBgColor "#eeeeee"
      set fvPref::activeBgColor "#cccccc"
   }

   if { $tcl_platform(platform)!="windows" } {
      set fvPref::BrowserSetting "None Defined"
   }

   if [info exists env(HERA_SERVER) ] {
      set fvPref::webModulesURLsHead "http://$env(HERA_SERVER)/hera/"
   } 
      
   #  Read in the preferences file
   _read
}

itcl::body fvPreferences::destructor { } {
   if { [winfo exists .fvpref] } {
      destroy .fvpref
   }
}


itcl::body fvPreferences::edit { {page "fv"} } {
   global tcl_platform
   global g_titleFont
   
   if { [winfo exists .fvpref] } {
      if {$tcl_platform(platform)!= "windows"} { 
        .fvpref activate
        Notebook:raise [.fvpref childsite].notebook $page
        # since the notebook does not actual destroy, we need this line to update graph size
        _setGraphSize [.fvpref childsite].notebook.f3.graphsize $fvPref::graphDispSize
      } else {
        focus .fvpref
        raise .fvpref        
        Notebook:raise .fvpref.notebook $page
      }        
      return
   }

   ###########
   # Setup Dialog Box
   ###########
   if { $tcl_platform(platform) != "windows" } {  
    ::iwidgets::dialogshell .fvpref -title "fv: Preferences" -modality "none"
    .fvpref add Save -text "Save" -command [itcl::code $this save] -font g_titleFont
    .fvpref add Help -text "Help" -command "hhelp preferences" -font g_titleFont
    .fvpref add Exit -text "Exit" -command [itcl::code $this close] -font g_titleFont
    .fvpref default Exit
   } else {
     toplevel .fvpref -class Dialog
     wm  title .fvpref "fv: Preferences"
     frame .fvpref.buts
     button .fvpref.buts.save -text Save -command [itcl::code $this save] -font g_titleFont
     button .fvpref.buts.help -text Help -command "hhelp preferences" -font g_titleFont
     button .fvpref.buts.exit -text Exit -command [itcl::code $this close] -font g_titleFont
     pack .fvpref.buts.save -side left -padx 30 -pady 4          
     pack .fvpref.buts.help -side left -padx 30  -pady 4        
     pack .fvpref.buts.exit -side right -padx 30 -pady 4
                
   }

   bind .fvpref <<CloseWindow>> [itcl::code $this close]

   ###########
   # Setup Tabbed Notebook Widget
   ###########

   if {$tcl_platform(platform) != "windows" } {   
      set note "[.fvpref childsite].notebook"
   } else {
      set note .fvpref.notebook
   } 

   Notebook:create $note -pages $_tabLabels -pad 8

   foreach p $_tabLabels {
      eval _buildPage_$p [Notebook:frame $note $p]
   }

   pack $note -fill both -expand 1   
    
   ###########
   # Display dialog
   ###########

   Notebook:raise $note $page
   Notebook:raise.page $note [lsearch $_tabLabels "Graphs"]
   Notebook:resize $note
   if {$tcl_platform(platform) != "windows" } {    
      .fvpref activate
   } else {
      pack .fvpref.buts 
   }
}

itcl::body fvPreferences::raisePage { page } {

   Notebook:raise.page $note [lsearch $_tabLabels $page]
   Notebook:resize $note
     
}

itcl::body fvPreferences::close { } {
   global tcl_platform
   if { [winfo exists .fvpref] } {
      if {$tcl_platform(platform) == "windows" } { 
        destroy .fvpref
      } else {
        .fvpref deactivate
      }
   }
   checkForExit
}


itcl::body fvPreferences::_read { } {
   global tcl_platform

   if { ![catch { set rcFile [open $_FVRC r] } ] } {
      while { [gets $rcFile line] != -1} {
         set entry [lindex $line 0]
         set value [lindex $line 1]
         if { [string length [string trim $entry]] <= 0 } continue
         switch -glob -- $entry {
            "Background" {	
               set fvPref::globalBgColor $value
            }
            "Foreground" {
               set fvPref::globalFgColor $value
            }
            "ActiveBackground" {
               set fvPref::activeBgColor $value
            }
            "ActiveForeground" {
               set fvPref::activeFgColor $value
            }
            "CheckButtonColor" {
               set fvPref::checkBBgColor $value
            }
            "ImageDisplayer" {
               set fvPref::imgDisplayer $value
            }
            "IfWriteHisKey" {
               set fvPref::ifWriteHisKey $value
            } 
            "IfDispFitsOnly" {
               set fvPref::ifDispFitsOnly $value
            } 	
            "IfWCSInfo" {
               set fvPref::ifWCSInfo $value
            }
            "IfProtectedKeys" {
               set fvPref::ifProtectedKeys $value
            }
            "IfAutoReformatKeys" {
               set fvPref::ifAutoReformatKeys $value
            }
            "WinManagement" {
               set fvPref::winManagement $value	
            }		
            "IfAutoUpdateChecksum" {
               set fvPref::ifAutoUpdateChecksum $value
            }	
            "IfLeftJustifyString" {
               set fvPref::ifLeftJustifyString $value
            }
            "IfUseDesktopManager" {
               if { $tcl_platform(platform) != "macintosh" } {
                  set fvPref::ifUseManager $value
               }
            }
            "IfAutoPlotPrimary" {
               set fvPref::ifAutoPlotPrimary $value
            }
            "GraphDisplaySize" {
               set fvPref::graphDispSize $value
            }
            "xpaLibPath" {
               set fvPref::xpaLibPath "$value"
            }
            "ds9LibPath" {
               set fvPref::ds9LibPath "$value"
            }
            "BrowserSetting" {
               set fvPref::BrowserSetting "$value"
            }
            "#*" {
            }
            default {
               set fvPref::$entry $value
            }
         }
      }			 
      ::close $rcFile
   }

   if ![info exists fvPref::GraphDefaultSizeList] {
      set ::fvPref::GraphDefaultSizeList [list 100x100 120x120 160x160 200x200 256x256 300x300 400x400 500x500 600x600 700x700 300x400 400x300]
   }

   if ![info exists fvPref::FontSize] {
      set ::fvPref::FontSize 12
   }
   option add *Background          $fvPref::globalBgColor
   option add *Foreground          $fvPref::globalFgColor
   option add *HighlightBackground $fvPref::globalBgColor
   option add *activeForeground    $fvPref::activeFgColor
   option add *activeBackground    $fvPref::activeBgColor
   option add *selectForeground    $fvPref::activeFgColor
   option add *selectBackground    $fvPref::activeBgColor
   option add *selectColor         $fvPref::checkBBgColor

}

itcl::body fvPreferences::save {} {
   global tcl_platform

# check if _FVRC file is there
   catch { [file delete -force $_FVRC]} 

   if { [catch {set rcFile [open $_FVRC w]} err] == 1} {
      error "Unable to open $_FVRC to write"
      return 
   }

   puts $rcFile "\# Please don't change this file. It's auto generated by fv"
   puts $rcFile "Background           $fvPref::globalBgColor"
   puts $rcFile "Foreground           $fvPref::globalFgColor"
   puts $rcFile "ActiveBackground     $fvPref::activeBgColor"
   puts $rcFile "ActiveForeground     $fvPref::activeFgColor"
   puts $rcFile "CheckButtonColor     $fvPref::checkBBgColor"
   puts $rcFile "ImageDisplayer       $fvPref::imgDisplayer"
   puts $rcFile "IfWriteHisKey        $fvPref::ifWriteHisKey"
   puts $rcFile "IfDispFitsOnly       $fvPref::ifDispFitsOnly"
   puts $rcFile "WinManagement        $fvPref::winManagement"
   puts $rcFile "IfWCSInfo            $fvPref::ifWCSInfo"
   puts $rcFile "IfProtectedKeys      $fvPref::ifProtectedKeys"
   puts $rcFile "IfAutoReformatKeys   $fvPref::ifAutoReformatKeys"
   puts $rcFile "IfAutoUpdateChecksum $fvPref::ifAutoUpdateChecksum"
   puts $rcFile "IfLeftJustifyString  $fvPref::ifLeftJustifyString"
   puts $rcFile "IfUseDesktopManager  $fvPref::ifUseManager"
   puts $rcFile "IfAutoPlotPrimary    $fvPref::ifAutoPlotPrimary"
   puts $rcFile "GraphDisplaySize     {$fvPref::graphDispSize}"
   puts $rcFile "GraphDefaultSizeList {$fvPref::GraphDefaultSizeList}"
   puts $rcFile "FontSize             $fvPref::FontSize"

   if { [info exists ::fvPref::BrowserSetting] && [string trim $fvPref::BrowserSetting] != "None Defined" } {
      puts $rcFile "BrowserSetting       $fvPref::BrowserSetting"
   }

   if {$tcl_platform(platform) != "windows" } { 
      if { [winfo exists .fvpref] && [winfo ismapped [.fvpref childsite].notebook.f3.graphsize] } {
         _setGraphSize [.fvpref childsite].notebook.f3.graphsize $fvPref::graphDispSize
      }
   } else {
      if { [winfo exists .fvpref] && [winfo ismapped .fvpref.notebook.f3.graphsize] } {
         _setGraphSize .fvpref.notebook.f3.graphsize $fvPref::graphDispSize
      }
   }

   if { [winfo exists .browseDirectorySetup] && [winfo ismapped .browseDirectorySetup] } {
      set browseDirectorySetupHList [.browseDirectorySetup.directory.directoryTree subwidget hlist]

      set _selectedDirectoryName [$browseDirectorySetupHList selection get]
      $_currentBrowseDirectoryEntry delete 0 end
      $_currentBrowseDirectoryEntry insert end $_selectedDirectoryName
   }

#  Pan Chai: uncomment these line when XPA available in Windows
   if {$tcl_platform(platform) == "windows" } { 
      if { [info exists ::fvPref::xpaLibPath] && [string length [string trim $fvPref::xpaLibPath]] > 0 } {
         puts $rcFile "xpaLibPath           \"$fvPref::xpaLibPath\""
      } elseif { $::fvPref::imgDisplayer == "DS9" } {
         _askDirectorySetup
         puts $rcFile "xpaLibPath           \"$fvPref::xpaLibPath\""
         puts $rcFile "ds9LibPath           \"$fvPref::ds9LibPath\""
      }

      if { [info exists ::fvPref::ds9LibPath] && [string length [string trim $fvPref::ds9LibPath]] > 0 } {
         puts $rcFile "ds9LibPath           \"$fvPref::ds9LibPath\""
      } elseif { $::fvPref::imgDisplayer == "DS9" } {
         _askDirectorySetup
         puts $rcFile "ds9LibPath           \"$fvPref::ds9LibPath\""
         puts $rcFile "xpaLibPath           \"$fvPref::xpaLibPath\""
      }

      if [winfo exists .fvwinkeeper] {
         .fvwinkeeper updateDisplayDeviceStatus
      }
   }

   ::close $rcFile

   set _previousDirectory ""
   catch { wm deiconify .browseDirectorySetup }
   catch { wm withdraw .browseDirectorySetup }
   fvApp::_setupFont

# puts "Options saved in $_FVRC"
}

itcl::body fvPreferences::_askDirectorySetup {} {
    option add *textBackground white

    if [winfo exists .askDirectorySetup] {
       destroy .askDirectorySetup
    }
    toplevel .askDirectorySetup 
    wm title .askDirectorySetup "Input DS9 and XPA Directory"
    set top .askDirectorySetup

    frame $top.selection
    set selectionFrame $top.selection

    ::iwidgets::entryfield $selectionFrame.ds9Path -labeltext "DS9 Path" -labelfont g_titleFont -width 30 \
                                             -textvariable fvPref::ds9LibPath
    ::iwidgets::entryfield $selectionFrame.xpaPath -labeltext "XPA Path" -labelfont g_titleFont -width 30 \
                                          -textvariable fvPref::xpaLibPath
    button $selectionFrame.ds9PathBrowse -text "Browse" -font g_titleFont
    button $selectionFrame.xpaPathBrowse -text "Browse" -font g_titleFont

    bind $selectionFrame.ds9PathBrowse <ButtonRelease-1> [itcl::code $this _browseDirectory %W]
    bind $selectionFrame.xpaPathBrowse <ButtonRelease-1> [itcl::code $this _browseDirectory %W]

    grid $selectionFrame.ds9Path -row 0  -column 0  -columnspan 4 -sticky news
    grid $selectionFrame.xpaPath -row 1  -column 0  -columnspan 4 -sticky news

    grid $selectionFrame.ds9PathBrowse -row 0  -column 6  -sticky news
    grid $selectionFrame.xpaPathBrowse -row 1  -column 6  -sticky news
    grid $selectionFrame -row 0  -column 0 -columnspan 6 -rowspan 2 -sticky news
    
    #grid columnconfigure $top.selection 1 -weight 1
    #grid rowconfigure $top.selection 0 -weight 1

    frame $top.action 
    button $top.action.ok     -text "OK" -font g_titleFont -command { destroy .askDirectorySetup }
    
    grid $top.action.ok     -row 0 -column 1 -sticky w
    grid $top.action -row 2 -column 0 -columnspan 6 -sticky news
}

##########################################################
#############      Private Functions      ################
##########################################################


##################
#
# Setup Page: fv
#
##################

itcl::body fvPreferences::_buildPage_App { w } {
   global tcl_platform
   global g_titleFont

   checkbutton $w.fits  -text "List FITS files only"  \
         -font g_titleFont \
         -variable fvPref::ifDispFitsOnly

   if { $tcl_platform(platform)!="macintosh" } {
      checkbutton $w.mngr  -text "Show desktop manager"  \
            -variable fvPref::ifUseManager \
            -font g_titleFont \
            -command ".fvwinkeeper updateVisibility"
   }

   checkbutton $w.filedpv  -text "Kepp File Dialog Panel visible"  \
         -font g_titleFont \
         -variable fvPref::keepFileDialogVisible

   ::iwidgets::radiobox $w.windows -labeltext "Window Management" -labelfont g_titleFont

   $w.windows add NONE \
         -text "Leave existing windows open when opening new one"
   $w.windows buttonconfigure 0 -variable fvPref::winManagement -value "Keep"
   $w.windows add HIDE \
         -text "Hide existing windows when opening new one"
   $w.windows buttonconfigure 1 -variable fvPref::winManagement -value "Hide"
   $w.windows add CLOSE \
         -text "Close existing windows when opening new one"
   $w.windows buttonconfigure 2 -variable fvPref::winManagement -value "Close"

   option add *textBackground white
   ::iwidgets::entryfield $w.browser -labeltext "Html Browser:" -labelfont g_titleFont -width 15 \
                                     -textvariable fvPref::BrowserSetting

   grid $w.fits     -row 1  -column 1  -sticky w   -pady $_padYAmnt \
         -padx $_padXAmnt
   if { $tcl_platform(platform)!="macintosh" } {
      grid $w.mngr  -row 2  -column 1  -sticky w   -pady $_padYAmnt \
            -padx $_padXAmnt
   }
   grid $w.filedpv -row 3  -column 1  -sticky w   -pady $_padYAmnt \
         -padx $_padXAmnt

   grid $w.windows  -row 4  -column 1  -sticky we  -pady $_padYAmnt

   if { !$::isWin } {
      grid $w.browser -row 5  -column 1  -sticky w -pady $_padYAmnt
   }

   grid columnconfig $w 1 -weight 1
}

##################
#
# Setup Page: Keywords
#
##################

itcl::body fvPreferences::_buildPage_Keywords { w } {
   global g_titleFont
   checkbutton $w.hist  -text "Write history keyword after modification" \
         -font g_titleFont \
         -variable fvPref::ifWriteHisKey
   checkbutton $w.format  -text "Auto-reformat keywords"  \
         -font g_titleFont \
         -variable fvPref::ifAutoReformatKeys
   checkbutton $w.protect  -text "Protect required keywords"  \
         -font g_titleFont \
         -variable fvPref::ifProtectedKeys

   ::iwidgets::radiobox $w.chksm -labeltext "Checksum Updates" -labelfont g_titleFont

   $w.chksm add ALWAYS -text "Always update"
   $w.chksm buttonconfigure 0 -variable fvPref::ifAutoUpdateChecksum -value 2
   $w.chksm add EXIST  -text "Update only if they exist"
   $w.chksm buttonconfigure 1 -variable fvPref::ifAutoUpdateChecksum -value 1
   $w.chksm add NEVER  -text "Do not update"
   $w.chksm buttonconfigure 2 -variable fvPref::ifAutoUpdateChecksum -value 0

   grid $w.hist     -row 1  -column 1  -sticky w  -pady $_padYAmnt \
         -padx $_padXAmnt
   grid $w.format   -row 2  -column 1  -sticky w  -pady $_padYAmnt \
         -padx $_padXAmnt
   grid $w.protect  -row 3  -column 1  -sticky w  -pady $_padYAmnt \
         -padx $_padXAmnt
   grid $w.chksm    -row 4  -column 1  -sticky we -pady $_padYAmnt
   grid columnconfig $w 1 -weight 1
}

##################
#
# Setup Page: Table
#
##################

itcl::body fvPreferences::_buildPage_Tables { w } {
   global g_titleFont

   ::iwidgets::radiobox $w.justify -labeltext "ASCII Column Justification" -labelfont g_titleFont

   $w.justify add LFT -text Left
   $w.justify buttonconfigure 0 -variable fvPref::ifLeftJustifyString -value 1
   $w.justify add RGT -text Right
   $w.justify buttonconfigure 1 -variable fvPref::ifLeftJustifyString -value 0

   grid $w.justify  -row 2  -column 1 -sticky ew  -pady $_padYAmnt
   grid columnconfig $w 1 -weight 1
}

##################
#
# Setup Page: Graph
#
##################

itcl::body fvPreferences::_buildPage_Graphs { w } {
   global g_hasSAOtng g_hasDS9
   global g_titleFont
   global tcl_platform

   # Auto Plot Primary
   
   set viewerWindow $w.viewer

   checkbutton $w.auto  -text "Auto-Plot Primary Image" \
         -font g_titleFont \
         -variable fvPref::ifAutoPlotPrimary

   # WCS info
   
   checkbutton $w.wcs  -text "Use WCS Info" -variable fvPref::ifWCSInfo -font g_titleFont

   # Image Viewer

   ::iwidgets::radiobox $w.viewer -labeltext "Image Viewer" -labelfont g_titleFont

   $w.viewer add DS9 -text DS9
   $w.viewer buttonconfigure 0 -variable fvPref::imgDisplayer -value DS9 -command [itcl::code $this _toggleChoice]
   $w.viewer add POW -text POW 
   $w.viewer buttonconfigure 1 -variable fvPref::imgDisplayer -value POW -command [itcl::code $this _toggleChoice]
   #$w.viewer add SAO -text SAOtng
   #$w.viewer buttonconfigure 2 -variable fvPref::imgDisplayer -value SAOTNG -command [itcl::code $this _toggleChoice]

   if {$tcl_platform(platform) == "windows"} { 
      # DS9 entry

      option add *textBackground white

      ::iwidgets::entryfield $w.viewer.ds9Path -labeltext "DS9 Path" -labelfont g_titleFont -width 30 \
                                               -textvariable fvPref::ds9LibPath
      ::iwidgets::entryfield $w.viewer.xpaPath -labeltext "XPA Path" -labelfont g_titleFont -width 30 \
                                               -textvariable fvPref::xpaLibPath
      button $w.viewer.ds9PathBrowse -text "Browse" -font g_titleFont
      button $w.viewer.xpaPathBrowse -text "Browse" -font g_titleFont

      bind $w.viewer.ds9PathBrowse <ButtonRelease-1> [itcl::code $this _browseDirectory %W]
      bind $w.viewer.xpaPathBrowse <ButtonRelease-1> [itcl::code $this _browseDirectory %W]
   
      if { $fvPref::imgDisplayer == "DS9" } {
         grid $w.viewer.ds9Path -row 1  -column 2  -columnspan 4 -sticky swe
         grid $w.viewer.xpaPath -row 1  -column 2  -columnspan 4 -sticky nwe
   
         grid $w.viewer.ds9PathBrowse -row 1  -column 6  -sticky se
         grid $w.viewer.xpaPathBrowse -row 1  -column 6  -sticky ne
      }

      update idletasks
   }

   if { ! $g_hasDS9 } {
       $w.viewer buttonconfigure 0 -state disabled
   }

   if { ! $g_hasSAOtng } {
      # $w.viewer buttonconfigure 2 -state disabled
   }

   # Default graph display size
   
   ::iwidgets::optionmenu $w.graphsize -labeltext "Graph Size:" -labelpos w \
         -command [itcl::code $this _setGraphSize $w.graphsize] -labelfont g_titleFont -font g_titleFont

   set _graphDefaultWndw $w.graphsize
   foreach dim $fvPref::GraphDefaultSizeList {
       $w.graphsize insert end $dim
   }
   $w.graphsize insert end "-----"
   $w.graphsize insert end "Configure.."
   $w.graphsize disable "-----"
   _setGraphSize $w.graphsize $fvPref::graphDispSize

   grid $w.auto       -row 1  -column 1  -sticky w  -pady $_padYAmnt \
         -padx $_padXAmnt
   grid $w.wcs        -row 2  -column 1  -sticky w  -pady $_padYAmnt \
         -padx $_padXAmnt
   grid $w.viewer     -row 3  -column 1  -sticky we -pady $_padYAmnt
   grid $w.graphsize  -row 4  -column 1  -sticky w  -pady $_padYAmnt \
         -padx $_padXAmnt

   grid columnconfig $w 1 -weight 1
}

itcl::body fvPreferences::_toggleChoice { } {
     global isWin

     if { $isWin } {
        set wndw $viewerWindow

        if { $fvPref::imgDisplayer == "DS9" } {
           grid $wndw.ds9Path -row 1  -column 2  -columnspan 4 -sticky swe
           grid $wndw.xpaPath -row 1  -column 2  -columnspan 4 -sticky nwe
   
           grid $wndw.ds9PathBrowse -row 1  -column 6  -sticky se
           grid $wndw.xpaPathBrowse -row 1  -column 6  -sticky ne
           update idletasks
           
           scan [winfo geometry $wndw] "%dx%d+%d+%d" Pw Ph Px Py
           set width [expr [winfo reqwidth $wndw.ds9Path] + [winfo reqwidth $wndw.ds9PathBrowse]]
           scan [winfo geometry .fvpref] "%dx%d+%d+%d" Rw Rh Rx Ry
           if { $previousImgDisplayer != "DS9" } {
              catch { wm geometry .fvpref [expr ${width} + $Rw - $Pw * 5 / 7 ]x$Rh+$Rx+$Ry } err
           }
           update idletasks

        } else {
           
           scan [winfo geometry $wndw] "%dx%d+%d+%d" Pw Ph Px Py
           set width [expr ([winfo reqwidth $wndw.ds9Path] + [winfo reqwidth $wndw.ds9PathBrowse]) * 7 / 5]

           grid forget $wndw.ds9Path
           grid forget $wndw.xpaPath
   
           grid forget $wndw.ds9PathBrowse
           grid forget $wndw.xpaPathBrowse

           update idletasks
           scan [winfo geometry .fvpref] "%dx%d+%d+%d" Rw Rh Rx Ry

           if { $previousImgDisplayer == "DS9" } {
              catch { wm geometry .fvpref [expr $Rw - $width * 2 / 7 + 20]x$Rh+$Rx+$Ry } err
           }
           update idletasks
        }
        set previousImgDisplayer $fvPref::imgDisplayer
     }
}

##################
#
# Setup Page: Colors   grid columnconfig $w 1 -weight 1
#
##################

itcl::body fvPreferences::_buildPage_Colors { w } {
   global g_titleFont

   set row 1
   foreach lbl [list \
         Background: \
         Foreground: \
         "Active Background:" \
         "Active Foreground:" \
         "Check Button:"] \
         var [list \
         globalBg \
         globalFg \
         activeBg \
         activeFg \
         checkBBg] {
      label $w.clab$row -text $lbl -font g_titleFont
      button $w.cbtn$row -textvariable fvPref::${var}Color -width 7 \
            -command [itcl::code $this _changeColor $var] -font g_titleFont
      grid $w.clab$row -row $row -column 1 -sticky w  -pady 0 \
            -padx $_padXAmnt
      grid $w.cbtn$row -row $row -column 2 -sticky w  -pady 0 \
            -padx 0
      incr row
   }
   set currentFontSize [string trim [font configure g_titleFont -size] "-"]
   label $w.fontSizeLabel -text "Font Size:" -font g_titleFont
   tixControl $w.fontsize -label "" -integer true -min 1 \
                                         -option {
                                             font g_titleFont \
                                         } \
                                         -max 32 \
                                         -value $currentFontSize \
                                         -variable ::fvPref::FontSize

  #set yearSubWidget [$topWndw.${opt}Year subwidget entry]
  #set year [$yearSubWidget get]


   grid $w.fontSizeLabel -row $row -column 1 -sticky w -pady 0 -padx $_padXAmnt
   grid $w.fontsize -row $row -column 2 -sticky w  -pady 0 -padx 0

   grid columnconfigure $w 2 -weight 1
}

##################
# Setup Page: Library Path
##################

itcl::body fvPreferences::_setGraphSize { menu {val ""} } {
   if { $val=="" } {
      # Update parameter for menu selection

      set value [$menu get]
      if { $value=="Configure.." } {
         set val [_getGraphSize]
         _setGraphSize $menu $val
      } else {
         set fvPref::graphDispSize [split $value x]
      }

   } else {

      # Set menu selection to supplied value
      set value [join $val x]
      if { [catch {$menu index $value}] } {
         $menu insert 0 $value
      }
      $menu select $value
      set fvPref::graphDispSize $val

   }
}

itcl::body fvPreferences::_addNewSize {} {
    global tcl_platform

    if { $tcl_platform(platform) != "windows" } {  
       set w [.gSize childsite]
    } else {
       set w .gSize
    }

    set fa $w.add
    set fd $w.delete
    set newDim [format "%sx%s" [$fa.wide get] [$fa.hgte get]]

    set idx [lsearch -exact $fvPref::GraphDefaultSizeList $newDim]
    if { $idx >= 0 } return

    # use _deleteCheckedSize to update the list
    set ::fvPref::GraphDefaultSizeList [linsert $fvPref::GraphDefaultSizeList 0 $newDim]
    # set ::fvPref::GraphDefaultSizeList [lsort $fvPref::GraphDefaultSizeList]

    set prev_checkList $sizeCheckList
    set sizeCheckList {}

    _deleteCheckedSize add

    # now to select/deselect the item user already made
    set sizeCheckList $prev_checkList
    for { set i 0 } { $i < [llength $fvPref::GraphDefaultSizeList] } { incr i } {
        set dim [lindex $fvPref::GraphDefaultSizeList $i] 
        set checkIdx [lsearch -exact $sizeCheckList $dim]
        if { $checkIdx >= 0 } {
           $fd.$i select
        }
    }

    $_graphDefaultWndw select $newDim
    set entry {}
    lappend entry [$fa.wide get]
    lappend entry [$fa.hgte get]
    set ::fvPref::graphDispSize $entry
    destroy $fd.deleteB

    button $fd.deleteB -text "Delete" -command [itcl::code $this _deleteCheckedSize] -font [list Helvetica 8]
    grid $fd.deleteB -row [expr [llength $fvPref::GraphDefaultSizeList] + 1] -column 2 -sticky e 
}

itcl::body fvPreferences::_toggleCheckedSizeState { dim } {
    set idx [lsearch $sizeCheckList $dim]
    if { $idx < 0 } {
       lappend sizeCheckList $dim
    } else {
       set sizeCheckList [lreplace $sizeCheckList $idx $idx]
    }
}

itcl::body fvPreferences::_deleteCheckedSize { {op "delete"} } {
    global tcl_platform

    set selectGraphSize $fvPref::graphDispSize
    if { $tcl_platform(platform) != "windows" } {  
       set w [.gSize childsite]
    } else {
       set w .gSize
    }
    set fd $w.delete
    set fa $w.add
    for { set i 0 } { $i < [llength $fvPref::GraphDefaultSizeList] } { incr i } {
        catch { grid forget $fd.$i }
        catch { destroy $fd.$i }
    }

    $_graphDefaultWndw delete 0 end

    set newList {}

    foreach dim $fvPref::GraphDefaultSizeList {
       set idx [lsearch -exact $sizeCheckList $dim]

       if { $idx < 0 } {
          lappend newList $dim
       }
    }

    set ::fvPref::GraphDefaultSizeList $newList
    set sizeCheckList {}

    for { set i 0 } { $i < [llength $fvPref::GraphDefaultSizeList] } { incr i } {
        set dim [lindex $fvPref::GraphDefaultSizeList $i] 
        checkbutton $fd.$i -font g_titleFont -text $dim \
                           -command [itcl::code $this _toggleCheckedSizeState $dim]
        set checkIdx [lsearch -exact $sizeCheckList $dim]
        $fd.$i deselect
        grid $fd.$i -row [expr $i + 1] -column 0 -sticky w 
    }

    for {set i [expr [llength $fvPref::GraphDefaultSizeList] - 1] } { $i >= 0 } {incr i -1 } {
        $_graphDefaultWndw insert 0 [lindex $fvPref::GraphDefaultSizeList $i]
    }

    $_graphDefaultWndw insert end "-----"
    $_graphDefaultWndw insert end "Configure.."
    $_graphDefaultWndw disable "-----"

    set ::fvPref::graphDispSize $selectGraphSize

    if { $op == "delete" } {
       # check to see if default size has been deleted
       set dim [format "%sx%s" [lindex $fvPref::graphDispSize 0] [lindex $fvPref::graphDispSize 1]]
       if { [llength $fvPref::GraphDefaultSizeList] <= 0 } {
          lappend ::fvPref::GraphDefaultSizeList "300x300"
          $fa.wide delete 0 end
          $fa.hgte delete 0 end
          $fa.wide insert 0 300
          $fa.hgte insert 0 300
          set entry {}
          lappend entry 300
          lappend entry 300
          set ::fvPref::graphDispSize $entry
          $_graphDefaultWndw insert 0 "300x300"
       } else {
          set idx [lsearch -exact $fvPref::GraphDefaultSizeList $dim]
          if { $idx < 0 } {
             # default size has been deleted
             # pick first one
             set token [split [lindex $fvPref::GraphDefaultSizeList 0] "x"]
             set width [lindex $token 0]
             set height [lindex $token 1]
             $fa.wide delete 0 end
             $fa.hgte delete 0 end
             $fa.wide insert 0 $width 
             $fa.hgte insert 0 $height
             set entry {}
             lappend entry $width
             lappend entry $height
             set ::fvPref::graphDispSize $entry
             $_graphDefaultWndw select [lindex $fvPref::GraphDefaultSizeList 0]
          }
       }
    }

  
}

itcl::body fvPreferences::_getGraphSize { } {
   global tcl_platform
   global g_titleFont

   if { $tcl_platform(platform) != "windows" } {  
      ::iwidgets::dialogshell .gSize -title "fv: Graph Size" -modality "application" 
      .gSize add OK     -text Save -command {fvPref save ; .gSize deactivate 1} -font g_titleFont
      .gSize add CANCEL -text Cancel -command {.gSize deactivate 0} -font g_titleFont
      .gSize default OK
      set w [.gSize childsite]
   } else {
      toplevel .gSize -class Dialog
      wm  title .gSize "fv: Preferences"
      frame .gSize.buts
      button .gSize.buts.save -text Save -command { fvPref save ; destroy .gSize } -font g_titleFont
      button .gSize.buts.cancel -text Cancel -command { destroy .gSize } -font g_titleFont
      set w .gSize
   }

   label $w.titleA -text "Available Graph Size" -font g_titleFont
   grid $w.titleA  -row 0 -column 0 -sticky w

   # add Delete option
   set fd [frame $w.delete -relief groove -width 500 -bd 3]
   for { set i 0 } { $i < [llength $fvPref::GraphDefaultSizeList] } { incr i } {
       set dim [lindex $fvPref::GraphDefaultSizeList $i] 
       checkbutton $fd.$i -font g_titleFont -text $dim \
                          -command [itcl::code $this _toggleCheckedSizeState $dim]
       $fd.$i deselect
       grid $fd.$i -row [expr $i + 1] -column 0 -sticky w 
   }


   button $fd.deleteB -text "Delete" -command [itcl::code $this _deleteCheckedSize] -font [list Helvetica 8]
   grid $fd.deleteB -row [expr $i + 1] -column 2 -sticky e 
   grid $fd  -row 1 -column 0 -rowspan [llength $fvPref::GraphDefaultSizeList] -sticky w -columnspan 3

   label $w.titleN -text "New Graph Size" -font g_titleFont
   grid $w.titleN -row [expr [llength $fvPref::GraphDefaultSizeList] + 1]  -column 0  -padx 3 -pady 3 -sticky w -columnspan 2

   # add ADD option
   set fa [frame $w.add -relief groove -bd 3]
   label $fa.wid -text "Width:" -font g_titleFont
   label $fa.hgt -text "Height:" -font g_titleFont
   entry $fa.wide -font g_titleFont
   entry $fa.hgte -font g_titleFont
   button $fa.addB -text "Add" -command [itcl::code $this _addNewSize] -font [list Helvetica 8]

   $fa.wide insert 0 [lindex $fvPref::graphDispSize 0]
   $fa.hgte insert 0 [lindex $fvPref::graphDispSize 1]

   grid $fa.wid   -row 1  -column 0  -padx 3 -pady 3 -sticky e
   grid $fa.hgt   -row 2  -column 0  -padx 3 -pady 3 -sticky e
   grid $fa.wide  -row 1  -column 1  -padx 3 -pady 3 -sticky w
   grid $fa.hgte  -row 2  -column 1  -padx 3 -pady 3 -sticky w
   grid $fa.addB  -row 3  -column 1  -padx 3 -pady 3 -sticky e
   grid $fa  -row [expr [llength $fvPref::GraphDefaultSizeList] + 4] -column 0 -rowspan 4 -sticky news -columnspan 2

   if { $tcl_platform(platform) != "windows" } {  
      set result [.gSize activate]
      set width  [$fa.wide get]
      set height [$fa.hgte get]
      destroy .gSize
   } else {
      grid .gSize.buts.save -in .gSize.buts -row 0 -column 0 -sticky e
      grid .gSize.buts.cancel -in .gSize.buts -row 0 -column 0 -sticky e
      grid .gSize.buts -row [expr [llength $fvPref::GraphDefaultSizeList] + 8] -column 0 -rowspan 4 -sticky news -columnspan 2
      set width  [$fa.wide get]
      set height [$fa.hgte get]
   }

   if { $tcl_platform(platform) != "windows" } {  
      if { $result!="" && $result } {
         if { $width > 19 && $height > 19 && $width < 2001 && $height < 2001 } {
            return [list $width $height]
         }
         tk_messageBox -type ok -icon error -parent .fvpref -title "Bad Size" \
               -message "The graph size entered does not fall into the range\
               20-2000.  Reverting to previous value."
      }
   }
   return $fvPref::graphDispSize
}



itcl::body fvPreferences::_changeColor {type} {
   
   switch $type {
      "globalBg" { set currColor $fvPref::globalBgColor }
      "globalFg" { set currColor $fvPref::globalFgColor }
      "activeBg" { set currColor $fvPref::activeBgColor }
      "activeFg" { set currColor $fvPref::activeFgColor }
      "checkBBg" { set currColor $fvPref::checkBBgColor }
      default    { set currColor "black" }
   }

   set tmpColor [tk_chooseColor -initialcolor $currColor]
   if { $tmpColor == "" } return 
   
   switch $type {
      "globalBg" {
         option add *Background          $tmpColor
         option add *HighlightBackground $tmpColor
         set fvPref::globalBgColor       $tmpColor
      }
      "globalFg" {
         option add *Foreground          $tmpColor
         set fvPref::globalFgColor       $tmpColor
      }
      "activeFg" {
         option add *activeForeground    $tmpColor
         set fvPref::activeFgColor       $tmpColor
      }
      "activeBg" {
         option add *activeBackground    $tmpColor
         set fvPref::activeBgColor       $tmpColor
      }
      "checkBBg" {
         option add *selectColor         $tmpColor
         set fvPref::checkBBgColor       $tmpColor
      }
      default {error "Unknown color type"}
   }

   # find all the windows and change their color
   set tmpWins [winfo children .]
   foreach i $tmpWins {
      _changeWindowColors $i $type
   }
}


itcl::body fvPreferences::_changeWindowColors {win type} {

   switch $type {
      "globalBg" {
         catch {$win configure -background $fvPref::globalBgColor}
         catch {$win configure -highlightbackground $fvPref::globalBgColor}
         if { [winfo class $win] == "Checkbutton" } {
            catch {$win configure -activebackground $fvPref::globalBgColor}
         }
      }
      "globalFg" {
         Notebook:config $note -textFg $fvPref::globalFgColor
         catch {$win configure -foreground $fvPref::globalFgColor}	    
      }
      "activeFg" {
         catch {$win configure -activeforeground $fvPref::activeFgColor}
      }
      "activeBg" {
         catch {$win configure -activebackground $fvPref::activeBgColor}
      }
      "checkBBg" {
         catch {$win configure -selectcolor $fvPref::checkBBgColor}
      }
      default {error "Unknown bg type"}
   }
   
   set tmpChildren [winfo children $win]
   if { $tmpChildren == "" } {
      return
   } else {
      foreach i $tmpChildren {
         _changeWindowColors $i $type 
      }
   }
}

itcl::body fvPreferences::_browseDirectory { win } {

     if { [string first "ds9" $win] >= 0 } {
       set win [winfo parent $win].ds9Path
     } else {
       set win [winfo parent $win].xpaPath
     }

     set _currentBrowseDirectoryEntry $win

     if [winfo exist .browseDirectorySetup] {
        wm deiconify .browseDirectorySetup
        .browseDirectorySetup.directory.saveInEntry delete 0 end
        if { [$win get] != "" } {
           .browseDirectorySetup.directory.saveInEntry insert end [$win get]
           set directoryValue [$win get]
        } else {
           .browseDirectorySetup.directory.saveInEntry insert end [pwd]
           set directoryValue [pwd]
        }
        .browseDirectorySetup.directory.directoryTree configure -value $directoryValue

     } else {

        toplevel .browseDirectorySetup 
        wm title .browseDirectorySetup "Select Directory"

        grid rowconfigure .browseDirectorySetup 2 -weight 1
        grid columnconfigure .browseDirectorySetup 0 -weight 1
        grid columnconfigure .browseDirectorySetup 1 -weight 1

        frame .browseDirectorySetup.directory             -bd 2 -relief ridge
        label .browseDirectorySetup.directory.dirLabel    -text "Directory: " -font g_titleFont
        entry .browseDirectorySetup.directory.saveInEntry -width 35 -bg white -font g_titleFont

        .browseDirectorySetup.directory.saveInEntry delete 0 end
        if { [$win get] != "" } {
           .browseDirectorySetup.directory.saveInEntry insert end [$win get]
           set directoryValue [$win get]
        } else {
           .browseDirectorySetup.directory.saveInEntry insert end [pwd]
           set directoryValue [pwd]
        }

        tixDirTree .browseDirectorySetup.directory.directoryTree -value $directoryValue \
                   -browsecmd [itcl::code $this _selectDirectory] \
                   -options { \
                        hlist.foreground black \
                        hlist.background white \
                        hlist.font g_titleFont \
                        hlist.width 40 \
                   }

        set _selectedDirectoryName $directoryValue

        grid .browseDirectorySetup.directory               -row 2 -column 0 -sticky news -columnspan 5 -rowspan 10
        grid .browseDirectorySetup.directory.dirLabel      -row 2 -column 0 -sticky nw
        grid .browseDirectorySetup.directory.saveInEntry   -row 2 -column 1 -sticky new
        grid .browseDirectorySetup.directory.directoryTree -row 3 -column 0 -columnspan 5 -rowspan 10 -sticky news

        grid columnconfigure .browseDirectorySetup.directory 1 -weight 1
        grid rowconfigure .browseDirectorySetup.directory 3 -weight 1

        frame .browseDirectorySetup.action 
        button .browseDirectorySetup.action.ok     -text "OK" -font g_titleFont \
                                                   -command [itcl::code $this save]
        label .browseDirectorySetup.action.blanklabel -text " " -font g_titleFont
        button .browseDirectorySetup.action.cancel -text "Cancel" -font g_titleFont \
                                                   -command [itcl::code $this _selectDirectory true]
        
        grid .browseDirectorySetup.action -row 18 -column 0 -columnspan 6 -sticky news
        grid .browseDirectorySetup.action.ok     -row 0 -column 1 -sticky w
        grid .browseDirectorySetup.action.blanklabel -row 0 -column 2 -columnspan 2 -sticky news
        grid .browseDirectorySetup.action.cancel -row 0 -column 4 -sticky e
     }

}

itcl::body fvPreferences::_selectDirectory { dir } {
     set _selectedDirectoryName [_correctPath $dir]

     if { $_previousDirectory == "" } {
        set _previousDirectory $dir
     }

     if { $dir == "true" } {
        wm deiconify .browseDirectorySetup
        wm withdraw .browseDirectorySetup
        set _selectedDirectoryName $_previousDirectory
     }

     if { $_selectedDirectoryName != "true" } {
        $_currentBrowseDirectoryEntry delete 0 end
        $_currentBrowseDirectoryEntry insert end $_selectedDirectoryName

        .browseDirectorySetup.directory.saveInEntry delete 0 end 
        .browseDirectorySetup.directory.saveInEntry insert end $_selectedDirectoryName
     }
}

itcl::body fvPreferences::_correctPath { path } {
     set newStr ""
     set path [string trim $path "{}"]

     for {set i 0} {$i < [string length $path]} {incr i} {
         if { [string range $path $i $i] == "\\" } {
            set newStr [format "%s/" $newStr]
         } else {
            set newStr [format "%s%s" $newStr [string range $path $i $i]]
         }
     }
     set newStr [string trimleft $newStr "/"]
     return $newStr
}

