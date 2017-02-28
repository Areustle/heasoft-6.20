# When this class is initialized in FVFtool.tcl, the constructor will create a
# ::fguiPref namespace with various variable names

itcl::class fguiPreferences {

   # Create and initialize fguiPref namespace
   constructor { args } {}
   destructor { }

   public {
      method save  { }
      method edit  { {page "fgui"} }
      method close { }
      method raisePage { page }
      method _askDirectorySetup {}
   }

   private {
      method _read {}

      method _buildPage_App      { w }
      method _buildPage_Web      { w }
      method _buildPage_Colors   { w }

      method _changeColor  { type }
      method _changeWindowColors { win type }

      variable note
      variable _FGUIRC
      variable _padXAmnt 10
      variable _padYAmnt 3
      variable _tabLabels [list App Web Colors]
   }
}

itcl::body fguiPreferences::constructor { args } {
   global tcl_platform env FITSVIEWER_LIBRARY
   global g_titleFont

   switch $tcl_platform(platform) {
      "macintosh" {
         set _FGUIRC [file join $env(PREF_FOLDER) "fgui Preferences"]
      }
      "windows" {
         set  FITSVIEWER_LIBRARY $env(FITSVIEWER_LIBRARY)
         set _FGUIRC [file join $FITSVIEWER_LIBRARY "fgui.ini"]
         # Pan Chai: uncomment when XPA is available in Windows
         # lappend _tabLabels Library
      }
      default {
         set _FGUIRC "~/.fguirc"
      }
   }

   option add *fguiPreference.font g_titleFont
   # Create and initialize fguiPref namespace

   namespace eval ::fguiPref { 
      variable globalBgColor        #cccccc
      variable globalFgColor        black  
      variable activeBgColor        #eeeeee
      variable activeFgColor        black  
      variable checkBBgColor        red
      variable FtoolsHeadasRoot     "None Defined"
      variable BrowserSetting       "None Defined"
   }

   # Override some defaults based on enviroment

   set tmpBg [option get . background Background]
   if { $tmpBg != ""} {
      set fguiPref::globalBgColor $tmpBg
   }
   set tmpFg [option get . foreground Foreground]
   if { $tmpFg != ""} {
      set fguiPref::globalFgColor $tmpFg
   }

   if { $tcl_platform(platform)=="macintosh" } {
      set fguiPref::globalBgColor "#eeeeee"
      set fguiPref::activeBgColor "#cccccc"
   }

   if { $tcl_platform(platform) != "windows" } {
      set fguiPref::BrowserSetting "None Defined"
   }

   #  Read in the preferences file
   _read
}

itcl::body fguiPreferences::destructor { } {
   if { [winfo exists .fguipref] } {
      destroy .fguipref
   }
}


itcl::body fguiPreferences::edit { {page "fgui"} } {
   global tcl_platform
   global g_titleFont
   
   if { [winfo exists .fguipref] } {
      if {$tcl_platform(platform)!= "windows"} { 
        .fguipref activate
        fguiNotebook:raise [.fguipref childsite].notebook $page
        # since the notebook does not actual destroy, we need this line to update graph size
      } else {
        focus .fguipref
        raise .fguipref        
        fguiNotebook:raise .fguipref.notebook $page
      }        
      return
   }

   ###########
   # Setup Dialog Box
   ###########
   if { $tcl_platform(platform) != "windows" } {  
    ::iwidgets::dialogshell .fguipref -title "fv: Preferences" -modality "none"
    .fguipref add Save -text "Save" -command [itcl::code $this save] -font g_titleFont
    .fguipref add Help -text "Help" -command "hhelp preferences" -font g_titleFont
    .fguipref add Exit -text "Exit" -command [itcl::code $this close] -font g_titleFont
    .fguipref default Exit
   } else {
     toplevel .fguipref -class Dialog
     wm  title .fguipref "fgui: Preferences"
     frame .fguipref.buts
     button .fguipref.buts.save -text Save -command [itcl::code $this save] -font g_titleFont
     button .fguipref.buts.help -text Help -command "hhelp preferences" -font g_titleFont
     button .fguipref.buts.exit -text Exit -command [itcl::code $this close] -font g_titleFont
     pack .fguipref.buts.save -side left -padx 30 -pady 4          
     pack .fguipref.buts.help -side left -padx 30  -pady 4        
     pack .fguipref.buts.exit -side right -padx 30 -pady 4
                
   }

   bind .fguipref <<CloseWindow>> [itcl::code $this close]

   ###########
   # Setup Tabbed fguiNotebook Widget
   ###########

   if {$tcl_platform(platform) != "windows" } {   
      set note "[.fguipref childsite].notebook"
   } else {
      set note .fguipref.notebook
   } 

   catch { fguiNotebook:create $note -pages $_tabLabels -pad 8 } err

   foreach p $_tabLabels {
      if { $tcl_platform(platform) == "windows" && $p == "Web" } continue
      eval _buildPage_$p [fguiNotebook:frame $note $p]
   }

   pack $note -fill both -expand 1   
    
   ###########
   # Display dialog
   ###########

   fguiNotebook:raise $note $page
   fguiNotebook:raise.page $note [lsearch $_tabLabels "Graphs"]
   fguiNotebook:resize $note
   if {$tcl_platform(platform) != "windows" } {    
      .fguipref activate
   } else {
      pack .fguipref.buts 
   }
}

itcl::body fguiPreferences::raisePage { page } {

   fguiNotebook:raise.page $note [lsearch $_tabLabels $page]
   fguiNotebook:resize $note
     
}

itcl::body fguiPreferences::close { } {
   global tcl_platform
   if { [winfo exists .fguipref] } {
      if {$tcl_platform(platform) == "windows" } { 
        destroy .fguipref
      } else {
        .fguipref deactivate
      }
   }
}

itcl::body fguiPreferences::_read { } {
   global tcl_platform

   if { ![catch { set rcFile [open $_FGUIRC r] } ] } {
      while { [gets $rcFile line] != -1} {
         set entry [lindex $line 0]
         set value [lindex $line 1]
         if { [string length [string trim $entry]] <= 0 } continue
         switch -glob -- $entry {
            "Background" {	
               set fguiPref::globalBgColor $value
            }
            "Foreground" {
               set fguiPref::globalFgColor $value
            }
            "ActiveBackground" {
               set fguiPref::activeBgColor $value
            }
            "ActiveForeground" {
               set fguiPref::activeFgColor $value
            }
            "CheckButtonColor" {
               set fguiPref::checkBBgColor $value
            }
            "FtoolsHeadasRoot" {
               set fguiPref::FtoolsHeadasRoot $value
            }
            "BrowserSetting" {
               set fguiPref::BrowserSetting "$value"
            }
            "#*" {
            }
            default {
               if [info exist fguiPref::$entry] {
                  set fguiPref::$entry $value
               }
            }
         }
      }			 
      ::close $rcFile
   }

   option add *Background          $fguiPref::globalBgColor
   option add *Foreground          $fguiPref::globalFgColor
   option add *HighlightBackground $fguiPref::globalBgColor
   option add *activeForeground    $fguiPref::activeFgColor
   option add *activeBackground    $fguiPref::activeBgColor
   option add *selectForeground    $fguiPref::activeFgColor
   option add *selectBackground    $fguiPref::activeBgColor
   option add *selectColor         $fguiPref::checkBBgColor

}

itcl::body fguiPreferences::save {} {
   global tcl_platform

# check if _FGUIRC file is there
   catch { [file delete -force $_FGUIRC]} 

   if { [catch {set rcFile [open $_FGUIRC w]} err] == 1} {
      error "Unable to open $_FGUIRC to write"
      return 
   }

   puts $rcFile "\# Please don't change this file. It's auto generated by fgui"
   puts $rcFile "Background           $fguiPref::globalBgColor"
   puts $rcFile "Foreground           $fguiPref::globalFgColor"
   puts $rcFile "ActiveBackground     $fguiPref::activeBgColor"
   puts $rcFile "ActiveForeground     $fguiPref::activeFgColor"
   puts $rcFile "CheckButtonColor     $fguiPref::checkBBgColor"
   puts $rcFile "FtoolsHeadasRoot     $fguiPref::FtoolsHeadasRoot"

   if { [info exists ::fguiPref::BrowserSetting] && \
        [string trim $fguiPref::BrowserSetting] != "None Defined" && \
        $tcl_platform(platform) != "windows" } {
      puts $rcFile "BrowserSetting       $fguiPref::BrowserSetting"
   }

   ::close $rcFile

# puts "Options saved in $_FGUIRC"
}

##########################################################
#############      Private Functions      ################
##########################################################


##################
#
# Setup Page: Web
#
##################

itcl::body fguiPreferences::_buildPage_Web { w } {
   global tcl_platform
   global g_titleFont

   option add *textBackground white
   ::iwidgets::entryfield $w.browser -labeltext "Html Browser:" -labelfont g_titleFont -width 15 \
                                     -textvariable fguiPref::BrowserSetting

   grid $w.browser -row 5  -column 1  -sticky w -pady $_padYAmnt
   grid columnconfig $w 1 -weight 1
}

##################
#
# Setup Page: File Selection
#
##################

itcl::body fguiPreferences::_buildPage_App { w } {
   global tcl_platform
   global g_titleFont

   ::iwidgets::entryfield $w.headas -labeltext "Headas Root:" -labelfont g_titleFont -width 15 \
                                     -textvariable fguiPref::FtoolsHeadasRoot

   grid $w.headas -row 1  -column 1  -sticky w   -pady $_padYAmnt -padx $_padXAmnt

   grid columnconfig $w 1 -weight 1
}

##################
#
# Setup Page: Colors   grid columnconfig $w 1 -weight 1
#
##################

itcl::body fguiPreferences::_buildPage_Colors { w } {
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
      button $w.cbtn$row -textvariable fguiPref::${var}Color -width 7 \
            -command [itcl::code $this _changeColor $var] -font g_titleFont
      grid $w.clab$row -row $row -column 1 -sticky w  -pady 0 \
            -padx $_padXAmnt
      grid $w.cbtn$row -row $row -column 2 -sticky w  -pady 0 \
            -padx 0
      incr row
   }
   grid columnconfigure $w 2 -weight 1
}

itcl::body fguiPreferences::_changeColor {type} {
   
   switch $type {
      "globalBg" { set currColor $fguiPref::globalBgColor }
      "globalFg" { set currColor $fguiPref::globalFgColor }
      "activeBg" { set currColor $fguiPref::activeBgColor }
      "activeFg" { set currColor $fguiPref::activeFgColor }
      "checkBBg" { set currColor $fguiPref::checkBBgColor }
      default    { set currColor "black" }
   }

   set tmpColor [tk_chooseColor -initialcolor $currColor]
   if { $tmpColor == "" } return 
   
   switch $type {
      "globalBg" {
         option add *Background          $tmpColor
         option add *HighlightBackground $tmpColor
         set fguiPref::globalBgColor       $tmpColor
      }
      "globalFg" {
         option add *Foreground          $tmpColor
         set fguiPref::globalFgColor       $tmpColor
      }
      "activeFg" {
         option add *activeForeground    $tmpColor
         set fguiPref::activeFgColor       $tmpColor
      }
      "activeBg" {
         option add *activeBackground    $tmpColor
         set fguiPref::activeBgColor       $tmpColor
      }
      "checkBBg" {
         option add *selectColor         $tmpColor
         set fguiPref::checkBBgColor       $tmpColor
      }
      default {error "Unknown color type"}
   }

   # find all the windows and change their color
   set tmpWins [winfo children .]
   foreach i $tmpWins {
      _changeWindowColors $i $type
   }
}


itcl::body fguiPreferences::_changeWindowColors {win type} {

   switch $type {
      "globalBg" {
         catch {$win configure -background $fguiPref::globalBgColor}
         catch {$win configure -highlightbackground $fguiPref::globalBgColor}
         if { [winfo class $win] == "Checkbutton" } {
            catch {$win configure -activebackground $fguiPref::globalBgColor}
         }
      }
      "globalFg" {
         fguiNotebook:config $note -textFg $fguiPref::globalFgColor
         catch {$win configure -foreground $fguiPref::globalFgColor}	    
      }
      "activeFg" {
         catch {$win configure -activeforeground $fguiPref::activeFgColor}
      }
      "activeBg" {
         catch {$win configure -activebackground $fguiPref::activeBgColor}
      }
      "checkBBg" {
         catch {$win configure -selectcolor $fguiPref::checkBBgColor}
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
