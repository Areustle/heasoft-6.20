itcl::class fvWinKeeper  {
    inherit itk::Toplevel

    constructor {args} {}
    destructor  {}

    private variable _namList ""
    private variable _winList ""
    private variable _typList ""
    private variable _extList ""
    private variable _objList ""
    private variable _tmpWinList ""
    private variable _tmpObjList ""
    private variable _tmpTypList ""

    public method register {win type fName extNum obj}
    public method signoff {win}
    public method intCheck {type}
    public method setCursor { cursor }
    public method actOn {win}
    public method updateMacWindows { }
    public method updateVisibility { }
    public method updateDisplayDeviceStatus {}

    private method _hide {}
    private method _clean {} 
    private method _refresh {}

}

itcl::body fvWinKeeper::constructor {args} {
   global isMac isWin
   global env
   global g_titleFont
   global xpaFlag
   
   wm withdraw $itk_interior

   if { ! $isMac } {
      itk_component add mainMenu {
         menu $itk_interior.menu -tearoff false -type tearoff -bd 0
      }
      wm title $itk_interior.menu "fv"
      wm geometry $itk_interior.menu +0+0

      set itk_interior $itk_component(mainMenu)
      
      $itk_component(mainMenu) add command  -label "New File..." \
            -font g_titleFont \
            -command createNewFITS
      $itk_component(mainMenu) add command  -label "Open File..." \
            -font g_titleFont \
            -command selFile 

      $itk_component(mainMenu) add command -label "SkyView..." \
            -font g_titleFont \
            -command {
                 global g_skyvflag
                 if ![info exists g_skyvflag ] {
                      set g_skyvflag 1
                      FVSkyview skyv
                 }
                 skyv getskyvfile
            }
      $itk_component(mainMenu) add command -label "Catalogs..." \
            -font g_titleFont \
            -command {
                 global g_skyvflag
                 if ![info exists g_skyvflag ] {
                      set g_skyvflag 1
                      FVSkyview skyv
                 }
                 skyv getskyvcat
            }
      $itk_component(mainMenu) add command -label "VizieR..." \
            -font g_titleFont \
            -command {
                 global g_vizier
                 if ![info exists g_vizier ] {
                      set g_vizier 1
                      FVVizier viz
                 }
                 viz selectVizier
            }
      if { [info exists env(FTOOLS)] || [info exists env(LHEASOFT)] } {  
            $itk_component(mainMenu) add command  -label "Run Ftool..." \
            -font g_titleFont \
            -command {
                  set text_ "The \"Run Ftool\" option has been removed from fv.\nInstead, run the \"fgui\" task that is distributed with ftools.\n\n\nThe ftools software is available from\n\nhttp://heasarc.gsfc.nasa.gov/ftools"

                  if {$isWin } {
                     tk_messageBox -icon info -type ok \
                                   -message $text_
                  } else {
                     if ![winfo exists .nomoreRunFtool] {
                        iwidgets::messagedialog .nomoreRunFtool -title "fv: \"Run Ftool\" disabled" \
                             -font g_titleFont \
                             -bitmap info \
                             -modality application
                     }
                     wm geom .nomoreRunFtool +[expr [winfo screenwidth .] / 3]+[expr [winfo screenheight .] / 2]
                     .nomoreRunFtool configure -text $text_
                     .nomoreRunFtool hide Cancel
                     .nomoreRunFtool activate
                  }
            }
      }

      $itk_component(mainMenu) add command -label "Connect to Hera..." \
            -font g_titleFont \
            -command {
                 global g_vizier
                 set fvExecutable [info nameofexecutable]
                 if { [string tolower [file tail $fvExecutable]] != "fv" && \
                      [string tolower [file tail $fvExecutable]] != "fv.exe" } {
                    # this is NOT one-click or Windows
                    set fvExecutable "fv"
                 }
                 catch { exec $fvExecutable -mod hera & }
            }

      $itk_component(mainMenu) add separator

      itk_component add displayFrame {
           menu $itk_interior.df -tearoff false
      }

      $itk_component(displayFrame) add radiobutton \
            -font g_titleFont \
            -variable [itcl::scope ::fvPref::imgDisplayer] \
            -selectcolor $fvPref::checkBBgColor  \
            -activeforeground black -activebackground $fvPref::globalBgColor \
            -label "DS9" -command { fvPref save }

      $itk_component(displayFrame) add radiobutton \
            -font g_titleFont \
            -variable [itcl::scope ::fvPref::imgDisplayer] \
            -label "POW" -command { fvPref save } \
            -selectcolor $fvPref::checkBBgColor \
            -activeforeground black -activebackground $fvPref::globalBgColor

      if { !$isWin } {
         if { $xpaFlag == "NOT_AVAILABLE" } {
            # $itk_component(displayFrame) entryconfigure 0 -state disabled
            set ::fvPref::imgDisplayer "POW"
            fvPref save
         }
      } else {
         $itk_component(displayFrame) entryconfigure 0 -state normal 
      }

      $itk_component(mainMenu) add cascade \
            -font g_titleFont \
	    -menu $itk_component(displayFrame) -label "Display Device"

      $itk_component(mainMenu) add separator

      $itk_component(mainMenu) add command -label "Hide All Windows" \
            -font g_titleFont \
            -command [itcl::code $this actOn all]

      itk_component add summarymenu {
         menu $itk_interior.sm -tearoff false
      }
      $itk_component(mainMenu) add cascade \
            -font g_titleFont \
	    -menu $itk_component(summarymenu) -label "File Summary"

      itk_component add headermenu {
         menu $itk_interior.hm -tearoff false
      }
      $itk_component(mainMenu) add cascade \
            -font g_titleFont \
	    -menu $itk_component(headermenu) -label "Header"

      itk_component add tablemenu {
         menu $itk_interior.tm -tearoff false
      }
      $itk_component(mainMenu) add cascade \
            -font g_titleFont \
	    -menu $itk_component(tablemenu) -label "Table"

      itk_component add itablemenu {
         menu $itk_interior.itm -tearoff false
      }
      $itk_component(mainMenu) add cascade \
            -font g_titleFont \
	    -menu $itk_component(itablemenu) -label "Image Table"

      itk_component add vtablemenu {
         menu $itk_interior.vtm -tearoff false
      }
      $itk_component(mainMenu) add cascade \
            -font g_titleFont \
	    -menu $itk_component(vtablemenu) -label "Vector Table"

      $itk_component(mainMenu) add separator
      $itk_component(mainMenu) add command -label "Preference" \
            -font g_titleFont \
            -command {fvPref edit}

      $itk_component(mainMenu) add separator
      $itk_component(mainMenu) add command -label "Clipboard" \
            -font g_titleFont \
            -command {fvClipBoard toggleView}
      
      $itk_component(mainMenu) add separator
      $itk_component(mainMenu) add command -label "Help" \
            -font g_titleFont \
            -command {hhelp deskTopManager}   

      $itk_component(mainMenu) add command -label "Quit" \
            -font g_titleFont \
            -command "doMenuEvent <<Quit>> ."

   } else {

      set itk_component(summarymenu) .mbar.wind.sm
      set itk_component(headermenu)  .mbar.wind.hm
      set itk_component(tablemenu)   .mbar.wind.tm
      set itk_component(vtablemenu)  .mbar.wind.vtm
      set itk_component(itablemenu)  .mbar.wind.itm

   }
   wm protocol $itk_interior WM_DELETE_WINDOW [itcl::code $this _hide]
   _refresh
}

itcl::body fvWinKeeper::destructor {} {
}

itcl::body fvWinKeeper::_clean {} {
    $itk_component(summarymenu) delete 0 last
    $itk_component(headermenu)  delete 0 last
    $itk_component(tablemenu)   delete 0 last
    $itk_component(vtablemenu)  delete 0 last
    $itk_component(itablemenu)  delete 0 last
}

itcl::body fvWinKeeper::_refresh {} {

    set hcount  0
    set tcount  0
    set vtcount 0
    set itcount 0
    set scount  0

    for {set i 0} {$i < [llength $_winList]} {incr i} {
	set win    [lindex $_winList  $i]
	set type   [lindex $_typList  $i]
	set name   [lindex $_namList  $i]
# starts with 0
	set ext    [expr [lindex $_extList  $i] -1] 
	
	switch $type {
	    "Header" {
		$itk_component(headermenu) add command -label ${name}\[$ext\] \
                    -font g_titleFont \
		    -command [itcl::code $this actOn $win] 
		incr hcount
	    } 
	    "Table" {
		$itk_component(tablemenu) add command -label ${name}\[$ext\] \
                    -font g_titleFont \
		    -command [itcl::code $this actOn $win]
		incr tcount
	    }
	    "Vector Table" {
		$itk_component(vtablemenu) add command -label ${name}\[$ext\] \
                    -font g_titleFont \
		    -command [itcl::code $this actOn $win]
		incr vtcount
	    }	    
	    "Image Table" {
		$itk_component(itablemenu) add command -label ${name}\[$ext\] \
                    -font g_titleFont \
		    -command [itcl::code $this actOn $win]
		incr itcount
	    }	 
	    "Highlight" {
		$itk_component(summarymenu) add command -label $name \
                    -font g_titleFont \
		    -command [itcl::code $this actOn $win]
		incr scount
	    }	 
	    default {
		error "Unknown type of window to manage: $type"
		return
	    }
	}
    }
    foreach {var menu} [list hcount header tcount table vtcount vtable \
                        itcount itable scount summary] {
        if { [expr $$var] == 0 } {
            $itk_component(${menu}menu) add command -label none \
                    -font g_titleFont \
                    -state disabled
        }
    }
}

itcl::body fvWinKeeper::updateMacWindows { } {
    _clean
    _refresh
}

itcl::body fvWinKeeper::actOn {win} {
    if {$win == "all"} {
	if {[llength $_winList ] == 0} {
	    set tmpwinlist ".fD"
	} else {
	    set tmpwinlist $_winList
	    lappend tmpwinlist ".fD"
	}
	foreach w $tmpwinlist {
	    wm withdraw $w
	}	
    } else {
	if { [winfo viewable $win] == 1} {
	    wm withdraw $win
	} else {
	    wm deiconify $win
	}	 
    }
}


itcl::body fvWinKeeper::setCursor { cursor } {
   foreach w $_winList {
      catch {$w configure -cursor $cursor}
   }
   $this configure -cursor $cursor
}

itcl::body fvWinKeeper::register {win type fName extNum obj} {
    
    set _tmpWinList $_winList
    set _tmpObjList $_objList
    set _tmpTypList $_typList

    catch {intCheck $type}
    
    _clean 

    if { [lsearch $_winList $win] == -1} {
	lappend _winList $win
	lappend _namList $fName
	lappend _typList $type
	lappend _extList $extNum
	lappend _objList $obj
    }

    _refresh
}

itcl::body fvWinKeeper::signoff {win} {
#   if { ![winfo exists $itk_component(mainMenu)] } return
    _clean
    set idx [lsearch $_winList $win]
    if { $idx !=-1} {
	set _winList [lreplace  $_winList $idx $idx]
	set _typList [lreplace  $_typList $idx $idx]
	set _namList [lreplace  $_namList $idx $idx]
	set _extList [lreplace  $_extList $idx $idx]
	set _objList [lreplace  $_objList $idx $idx]
    } 
    _refresh
    if { [llength $_winList]==0 } {
       event generate . <<CheckExit>> -when tail
    }
}

itcl::body fvWinKeeper::intCheck {t} {

    switch $fvPref::winManagement {
	"Keep" {
	    return
	}
	"Hide" {
           for {set i 0} { $i< [llength $_tmpWinList]} {incr i} {
              set win [lindex $_tmpWinList $i]
              set tmpName [lindex $_tmpTypList $i]
              if { $tmpName == $t } {
                 wm withdraw $win
              }
           }	
	}
	"Close" {
           for {set i 0} { $i< [llength $_tmpWinList]} {incr i} {
              set win [lindex $_tmpWinList $i]
              set tmpName [lindex $_tmpTypList $i]
              if { $tmpName == $t } {
                 event generate $win <<CloseWindow>>
              }
           }	
	}
	default {
	    puts "fv Error: unknown type of window handling"
	}
    }

}

itcl::body fvWinKeeper::updateDisplayDeviceStatus {} {
     # Pan Chai: this function will not be called until XPA is available in Windows
     global isWin
     global env
     if { $isWin } {
        if { [info exists ::fvPref::ds9LibPath] && \
             [string length ${::fvPref::ds9LibPath}] > 0 && \
             [file exists ${::fvPref::ds9LibPath}/ds9.exe] && \
             [info exists ::fvPref::xpaLibPath] && \
             [string length ${::fvPref::xpaLibPath}] > 0 && \
             [file exists ${::fvPref::xpaLibPath}] } {
           $itk_component(displayFrame) entryconfigure 0 -state normal
           set origPath $env(PATH)
           set env(PATH) "$origPath;${::fvPref::ds9LibPath}"
        } else {
           #$itk_component(displayFrame) entryconfigure 0 -state disabled
        }

        if { [info exists ::fvPref::xpaLibPath] && [string length ${::fvPref::xpaLibPath}] > 0 && [file exists ${::fvPref::xpaLibPath}] } {
           set origPath $env(PATH)
           set env(PATH) "$origPath;${::fvPref::xpaLibPath}"
           ::powXPA::init
        }
     }
}

itcl::body fvWinKeeper::updateVisibility { } {
   global isMac

   if { !$isMac && $fvPref::ifUseManager } {
      wm deiconify $itk_interior
      if { ! [winfo ismapped $itk_component(mainMenu)] } {
         $itk_component(mainMenu) post 0 0
      }
   } else {
      wm withdraw $itk_interior
   }

}


itcl::body fvWinKeeper::_hide { } {
   global isMac

   set fvPref::ifUseManager 0
   updateVisibility
   if { !$isMac && [llength $_winList]==0 } {
      checkForExit
   }
}
