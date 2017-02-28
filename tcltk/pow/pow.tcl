proc printStack {} {
    set level [info level]
    for {set i 1} {$i < $level} {incr i} {
	puts "Level $i: [info level $i]"
    }
}


proc powInitGlobals {} {
#puts "powInitGlobals start"
   ##############################
   # This routine sets up various global variables which should be initialized
   # when POW is loaded... Some are used before the TCL routine powInit 
   # is called... eg, in powSetupColormap.  Putting it here instead of in
   # the C initialization routine, PowInit, makes it easily updated/modified
   # by developers (and us).  PowInit must call this routine.
   ##############################

   global powMinColorcells
   global powPseudoImages
   global powbg
   global powFrameForTop
   global yellowLineWidth
   global powWCS
   global regionParam xRangeParam
   global powLutButton powROIButton
   global POWRC tcl_platform env
   global isMac
   global powIsDragging
   global powOrderedGraphList
   global localPowObject
   global searchPath
   global powOutputPaperSize
   global powConvertFunction
   global powSelectDirectory
   global powPrintFunction
   global powPaperSizeSelected
   global tixOption
   global g_titleFont
   global powRegionListGlobal

   global powNotifications
   global ghostScript
   global g_backupDir

   ##############################
   #  Notifications sent by POW:
   #       on graph creation/redraw:  graphHasFinishedDrawing
   #       on graph destruction:      graphHasBeenDestroyed
   #       on graph deselection:      graphHasBeenUnselected
   #       on graph selection:        graphHasBeenSelected
   #       on image selection:        imageHasBeenSelected
   #       on graph resize:           graphHasResized
   #       on graph moved:            graphHasMoved   dx  dy

   if {($tcl_platform(platform) ne "windows") && ($tcl_platform(os) ne "Windows NT")} {
     package require Tix
     tix configure -fontset 14Point
   }

   set ghostScript "gs"
   if { $tcl_platform(platform)=="windows"  } { 
      set ghostScript "gswin32c"
   }
   set errorFlag [ catch { exec $ghostScript -help } result ]
   set searchPath [determineSearchPath $result]

   if { [string first "Available devices:" $result] < 0 } {
      set ghostScript ""
   }

   if ![info exists g_backupDir] {
      set g_backupDir $::env(HOME)
   }

   set powOutputPaperSize [list Letter 8.0i 10.5i 672.4 985.6 612 792 \
                                A4 7.76389i 11.1944i 652.6 1050.8 595 842 ]

#                               A5 5.34722i 7.76389i 449.43 728.8 420 595 
#                               Legal 8.0i 13.5i 672.4 1267.2 612 1008 
#                               11x17 10.5i 16.5i 882.53 1548.8 792 1224 
#                               Ledger 16.5i 10.5i 1386.82 985.86 1224 792

   set powConvertFunction [list {bmp bmp256 bmp "Windows Bitmap"} \
                                {jpeg jpeg jpg "JPEG File"} \
                                {postscript pswrite ps "Postscript Files"} \
                                {pbm pbm pbm "Portable Bitmap File"} \
                                {ppm ppm ppm "Portable Pixmap File"} \
                                {png png256 png "Portable Network Graphics"} \
                                {pnm pnm pnm "Portable any Map File"} \
                                {ppm ppm ppm "Portable Pix Map File"} \
                                {tiff tiff24nc tiff "Tagged Image File Format"}]

   set powSelectDirectory [pwd]
   set powPaperSizeSelected Letter
   set powRegionListGlobal {}

   set powPrintFunction "lpr"
   if { $tcl_platform(platform)=="windows"  } { 
      set powPrintFunction "winPrint"
   }

   set powNotifications [gNotifications]

   #
   ##############################

   # Identify if on a Mac
   if { $tcl_platform(platform)=="macintosh" } {
      set isMac 1
   } else {
      set isMac 0
   }

   #   Try to grab 128 colorcells to hold POW's colormaps
   set powMinColorcells 128
   #   Use pseudocolor images by default 
   set powPseudoImages 1

   #   Also need this so powToplevel can get going in safe mode
   if { $isMac } {
      # Set default background to a more "Platinum" appearance
      set powbg "#eeeeee"
   } else {
      set powbg "#cccccc"
   }

   #   By default, assume we're not running safe (need to make a powSafeInit
   #   entry point to do this eventually
   set powFrameForTop 0
   #   How wide should the yellow line around the selected graph be?
   set yellowLineWidth 3

   #   Initialize a couple of region handling globals
   set regionParam(nItems) 0
   set regionParam(gn) ""

   set xRangeParam(nItems) 0
   set xRangeParam(gn) ""


   # These two variables are here so developers can easily override them...
   # The first determines which mouse button "diddles" the image colortable,
   # the second drags out a Region Of Interest
   set powLutButton 3
   set powROIButton 1

   trace variable powWCS w powWCSCallback

   # Define location of the POW resource file

   switch $tcl_platform(platform) {
      "unix"      { 
            set POWRC "~/.powrc"
      }
      "windows"   { 
            set POWRC [file join $env(POW_LIBRARY) "pow.ini"] 
      }
      "macintosh" { set POWRC [file join $env(PREF_FOLDER) "pow Preferences"] }
   }

# Comment out powXPA init until future need for XPA
   powXPA::init
   set localPowObject [gPowCmdsClass]

   # This is just a state variable
   set powIsDragging 0

   # This lists the graphs created, in order
   set powOrderedGraphList {}
}

########################################################################

proc powSetGlobal_bg { val } {
#puts "powSetGlobal_bg start"
   global powbg

   if { $val == "" } return
   set powbg $val
   powChangeBg
}

proc powSetGlobal_cursor { val } {
#puts "powSetGlobal_cursor start"
   global powcursor
   global powGraphCoordsTracker

   set powcursor $val
   .pow       configure -cursor $val
   .pow.pow   configure -cursor $val
   .pow.scope configure -cursor $val
}

proc powSetGlobal_ResizeMain { val } {
#puts "powSetGlobal_ResizeMain start"
   global powResizeMain

   set powResizeMain $val
}

proc powSetGlobal_GUIposition { val } {
#puts "powSetGlobal_GUIposition start"
   global powGUIposition

   set powGUIposition $val
   powLayoutGUI
   powUpdateGeometry
}

proc powSetGlobal_ScopeSize { val } {
#puts "proc powSetGlobal_ScopeSize start"
   eval powCmds::scope $val
}

########################################################################

proc powGetVersion { } {
#puts "powGetVersion start"
   set revisionString {$Revision: 1.516 $}
   regexp {Revision: ([0-9.]+)} $revisionString blob powVersion
   set powVersion "(Build $powVersion)"
   return $powVersion
}

proc powWCSCallback {array element op} {
    global powWCS
    if {$powWCS($element) == ""} return
    if {![catch {image type $element}]} {
	eval [concat powWCSInitImage $element $powWCS($element)];
    } elseif { [lsearch -exact [powListCurves] $element]!=-1 } {
	eval [concat powWCSInitCurve $element $powWCS($element)];
    }
}

proc powInit { {powXColormapWindow none} {powContainerPath none} {powgui 1}} {
#puts "powInit start"
#This procedure creates the .pow window and sets up all the bindings
#  powXColormapWindow -  a Tk path specifying a window to use to find the
#                        X colormap to use.
#  powContainer - what window to "use" to put POW in
#  powgui - Do you want the POW user GUI header (usually yes, unless you're embedding POW in some other application)
    global currgn currimg mag powLargeFont Private_Colormap env
    global cmap cmap_inv powResizeMain powcursor
    global powImageParam powCurveParam powMenuOption
    global powbg powScopeWidth powScopeHeight powPlotParam powFontParam
    global powMinHeight powMinWidth powMaxHeight powMaxWidth
    global powFirstTimeThroughFlag
    global powShowHandlesFlag 
    global powTrackText
    global powGraphCoordsTracker powImagePixelTracker powImageValueTracker powPhysicalPixelTracker
    global powHelpTopics curr_img powFirstPixel
    global powSqueeze powSlide powPseudoImages powMinColorcells
    global Pow_Allocated powGUIposition
    global powScopeMargin powScopeSize powGridLines powShowScope powScopeGn
    global powGUI showlinks powContainer powFrameForTop powDWP
    global powLutButton powROIButton POWRC
    global tcl_platform
    global menuBarDeleteFlag
    global g_titleFont g_backupDir
    global g_magnification
    global prev_magnification
    global g_multiplier
    global g_showpow_flag
    global powFitsHeader powFitsHeaderCnt 

    set powGUI $powgui
    set powContainer $powContainerPath
    if {($powXColormapWindow == "safe" || $powFrameForTop == 1) && \
	    $powContainer == "none"} {
	set powContainer "."
    }

    event add <<powExit>> <Control-c>
    set powFirstTimeThroughFlag 1
    set powShowHandlesFlag 1


#    trace variable powPlotParam w debug_trace


#set defaults for options

    if {$powXColormapWindow == "safe"} {
#don't set up a colormap if we're running in a tclet
	set powFrameForTop 1
	if {[winfo visual $powContainer] == "pseudocolor"} {
#	    set powPseudoImages 1
#	    This seems to break the plugin, so for now
	    set powPseudoImages 0
	} else {
	    set powPseudoImages 0
	}
	powToplevel .pow safe
    } else {	
        if {$powXColormapWindow == "none" || $powXColormapWindow == "NULL"} {
	    if {$powContainer != "none" && $powContainer != "NULL"} {
		powSetupColormap .pow 0 [list -use [winfo id $powContainer]]
	    } else {
		powSetupColormap .pow 0 
	    }
	    set powXColormapWindow .pow 
	} else {
	    set visual [winfo visual $powXColormapWindow]
	    set depth [winfo depth $powXColormapWindow]
	    if {![regexp -nocase "pseudocolor" $visual] || $depth != 8} {
                # This doesn't seem so bad
		# puts stderr "Visual of window $powXColormapWindow is $visual $depth."
		set powPseudoImages 0
	    }
	    
	    if {$powContainer != "none" && $powContainer != "NULL"} {
		powToplevel .pow $powXColormapWindow [list -use [winfo id $powContainer]]
	    } else {
		powToplevel .pow $powXColormapWindow 
	    }
	}



#Check that there's still enough colors for POW to function i.e. they haven't
#filled up the Colormap since we set it up, this should only happen if 
#they're using the default colormap.
	if {$powPseudoImages} {
	    set free_colors [powTestColormap $powXColormapWindow]
	    if {(($Pow_Allocated != 0) && ($free_colors < 10)) || \
		    ($Pow_Allocated == 0) && ($free_colors < 60) } {
		puts stderr "Colormap full";
		set powPseudoImages 0;
	    }
	}
    }


#Since a master window must be the parent of a slave window or the descendant
#of the slaves parent, we can't use .pow.whatever for "popup" dialogs 
#DWP stands for DialogWindowPrefix
    if $powFrameForTop {
	set powDWP ".pow"
    } else {
	set powDWP ".pow."
    }

    if { [info exists g_showpow_flag] && $g_showpow_flag == "noshow" } {
       wm withdraw .pow
    }

	#powPseudoImages is set by powSetupColormap
	    if {!$powPseudoImages} {
	#	puts stderr "Pseudocolor images disabled; using Truecolor images."
		powSetupPhotoImages
	    }

	    #  Huge lists of fonts can't be listed on screen, so just grab
	    #  some common ones.

	    set allFonts [lsort [font families]]
	    set powFontParam(allFonts,powDef) {}
	    foreach fnt [list \
		  {[Aa]rial} \
		  {[Cc]ourier} \
		  {[Hh]elvet} \
		  {[Pp]alat} \
		  {[Tt]imes} \
		  {[Ss]ymbol} \
		  ] {
	       set i [lsearch -regexp $allFonts "^${fnt}.*"]
	       if { $i >= 0 } {
		  lappend powFontParam(allFonts,powDef) [lindex $allFonts $i]
	       }
	    }
	    set fnt {[Cc]ourier}
	    set i [lsearch -regexp $powFontParam(allFonts,powDef) "^${fnt}.*"]
	    if { $i < 0 } {set i 0}
	    set fnt [lindex $powFontParam(allFonts,powDef) $i]

	    set powFontParam(allTypes,powDef) [list title axis tick note]
	    set powFontParam(allOpts,powDef)  [list Font Size Weight Slant Color]
	    foreach lbl $powFontParam(allTypes,powDef) {
		set powFontParam(${lbl}Font,powDef)   $fnt
		set powFontParam(${lbl}Size,powDef)   12
		set powFontParam(${lbl}Weight,powDef) normal
		set powFontParam(${lbl}Slant,powDef)  roman
		set powFontParam(${lbl}Color,powDef)  "#000000"
	    }
	    set powFontParam(titleSize,powDef) 16

	    set powImageParam(allOpts,powDef) [list colormap invert scale]
	    set powImageParam(colormap,powDef) gray
	    set powImageParam(invert,powDef) No
	    set powImageParam(scale,powDef) log
	    set powImageParam(allMaps,powDef) [list \
		  [list Continuous gray blkbdy hot cold spectrum inv_spec \
				   color1 color2 color3] \
		  [list Ramps gray-ramp2 gray-ramp4 bgr-ramp bgr-ramp2 \
				   rygcbm-ramp bowlerhat] \
		  [list Steps gray-step4 gray-step8 bgr-step bgr-step2 \
				   rygcbm-step tophat] \
		  ]

	    set currgn "powDef"
	    set powScopeGn "-"

	    set powCurveParam(allOpts,powDef) \
		    [list pDisp pShape pSizeErr pSize pFill pColor \
			  lDisp lStyle lWidth lStep lBoxFill lColor \
			  logX logY LOD]
	    foreach opt $powCurveParam(allOpts,powDef) \
		    val [list Yes Cross No 4 No #000000 \
			      No " " 1 No No #000000 \
			      No No 0] {
		set powCurveParam($opt,powDef) $val
	    }
	    set powCurveParam(allColors,powDef) \
		  [list Black  #000000  Red   #FF0000  Blue    #0000FF  Green  #00FF00 \
			Yellow #FFFF00  White #FFFFFF  Purple  #9900FF  Orange #FF9900 \
			Aqua   #00FFFF  Grey  #999999  Fuchsia #FF00FF  ]



	    set screenHeight [winfo screenheight .]
	    set powScopeWidth 150
	    set powScopeHeight 150
	    set powMinHeight 350
	    set powMinWidth  350
	    set powGUIposition top
	    set powShowScope 1

	    set powcursor crosshair
	    set powResizeMain 0
	    set showlinks 0
	    set powFirstPixel 1


	    set powPlotParam(allOpts,powDef) [list \
		  tickLabels xTickScal yTickScal xNumTicks yNumTicks \
		  xTickLength yTickLength xLabelTicks yLabelTicks \
		  tickFormatCmdX tickFormatCmdY \
		  GridLines GridColor GridDash \
		  xdimdisp ydimdisp ]
	    set powPlotParam(wcsName,powDef) "WCS"
	    set powPlotParam(tickLabels,powDef) "degrees"
	    set powPlotParam(tickFormatCmdX,powDef) "format %.6lg"	   
	    set powPlotParam(tickFormatCmdY,powDef) "format %.6lg"	   
	    set powPlotParam(xTickScal,powDef) "linear"
	    set powPlotParam(yTickScal,powDef) "linear"
	    set powPlotParam(xNumTicks,powDef) 3
	    set powPlotParam(yNumTicks,powDef) 3
	    set powPlotParam(GridLines,powDef) No
	    set powPlotParam(GridColor,powDef) "#FFFFFF"
	    set powPlotParam(GridDash,powDef) " "
	    set powPlotParam(xdimdisp,powDef) 350
	    set powPlotParam(ydimdisp,powDef) 350

	    # order is [lft rgt top bot]
	    set powPlotParam(xTickLength,powDef) [list 10 10 10 10]
	    set powPlotParam(yTickLength,powDef) [list 10 10 10 10]
	    set powPlotParam(xLabelTicks,powDef) [list Yes No No Yes]
	    set powPlotParam(yLabelTicks,powDef) [list Yes No No Yes]


	    # Initialize other powDef variables to simplify creating new graphs

	    set powPlotParam(curves,powDef)   NULL
	    set powPlotParam(images,powDef)   NULL
	    set powPlotParam(xunits,powDef)   NULL
	    set powPlotParam(yunits,powDef)   NULL
	    set powPlotParam(xlabel,powDef)   X
	    set powPlotParam(ylabel,powDef)   Y
	    set powPlotParam(xBot,powDef)     NULL
	    set powPlotParam(yBot,powDef)     NULL
	    set powPlotParam(xTop,powDef)     NULL
	    set powPlotParam(yTop,powDef)     NULL
            set powPlotParam(flipD,powDef)    "U"

	    set g_magnification               1.0
	    set prev_magnification            1.0
	    set g_multiplier                  4.0

	#read user's option file if present

	    catch {if [file readable $POWRC] { source $POWRC } }

	# test for obsolete variable powCurvetype

	    if { [info exists powCurvetype] } {
		if { [string first Points $powCurvetype]==-1 } {
		    set powCurveParam(pDisp,powDef) No
		} else {
		    set powCurveParam(pDisp,powDef) Yes
		}
		if { [string first Line $powCurvetype]==-1 } {
		    set powCurveParam(lDisp,powDef) No
		} else {
		    set powCurveParam(lDisp,powDef) Yes
		}
		unset powCurvetype
	    }
	# Convert old 1/0 booleans to Yes/No strings

	    foreach opt [list pDisp pSizeErr pFill lDisp lStep] {
	       if { $powCurveParam($opt,powDef) } {
		  set powCurveParam($opt,powDef) Yes
	       } else {
		  set powCurveParam($opt,powDef) No
	       }
	    }

	    set powSqueeze 0.0
	    set powSlide 0.0

	# Convert old powNumTicks to powX/YNumTicks

	    if { [info exists powNumTicks] } {
	       set powPlotParam(xNumTicks,powDef) $powNumTicks
	       set powPlotParam(yNumTicks,powDef) $powNumTicks
	    }

	# Convert old pow* options to powPlotParam(...)

	    if { [info exists powGrid] } {
	       if { $powGrid } {
		  set powPlotParam(GridLines,powDef) Yes
	       } else {
		  set powPlotParam(GridLines,powDef) No
	       }
	       set powGridLines $powPlotParam(GridLines,powDef)
	    }

	    if { [info exists powGridColor] } {
	       set powPlotParam(GridColor,powDef) $powGridColor
	    }
	    if { [info exists powGridDash] } {
	       set powPlotParam(GridDash,powDef) $powGridDash
	    }
	    if { [info exists powSixties] } {
	       if { $powSixties } {
		  set powPlotParam(tickLabels,powDef) "degrees"
	       } else {
		  set powPlotParam(tickLabels,powDef) "decimal"
	       }
	    }

	    set powMenuOption(tickScal) \
		  "$powPlotParam(xTickScal,powDef)-$powPlotParam(yTickScal,powDef)"
	    
	# Convert text colors to hex...
	    if { [info exists powCurveParam(Color,powDef)] } {
	       set powCurveParam(lColor,powDef) $powCurveParam(Color,powDef)
	       set powCurveParam(pColor,powDef) $powCurveParam(Color,powDef)
	       unset powCurveParam(Color,powDef)
	    }

	    set powCurveParam(lColor,powDef) \
		  [powColorToHex $powCurveParam(lColor,powDef)]

	    set powCurveParam(pColor,powDef) \
		  [powColorToHex $powCurveParam(pColor,powDef)]

	    set powPlotParam(GridColor,powDef) \
		  [powColorToHex $powPlotParam(GridColor,powDef)]

	# Calculate Scopebox margin/size from current Width/Height values

	    set powScopeSize [list $powScopeWidth $powScopeHeight]
	    if { !($powScopeWidth && $powScopeHeight) } {
	       set powScopeWidth  10
	       set powScopeHeight 10
	       set powShowScope   0
	    } else {
	       set powShowScope 1
	    }
	    set powScopeMargin [expr ($powScopeWidth+$powScopeHeight)/20]

	    set Private_Colormap 4
    
    if {!$powPseudoImages} {
#	powSetColorTable
    }

    .pow configure -bg $powbg
    catch {wm title .pow "POW [powGetVersion]"}
    catch {wm geometry .pow +0+240}

    if $powGUI {

       ##############################################################
       #      Start Menus
       ##############################################################
       global isMac

       event add <<powSave>>    <Alt-Key-s>
       event add <<powPrint>>    <Alt-Key-p>
       event add <<powZoomIn>>  <Control-Key-i>
       event add <<powZoomOut>> <Control-Key-o>
       event add <<powZoomReset>> <Control-Key-r>

       bind .pow <<PostMenus>>   "powEvents::postMenus %W"
       bind .pow <<powSave>>     "powSave"
       bind .pow <<powPrint>>    "powPrintBox"
       bind .pow <<powZoomIn>>   "powScopeZoom in yes 2.0"
       bind .pow <<powZoomOut>>  "powScopeZoom out yes 2.0"
       bind .pow <<powZoomReset>>  "powScopeZoom reset yes"

       .pow config -menu .pow.mbar
       menu .pow.mbar -postcommand "powEvents::generate <<PostMenus>>" -bg $powbg -font g_titleFont
       if { $isMac } {
          set cmdkey "Cmd"
          set bdVal 0
          .pow.mbar add cascade -menu .pow.mbar.apple
          .pow.mbar add cascade -menu .pow.mbar.file    -label "File" -font g_titleFont
          .pow.mbar add cascade -menu .pow.mbar.edit    -label "Edit" -font g_titleFont
          .pow.mbar add cascade -menu .pow.mbar.colors  -label "Colors" -font g_titleFont
          .pow.mbar add cascade -menu .pow.mbar.tools   -label "Tools" -font g_titleFont
          .pow.mbar add cascade -menu .pow.mbar.zoom    -label "Zoom" -font g_titleFont
          .pow.mbar add cascade -menu .pow.mbar.help    -label "Help" -font g_titleFont
          menu .pow.mbar.apple -tearoff False
          .pow.mbar.apple add command -label "About POW" \
                -command {powHelp About.html} -font g_titleFont
       } else {
          set cmdkey "Alt"
          set bdVal 2
          .pow.mbar add cascade -menu .pow.mbar.file    -label "File" -font g_titleFont
          .pow.mbar add cascade -menu .pow.mbar.edit    -label "Edit" -font g_titleFont
          .pow.mbar add cascade -menu .pow.mbar.colors  -label "Colors" -font g_titleFont
          .pow.mbar add cascade -menu .pow.mbar.tools   -label "Tools" -font g_titleFont
          .pow.mbar add cascade -menu .pow.mbar.zoom    -label "Zoom" -font g_titleFont
       }

       #
       # FILE
       #
       
       set ::env(PSTMPDIR) $g_backupDir
       menu .pow.mbar.file -bg $powbg -bd $bdVal \
           -postcommand "powEvents::generate <<PostMenus>>" -font g_titleFont

       # .pow.mbar.file add command -label "Page Setup..." \
             -command {powSetupPage} -font g_titleFont

       .pow.mbar.file add command -label "Print..." \
             -command {powPrintPreview} -font g_titleFont
       # .pow.mbar.file add command -label "Print..." \
             -command "powPrintBox" \
             -accelerator "$cmdkey+P" -font g_titleFont

       .pow.mbar.file add separator

       .pow.mbar.file add command -label "Close" \
             -command "::powEvents::ExitPOW" \
             -accelerator "$cmdkey+W" -font g_titleFont

       #
       # COLORS
       #

       menu .pow.mbar.colors -bg $powbg -bd $bdVal \
           -postcommand "powEvents::generate <<PostMenus>>" -font g_titleFont
       foreach colorGrp $powImageParam(allMaps,powDef) {
          set cName [lindex $colorGrp 0]
          menu .pow.mbar.colors.c$cName -bg $powbg -bd $bdVal
          .pow.mbar.colors add cascade -menu .pow.mbar.colors.c$cName \
                -label "$cName" -font g_titleFont
          foreach color [lrange $colorGrp 1 end] {
             .pow.mbar.colors.c$cName add radiobutton -label $color \
                   -value $color \
                   -variable powImageParam(colormap,powDef) \
                   -command "powCmds::colormap $color" -font g_titleFont
          }
       }

       .pow.mbar.colors add separator

       .pow.mbar.colors add checkbutton -label "Invert Colortable" \
             -variable powImageParam(invert,powDef) \
             -onvalue Yes -offvalue No \
             -command {powCmds::colormap invert $powImageParam(invert${currimg},$currgn)} -font g_titleFont

        .pow.mbar.colors add separator

        .pow.mbar.colors add radiobutton -label "linear"         -value linear \
              -variable powImageParam(scale,powDef) \
              -command "powCmds::colormap scale linear" -font g_titleFont
        .pow.mbar.colors add radiobutton -label "square root"    -value sqrt \
              -variable powImageParam(scale,powDef) \
              -command "powCmds::colormap scale sqrt" -font g_titleFont
        .pow.mbar.colors add radiobutton -label "logarithmic"    -value log \
              -variable powImageParam(scale,powDef) \
              -command "powCmds::colormap scale log" -font g_titleFont
        .pow.mbar.colors add radiobutton -label "Histo Equalize" -value histo \
              -variable powImageParam(scale,powDef) \
              -command "powCmds::colormap scale histo" -font g_titleFont

	.pow.mbar.colors add command -label "Rescale Image..." \
              -command {powRescaleBox} -font g_titleFont

	.pow.mbar.colors add separator

	.pow.mbar.colors add command -label "Create Colorbar" \
              -command {powColorbar} -font g_titleFont
    
        #
        # EDIT
        #

	menu .pow.mbar.edit -bg $powbg -bd $bdVal \
            -postcommand "powEvents::generate <<PostMenus>>" -font g_titleFont

        if { $isMac } {
            .pow.mbar.edit add command -label "Can't Undo" -state disabled -font g_titleFont
            .pow.mbar.edit add separator
        }
        
	.pow.mbar.edit add command -label "Edit Graph..." \
	    -command {powEditGraphDlg $currgn} -font g_titleFont
	.pow.mbar.edit add command -label "Add Text Label..." \
	    -command {powEditNoteDlg $currgn -1} -font g_titleFont
	.pow.mbar.edit add command -label "Choose Graph Size..." \
	    -command powSetGraphSize -font g_titleFont

        .pow.mbar.edit add separator

	.pow.mbar.edit add command -label "Duplicate Graph" \
	    -command {powEditCreateNewGraph $currgn} -font g_titleFont
	.pow.mbar.edit add command -label "Delete Graph" \
	    -command {powDeleteGraph $currgn} -font g_titleFont
	menu .pow.mbar.edit.merge -bg $powbg -bd $bdVal \
            -postcommand {powGenerateMergeCascade} -font g_titleFont
	.pow.mbar.edit add cascade -label "Merge Graphs" \
	    -menu .pow.mbar.edit.merge  -font g_titleFont
	menu .pow.mbar.edit.unmap -bg $powbg -bd $bdVal \
	    -postcommand {powGenerateUnmapCascade} -font g_titleFont
	.pow.mbar.edit add cascade -label "Hide Graph" \
	    -menu .pow.mbar.edit.unmap -font g_titleFont
	menu .pow.mbar.edit.replot -bg $powbg -bd $bdVal \
	    -postcommand {powGenerateReplotCascade}   -font g_titleFont
	.pow.mbar.edit add cascade -label "Replot Graph" \
	    -menu .pow.mbar.edit.replot -font g_titleFont

    .pow.mbar.edit add separator

	menu .pow.mbar.edit.log -bg $powbg -bd $bdVal
	.pow.mbar.edit add cascade -label "Axes Transforms" \
	      -menu .pow.mbar.edit.log -font g_titleFont
       .pow.mbar.edit.log add radiobutton -label "Linear-Linear" \
	    -variable powMenuOption(tickScal) -value "linear-linear" \
	    -command { powCmds::axes linear linear } -font g_titleFont
       .pow.mbar.edit.log add radiobutton -label "Linear-Log" \
	    -variable powMenuOption(tickScal) -value "linear-log" \
	    -command { powCmds::axes linear log } -font g_titleFont
       .pow.mbar.edit.log add radiobutton -label "Log-Linear" \
	    -variable powMenuOption(tickScal) -value "log-linear" \
	    -command { powCmds::axes log linear } -font g_titleFont
       .pow.mbar.edit.log add radiobutton -label "Log-Log" \
	    -variable powMenuOption(tickScal) -value "log-log" \
	    -command { powCmds::axes log log } -font g_titleFont

    #  Tick Label format

	menu .pow.mbar.edit.tlabels -bg $powbg -bd $bdVal
	.pow.mbar.edit add cascade -label "Tick Labels" \
	    -menu .pow.mbar.edit.tlabels -font g_titleFont
	.pow.mbar.edit.tlabels add radiobutton -label "Decimal" \
	    -variable powPlotParam(tickLabels,$currgn) -value "decimal"  \
	    -command {
		powAdornGraph $currgn .pow.pow;
	        powRedrawGraphHandles $currgn
	    } -font g_titleFont
	.pow.mbar.edit.tlabels add radiobutton -label "Base 60 (deg)" \
	    -variable powPlotParam(tickLabels,$currgn) -value "degrees"  \
	    -command {
		powAdornGraph $currgn .pow.pow
	        powRedrawGraphHandles $currgn
	    } -font g_titleFont

       # Grid Line Options

	menu .pow.mbar.edit.grid -bg $powbg -bd $bdVal
	.pow.mbar.edit add cascade -label "Grid Line Options" \
	    -menu .pow.mbar.edit.grid -font g_titleFont
	.pow.mbar.edit.grid add checkbutton -label "Show Grid Lines" \
	    -variable powPlotParam(GridLines,$currgn) \
	    -onvalue Yes -offvalue No \
	    -command {powChangeGrid 1} -font g_titleFont

	.pow.mbar.edit.grid add separator

        foreach {clr hex} [list White #FFFFFF Black #000000 \
	      Blue #0000FF Red #FF0000] {
	   .pow.mbar.edit.grid add radiobutton -label $clr \
		 -variable powPlotParam(GridColor,$currgn) -value $hex \
		 -command {powChangeGrid 0} -font g_titleFont
	}

	.pow.mbar.edit.grid add separator

	foreach [list opt val] \
	        [list Solid " " "Small Dash" 10 "Large Dash" 20] {
	   .pow.mbar.edit.grid add radiobutton -label $opt \
		 -variable powPlotParam(GridDash,$currgn) -value $val \
		 -command {powChangeGrid 0} -font g_titleFont
	}

	.pow.mbar.edit.grid add separator

	.pow.mbar.edit.grid add command -label "Fewer Lines" \
	    -command {
	       if {$powPlotParam(xNumTicks,$currgn)>0} {
		  incr powPlotParam(xNumTicks,$currgn) -1
	       }
	       if {$powPlotParam(yNumTicks,$currgn)>0} {
		  incr powPlotParam(yNumTicks,$currgn) -1
	       }
               powChangeGrid 1
        } -font g_titleFont
	.pow.mbar.edit.grid add command -label "More Lines" \
	    -command {
                incr powPlotParam(xNumTicks,$currgn)
                incr powPlotParam(yNumTicks,$currgn)
                powChangeGrid 1
        } -font g_titleFont

	menu .pow.mbar.edit.wcs -bg $powbg -bd $bdVal
	.pow.mbar.edit add cascade -label "WCS" \
	    -menu .pow.mbar.edit.wcs -font g_titleFont

	.pow.mbar.edit.wcs add radiobutton -label "WCS" \
	     -variable powPlotParam(wcsName,$currgn) -value WCS \
             -command {powSwitch2NewWCSHeader} -font g_titleFont

	.pow.mbar.edit.wcs add separator

        set idx 3
	foreach wcsName [list a b c d e f g h i j k l m n o p q r s t u v w x y z] {
	   .pow.mbar.edit.wcs add radiobutton -label "WCS $wcsName" \
		 -variable powPlotParam(wcsName,$currgn) -value WCS$wcsName \
		 -command {powSwitch2NewWCSHeader} -font g_titleFont
           .pow.mbar.edit.wcs entryconfigure $idx -state disable
           incr idx
	}

	.pow.mbar.edit add separator
	.pow.mbar.edit add command -label "Preferences..." \
		-command {powDefaultOptions} -font g_titleFont

# Eliminate in favor of Preferences box with "Get Current" button
#	.pow.mbar.edit add separator
#	.pow.mbar.edit add command -label "Save Current Settings" \
#	    -command {powGetCurrVariables; powSaveConfig}

       #
       # TOOLS
       #

       menu .pow.mbar.tools -bg $powbg -bd $bdVal \
           -postcommand "powEvents::generate <<PostMenus>>" -font g_titleFont
       .pow.mbar.tools add command -label "Blink Graphs..." \
             -command {powBlinkGraphDlg} -font g_titleFont
       .pow.mbar.tools add command -label "Blink Images..." \
             -command {powMovie} -font g_titleFont
       .pow.mbar.tools add command -label "Region Files..." \
             -command {powRegion} -font g_titleFont
       .pow.mbar.tools add command -label "Make Contour Map..." \
             -command {powContour} -font g_titleFont
       .pow.mbar.tools add command -label "Draw Profile..." \
             -command {ProfileDlg} -font g_titleFont
       .pow.mbar.tools add command -label "Ruler..." \
             -command {OpenRuler} -font g_titleFont
       .pow.mbar.tools add command -label "Image Probe" \
             -command {imgProbeDialog} -font g_titleFont
       .pow.mbar.tools add command -label "Select X Range.." \
             -command {powXRange} -font g_titleFont

       #
       # ZOOM Functions
       #

       menu .pow.mbar.zoom -bg $powbg -bd $bdVal \
           -postcommand "powEvents::generate <<PostMenus>>" -font g_titleFont

       .pow.mbar.zoom add command   -label "Zoom In (2x)" \
              -command "powScopeZoom in yes 2.0" -font g_titleFont -accelerator "Ctrl+I"
       .pow.mbar.zoom add command  -label "Zoom Out (2x)" \
              -command "powScopeZoom out yes 2.0"  -font g_titleFont -accelerator "Ctrl+O"
       .pow.mbar.zoom add command  -label "Zoom Reset" \
              -command "powScopeZoom reset yes"  -font g_titleFont -accelerator "Ctrl+R"
       .pow.mbar.zoom add separator

       foreach { maglabel mag } [list "zoom in at 1x" 1.0 "zoom in at 2x" 2.0 "zoom in at 4x" 4.0 \
                                      "zoom in at 8x" 8.0 "zoom in at 16x" 16.0 "zoom in at 32x" 32.0] {
           .pow.mbar.zoom add radiobutton -label $maglabel \
                 -variable g_magnification -value $mag \
                 -font g_titleFont -command "powSetMagnification ; powScopeZoom in yes"
       }

       .pow.mbar.zoom add separator
       .pow.mbar.zoom add command   -label "Invert X Axis" \
              -command "powFlipImage X"  -font g_titleFont
       .pow.mbar.zoom add command   -label "Invert Y Axis" \
              -command "powFlipImage Y"  -font g_titleFont
       .pow.mbar.zoom add command   -label "Invert Both" \
              -command "powFlipImage B"  -font g_titleFont
       .pow.mbar.zoom add command   -label "Undo Invert" \
              -command "powFlipImage U"  -font g_titleFont

       .pow.mbar add command   -label "Replot" \
              -command "powReplotReset"  -font g_titleFont

       if { !$isMac } {
          .pow.mbar add cascade -menu .pow.mbar.help    -label "Help" -font g_titleFont
       }
       set menuBarDeleteFlag "false"

        #
        # HELP
        #

	menu .pow.mbar.help -bg $powbg -bd $bdVal \
            -postcommand "powEvents::generate <<PostMenus>>" -font g_titleFont
        if { $isMac } {
            .pow.mbar.help config -tearoff False
        }
	set powHelpTopics(About.html) About
	set powHelpTopics(Overview.html) Overview
	set powHelpTopics(Blinking.html) "Blinking"
	set powHelpTopics(Color.html) "Colors"
	set powHelpTopics(Contours.html) "Contours"
	set powHelpTopics(Profile.html) "Drawing Profile"
	set powHelpTopics(Edit.html) "Edit Menu"
	set powHelpTopics(EditGraphs.html) "Editting Graphs"
	set powHelpTopics(EditObjects.html) "Editting Objects"
	set powHelpTopics(File.html) "File Menu"
	set powHelpTopics(Probe.html) "Image Probe"
	set powHelpTopics(Ruler.html) "Measure"
	set powHelpTopics(Moving_Graphs.html) "Moving Graphs"
	set powHelpTopics(ROI.html) "Panning/Zooming"
	set powHelpTopics(PrintControl.html) "Printing"
	set powHelpTopics(DefaultOptions.html) "Preferences"
	set powHelpTopics(Regions.html) "Region Files"
	set powHelpTopics(Scripting.html) "Scripting"
	set powHelpTopics(Tools.html) "Tool Menu"

	foreach topic [lsort [array names powHelpTopics]] {
           .pow.mbar.help add command -label $powHelpTopics($topic) \
                 -command "powHelp $topic" \
        }


        ##############################################################
        #  Build Scope Objects
        ##############################################################

#        frame .pow.scopebuttons -bg $powbg
#        button .pow.scopein -bg $powbg -text "Zoom In" \
#              -command "powScopeZoom in"
#        button .pow.scopezoom1 -bg $powbg -text "Replot" \
#              -command "powEndROI 1"
#        button .pow.scopeout -bg $powbg -text "Zoom Out" \
#              -command "powScopeZoom out"

        frame .pow.scopeframe -bg $powbg
        canvas .pow.scope -bg $powbg -cursor $powcursor \
              -width $powScopeWidth -height $powScopeHeight
        label .pow.currgn -textvariable powScopeGn -background yellow \
              -relief sunken -foreground black -font g_titleFont -bd 1

        ##############################################################
        #  Build Tracker Objects
        ##############################################################

	set powTrackText(gn)   "NULL"
	set powTrackText(img)  "NULL"
	set powTrackText(rx)   "X"
	set powTrackText(ry)   "X"
	set powTrackText(imgx) "X"
	set powTrackText(imgy) "X"
	set powTrackText(imgz) "X"
	set powTrackText(zunits) " "

	powUpdateTrackVars
    
	set powTrackerWidth 30
	frame .pow.trackers -bg $powbg -width $powTrackerWidth
	label .pow.graphtrack  -textvariable powGraphCoordsTracker \
	    -background $powbg -relief sunken -anchor w -justify left \
            -width $powTrackerWidth -font g_titleFont -bd 1
	label .pow.ppixeltrack -textvariable powPhysicalPixelTracker \
	    -background $powbg -relief sunken -anchor w -justify left \
            -width $powTrackerWidth -font g_titleFont -bd 1
	label .pow.pixeltrack  -textvariable powImagePixelTracker \
	    -background $powbg -relief sunken -anchor w -justify left \
            -width $powTrackerWidth -font g_titleFont -bd 1
	label .pow.pixvaltrack -textvariable powImageValueTracker \
	    -background $powbg -relief sunken -anchor w -justify left \
            -width $powTrackerWidth -font g_titleFont -bd 1

        frame .pow.gui -bg $powbg

        powLayoutGUI
    } 
#end powGUI if block

    frame .pow.bottom -bg $powbg 
    canvas .pow.pow -bg $powbg -cursor $powcursor\
	-xscrollcommand ".pow.scrollx set" \
	-yscrollcommand ".pow.scrolly set"  \
        -scrollregion [list 0 0 50 50]
    scrollbar .pow.scrolly -command "powScrollMainCanvas Y" -orient vertical\
	-bg $powbg
    scrollbar .pow.scrollx -command "powScrollMainCanvas X" -orient horizontal\
	-bg $powbg

    grid configure .pow.bottom -row 1 -column 1 -sticky "n s e w"
    grid rowconfigure    .pow 1  -weight 1
    grid columnconfigure .pow 1  -weight 1


    grid configure .pow.pow -sticky "n s e w" -row 0 -column 0 \
	-in .pow.bottom
    grid configure .pow.scrolly -sticky "n s e"  -row 0 -column 1 \
	-in .pow.bottom
    grid configure .pow.scrollx -sticky "n e w" -row 1 -column 0 \
	-in .pow.bottom
    
    
	
    grid rowconfigure .pow.bottom 0 -weight 1
    grid columnconfigure .pow.bottom 0 -weight 1

#put .pow.top above .pow.pow in the stacking order, 
#This should prevent window items on the
#.pow.pow canvas from "bleeding" off the edges of the .pow.pow canvas
    lower .pow.bottom

    powSetGeometry
    if $powGUI {
       if { ! $powShowScope } {
	  grid remove .pow.scopeframe
          grid remove .pow.trackers
          grid remove .pow.gui
          powDeleteMenuBarItem
       }

       bind .pow.pow   <Motion> {set_tracker_info %x %y .pow.pow}
       bind .pow.scope <Motion> {set_tracker_info %x %y .pow.scope}

       # The 'powProcessClick' is required here since both BtnPress 
       # and ROI is bound to canvas, but only 1 can be executed

#puts "calling powBindBtn"
       powBindBtn <<ROI>> "bind .pow.pow" \
	     {powBeginROI %x %y .pow.pow} \
	     {powDragROI  %x %y .pow.pow; set_tracker_info %x %y .pow.pow} \
	     {powEndROI   0     .pow.pow}
	   
       powBindBtn <<ROI>> "bind .pow.scope" \
	     {powBeginROI %x %y .pow.scope} \
	     {powDragROI  %x %y .pow.scope; set_tracker_info %x %y .pow.scope} \
	     {powEndROI   0     .pow.scope}

       powBindBtn <<ROI_Drag>> "bind .pow.scope"  \
	     {catch {powPanROI %x %y .pow.scope}} \
	     {catch {powPanROI %x %y .pow.scope}} \
	     {catch {powEndROI 0     .pow.scope}}

       bind .pow.pow   <<BackToOriginal>> \
          { set x [.pow.pow canvasx %x] 
            set y [.pow.pow canvasy %y]
              powDrawOriginal $x $y
          }
       bind .pow.scope <<BackToOriginal>> \
          { set x [.pow.scope canvasx %x] 
            set y [.pow.scope canvasy %y]
              powDrawOriginal $x $y
          }
       bind .pow.scope <<DblBtnPress>> {powProcessClick %x %y B%bD}
    }

    bind .pow <<powExit>> {powExit}

    bind .pow.pow <<ROI>>         {+powProcessClick %x %y B%b}
    bind .pow.pow <<BtnPress>>    {powProcessClick %x %y B%b}
    bind .pow.pow <<DblBtnPress>> {powProcessClick %x %y B%bD}

    bind .pow <Down> {event generate .pow <Motion> -warp yes -x %x \
	  -y [expr %y+1]}
    bind .pow <Up> {event generate .pow <Motion> -warp yes -x %x \
	  -y [expr %y-1]}
    bind .pow <Left> {event generate .pow <Motion> -warp yes -x [expr %x - 1] \
	  -y %y}
    bind .pow <Right> {event generate .pow <Motion> -warp yes \
	  -x [expr %x + 1] -y %y}


    .pow.pow bind graphDragable <Enter> {
       if { !$powIsDragging } {
          .pow.pow configure -cursor fleur
       }
    }
    .pow.pow bind graphDragable <Leave> {
       if { !$powIsDragging } {
          .pow.pow configure -cursor $powcursor
       }
    }

    powBindBtn <<Drag>> ".pow.pow bind graphDragable" \
          {powDragGraph start %X %Y} \
          {powDragGraph drag  %X %Y} \
          {powDragGraph end   %X %Y}

    ####
    # Create the virtual events and default bindings
    ####

    event add <<BtnPress>>     <ButtonPress-1> <ButtonPress-2> <ButtonPress-3>
    event add <<DblBtnPress>>  <Double-ButtonPress-$powLutButton>

    if { $powLutButton != 0 && $powLutButton != "NULL" } {
       event add    <<LUT>>  <ButtonPress-$powLutButton>
       if { $powLutButton == 3  &&  $tcl_platform(platform) == "macintosh" } {
	  event add <<LUT>>  <Command-ButtonPress-1>
       }
    }

    if { $powROIButton != 0 && $powROIButton != "NULL" } {
       # Must delete BtnPress sequence to prevent it from hiding the ROI event
       event delete <<BtnPress>>  <ButtonPress-$powROIButton>
       event add    <<ROI>>       <ButtonPress-$powROIButton>
       event add    <<ROI_Drag>>  <ButtonPress-$powLutButton>

       # we are dedicate the back to original to 3
       event add    <<BackToOriginal>>    <Double-ButtonPress-$powROIButton>
       if { $powROIButton == 3  &&  $tcl_platform(platform) == "macintosh" } {
	  event add <<ROI>>       <Command-ButtonPress-1>
          event add    <<BackToOriginal>>    <Command-Double-ButtonPress-1>
       }
    }


    if { $tcl_platform(platform) != "macintosh" } {
        event add <<RGN_Create>>   <ButtonPress-1> 
    } else {
        event add <<RGN_Create>>         <ButtonPress-3>  <Command-ButtonPress-1>
    }

    event add <<RGN_CreateDelete>>       <Shift-ButtonPress-1>

    event add <<RGN_Drag>>     <ButtonPress-1>
    event add <<RGN_DragPt>>   <ButtonPress-1>

    if { $tcl_platform(platform) != "macintosh" } {
        event add <<RGN_InsertPt>> <ButtonPress-1>
    } else {
        event add <<RGN_InsertPt>> <ButtonPress-3> <Command-ButtonPress-1>
    }
    event add <<RGN_Rotate>>   <ButtonPress-1>

    event add <<Drag>>         <ButtonPress-1> 

    update idletasks
}

# The next two functions prevent multiple bindings from being executed
# for Btn events.  Only the first set of events will be accepted.

proc powBindBtn { event bindCmd beginCmd dragCmd endCmd } {
#puts "powBindBtn start\n event: $event\n bindCmd: $bindCmd\n beginCmd: $beginCmd\n dragCmd: $dragCmd\n endCmd: $endCmd"
   regsub -all % $dragCmd %% dragCmd
   regsub -all % $endCmd %% endCmd

   set c1 [concat powBtn Begin $event [list $beginCmd] ]
   set c2 [concat $bindCmd <B%b-Motion> \{ \
	 [list powBtn Drag $event $dragCmd] \
	 \}]
   set c3 [concat $bindCmd <ButtonRelease-%b> \{ \
	 [concat $bindCmd <B%b-Motion> \{\}] \; \
	 [concat $bindCmd <ButtonRelease-%b> \{\}] \; \
	 [list    powBtn End $event $endCmd] \; \
	 \}]

   eval $bindCmd $event {"$c3; $c2; $c1"}
}

proc powBtn { evt cntxt cmd } {
#puts "powBtn start"
    global powBtnState powBtnContext

    if { ![info exists powBtnState] } {
	set powBtnState   none
	set powBtnContext none
    }

    # If this is a different event from last time, execute command and remember
    # context... otherwise execute only if same context

    if { $evt != $powBtnState || $cntxt==$powBtnContext } {
#puts "powBtn 1"
	set powBtnState $evt
	set powBtnContext $cntxt
	uplevel #0 $cmd
#puts "powBtn 1 end"
    }
}


proc powLayoutGUI { } {
   global powGUIposition powShowScope
#puts "powLayoutGUI start"

   # Build Tracker Box
   grid configure .pow.graphtrack -row 0 -column 0 -sticky ew \
         -in .pow.trackers
   grid configure .pow.ppixeltrack -row 1 -column 0 -sticky ew \
         -in .pow.trackers
   grid configure .pow.pixeltrack -row 2 -column 0 -sticky ew \
         -in .pow.trackers
   grid configure .pow.pixvaltrack -row 3 -column 0 -sticky ew \
         -in .pow.trackers 

   # Build ScopeWindow
   grid configure .pow.currgn -row 0 -column 0 -sticky s \
         -in .pow.scopeframe
   grid configure .pow.scope  -row 1 -column 0 -sticky n \
         -in .pow.scopeframe

   if { $powGUIposition == "top" || $powGUIposition=="bottom" } {
      # Build ScopeButtons
#      grid configure .pow.scopein    -row 0 -column 1 -sticky ew \
#            -in .pow.scopebuttons
#      grid configure .pow.scopezoom1 -row 1 -column 1 -sticky ew \
#            -in .pow.scopebuttons
#      grid configure .pow.scopeout   -row 2 -column 1  -sticky ew \
#            -in .pow.scopebuttons

      grid configure .pow.trackers      -in .pow.gui -row 1 -column 0 \
            -sticky w -padx 3 -pady 3
      if { $powShowScope } {
         grid configure .pow.scopeframe -in .pow.gui -row 1 -column 1 \
               -sticky e
      }
#      grid configure .pow.scopebuttons  -in .pow.gui -row 1 -column 2 \
#            -sticky e

      grid columnconfigure .pow.gui 1 -weight 1

      grid rowconfigure    .pow.gui 0 -weight 0
      grid rowconfigure    .pow.gui 1 -weight 1

   } else {
      # Build ScopeButtons
#      grid configure .pow.scopein    -row 1 -column 0 -sticky ew \
#            -in .pow.scopebuttons
#      grid configure .pow.scopezoom1 -row 1 -column 1 -sticky ew \
#            -in .pow.scopebuttons
#      grid configure .pow.scopeout   -row 1 -column 2  -sticky ew \
#            -in .pow.scopebuttons

      grid configure .pow.trackers      -in .pow.gui -row 2 -column 1 \
            -sticky n -padx 5 -pady 5
      if { $powShowScope } {
         grid configure .pow.scopeframe -in .pow.gui -row 0 -column 1 \
               -sticky n
      }
#      grid configure .pow.scopebuttons  -in .pow.gui -row 1 -column 1 \
#            -sticky n

      grid columnconfigure .pow.gui 1 -weight 1

      grid rowconfigure    .pow.gui 0 -weight 1
      grid rowconfigure    .pow.gui 1 -weight 0
   }

   switch $powGUIposition {
      "top" {
         grid configure .pow.gui -in .pow -row 0 -column 1 \
               -sticky news
      }
      "bottom" {
         grid configure .pow.gui -in .pow -row 2 -column 1 \
               -sticky news
      }
      "left" {
         grid configure .pow.gui -in .pow -row 1 -column 0 \
               -sticky n
      }
      "right" {
         grid configure .pow.gui -in .pow -row 1 -column 2 \
               -sticky n
      }
      default {
         grid remove .pow.gui
      }
   }

   lower .pow.gui
}


proc powScrollMainCanvas {x_or_y args} {
#puts "powScrollMainCanvas start"
    global powPreScrollCallback powPostScrollCallback
    if [info exists powPreScrollCallback] {
	eval $powPreScrollCallback $x_or_y $args
    }
    
    if {$x_or_y == "X"} {
	eval .pow.pow xview $args
    } else {
	eval .pow.pow yview $args
    }

    if [info exists powPostScrollCallback] {
	eval $powPostScrollCallback $x_or_y $args
    }
}
 
			  

proc powSetGeometry { } {
#puts "powSetGeometry start"
   global powHeaderHeight powHeaderWidth powGUIposition
   global powMaxWidth powMaxHeight powMinWidth powMinHeight
   global powRealMinWidth powRealMinHeight
   global powGUI powScopeSize powContainer tcl_platform

   update idletasks

   if $powGUI {

      if { $powGUIposition=="top" || $powGUIposition=="bottom" } {
         set powHeaderHeight [lindex [lsort -integer -decreasing [list \
               [expr 3*[winfo height .pow.graphtrack]] \
               [expr [winfo height .pow.scope]+[winfo height .pow.currgn]] \
               ] ] 0]
         if { $tcl_platform(platform) != "macintosh" } {
            # Add in the menubar
            incr powHeaderHeight 32
         }
         set powHeaderWidth  0
         set minGuiWidth [expr [winfo width .pow.graphtrack] \
                                 + [winfo width .pow.scope] \
                                 + 15]
         if { $minGuiWidth > $powMinWidth } {
            set powRealMinWidth $minGuiWidth
         } else {
            set powRealMinWidth $powMinWidth
         }
         set powRealMinHeight [expr $powMinHeight + $powHeaderHeight]

      } elseif { $powGUIposition=="left" || $powGUIposition=="right" } {

         set powHeaderWidth [lindex [lsort -integer -decreasing [list \
               [winfo width .pow.graphtrack] \
               [winfo width .pow.scope] \
               ] ] 0]
         set powHeaderHeight 0
         set minGuiHeight [expr 3* [winfo height .pow.graphtrack] \
                                 + [winfo height .pow.scope]      \
                                 + [winfo height .pow.currgn]     \
                                 + 10]
         if { $minGuiHeight > $powMinHeight } {
            set powRealMinHeight $minGuiHeight
         } else {
            set powRealMinHeight $powMinHeight
         }
         if { $tcl_platform(platform) != "macintosh" } {
            # Add in the menubar
            incr powRealMinHeight 32
            set powHeaderHeight 32
         }
         set powRealMinWidth [expr $powMinWidth + $powHeaderWidth]

      } else {

         set powHeaderHeight 0
         set powHeaderWidth  0

         set powRealMinWidth  $powMinWidth
         set powRealMinHeight $powMinHeight
      }

   } else {
      set powHeaderHeight 0
      set powHeaderWidth  0

      set powRealMinWidth  0
      set powRealMinHeight 0
   }

   set powMaxHeight  [expr [winfo screenheight .pow] - $powHeaderHeight]
   set powMaxWidth   [expr [winfo screenwidth  .pow] - $powHeaderWidth]
      
   if {$powContainer == "none" || $powContainer == "NULL" } {
      catch {
	 wm minsize .pow $powRealMinWidth $powRealMinHeight
      }
      catch {
	 wm maxsize .pow [expr $powMaxWidth + $powHeaderWidth] \
               [expr $powMaxHeight + $powHeaderHeight]
      }
   } else {
      catch {wm geometry .pow "[winfo geometry $powContainer]"}
   }
}

proc powGetGraphOptions {gn} {
#puts "powGetGraphOptions start"
#returns list of extra graph options
    global powPlotParam

#test for requested graph    
    if { ![powListGraphs $gn] } {
	error "Graph $gn does not exist"
	return
    }

    lappend optlist "bgcolor" $powPlotParam(bgcolor,$gn)
    lappend optlist "xmargin" $powPlotParam(xmargin,$gn)
    lappend optlist "ymargin" $powPlotParam(ymargin,$gn)

    return $optlist
}


proc powGraphOptions {gn args} {
#puts "powGraphOptions start"
#add extra graph options here.  Arguments are "option value" pairs.
    global powPlotParam currgn

    set argc [llength $args]
    if { $argc == 1 } {
       if { [info exists powPlotParam($args,$gn)] } {
          return $powPlotParam($args,$gn)
       } else {
          return ""
       }
    } elseif { [expr $argc %2] != 0 } {
       error "Arguments must be in 'option value' pairs"
    }

    foreach [list option value] $args {
	switch $option {
	    bgcolor {
		set powPlotParam(bgcolor,$gn) $value
                if [winfo exists .pow.pow] {
                   set cleanName [powCleanName $gn]handle
                   .pow.pow itemconfigure ${gn}bkg -fill $value -outline $value
                   if [winfo exists .pow.s$cleanName] {
                      .pow.s$cleanName configure  -bg $value
                   }
                   if [winfo exists .pow.ms$cleanName] {
                      .pow.ms$cleanName configure -bg $value
                   }
                   .pow.pow lower ${gn}bkg
                }
	    }

            xdimdisp -
            ydimdisp -
	    xmargin -
	    ymargin -
            zoomed -
	    FixedAspect {
		set powPlotParam($option,$gn) $value
	    }

	    titleString -
	    titleAnchor -
	    titlePosition -
	    xNumTicks -
	    yNumTicks -
            flipD -
	    xlabel -
	    ylabel -
	    xunits -
	    yunits {
	       set powPlotParam($option,$gn) $value
	       set doAction(adornGraph) 1
	    }

	    xTickLength -
	    yTickLength -
	    xLabelTicks -
	    yLabelTicks -
	    tickLabels	-
	    tickFormatCmdX -
	    tickFormatCmdY -
	    xTickScal -
	    yTickScal {
		set powPlotParam($option,$gn) $value
		set doAction(adornGraph) 1
		set doAction(redrawGraphHandles) 1
	    }

            wcsName {
		set powPlotParam(wcsName,$gn) $value
            }

	    # Grid and GridLines are same option.  Grid is archaic.
	    Grid -
	    GridLines {
		set powPlotParam(GridLines,$gn) $value
		set doAction(adornGraph) 1
	    }
	    GridColor {
		set clr [powColorToHex $value]
		set powPlotParam(GridColor,$gn) $clr
		if [winfo exists .pow.pow] {
		   .pow.pow itemconfig ${gn}grid -fill $clr
		}
	    }
	    GridDash {
		set powPlotParam(GridDash,$gn) $value
		if [winfo exists .pow.pow] {
		   .pow.pow itemconfig ${gn}grid -dash $value
		}
	    }

	    default {
		error "Unknown option $option"
	    }
	}
    }

    # Perform delayed actions if graph (and window) already exists

    if { [powListGraphs $gn] && [winfo exists .pow.pow] \
          && [.pow.pow find withtag ${gn}box]!="" } {

       if { [info exists doAction(adornGraph)] } {
          powAdornGraph $gn .pow.pow
       }
       if { [info exists doAction(redrawGraphHandles)] } {
          powRedrawGraphHandles $gn
       }

       if { $gn == $currgn } {
          powUpdateGraphMenuOptions
       }
    }

}

proc powColorToHex { color } {
#puts "powColorToHex start"
   global powCurveParam

   if { [string index $color 0]!="#" } {
      set idx [lsearch $powCurveParam(allColors,powDef) $color]
      if { $idx>-1 } {
	 incr idx
	 set color [lindex $powCurveParam(allColors,powDef) $idx]
      }
   }
   return $color
}

proc powToplevel {topwin refwin {options ""}} {
#this implements what *should* be default behavior for X colormap handling.  
#Apparently the evil of Xlib colormap handling is contagious.
#if we're running in the plugin or in single window mode, this creates
#a frame instead of a toplevel.
    global powFrameForTop powbg powContainer

#puts "powToplevel start"
    
    if {!$powFrameForTop} {
#puts "powToplevel 1"
#puts "refwin: $refwin"
#puts "options: $options"
       catch { eval {toplevel $topwin -colormap $refwin \
             -visual [list [winfo visual $refwin] [winfo depth $refwin]]} \
             $options } err
#puts "err: $err"
       focus $topwin
    } else {
#in a tclet just use frames
	frame $topwin -bg $powbg 

	if [info exists powContainer] {
	    grid $topwin -sticky news -in $powContainer
	} else {
	    grid $topwin -sticky news
	}
    }
}
    

proc powReditherImage {gn img {canvas .pow.pow} } {
#This recalculates the colors/intensities in the base Photo image.
#The call to powMagImage is necessary to make them appear.
    global powPseudoImages powImageParam

#puts "powReditherImage start"
#We could redither pseudocolors, but it isn't necessary
    if { ! $powPseudoImages } {
        powReditherPhotoBlock $img \
                $powImageParam(RBmin${img},$gn) $powImageParam(RBmax${img},$gn)
    }

    if { [$canvas find withtag ${img}disp${gn}]!="" } {
       powMagImage $gn $img $canvas
    }
}


proc powReditherImages {gn img} {
#puts "powReditherImages start"
#This checks whether we need to redither both a color bar and an image
#It also redithers the scope box if it is the current graph
    global powPseudoImages powPlotParam powImageParam currgn powGUI

#We could redither pseudocolors, but it isn't necessary
#    if $powPseudoImages return 

# Redither this image

    powReditherImage $gn $img

# Redither Scope

    if { $powGUI && $currgn == $gn && \
	  [.pow.scope find withtag ${img}disp${gn}scope]!="" } {
       powReditherImage ${gn}scope $img .pow.scope
    }

# Redither original image or colorbar if other exists

    foreach {gn2 img2} [powGetColorbarLink $gn $img] {}
    if { $gn2 != "" } {
       powReditherImage $gn2 $img2
    }
}


proc powCmapInvert { } {
#puts "powCmapInvert start"
global powPseudoImages curr_img
    if $powPseudoImages {
	$curr_img colormap invert
    } else {
	powPhotoColorTable invert
    }
}

proc powSetRanges { gn img min max } {
#puts "powSetRanges start"
   global powGUI currgn powPlotParam powImageParam

   powSetRange $gn $img $min $max

# Set Scope

    if { $powGUI && $currgn == $gn } {
	powSetRange ${gn}scope $img $min $max
    }

# Set original image or colorbar if other exists

    foreach {gn2 img2} [powGetColorbarLink $gn $img] {}

    if { $gn2 != "" } {

	powSetRange $gn2 $img2 $min $max

        # Also need to update RB for undisplayed scope so must break modularity!
        if { $powGUI } {
	   set powImageParam(RBmin${img2},${gn2}scope) $min
	   set powImageParam(RBmax${img2},${gn2}scope) $max
	}

    }
}

proc powSetRange { gn img min max } {
#puts "powSetRange start"
   global powPseudoImages powImageParam powGUI

   set powImageParam(RBmin${img},$gn) $min
   set powImageParam(RBmax${img},$gn) $max

   if {$powPseudoImages} {
      ${img}disp${gn} range $min $max
   } else {
      if { $powGUI && [regexp scope$ $gn] } {
	 powReditherImage $gn $img .pow.scope
      } else {
	 powReditherImage $gn $img .pow.pow
      }
   }
}

proc powSetColorTable { gn img } { 
#puts "powSetColorTable start"
    global powPseudoImages currimg currgn powImageParam

    powSetLut $gn $img $powImageParam(scale${img},$gn)
    if {$powPseudoImages}  {
       ${img}disp${gn} colormap $powImageParam(colormap${img},$gn)
    } else {
	powPhotoColorTable $powImageParam(colormap${img},$gn)
    }
    invert_cmap_if_flag_set $gn $img
}

proc powGenerateReplotCascade { } {
#puts "powGenerateReplotCascade start"
    global powPlotParam powOrderedGraphList

    set idx 0
    .pow.mbar.edit.replot delete 0 end
    foreach gn $powOrderedGraphList {
       incr idx
       if { $gn=="" } continue
       if { [.pow.pow find withtag ${gn}box]=="" } {
          set state normal
       } else {
          set state disabled
       }
       set title $powPlotParam(titleString,$gn)
       if { $title=="" } {
          set title "Untitled $idx"
       }
       .pow.mbar.edit.replot add command -label $title \
             -command "powMapGraph $gn" -state $state
    }
}

proc powGenerateUnmapCascade { } {
#puts "powGenerateUnmapCascade start"
    global powPlotParam powOrderedGraphList

    set idx 0
    .pow.mbar.edit.unmap delete 0 end
    foreach gn $powOrderedGraphList {
       incr idx
       if { $gn=="" } continue
       if { [.pow.pow find withtag ${gn}box]=="" } {
          set state disabled
       } else {
          set state normal
       }
       set title $powPlotParam(titleString,$gn)
       if { $title=="" } {
          set title "Untitled $idx"
       }
       .pow.mbar.edit.unmap add command -label $title \
             -command "powUnmapGraph $gn" -state $state
    }
}

proc powGenerateMergeCascade { } {
#puts "powGenerateMergeCascade start"
    global currgn powPlotParam powOrderedGraphList

    set idx 0
    .pow.mbar.edit.merge delete 0 end
    foreach gn $powOrderedGraphList {
       incr idx
       if { $gn=="" } continue
       if { $gn==$currgn } {
          set state disabled
       } else {
          set state normal
       }
       set title $powPlotParam(titleString,$gn)
       if { $title=="" } {
          set title "Untitled $idx"
       }
       .pow.mbar.edit.merge add command -label $title \
             -command "powMergeGraphs $gn" -state $state
    }
}

proc powProcessClick { x y binding} {
#puts "powProcessClick start binding: $binding"
    global powClickCallback

    set gn [powWhereAmI [.pow.pow canvasx $x] [.pow.pow canvasy $y]] 
    if {$gn != "NULL"} {
#	powSelectGraph $gn
	if [info exists powClickCallback] {
	    set gcoords [powCanvasToGraph $gn [.pow.pow canvasx $x] [.pow.pow canvasy $y]]
	    set rx [lindex $gcoords 0]
	    set ry [lindex $gcoords 1]
	    $powClickCallback $gn $rx $ry $binding
	}
    }
#puts "powProcessClick end"
}

proc determineSearchPath { data } {
     global tcl_platform 
     global env

     set searchPath ""
     set findFlag false
     set delimiter ":"
     if { $tcl_platform(platform)=="windows"  } {
        return "$env(FITSVIEWER_LIBRARY)/../../gs6.52/bin;$env(FITSVIEWER_LIBRARY)/../../gs6.52/fonts"
     }

     set tokens [split $data "\n"]

     for {set i 0} {$i < [llength $tokens]} {incr i} {
         if { $findFlag == "true" } {
            if { [string first "For more" [lindex $tokens $i]] >= 0 } {
               break
            }
            set pathToken [split [lindex $tokens $i] $delimiter]
            foreach path $pathToken {
                set path [string trim $path]
                if { $path == "" } continue
                set lastpath $path
                set searchPath [format "%s%s%s" $searchPath $delimiter $path]
            }
            set searchPath [format "%s%s%s" $searchPath $delimiter [file dirname $lastpath]/fonts]
            break
         }

         if { [string first "Search path" [lindex $tokens $i]] >= 0 } {
            set findFlag true
            continue
         }
     }
     return $searchPath
}
	
proc powRescaleBox { } {
    global powbg curr_img currimg currgn powRBmin powRBmax powbg powDWP
    global powImageParam powHisto

    if { [winfo exists ${powDWP}powRB] } { destroy ${powDWP}powRB }
    if {![info exists curr_img]} {
	error "You must first select an image"
	return
    }

    powToplevel ${powDWP}powRB .pow "-bg $powbg -class \"POW Rescale\"" 
    bind ${powDWP}powRB <<CloseWindow>> "destroy ${powDWP}"
    bind ${powDWP}powRB <Tab> "+powUpdateHisto ?"
    bind ${powDWP}powRB <Return> "+powUpdateHisto ?"
    
    label ${powDWP}powRB.min -text "Image min: $powRBmin($currimg)" -bg $powbg
    label ${powDWP}powRB.max -text "Image max: $powRBmax($currimg)" -bg $powbg

    label ${powDWP}powRB.cmin -text "Current min:" -bg $powbg
    entry ${powDWP}powRB.ecmin -relief sunken -bg $powbg \
	  -textvariable powImageParam(RBmin${currimg},$currgn) -bd 1

    label ${powDWP}powRB.cmax -text "Current max:" -bg $powbg
    entry ${powDWP}powRB.ecmax -relief sunken -bg $powbg \
	  -textvariable powImageParam(RBmax${currimg},$currgn) -bd 1
    
    frame ${powDWP}powRB.buttonFrame -bg $powbg
    button ${powDWP}powRB.apply -text Apply -command {
       powUpdateHisto 0
#      powSetRanges $currgn $currimg
       powCmds::colormap scale $powImageParam(scale${currimg},$currgn) \
	     $powImageParam(RBmin${currimg},$currgn) \
	     $powImageParam(RBmax${currimg},$currgn)
       foreach {gn2 img2} [powGetColorbarLink $currgn $currimg] {}
       if { $gn2 != "" || $img2 != ""   } {
          powDeleteImage $gn2 $img2
          powColorbar
       }
    }  -bg $powbg
    button ${powDWP}powRB.reset -text Reset -command {
#      powSetRanges $currgn $currimg
       powCmds::colormap scale $powImageParam(scale${currimg},$currgn) \
	     $powRBmin($currimg) $powRBmax($currimg)
       powUpdateHisto ?
       foreach {gn2 img2} [powGetColorbarLink $currgn $currimg] {}
       if { $gn2 != "" || $img2 != ""   } {
          powDeleteImage $gn2 $img2
          powColorbar
       }
    }  -bg $powbg
    button ${powDWP}powRB.exit -text Exit -command {destroy ${powDWP}powRB} \
            -bg $powbg

    grid configure ${powDWP}powRB.min -row 0 -column 0 -columnspan 2 -sticky w
    grid configure ${powDWP}powRB.max -row 0 -column 2 -columnspan 2 -sticky w
    grid configure ${powDWP}powRB.cmin -row 1 -column 0  -sticky w
    grid configure ${powDWP}powRB.ecmin -row 1 -column 1 -sticky w
    grid configure ${powDWP}powRB.cmax -row 1 -column 2  -sticky w
    grid configure ${powDWP}powRB.ecmax -row 1 -column 3  -sticky w
    grid configure ${powDWP}powRB.buttonFrame -row 4 -column 0 -columnspan 4 -sticky ew
    grid configure ${powDWP}powRB.apply -row 0 -column 0 -in ${powDWP}powRB.buttonFrame -sticky w
    grid configure ${powDWP}powRB.reset -row 0 -column 1 -in ${powDWP}powRB.buttonFrame
    grid configure ${powDWP}powRB.exit -row 0 -column 2 -in ${powDWP}powRB.buttonFrame -sticky e


    frame ${powDWP}powRB.histo -bg $powbg
    grid ${powDWP}powRB.histo -row 2 -column 0 -columnspan 4 \
            -padx 3 -pady 5 -sticky news
    
    canvas ${powDWP}powRB.histo.grph -relief sunken -width 256 -height 150 \
            -bg $powbg -bd 3
    grid ${powDWP}powRB.histo.grph -row 1 -column 2 -rowspan 3
    powBindBtn <<BtnPress>> "bind ${powDWP}powRB.histo.grph " \
            { powDragHistoBounds b1 %x; powDragHistoBounds b2 %x } \
            { powDragHistoBounds b1 %x } \
            { powUpdateHisto ? }
    
    frame ${powDWP}powRB.histo.scale -bg $powbg
    grid ${powDWP}powRB.histo.scale -row 1 -column 0 -rowspan 3

    radiobutton ${powDWP}powRB.histo.scale.linear \
                -text "Linear" -bg $powbg -variable powHisto(scale) \
                -value linear -highlightthickness 0 -command { set powHisto(scale) linear ; powUpdateHisto 1 }

    radiobutton ${powDWP}powRB.histo.scale.logY \
                -text "LogY" -bg $powbg -variable powHisto(scale) \
                -value log -highlightthickness 0 -command { set powHisto(scale) log; powUpdateHisto 1 }

    grid ${powDWP}powRB.histo.scale.linear -row 0 -column 0 -sticky nsw
    grid ${powDWP}powRB.histo.scale.logY   -row 2 -column 0 -sticky nsw

    canvas ${powDWP}powRB.histo.scale.list -width 50 -height 150 \
            -bg $powbg -bd 0 -highlightthickness 0
    grid ${powDWP}powRB.histo.scale.list -row 0 -column 1 -rowspan 3 -sticky ns
    
    canvas ${powDWP}powRB.histo.bnds -relief flat -width 266 -height 6 \
            -bg $powbg -bd 0 -highlightthickness 0
    grid ${powDWP}powRB.histo.bnds -row 0 -column 2
    
    frame ${powDWP}powRB.histo.histbutt -bg $powbg
    grid ${powDWP}powRB.histo.histbutt -row 4 -column 2

    radiobutton ${powDWP}powRB.histo.histbutt.fullrange \
            -text "Full Range" -bg $powbg -variable powHisto(range) \
            -value full -highlightthickness 0 -command {
        after idle { powUpdateHisto 1 }
    }
    radiobutton ${powDWP}powRB.histo.histbutt.currrange \
            -text "Current Range" -bg $powbg -variable powHisto(range) \
            -value curr -highlightthickness 0 -command {
        after idle { powUpdateHisto 1 }
    }
    grid ${powDWP}powRB.histo.histbutt.fullrange -row 1 -column 1
    grid ${powDWP}powRB.histo.histbutt.currrange -row 1 -column 2

    set powHisto(image) $currimg
    set powHisto(graph) $currgn
    set powHisto(min) $powRBmin($currimg)
    set powHisto(max) $powRBmax($currimg)
    set powHisto(range) full
    set powHisto(scale) linear

    powUpdateHisto 1
}

proc powUpdateHisto { new {scale linear} } {
#puts "powUpdateHisto start"
    global powDWP powRBmin powRBmax
    global powImageParam powHisto

    if { ![winfo exists ${powDWP}powRB] } { return }

    set img $powHisto(image)
    set gn  $powHisto(graph)

    if { $new=="?" } {
        if { $powHisto(range)!="full" } {
            set new 1
        } else {
            set new 0
        }
    }

    if { $new } {
        if { $powHisto(range)=="full" } {
            set powHisto(min) $powRBmin($img)
            set powHisto(max) $powRBmax($img)
        } else {
            set powHisto(min) $powImageParam(RBmin${img},$gn)
            set powHisto(max) $powImageParam(RBmax${img},$gn)
        }
    }

    set min $powImageParam(RBmin${img},$gn)
    set max $powImageParam(RBmax${img},$gn)
    if { $min > $max } {
        set tmp $min
        set min $max
        set max $tmp
        set powImageParam(RBmin${img},$gn) $min
        set powImageParam(RBmax${img},$gn) $max
    }

    set scale [expr $powHisto(max) - $powHisto(min) ]
    if { $scale != 0.0 } {
        set scale [expr 255.0 / $scale]
        set minBounds [expr round($scale * ($min - $powHisto(min)))]
        set maxBounds [expr round($scale * ($max - $powHisto(min)))]
    } else {
        set minBounds 0
        set maxBounds 255
    }
    
    if { $new } {
        set powHisto(b1) $min
        set powHisto(b2) $max

        ${powDWP}powRB.histo.bnds delete all

        ${powDWP}powRB.histo.bnds create polygon \
                [expr $minBounds+0] 0 \
                [expr $minBounds+10] 0 \
                [expr $minBounds+5] 5 \
                -fill black -tags b1
    
        ${powDWP}powRB.histo.bnds create polygon \
                [expr $maxBounds+0] 0 \
                [expr $maxBounds+10] 0 \
                [expr $maxBounds+5] 5 \
                -fill black -tags b2

        powBindBtn <ButtonPress-1> "${powDWP}powRB.histo.bnds bind b1" \
                { powDragHistoBounds b1 %x } \
                { powDragHistoBounds b1 %x } \
                { powUpdateHisto ? }
	   
        powBindBtn <ButtonPress-1> "${powDWP}powRB.histo.bnds bind b2" \
                { powDragHistoBounds b2 %x } \
                { powDragHistoBounds b2 %x } \
                { powUpdateHisto ? }
	   
        ${powDWP}powRB.histo.bnds bind b1 <Enter> \
                { ${powDWP}powRB.histo.bnds itemconfig b1 -fill red }
        ${powDWP}powRB.histo.bnds bind b1 <Leave> \
                { ${powDWP}powRB.histo.bnds itemconfig b1 -fill black }
        ${powDWP}powRB.histo.bnds bind b2 <Enter> \
                { ${powDWP}powRB.histo.bnds itemconfig b2 -fill red }
        ${powDWP}powRB.histo.bnds bind b2 <Leave> \
                { ${powDWP}powRB.histo.bnds itemconfig b2 -fill black }

    } else {

        if { $powHisto(b1) < $powHisto(b2) } {
            set minTag b1
            set maxTag b2
        } else {
            set minTag b2
            set maxTag b1
        }
        set powHisto($minTag) $min
        set powHisto($maxTag) $max
        ${powDWP}powRB.histo.bnds coords $minTag \
                [expr $minBounds+0] 0 \
                [expr $minBounds+10] 0 \
                [expr $minBounds+5] 5

        ${powDWP}powRB.histo.bnds coords $maxTag \
                [expr $maxBounds+0] 0 \
                [expr $maxBounds+10] 0 \
                [expr $maxBounds+5] 5
    }
    
    if { $new } {
        ${powDWP}powRB.histo.scale.list delete scale_0_Text scale_0_Line \
                                               scale_H_Text scale_H_line \
                                               scale_T_Text scale_T_line
        set histo [powGetHisto $img $powHisto(min) $powHisto(max)]
        set pixmax 0
        set pixmin 9999999999
        foreach val [lrange $histo 1 254] {
            if { $val>$pixmax } {
                set pixmax $val
            }
            if { $val > 0 && $val < $pixmin } {
                set pixmin $val
            }
        }

        if { $pixmax==0 } {
            set pixmax 1
        }
        ${powDWP}powRB.histo.grph delete all
        set i 4
        set sList {}
        foreach val $histo {
            set scaledVal [expr $val * 149 / $pixmax]

            if { $powHisto(scale) == "log" } {
               if { $val > 0 } {
                  set scaledVal [expr log10($val) * 149.0 / log10($pixmax)]
               } else {
                  set scaledVal 0
               }
            }

            lappend sList $scaledVal

            if { $scaledVal > 153 } {
               set scaledVal 153
            }

            ${powDWP}powRB.histo.grph create line $i 153 $i [expr 153-$scaledVal] -fill blue

            incr i
        }
        ${powDWP}powRB.histo.scale.list create text 20 145 -fill red \
                      -text "0" -tag scale_0_Text
        ${powDWP}powRB.histo.scale.list create line 45 149 50 149 -fill red \
                      -tag scale_0_Line

        if { $powHisto(scale) == "log" } {
           ${powDWP}powRB.histo.scale.list create text 20 5 -fill red \
                         -text [format "%5.3f" [expr log10($pixmax)]] -tag scale_T_Text
           ${powDWP}powRB.histo.scale.list create text 20 75 -fill red \
                         -text [format "%5.3f" [expr log10($pixmax/2.0)]] -tag scale_H_Text
        } else {
           ${powDWP}powRB.histo.scale.list create text 20 5 -fill red \
                      -text $pixmax -tag scale_T_Text
           ${powDWP}powRB.histo.scale.list create text 20 75 -fill red \
                      -text [expr $pixmax/2] -tag scale_H_Text
        }
        ${powDWP}powRB.histo.scale.list create line 45 0 50 0 -fill red \
                      -tag scale_T_Line
        ${powDWP}powRB.histo.scale.list create line 45 75 50 75 -fill red \
                      -tag scale_H_Line
    }

    ${powDWP}powRB.histo.grph addtag blackLines enclosed \
            0 0 [expr $minBounds+3.5] 160
    ${powDWP}powRB.histo.grph addtag greyLines enclosed \
            [expr $minBounds+3.5] 0 [expr $maxBounds+4.5] 160
    ${powDWP}powRB.histo.grph addtag whiteLines enclosed \
            [expr $maxBounds+4.5] 0 270 160

    ${powDWP}powRB.histo.grph itemconfig blackLines -fill black
    ${powDWP}powRB.histo.grph itemconfig greyLines  -fill blue
    ${powDWP}powRB.histo.grph itemconfig whiteLines -fill white

    ${powDWP}powRB.histo.grph dtag blackLines
    ${powDWP}powRB.histo.grph dtag greyLines
    ${powDWP}powRB.histo.grph dtag whiteLines
}

proc powDragHistoBounds { tag x } {
#puts "powDragHistoBounds start"
    global powHisto powImageParam

    set img $powHisto(image)
    set gn  $powHisto(graph)

    set scale [expr $powHisto(max) - $powHisto(min) ]
    if { $scale == 0.0 } {
        return
    }
    set scale [expr 255.0 / $scale]
    set val [expr ($x-5)/$scale + $powHisto(min)]
    set powHisto($tag) $val

    if { $powHisto(b1) < $powHisto(b2) } {
        set powImageParam(RBmin${img},$gn) $powHisto(b1)
        set powImageParam(RBmax${img},$gn) $powHisto(b2)
    } else {
        set powImageParam(RBmin${img},$gn) $powHisto(b2)
        set powImageParam(RBmax${img},$gn) $powHisto(b1)
    }

    powUpdateHisto 0
}

proc powSetMagstepBox { } {
#puts "powSetMagstepBox start"
    global powPlotParam currgn powbg powDWP
    global powXMagstep powYMagstep powSaveXMagstep powSaveYMagstep

    if { $currgn=="powDef" } {
	error "You must first select a graph"
	return
    }
    if { [winfo exists ${powDWP}magstep] } {
	raise ${powDWP}magstep
	return
    }
    powToplevel ${powDWP}magstep .pow "-bg $powbg -class \"POW Magstep\"" 
    bind ${powDWP}magstep <<CloseWindow>> "destroy ${powDWP}"

    set powXMagstep $powPlotParam(xmagstep,$currgn) 
    set powYMagstep $powPlotParam(ymagstep,$currgn) 
    set powSaveXMagstep $powXMagstep
    set powSaveYMagstep $powYMagstep

    label ${powDWP}magstep.label -text "Current magstep:" -bg $powbg
    label ${powDWP}magstep.xlabel -text "X " -bg $powbg
    label ${powDWP}magstep.ylabel -text "Y " -bg $powbg
    entry ${powDWP}magstep.xmagstep -textvariable powXMagstep \
	    -relief sunken -bg $powbg -bd 1
    entry ${powDWP}magstep.ymagstep -textvariable powYMagstep \
	    -relief sunken -bg $powbg -bd 1
    
    frame ${powDWP}magstep.buttonFrame -bg $powbg
    button ${powDWP}magstep.apply -text Apply -command \
	{powMagGraph $currgn $powXMagstep $powYMagstep; \
	     set powXMagstep $powPlotParam(xmagstep,$currgn); \
	     set powYMagstep $powPlotParam(ymagstep,$currgn)}  -bg $powbg
    button ${powDWP}magstep.reset -text Reset -command \
	{set powXMagstep $powSaveXMagstep; \
	 set powYMagstep $powSaveYMagstep; \
	 powMagGraph $currgn $powXMagstep $powYMagstep}  -bg $powbg
    button ${powDWP}magstep.exit -text Exit -command {destroy ${powDWP}magstep}  -bg $powbg

    grid configure ${powDWP}magstep.label -row 0 -column 0 -sticky w
    grid configure ${powDWP}magstep.xlabel -row 0 -column 1
    grid configure ${powDWP}magstep.ylabel -row 1 -column 1
    grid configure ${powDWP}magstep.xmagstep -row 0 -column 2 -sticky w
    grid configure ${powDWP}magstep.ymagstep -row 1 -column 2 -sticky w
    grid configure ${powDWP}magstep.buttonFrame -row 2 -column 0 -columnspan 3 -sticky ew
    grid configure ${powDWP}magstep.apply -row 0 -column 0 -in ${powDWP}magstep.buttonFrame -sticky w
    grid configure ${powDWP}magstep.reset -row 0 -column 1 -in ${powDWP}magstep.buttonFrame
    grid configure ${powDWP}magstep.exit -row 0 -column 2 -in ${powDWP}magstep.buttonFrame -sticky e
}


proc powSetGraphSize { } {
#puts "powSetGraphSize start"
    global powPlotParam currgn powbg powFrameForTop
    global powXDim powYDim powSaveXDim powSaveYDim powDWP g_titleFont

    if { $currgn=="powDef" } {
	error "You must first select a graph"
	return
    }
    if { [winfo exists ${powDWP}dim] } {
	raise ${powDWP}dim
	return
    }
    powToplevel ${powDWP}dim .pow "-bg $powbg -class \"POW Dim\"" 
    bind ${powDWP}dim <<CloseWindow>> "destroy ${powDWP}"

    if {!$powFrameForTop} {
	wm title ${powDWP}dim "Set Graph Dimensions"
    }

    set powXDim [tagXdim .pow.pow ${currgn}box]
    set powYDim [tagYdim .pow.pow ${currgn}box]
    set powSaveXDim $powXDim
    set powSaveYDim $powYDim

    label ${powDWP}dim.label -text "Current Dim:" -bg $powbg -font g_titleFont
    label ${powDWP}dim.xlabel -text "X " -bg $powbg -font g_titleFont
    label ${powDWP}dim.ylabel -text "Y " -bg $powbg -font g_titleFont
    entry ${powDWP}dim.xDim -textvariable powXDim \
	    -relief sunken -bg $powbg -width 10 -font g_titleFont -bd 1
    entry ${powDWP}dim.yDim -textvariable powYDim \
	    -relief sunken -bg $powbg -width 10 -font g_titleFont -bd 1
    
    frame ${powDWP}dim.buttonFrame -bg $powbg
    button ${powDWP}dim.apply -text Apply -command \
	{powStretchGraphToSize $currgn $powXDim $powYDim}  -bg $powbg -font g_titleFont
    button ${powDWP}dim.reset -text Reset -command \
	{set powXDim $powSaveXDim; \
	 set powYDim $powSaveYDim; \
	 powStretchGraphToSize $currgn $powXDim $powYDim}  -bg $powbg -font g_titleFont
    button ${powDWP}dim.exit -text Exit -command {destroy ${powDWP}dim}  -bg $powbg -font g_titleFont

    grid configure ${powDWP}dim.label -row 0 -column 0 -sticky w
    grid configure ${powDWP}dim.xlabel -row 0 -column 1
    grid configure ${powDWP}dim.ylabel -row 1 -column 1
    grid configure ${powDWP}dim.xDim -row 0 -column 2 -sticky w
    grid configure ${powDWP}dim.yDim -row 1 -column 2 -sticky w
    grid configure ${powDWP}dim.buttonFrame -row 2 -column 0 -columnspan 3 -sticky ew
    grid configure ${powDWP}dim.apply -row 0 -column 0 -in ${powDWP}dim.buttonFrame -sticky w
    grid configure ${powDWP}dim.reset -row 0 -column 1 -in ${powDWP}dim.buttonFrame
    grid configure ${powDWP}dim.exit -row 0 -column 2 -in ${powDWP}dim.buttonFrame -sticky e
}

proc powSave { {inputFile {}} } {
     global powSelectDirectory
     global powGraphSelection
     global powbg powOutputFileName
     global powStretch powOutputPaperSize
     global powConvertFormat powConvertFunction
     global powHandles powDWP g_titleFont
     global powPaperDefXsizeInch powPaperDefYsizeInch
     global powPaperDefXsizePixel powPaperDefYsizePixel
     global powOutputPaperXsizeInch powOutputPaperYsizeInch
     global powOutputPaperXsizePixel powOutputPaperYsizePixel
     global powPlacement powPostOrient
     global powPaperSizeSelected powPixelToInchRatio

     global ghostScript
     global powOutputFileType
     global tcl_platform
     global searchPath
     global tcl_platform

     set fileNameList {}
     if { [llength $inputFile] == 0 } {
        set fileNameList [powAssemblePSfile]
     } else {
        lappend fileNameList $inputFile
     }

     if ![info exists powOutputFileName] {
        powSelectConvertFormat "postscript - Postscript Files"
     }
 
     set idx 0
     foreach fileName $fileNameList {
        # Save to any graph format 
        set outputName $powOutputFileName

        if { [llength $fileNameList] > 1 } {
           set token [split $powOutputFileName "."]
           set outputName [format "%s_%s.%s" [lindex $token 0] $idx [lindex $powOutputFileType 2]]
        }

        set realDirectory $powSelectDirectory
        if { $tcl_platform(platform) == "windows" } {
           set powSelectDirectory [string trim $powSelectDirectory "{}/"]
           set realDirectory [_changeWinDirectoryToUnixFormat $powSelectDirectory]
        }

        if { [lindex $powOutputFileType 2] != "ps" } {
           set errorFlag [ catch {
               exec $ghostScript -sDEVICE=[lindex $powOutputFileType 1] \
                                 -dNOPAUSE -dBATCH -dQUIET \
                                 -sPAPERSIZE=[string tolower $powPaperSizeSelected] \
                                 -I$searchPath \
                                 -sOutputFile=$realDirectory/$outputName $fileName
           } err ]
#puts "err: <$err>"

        } else {
           set errorFlag [ catch {
               file copy -force $fileName $realDirectory/$outputName
           } err ]
        }

        incr idx
        if { $idx >= [llength $fileNameList] } {
           # tk_messageBox -icon info -parent .pow -type ok -message "Successful save graphs to $powSelectDirectory."
        }
     }
}

proc powSaveAs {} {
     global powDWP powbg
     global g_titleFont
     global powConvertFunction powOutputFileName
     global powOutputFileType powSelectDirectory
     global currentPreviewGraph
     global powGraphCoordList powGraphSelection

     if [winfo exists ${powDWP}saveAsSetup] {
        wm deiconify ${powDWP}saveAsSetup
        ${powDWP}saveAsSetup.directory.saveInEntry delete 0 end
        if { [${powDWP}print.option.direntry get] != "" } {
           ${powDWP}saveAsSetup.directory.saveInEntry insert end [${powDWP}print.option.direntry get]
        } else {
           ${powDWP}saveAsSetup.directory.saveInEntry insert end [pwd]
        }
        ${powDWP}saveAsSetup.file.fileNameEntry delete 0 end
        ${powDWP}saveAsSetup.file.fileNameEntry insert end $powOutputFileName

        set token [split $powOutputFileName "."]
        set powOutputFileType {postscript pswrite ps "Postscript Files"}

        foreach cvf $powConvertFunction {
            if { [lindex $cvf 2] == [lindex $token 1] } {
               set powOutputFileType $cvf
               tixSetSilent ${powDWP}saveAsSetup.file.convertType "[lindex $cvf 0] - [lindex $cvf 3]"
               break
            } 
        }

        [${powDWP}saveAsSetup.directory.directoryTree subwidget hlist] delete all
        destroy ${powDWP}saveAsSetup.directory.directoryTree
        tixDirTree ${powDWP}saveAsSetup.directory.directoryTree \
                   -value [${powDWP}saveAsSetup.directory.saveInEntry get] \
                   -browsecmd {powSelectDir} -command {powSelectDir} \
                   -options { \
                        hlist.foreground black \
                        hlist.background white \
                        hlist.font g_titleFont \
                        hlist.width 40 \
                   }
        grid ${powDWP}saveAsSetup.directory.directoryTree -row 3 -column 0 -columnspan 5 -rowspan 10 -sticky news
        return
     }

     powToplevel ${powDWP}saveAsSetup .pow "-bg $powbg -class \"POW Print\""
     bind ${powDWP}saveAsSetup <<CloseWindow>> "destroy ${powDWP}"
     wm title ${powDWP}saveAsSetup "Save POW Image/Plot As"

     grid rowconfigure ${powDWP}saveAsSetup 2 -weight 1
     grid columnconfigure ${powDWP}saveAsSetup 0 -weight 1
     grid columnconfigure ${powDWP}saveAsSetup 1 -weight 1

     frame ${powDWP}saveAsSetup.directory             -bg $powbg -bd 2 -relief ridge
     label ${powDWP}saveAsSetup.directory.dirLabel    -text "Directory: " -bg $powbg -font g_titleFont
     entry ${powDWP}saveAsSetup.directory.saveInEntry -width 35 -bg white -font g_titleFont
   
     ${powDWP}saveAsSetup.directory.saveInEntry delete 0 end
     if { [${powDWP}print.option.direntry get] != "" } {
        ${powDWP}saveAsSetup.directory.saveInEntry insert end [${powDWP}print.option.direntry get]
        set directoryValue [${powDWP}print.option.direntry get]
     } else {
        ${powDWP}saveAsSetup.directory.saveInEntry insert end [pwd]
        set directoryValue [pwd]
     }

     tixDirTree ${powDWP}saveAsSetup.directory.directoryTree -value $directoryValue \
                -browsecmd {powSelectDir} -command {powSelectDir} \
                -options { \
                     hlist.foreground black \
                     hlist.background white \
                     hlist.font g_titleFont \
                     hlist.width 40 \
                }

     set powSelectDirectory $directoryValue

     grid ${powDWP}saveAsSetup.directory               -row 2 -column 0 -sticky news -columnspan 5 -rowspan 10
     grid ${powDWP}saveAsSetup.directory.dirLabel      -row 2 -column 0 -sticky nw
     grid ${powDWP}saveAsSetup.directory.saveInEntry   -row 2 -column 1 -sticky new
     grid ${powDWP}saveAsSetup.directory.directoryTree -row 3 -column 0 -columnspan 5 -rowspan 10 -sticky news

     grid columnconfigure ${powDWP}saveAsSetup.directory 1 -weight 1
     grid rowconfigure ${powDWP}saveAsSetup.directory 3 -weight 1

     bind ${powDWP}saveAsSetup.directory.saveInEntry <Return> {
          [${powDWP}saveAsSetup.directory.directoryTree subwidget hlist] delete all
          destroy ${powDWP}saveAsSetup.directory.directoryTree
          tixDirTree ${powDWP}saveAsSetup.directory.directoryTree \
                     -value [${powDWP}saveAsSetup.directory.saveInEntry get] \
                     -browsecmd {powSelectDir} -command {powSelectDir} \
                     -options { \
                          hlist.foreground black \
                          hlist.background white \
                          hlist.font g_titleFont \
                          hlist.width 40 \
                     }
          grid ${powDWP}saveAsSetup.directory.directoryTree -row 3 -column 0 -columnspan 5 -rowspan 10 -sticky news
     }

     frame ${powDWP}saveAsSetup.file               -bg $powbg
     label ${powDWP}saveAsSetup.file.fileNameLbl   -text "File name:" -bg $powbg -font g_titleFont
     entry ${powDWP}saveAsSetup.file.fileNameEntry -width 35 -bg white -font g_titleFont
     tixComboBox ${powDWP}saveAsSetup.file.convertType -editable true \
                                                       -label "Save as type:" \
                                                   -options { \
                                                        listbox.height 4 \
                                                        label.font g_titleFont \
                                                        listbox.font g_titleFont \
                                                        entry.font g_titleFont \
                                                        entry.background white \
                                                        entry.width 30 \
                                                        entry.ipady 5 \
                                                   } \
                                                   -command powSelectConvertFormat

     foreach functionList $powConvertFunction {
         set formatStr [format "%s - %s" [lindex $functionList 0] [lindex $functionList 3]]
         ${powDWP}saveAsSetup.file.convertType insert end $formatStr
     }

     ${powDWP}saveAsSetup.file.fileNameEntry insert end $powOutputFileName
     set token [split $powOutputFileName "."]
     set powOutputFileType {postscript pswrite ps "Postscript Files"}

     foreach cvf $powConvertFunction {
         if { [lindex $cvf 2] == [lindex $token 1] } {
            set powOutputFileType $cvf
            tixSetSilent ${powDWP}saveAsSetup.file.convertType "[lindex $cvf 0] - [lindex $cvf 3]"
            break
         } 
     }

     grid ${powDWP}saveAsSetup.file               -row 13 -column 0 -sticky news -columnspan 5 -rowspan 2
     grid ${powDWP}saveAsSetup.file.fileNameLbl   -row 0 -column 0 -sticky nw
     grid ${powDWP}saveAsSetup.file.fileNameEntry -row 0 -column 1 -sticky new
     grid ${powDWP}saveAsSetup.file.convertType   -row 1 -column 0 -sticky nw -columnspan 5

     grid columnconfigure ${powDWP}saveAsSetup.file 1 -weight 1

     frame ${powDWP}saveAsSetup.action -bg $powbg 
     button ${powDWP}saveAsSetup.action.ok     -text "OK"     -bg $powbg -font g_titleFont \
                        -command { \
                          if ![info exists currentPreviewGraph] { \
                             set currentPreviewGraph "" ; \
                             if { $powGraphSelection == "one" } { \
                                foreach fileCoordList $powGraphCoordList { \
                                   if { [lindex $fileCoordList 5] == $currgn } { \
                                      set currentPreviewGraph [lindex $fileCoordList 0] ; \
                                      break ; \
                                   } ; \
                                } ; \
                             } ; \
                          } ; \
                          set powOutputFileName [${powDWP}saveAsSetup.file.fileNameEntry get] ; \
                          set powSelectDirectory [${powDWP}saveAsSetup.directory.saveInEntry get] ; \
                          ${powDWP}print.option.direntry delete 0 end ; \
                          ${powDWP}print.option.direntry insert 0 $powSelectDirectory ; \
                          ${powDWP}print.option.fileentry delete 0 end ; \
                          ${powDWP}print.option.fileentry insert 0 $powOutputFileName ; \
                          wm deiconify ${powDWP}print ; \
                          wm withdraw ${powDWP}saveAsSetup }

     label ${powDWP}saveAsSetup.action.blanklabel -text " " -bg $powbg -font g_titleFont
     button ${powDWP}saveAsSetup.action.cancel -text "Cancel" -bg $powbg -font g_titleFont \
                                                  -command { wm deiconify ${powDWP}print ; \
                                                             wm withdraw ${powDWP}saveAsSetup }

     grid ${powDWP}saveAsSetup.action -row 18 -column 0 -columnspan 6 -sticky news
     grid ${powDWP}saveAsSetup.action.ok     -row 0 -column 1 -sticky w
     grid ${powDWP}saveAsSetup.action.blanklabel -row 0 -column 2 -columnspan 2 -sticky news
     grid ${powDWP}saveAsSetup.action.cancel -row 0 -column 4 -sticky e
}

proc powSelectConvertFormat { item } {
     global powOutputFileType powDWP
     global powOutputFileName powConvertFunction

     regsub -all " " $item "" result
     set token [split $result "-"]

     if { ![winfo exists ${powDWP}saveAsSetup.file.fileNameEntry] && \
          ![winfo exists ${powDWP}print.option.fileentry] } {
        set fileName "powGraph"
     } else {
        if { [winfo exists ${powDWP}print.option.fileentry] && [winfo ismapped ${powDWP}print.option.fileentry] } {
           set fileName [lindex [split [${powDWP}print.option.fileentry get] "."] 0]
        }
        if { [winfo exists ${powDWP}saveAsSetup.file.fileNameEntry] && [winfo ismapped ${powDWP}saveAsSetup.file.fileNameEntry] } {
           set fileName [lindex [split [${powDWP}saveAsSetup.file.fileNameEntry get] "."] 0]
        }
     }

     foreach functionList $powConvertFunction {
         if { [lindex $token 0] == [lindex $functionList 0] } {
            set powOutputFileType $functionList
            set powOutputFileName [format "%s.%s" $fileName [lindex $functionList 2]]
            break
         }
     }

     if [winfo exists ${powDWP}saveAsSetup.file.fileNameEntry] {
        ${powDWP}saveAsSetup.file.fileNameEntry delete 0 end
        ${powDWP}saveAsSetup.file.fileNameEntry insert end $powOutputFileName
     }

     if [winfo exists ${powDWP}print.option.fileentry] {
        ${powDWP}print.option.fileentry delete 0 end
        ${powDWP}print.option.fileentry insert end $powOutputFileName
     }
}

proc powSetupPage {} {
     global powDWP powbg
     global powOutputPaperSize g_titleFont powStretch
     global powPaperSizeSelected powHandles powPostOrient powPlacement
     global old_powPaperSizeSelected old_powPostOrient old_powPlacement 
     global powGraphSelection

     if [winfo exists ${powDWP}printPageSetup] {
        destroy ${powDWP}printPageSetup
     }

     powToplevel ${powDWP}printPageSetup .pow "-bg $powbg -class \"POW Print\""
     bind ${powDWP}printPageSetup <<CloseWindow>> "destroy ${powDWP}"
     wm title ${powDWP}printPageSetup "Page Setup"

     tixComboBox ${powDWP}printPageSetup.papersize -editable true \
                                                   -label "Paper Size:" \
                                                   -options { \
                                                       listbox.height 4 \
                                                       label.font g_titleFont \
                                                       listbox.font g_titleFont \
                                                       entry.font g_titleFont \
                                                       entry.background white \
                                                       entry.ipady 5 \
                                                   } \
                                                   -command powPaperSizeSelection
     foreach [list name xSizeInch ySizeInch xSizePixel ySizePixel xPt yPt ] $powOutputPaperSize {
         ${powDWP}printPageSetup.papersize insert end $name
     }

     set old_powPaperSizeSelected Letter
     if [info exists powPaperSizeSelected] {
        set old_powPaperSizeSelected $powPaperSizeSelected
        tixSetSilent ${powDWP}printPageSetup.papersize $powPaperSizeSelected
     } else {
        tixSetSilent ${powDWP}printPageSetup.papersize Letter
        set powPaperSizeSelected Letter
     }

     grid ${powDWP}printPageSetup.papersize -row 1 -column 0 -sticky w -columnspan 6

     image create bitmap landscapeIcon -data {
         #define landscape_width 41
         #define landscape_height 36
         static char landscape_bits[] = {
          0x00,0x00,0x00,0x00,0x00,0xfe,0x00,0x00,0x00,0x00,0x00,0xfe,0x00,0x00,0x00,
          0x00,0x00,0xfe,0x00,0x00,0x00,0x00,0x00,0xfe,0x00,0x00,0x00,0x00,0x00,0xfe,
          0x00,0x00,0x00,0x00,0x00,0xfe,0x00,0x00,0x00,0x00,0x00,0xfe,0xf0,0xff,0xff,
          0x1f,0x00,0xfe,0x10,0x00,0x00,0x30,0x00,0xfe,0x10,0x00,0x00,0x50,0x00,0xfe,
          0x10,0x00,0x06,0x90,0x00,0xfe,0x10,0x00,0x06,0x10,0x01,0xfe,0x10,0x00,0x0f,
          0x10,0x02,0xfe,0x10,0x00,0x0f,0xf0,0x07,0xfe,0x10,0x80,0x1d,0x00,0x0c,0xfe,
          0x10,0x80,0x1c,0x00,0x04,0xfe,0x10,0xc0,0x3c,0x00,0x0c,0xfe,0x10,0x40,0x38,
          0x00,0x04,0xfe,0x10,0x60,0x78,0x00,0x0c,0xfe,0x10,0x20,0x70,0x00,0x04,0xfe,
          0x10,0xf0,0xff,0x00,0x0c,0xfe,0x10,0x10,0xe0,0x00,0x04,0xfe,0x10,0x18,0xe0,
          0x01,0x0c,0xfe,0x10,0x08,0xc0,0x01,0x04,0xfe,0x10,0x0c,0xc0,0x03,0x0c,0xfe,
          0x10,0x3f,0xf0,0x0f,0x04,0xfe,0x10,0x00,0x00,0x00,0x0c,0xfe,0x10,0x00,0x00,
          0x00,0x04,0xfe,0x10,0x00,0x00,0x00,0x0c,0xfe,0x10,0x00,0x00,0x00,0x04,0xfe,
          0xf0,0xff,0xff,0xff,0x0f,0xfe,0x50,0x55,0x55,0x55,0x05,0xfe,0x00,0x00,0x00,
          0x00,0x00,0xfe,0x00,0x00,0x00,0x00,0x00,0xfe,0x00,0x00,0x00,0x00,0x00,0xfe,
          0x00,0x00,0x00,0x00,0x00,0xfe};
     }

     image create bitmap portraitIcon -data {
         #define portrait_width 40
         #define portrait_height 37
         static char portrait_bits[] = {
          0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
          0x00,0x00,0x00,0x00,0x00,0x80,0xff,0xff,0x03,0x00,0x80,0x00,0x00,0x06,0x00,
          0x80,0x00,0x00,0x0a,0x00,0x80,0x00,0x00,0x12,0x00,0x80,0x00,0x00,0x22,0x00,
          0x80,0x00,0x00,0x42,0x00,0x80,0x00,0x00,0xfe,0x00,0x80,0x00,0x18,0x80,0x01,
          0x80,0x00,0x18,0x80,0x00,0x80,0x00,0x3c,0x80,0x01,0x80,0x00,0x3c,0x80,0x00,
          0x80,0x00,0x76,0x80,0x01,0x80,0x00,0x72,0x80,0x00,0x80,0x00,0xf3,0x80,0x01,
          0x80,0x00,0xe1,0x80,0x00,0x80,0x80,0xe1,0x81,0x01,0x80,0x80,0xc0,0x81,0x00,
          0x80,0xc0,0xff,0x83,0x01,0x80,0x40,0x80,0x83,0x00,0x80,0x60,0x80,0x87,0x01,
          0x80,0x20,0x00,0x87,0x00,0x80,0x30,0x00,0x8f,0x01,0x80,0xfc,0xe0,0xbf,0x00,
          0x80,0x00,0x00,0x80,0x01,0x80,0x00,0x00,0x80,0x00,0x80,0x00,0x00,0x80,0x01,
          0x80,0x00,0x00,0x80,0x00,0x80,0x00,0x00,0x80,0x01,0x80,0x00,0x00,0x80,0x00,
          0x80,0x00,0x00,0x80,0x01,0x80,0xff,0xff,0xff,0x00,0x00,0x55,0x55,0x55,0x01,
          0x00,0x00,0x00,0x00,0x00};
     }

     frame ${powDWP}printPageSetup.orientation -bg $powbg -bd 2
     label ${powDWP}printPageSetup.orientationframelabel -text Orientation -bg $powbg -font g_titleFont
     label ${powDWP}printPageSetup.orientation.blanklabel  -text " " -bg $powbg -font g_titleFont
     label ${powDWP}printPageSetup.orientation.blankcolumn -text " " -bg $powbg -font g_titleFont
     label ${powDWP}printPageSetup.orientation.iconlabel -image portraitIcon
     label ${powDWP}printPageSetup.orientation.blanklbl2  -text " " -bg $powbg -font g_titleFont
     label ${powDWP}printPageSetup.orientation.blanklbl1  -text " " -bg $powbg -font g_titleFont
     radiobutton ${powDWP}printPageSetup.orientation.portrait  -text Portrait  -value 0 \
                          -variable powPostOrient -font g_titleFont \
                          -command { \
                              ${powDWP}printPageSetup.orientation.iconlabel configure -image portraitIcon ; \
                           }
     radiobutton ${powDWP}printPageSetup.orientation.landscape -text Landscape -value 1 \
                          -variable powPostOrient -font g_titleFont \
                          -command { \
                              ${powDWP}printPageSetup.orientation.iconlabel configure -image landscapeIcon ; \
                           }
 
     grid ${powDWP}printPageSetup.orientation.blanklbl1   -row 1 -column 0 -sticky sw
     grid ${powDWP}printPageSetup.orientation             -row 2 -column 0 -columnspan 2 -sticky news -rowspan 4
     grid ${powDWP}printPageSetup.orientationframelabel   -row 2 -column 0 -sticky nw
     grid ${powDWP}printPageSetup.orientation.iconlabel   -row 2 -column 0 -sticky news -rowspan 2
     grid ${powDWP}printPageSetup.orientation.blankcolumn -row 2 -column 1 -sticky news -rowspan 2
     grid ${powDWP}printPageSetup.orientation.portrait    -row 2 -column 2 -sticky w -columnspan 2 
     grid ${powDWP}printPageSetup.orientation.landscape   -row 3 -column 2 -sticky w -columnspan 2
     grid ${powDWP}printPageSetup.orientation.blanklbl2   -row 4 -column 0 -sticky sw

     set old_powPostOrient 0
     if [info exists powPostOrient] {
        if { $powPostOrient == "" } {
           set powPostOrient 0
        } else {
           set old_powPostOrient $powPostOrient
        }
     } else {
        ${powDWP}printPageSetup.orientation.portrait select
     }

     frame ${powDWP}printPageSetup.placement      -bg $powbg -bd 2 
     label ${powDWP}printPageSetup.placementlabel -text "Placement" -bg $powbg -font g_titleFont
     label ${powDWP}printPageSetup.placement.blanklabel -text " " -bg $powbg -font g_titleFont
     radiobutton ${powDWP}printPageSetup.placement.placementFOOP -text "Fit on one page" \
                                                     -value "FOOP" -variable powPlacement -font g_titleFont
     radiobutton ${powDWP}printPageSetup.placement.placementBFMP -text "Best Fit on multiple pages" \
                                                     -value "BFMP" -variable powPlacement -font g_titleFont
     radiobutton ${powDWP}printPageSetup.placement.placementOGPP -text "One graph per page" \
                                                     -value "OGPP" -variable powPlacement -font g_titleFont

     grid ${powDWP}printPageSetup.placement               -row 6 -column 0 -columnspan 4 -sticky news \
                                                          -rowspan 6
     grid ${powDWP}printPageSetup.placementlabel          -row 6 -column 0 -sticky nw
     grid ${powDWP}printPageSetup.placement.blanklabel    -row 1 -column 0 -sticky nw
     grid ${powDWP}printPageSetup.placement.placementFOOP -row 2 -column 0 -sticky nw
     grid ${powDWP}printPageSetup.placement.placementBFMP -row 3 -column 0 -sticky nw
     grid ${powDWP}printPageSetup.placement.placementOGPP -row 4 -column 0 -sticky nw

     
     set old_powPlacement "FOOP"
     if [info exists powPlacement] {
        if { $powPlacement == "" } {
           set powPlacement "FOOP"
        } else {
           set old_powPlacement $powPlacement
        }
     } else {
        ${powDWP}printPageSetup.placement.placementFOOP select
     }

#     set powGraphSelection all
     set pwoStretch "no"

     frame ${powDWP}printPageSetup.action -bg $powbg 
     button ${powDWP}printPageSetup.action.ok     -text "OK"     -bg $powbg -font g_titleFont \
                                                  -command { \
                                                      if [winfo exists ${powDWP}printPreview] { \
                                                         powPrintPreview ; \
                                                      } else { \
                                                         set resp [tk_messageBox -icon info \
                                                                  -type yesno \
                                                                  -message "Would you like to preview images?" \
                                                                  -title "Ask"] ; \
                                                                  if { $resp=="yes" } { \
                                                                      powPrintPreview \
                                                                  } ; \
                                                      } ; \
                                                      destroy ${powDWP}printPageSetup }
                                        
     label ${powDWP}printPageSetup.action.blanklabel -text " " -bg $powbg -font g_titleFont
     button ${powDWP}printPageSetup.action.cancel -text "Cancel" -bg $powbg -font g_titleFont \
                                                  -command { \
                                                     set powPaperSizeSelected $old_powPaperSizeSelected ; \
                                                     set powPostOrient $old_powPostOrient ; \
                                                     set powPlacement $old_powPlacement ; \
                                                     destroy ${powDWP}printPageSetup }

     grid ${powDWP}printPageSetup.action            -row 13 -column 0 -columnspan 6 -sticky news
     grid ${powDWP}printPageSetup.action.ok         -row 0 -column 1 -sticky w
     grid ${powDWP}printPageSetup.action.blanklabel -row 0 -column 2 -columnspan 2 -sticky news
     grid ${powDWP}printPageSetup.action.cancel     -row 0 -column 4 -sticky e
}

proc toggleFitToPageButton {} {
     global powDWP
     global currentPreviewState

     set currentPreviewState [${powDWP}printPreview.action.imageSize cget -text]

     if { $currentPreviewState == "Original Size" } {
        ${powDWP}printPreview.action.imageSize configure -text "Fit to Page"
        powFitToPage [${powDWP}printPreview.action.page.pageNumber get] no
     } else {
        ${powDWP}printPreview.action.imageSize configure -text "Original Size"
        powFitToPage [${powDWP}printPreview.action.page.pageNumber get] yes
     }
}

proc powPrintPreview {} {
     global powDWP powbg
     global ghostScript searchPath
     global previewNameList
     global g_titleFont
     global currentPreviewGraph
     global powPaperSizeSelected
     global powPaperSizeList
     global powOutputPaperSize
     global powStretch
     global powPostOrient
     global currentPreviewGraph
     global powGraphSelection
     global currentPreviewState

     if [winfo exists ${powDWP}printPreview] {
        destroy ${powDWP}printPreview
     }

     powShowHandles 0

     powToplevel ${powDWP}printPreview .pow "-bg $powbg -class \"POW Print\""
     bind ${powDWP}printPreview <<CloseWindow>> "destroy ${powDWP}"
     wm title ${powDWP}printPreview "Print Preview"
     wm geometry ${powDWP}printPreview +150+0

     bind ${powDWP}printPreview <Destroy> {
          global previewNameList
          foreach nameList $previewNameList {
              catch { file delete -force [lindex $nameList 0] }
              catch { file delete -force [lindex $nameList 1] }
          }
     }

     frame ${powDWP}printPreview.action
     button ${powDWP}printPreview.action.imageSize -text "Fit to Page" -bg $powbg -font g_titleFont \
                       -command toggleFitToPageButton

     button ${powDWP}printPreview.action.print -text "Print" -bg $powbg -font g_titleFont \
                                               -command { powPrintBox preview }

     button ${powDWP}printPreview.action.setup -text "Page Setup" -bg $powbg -font g_titleFont \
                                               -command { powSetupPage }

     image create bitmap pointLeftIcon -data {
         #define pointLeft_width 24
         #define pointLeft_height 20
         static char pointLeft_bits[] = {
              0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x00,0x00,0x0c,0x00,0x00,0x0a,0x00,
              0x00,0x09,0x00,0x80,0xf8,0x0f,0x40,0x00,0x08,0x60,0x00,0x08,0x70,0x00,0x08,
              0xf0,0x01,0x08,0xc0,0x02,0x08,0xc0,0xff,0x0f,0x00,0x5b,0x05,0x00,0x0f,0x00,
              0x00,0x0c,0x00,0x00,0x0c,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
              };
     }
     image create bitmap pointRightIcon -data {
         #define pointRight_width 24
         #define pointRight_height 20
         static char pointRight_bits[] = {
             0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x10,0x00,0x00,0x30,0x00,0x00,0x50,0x00,
             0x00,0x90,0x00,0xf0,0x1f,0x01,0x10,0x00,0x02,0x10,0x00,0x06,0x10,0x00,0x0e,
             0x10,0x80,0x0f,0x10,0x40,0x03,0xf0,0xff,0x03,0xa0,0xda,0x00,0x00,0xf0,0x00,
             0x00,0x30,0x00,0x00,0x30,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
             };
     }

     frame ${powDWP}printPreview.action.page -bg $powbg
     button ${powDWP}printPreview.action.page.previous -image pointLeftIcon \
                        -command { \
                             ${powDWP}printPreview.action.page.next configure -state normal ; \
                             set idx [${powDWP}printPreview.action.page.pageNumber get] ; \
                             incr idx -1 ; \
                             if { $idx <= 1 } { \
                                set idx 1 ; \
                                ${powDWP}printPreview.action.page.previous configure -state disable ; \
                             } ; \
                             ${powDWP}printPreview.action.page.pageNumber delete 0 end ; \
                             ${powDWP}printPreview.action.page.pageNumber insert end $idx ; \
                             powShowPreviewGraph [expr $idx - 1] }
     label ${powDWP}printPreview.action.page.pageNumberLbl1 -text "Page: " -bg $powbg -font g_titleFont
     entry ${powDWP}printPreview.action.page.pageNumber -width 2 -bg white -font g_titleFont -fg blue
     label ${powDWP}printPreview.action.page.pageNumberLbl2 -text "" -bg $powbg -font g_titleFont
     button ${powDWP}printPreview.action.page.next -image pointRightIcon \
                        -command { \
                             ${powDWP}printPreview.action.page.previous configure -state normal ; \
                             set idx [${powDWP}printPreview.action.page.pageNumber get] ; \
                             incr idx ; \
                             if { $idx >= [llength $previewNameList] } { \
                                ${powDWP}printPreview.action.page.next configure -state disable ; \
                                set idx [llength $previewNameList] ; \
                             } ; \
                             ${powDWP}printPreview.action.page.pageNumber delete 0 end ; \
                             ${powDWP}printPreview.action.page.pageNumber insert end $idx ; \
                             powShowPreviewGraph [expr $idx - 1] }

     grid ${powDWP}printPreview.action.page.previous       -row 0 -column 0 -sticky w
     grid ${powDWP}printPreview.action.page.pageNumberLbl1 -row 0 -column 1 -sticky w
     grid ${powDWP}printPreview.action.page.pageNumber     -row 0 -column 2 -sticky w
     grid ${powDWP}printPreview.action.page.pageNumberLbl2 -row 0 -column 3 -sticky w
     grid ${powDWP}printPreview.action.page.next           -row 0 -column 4 -sticky w

     button ${powDWP}printPreview.action.cancel  -text "Close" -bg $powbg -font g_titleFont \
                                                 -command { set currentPreviewGraph "" ; \
                                                            set powStretch no ; \
                                                            destroy ${powDWP}printPreview }

     grid ${powDWP}printPreview.action           -row 0 -column 0 -columnspan 20 -sticky news
     grid ${powDWP}printPreview.action.print     -row 0 -column 0 -sticky w  -padx 5 -columnspan 2
     grid ${powDWP}printPreview.action.imageSize -row 0 -column 2  -sticky w  -padx 5 -columnspan 2
     grid ${powDWP}printPreview.action.setup     -row 0 -column 6  -sticky w  -padx 5 -columnspan 2
     grid ${powDWP}printPreview.action.page      -row 0 -column 8  -sticky ew -padx 5 -columnspan 5
     grid ${powDWP}printPreview.action.cancel    -row 0 -column 13 -sticky e  -padx 5 -columnspan 2

     canvas ${powDWP}printPreview.preview \
            -xscrollcommand [list ${powDWP}printPreview.xscroll set] \
            -yscrollcommand [list ${powDWP}printPreview.yscroll set] \
            -background white \
            -highlightthickness 0 \
            -borderwidth 0 \
            -scrollregion { 0 0 1024 768 }

     scrollbar ${powDWP}printPreview.xscroll -orient horizontal \
                                                    -command [list ${powDWP}printPreview.preview xview]
     scrollbar ${powDWP}printPreview.yscroll -orient vertical \
                                                    -command [list ${powDWP}printPreview.preview yview]

     grid ${powDWP}printPreview.preview -row 1  -column 0 -rowspan 10 -columnspan 15 -sticky news
     grid ${powDWP}printPreview.yscroll -row 1  -column 13 -rowspan 11 -sticky ns
     grid ${powDWP}printPreview.xscroll -row 11 -column 0 -sticky ew

     grid rowconfigure ${powDWP}printPreview 1 -weight 1
     grid columnconfigure ${powDWP}printPreview 0 -weight 1

     ${powDWP}printPreview.preview create text 225 100 \
	       -anchor center -tags deleteMe -text "Building graph... Please wait..." \
               -font [list Helvetica 12 bold] -fill red
     ${powDWP}printPreview configure -cursor watch

     update idletask

     foreach [list name xIn yIn xPix yPix xPt yPt] $powOutputPaperSize {
         if { $powPaperSizeSelected == $name } {
            break
         }
     }

     set powPaperSizeList [list $name $xIn $yIn $xPix $yPix] 

     set powGraphSelection "all"
     set fileNameList [powAssemblePSfile]

     set idx -1
     set previewNameList {}

#puts "paper size selected: $powPaperSizeSelected"

     foreach fileName $fileNameList {
         set previewName "[expr [pid] + $idx]preview.ppm"
         set errorFlag [ catch {
             exec $ghostScript -sDEVICE=ppmraw \
                               -dNOPAUSE -dBATCH -dQUIET \
                               -sPAPERSIZE=[string tolower $powPaperSizeSelected] \
                               -I$searchPath \
                               -sOutputFile=$::env(PSTMPDIR)/$previewName $fileName
         } result ]

         if { !$errorFlag } {
            lappend previewNameList [list $::env(PSTMPDIR)/$previewName $fileName]
         }
         incr idx
     }

     set currentPreviewGraph [lindex [lindex $previewNameList 0] 1]

     # show the first page, sequential pages will be toggle by GUI
     powShowPreviewGraph 0
     ${powDWP}printPreview.action.page.pageNumber delete 0 end
     ${powDWP}printPreview.action.page.pageNumber insert end 1
     ${powDWP}printPreview.action.page.previous configure -state disable
     ${powDWP}printPreview.action.page.pageNumberLbl2 configure -text "of [llength $previewNameList]"

     if { [llength $previewNameList] == 1 } {
        ${powDWP}printPreview.action.page.next configure -state disable
     }

     if [info exists currentPreviewState] { 
        if { $currentPreviewState == "Original Size" } {
           ${powDWP}printPreview.action.imageSize configure -text "Fit to Page"
           powFitToPage 1 no
        } else {
           ${powDWP}printPreview.action.imageSize configure -text "Original Size"
           powFitToPage 1 yes
        }
     }

     ${powDWP}printPreview configure -cursor {}
     powShowHandles 1
}

proc powFitToPage { page stretch } {
     global powStretch
     global previewNameList
     global powDWP
     global powGraphCoordList
     global powOutputPaperSize
     global currentPreviewGraph
     global powPaperSizeSelected
     global powPaperSizeList
     global ghostScript
     global searchPath
     global currentPreviewState
     global powOutputPaperXsizePoint powOutputPaperYsizePoint

     ${powDWP}printPreview.preview delete deleteMe
     ${powDWP}printPreview.preview create text 300 300 \
	       -anchor center -tags deleteMe -text "Rebuilding graph... Please wait..." \
               -font [list Helvetica 12 bold] -fill red
     ${powDWP}printPreview configure -cursor watch
     update idletask

     powShowHandles 0
     foreach nameList $previewNameList {
          catch { file delete -force [lindex $nameList 0] }
          catch { file delete -force [lindex $nameList 1] }
     }

     set powStretch $stretch
     set fileNameList [powAssemblePSfile]
     set idx -1
     set previewNameList {}
     foreach fileName $fileNameList {
         set previewName "[expr [pid] + $idx]preview.ppm"
         set errorFlag [ catch {
             exec $ghostScript -sDEVICE=ppmraw \
                               -dNOPAUSE -dBATCH -dQUIET \
                               -dDEVICEWIDTHPOINTS=$powOutputPaperXsizePoint \
                               -dDEVICEHEIGHTPOINTS=$powOutputPaperYsizePoint \
                               -I$searchPath \
                               -sOutputFile=$::env(PSTMPDIR)/$previewName $fileName
         } result ] 

         if { !$errorFlag } {
            lappend previewNameList [list $::env(PSTMPDIR)/$previewName $fileName]
         }
         incr idx
     }

#puts "powFitToPage, previewNameList: $previewNameList"
#puts "powFitToPage, page: $page"
     set currentPreviewGraph [lindex [lindex $previewNameList [expr $page - 1]] 1]

     # show the current page, sequential pages will be toggle by GUI
     powShowPreviewGraph [expr $page - 1]
     ${powDWP}printPreview.action.page.pageNumber delete 0 end
     ${powDWP}printPreview.action.page.pageNumber insert end $page
     if { $page <= 1 } {
        ${powDWP}printPreview.action.page.previous configure -state disable
     }
     ${powDWP}printPreview.action.page.pageNumberLbl2 configure -text "of [llength $previewNameList]"

     if { [llength $previewNameList] == 1 } {
        ${powDWP}printPreview.action.page.next configure -state disable
     }

     if { $stretch == "yes" } {
        set currentPreviewState "Fit to Page"
        ${powDWP}printPreview.action.imageSize configure -text "Original Size"
     } else {
        set currentPreviewState "Original Size"
        ${powDWP}printPreview.action.imageSize configure -text "Fit to Page"
     }
     ${powDWP}printPreview configure -cursor {}
     powShowHandles 1
}

proc powShowPreviewGraph { page } {
     global previewNameList
     global powDWP
     global powGraphCoordList
     global currentPreviewGraph
     global powPaperSizeList
     global powPostOrient
     global powCurrentPreviewPage

     set powCurrentPreviewPage $page 
     set param [lindex $powGraphCoordList $page]
     set fileName [lindex $param 0]
     set width    [lindex $param 1]
     set height   [lindex $param 2]
     set xCoord   [lindex $param 3]
     set yCoord   [lindex $param 4]
     set orgFileName [lindex $param 5]

#puts "page: $page"
#puts "previewNameList: $previewNameList"
     set currentPreviewGraph [lindex [lindex $previewNameList $page] 1]

     set errorFlag [ catch {
         set im [ image create photo -file [lindex [lindex $previewNameList $page] 0]]
     } err ]

     if { $errorFlag } {
        tk_messageBox -icon warning -type ok -message "Bummper!!!"
        return
     }

     set imWidth [image width $im]

     set displayWidth $imWidth

#puts "imWidth: $imWidth"
#puts "screenwidth: [winfo screenwidth .]"

     if { [expr [winfo screenwidth .] - 10 ] < $displayWidth } {
         set displayWidth [expr [winfo screenwidth .] - 10 ]
     }

     set imHeight [image height $im]
     set displayHeight $imHeight

     if { [expr [winfo screenheight .] - 60 ] < $displayHeight } {
        set displayHeight [expr [winfo screenheight .] - 60 ]
     }

     ${powDWP}printPreview.preview delete deleteMe
     ${powDWP}printPreview.preview create image 0 0 -image $im -anchor nw -tags deleteMe

     set regionList [list 0 0 $imWidth $imHeight]
     #grid ${powDWP}printPreview.yscroll -row 0  -column 12 -rowspan 10  -sticky ns
     #grid ${powDWP}printPreview.xscroll -row 10 -column 0 -columnspan 12 -sticky ew

     ${powDWP}printPreview.preview configure -scrollregion $regionList \
                                                    -width  $displayWidth \
                                                    -height $displayHeight

     if { $imWidth == $displayWidth } {
        grid forget ${powDWP}printPreview.xscroll
     }

     if { $imHeight == $displayHeight } {
        grid forget ${powDWP}printPreview.yscroll
     }

}

proc powPaperSizeSelection { item } {
    global powPaperSizeSelected
    set powPaperSizeSelected $item
}

proc powSelectDir { dir } {
     global powSelectDirectory powDWP

     set powSelectDirectory $dir

     ${powDWP}saveAsSetup.directory.saveInEntry delete 0 end
     ${powDWP}saveAsSetup.directory.saveInEntry insert end $powSelectDirectory

}

proc powExtractGraph { {graph ""} } {
     global currgn powGraphSelection

     if ![info exists powGraphSelection] {
        set powGraphSelection "all"
     }

     if { $powGraphSelection == "all" } {
        set selection all
     }


     if { $graph == "" } {
        if { $powGraphSelection != "all" } {
           set selection $currgn
        }
     } else {
        set selection $graph
     }

     set bbox   [.pow.pow bbox $selection]
     if { [llength $bbox] <= 0 } {
        return [list 0 0 0 0]
     }

     set width  [expr [lindex $bbox 2]-[lindex $bbox 0]]
     set height [expr [lindex $bbox 3]-[lindex $bbox 1]]

#puts " width  :  $width"
#puts " height :  $height"
#puts " bbox   :  $bbox"
#puts " xCoord :  [lindex $bbox 0]"
#puts " yCoord :  [lindex $bbox 1]"

     return [list $width $height [lindex $bbox 0] [lindex $bbox 1]] 
}

proc powSelectPaper { {orient 0} } {
     global powPaperSizeSelected powOutputPaperSize
     global powPaperDefXsizeInch powPaperDefYsizeInch
     global powPaperDefXsizePixel powPaperDefYsizePixel
     global powPaperDefXsizePoint powPaperDefYsizePoint
     global powOutputPaperXsizeInch powOutputPaperYsizeInch
     global powOutputPaperXsizePixel powOutputPaperYsizePixel
     global powOutputPaperXsizePoint powOutputPaperYsizePoint

     foreach [list name powPaperDefXsizeInch powPaperDefYsizeInch \
                        powPaperDefXsizePixel powPaperDefYsizePixel \
                        powPaperDefXsizePoint powPaperDefYsizePoint ] $powOutputPaperSize {
         if { $name == $powPaperSizeSelected } {
            set powOutputPaperXsizeInch $powPaperDefXsizeInch
            set powOutputPaperYsizeInch $powPaperDefYsizeInch
            set powOutputPaperXsizePixel $powPaperDefXsizePixel
            set powOutputPaperYsizePixel $powPaperDefYsizePixel
            set powOutputPaperXsizePoint $powPaperDefXsizePoint
            set powOutputPaperYsizePoint $powPaperDefYsizePoint
            break
         }
     }
}

proc powDetermineGraphDirection { dataList { directionOnly "no"} } {
     global powPaperDefXsizePixel powPaperDefYsizePixel
     global powStretch

     set direction "Y"
    
     set pfileName    "none"
     set pwidth       "none"
     set pheight      "none"
     set pxCoord      "none"
     set pyCoord      "none"
     set porgFileName "none"
   
     foreach param $dataList {
  
        set fileName      [lindex $param 0]
        set width         [lindex $param 1]
        set height        [lindex $param 2]
        set xCoord        [lindex $param 3]
        set yCoord        [lindex $param 4]
        set orgFileName   [lindex $param 5]

        if { $pfileName == "none" } {
           set pfileName [lindex $param 0]
           set pwidth    [lindex $param 1]
           set pheight   [lindex $param 2]
           set pxCoord   [lindex $param 3]
           set pyCoord   [lindex $param 4]
           set porgFileName [lindex $param 5]
        } else {
           if { [expr $pxCoord + $pwidth] <= $xCoord } {
              set direction "X"
           }
           break
        }
     }
     
     if { $direction == "Y" } {
        set dataList [lsort -real -increasing -index 4 $dataList ]
     }

#puts "direction: $direction"
#puts "$dataList"

     if { $directionOnly == "yes" } {
        return $direction
     }

     set start_xCoord 0.0
     set start_yCoord 0.0

     # start breaking the graphList
     set resultList {}
     set idx 0
     set subListFile   "psTemp"
     set subListFormat "ps"

     # first ps file name
     set subListFileName [format "%s/%s_%s.%s" $::env(PSTMPDIR) $subListFile $idx $subListFormat]
     set resultSubList {}

     foreach param $dataList {
        set fileName [lindex $param 0]
        set width    [lindex $param 1]
        set height   [lindex $param 2]
        set xCoord   [lindex $param 3]
        set yCoord   [lindex $param 4]
        set orgFileName   [lindex $param 5]

#puts "s fileName: $fileName"
#puts "s width: $width"
#puts "s height: $height"
#puts "s xCoord: $xCoord"
#puts "s yCoord: $yCoord"

#puts "s powPaperDefXsizePixel: $powPaperDefXsizePixel"
#puts "s powPaperDefYsizePixel: $powPaperDefYsizePixel"

        if { $start_xCoord == 0.0 } {
           set start_xCoord $xCoord
           set start_yCoord $yCoord
           set max_width    $width
           set max_height   $height
        }

        set testResult "false"
#puts "powPaperDefXsizePixel: $powPaperDefXsizePixel"
#puts "powPaperDefYsizePixel: $powPaperDefYsizePixel"

        if { $direction == "X" } {
#puts "X: $xCoord + $width - $start_xCoord: [expr $xCoord + $width - $start_xCoord]"
           if { [expr $xCoord + $width - $start_xCoord] < $powPaperDefXsizePixel} {
              set resultSubList {}
              lappend resultSubList $subListFileName
              lappend resultSubList [expr $xCoord + $width - $start_xCoord]
              
              if { $height > $max_height } {
                 set max_height $height
              }
           } else {
              lappend resultSubList $max_height
              lappend resultSubList $start_xCoord 
              lappend resultSubList $start_yCoord 
              incr idx
#puts "X resultSubList: $resultSubList"
              lappend resultList $resultSubList
              set resultSubList {}
              set subListFileName [format "%s/%s_%s.%s" $::env(PSTMPDIR) $subListFile $idx $subListFormat]
              lappend resultSubList $subListFileName
              lappend resultSubList $width
              set start_xCoord $xCoord
              set start_yCoord $yCoord
              set max_width    $width
              set max_height   $height
           }
        } else {
#puts "Y: $yCoord + $height - $start_yCoord: [expr $yCoord + $height - $start_yCoord]"
           if { [expr $yCoord + $height - $start_yCoord] < $powPaperDefYsizePixel} {
              set resultSubList {}
              lappend resultSubList $subListFileName
              
              if { $width > $max_width } {
                 set max_width $width
              }

              lappend resultSubList $max_width
              lappend resultSubList [expr $yCoord + $height - $start_yCoord]
           } else {
              lappend resultSubList $start_xCoord 
              lappend resultSubList $start_yCoord 
              incr idx
#puts "Y resultSubList: $resultSubList"
              lappend resultList $resultSubList
              set resultSubList {}
              set subListFileName [format "%s/%s_%s.%s" $::env(PSTMPDIR) $subListFile $idx $subListFormat]
              lappend resultSubList $subListFileName
              lappend resultSubList $width
              lappend resultSubList $height
              set start_xCoord $xCoord
              set start_yCoord $yCoord
              set max_width    $width
              set max_height   $height
           }
        }
     }

     if { [llength $resultSubList] != 5 } {
        # make sure the incomplete list finished.
        if { $direction == "X" } {
           lappend resultSubList $max_height
        }
        lappend resultSubList $start_xCoord 
        lappend resultSubList $start_yCoord 
        lappend resultList $resultSubList
     }

#puts $resultList
     return $resultList
}

proc powCombineGraph { master } {
}

proc powAssemblePSfile {{graph {}}} {
     global powSelectDirectory
     global powGraphSelection
     global powbg pcom_fname
     global powStretch powOutputPaperSize
     global powConvertFormat powConvertFunction
     global powHandles powDWP g_titleFont
     global powPaperDefXsizeInch powPaperDefYsizeInch
     global powPaperDefXsizePixel powPaperDefYsizePixel
     global powOutputPaperXsizeInch powOutputPaperYsizeInch
     global powOutputPaperXsizePixel powOutputPaperYsizePixel
     global powPlacement powPostOrient
     global powPaperSizeSelected powPixelToInchRatio
     global powGraphCoordList

     global ghostScript
     global powOutputFileType
     global tcl_platform
     global searchPath

     set tempList [powListGraphs]  
     set px 99999999
     set py 99999999

     # reverse the order so the graphList has the order of graph been created
     set graphList {}
     if { [llength $graph] == 0 } {
        foreach graph $tempList {
            if { [string first "scope" $graph] >= 0 } continue
            set bboxToken [.pow.pow bbox $graph]
            if { [llength $bboxToken] <= 0 } continue
           
            foreach [list x1 y1 x2 y2] [.pow.pow bbox $graph] {}
   
            if { $x1 < $px } {
               set graphList [linsert $graphList 0 $graph]
               set px $x1
               set py $y1
            } else {
               if { $y1 < $py } {
                  set graphList [linsert $graphList 0 $graph]
                  set px $x1
                  set py $y1
               } else {
                  lappend graphList $graph
               }
            }
         }
     } else {
        set graphList $graph
     }

     set idx -1
     set psNameList {}
     set allCoordList {}

     ####################################################
     # selectedGraphCoord is either all or selected graph
     ####################################################
     set selectedGraphCoord [powExtractGraph] 

     ####################################################
     # allCoordList contains all graph's coordination
     ####################################################

     if ![info exists powPostOrient] {
        set powPostOrient 0
     }

     if ![info exists powPlacement] {
        set powPlacement FOOP
     }

     if ![info exists powStretch] {
        set powStretch "no"
     }

     if ![info exists powPaperSizeSelected] {
        set powPaperSizeSelected "Letter"
     }

#puts "powPostOrient: $powPostOrient"
#puts "powStretch   : $powStretch"
#puts "powPlacement : $powPlacement"

     powSelectPaper $powPostOrient

     switch -exact $powPlacement {
            "FOOP" {
                # fit on one page
                set tname "[pid]FOOPextract.ps"
                set width  [lindex $selectedGraphCoord 0]
                set height [lindex $selectedGraphCoord 1]
                set xCoord [lindex $selectedGraphCoord 2]
                set yCoord [lindex $selectedGraphCoord 3]

                lappend psNameList [list $::env(PSTMPDIR)/$tname $width $height $xCoord $yCoord all]
            }
            "BFMP" {
                # best fit on mulitple pages
                set findColorBar false

                foreach graph $graphList {
                    if { [string first "scope" $graph] >= 0 } continue

                    if { [string first "colorbar" $graph] >= 0 } {
                       set findColorBar true
                       continue
                    }

                    set resultGraphCoord [powExtractGraph $graph] 
                    set tname "[expr [pid] + $idx]BFMPextract.ps"

                    set width  [lindex $resultGraphCoord 0]
                    set height [lindex $resultGraphCoord 1]
                    set xCoord [lindex $resultGraphCoord 2]
                    set yCoord [lindex $resultGraphCoord 3]

                    lappend psNameList [list $::env(PSTMPDIR)/$tname $width $height $xCoord $yCoord $graph]
                    incr idx
                }

                if { $findColorBar == "true" } {
                   foreach graph $graphList {
                       set idx [string first "colorbar" $graph]

                       if { [string first "colorbar" $graph] >= 0 } {
                          set masterKey [string range $graph 0 [expr $idx - 2]]
                          set resultGraphCoord [powExtractGraph $graph] 
                          set width  [lindex $resultGraphCoord 0]
                          set height [lindex $resultGraphCoord 1]
                          set xCoord [lindex $resultGraphCoord 2]
                          set yCoord [lindex $resultGraphCoord 3]
                          set x1Coord [expr $xCoord + $width]
                          set y1Coord [expr $yCoord + $height]
                          
                          set idx 0
                          foreach token $psNameList {
                              set name [lindex $token 0]
                              set w    [lindex $token 1]
                              set h    [lindex $token 2]
                              set x    [lindex $token 3]
                              set y    [lindex $token 4]
                              set g    [lindex $token 5]

                              if { $g == $masterKey } {
                                 set x1 [expr $x + $w]
                                 set y1 [expr $y + $h]

                                 if { $x1Coord > $x1 && $xCoord < $x } {
                                    # colorbar is larger than the graph 
                                    set x $xCoord
                                    set w $width
                                 } elseif { $x1Coord > $x1 && $xCoord > $x } {
                                    set w [expr $x1Coord - $x]
                                 } elseif { $x1Coord < $x1 && $xCoord > $x } {
                                    # don't do anything
                                 } else {
                                    # x1Coord < x1 && xCoord < $x
                                    set w [expr $x1 - $xCoord]
                                 }

                                 if { $y1Coord > $y1 && $yCoord < $y } {
                                    # colorbar is larger than the graph 
                                    set y $yCoord
                                    set h $height
                                 } elseif { $y1Coord > $y1 && $yCoord > $y } {
                                    set h [expr $y1Coord - $y]
                                 } elseif { $y1Coord < $y1 && $yCoord > $y } {
                                    # don't do anything
                                 } else {
                                    # y1Coord < y1 && yCoord < $y
                                    set h [expr $y1 - $yCoord]
                                 }
                                 set newList [list $name $w $h $x $y $g]
                                 set psNameList [lreplace $psNameList $idx $idx $newList]
                                 break
                              }
                              incr idx
                          } 
                       }
                   }
                }

                if { $idx > 0 } {
                   # more than one graph on the canvas
                   # sort against X direction first
                   set psNameList [lsort -real -increasing -index 3 $psNameList ]
                   set psNameList [powDetermineGraphDirection $psNameList]
                }

            }
            "OGPP" {
                # one graph per page
                set findColorBar false
                foreach graph $graphList {
                    if { [string first "scope" $graph] >= 0 } continue

                    if { [string first "colorbar" $graph] >= 0 } {
                       set findColorBar true
                       continue
                    }

                    set resultGraphCoord [powExtractGraph $graph] 
                    set tname "[expr [pid] + $idx]OGPPextract.ps"

                    set width  [lindex $resultGraphCoord 0]
                    set height [lindex $resultGraphCoord 1]
                    set xCoord [lindex $resultGraphCoord 2]
                    set yCoord [lindex $resultGraphCoord 3]

                    lappend psNameList [list $::env(PSTMPDIR)/$tname $width $height $xCoord $yCoord $graph]
                    incr idx
                }

                if { $findColorBar == "true" } {
                   foreach graph $graphList {
                       set idx [string first "colorbar" $graph]

                       if { [string first "colorbar" $graph] >= 0 } {
                          set masterKey [string range $graph 0 [expr $idx - 2]]
                          set resultGraphCoord [powExtractGraph $graph] 
                          set width  [lindex $resultGraphCoord 0]
                          set height [lindex $resultGraphCoord 1]
                          set xCoord [lindex $resultGraphCoord 2]
                          set yCoord [lindex $resultGraphCoord 3]
                          set x1Coord [expr $xCoord + $width]
                          set y1Coord [expr $yCoord + $height]
                          
                          set idx 0
                          foreach token $psNameList {
                              set name [lindex $token 0]
                              set w    [lindex $token 1]
                              set h    [lindex $token 2]
                              set x    [lindex $token 3]
                              set y    [lindex $token 4]
                              set g    [lindex $token 5]

                              if { $g == $masterKey } {
                                 set x1 [expr $x + $w]
                                 set y1 [expr $y + $h]

                                 if { $x1Coord > $x1 && $xCoord < $x } {
                                    # colorbar is larger than the graph 
                                    set x $xCoord
                                    set w $width
                                 } elseif { $x1Coord > $x1 && $xCoord > $x } {
                                    set w [expr $x1Coord - $x]
                                 } elseif { $x1Coord < $x1 && $xCoord > $x } {
                                    # don't do anything
                                 } else {
                                    # x1Coord < x1 && xCoord < $x
                                    set w [expr $x1 - $xCoord]
                                 }

                                 if { $y1Coord > $y1 && $yCoord < $y } {
                                    # colorbar is larger than the graph 
                                    set y $yCoord
                                    set h $height
                                 } elseif { $y1Coord > $y1 && $yCoord > $y } {
                                    set h [expr $y1Coord - $y]
                                 } elseif { $y1Coord < $y1 && $yCoord > $y } {
                                    # don't do anything
                                 } else {
                                    # y1Coord < y1 && yCoord < $y
                                    set h [expr $y1 - $yCoord]
                                 }
                                 set newList [list $name $w $h $x $y $g]
                                 set psNameList [lreplace $psNameList $idx $idx $newList]
                                 break
                              }
                              incr idx
                          } 
                       }
                   }
                }

                if { $idx > 1 } {
                   # more than one graph on the canvas
            
                   set psNameList [lsort -real -increasing -index 3 $psNameList ]
                   set direction [powDetermineGraphDirection $psNameList yes]
                   if { $direction == "Y" } {
                      set psNameList [lsort -real -increasing -index 4 $psNameList ]
                   }
                }
            }
     }

     set powGraphCoordList $psNameList

     set fileNameList {}
     foreach param $psNameList {
        set fileName [lindex $param 0]
        set width    [lindex $param 1]
        set height   [lindex $param 2]
        set xCoord   [lindex $param 3]
        set yCoord   [lindex $param 4]
        
#puts " fileName $fileName"
#puts " width    $width"
#puts " height   $height"
#puts " xCoord   $xCoord"
#puts " yCoord   $yCoord"
#puts " powPostOrient $powPostOrient"

        set pageWidthPixel  $powOutputPaperXsizePixel
        set pageHeightPixel $powOutputPaperYsizePixel

        set pageWidth  $powOutputPaperXsizeInch
        set pageHeight $powOutputPaperYsizeInch

#puts "powOutputPaperXsizeInch: $powOutputPaperXsizeInch"
#puts "powOutputPaperXsizePixel: $powOutputPaperXsizePixel"
#puts "powOutputPaperYsizeInch: $powOutputPaperYsizeInch"
#puts "powOutputPaperYsizePixel: $powOutputPaperYsizePixel"
#puts "OX1 pageWidth: $pageWidth"
#puts "OX1 pageheight: $pageHeight"


        set canvasHeight $height
        set canvasWidth  $width

        if { $width <= $powOutputPaperXsizePixel && $height <= $powOutputPaperYsizePixel } {
           # whole canvas or individual image is smaller than the output page size in Pixel

           if { $powStretch == "no" } {
              # no stretch to fit the page
              # Don't specify pageheight or pagewidth in canvas postscript command
              set pageWidth  0.0
              set pageHeight 0.0
           } else {
              # strectch the image to fit the page
              # adjust pageWidth and pageHeight and select the samller of two adjust size
              #
              # 1. calculate the factor from original image width to max width allows
              #    calculate the factor from original image height to max height allows
              set widthFactor  [expr $powOutputPaperXsizePixel / $width]
              set heightFactor [expr $powOutputPaperYsizePixel / $height]
              set imageWidthInch [format "%si" [expr [string range $powOutputPaperXsizeInch 0 [expr [string length $powOutputPaperXsizeInch] - 2]] / $widthFactor]]
              set imageHeightInch [format "%si" [expr [string range $powOutputPaperYsizeInch 0 [expr [string length $powOutputPaperYsizeInch] - 2]] / $heightFactor]]

#puts "imageWidthInch : $imageWidthInch"
#puts "imageHeightInch: $imageHeightInch"

              # 2. use the height factor * current width and get new width
              #    use the width factor * current height and get new height
              set newWidthPixel  [expr $heightFactor * $width]
              set newHeightPixel [expr $widthFactor * $height]

#puts "newWidthPixel : $newWidthPixel"
#puts "newHeightPixel: $newHeightPixel"
              set zoomFactor 1.0
              set operator "/"

              if { $newWidthPixel > $powOutputPaperXsizePixel } {
                 # width can't zoom in or out with heightFactor
                 set zoomFactor $widthFactor
                 if { $width < $powOutputPaperXsizePixel } {
                    set operator "*"
                 }
                 set pageWidth [format "%si" [expr [string range $imageWidthInch 0 [expr [string length $pageWidth] - 2]] $operator $zoomFactor]]
                 set pageHeight 0.0
              }

              if { $newHeightPixel > $powOutputPaperYsizePixel } {
                 # height can't zoom in or out with widthFactor
                 set zoomFactor $heightFactor
                 if { $height < $powOutputPaperYsizePixel } {
                    set operator "*"
                 }
                 set pageHeight [format "%si" [expr [string range $imageHeightInch 0 [expr [string length $pageHeight] - 2]] $operator $zoomFactor]]
                 set pageWidth 0.0
              }

              if { $pageHeight == 0.0 && $pageWidth == 0.0 } {
                 if { $powOutputPaperXsizePixel > $powOutputPaperYsizePixel } {
                    # width larger than height, use powOutputPaperYsizeInch
                    set pageHeight $powOutputPaperYsizeInch
                 } else {
                    set pageWidth $powOutputPaperXsizeInch
                 }
              }
           }
#puts "pageWidth: $pageWidth"
#puts "pageHeight: $pageHeight"
        } elseif { $width > $powOutputPaperXsizePixel && $height <= $powOutputPaperYsizePixel } {
           # the width of whole canvas or individual image is larger than output page width in pixel
           # but height of whole canvas or individual image is smaller than output page height in pixel

           if { $powStretch == "no" } {
              # want width of the image to reduce by canvas postscript command to fit the page
              # set pageHeight to 0.0 and se the pagewidth = powOutputPagerXsizePixel to reduce 
              # the width of image
              set pageHeight 0.0
           } else {
              # strectch the image to fit the page
              if { $powOutputPaperXsizePixel > $powOutputPaperYsizePixel } {
                 # using the smaller of paper width and height to be the final size.
                 set pageWidth 0.0
                 set pageHeight $powOutputPaperYsizeInch
              } else {
                 set pageHeight 0.0
                 set pageWidth $powOutputPaperXsizeInch
              }
           }
        } elseif { $width <= $powOutputPaperXsizePixel && $height > $powOutputPaperYsizePixel } {
           # the width of whole canvas or individual image is smaller than output page width in pixel
           # but height of whole canvas or individual image is larger than output page height in pixel

           if { $powStretch == "no" } {
              # want height of the image to reduce by canvas postscript command to fit the page
              # set pageWidth to 0.0 and se the pageHeight = powOutputPagerYsizePixel to reduce 
              # the height of image
              set pageWidth 0.0
           } else {
              # strectch the image to fit the page
              if { $powOutputPaperXsizePixel > $powOutputPaperYsizePixel } {
                 # using the smaller of paper width and height to be the final size.
                 set pageWidth 0.0
                 set pageHeight $powOutputPaperYsizeInch
              } else {
                 set pageHeight 0.0
                 set pageWidth $powOutputPaperXsizeInch
              }
           }
        } elseif { $width > $powOutputPaperXsizePixel && $height > $powOutputPaperYsizePixel } {
           # the width and the height of whole canvas or individual image is larger than output page 
           # width and height in pixel

           if { $width > $height } {
              if { $powStretch == "no" } {
                 # width is larger than height, so reduce width will indicate that reduced height
                 # will also fit the page.
                 # set pageHeigth to 0.0 and se the pageWidth = powOutputPagerXsizePixel to reduce 
                 # the width of image
                 set pageHeight 0.0
                 set pageWidth $powOutputPaperXsizeInch
              } else {
                 # same idea, since now we need to reduce the image to fit the page, reduce the
                 # width will reduce the height to fit the page also
                 # set pageHeigth to 0.0 and se the pageWidth = powOutputPagerXsizePixel to reduce 
                 # the width of image
                 if { $powPostOrient == 0 } {
                    set pageHeight 0.0
                    set pageWidth $powOutputPaperXsizeInch
                 } else {
                    # rotate 90 degree and stretch. Even though width is larger than height,
                    # the new width is now smaller than new height. Need pageHeight = powOutputPagerYsizePixel
                    # to reduce the new height (previously the width of image) to fit the page.
                    set pageWidth 0.0
                    set pageHeight $powOutputPaperYsizeInch
                 }
              }
           } else {
              if { $powStretch == "no" } {
                 # height is larger than width, so reduce height will indicate that reduced width
                 # will also fit the page.
                 # set pageWidth to 0.0 and se the pageHeight = powOutputPagerYsizePixel to reduce 
                 # the height of image
                 set pageWidth 0.0
                 set pageHeight $powOutputPaperYsizeInch
              } else {
                 # same idea, since now we need to reduce the image to fit the page, reduce the
                 # height will reduce the width to fit the page also.
                 # set pageWidth to 0.0 and se the pageHeight = powOutputPagerYsizePixel to reduce 
                 # the height of image
                 if { $powPostOrient == 0 } {
                    set pageWidth 0.0
                    set pageHeight $powOutputPaperYsizeInch
                 } else {
                    # rotate 90 degree and stretch. Even though height is larger than width,
                    # the new height is now smaller than new width. Need pagewidth = powOutputPagerXsizePixel
                    # to reduce the new width (previously the height of image) to fit the page.
                    set pageHeight 0.0
                    set pageWidth $powOutputPaperXsizeInch
                 }
              }
           }
        }

#puts "pageWidth: $pageWidth"
#puts "pageheight: $pageHeight"

        if { $pageWidth != 0.0 } {
           if { $powPostOrient == 1 } {
              # this is a hack until I could find a better way to stretch plot/image slice
              # to fit the page while rotate 90 degree
              set pageWidth [format "%si" [expr [string range $pageWidth 0 [expr [string length $pageWidth] - 2]] - 0.4]]
           }
           catch { .pow.pow postscript -colormode color -rotate $powPostOrient -file $fileName \
                                       -width $canvasWidth -height $canvasHeight \
                                       -pagewidth $pageWidth \
                                       -x $xCoord -y $yCoord } err
        } elseif { $pageHeight != 0.0 } {
           if { $powPostOrient == 1 } {
              # this is a hack until I could find a better way to stretch plot/image slice
              # to fit the page while rotate 90 degree
              set pageHeight [format "%si" [expr [string range $pageHeight 0 [expr [string length $pageHeight] - 2]] - 0.4]]
           }
           catch { .pow.pow postscript -colormode color -rotate $powPostOrient -file $fileName \
                                       -width $canvasWidth -height $canvasHeight \
                                       -pageheight $pageHeight \
                                       -x $xCoord -y $yCoord } err
        } else {
           catch { .pow.pow postscript -colormode color -rotate $powPostOrient -file $fileName \
                                       -width $canvasWidth -height $canvasHeight \
                                       -x $xCoord -y $yCoord } err
        }
        lappend fileNameList $fileName
     }

     return $fileNameList
}

proc powParseGraphRange { range } {
     global powPlacement 

     set oldPowPlacement ""
     if [info exists powPlacement] {
        set oldPowPlacement $powPlacement
     }
     set powPlacement OGPP

     regsub -all " " $range "" result
     set tokenList [split $result ","]
     set indxList {}
     set returnList {}

     foreach token $tokenList {
        set subToken [split $token "-"]
        if { [llength $subToken] > 1 } {
           # it is range
           set start [expr [lindex $subToken 0] - 1]
           set end   [expr [lindex $subToken 1] - 1]
        } else {
           set start [expr [lindex $subToken 0] - 1]
           set end   [expr [lindex $subToken 0] - 1]
        }

        set value 1

        if { $start > $end } {
           set value -1
        }

        for { set i $start } {$i <= $end} { incr i $value } {
           if { [lsearch $indxList $i] < 0 } {
              lappend indxList $i 
           }
        }
     }

     # delete from back of list
     set indxList [lsort -integer -increasing $indxList]

     set returnList [powAssemblePSfile]
     set finalList {}
    
     for {set i 0} {$i < [llength $indxList]} {incr i} {
         lappend finalList [lindex $returnList [lindex $indxList $i]]
     } 

     set powPlacement $oldPowPlacement
     return $finalList
}

proc powPrintBox { {fromWhere "pow"} } {
     global powGraphSelection
     global powSelectDirectory
     global powConvertFunction
     global powDWP g_titleFont powbg
     global powGraphSelection
     global currentPreviewGraph
     global powPrintType
     global powPrintFunction
     global currgn
     global previewNameList
     global powCurrentPreviewPage
     global powOutputFileName
     global callingRoutine

     set callingRoutine $fromWhere

     if [winfo exists ${powDWP}print] {
        destroy ${powDWP}print
     }

     powToplevel ${powDWP}print .pow "-bg $powbg -class \"POW Print\""
     bind ${powDWP}print <<CloseWindow>> "destroy ${powDWP}"
     wm title ${powDWP}print "Print"

     set rowIdx 0
     frame ${powDWP}print.choice               -bg $powbg -bd 2 
     radiobutton ${powDWP}print.choice.printer -text "Printer" -bg $powbg -font g_titleFont \
                                               -variable powPrintType -value "Printer" \
                                               -command { \
                                                   ${powDWP}print.option.filelbl configure -state disable ; \
                                                   ${powDWP}print.option.fileentry configure -state disable ; \
                                                   ${powDWP}print.option.dirlbl configure -state disable ; \
                                                   ${powDWP}print.option.direntry configure -state disable ; \
                                                   ${powDWP}print.option.filebutton configure -state disable; \
                                                   ${powDWP}print.option.convertType configure -state disable; \
                                                   ${powDWP}print.option.printerlbl configure -state normal ; \
                                                   ${powDWP}print.option.printerentry configure -state normal; \
                                               }
     radiobutton ${powDWP}print.choice.file    -text "File" -bg $powbg -font g_titleFont \
                                               -variable powPrintType -value "File" \
                                               -command { \
                                                   ${powDWP}print.option.filelbl configure -state normal; \
                                                   ${powDWP}print.option.convertType configure -state normal; \
                                                   ${powDWP}print.option.fileentry configure -state normal; \
                                                   ${powDWP}print.option.dirlbl configure -state normal ; \
                                                   ${powDWP}print.option.direntry configure -state normal ; \
                                                   ${powDWP}print.option.filebutton configure -state normal; \
                                                   ${powDWP}print.option.printerlbl configure -state disable ; \
                                                   ${powDWP}print.option.printerentry configure -state disable ; \
                                               }
     label ${powDWP}print.choice.blanklabel    -text " " -bg $powbg -font g_titleFont -width 10 

     grid ${powDWP}print.choice                -row $rowIdx -column 0 -sticky ew -rowspan 2
     grid ${powDWP}print.choice.printer        -row 1 -column 0 -sticky nw 
     grid ${powDWP}print.choice.blanklabel     -row 1 -column 1 -sticky news -columnspan 3
     grid ${powDWP}print.choice.file           -row 1 -column 8 -sticky ne 

     grid rowconfigure ${powDWP}print 0 -weight 1
     grid columnconfigure ${powDWP}print 8 -weight 1

     incr rowIdx 2

     frame ${powDWP}print.option                  -bg $powbg -bd 2
     label ${powDWP}print.option.printerlbl       -text "Printer Command:" -bg $powbg -font g_titleFont
     entry ${powDWP}print.option.printerentry     -bg white -font g_titleFont
     label ${powDWP}print.option.filelbl          -text "File Name:" -bg $powbg -font g_titleFont
     entry ${powDWP}print.option.fileentry        -bg white -font g_titleFont
     tixComboBox ${powDWP}print.option.convertType -editable true \
                                                       -label "format:" \
                                                   -options { \
                                                        listbox.height 4 \
                                                        label.font g_titleFont \
                                                        listbox.font g_titleFont \
                                                        entry.font g_titleFont \
                                                        entry.background white \
                                                        entry.width 30 \
                                                        entry.ipady 5 \
                                                   } \
                                                   -command powSelectConvertFormat

     foreach functionList $powConvertFunction {
         set formatStr [format "%s - %s" [lindex $functionList 0] [lindex $functionList 3]]
         ${powDWP}print.option.convertType insert end $formatStr
     }

     label ${powDWP}print.option.dirlbl           -text "Directory:" -bg $powbg -font g_titleFont
     entry ${powDWP}print.option.direntry         -bg white -font g_titleFont
     button ${powDWP}print.option.filebutton      -text "Browse" -font g_titleFont -bg $powbg \
            -command { set powOutputFileName [${powDWP}print.option.fileentry get] ; \
                       set powSelectDirectory [${powDWP}print.option.direntry get] ; \
                       wm withdraw ${powDWP}print; powSaveAs }

     grid ${powDWP}print.option                   -row $rowIdx -column 0 -columnspan 10 -sticky news -rowspan 2
     grid ${powDWP}print.option.printerlbl        -row 1 -column 0 -sticky nw -columnspan 2
     grid ${powDWP}print.option.printerentry      -row 1 -column 2 -sticky nw -columnspan 7
     grid ${powDWP}print.option.filelbl           -row 2 -column 0 -sticky nw -columnspan 2
     grid ${powDWP}print.option.fileentry         -row 2 -column 2 -sticky new -columnspan 4
     grid ${powDWP}print.option.convertType       -row 3 -column 2 -sticky new -columnspan 4
     grid ${powDWP}print.option.dirlbl            -row 4 -column 0 -sticky nw -columnspan 2
     grid ${powDWP}print.option.direntry          -row 4 -column 2 -sticky new -columnspan 4
     grid ${powDWP}print.option.filebutton        -row 4 -column 6 -sticky nw 

     grid rowconfigure ${powDWP}print $rowIdx -weight 1

     grid rowconfigure ${powDWP}print.option 1 -weight 1
     grid rowconfigure ${powDWP}print.option 2 -weight 1
     grid rowconfigure ${powDWP}print.option 3 -weight 1
     grid columnconfigure ${powDWP}print.option 2 -weight 1

     incr rowIdx 4

     frame ${powDWP}print.selection                -bg $powbg -bd 2 -relief ridge
     label ${powDWP}print.selectionframelabel      -text "Print/Save Range" -bg $powbg -font g_titleFont
     label ${powDWP}print.selection.blanklabel     -text " " -bg $powbg -font g_titleFont

     radiobutton ${powDWP}print.selection.allgraph -text "All Graphs" -value "all" \
                                                   -bg $powbg \
                                                   -variable powGraphSelection -font g_titleFont
     radiobutton ${powDWP}print.selection.selectedgraph -text "Selected Graph" -value "one" \
                                                   -bg $powbg \
                          -variable powGraphSelection -font g_titleFont \
                          -command { \
                               if ![info exists currentPreviewGraph] { \
                                  set currentPreviewGraph "" ; \
                                  if { $powGraphSelection == "one" } { \
                                     set currentPreviewGraph [lindex [powAssemblePSfile $currgn] 0] ; \
                                  } ; \
                               } ; \
                           }
     radiobutton ${powDWP}print.selection.range    -text "Graphs No." -value "range" \
                                                   -bg $powbg \
                                                   -variable powGraphSelection -font g_titleFont
     entry ${powDWP}print.selection.entry          -bg white -width 25 -font g_titleFont
     label ${powDWP}print.selection.label1         -text "Enter graph numbers and/or graph ranges separated by" \
                                                   -bg $powbg \
                                                   -font [list Helvetica 10 italic] -fg blue
     label ${powDWP}print.selection.label2         -text "commas, i.e. 1,3,5,5-12 (1 being leftmost/topmost graph)" \
                                                   -bg $powbg \
                                                   -font [list Helvetica 10 italic] -fg blue
    
     if { $fromWhere == "preview" } {
        ${powDWP}print.selection.allgraph configure -text "All Pages"
        ${powDWP}print.selection.selectedgraph configure -text "Current Page"
        ${powDWP}print.selection.range configure -text "Page No."
        ${powDWP}print.selection.label1 configure -text "Enter page numbers and/or page ranges separated by"
        ${powDWP}print.selection.label2 configure -text "commas, i.e. 1,3,5,5-12"
     }

     grid ${powDWP}print.selection                -row $rowIdx -column 0 -columnspan 10 -sticky news -rowspan 7
     grid ${powDWP}print.selectionframelabel      -row $rowIdx -column 0 -sticky nw
     grid ${powDWP}print.selection.blanklabel     -row 1 -column 0 -sticky nw
     grid ${powDWP}print.selection.allgraph       -row 2 -column 1 -sticky nw -columnspan 2
     grid ${powDWP}print.selection.selectedgraph  -row 3 -column 1 -sticky nw -columnspan 2

     grid ${powDWP}print.selection.range          -row 4 -column 1 -sticky nw -columnspan 2
     grid ${powDWP}print.selection.entry          -row 4 -column 3 -sticky nw -columnspan 5
     grid ${powDWP}print.selection.label1         -row 5 -column 1 -sticky nw -columnspan 6
     grid ${powDWP}print.selection.label2         -row 6 -column 1 -sticky nw -columnspan 6

     grid rowconfigure ${powDWP}print $rowIdx -weight 1

     grid rowconfigure ${powDWP}print.selection 1 -weight 1
     grid rowconfigure ${powDWP}print.selection 2 -weight 1
     grid rowconfigure ${powDWP}print.selection 3 -weight 1
     grid rowconfigure ${powDWP}print.selection 4 -weight 1
     grid rowconfigure ${powDWP}print.selection 5 -weight 1
     grid rowconfigure ${powDWP}print.selection 6 -weight 1
     grid rowconfigure ${powDWP}print.selection 7 -weight 1
     grid columnconfigure ${powDWP}print.selection 3 -weight 1

     incr rowIdx 7

     frame ${powDWP}print.action            -bg $powbg 
     button ${powDWP}print.action.ok        -text "OK" -bg $powbg -font g_titleFont \
                        -command { \
                          if ![info exists currentPreviewGraph] { \
                             set currentPreviewGraph "" ; \
                             if { $powGraphSelection == "one" } { \
                                set currentPreviewGraph [lindex [powAssemblePSfile $currgn] 0] ; \
                             } ; \
                          } ; \
                          if { $powPrintType == "Printer" } { \
                             set powPrintFunction [${powDWP}print.option.printerentry get]
                             if { $powGraphSelection == "all" } { \
                                powShowHandles 0 ; \
                                powPrint ; \
                                powShowHandles 1 ; \
                             } elseif { $powGraphSelection == "one" } { \
                                powShowHandles 0 ; \
                                powPrint $currentPreviewGraph ; \
                                powShowHandles 1 ; \
                             } elseif { $powGraphSelection == "range" } { \
                                powShowHandles 0 ; \
                                powPrint [powParseGraphRange \
                                         [${powDWP}print.selection.entry get]] ;\
                                powShowHandles 1 ; \
                             } ; \
                          } else {
                             set powOutputFileName [${powDWP}print.option.fileentry get] ; \
                             set powSelectDirectory [${powDWP}print.option.direntry get] ; \
                             set token [split $powOutputFileName "."] ; \
                             set powOutputFileType {} ; \
                             foreach cvf $powConvertFunction {
                               if { [lindex $cvf 2] == [lindex $token 1] } {\
                                  set powOutputFileType $cvf ; \
                                  break;
                               } \
                             } ; \
                             if { [llength $powOutputFileType] == 0 } {
                                tk_messageBox -icon warning -parent .pow -type ok \
                                              -message "Can't save to [lindex $token 1] format."
                             } else {
                                if { $powGraphSelection == "range" } { \
                                   powShowHandles 0 ; \
                                   powSave [powParseGraphRange \
                                           [${powDWP}print.selection.entry get]] ; \
                                   powShowHandles 1 ; \
                                } elseif { $powGraphSelection == "one" } { \
                                   powShowHandles 0 ; \
                                   if { $callingRoutine == "pow" } { \
                                      powSave [lindex [powAssemblePSfile $currgn] 0] ; \
                                   } else { \
                                      powSave [lindex [lindex $previewNameList $powCurrentPreviewPage] 1]
                                   } ; \
                                   powShowHandles 1 ; \
                                } else { \
                                   powShowHandles 0 ; \
                                   powSave ; \
                                   powShowHandles 1 ; \
                                } ; \
                             } ; \
                          }; \
                         destroy ${powDWP}print }
     label ${powDWP}print.action.blanklabel -text " " -bg $powbg -font g_titleFont
     button ${powDWP}print.action.cancel    -text "Cancel" -bg $powbg -font g_titleFont \
                                            -command { destroy ${powDWP}print }

     grid ${powDWP}print.action            -row $rowIdx -column 0 -columnspan 10 -sticky news
     grid ${powDWP}print.action.ok         -row 0 -column 1 -sticky w -padx 30
     grid ${powDWP}print.action.cancel     -row 0 -column 4 -sticky e -padx 30

     # grid rowconfigure ${powDWP}print 0 -weight 1
     # grid columnconfigure ${powDWP}print 0 -weight 1

     if { [${powDWP}print.option.printerentry get] == "" } {
        ${powDWP}print.option.printerentry insert end $powPrintFunction
     } else {
        set powPrintFunction [${powDWP}print.option.printerentry get]
     }

     if ![info exists powOutputFileName] {
        ${powDWP}print.option.fileentry insert end "powGraph.ps"
     } else {
        ${powDWP}print.option.fileentry insert end $powOutputFileName
     }
     set powOutputFileName [${powDWP}print.option.fileentry get]

     if ![info exists powSelectDirectory ] {
        ${powDWP}print.option.direntry insert end [pwd]
     } else {
        ${powDWP}print.option.direntry insert end $powSelectDirectory
     }

     if { ![info exists powPrintType] || $powPrintType == "" } {
        ${powDWP}print.choice.printer select
        ${powDWP}print.option.filelbl configure -state disable 
        ${powDWP}print.option.convertType configure -state disable 
        ${powDWP}print.option.fileentry configure -state disable
        ${powDWP}print.option.dirlbl configure -state disable 
        ${powDWP}print.option.direntry configure -state disable
        ${powDWP}print.option.filebutton configure -state disable
        ${powDWP}print.option.printerlbl configure -state normal
        ${powDWP}print.option.printerentry configure -state normal
     } else {
        if { $powPrintType == "Printer" } {
           ${powDWP}print.option.filelbl configure -state disable 
           ${powDWP}print.option.convertType configure -state disable 
           ${powDWP}print.option.fileentry configure -state disable
           ${powDWP}print.option.filebutton configure -state disable
           ${powDWP}print.option.dirlbl configure -state disable 
           ${powDWP}print.option.direntry configure -state disable
           ${powDWP}print.option.printerlbl configure -state normal
           ${powDWP}print.option.printerentry configure -state normal
        } else {
           ${powDWP}print.option.filelbl configure -state normal
           ${powDWP}print.option.convertType configure -state normal
           ${powDWP}print.option.fileentry configure -state normal
           ${powDWP}print.option.filebutton configure -state normal
           ${powDWP}print.option.dirlbl configure -state normal
           ${powDWP}print.option.direntry configure -state normal
           ${powDWP}print.option.printerlbl configure -state disable
           ${powDWP}print.option.printerentry configure -state disable
        }
     }

     if [info exists powGraphSelection] {
        if { $powGraphSelection == "one" && \
            (![info exists currentPreviewGraph] || $currentPreviewGraph == "") } {
           set currentPreviewGraph [lindex [powAssemblePSfile $currgn] 0]
        }
     } else {
        ${powDWP}print.selection.allgraph select
     }

     set token [split $powOutputFileName "."]
     set powOutputFileType {postscript pswrite ps "Postscript Files"}

     foreach cvf $powConvertFunction {
         if { [lindex $cvf 2] == [lindex $token 1] } {
            set powOutputFileType $cvf
            tixSetSilent ${powDWP}print.option.convertType "[lindex $cvf 0] - [lindex $cvf 3]"
            break
         } 
     }

     bind ${powDWP}print.selection.entry <ButtonRelease-1> {
          global powDWP
          ${powDWP}print.selection.range select
     }
}

proc powPrint { {inputFile {}} } {
     global powSelectDirectory
     global powGraphSelection
     global powbg pcom_fname
     global powStretch powOutputPaperSize
     global powConvertFormat powConvertFunction
     global powHandles powDWP g_titleFont
     global powPaperDefXsizeInch powPaperDefYsizeInch
     global powPaperDefXsizePixel powPaperDefYsizePixel
     global powOutputPaperXsizeInch powOutputPaperYsizeInch
     global powOutputPaperXsizePixel powOutputPaperYsizePixel
     global powPlacement powPostOrient
     global powPaperSizeSelected powPixelToInchRatio
     global powPrintFunction
     global previewNameList

     global ghostScript
     global powOutputFileType
     global tcl_platform
     global searchPath

#puts "inputFile: $inputFile"
     set fileNameList {}
     if { [llength $inputFile] == 0 } {
        set fileNameList [powAssemblePSfile]
        if [info exist ${powDWP}print.selection.allgraph] {
           ${powDWP}print.selection.allgraph select
        }
     } else {
        if { [llength $inputFile] == 1 } {
           lappend fileNameList $inputFile
           if [info exist ${powDWP}print.selection.selectedgraph] {
              ${powDWP}print.selection.selectedgraph select
           }
        } else {
           lappend fileNameList $inputFile
           if [info exist ${powDWP}print.selection.range] {
              ${powDWP}print.selection.range select
           }
        }
     }
  
     set idx -1
#puts "fileNameList: $fileNameList"
#puts "fileNameList length: [llength $fileNameList]"
#puts "previewNameList: $previewNameList"
     foreach fileName $fileNameList {
#puts "fileName: $fileName"
        if { $tcl_platform(platform) != "windows" } {
           set errorFlag [ catch {
               set comm "cat $fileName | $powPrintFunction"
	       exec /bin/sh -c $comm
           } result ]

           # file delete -force $fileName

        } elseif { $tcl_platform(platform) == "windows" } {
           # this is Windows environment print
           set previewIdx [lsearch -glob $previewNameList [list * $fileName]]
           set outputName ""

#puts "previewIdx: $previewIdx"
           if { $previewIdx < 0 || ![file exists [lindex [lindex $previewNameList $previewIdx] 0]] } {
              set errorFlag [ catch {
                  exec $ghostScript -sDEVICE=jpeg \
                                    -dNOPAUSE -dBATCH -dQUIET \
                                    -sPAPERSIZE=[string tolower $powPaperSizeSelected] \
                                    -I$searchPath \
                                    -sOutputFile=tmpPrint.jpg $fileName
              } result ]

              if { !$errorFlag } {
                 set outputName $::env(PSTMPDIR)/tmpPrint.jpg
              }
           } else {
              set outputName [lindex [lindex $previewNameList $previewIdx] 0]
           }
#puts "outputName: $outputName"
         
           if { $outputName != "" } {
              set errorFlag [ catch {
                  exec $powPrintFunction "[_convertToWindowFileName $outputName]" "/print"
              } err ]
           } else {
              #tk_messageBox -icon error -parent .pow -type ok -message "Error sending graphs to printer."
           }
        }
        incr idx
        if { $idx > [llength $fileNameList] } {
        #   tk_messageBox -icon info -parent .pow -type ok -message "Successful send graphs to printer."
        }
     }
}

proc _convertToWindowFileName { path } {
    set newStr ""
    for {set i 0} {$i < [string length $path]} {incr i} {
         if { [string range $path $i $i] == "/" } {
            set newStr [format "%s\\" $newStr]
         } else {
            set newStr [format "%s%s" $newStr [string range $path $i $i]]
         }
    }

    return $newStr
}

proc powMergeGraphs { newgn } {
#puts "powMergeGraphs start"
    global currgn powPlotParam powGUI

    powPlotImages $currgn $powPlotParam(images,$newgn) .pow.pow
    powPlotCurves $currgn $powPlotParam(curves,$newgn) .pow.pow
    if { $powGUI } { powRedrawScopebox }
}

proc powEditNoteDlg { gn idx {id ""} } {
#puts "powEditNoteDlg start"
   global powPlotParam powFontParam powNote powDWP powbg
   global g_titleFont

   if { $idx=="" } {
      set idx [powFindNoteIdx $gn $id]
   }
   powSetupNoteVar $gn $idx

   #
   #  Build Dialog Window
   #

   set w ${powDWP}note
   if { [winfo exists $w] } {
      raise $w
      focus $w
      if { $powNote(idx)<0 } {
         $w.buttons.apply  config -state disabled
         $w.buttons.delete config -state disabled
      } else {
         $w.buttons.apply  config -state normal
         $w.buttons.delete config -state normal
      }
      return
   }
   powToplevel $w .pow "-bg $powbg"
   bind $w <<CloseWindow>> "destroy $w"
   wm title $w "Annotations"

   set row 1
   
   label $w.title -text "Edit/Add Annotations" -bg $powbg -font g_titleFont
   grid $w.title -row $row -column 1 -columnspan 2 -sticky n
   incr row

   grid rowconfigure $w $row -minsize 10
   incr row

   label $w.lbl  -text "Label:"     -bg $powbg -font g_titleFont
   entry $w.lblentry -width 30 -bg $powbg -textvariable powNote(string) -font g_titleFont
   grid $w.lbl      -row $row -column 1 -sticky e
   grid $w.lblentry -row $row -column 2 -sticky ew -padx 5
   incr row

   grid rowconfigure $w $row -minsize 3
   incr row

   label $w.fnt  -text "Font:"      -bg $powbg -font g_titleFont
   frame $w.fntframe -bg $powbg

   set mnu [eval tk_optionMenu $w.fntframe.fnt \
         powNote(Font) $powFontParam(allFonts,powDef)]
   $w.fntframe.fnt config -bg $powbg -highlightthickness 0 -width 20 -font g_titleFont
   $mnu config -bg $powbg -font g_titleFont

   set mnu [tk_optionMenu $w.fntframe.siz \
         powNote(Size) 7 9 12 14 16 18 24 32 40]
   $w.fntframe.siz config -bg $powbg -highlightthickness 0 -width 3 -font g_titleFont
   $mnu config -bg $powbg -font g_titleFont

   pack $w.fntframe.fnt -side left -padx 5
   pack $w.fntframe.siz -side left -padx 5
   grid $w.fnt       -row $row -column 1 -sticky e
   grid $w.fntframe  -row $row -column 2 -sticky w
   incr row

   grid rowconfigure $w $row -minsize 3
   incr row

   label $w.stl  -text "Style:"     -bg $powbg -font g_titleFont
   frame $w.stlframe -bg $powbg
   checkbutton $w.stlframe.bld -text Bold   -onvalue bold   -offvalue normal \
         -bg $powbg -variable powNote(Weight)  -highlightthickness 0 -font g_titleFont
   checkbutton $w.stlframe.itl -text Italic -onvalue italic -offvalue roman \
         -bg $powbg -variable powNote(Slant)   -highlightthickness 0 -font g_titleFont
   pack $w.stlframe.bld -side left -padx 5
   pack $w.stlframe.itl -side left -padx 5
   grid $w.stl      -row $row -column 1 -sticky e
   grid $w.stlframe -row $row -column 2 -sticky w
   incr row

   grid rowconfigure $w $row -minsize 3
   incr row

   label $w.clr -text "Color:" -bg $powbg -font g_titleFont
   button $w.clrbtn -textvariable powNote(Color) \
         -bg $powbg -highlightthickness 0 -width 7 \
         -command "powSelectColor powNote(Color)" -font g_titleFont
   grid $w.clr      -row $row -column 1 -sticky e
   grid $w.clrbtn   -row $row -column 2 -sticky w -padx 5
   incr row

   grid rowconfigure $w $row -minsize 3
   incr row

   label $w.pos  -text "Position:"   -bg $powbg -font g_titleFont
   frame $w.posframe -bg $powbg
   entry $w.posframe.x -width 14 -bg $powbg -textvariable powNote(xpos) -font g_titleFont
   entry $w.posframe.y -width 14 -bg $powbg -textvariable powNote(ypos) -font g_titleFont
   pack $w.posframe.x -side left -padx 5
   pack $w.posframe.y -side left -padx 5
   grid $w.pos      -row $row -column 1 -sticky e
   grid $w.posframe -row $row -column 2 -sticky w
   incr row
   
   grid rowconfigure $w $row -minsize 3
   incr row

   label $w.crd  -text "Attach To:" -bg $powbg -font g_titleFont
   frame $w.crdframe -bg $powbg
   radiobutton $w.crdframe.graph -text "Graph" \
         -variable powNote(coordSys) \
         -value "graph" -highlightthickness 0 -bg $powbg \
         -command powUpdateNoteCoord -font g_titleFont
   radiobutton $w.crdframe.coord -text "Coordinates" \
         -variable powNote(coordSys) \
         -value "coord" -highlightthickness 0 -bg $powbg \
         -command powUpdateNoteCoord -font g_titleFont
   pack $w.crdframe.graph -side left -padx 5
   pack $w.crdframe.coord -side left -padx 5
   grid $w.crd      -row $row -column 1 -sticky e
   grid $w.crdframe -row $row -column 2 -sticky w
   incr row

   grid rowconfigure $w $row -minsize 10
   incr row

   frame $w.buttons -bg $powbg
   button $w.buttons.apply  -text "Apply"  -bg $powbg -highlightthickness 0 \
         -command {powUpdateNote apply} -font g_titleFont
   button $w.buttons.add    -text "Add"    -bg $powbg -highlightthickness 0 \
         -command {powUpdateNote add} -font g_titleFont
   button $w.buttons.delete -text "Delete" -bg $powbg -highlightthickness 0 \
         -command {powUpdateNote delete} -font g_titleFont
   button $w.buttons.done  -text "Exit"    -bg $powbg -highlightthickness 0 \
         -command "destroy $w" -font g_titleFont
   pack $w.buttons.add    -side left -padx 5
   pack $w.buttons.delete -side left -padx 5
   pack $w.buttons.apply  -side left -padx 5
   pack $w.buttons.done   -side left -padx 5
   grid $w.buttons  -row $row -column 1 -columnspan 2
   incr row

   grid rowconfigure $w $row -minsize 5
   incr row

   if { $powNote(idx)<0 } {
      $w.buttons.apply  config -state disabled
      $w.buttons.delete config -state disabled
   } else {
      $w.buttons.apply  config -state normal
      $w.buttons.delete config -state normal
   }
}

proc powSetupNoteVar { gn idx } {
#puts "powSetupNoteVar start"
   global powNote powFontParam powPlotParam

   set powNote(gn) $gn
   if { $idx<0 && [llength $powPlotParam(Notes,$gn)]==0 } {
      # This is a new record
      # Grab font info from graph defaults

      foreach opt $powFontParam(allOpts,powDef) {
         set powNote($opt) $powFontParam(note${opt},$gn)
      }
      set powNote(xpos)     1.01
      set powNote(ypos)     0.9
      set powNote(string)   "Blank"
      set powNote(coordSys) "graph"

   } else {

      if { $idx<0 || $idx >= [llength $powPlotParam(Notes,$gn)] } {
         # Grab font info from last note
         set record [lindex $powPlotParam(Notes,$gn) end]
         set record [lreplace $record 0 0 "Blank"]
         set record [lreplace $record end end -1]
         set idx -1
      } else {
         set record [lindex $powPlotParam(Notes,$gn) $idx]
      }

      foreach [list string Font Size Weight Slant Color xpos ypos coordSys id] \
            $record {}
      foreach opt \
            [list string Font Size Weight Slant Color xpos ypos coordSys id] {
         set powNote($opt) [subst \$$opt]
      }
   }
   set powNote(idx) $idx
   set powNote(oldCoord) $powNote(coordSys)
}

proc powUpdateNote { method } {
#puts "powUpdateNote start"
   global powNote powPlotParam powDWP

   set gn $powNote(gn)
   if { $method=="delete" } {

      # Delete this Note

      if { $powNote(idx)>=0 } {
         powDeleteNote $gn $powNote(idx)
         set powNote(idx) -1
      }

   } elseif { $powNote(string)=="" } {

      error "Cannot Add/Apply an empty label"

   } else {

      # Update the Note

      set record {}
      foreach opt [list \
            string Font Size Weight Slant Color xpos ypos coordSys] {
         lappend record $powNote($opt)
      }

      if { $method=="add" || $powNote(idx)<0 } {

         # Create a new Note

         set powNote(idx) [llength $powPlotParam(Notes,$gn)]
         lappend record -1
         lappend powPlotParam(Notes,$gn) $record

      } else {  # Apply

         # Grab current ID number of the current Note

         set r [lindex $powPlotParam(Notes,$gn) $powNote(idx)]
         lappend record [lindex $r 9]

      }

      set newID [powDrawNote $gn $record]
      set record [lreplace $record 9 9 $newID]
      set powPlotParam(Notes,$gn) [lreplace \
            $powPlotParam(Notes,$gn) $powNote(idx) $powNote(idx) $record]

      powRedrawGraphHandles $gn
   }

   if { $powNote(idx)<0 } {
      ${powDWP}note.buttons.apply  config -state disabled
      ${powDWP}note.buttons.delete config -state disabled
   } else {
      ${powDWP}note.buttons.apply  config -state normal
      ${powDWP}note.buttons.delete config -state normal
   }
}


proc powUpdateNoteCoord { } {
#puts "powUpdateNoteCoord start"
   global powNote powPlotParam

   set gn   $powNote(gn)
   set xpos $powNote(xpos)
   set ypos $powNote(ypos)

   foreach [list x0 y1 x1 y0] [.pow.pow coord ${gn}box] {}

   if { $powNote(oldCoord)==$powNote(coordSys) } {
   
      set idx $powNote(idx)
      if { $idx>=0 } {
         set r [lindex $powPlotParam(Notes,$gn) $idx]
         set xpos [lindex $r 6]
         set ypos [lindex $r 7]
         set crd  [lindex $r 8]
         if { $crd==$powNote(coordSys) } {
            set powNote(xpos) $xpos
            set powNote(ypos) $ypos
            return
         }
      } else {
         return
      }

   }

   if { $powNote(coordSys)=="graph" } {

      foreach {x y} [powGraphToCanvas $gn $xpos $ypos .pow.pow] {}
      set xpos [expr ($x - $x0) / ($x1 - $x0)]
      set ypos [expr ($y - $y0) / ($y1 - $y0)]

   } elseif { $powNote(coordSys)=="coord" } {

      set x [expr $xpos * ($x1-$x0) + $x0]
      set y [expr $ypos * ($y1-$y0) + $y0]
      foreach {xpos ypos} [powCanvasToGraph $gn $x $y .pow.pow] {}

   }      
   set powNote(xpos) $xpos
   set powNote(ypos) $ypos
   set powNote(oldCoord) $powNote(coordSys)
}


proc powAddNote { gn xpos ypos string args } {
#puts "powAddNote start"
   global powPlotParam powFontParam

   array set opts [list \
         -font   $powFontParam(noteFont,powDef) \
         -size   $powFontParam(noteSize,powDef) \
         -weight $powFontParam(noteWeight,powDef) \
         -slant  $powFontParam(noteSlant,powDef) \
         -color  $powFontParam(noteColor,powDef) \
         -coord  graph \
         ]

   foreach {opt val} $args {
      if { [info exists opts($opt)] } {
         set opts($opt) $val
      } else {
         puts "Unrecognized option $opt"
      }
   }

   set record [list $string $opts(-font) $opts(-size) $opts(-weight) \
         $opts(-slant) $opts(-color) $xpos $ypos $opts(-coord) -1]
   set newID [powDrawNote $gn $record]
   set record [lreplace $record end end $newID]
   lappend powPlotParam(Notes,$gn) $record

   powRedrawGraphHandles $gn

   return $newID
}


proc powDeleteNote { gn idx } {
#puts "powDeleteNote start"
   global powPlotParam

   set r [lindex $powPlotParam(Notes,$gn) $idx]
   set id [lindex $r 9]
   if { $id>=0 && [.pow.pow find withtag $id]!="" } {
      .pow.pow delete $id
   }
   set powPlotParam(Notes,$gn) \
         [lreplace $powPlotParam(Notes,$gn) $idx $idx]

   powRedrawGraphHandles $gn
}


proc powDrawNote { gn record } {
#puts "powDrawNote start"

   foreach [list string Font Size Weight Slant Color xpos ypos coordSys id] \
         $record {}

   foreach [list x0 y1 x1 y0] [.pow.pow coord ${gn}box] {}

   if { $coordSys=="graph" } {

      set x [expr $xpos * ($x1-$x0) + $x0]
      set y [expr $ypos * ($y1-$y0) + $y0]

   } elseif { $coordSys=="coord" } {

      foreach {x y} [powGraphToCanvas $gn $xpos $ypos .pow.pow] {}
      if { $x<$x0 || $x>$x1 || $y<$y1 || $y>$y0 } {
         # Note not inside graph box
         if { $id>=0 && [.pow.pow find withtag $id]!="" } {
            .pow.pow delete $id
         }
         return -1
      }
      
   } else {
      puts "Unsupported coordSys $coordSys"
      return -1
   }

   if { $id>=0 && [.pow.pow find withtag $id]!="" } {

      .pow.pow coords $id $x $y
      .pow.pow itemconfig $id -text $string -fill $Color \
            -font [list $Font $Size $Weight $Slant] -anchor sw \
            -tags "$gn ${gn}text"

   } else {

      set id [.pow.pow create text $x $y -text $string -fill $Color \
            -font [list $Font $Size $Weight $Slant] -anchor sw \
            -tags "$gn ${gn}text"]

      .pow.pow bind $id <<DblBtnPress>> "powEditNoteDlg $gn {} $id"
#puts "calling powBindBtn 4"
      powBindBtn <<Drag>> ".pow.pow bind $id" \
            " powDragNote start $gn $id %X %Y " \
            " powDragNote drag  $gn $id %X %Y " \
            " powDragNote end   $gn $id %X %Y "

   }

   return $id
}

proc powDragNote { mode gn id X Y } {
#puts "powDragNote start"
   global powMoveX powMoveY powIsDragging powResizeMain powNote powDWP

   switch -exact $mode {
      start {
         set powIsDragging 1
      }
      drag {
         set dx [expr $X - $powMoveX]
         set dy [expr $Y - $powMoveY]
         .pow.pow move $id $dx $dy
      }
      end {
         set idx [powFindNoteIdx $gn $id]
         powRepositionNote $gn $idx
         powRedrawGraphHandles $gn
         set powIsDragging 0
         if { [winfo exist ${powDWP}note] } {
            if { $gn==$powNote(gn) && $idx==$powNote(idx) } {
               powUpdateNoteCoord
            } else {
               powEditNoteDlg $gn {} $id
            }
         }
      }
   }
   set powMoveX $X
   set powMoveY $Y
}


proc powRepositionNote { gn idx } {
#puts "powRepositionNote start"
   # Calculate the appropriate x/y position for
   # the note's current canvas position

   global powPlotParam

   if { $idx<0 || $idx>[llength $powPlotParam(Notes,$gn)] } return

   set r     [lindex $powPlotParam(Notes,$gn) $idx]
   set coord [lindex $r 8]
   set id    [lindex $r 9]
   foreach {x y}         [.pow.pow coord $id] {}
   foreach {x0 y1 x1 y0} [.pow.pow coord ${gn}box] {}

   if { $coord=="graph" } {

      set xpos [expr ($x - $x0) / ($x1 - $x0)]
      set ypos [expr ($y - $y0) / ($y1 - $y0)]

   } elseif { $coord=="coord" } {

      foreach {xpos ypos} [powCanvasToGraph $gn $x $y .pow.pow] {}
      if { $x<$x0 || $x>$x1 || $y<$y1 || $y>$y0 } {
         # Note not inside graph box
         if { $id>=0 && [.pow.pow find withtag $id]!="" } {
            .pow.pow delete $id
         }
         set r [lreplace $r 9 9 -1]
      }

   } else {

      error "Unknown coordinate system: $coord"

   }

   set r [lreplace $r 6 7 $xpos $ypos]
   set powPlotParam(Notes,$gn) [lreplace $powPlotParam(Notes,$gn)\
         $idx $idx $r]
}


proc powFindNoteIdx { gn id } {
#puts "powFindNoteIdx start"
   global powPlotParam

   set idx 0
   foreach r $powPlotParam(Notes,$gn) {
      if { [lindex $r end]==$id } break
      incr idx
   }
   if { $idx >= [llength $powPlotParam(Notes,$gn)] } {
      set idx -1
   }
   return $idx
}


proc powRedrawNotes { gn } {
#puts "powRedrawNotes start"
   global powPlotParam

   set i 0
   set notes {}
   foreach r $powPlotParam(Notes,$gn) {
      set newID [powDrawNote $gn $r]
      set r [lreplace $r end end $newID]
      lappend notes $r
   }
   set powPlotParam(Notes,$gn) $notes
}


proc powAddTextToGraphDoIt { } {
#puts "powAddTextToGraphDoIt start"
   global powDWP
   if [catch {selection get} gn] {
      set gn " "
   }
   set savebinding [bind .pow.pow <ButtonPress-1>]
   bind .pow.pow <ButtonPress-1> "\
         powPlaceText [list [${powDWP}addtext.text get 0.0 end] $gn %x %y .pow.pow]; destroy ${powDWP}addtext;\
         bind .pow.pow <ButtonPress-1> \{$savebinding\}"
}


proc powSelectColor { varName } {
#puts "powSelectColor start"
   upvar #0 $varName var
   set newClr [tk_chooseColor -initialcolor $var]
   if {$newClr != ""} {set var $newClr}
}


proc swap { a b} {
#puts "swap start"
    upvar $a one
    upvar $b two
    set tmp $one
    set one  $two
    set two $tmp
}

 
proc debug_trace {name element op} { if {$element != ""} { set name
#puts "debug_trace start"
    ${name}($element) } upvar $name x puts "Variable $name set to $x"
}


proc powUpdateTrackVars {} { 
#puts "powUpdateTrackVars start"
    global powGraphCoordsTracker powImagePixelTracker powImageValueTracker powPhysicalPixelTracker
    global powFirstPixel powPlotParam powTrackText

    set gn $powTrackText(gn)
    if [regexp {[0-9]} $powTrackText(rx)] {
	if {$powPlotParam(tickLabels,$gn)=="degrees" \
	      && [powWCSexists $gn]} {
	    set rx  [powHourRA $powTrackText(rx)]
	    set ry  [powDegDec $powTrackText(ry)]
	    set powGraphCoordsTracker \
		"Graph coordinates:\n   ( $rx, $ry )"
	} else {

	   set rxVal $powTrackText(rx)
	   if {$powPlotParam(xTickScal,$gn)=="log"} {
	      # Make sure the log value isn't out-of-bounds
	      if { $rxVal>300 || $rxVal<-300 } {
		 set rxVal "***"
	      } else {
		 set rxVal [format "%.6lg" [expr pow(10.0,$rxVal)]]
	      }
	   } else {
	      set rxVal [format "%.6lg" $rxVal]
	   }

	   set ryVal $powTrackText(ry)
	   if {$powPlotParam(yTickScal,$gn)=="log"} {
	      # Make sure the log value isn't out-of-bounds
	      if { $ryVal>300 || $ryVal<-300 } {
		 set ryVal "***"
	      } else {
		 set ryVal [format "%.6lg" [expr pow(10.0,$ryVal)]]
	      }
	   } else {
	      set ryVal [format "%.6lg" $ryVal]
	   }

	   set powGraphCoordsTracker "Graph coordinates:\n   ( $rxVal, $ryVal )"
	}
    } else {
	set powGraphCoordsTracker "Graph coordinates:\n   ( X , X )"
    }


#puts $powTrackText(imgx)
    if [regexp {[0-9]} $powTrackText(imgx)] {
       set result [powConvertImage2Physical [expr $powTrackText(imgx) + $powFirstPixel] [expr $powTrackText(imgy) + $powFirstPixel]]
        set powPhysicalPixelTracker "Physical pixel:\n   ( [lindex $result 0], [lindex $result 1])"
	set powImagePixelTracker "Image pixel:\n   ( [expr $powTrackText(imgx) + $powFirstPixel], [expr $powTrackText(imgy) + $powFirstPixel] )"

    } else {
	set powPhysicalPixelTracker "Physical pixel:\n   ( X , X )"
	set powImagePixelTracker "Image pixel:\n   ( X , X )"
    }
	
    if [regexp {[0-9]} $powTrackText(imgz)] {
       set ctoken [split $powTrackText(imgz) "."]
       if { [llength $ctoken] == 2 && [regexp {[0-9]} [lindex $ctoken 1]] } {
          set pixval [format %.16lg [expr $powTrackText(imgz)]]
       } else {
          set pixval $powTrackText(imgz)
       }
    } elseif { $powTrackText(imgz)=="NULL" } {
       set pixval "NULL"
    } else {
	set pixval "X"
    }

    set powImageValueTracker "Pixel value:\n    $pixval ($powTrackText(zunits))"
}

proc powConvertPhysical2Image { x y } {
     global powLTM_11 powLTM_12 powLTM_21 powLTM_22 powLTV1 powLTV2
     global currgn 

     set image_x $x
     set image_y $y

     if ![info exists powLTM_11($currgn)] {
        set powLTM_11($currgn) [powDetermineKeyWordExist $currgn "LTM1_1"]
        if { $powLTM_11($currgn) == false } {
           # LTM1_1, LTM1_2, LTM2_1, LTM2_2, LTV1, LTV2 not exist
           unset powLTM_11($currgn)
        } else {
           set powLTM_12($currgn) [powDetermineKeyWordExist $currgn "LTM1_2"]
           set powLTM_21($currgn) [powDetermineKeyWordExist $currgn "LTM2_1"]
           set powLTM_22($currgn) [powDetermineKeyWordExist $currgn "LTM2_2"]
           set powLTV1($currgn)   [powDetermineKeyWordExist $currgn "LTV1"]
           set powLTV2($currgn)   [powDetermineKeyWordExist $currgn "LTV2"]
        }
     }

     if [info exists powLTM_11($currgn)] {

        if { $powLTM_12($currgn) == "false" } {
           set powLTM_12($currgn) 0
        }
        if { $powLTM_21($currgn) == "false" } {
           set powLTM_21($currgn) 0
        }
        if { $powLTM_22($currgn) == "false" } {
           set powLTM_22($currgn) 0
        }
        if { $powLTV1($currgn) == "false" } {
           set powLTV1($currgn) 0
        }
        if { $powLTV2($currgn) == "false" } {
           set powLTV2($currgn) 0
        }
        set image_x [expr $powLTM_11($currgn) * $x + $powLTM_12($currgn) * $y + $powLTV1($currgn)]
        set image_y [expr $powLTM_21($currgn) * $x + $powLTM_22($currgn) * $y + $powLTV2($currgn)]
     }

     return [list $image_x $image_y]
}

proc powConvertRadiusPhysical2Image { phy_x phy_y img_x phy_radius } {
     set phy_outer_x [expr $phy_x + $phy_radius]
     set result [powConvertPhysical2Image $phy_outer_x $phy_y]
     return [expr abs([lindex $result 0] - $img_x)]
}

proc powConvertImage2Physical { x y } {
     global powLTM_11 powLTM_12 powLTM_21 powLTM_22 powLTV1 powLTV2
     global currgn 

     set physical_x $x
     set physical_y $y

     if ![info exists powLTM_11($currgn)] {
        set powLTM_11($currgn) [powDetermineKeyWordExist $currgn "LTM1_1"]
        if { $powLTM_11($currgn) == false } {
           # LTM1_1, LTM1_2, LTM2_1, LTM2_2, LTV1, LTV2 not exist
           unset powLTM_11($currgn)
        } else {
           set powLTM_12($currgn) [powDetermineKeyWordExist $currgn "LTM1_2"]
           set powLTM_21($currgn) [powDetermineKeyWordExist $currgn "LTM2_1"]
           set powLTM_22($currgn) [powDetermineKeyWordExist $currgn "LTM2_2"]
           set powLTV1($currgn)   [powDetermineKeyWordExist $currgn "LTV1"]
           set powLTV2($currgn)   [powDetermineKeyWordExist $currgn "LTV2"]
        }
     }

     if [info exists powLTM_11($currgn)] {
        if { $powLTM_12($currgn) == "false" } {
           set powLTM_12($currgn) 0
        }
        if { $powLTM_21($currgn) == "false" } {
           set powLTM_21($currgn) 0
        }
        if { $powLTM_22($currgn) == "false" } {
           set powLTM_22($currgn) 0
        }
        if { $powLTV1($currgn) == "false" } {
           set powLTV1($currgn) 0
        }
        if { $powLTV2($currgn) == "false" } {
           set powLTV2($currgn) 0
        }
        set physical_x [expr ($powLTM_22($currgn) * ($x - $powLTV1($currgn)) - $powLTM_21($currgn) * ($y - $powLTV2($currgn))) / ($powLTM_11($currgn) * $powLTM_22($currgn) - $powLTM_12($currgn) * $powLTM_21($currgn))]
        set physical_y [expr ( -1.0 * $powLTM_12($currgn) * ($x - $powLTV1($currgn)) + $powLTM_11($currgn) * ($y - $powLTV2($currgn))) / ($powLTM_11($currgn) * $powLTM_22($currgn) - $powLTM_12($currgn) * $powLTM_21($currgn))]
     }

     return [list $physical_x $physical_y]
}

proc powConvertRadiusImage2Physical { img_x img_y phy_x img_radius } {
     set img_outer_x [expr $img_x + $img_radius]
     set result [powConvertImage2Physical $img_outer_x $img_y]
     return [expr abs([lindex $result 0] - $phy_x)]
}

proc powHelp { topic } {
#puts "powHelp start topic"
   global env tcl_platform powHelpTopics powbg powDWP
   global Rw

   if { [string match "*.html" $topic] } {
      set topic [string range $topic 0 end-5]
   }

   if { [winfo exist ${powDWP}hyperhelp] == 0} {

      if { $tcl_platform(platform)=="windows"  } {
         set size large
      } else {
         set size medium
      }
      set allTopics {}
      foreach aTopic [lsort [array names powHelpTopics]] {
         lappend allTopics [list $powHelpTopics($aTopic) $aTopic]
      }

      iwidgets::hyperhelp ${powDWP}hyperhelp -title "POW: Hyperhelp" \
          -topics      $allTopics \
          -fontname    courier \
          -fontsize    $size \
          -helpdir     $env(POW_HELPDIR) \
          -background  $powbg \
          -textbackground  $powbg \
          -beforelink "powHelpResolveLink"
#        -helpdir     $env(POW_LIBRARY)
      catch { unset Rw }
   }

   ${powDWP}hyperhelp showtopic $topic
   ${powDWP}hyperhelp activate

   update idletasks
   if ![info exists Rw] {
      scan [winfo geometry ${powDWP}hyperhelp] "%dx%d+%d+%d" Rw Rh Rx Ry
      catch { wm geometry ${powDWP}hyperhelp [expr $Rw / 2]x[expr $Rh / 2]+0+0 } err
   } else {
      catch { wm geometry ${powDWP}hyperhelp ${Rw}x${Rh}+0+0 } err
   }
}

proc powHelpResolveLink { path } {
     global g_backupDir powDWP env

     if {![file exists $g_backupDir/$path] && [string first $env(POW_HELPDIR) $path] < 0 } {
        powHelp [${powDWP}hyperhelp cget -helpdir]/$path
     } else {
        powHelp $path
     }
}

proc powMax { a b } {
#puts "powMax start"
    return [expr ($a > $b) ? $a : $b]
}

proc powMin { a b } {
#puts "powMin start"
    return [expr ($a < $b) ? $a : $b]
}

proc powFindFont { w {pointsizes 120} {weight medium} {slant r}} {
#puts "powFindFont start"
    foreach family {times courier helvetica } {
	foreach points $pointsizes {
	    if {[catch {$w config -font \
			    -*-$family-$weight-$slant-*-*-*-$points-*}] == 0} {
		return -*-$family-$weight-$slant-*-*-*-$points-*
	    }
	}
    }
    $w config -font fixed
    return fixed
}

proc powGetFontList { gn lbl } {
#puts "powGetFontList start"
    global powFontParam
    
    return [list \
            $powFontParam(${lbl}Font,$gn) \
            $powFontParam(${lbl}Size,$gn) \
            $powFontParam(${lbl}Weight,$gn) \
            $powFontParam(${lbl}Slant,$gn)]
}
      

proc powScopeZoom { in_or_out {scale "no"} {value -1.0}} {
    global currgn saveROI
    global powPlotParam powZoomStart
    global g_magnification

#puts "powScopeZoom start, g_magnification: $g_magnification"
#puts "powScopeZoom start, scale: $scale"

    if ![info exists powPlotParam(prev_magnification,$currgn)] {
       set powPlotParam(prev_magnification,$currgn) 1.0
       set powPlotParam(new_magnification,$currgn) 1.0
       set powPlotParam(g_multiplier,$currgn) 4.0
       set powPlotParam(g_magnification,$currgn) 1.0
    }

    set powPlotParam(prev_magnification,$currgn) $powPlotParam(new_magnification,$currgn)
    set powPlotParam(new_magnification,$currgn) $g_magnification
    set powPlotParam(g_magnification,$currgn) $g_magnification

    if ![info exists g_magnification] {
       if { $value == -1.0 } {
          set powPlotParam(new_magnification,$currgn) 1.0
          set powPlotParam(g_multiplier,$currgn) 4.0
          set powPlotParam(g_magnification,$currgn) 1.0
          set g_magnification 1.0
       }
    } else {
       if { $value == -1.0 } {
          # exact magnification
          if [regexp "reset" $in_or_out] {
             set g_magnification 1.0
          }
          set powPlotParam(new_magnification,$currgn) $g_magnification
       } else {
          if [regexp "in" $in_or_out] {
             set powPlotParam(new_magnification,$currgn) \
                 [expr $powPlotParam(prev_magnification,$currgn) * $value] 
          } else {
             set powPlotParam(new_magnification,$currgn) \
                 [expr $powPlotParam(prev_magnification,$currgn) / $value] 
          }
          set g_magnification $powPlotParam(new_magnification,$currgn)
          set powPlotParam(g_magnification,$currgn) $g_magnification
       }
       set powPlotParam(g_multiplier,$currgn) [expr 1.0 / $powPlotParam(new_magnification,$currgn)]
       set value $powPlotParam(new_magnification,$currgn)
    }

    #set select_magnification $g_magnification

#puts "powPlotParam(prev_magnification,$currgn): $powPlotParam(prev_magnification,$currgn)"
#puts "powPlotParam(new_magnification,$currgn): $powPlotParam(new_magnification,$currgn)"
#puts "powPlotParam(g_multiplier,$currgn): $powPlotParam(g_multiplier,$currgn)"
#puts "powPlotParam(g_magnification,$currgn): $powPlotParam(g_magnification,$currgn)"
#puts "g_magnification: $g_magnification"
#puts "value: $value"

    if { $powPlotParam(new_magnification,$currgn) < [expr 1.0 / 64.0] } {
       set powPlotParam(new_magnification,$currgn) [expr 1.0 / 64.0]
       set g_magnification [expr 1.0 / 64.0]
       tk_messageBox -icon error -type ok -parent .pow \
                     -message "Couldn't zoom out any further."
       return
    }

    if { $powPlotParam(prev_magnification,$currgn) == $powPlotParam(new_magnification,$currgn) } {
       powEndROI 1
       return
    }
 
    set powZoomStart($currgn) 1
    #powEndROI 1
    set powZoomStart($currgn) 0

    #set powPlotParam(g_magnification,$currgn) $powPlotParam(new_magnification,$currgn)
    #set powPlotParam(prev_magnification,$currgn) $powPlotParam(g_magnification,$currgn)


    if { $powPlotParam(new_magnification,$currgn) == 1.0 } {
       powEndROI 1
       set powPlotParam(prev_magnification,$currgn) 1.0
       set powPlotParam(new_magnification,$currgn) 1.0
       set powPlotParam(g_multiplier,$currgn) 4.0
       set powPlotParam(g_magnification,$currgn) 1.0
       set g_magnification 1.0
       return
    }

    if [regexp "in" $in_or_out] {
       set multiplier [expr $powPlotParam(prev_magnification,$currgn) / $powPlotParam(new_magnification,$currgn)]
    } else {
       set multiplier 2.0
    }

    set powPlotParam(g_multiplier,$currgn) $multiplier

    # set GUI value 
    #set powPlotParam(g_magnification,$currgn) [expr 1.0 / $multiplier]
    #set g_magnification $powPlotParam(g_magnification,$currgn)

#puts "A1 powPlotParam(prev_magnification,$currgn): $powPlotParam(prev_magnification,$currgn)"
#puts "A1 powPlotParam(new_magnification,$currgn): $powPlotParam(new_magnification,$currgn)"
#puts "A1 powPlotParam(g_multiplier,$currgn): $powPlotParam(g_multiplier,$currgn)"
#puts "A1 powPlotParam(g_magnification,$currgn): $powPlotParam(g_magnification,$currgn)"
#puts "A1 g_magnification: $g_magnification"

    if {! [string compare [.pow.scope find  withtag ROI] ""] || ![winfo ismapped .pow.scope]} {
	# no ROI currently, ROI is whole graph OR no scopebox at all
	set ROIbbox [.pow.pow coords ${currgn}box]
	set halfwidth [expr ([lindex $ROIbbox 2] - [lindex $ROIbbox 0])/2.0]
	set halfheight [expr ([lindex $ROIbbox 3] - [lindex $ROIbbox 1])/2.0]
	set x_center [expr [lindex $ROIbbox 0] + $halfwidth]
	set y_center [expr [lindex $ROIbbox 1] + $halfheight]
	set new_halfwidth [expr $halfwidth * $multiplier]
	set new_halfheight [expr $halfheight * $multiplier]
	.pow.pow create rectangle \
	    [expr $x_center - $new_halfwidth]  \
	    [expr $y_center - $new_halfheight] \
	    [expr $x_center + $new_halfwidth]  \
	    [expr $y_center + $new_halfheight] \
	    -tags ROI -outline blue
	powEndROI 2 .pow.pow
	return
    } else {
	set ROIbbox [.pow.scope coords ROI]
	set saveROI $ROIbbox
	.pow.scope delete ROI
    }
    set halfwidth [expr ([lindex $ROIbbox 2] - [lindex $ROIbbox 0])/2.0]
    set halfheight [expr ([lindex $ROIbbox 3] - [lindex $ROIbbox 1])/2.0]
    set x_center [expr [lindex $ROIbbox 0] + $halfwidth]
    set y_center [expr [lindex $ROIbbox 1] + $halfheight]
    set new_halfwidth [expr $halfwidth * $multiplier]
    set new_halfheight [expr $halfheight * $multiplier]
    .pow.scope create rectangle \
          [expr $x_center - $new_halfwidth]  \
          [expr $y_center - $new_halfheight] \
          [expr $x_center + $new_halfwidth]  \
          [expr $y_center + $new_halfheight] \
          -tags ROI -outline blue
    powEndROI 2 .pow.scope
#puts "A powPlotParam(prev_magnification,$currgn): $powPlotParam(prev_magnification,$currgn)"
#puts "A powPlotParam(new_magnification,$currgn): $powPlotParam(new_magnification,$currgn)"
#puts "A powPlotParam(g_multiplier,$currgn): $powPlotParam(g_multiplier,$currgn)"
#puts "A powPlotParam(g_magnification,$currgn): $powPlotParam(g_magnification,$currgn)"
#puts "A g_magnification: $g_magnification"
    #set g_magnification $select_magnification

}

proc powGetCurrVariables {} {
#puts "powGetCurrVariables start"
   global powPlotParam currgn currimg
   global powCurveParam
   global powImageParam
   global powFontParam

   if { ![info exists currgn] || $currgn=="powDef" } return

   foreach opt $powPlotParam(allOpts,powDef) {
      set powPlotParam(${opt},powDef) $powPlotParam(${opt},$currgn)
   }

   if { [info exists currimg] && $currimg != "" } {
      foreach opt $powImageParam(allOpts,powDef) {
	 set powImageParam(${opt},powDef) \
	       $powImageParam(${opt}${currimg},$currgn)
      }
   }

   set crv [lindex $powPlotParam(curves,$currgn) 0]
   if { $crv != "NULL" } {
      foreach opt $powCurveParam(allOpts,powDef) {
	 set powCurveParam(${opt},powDef) \
	       $powCurveParam(${opt}${crv},$currgn)
      }
   }

   foreach lbl $powFontParam(allTypes,powDef) {
      foreach opt $powFontParam(allOpts,powDef) {
         set powFontParam(${lbl}${opt},powDef) \
               $powFontParam(${lbl}${opt},$currgn)
      }
   }

}

proc powSaveConfig { } {
    global powbg powCurveParam powImageParam powFontParam
    global powcursor powResizeMain currgn
    global showlinks powScopeHeight powScopeWidth powMinHeight powMinWidth
    global powPlotParam powShowScope powGUIposition
    global powLutButton powROIButton
    global POWRC currgn fvPrefObj

    event delete <<BtnPress>> <ButtonPress-1> <ButtonPress-2> <ButtonPress-3>
    event delete <<LUT>>      <ButtonPress-1> <ButtonPress-2> <ButtonPress-3>
    event delete <<ROI>>      <ButtonPress-1> <ButtonPress-2> <ButtonPress-3>
    event delete <<ROI_Drag>> <ButtonPress-1> <ButtonPress-2> <ButtonPress-3>
    event delete <<DblBtnPress>>     <Double-ButtonPress-$powROIButton> <Double-ButtonPress-$powLutButton>
    event delete <<BackToOriginal>>  <Double-ButtonPress-$powLutButton> <Double-ButtonPress-$powROIButton>

    event add <<DblBtnPress>>     <Double-ButtonPress-$powLutButton>
    event add <<BackToOriginal>>  <Double-ButtonPress-$powROIButton>
    event add    <<BtnPress>> <ButtonPress-1> <ButtonPress-2> <ButtonPress-3>
    event add    <<LUT>>  <ButtonPress-$powLutButton>

    if { $powROIButton != 0 && $powROIButton != "NULL" } {
       # Must delete BtnPress sequence to prevent it from hiding the ROI event
       event delete <<BtnPress>>          <ButtonPress-$powROIButton>
       event add    <<ROI>>               <ButtonPress-$powROIButton>
       event add    <<ROI_Drag>>          <ButtonPress-$powLutButton>
    }

    if [catch {open $POWRC w} RCFILE] {
	error "Couldn't open $POWRC, not saving configuration"
    } else {
        puts $RCFILE "\n# Application parameters:"
	puts $RCFILE "set powbg \"$powbg\""
	puts $RCFILE "set powcursor \"$powcursor\""
	puts $RCFILE "set powResizeMain \"$powResizeMain\""
	puts $RCFILE "set showlinks \"$showlinks\""
        if { $powShowScope } {
	   puts $RCFILE "set powScopeWidth \"$powScopeWidth\""
	   puts $RCFILE "set powScopeHeight \"$powScopeHeight\""
	} else {
	   puts $RCFILE "set powScopeWidth \"0\""
	   puts $RCFILE "set powScopeHeight \"0\""
	}
	puts $RCFILE "set powMinHeight \"$powMinHeight\""
	puts $RCFILE "set powMinWidth \"$powMinWidth\""
        puts $RCFILE "set powGUIposition \"$powGUIposition\""
        puts $RCFILE "set powLutButton \"$powLutButton\""
        puts $RCFILE "set powROIButton \"$powROIButton\""

        puts $RCFILE "\n# Default Font Parameters:"

        foreach lbl $powFontParam(allTypes,powDef) {
           foreach opt $powFontParam(allOpts,powDef) {
              puts $RCFILE "set powFontParam(${lbl}${opt},powDef)\
                    \"$powFontParam(${lbl}${opt},powDef)\""
           }
        }

        if [info exists powPlotParam(xdimdisp,$currgn)] {
           set powPlotParam(xdimdisp,powDef) $powPlotParam(xdimdisp,$currgn)
           set powPlotParam(ydimdisp,powDef) $powPlotParam(ydimdisp,$currgn)
           # set result [$fvPrefObj setNewGraphSize [list $powPlotParam(xdimdisp,$currgn) $powPlotParam(ydimdisp,$currgn)]]
        } 
        puts $RCFILE "\n# Default Graph Parameters:"
	foreach opt $powPlotParam(allOpts,powDef) {
	    puts $RCFILE "set powPlotParam($opt,powDef)\
		  \"$powPlotParam($opt,powDef)\""
	}

        puts $RCFILE "\n# Default Curve Parameters:"
	foreach opt $powCurveParam(allOpts,powDef) {
	    puts $RCFILE "set powCurveParam($opt,powDef) \"$powCurveParam($opt,powDef)\""
	}

        puts $RCFILE "\n# Default Image Parameters:"
	foreach opt $powImageParam(allOpts,powDef) {
	    puts $RCFILE "set powImageParam($opt,powDef) \"$powImageParam($opt,powDef)\""
	}

	close $RCFILE
    }
}

proc powBreakAllLinks { } {
#puts "powBreakAllLinks start"
#deletes all link info
    global axisToChainHash chainToAxisHash nextchain
 
    if [info exists axisToChainHash] {
	unset axisToChainHash 
	unset chainToAxisHash 
	unset nextchain
    }
    .pow.pow delete link
}
    


proc powLinkAxes {gn1 axis1 gn2 axis2} {
#puts "powLinkAxes start"
    global axisToChainHash chainToAxisHash nextchain

#catch stupid input
    if {$gn1 == $gn2} {return}
    

    if {![info exists nextchain]} {
	set nextchain 1
    }
    set graphlist [powListGraphs] 

    if {[lsearch $graphlist $gn1] == -1} {return "graph $gn1 does not exist";}
    if {[lsearch $graphlist $gn2] == -1} {return "graph $gn2 does not exist";}

    set chain1 0
    set chain2 0
    
    if {[array names axisToChainHash $gn1$axis1] != ""} {set chain1 $axisToChainHash($gn1$axis1)}
    if {[array names axisToChainHash $gn2$axis2] != ""} {set chain2 $axisToChainHash($gn2$axis2)}
    if {$chain1 == 0 && $chain2 == 0} {
#new chain
	set axisToChainHash($gn1$axis1) $nextchain
	set axisToChainHash($gn2$axis2) $nextchain
	set chainToAxisHash($nextchain) [list $gn1$axis1 $gn2$axis2]
	incr nextchain
    } elseif {$chain1 != 0 && $chain2 !=0} {
#two chains, delete one
	powMergeChains $chain1 $chain2
    } elseif {$chain1 != 0} {
#add axis 2 to chain 1	
	set axisToChainHash($gn2$axis2) $chain1
	lappend chainToAxisHash($chain1) $gn2$axis2
    } else {
#add axis 1 to chain 2
	set axisToChainHash($gn1$axis1) $chain2
	lappend chainToAxisHash($chain2) $gn1$axis1
    }
}

proc powMergeChains {chain1 chain2} {
#puts "powMergeChains start"
#delete two existing chains and make a new one consisting of all of their
#members    
    global axisToChainHash chainToAxisHash nextchain
    foreach axis [array names axisToChainHash] {
	if {$axisToChainHash($axis) == $chain1 || \
		$axisToChainHash($axis) == $chain2} {
	    set axisToChainHash($axis) $nextchain
	}
    }
    set chainToAxisHash($nextchain) [concat $chainToAxisHash($chain1) $chainToAxisHash($chain2)]
    unset chainToAxisHash($chain1)
    unset chainToAxisHash($chain2)
}
    
 
proc powBreakLink {gn axis} {
#puts "powBreakLink start"
#removes graph 1 axis 1 from the chain it belongs to
    global axisToChainHash chainToAxisHash
    set chain 0
    if {[array names axisToChainHash $gn$axis] != ""} {
	set chain $axisToChainHash($gn$axis)
    } else {
	puts "Graph $gn axis $axis is not part of a chain"
	return
    }
    unset axisToChainHash($gn$axis)
    if {[llength $chainToAxisHash($chain)] <= 2} {
	unset chainToAxisHash($chain)
    } else {
	set chainToAxisHash($chain) [lreplace $chainToAxisHash($chain) [set bozo [lsearch $chainToAxisHash($chain) $gn$axis]] $bozo]
    }
}

proc chop {theString} {
#puts "chop start"
    return [string range $theString 0 [expr [string length $theString]-2]]
}
    

proc powAlignChain {gn axis orient {gap default}} {
#puts "powAlignChain start"
#stacks all graphs in a chain either (H)orizontally or (V)ertically
    global axisToChainHash chainToAxisHash powResizeMain powPlotParam   
    if {![info exists axisToChainHash($gn$axis)]} return;


    foreach graph $chainToAxisHash($axisToChainHash($gn$axis)) {
    #some (all?) graphs in a chain may be unmapped so loop until we get one
    #that is mapped
	set oldgraph [lindex $chainToAxisHash($axisToChainHash($gn$axis)) 0]
	set oldgraph [chop $oldgraph]
	if {[.pow.pow find withtag $oldgraph] != ""} break
    }
    foreach graph [lrange $chainToAxisHash($axisToChainHash($gn$axis)) 1 end] {
	set graph [chop $graph]
	if {[.pow.pow find withtag $graph] != ""} {
	    #good, this graph is mapped
	    set gbox [.pow.pow coords ${oldgraph}box]
	    set bbox [.pow.pow bbox ${oldgraph}]
	    if {[lindex $orient 0] == "H"} {
		if {$gap == "default"} {
		    set gap [expr 2 * $powPlotParam(xmargin,$graph)]
		}
		set toX [expr [lindex $bbox 2] \
			     + $gap ]
		set toY [lindex $gbox 1]
	    } else {
		if {$gap == "default"} {
		    set gap [expr 2 * $powPlotParam(ymargin,$graph)]
		}
		set toX [lindex $gbox 0]
		set toY [expr [lindex $bbox 3] + $gap ]
	    }
	    set coords [.pow.pow coords ${graph}box]
	    set fromX [lindex $coords 0] 
	    set fromY [lindex $coords 1] 
	    powMoveGraph $graph [expr int($toX - $fromX)] [expr int($toY - $fromY)]
	    set oldgraph $graph
	}
    }
    powReconfigureToplevel $powResizeMain
}


proc powReconfigureToplevel {{resizemain 1}  } {
#puts "powReconfigureToplevel start"
    global powMinHeight powMinWidth powMaxWidth powMaxHeight
    global powRealMinHeight powRealMinWidth
    global powHeaderWidth powHeaderHeight
    global powContainer

#resize POW window if necessary
#    update idletasks
    set bigbbox [.pow.pow bbox all]
    if {$resizemain &&  ($powContainer == "none" || $powContainer == "NULL")} {
        set windowX [expr [lindex $bigbbox 2] - [lindex $bigbbox 0] + 50]
	set windowY [expr [lindex $bigbbox 3] - [lindex $bigbbox 1] + 50]
       
        set windowX [powMax $windowX $powMinWidth ]
        set windowY [powMax $windowY $powMinHeight]

        set windowX [powMin $windowX $powMaxWidth ]
        set windowY [powMin $windowY $powMaxHeight]

        incr windowX $powHeaderWidth
        incr windowY $powHeaderHeight

        set windowX [powMax $windowX $powRealMinWidth ]
        set windowY [powMax $windowY $powRealMinHeight]

        foreach {x y} [lrange [split [wm geometry .pow] {x+-}] 2 3] {}
	if {$x != 0 && $y != 0} {
	    catch {wm geometry .pow "${windowX}x${windowY}+$x+$y"}
	} else {
	    catch {wm geometry .pow "${windowX}x${windowY}"}
	}
    }

    # Check if scrollregion has significantly changed so that one doesn't
    # force a full-screen update

    set currBnds [.pow.pow cget -scrollregion]
    set newBnds [list [expr [lindex $bigbbox 0] - 20] \
	              [expr [lindex $bigbbox 1] - 20] \
	              [expr [lindex $bigbbox 2] + 20] \
	              [expr [lindex $bigbbox 3] + 20] ]

    if { [expr abs([lindex $currBnds 0]-[lindex $newBnds 0])]>10 \
	    || [expr abs([lindex $currBnds 1]-[lindex $newBnds 1])]>10 \
	    || [expr abs([lindex $currBnds 2]-[lindex $newBnds 2])]>10 \
	    || [expr abs([lindex $currBnds 3]-[lindex $newBnds 3])]>10 } {
	.pow.pow configure -scrollregion $newBnds
    }

    powShowLinks
}
    


proc powChangeBg {} {
#puts "powChangeBg start"
    global powbg powShowHandlesFlag powPlotParam

    set oldpowbg [.pow.pow cget -background]

    foreach com [info commands .pow.*] {
	if {[$com cget -bg] == $oldpowbg} {
	    catch {$com configure  -bg $powbg}
	}
	if {[$com cget -background] == $oldpowbg} {
	    catch {$com configure -background $powbg}
	}
    }

#catch the next line in case no graphs yet    
    catch {.pow.currgn configure -background yellow}

    .pow configure -bg $powbg

    foreach gn [powListGraphs] {
	if {![regexp {scope$} $gn] && \
		     $powPlotParam(bgcolor,$gn) == $oldpowbg} {
	    powGraphOptions $gn bgcolor $powbg
	}
    }
    
}


proc powExit { }  {
    global axisToChainHash chainToAxisHash nextchain powGUI powRegionListGlobal 
    #set powRegionListGlobal {}

    destroy .pow
    catch { unset axisToChainHash }
    catch { unset chainToAxisHash }
    catch { unset nextchain}
    powCleanUp
}

proc powLogGraph { gn x y } {
#puts "powLogGraph start"
   global powPlotParam powCurveParam

   # Cannot have log plots with WCS information
   if { [powWCSexists $gn] && ($x=="log" || $y=="log") } {
      error "Cannot apply log transforms to WCS graphs"
   }

   foreach bnd [list xBot yBot xTop yTop xTickScal yTickScal] {
      set $bnd $powPlotParam($bnd,$gn)
   }

   if { $powPlotParam(curves,$gn) != "NULL" } {

      # Try to preserve the bounding box region

      if { $x!=$xTickScal } {
	 if { $x=="log" } {
	    if { $xBot>0.0 } {
	       set xBot [expr log10($xBot)]
	    } else {
	       set xBot NULL
	    }
	    if { $xTop>0.0 } {
	       set xTop [expr log10($xTop)]
	    } else {
	       set xTop NULL
	    }
	 } else {
	    if { $xBot<-300 || $xBot>300 } {
	       set xBot NULL
	    } else {
	       set xBot [expr pow(10.0,$xBot)]
	    }
	    if { $xTop<-300 || $xTop>300 } {
	       set xTop NULL
	    } else {
	       set xTop [expr pow(10.0,$xTop)]
	    }
	 }
      }
      if { $y!=$yTickScal } {
	 if { $y=="log" } {
	    if { $yBot>0.0 } {
	       set yBot [expr log10($yBot)]
	    } else {
	       set yBot NULL
	    }
	    if { $yTop>0.0 } {
	       set yTop [expr log10($yTop)]
	    } else {
	       set yTop NULL
	    }
	 } else {
	    if { $yBot<-300 || $yBot>300 } {
	       set yBot NULL
	    } else {
	       set yBot [expr pow(10.0,$yBot)]
	    }
	    if { $yTop<-300 || $yTop>300 } {
	       set yTop NULL
	    } else {
	       set yTop [expr pow(10.0,$yTop)]
	    }
	 }
      }

      set powPlotParam(xBot,$gn) $xBot
      set powPlotParam(yBot,$gn) $yBot
      set powPlotParam(xTop,$gn) $xTop
      set powPlotParam(yTop,$gn) $yTop
   }

   set powPlotParam(xTickScal,$gn) $x
   set powPlotParam(yTickScal,$gn) $y

   if { $x=="log" } { set x Yes } else { set x No }
   if { $y=="log" } { set y Yes } else { set y No }
   foreach crv $powPlotParam(curves,$gn) {
      if { $crv == "NULL" } continue
      set powCurveParam(logX${crv},$gn) $x
      set powCurveParam(logY${crv},$gn) $y
   }
   powEraseGraph $gn 1
   powMapGraph $gn
#   powAdornGraph $gn .pow.pow
#   powRedrawGraphHandles $gn
}

proc powEraseGraph { gn {scope 1}} {
#puts "powEraseGraph start"
   global powGUI currgn

#Removes a graph from the display
   .pow.pow delete $gn
   .pow.pow delete ${gn}handles
   if { $currgn==$gn } {
      .pow.pow delete current_gn
      if {$powGUI && $scope} {
	 .pow.scope delete all
      }
   }
}

proc powDeleteGraph { gn {opt "prompt"} } {
   global powFitsHeader powFitsHeaderCnt
   global xCount yCount powWCS

#puts "powDeleteGraph start"
   if { $opt == "prompt" } {
      set feedback [promptMsg "This will permanently delete current graph.\n Do you want to continue?" \
                    return Yes No]

      if { $feedback == "CANCEL" } return
   }

   catch {
     powDestroyGraph $gn
   } err
   catch {
     powDeleteImage $gn $gn
   } err
   catch {
     powDeleteCurve $gn $gn
   } err
   catch { unset powFitsHeader($gn) }
   catch { unset powFitsHeaderCnt($gn) }
   catch { unset powFitsHeader(${gn}scope) }
   catch { unset powFitsHeaderCnt(${gn}scope) }
   catch { unset xCount($gn) }
   catch { unset xCount(${gn}scope) }
   catch { unset yCount($gn) }
   catch { unset yCount(${gn}scope) }
   catch { unset powWCS($gn) }
   catch { unset powWCS(${gn}scope) }
}

proc powFreeGraph { gn } {
#puts "powFreeGraph start"
   # Called from powDestroyGraph
   global powOrderedGraphList

   set idx [lsearch -exact $powOrderedGraphList $gn]
   if { $idx >= 0 } {
      set powOrderedGraphList \
            [lreplace $powOrderedGraphList $idx $idx ""]
   }

   [gNotifications default] postMessage $gn graphHasBeenDestroyed
}

proc powUnmapGraph { gn } {
#puts "powUnmapGraph start"
   global currgn powScopeGn

   powEraseGraph $gn 1
   if { $currgn == $gn } {
      set currgn "powDef"
      set powScopeGn "-"
      set otherGraphs [.pow.pow find withtag gbox]
      if {$otherGraphs != ""} {
	 set newGraph [lindex [.pow.pow gettags [lindex $otherGraphs end]] 0]
	 powSelectGraph $newGraph
      } else {
         powUpdateGraphMenuOptions
         powUpdateCurrentDialogs
      }
   }
}

proc powMapGraph { gn {restore_position 0}} {
#puts "powMapGraph start"
    global powPlotParam

    if $restore_position {
	set xo $powPlotParam(xo,$gn) 
	set yo $powPlotParam(yo,$gn)
    }


    powCreateGraph $gn $powPlotParam(curves,$gn) $powPlotParam(images,$gn) \
	$powPlotParam(xunits,$gn) $powPlotParam(yunits,$gn) \
	$powPlotParam(xlabel,$gn) $powPlotParam(ylabel,$gn) \
	$powPlotParam(xdimdisp,$gn) $powPlotParam(ydimdisp,$gn) \
	$powPlotParam(xBot,$gn) $powPlotParam(yBot,$gn) \
	$powPlotParam(xTop,$gn) $powPlotParam(yTop,$gn) 
	

    if $restore_position {
	powMoveGraphTo $gn $xo $yo .pow.pow
    }
}

proc powDeleteImage  {gn img} {
#puts "powDeleteImage start"
    global powPlotParam 
    .pow.pow delete ${img}disp$gn
    catch {.pow.scope delete ${img}disp${gn}scope}
    .pow.pow delete current_img

    catch {
       image delete $img
       image delete $gn
    } err
    catch {
       image delete ${img}scope
       image delete ${gn}scope
    } err
    catch {
       image delete ${curve}$img
       image delete ${curve}$gn
    } err
    set whichImage [lsearch $powPlotParam(images,$gn) $img] 
    set powPlotParam(images,$gn) [lreplace $powPlotParam(images,$gn) $whichImage $whichImage]
}

proc powDeleteCurve  {gn curve} {
#puts "powDeleteCurve start"
#deletes 
    global powPlotParam 
    .pow.pow delete ${curve}$gn
    catch {.pow.scope delete ${curve}${gn}scope}
    set whichCurve [lsearch $powPlotParam(curves,$gn) $curve] 
    set powPlotParam(curves,$gn) [lreplace $powPlotParam(curves,$gn) $whichCurve $whichCurve]
}


proc invert_cmap_if_flag_set { gn img } {
#puts "invert_cmap_if_flag_set start"
   global cmap_inv powPseudoImages currimg currgn powImageParam

   if { $powImageParam(invert${img},$gn) } {
      if $powPseudoImages {
	 ${img}disp${gn} colormap invert
      } else {
	 powPhotoColorTable invert
      }
   }
}

proc powAddCustomLut { cmapName lut } {
#puts "powAddCustomLut start"
   global powImageParam powbg

   if { [expr [llength $lut]%3]!=0 } {
      error "Lut must be list with multiple-of-3 elements (R G B)"
   }

   set powImageParam(cmapLUT_$cmapName,powDef) $lut

   set allMaps $powImageParam(allMaps,powDef)
   set map [lindex $allMaps end]
   if { [lindex $map 0] == "Custom" } {
      if { [lsearch -exact $map $cmapName]==-1 } {
         lappend map $cmapName
         # Add menu item to Custom menu
         .pow.mbar.colors.cCustom add radiobutton -label $cmapName \
               -value $cmapName \
               -variable powImageParam(colormap,powDef) \
               -command "powCmds::colormap $cmapName"
      }
      set powImageParam(allMaps,powDef) [lreplace $allMaps end end $map]
   } else {
      lappend powImageParam(allMaps,powDef) [list Custom $cmapName]
      #  Add Cust menu plus this item
      set bdVal [.pow.mbar.colors cget -bd]
      set idx [.pow.mbar.colors index [lindex $map 0]]
      incr idx
      menu .pow.mbar.colors.cCustom -bg $powbg -bd $bdVal
      .pow.mbar.colors insert $idx cascade -menu .pow.mbar.colors.cCustom \
            -label "Custom"
      .pow.mbar.colors.cCustom add radiobutton -label $cmapName \
            -value $cmapName \
            -variable powImageParam(colormap,powDef) \
            -command "powCmds::colormap $cmapName"
   }
   return
}

proc powSetLut { gn img scale {recalc {}} } {
#puts "powSetLut start"
    global powSqueeze powSlide powClen
    global currimg currgn powPlotParam powImageParam

    set powClen 255

    if {$scale == $powImageParam(scale${img},$gn) && $recalc=="" \
            && [info exists powImageParam(lut${img},$gn)] } {
       powSetImageScale $gn $img $scale
       powCmapStretchIntensity $gn $img $powClen $powClen \
               $powImageParam(lut${img},$gn)
       return
    }

    powSetImageScale $gn $img $scale

    set powSqueeze 0.0
    set powSlide 0.0
    powCmapStretchIntensity $gn $img $powClen $powClen \
            [list 0 0 $powClen $powClen]
}

proc powSetImageScale { gn img scale } {
#puts "powSetImageScale start"
    global powPseudoImages currimg currgn
    global powPlotParam powImageParam powGUI
    
    foreach {gn2 img2} [powGetColorbarLink $gn $img] {}
    set powImageParam(scale${img},$gn) $scale
    if { $gn2 != "" } {
       set powImageParam(scale${img2},$gn2) $scale
    }
    if { $powGUI && ![regexp scope$ $gn] } {
        set powImageParam(scale${img},${gn}scope) $scale
        if { $gn2 != "" } {
            set powImageParam(scale${img2},${gn2}scope) $scale
        }
    }

    #  Make sure we only do the equalization on original image, not colorbar
    #  nor colorbar's scope image

    if { $scale == "histo" } {
        if { $powGUI && [regexp scope$ $gn] } {
            set gn [string range $gn 0 [expr [string length $gn]-6]]
            foreach {gn2 img2} [powGetColorbarLink $gn $img] {}
        }
        if { [regexp _colorbar$ $img] } {
            if { $img2 != "" } {
                set gn $gn2
                set img $img2
            }
        }
    }

    if { $scale == "histo" } {
        set minmax [powImageScale $scale $img \
                $powImageParam(RBmin${img},$gn) \
                $powImageParam(RBmax${img},$gn)]
    } else {
        powImageScale $scale
    }
}


	
proc powCmapStretchIntensity { gn img cwidth clen lut } {
#puts "powCmapStretchIntensity start"
    global powPseudoImages
    global powPlotParam powImageParam powGUI
    
    foreach {gn2 img2} [powGetColorbarLink $gn $img] {}
    set powImageParam(lut${img},$gn) $lut
    if { $gn2 != "" } {
       set powImageParam(lut${img2},$gn2) $lut
    }
    if { $powGUI && ![regexp scope$ $gn] } {
       set powImageParam(lut${img},${gn}scope) $lut
       if { $gn2 != "" } {
           set powImageParam(lut${img2},${gn2}scope) $lut
       }
    }

    if $powPseudoImages {
       ${img}disp${gn} cmap_stretch intensity $cwidth $clen $lut
       if { $gn2 != "" } {
	  ${img2}disp${gn2} cmap_stretch intensity $cwidth $clen $lut
       }
    } else {
#       powPhotoColorTable $powImageParam(colormap${img},$gn)
       powPhotoCmapStretch $cwidth $clen $lut
    }
}

	
proc powBoundDiddleLut {gn img x y} {
#puts "powBoundDiddleLut start"
    set cx [.pow.pow canvasx $x]
    set cy [.pow.pow canvasy $y]
    set bbox [.pow.pow coords ${gn}box]
    set lx [lindex $bbox 0]
    set ly [lindex $bbox 1]
    set ux [lindex $bbox 2]
    set uy [lindex $bbox 3]
#make range from -1 to 1
    set fx [expr (2.0 * ($cx - $lx)/($ux - $lx) - 1.0)]
    set fy [expr (2.0 * ($cy - $ly)/($uy - $ly) - 1.0)]

    powDiddleLut $gn $img $fx $fy

}
    


proc powDiddleLut { gn img slide squeeze } {
#puts "powDiddleLut start"
#$squeze and $slide should range from -1 to 1, not inclusive
    global powSqueeze powSlide currimg powClen
    global powPseudoImages powPlotParam powImageParam currgn

    set powSqueeze $squeeze
    set powSlide $slide

    if { ![info exist powClen] } return

    set squeeze [expr double($powSqueeze)]
    set slide [expr double($powSlide)]
    set increment [expr int($powClen * $slide)]

    if {$squeeze > 0 && $squeeze < 1} {
	set factor [expr (1.0 - $squeeze)] 
	set increment [expr $increment + $squeeze*$powClen/5.0]
    } elseif {$squeeze < 0 && $squeeze > - 1} {
	set factor [expr 1.0/(1.0 + $squeeze)] 
	set increment [expr $increment - ($factor - 1.0) * $powClen/5.0]
    } else {
	set factor 1.0
    }

    for {set i 0} {$i <= $powClen} {incr i 5} {
	set newi [expr $i * $factor + $increment]
	set newi [expr ($newi < 0 ) ? 0 : $newi]
	set newi [expr ($newi > $powClen ) ? $powClen : $newi]
	lappend l2 [expr int(floor($newi))]
        lappend l2 $i
    }

    powCmapStretchIntensity $gn $img $powClen $powClen $l2
    if { !$powPseudoImages } {
        powReditherImages $gn $img
    }
}

proc powShowHandles {showhandles } {
#puts "powShowHandles start"
    global powShowHandlesFlag currgn powbg powPlotParam tcl_platform
    global powHiddenWindows

    set powShowHandlesFlag $showhandles
    if {$showhandles} then {
	.pow.pow raise current_img
        .pow.pow raise current_gn
	.pow.pow itemconfigure current_img -outline green
	.pow.pow itemconfigure current_gn -outline yellow
	.pow.pow raise buttonfg
	.pow.pow configure -bg $powbg
	foreach graph [powListGraphs] {
	    .pow.pow itemconfigure ${graph}bkg -fill $powPlotParam(bgcolor,$graph) \
		-outline $powPlotParam(bgcolor,$graph)
#for win32
		    if {[string match "Win*" $tcl_platform(os) ] &&\
			    ![regexp {scope$} $graph]} {
			.pow.pow delete deleteMe
			set images $powPlotParam(images,$graph)
			set powPlotParam(images,$graph) 'NULL'
			powPlotImages $graph  $images .pow.pow
		    }
	}
	foreach key [array names powHiddenWindows "*,loc"] {
	   foreach [list wind k] [split $key ,] {}
	   eval .pow.pow create window $powHiddenWindows($wind,loc) \
		 -tags {$powHiddenWindows($wind,tags)} -window $wind \
		 -anchor $powHiddenWindows($wind,anchor)
	   unset powHiddenWindows($key)
	}
    } else {
	.pow.pow itemconfigure ohandle -outline {}
	.pow.pow lower ohandle
	.pow.pow itemconfigure graphbkg -fill {} -outline {}
#	.pow.pow configure -bg white
	.pow.pow raise ${currgn}line
	foreach wind [.pow.pow find withtag canvas_window] {
	    set windowname [.pow.pow itemcget $wind -window]
	    set powHiddenWindows($windowname,loc) \
		  [.pow.pow coord $wind]
	    set powHiddenWindows($windowname,tags) \
		  [.pow.pow itemcget $wind -tags]
	    set powHiddenWindows($windowname,anchor) \
		  [.pow.pow itemcget $wind -anchor]
	    .pow.pow delete $wind
#	    $windowname configure -foreground white -background white \
#		-highlightthickness 0 -relief flat
	}

    }
}

proc chopped {theString} {
#puts "chopped start"
    return [string index $theString [expr [string length $theString] - 1]]
}


proc powShowLinks { } {
#puts "powShowLinks start"
    global showlinks axisToChainHash chainToAxisHash
    .pow.pow delete link
    if {$showlinks} then {
	foreach chain [array names chainToAxisHash] {
	    set oldaxis [chopped $chainToAxisHash($chain)]
	    set oldgraph [chop [lindex $chainToAxisHash($chain) 0]]
	    foreach graph [lrange $chainToAxisHash($chain) 1 end] {
		set axis [chopped $graph]
		set graph [chop $graph]
		set abox [.pow.pow coords ${oldgraph}box]
		set bbox [.pow.pow coords ${graph}box]
		if {$oldaxis == "X"} {
		    set fromX [expr int(([lindex $abox 2]+[lindex $abox 0])/2.0)]
		    set fromY [expr int([lindex $abox 3])]
		} else {
		    set fromY [expr int(([lindex $abox 3]+[lindex $abox 1])/2.0)]
		    set fromX [expr int([lindex $abox 0])]
		}
		if {$axis == "X"} {
		    set toX [expr int(([lindex $bbox 2]+[lindex $bbox 0])/2.0)]
		    set toY [expr int([lindex $bbox 3])]
		} else {
		    set toY [expr int(([lindex $bbox 3]+[lindex $bbox 1])/2.0)]
		    set toX [expr int([lindex $bbox 0])]
		}
		.pow.pow create line $fromX $fromY $toX $toY -tags "link" -fill pink
		set oldaxis $axis
		set oldgraph $graph
	    }
	}
    }
}


proc powGetColorbarLink { gn img } {
#puts "powGetColorbarLink start"
   global powPlotParam

   if { [info exists powPlotParam(Colorbar${img},$gn)] } {
      regexp "(.*)disp(.*)" $powPlotParam(Colorbar${img},$gn) z img2 gn2
   } elseif { [info exists \
	 powPlotParam(Colorbar${img}_colorbar,${gn}_colorbar)] } {
      set img2 ${img}_colorbar
      set gn2 ${gn}_colorbar
   } else {
      set img2 ""
      set gn2 ""
   }
   return [list $gn2 $img2]
}


proc powSetCurrImageOpts { args } {
#puts "powSetCurrImageOpts start"
   global powImageParam powPlotParam curr_img currimg currgn

   if { [info exists curr_img] && $currgn!="powDef" } {
      foreach img $powPlotParam(images,$currgn) {
	 eval powSetImageOptions $currgn $img $args
      }
   }
}

proc powSetImageOptions {gn image {args ""}} {
#puts "powSetImageOptions start"
   global powPlotParam powImageParam powGUI currgn

   foreach {gn2 image2} [powGetColorbarLink $gn $image] {}

   if { $args == "" } {
      set lst ""
      foreach opt $powImageParam(allOpts,powDef) {
	 catch {lappend lst $opt $powImageParam(${opt}${image},$gn)}
      }
      return $lst
   } else {
      foreach {opt val} $args {
	 set idx [lsearch -exact $powImageParam(allOpts,powDef) $opt]
	 if { $idx != -1 } {
	    set powImageParam(${opt}${image},$gn) $val
	    if { $gn2 != "" } {
	       set powImageParam(${opt}${image2},$gn2) $val
	    }
	 }
	 if { $opt=="scale" } {
	    powSetLut $gn $image $val forceIt
	 }
      }
      if { [.pow.pow find withtag ${image}disp${gn}] != "" } {
	 powSetColorTable $gn $image
	 powReditherImages $gn $image
      }
   }
}

proc powPlotImages {gn images {canvas ".pow.pow"}} {
#puts "powPlotImage starts"
    global powPlotParam powImageParam
    global filename_array powcursor
    global powPseudoImages powRBmin powRBmax

    if [regexp "NULL" $images] return

    #   remove "NULL" from images list if present
    if [regexp "NULL" $powPlotParam(images,$gn)] {
       set powPlotParam(images,$gn) {}
    }

    set imgcnt 0
    foreach current_image "$images" {

#   if image is already in list, don't plot it
	if {[lsearch $powPlotParam(images,$gn) $current_image]>=0} continue

	# Check if image's WCS/scaling is consistent with this graph
       if { [catch {powTestImage $gn $current_image} err] } {
	  tk_messageBox -icon error -type ok -parent .pow \
		-message "Couldn't place $current_image into graph...\
		\n\n\"$err\"\n\nSkipping image."
	  continue
       }

	incr imgcnt

#puts "\nplotting image: $current_image in $gn"

# Setup defaults... powDef for images, original for colorbars

       if { $canvas == ".pow.scope" } {
	  set trueGn [string range $gn 0 [expr [string length $gn]-6] ]
	  foreach opt $powImageParam(allOpts,powDef) {
	     set powImageParam(${opt}${current_image},$gn) \
		   $powImageParam(${opt}${current_image},$trueGn)
	  }
	  set powImageParam(RBmin${current_image},$gn) \
		$powImageParam(RBmin${current_image},$trueGn)
	  set powImageParam(RBmax${current_image},$gn) \
		$powImageParam(RBmax${current_image},$trueGn)
#	  set powImageParam(lut${current_image},$gn) \
#		$powImageParam(lut${current_image},$trueGn)
       } elseif { [string match "*_colorbar" $gn] && \
	      [string match "*_colorbar" $current_image] } {
	   regexp "(.*)disp(.*)" \
		 $powPlotParam(Colorbar${current_image},$gn) z orig_img orig_gn
	   foreach opt $powImageParam(allOpts,powDef) {
	      if { ![info exists powImageParam(${opt}${current_image},$gn)] } {
		 set powImageParam(${opt}${current_image},$gn) \
		       $powImageParam(${opt}${orig_img},$orig_gn)
	      }
	      set powImageParam(lut${current_image},$gn) \
		    $powImageParam(lut${orig_img},$orig_gn)
	   }
	} else {
	   foreach opt $powImageParam(allOpts,powDef) {
#puts "pow.tcl: opt: ${opt}"
	      if { ![info exists powImageParam(${opt}${current_image},$gn)] } {
		 set powImageParam(${opt}${current_image},$gn) \
		       $powImageParam(${opt},powDef)
#puts "pow.tcl: powImageParam(${opt},powDef): $powImageParam(${opt},powDef)"
	      }
	   }
	}

# make a copy of the current image 
	if $powPseudoImages {
	    image create pict ${current_image}disp$gn
	} else {
	    image create photo ${current_image}disp$gn
	}

#This if block allows rescalings to persist through ROI zooms
#you could get very bizarre behavior is somebodies reusing image names....
#nothing I can think of to do about that though
	if {![info exists powImageParam(RBmin${current_image},$gn)]} {
#puts "pow.tcl: RBmin not exists"
	    set powImageParam(RBmin${current_image},$gn) \
		  $powRBmin($current_image)
	    set powImageParam(RBmax${current_image},$gn) \
		  $powRBmax($current_image)
	}


	set clipbox [powGetImageClipbox $gn $current_image $canvas]
	set powPlotParam(clipbox$current_image,$gn) $clipbox

	if {![regexp "clipped" $clipbox]}  {

	    # First two elements indicate location on graph to place image
	    set x0 [lindex $clipbox 0]
	    set y0 [lindex $clipbox 1]
#puts "pow.tcl: BX0: $x0"
#puts "pow.tcl: BY0: $y0"
	    set pcoords [powGraphToCanvas $gn $x0 $y0 $canvas]
	    set x0 [lindex $pcoords 0]
	    set y0 [lindex $pcoords 1]
#puts "pow.tcl: AX0: $x0"
#puts "pow.tcl: AY0: $y0"

	    set image_id [$canvas create image $x0 $y0 \
			      -image ${current_image}disp$gn -anchor sw \
			      -tags "$gn disp$gn\
				     ${current_image}disp$gn image_body\
				     img_$current_image"]

	    if {$canvas == ".pow.pow" } {
	       powBindBtn <<LUT>> ".pow.pow bind $image_id" \
		     "powSelectImage $gn $current_image" \
		     "powBoundDiddleLut $gn $current_image %x %y" \
		     {}

	       # The following prevents the <<LUT>> binding from executing if
	       # the <<ROI>> binding is more appropriate due to modifiers
	       # Pan Chai - commented out to make sure RegionList create correctly
	       # powBindBtn <<ROI>> ".pow.pow bind $image_id" {} {} {}

	       if { ![info exists selImg] } {set selImg $current_image}
	    }
	}
	lappend powPlotParam(images,$gn) $current_image 

	powSetColorTable $gn $current_image
	powReditherImage $gn $current_image $canvas

    }
    if { [llength $powPlotParam(images,$gn)]==0 } {
       set powPlotParam(images,$gn) "NULL"
    }

    if { [info exists selImg] } {
       powSelectImage $gn $selImg
    }
#puts "pow.tcl: done"
}


proc powDeSelectImage { } {
#puts "powDeSelectImage start"
    global curr_img currimg powPlotParam
    if {[string compare [.pow.pow find  withtag current_img] ""]} {
	.pow.pow delete current_img
    }
    catch {unset curr_img}
    catch {unset currimg}
}

proc powSelectImage {gn img} {
#puts "powSelectImage start"
    global curr_img currimg powPlotParam powGUI currgn powPseudoImages
#puts "gn: $gn, currgn: $currgn"

    set powPlotParam(currimg,$gn) $img

    if { $gn != $currgn } return

#delete previous bbox rectangle
    .pow.pow delete current_img 
#make all things visible
    .pow.pow raise ${gn}line

    if { [info exists currimg] } {
       set prevImg $currimg
       set prevGn  $currgn
    } else {
       set prevImg ""
       set prevGn  ""
    }

    set currimg  $img
    set curr_img ${img}disp$gn

    set tags [.pow.pow find withtag disp$gn]
    if { $tags != "" } {
	.pow.pow raise $curr_img [lindex $tags end]
    }
    if $powGUI {
	set scopeids [.pow.scope find withtag disp${gn}scope]
	if {$scopeids != ""} {
	    .pow.scope raise img_$img [lindex $scopeids end]
	}
    }

    set ibbox [.pow.pow bbox ${img}disp$gn]
    if {$ibbox != ""} {
	eval [concat .pow.pow create rectangle  $ibbox \
	      -tags [list "current_img $gn handle ohandle"] -outline green]
    }

    if { $currgn != $prevGn || $currimg != $prevImg } {
       powSetColorTable $currgn $currimg
       powUpdateGraphMenuOptions
       [gNotifications default] postMessage $currimg imageHasBeenSelected
    }

}

proc powSelectGraph {gn} {
    global powDWP
    global currgn currimg mag powPlotParam
    global powGUI powScopeGn yellowLineWidth
    global g_magnification

#add a bit of slack around bbox
    foreach [list x0 y0 x1 y1] [.pow.pow bbox $gn] {}
            
            if { [llength [.pow.pow bbox $gn]] == 0 } return
    incr x0 -2
    incr y0 -2
    incr x1 2
    incr y1 2
    foreach [list ox0 oy0 ox1 oy1] [.pow.pow coord current_gn] {}
    
    if { ![info exists ox0] || \
	  [expr abs( $x0 - $ox0 ) + abs( $y0 - $oy0 ) + \
	  abs( $x1 - $ox1 ) + abs( $y1 - $oy1 ) ] > 1 } {
       #delete previous bbox rectangle
       .pow.pow delete current_gn
       .pow.pow create rectangle  $x0 $y0 $x1 $y1 \
	     -tags "current_gn graphDragable ${gn}yhandle handle ohandle" \
	     -outline yellow -width $yellowLineWidth
    }

# Rearrange graph layers and select current image if selecting new graph
    if { $currgn!=$gn } {

       [gNotifications default] postMessage $currgn graphHasBeenUnselected

       set currgn $gn
       if {[regexp "NULL" $powPlotParam(images,$gn)]} {
	 .pow.pow delete current_img
	 set powPlotParam(currimg,$gn) "NULL"
       }

       if $powGUI {
	  if {! [string compare [.pow.scope find withtag ${gn}scopebox] ""]} {
	     powRedrawScopebox
	  }
       }

       powUpdateCurrentDialogs

       if ![info exists powPlotParam(prev_magnification,$currgn)] {
          set powPlotParam(prev_magnification,$currgn) 1.0
          set powPlotParam(new_magnification,$currgn) 1.0
          set powPlotParam(g_multiplier,$currgn) 4.0
          set powPlotParam(g_magnification,$currgn) 1.0
       }

#puts "powSelectGraph powPlotParam(prev_magnification,$currgn): $powPlotParam(prev_magnification,$currgn)"
#puts "powSelectGraph powPlotParam(new_magnification,$currgn): $powPlotParam(new_magnification,$currgn)"
#puts "powSelectGraph powPlotParam(g_multiplier,$currgn): $powPlotParam(g_multiplier,$currgn)"
#puts "powSelectGraph powPlotParam(g_magnification,$currgn): $powPlotParam(g_magnification,$currgn)"

       set g_magnification $powPlotParam(g_magnification,$currgn)

    } elseif $powGUI {
       if {! [string compare [.pow.scope find withtag ${gn}scopebox] ""]} {
	  powRedrawScopebox
       }
    }

    # Now restore current image for this graph
    if { [info exists powPlotParam(currimg,$gn)] \
	  && $powPlotParam(currimg,$gn)!="NULL" } {
       powSelectImage $gn $powPlotParam(currimg,$gn)
    } elseif [info exists currimg] {
       unset currimg
    }
       
    .pow.pow raise ${gn}handles
    .pow.pow lower ${gn}bkg
    .pow.pow raise ${gn}text
    .pow.pow raise $gn
    .pow.pow lower graphSelect_$gn $gn
    .pow.pow raise current_gn

    powUpdateGraphMenuOptions

    if { [info exists powDWP] && [winfo exists ${powDWP}region]} {
       powSetupRegions $gn
       powUpdateRegionDlg $gn
    }

    [gNotifications default] postMessage $gn graphHasBeenSelected
}

proc powUpdateGraphMenuOptions {} {
#puts "powUpdateGraphMenuOptions start"
   global currgn currimg powGUI
   global powPlotParam powImageParam powMenuOption

   if $powGUI {

      set powMenuOption(tickScal) \
	 "$powPlotParam(xTickScal,$currgn)-$powPlotParam(yTickScal,$currgn)"

      .pow.mbar.edit.tlabels entryconfigure "Decimal" \
	    -variable powPlotParam(tickLabels,$currgn)
      .pow.mbar.edit.tlabels entryconfigure "Base 60 (deg)" \
	    -variable powPlotParam(tickLabels,$currgn)

      .pow.mbar.edit.grid entryconfigure "Show Grid Lines" \
	    -variable powPlotParam(GridLines,$currgn)

      foreach clr [list White Black Blue Red] {
	 .pow.mbar.edit.grid entryconfigure $clr \
	       -variable powPlotParam(GridColor,$currgn)
      }
      foreach opt [list Solid "Small Dash" "Large Dash"] {
	 .pow.mbar.edit.grid entryconfigure $opt \
	       -variable powPlotParam(GridDash,$currgn)
      }

      if { [info exists currimg] && $currimg != "" } {
	 set img $currimg
	 set gn $currgn
      } else {
	 set img ""
	 set gn powDef
      }

      foreach colorGrp $powImageParam(allMaps,powDef) {
	 set cName [lindex $colorGrp 0]
	 foreach color [lrange $colorGrp 1 end] {
	    .pow.mbar.colors.c$cName entryconfigure $color \
		  -variable powImageParam(colormap${img},$gn)
	 }
      }
      .pow.mbar.colors entryconfigure "Invert Colortable" \
	    -variable powImageParam(invert${img},$gn)
      .pow.mbar.colors entryconfigure linear \
	    -variable powImageParam(scale${img},$gn)
      .pow.mbar.colors entryconfigure "square root" \
	    -variable powImageParam(scale${img},$gn)
      .pow.mbar.colors entryconfigure logarithmic \
	    -variable powImageParam(scale${img},$gn)
      .pow.mbar.colors entryconfigure "Histo Equalize" \
	    -variable powImageParam(scale${img},$gn)
   }
#puts "powUpdateGraphMenuOptions end"
}

proc powUpdateCurrentDialogs { } {
#puts "powUpdateCurrentDialogs start"
   global currgn powDWP

   if { [winfo exists ${powDWP}gEdit] } {
      powEditResetDialog
   }

}


proc powRedrawScopebox {  } {
#puts "powRedrawScopebox start"
   global currgn powPlotParam powScopeWidth powScopeHeight powScopeMargin
   global currimg powScopeGn powOrderedGraphList

   .pow.scope delete all

   # Make sure the scope's Title is up-to-date

   set title $powPlotParam(titleString,$currgn)
   if { $currgn=="powDef" } {
      set powScopeGn "-"
   } elseif { [string length $title] > 24 } {
      set ll [expr [string length $title]-10]
      set powScopeGn "[string range $title 0 11]...[string range $title $ll end]"
   } elseif { $title=="" } {
      set idx [expr [lsearch $powOrderedGraphList $currgn]+1]
      set powScopeGn "Untitled $idx"
   } else {
      set powScopeGn $title
   }
       
   # Do we need to go any further?  Any curves/images in graph?

   if {[regexp "NULL" $powPlotParam(curves,$currgn)] && \
	 [regexp "NULL" $powPlotParam(images,$currgn)] } return
   set width  [expr $powScopeWidth  - 2*$powScopeMargin]
   set height [expr $powScopeHeight - 2*$powScopeMargin]
   set width  [expr ($width < 10 ? 10 : $width )]
   set height [expr ($height< 10 ? 10 : $height)]
   powCreateGraph ${currgn}scope $powPlotParam(curves,$currgn) \
	 $powPlotParam(images,$currgn) $powPlotParam(xunits,$currgn) \
	 $powPlotParam(yunits,$currgn) $powPlotParam(xlabel,$currgn) \
	 $powPlotParam(ylabel,$currgn) \
	 $width $height \
	 NULL NULL NULL NULL .pow.scope

   # Raise current image
   if { [info exists currimg] && $currimg != "" } {
      set scopeids [.pow.scope find withtag disp${currgn}scope]
      if {$scopeids != ""} {
	 .pow.scope raise img_$currimg [lindex $scopeids end]
      }
   }

   powDrawScopeROI [list \
	 $powPlotParam(xBot,$currgn) \
	 $powPlotParam(yBot,$currgn) \
	 $powPlotParam(xTop,$currgn) \
	 $powPlotParam(yTop,$currgn)]

}

proc powResizeScope { width height } {
#puts "powResizeScope start"
   global powScopeWidth powScopeHeight powScopeMargin currgn
   global powShowScope powScopeSize
   global menuBarDeleteFlag

   # Resize Scopebox window

   set powScopeSize [list $width $height]
   if { $width && $height } {
      set powShowScope   1
      set powScopeWidth  $width
      set powScopeHeight $height
      set powScopeMargin [expr ($width+$height)/20]
      .pow.scope configure -width $width -height $height
      powLayoutGUI
      powRedrawScopebox
   } else {
      # set powShowScope 0
      grid remove .pow.scopeframe
      grid remove .pow.trackers
      grid remove .pow.gui
      powDeleteMenuBarItem
      .pow.scope configure -width 1 -height 1
   }

   powUpdateGeometry
}

proc powUpdateGeometry {} {
#puts "powUpdateGeometry start"
   global powRealMinWidth powRealMinHeight powResizeMain

   # Update window geometry

   powSetGeometry

   set resize 0
   foreach {dx dy} [lrange [split [wm geometry .pow] {x+-}] 0 1] {}
   if { $powRealMinWidth>$dx } {
      set dx $powRealMinWidth
      set resize 1
   }
   if { $powRealMinHeight>$dy } {
      set dy $powRealMinHeight
      set resize 1
   }
   if { $resize } {
#      set x [winfo x .pow]
#      set y [winfo y .pow]
      wm geometry .pow "${dx}x${dy}"
   }

   powReconfigureToplevel $powResizeMain
}

proc powGetCurrentGraph { } {
#puts "powGetCurrentGraph start"
   global currgn

   if [info exist currgn] {
      if { $currgn=="powDef" } { return "" }
      return $currgn
   } else {
      return ""
   }
}

proc powMagImage {gn img {canvas .pow.pow}} {
#puts "powMagImage start"
#lowlevel routine, don't call this yourself
#resizes image to match the current magstep and ROI window
    global powPlotParam curr_img currimg
    global powPseudoImages powImageParam isMac
    global menuBarDeleteFlag
    
    set clipbox $powPlotParam(clipbox${img},$gn)
    if [regexp "clipped" $clipbox] return
    
    catch {image delete ${img}disp$gn}
    if $powPseudoImages {
	image create pict ${img}disp$gn
    } else {
	image create photo ${img}disp$gn
    }
    
    set width  [image width $img]
    set height [image height $img]

    #collect up the inputs to ship to Tk_(Pict||Photo)PutScaledBlock
    set x0 [lindex $clipbox 2]
    set y0 [lindex $clipbox 3]
    set x1 [lindex $clipbox 4]
    set y1 [lindex $clipbox 5]
    foreach {X0 Y0} [powPixelToCanvas $gn $img -0.5 -0.5 $canvas] {}
    foreach {X1 Y1} [powPixelToCanvas $gn $img \
	    [expr $width-0.5] [expr $height-0.5] $canvas] {}
    
    set zoomX [expr ($X1-$X0)/$width]
    set zoomY [expr ($Y0-$Y1)/$height]
		     
    set width  [expr int( ($x1 - $x0)*$zoomX + 0.5 )]
    set height [expr int( ($y1 - $y0)*$zoomY + 0.5 )]

    if { $isMac && ![powTestMacMemory [expr $width*$height]] } {
	tk_messageBox -type ok -icon error \
		-message "Not enough memory to display $img.  Will hide it\
		until memory becomes available."
    } else {
	powPutZoomedBlock $img $gn $x0 $y0 $width $height $zoomX $zoomY
    }
    
#    set curr_img ${img}disp$gn

    if { $powPseudoImages } {
       powSetRange $gn $img \
	     $powImageParam(RBmin${img},$gn) $powImageParam(RBmax${img},$gn)
    }
}

proc powSetCurveOptions {gn curve {args ""}} {
   global powCurveParam powGUI currgn
   global powWCS powFitsHeader powFitsHeaderCnt xCount yCount powPlotParam

   if { $args == "" } {
      set lst ""
      foreach opt $powCurveParam(allOpts,powDef) {
	 catch {lappend lst $opt $powCurveParam(${opt}${curve},$gn)}
      }
      return $lst
   } elseif { [llength $args] == 1 } {
      set opt [lindex $args 0]
      set idx [lsearch -exact $powCurveParam(allOpts,powDef) $opt]
      if { $idx != -1 } {
	 if { $opt=="pColor" || $opt=="lColor" } {
	    set val [powColorToHex $val]
	 }
	 return $powCurveParam(${opt}${curve},$gn)
      } else {
	 return ""
      }
   } else {
      foreach {opt val} $args {
	 set idx [lsearch -exact $powCurveParam(allOpts,powDef) $opt]
	 if { $idx != -1 } {
	    if { $opt=="pColor" || $opt=="lColor" } {
	       set val [powColorToHex $val]
	    }
	    set powCurveParam(${opt}${curve},$gn) $val
	 }
      }
      if { [.pow.pow find withtag ${curve}${gn}] != "" } {
	 .pow.pow delete ${curve}${gn}
	 powPlot1Curve $gn $curve .pow.pow
	  if {$powGUI && ($gn == $currgn)}  {
	    .pow.scope delete ${curve}${gn}scope
	    powPlot1Curve ${gn}scope $curve .pow.scope
	 }
      }
   }
}

proc powAddCurves {gn curves} {
#puts "powAddCurves start"
   global powPlotParam currgn powGUI

   powPlotCurves $gn $curves .pow.pow
   if { $powGUI && $gn == $currgn } {
      powRedrawScopebox
   }
}

proc powAddImages {gn images} {
#puts "powAddImages start"
   global powPlotParam currgn powGUI

   powPlotImages $gn $images .pow.pow
   if { $powGUI && $gn == $currgn } {
      powRedrawScopebox
   }
}

proc powRemoveCurves {gn curves} {
#puts "powRemoveCurves start"
   global powPlotParam currgn powGUI

   set hasChanged 0
   foreach c $curves {
      set idx [lsearch -exact $powPlotParam(curves,$gn) $c]
      if { $c != "NULL" && $idx != -1 } {
	 set hasChanged 1
	 .pow.pow delete $c$gn
	 set powPlotParam(curves,$gn) \
	       [lreplace $powPlotParam(curves,$gn) $idx $idx]
      }
   }
   if { [llength $powPlotParam(curves,$gn)]==0 } {
      set powPlotParam(curves,$gn) "NULL"
   }

   if { $powGUI && $gn == $currgn && $hasChanged } {
      powRedrawScopebox
   }
}

proc powRemoveImages {gn images} {
#puts "powRemoveImages start"
   global powPlotParam currgn powGUI

   set hasChanged 0
   foreach i $images {
      set idx [lsearch -exact $powPlotParam(images,$gn) $i]
      if { $i != "NULL" && $idx != -1 } {
	 set hasChanged 1
	 .pow.pow delete ${i}disp$gn
	 set powPlotParam(images,$gn) \
	       [lreplace $powPlotParam(images,$gn) $idx $idx]
      }
   }
   if { [llength $powPlotParam(images,$gn)]==0 } {
      set powPlotParam(images,$gn) "NULL"
   }

   if { $powGUI && $gn == $currgn && $hasChanged } {
      powRedrawScopebox
   }
}

proc powGetCurveLength { crv } {
#puts "powGetCurveLength start"
   array set crvInfo [powFetchCurveInfoHash $crv]
   array set vecInfo [powFetchVectorInfoHash $crvInfo(X)]
   return [powFetchDataLength $vecInfo(data)]
}

proc powPlot1Curve {gn crv {canvas .pow.pow}} {
   global powCurveParam powScopeWidth powScopeHeight
   global powPlotParam
   global xCount yCount

   if { $canvas == ".pow.scope" } {
      set trueGn [string range $gn 0 [expr [string length $gn]-6] ]
      foreach opt $powCurveParam(allOpts,powDef) {
	 set powCurveParam(${opt}${crv},$gn) \
	       $powCurveParam(${opt}${crv},$trueGn)
      }
   } else {
      foreach opt $powCurveParam(allOpts,powDef) {
	 if {! [info exists powCurveParam(${opt}${crv},$gn)]} {
	    if { $opt == "pShape" && [powGetCurveLength $crv]>10000 \
		  && ($powCurveParam(LOD,powDef) == 0 || \
		  $powCurveParam(LOD,powDef)  > 10000) } {
	       set powCurveParam(${opt}${crv},$gn) Dot
	    } elseif { [info exists powCurveParam(${opt}${crv},powDef)] } {
	       set powCurveParam(${opt}${crv},$gn) \
		     $powCurveParam(${opt}${crv},powDef)
	    } else {
	       set powCurveParam(${opt}${crv},$gn) \
		     $powCurveParam(${opt},powDef)
	    }
	 }
      }
   }
   
   foreach opt $powCurveParam(allOpts,powDef) {
      set $opt $powCurveParam(${opt}${crv},$gn)
   }
   if { $logX || $logY } {
      if { [powWCSexists $gn] } {
	 # Cannot mix WCS and log, so change options to No
	 set powCurveParam(logX${crv},$gn) No
	 set powCurveParam(logY${crv},$gn) No
	 set logX No
	 set logY No
      }
   }
   
   if { $canvas == ".pow.scope" } {
      # Shrink point size if drawing in the scope window
      if { $pSize>0 } {
	 set pSize [expr round($pSize*($powScopeWidth+$powScopeHeight)/800.0)]
	 if { $pSize<=1 } {
	    set pShape Dot
	 }
      }
   }

   #   call curve plotting routine
   $canvas create powCurve $crv $gn \
	 -pointdisplay $pDisp \
	 -pointtype $pShape \
	 -pointsize $pSize \
	 -pointerror $pSizeErr \
	 -pointfill $pFill \
	 -linedisplay $lDisp \
	 -dash $lStyle \
	 -width $lWidth \
	 -stairstep $lStep \
	 -boxfill $lBoxFill \
	 -lfill $lColor \
	 -pfill $pColor \
	 -logx $logX \
	 -logy $logY \
	 -tags "$gn $crv$gn" \
	 -LOD $LOD
}

proc powPlotCurves {gn curves {canvas .pow.pow}} {
    global powPlotParam powCurveParam powcursor powResizeMain

    if [regexp "NULL" $curves] return

#remove "NULL" from curves list if present
    if [regexp "NULL" $powPlotParam(curves,$gn)] {set powPlotParam(curves,$gn) { }}

    set crvCnt [llength $powPlotParam(curves,$gn)]
    foreach current_curve $curves {

#   if curve is already in list, don't plot it
	if [regexp $current_curve $powPlotParam(curves,$gn)] continue

#   Check if we need to assign a new color to this curve
	if { ![info exists powCurveParam(lColor${current_curve},$gn)] && \
	      ![info exists powCurveParam(lColor${current_curve},powDef)] } {
	   set colors $powCurveParam(allColors,powDef)
	   set nElem [lsearch $colors $powCurveParam(lColor,powDef)]
	   # Must increment by 2* because list contains COLOR #HEX COLOR #HEX
	   incr nElem [expr $crvCnt+$crvCnt]
	   if { $nElem<0 } {
	      set powCurveParam(lColor${current_curve},$gn) \
		    $powCurveParam(lColor,powDef)
	   } else {
	      while { $nElem >= [llength $colors] } {
		 incr nElem -[llength $colors]
	      }
	      set powCurveParam(lColor${current_curve},$gn) \
		    [lindex $colors $nElem]
	   }
	}
	if { ![info exists powCurveParam(pColor${current_curve},$gn)] && \
	      ![info exists powCurveParam(pColor${current_curve},powDef)] } {
	   set colors $powCurveParam(allColors,powDef)
	   set nElem [lsearch $colors $powCurveParam(pColor,powDef)]
	   # Must increment by 2* because list contains COLOR #HEX COLOR #HEX
	   incr nElem [expr $crvCnt+$crvCnt]
	   if { $nElem<0 } {
	      set powCurveParam(pColor${current_curve},$gn) \
		    $powCurveParam(pColor,powDef)
	   } else {
	      while { $nElem >= [llength $colors] } {
		 incr nElem -[llength $colors]
	      }
	      set powCurveParam(pColor${current_curve},$gn) \
		    [lindex $colors $nElem]
	   }
	}
	       
	if { [catch {powPlot1Curve $gn $current_curve $canvas} err] } {
	   tk_messageBox -icon error -type ok -parent .pow \
		 -message "Couldn't place $current_curve into graph...\
		 \n\n\"$err\"\n\nSkipping curve."
	} else {
#        add name to list of curves
	   lappend powPlotParam(curves,$gn) $current_curve
	   incr crvCnt
	}
    }
}

proc powWhereAmI {x y {canvas ".pow.pow"}} {
    set boxes [$canvas find withtag gbox]
    set topbox -1
    set topgraph ""
    foreach graph [powListGraphs] {
	set gbox [$canvas coords ${graph}box]
	if { $gbox == "" } continue
	if { $x >= [lindex $gbox 0] && $x <= [lindex $gbox 2] \
		&& $y >= [lindex $gbox 1]  && $y <= [lindex $gbox 3] } {
	    set order [lsearch $boxes [$canvas find withtag ${graph}box]]
	    if {$order>$topbox} {set topbox $order; set topgraph $graph }
	}
    }
    if {$topbox>=0} {return $topgraph} else {return "NULL"}
}


proc powWhereAmI_img {gn x y {canvas ".pow.pow"}} {
   set images [$canvas find withtag disp$gn]
   set N [llength $images]
   if { $N==0 } { return "NULL" }

   for { set i $N } { $i>0 } {  } {
      incr i -1
      set img [lindex $images $i]
      set ibox [$canvas bbox $img]
      if { $ibox == "" } continue
      if { $x >= [lindex $ibox 0] && $x <= [lindex $ibox 2] \
	    && $y >= [lindex $ibox 1]  && $y <= [lindex $ibox 3] } {
	 set tags [$canvas gettags $img]
#puts "tags: $tags"
	 set elem [lsearch -glob $tags ?*disp$gn]
#puts "elem: $elem"

         set check [split $gn "()"]
#puts "check: $check"

         if { [llength $check] > 1 } {
            # Pan Chai: check patten needed to update if the gn name is changed of table image
            # we got "()" special character in the gn name and only has one set
            if { [regexp "^(.+)disp[lindex $check 0](\\()[lindex $check 1](\\))[lindex $check 2]$" [lindex $tags $elem] dmy theImage] } {
	       return $theImage
            }
           
         } else {
	    if { [regexp "^(.+)disp$gn$" [lindex $tags $elem] dmy theImage] } {
	       return $theImage
            }
	 }
      }
   }
   return "NULL"
}


#  C  routines: CanvasToGraph, GraphToPixel
#               PixelToGraph, GraphToCanvas
# TCL routines: CanvasToPixel, PixelToCanvas

proc powPixelToCanvas {gn img x y {canvas .pow.pow}} {
#puts "powPixelToCanvas start"

    set ccoords [powPixelToGraph $img $x $y]
    set rx [lindex $ccoords 0]
    set ry [lindex $ccoords 1]
    set ccoords [powGraphToCanvas $gn $rx $ry $canvas]

#puts "PixelToCanvas - $x $y $ccoords $gn"
    return $ccoords 
}

proc powCanvasToPixel {gn img x y {canvas .pow.pow}} {
#puts "powCanvasToPixel start"
    global powPlotParam

    set ccoords [powCanvasToGraph $gn $x $y $canvas]
#puts "powCanvasToPixel: ccoords: $ccoords"
    set rx [lindex $ccoords 0]
    set ry [lindex $ccoords 1]
    set ccoords [powGraphToPixel $img $rx $ry]

#puts "ccoords - $x $y $ccoords"
    return $ccoords 
}

proc set_tracker_info {x y {canvas ".pow.pow"}} {
    global powPlotParam powTrackText currimg currgn
    global powPseudoImages powEditObject
    global xCount yCount

    set cx [$canvas canvasx $x]
    set cy [$canvas canvasy $y]
    set gn [powWhereAmI $cx $cy $canvas]
    set powTrackText(gn) $gn
    if { $gn != "NULL" } {
       set gcoords [powCanvasToGraph $gn $cx $cy $canvas]
       set powTrackText(rx) [lindex $gcoords 0]
       set powTrackText(ry) [lindex $gcoords 1]

       set img [powWhereAmI_img $gn $cx $cy $canvas]
       set powTrackText(img) $img
       if { $img != "NULL" } {
	  set icoords [powCanvasToPixel $gn $img $cx $cy $canvas]
	  set imgx [expr int([lindex $icoords 0]+0.5)]
	  set imgy [expr int([lindex $icoords 1]+0.5)]

	  set width  [image width  $img]
	  set height [image height $img]
	
#puts "imgx: $imgx, imgy: $imgy, width: $width, height: $height"

	  if { ($imgx < $width) && ($imgy < $height) \
		&& ($imgx >= 0) && ($imgy >= 0) } {
	     set powTrackText(imgx) $imgx
	     set powTrackText(imgy) $imgy
             if [info exist powPlotParam(flipD,$gn)] {
                switch $powPlotParam(flipD,$gn) {
                   "X" {
                      if { [info exists xCount($gn)] && [expr $xCount($gn) % 2] != 0 } {
                         set powTrackText(imgx) [expr $width - $powTrackText(imgx) - 1]
                      }
                   }
                   "Y" {
                      if { [info exists yCount($gn)] && [expr $yCount($gn) % 2] != 0 } {
                         set powTrackText(imgy) [expr $height - $powTrackText(imgy) - 1]
                      }
                   }
                   "B" {
                      if { [info exists xCount($gn)] && [expr $xCount($gn) % 2] != 0 } {
                         set powTrackText(imgx) [expr $width - $powTrackText(imgx) - 1]
                      }
                      if { [info exists yCount($gn)] && [expr $yCount($gn) % 2] != 0 } {
                         set powTrackText(imgy) [expr $height - $powTrackText(imgy) - 1]
                      }
                   }
                }
             }

	     set powTrackText(imgz) [powGetImageZ $img $imgx $imgy]
	  } else {
	     set powTrackText(imgx) "X"
	     set powTrackText(imgy) "X"
	     set powTrackText(imgz) "X"
	  }
	  set powTrackText(zunits) [powGetImageUnits $img Z]
       } else {
	  set powTrackText(imgx) "X"
	  set powTrackText(imgy) "X"
	  set powTrackText(imgz) "X"
	  set powTrackText(zunits) " "
       }

    } else {

       set powTrackText(rx) X
       set powTrackText(ry) X
       set powTrackText(imgx) "X"
       set powTrackText(imgy) "X"
       set powTrackText(imgz) "X"
       set powTrackText(zunits) " "

    }
    powUpdateTrackVars
}


proc powStretchGraph {gn xfactor yfactor {canvas ".pow.pow"}} {
#puts "powStretchGraph start, xfactor: $xfactor, yfactor: $yfactor"
    global powPlotParam

    powResizeGraph $gn $xfactor $yfactor $canvas
}


proc powMagGraph {gn newxmagstep newymagstep {canvas ".pow.pow"}} {
#puts "powMagGraph start"
    global powPlotParam

    set xfactor \
	    [expr double($newxmagstep) / double($powPlotParam(xmagstep,$gn))]
    set yfactor \
	    [expr double($newymagstep) / double($powPlotParam(ymagstep,$gn))]
    powResizeGraph $gn $xfactor $yfactor $canvas
}

proc powResizeGraph {gn xfactor yfactor {canvas ".pow.pow"}} {
#puts "powResizeGraph start"
#lowlevel routine, don't call this yourself.  Use powMagGraph or powStretchGraph
#all "resizings" of a graph are done here.  Don't you dare
#do them elsewhere or you'll regret it.
    global powPlotParam powcursor powResizeMain
    global currimg powScopeMargin
    global baseX baseY
    global xFactor yFactor

    foreach el  [array names powPlotParam]  {
	set p1 [lindex [split $el ,] 0]
	set p2 [lindex [split $el ,] 1]
	if { $p2 == $gn } {
	    set $p1 $powPlotParam($p1,$p2)
	}
    }
    
    
    if { [$canvas find withtag ${gn}box] == "" } return

    set bbox [$canvas coords ${gn}box]

#    save initial coordinates

    if {$canvas == ".pow.scope"} {
	set ul [list $powScopeMargin $powScopeMargin]
    } else {
	set ul [list [lindex $bbox 0] [lindex $bbox 1]]
    }

    $canvas scale $gn [lindex $bbox 0] [lindex $bbox 1] $xfactor $yfactor

    set fbox [$canvas coords ${gn}box]

    if { ![info exists baseX] } {
       set baseX [expr [lindex $bbox 2] - [lindex $bbox 0]]
       set baseY [expr [lindex $bbox 3] - [lindex $bbox 1]]
    }

    set xFactor [expr [expr [lindex $fbox 2] - [lindex $fbox 0]] / $baseX]
    set yFactor [expr [expr [lindex $fbox 3] - [lindex $fbox 1]] / $baseY]

    set powPlotParam(xmagstep,$gn) [expr $xfactor * $powPlotParam(xmagstep,$gn)]
    set powPlotParam(ymagstep,$gn) [expr $yfactor * $powPlotParam(ymagstep,$gn)]

    powSetGraphMagstep $gn $powPlotParam(xmagstep,$gn) \
	    $powPlotParam(ymagstep,$gn) 
    
    if {![regexp "NULL" $images]} {
	foreach img $powPlotParam(images,$gn) {
	    powMagImage $gn $img $canvas
	}
    }
    
    
    if {$canvas == ".pow.pow"} {
       # Redraw all the adornments

	.pow.pow delete ${gn}handles
	.pow.pow delete ${gn}shandle
	.pow.pow delete ${gn}yhandle
	
	#draw new tick marks and numbers to go with and new labels
	powDrawTicks $gn $canvas
	
	#make new GraphHandles
	powMakeGraphLabels $gn
	powMakeGraphHandles $gn
	powSelectGraph $gn
    }

    [gNotifications default] postMessage $gn graphHasResized
}



proc powRestoreGraph {gn {canvas .pow.pow}} {
#puts "powRestoreGraph start"
    set bbox [$canvas bbox $gn]
    set x [lindex $bbox 0]
    set y [lindex $bbox 1]
    set mx [expr ($x < 20) ? 20 - $x : 0]
    set my [expr ($y < 20) ? 20 - $y : 0]
    powMoveGraph $gn $mx $my $canvas
}
    


proc tagXdim {can tag} {
#puts "tagXdim start"
    set bbox [$can coords $tag]
    return [expr [lindex $bbox 2] - [lindex $bbox 0]]
}

proc tagYdim {can tag} {
#puts "tagYdim start"
    set bbox [$can coords $tag]
    return [expr [lindex $bbox 3] - [lindex $bbox 1]]
}



proc powStretchGraphToSize {gn xdim ydim {canvas ".pow.pow"}} {
#puts "powStretchGraphToSize start, xdim: $xdim, ydim: $ydim"
#stretches/shrinks graph to fit in xdim/ydim size
    global  powPlotParam powEditPlotParam 

    set curr_xdim [tagXdim $canvas ${gn}box]
    set curr_ydim [tagYdim $canvas ${gn}box]

    set xfactor [expr double($xdim)/double($curr_xdim)]
    set yfactor [expr double($ydim)/double($curr_ydim)]
	
    powStretchGraph $gn $xfactor $yfactor $canvas
    
    #save requested current size of graph
    if ![info exists powEditPlotParam(xdimdisp,powDef)] {
       set powEditPlotParam(xdimdisp,powDef) $powPlotParam(xdimdisp,$gn)
       set powEditPlotParam(ydimdisp,powDef) $powPlotParam(ydimdisp,$gn)
    }

    set powPlotParam(xdimdisp,$gn) $xdim
    set powPlotParam(ydimdisp,$gn) $ydim

    set powEditPlotParam(xdimdisp,new) $xdim
    set powEditPlotParam(ydimdisp,new) $ydim
    
    if {$canvas == ".pow.pow"} {
	powSelectGraph $gn
    }

}


proc powDragGraph { stage X Y } {
#puts "powDragGraph start"
   global powMoveX powMoveY powIsDragging
   global currgn powResizeMain

   switch -exact $stage {
      start {
	 set powMoveX $X
	 set powMoveY $Y
	 set powIsDragging 1
      }
      drag {
	 powHideCurves $currgn
	 powMoveHandle $currgn $X $Y
      }
      end {
	 powShowCurves $currgn
	 powReconfigureToplevel $powResizeMain
	 set powIsDragging 0
      }
   }
}

#Plotting routines below here ...
proc powMoveGraph {gn xDist yDist {canvas ".pow.pow"}} {
#puts "powMoveGraph start"
    global powPlotParam 

    if { $xDist==0 && $yDist==0 } return
    $canvas move $gn $xDist $yDist
    $canvas move ${gn}handles $xDist $yDist
    $canvas move ${gn}yhandle $xDist $yDist
    incr powPlotParam(xo,$gn) $xDist
    incr powPlotParam(yo,$gn) $yDist

    [gNotifications default] postMessage $gn graphHasMoved $xDist $yDist
}

proc powMoveGraphTo {gn x y {canvas ".pow.pow"}} {
#puts "powMoveGraphTo start"
    global powPlotParam 
    set bbox [$canvas coords ${gn}box]
    set xDist [expr $x - [lindex $bbox 0]]
    set yDist [expr $y - [lindex $bbox 1]]

    $canvas move $gn $xDist $yDist
    $canvas move ${gn}handles $xDist $yDist
    $canvas move ${gn}yhandle $xDist $yDist
    set powPlotParam(xo,$gn) $x
    set powPlotParam(yo,$gn) $y
    # powRestoreGraph $gn $canvas
    
    [gNotifications default] postMessage $gn graphHasMoved $xDist $yDist
}


proc powRedrawGraphHandles {gn} {
#puts "powRedrawGraphHandles start"
   global currgn
   if { [.pow.pow find withtag ${gn}handles] != "" } {
      .pow.pow delete ${gn}handles
      .pow.pow delete ${gn}shandle
      powMakeGraphHandles $gn
      #  If this is current graph, call SelectGraph to update yellow box
      if { $gn == $currgn } {powSelectGraph $gn}
   }
}

proc powMakeGraphHandles {gn} {
#puts "powMakeGraphHandles start"
    global powPlotParam env
    
#    update idletasks
    set bbox [.pow.pow bbox $gn]
    set left [lindex $bbox 0]
    set top [lindex $bbox 1]
    set right [lindex $bbox 2 ]
    set bot [lindex $bbox 3]

#Make "handle" for graph,  you can pick up the graph and 
#move it around by dragging this
    if [string match "*t*" $powPlotParam(handleposition,$gn)] {
	set y $top
    } elseif [string match "*b*" $powPlotParam(handleposition,$gn)] {
	set y $bot
    } else {
	set y [expr ($top + $bot)/2.0]
    }

    if [string match "*l*" $powPlotParam(handleposition,$gn)] {
	set x $left
    } elseif [string match "*r*" $powPlotParam(handleposition,$gn)] {
	set x $right
    } else {
	set x [expr ($left + $right)/2.0]
    }

# .pow.ms${gn}handle - the 'ms' stands for Move/select and is necessary
# to allow graph names to start with a capital 

    set msName "ms[powCleanName $gn]handle"
    if [winfo exists .pow.$msName] {
	.pow.$msName configure -bg $powPlotParam(bgcolor,$gn) \
	    -text $powPlotParam(handletext,$gn) -cursor fleur 
    } else {
	button .pow.$msName \
		-bg $powPlotParam(bgcolor,$gn) \
		-text $powPlotParam(handletext,$gn) -cursor fleur 
	bind .pow.$msName <ButtonPress-1> \
		"set powMoveX %X ; set powMoveY %Y"
	bind .pow.$msName <B1-Motion> \
		"powHideCurves $gn ; powMoveHandle $gn %X %Y"
	bind .pow.$msName <ButtonRelease-1> \
	    "+powShowCurves $gn; powSelectGraph $gn; \
	     powReconfigureToplevel \$powResizeMain"

    }

    raise .pow.$msName .pow.pow

#    .pow.pow create window $x $y \
#	-tags "${gn}handle handle ghandle ${gn}handles canvas_window" \
#        -anchor $powPlotParam(handleanchor,$gn) \
#	-window .pow.$msName



#Make "stretch handle" for graph,  you will be able to expand the graph
#by dragging this.

    set sName "s[powCleanName $gn]handle"
    if [winfo exists .pow.$sName] {
	.pow.$sName configure -bg $powPlotParam(bgcolor,$gn) \
	    -bitmap stretcharrow\
	    -cursor bottom_right_corner
    } else {
	button .pow.$sName -bg $powPlotParam(bgcolor,$gn) \
	    -bitmap stretcharrow \
	    -cursor bottom_right_corner
	bind .pow.$sName <B1-Motion> \
	      "powHideCurves $gn; powStretch $gn %X %Y"
	bind .pow.$sName <ButtonPress-1> \
	    "powBeginStretch $gn %X %Y; \
	     set fixedStretch \"yes\"; \
	     powStretch $gn %X %Y; "
	bind .pow.$sName <Shift-ButtonPress-1> \
	    "powBeginStretch $gn %X %Y; \
	     set fixedStretch \"no\"; \
	     powStretch $gn %X %Y; "
	bind .pow.$sName <ButtonRelease-1> \
	    "powShowCurves $gn; powEndStretch $gn"
    }


    .pow.pow create window $right $bot\
	-tags " ${gn}shandle shandle handle ${gn}handles canvas_window" \
	-anchor se -window .pow.$sName
   

    raise .pow.$sName .pow.pow


#Make a colored background
    .pow.pow create rectangle $bbox \
	-tags "${gn}handles ${gn}bkg graphbkg" \
	-fill $powPlotParam(bgcolor,$gn) -outline $powPlotParam(bgcolor,$gn)
    .pow.pow lower ${gn}bkg

# Now create an underlying polygon with -fill {} to catch all clicks
       
    .pow.pow delete graphSelect_$gn
    foreach {x0 y0 x1 y1} $bbox {}
    .pow.pow create polygon $x0 $y0 $x0 $y1 $x1 $y1 $x1 $y0 $x0 $y0 \
	  -outline {} -fill {} -tags "${gn}handles graphSelect_$gn"
    .pow.pow lower graphSelect_$gn $gn

#Store position of "Select" handle relative to graph box
#    update idletasks
    set hcoords [.pow.pow coords ${gn}handle]
    set bcoords [.pow.pow coords ${gn}box]
    set powPlotParam(handleoffsetx,$gn) \
	[expr [lindex $hcoords 0] - [lindex $bcoords 0]]
    set powPlotParam(handleoffsety,$gn) \
	[expr [lindex $hcoords 1] - [lindex $bcoords 1]]
}

proc powCleanName {gn} {
#puts "powCleanName start"
    regsub -all {\.} $gn {_} a
    return $a
}


proc powHideCurves { gn } {
#puts "powHideCurves start"
    global powPlotParam

    foreach crv $powPlotParam(curves,$gn) {
	if {$crv=="NULL"} continue
#if the curve has string Z data, the next statement will fail
#since the string Z data is implemented as a separate
#canvas text item with the same tag and canvas text items don't
#have the -hidden option... so catch it
	catch {.pow.pow itemconfig ${crv}${gn} -hidden 1}
    }
}

proc powShowCurves { gn } {
#puts "powShowCurves start"
    global powPlotParam

    foreach crv $powPlotParam(curves,$gn) {
	if {$crv=="NULL"} continue
#if the curve has string Z data, the next statement will fail
#since the string Z data is implemented as a separate
#canvas text item with the same tag and canvas text items don't
#have the -hidden option... so catch it
	catch {.pow.pow itemconfig ${crv}${gn} -hidden 0}
    }
}

proc powMoveHandle {gn x y} {
#puts "powMoveHandle start"
    global powMoveX powMoveY powPlotParam

    # Calculate root bounding box of .pow.pow canvas (- a little)

    set left  [expr [winfo rootx  .pow.pow]         + 10]
    set top   [expr [winfo rooty  .pow.pow]         + 10]
    set right [expr [winfo width  .pow.pow] + $left - 20]
    set bott  [expr [winfo height .pow.pow] + $top  - 20]
    
    # Check whether we have moved outside of the .pow.pow canvas

    if { $x < $left } {
	set x $left
    } elseif { $x > $right } {
	set x $right
    }

    if { $y < $top } {
	set y $top
    } elseif { $y > $bott } {
	set y $bott
    }

    set dx [expr $x - $powMoveX]
    set dy [expr $y - $powMoveY]

    powMoveGraph $gn [expr $x - $powMoveX] [expr $y - $powMoveY]

    set powMoveX $x 
    set powMoveY $y
}


proc powFindOverlapGraph { Lft Top Rgt Bot } {
#puts "powFindOverlapGraph start"
   set gn ""
   foreach gnIdx [.pow.pow find withtag gbox] {
      foreach {lft top rgt bot} [.pow.pow bbox $gnIdx] {}
      if { $rgt<$Lft || $lft>$Rgt || $top>$Bot || $bot<$Top } continue
      set gn [lindex [.pow.pow gettags $gnIdx] 0]
   }
   return $gn
}


proc powBeginROI {x y {canvas .pow.pow}} {
    global roi_xo roi_yo saveROI
    global roi_xn roi_yn 
    global currgn currimg
    global roi_pixelxo roi_pixelyo 

    if {[$canvas find withtag ROI] != ""} {
	set saveROI [$canvas coords ROI]
    }

    set roi_xo [$canvas canvasx $x]
    set roi_yo [$canvas canvasy $y]
    set roi_xn $roi_xo
    set roi_yn $roi_yo
    if [info exists currimg] {
       set result [powCanvasToPixel $currgn $currimg $roi_xo $roi_yo ".pow.pow"]
       set roi_pixelxo [lindex $result 0]
       set roi_pixelyo [lindex $result 1]
    }
    $canvas create rectangle $x $y $x $y -tags ROI -outline blue
}

proc powDragROI {x y {canvas .pow.pow}} {
#puts "powDragROI start"
    global roi_xo roi_yo
    global roi_xn roi_yn
    $canvas delete ROI
      
    set roi_xn [$canvas canvasx $x]
    set roi_yn [$canvas canvasy $y]
    if {![info exists roi_xo] || ![info exists roi_yo] || ($roi_xo == $roi_xn && $roi_yo == $roi_yn)} {
       return
    }
    $canvas create rectangle $roi_xo $roi_yo \
	[$canvas canvasx $x] [$canvas canvasy $y] \
	-tags ROI -outline blue
}

proc powPanROI {x y {canvas .pow.pow}} {
#puts "powPanROI start"
    set ROIbbox [$canvas coords ROI]
    set halfwidth [expr ([lindex $ROIbbox 2] - [lindex $ROIbbox 0])/2.0]
    set halfheight [expr ([lindex $ROIbbox 3] - [lindex $ROIbbox 1])/2.0]
    $canvas delete ROI
    $canvas create rectangle [expr $x - $halfwidth] [expr $y - $halfheight] [expr $x + $halfwidth] [expr $y + $halfheight] -tags ROI -outline blue
}


proc powFlipImage { direction } {
     global powPlotParam powFlipPlotWCSDefault powFlipPlotFitsHeaderDefault
     global powContourParam
     global powWCS powFitsHeader powFitsHeaderCnt
     global currgn 
     global xCount yCount
     global profile_gn

     set inDirection $direction
     set token img 

     set idx [string first "_contour" $currgn]
     set currgn_contour ""
     set inputCurrgnIsContour "false"
     if { $idx >= 0 } {
        set inputCurrgnIsContour "true"
        set currgn_contour $currgn
        set currgn [string range $currgn 0 [expr $idx - 1]]
     } else {
        set currgn_contour ${currgn}_contour
     }

     set errorFlag [ catch {
         powFindData $currgn 
     } err ]

     if ![info exists powPlotParam(graphType,$currgn)] {
        set powPlotParam(graphType,$currgn) "image"
        set powPlotParam(graphType,${currgn}scope) "image"
     }

     if { $errorFlag } {
        set errorFlag [ catch {
            array set crvInfo [powFetchCurveInfoHash c1_$currgn]
        } err ]

        if { !$errorFlag && [info exist powWCS(c1_$currgn)] } {
           set powPlotParam(graphType,c1_$currgn) "binary"
           set powPlotParam(zoomed,c1_$currgn) $powPlotParam(zoomed,$currgn)
        }

        set powPlotParam(graphType,$currgn) "binary"
        set powPlotParam(graphType,${currgn}scope) "binary"
        set imageInfoList {}
        lappend imageInfoList "data"
        lappend imageInfoList c1_$currgn
        lappend imageInfoList "width"
        lappend imageInfoList $powPlotParam(xdimdisp,$currgn)
        lappend imageInfoList "height"
        lappend imageInfoList $powPlotParam(ydimdisp,$currgn)
     } else {
        set obj $currgn
        set imageInfoList [powFetchImageInfoHash $obj]
   
        if { [expr [llength $imageInfoList] % 2] != 0 } {
           lappend imageInfoList DONTCARE
        }
     }

     set useWCS true
     array set powEditObject $imageInfoList
     if { $powWCS($currgn) == "" || [lindex [lindex $powWCS($currgn) 0] 0] == 0.0 } {
        set useWCS false
     }

     if { $powPlotParam(graphType,$currgn) == "binary" } {
        if { $useWCS == "false" } {
           if [info exist powWCS(c1_$currgn)] {
              set powPlotParam(graphType,c1_$currgn) "binary"
           }

           set powPlotParam(graphType,$currgn) "binary"
           set powPlotParam(graphType,${currgn}scope) "binary"
        } else {
           #tk_messageBox -message "Flipping is not available for plot using WCS info." \
           #              -title "Not Available" -type ok -parent .pow
	   #return
        }
     }

     set refPixList [lindex $powWCS($currgn) 1]
     set gemoList [lindex $powWCS($currgn) 2]

     switch $direction {
            "X" -
            "Y" -
            "B" {
                if ![info exists powFlipPlotWCSDefault($currgn)] {
                   set powFlipPlotWCSDefault($currgn) $powWCS($currgn)
                   set powFlipPlotFitsHeaderDefault($currgn) $powFitsHeader($currgn)
                }
                if { ![info exists powFlipPlotWCSDefault(c1_$currgn)] && \
                      [info exists powWCS(c1_$currgn)] } {
                   set powFlipPlotWCSDefault(c1_$currgn) $powWCS(c1_$currgn)
                   set powFlipPlotFitsHeaderDefault(c1_$currgn) $powFitsHeader(c1_$currgn)
                }
            }
     }

     set directionList {}
     if ![info exists xCount($currgn)] {
        set xCount($currgn) 0
        set xCount(${currgn}scope) 0
     }
     if ![info exists yCount($currgn)] {
        set yCount($currgn) 0
        set yCount(${currgn}scope) 0
     }

     set powPlotParam(flipD,$currgn) $direction
     switch $direction {
            "X" -
            "Y" {
               lappend directionList $direction
            }
            "B" { 
               lappend directionList "X"
               lappend directionList "Y"
            }
            "U" {
               if { [expr $xCount($currgn) % 2] != 0 } {
                  lappend directionList "X"
               }
               if { [expr $yCount($currgn) % 2] != 0 } {
                  lappend directionList "Y"
               }
            }
     }

     set setWCSFlag true

     set yPos 2
     if { [llength $gemoList] <= 0 } {
        set setWCSFlag false
        #set powWCS($currgn) {{0.0 0.0} {$powEditObject(width) $powEditObject(height)} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
        #set powWCS($currgn) {{0.0 0.0)} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
        set gemoList   [list 1.0 -0.0 0.0 1.0]
        set refPixList [list $powEditObject(width) $powEditObject(height)]
     } else {
        set yPos [expr int(sqrt([llength $gemoList]))]
     }

     set naxisIdx $yPos

     # determine exact CDELT2 position in gemoList
     incr yPos


     set CDnExist [powDetermineKeyWordExist $currgn "CD1_1"]

     for {set d 0} { $d < [llength $directionList] } {incr d} {
         set direction [lindex $directionList $d]
         switch $direction {
            "X" {
                set CDELTExist [powDetermineKeyWordExist $currgn "CDELT1"]
               
                set refPixValue [expr abs($powEditObject(width) - [lindex $refPixList 0]) + 1]
                set refPixList [lreplace $refPixList 0 0 $refPixValue]

                # regardless if CDELT or CDn exist, this bit CDELT1 or CD1_1 has to be flipped
                set gemoValue [expr [lindex $gemoList 0] * -1.0]
                set gemoList [lreplace $gemoList 0 0 $gemoValue]

                if { $CDnExist != "false" } {
                   # CD1_1 and CD2_1 need to be flipped
                   set gemo2Value [expr [lindex $gemoList $naxisIdx] * -1.0]
                   set gemoList [lreplace $gemoList $naxisIdx $naxisIdx $gemo2Value]
                }

                if { $CDELTExist != "false" } {
                   set CDELTExist [expr $CDELTExist * -1.0]
                }

                if { $setWCSFlag == "true" } {
                   set powWCS($currgn) [lreplace $powWCS($currgn) 1 1 $refPixList]
                   set powWCS($currgn) [lreplace $powWCS($currgn) 2 2 $gemoList]
                }

                incr xCount($currgn)
                incr xCount(${currgn}scope)
                if { $useWCS == "true" } {
                   if { $CDELTExist != "false" } {
                      powChangeFitsHeaderKeyWordValue $currgn {"CDELT1" "CRPIX1"} \
                                                      $direction \
                                                      [list $CDELTExist \
                                                            [lindex $refPixList 0]] \
                                                      $refPixList
                   }
                   if { $CDnExist != "false" } {
                      powChangeFitsHeaderKeyWordValue $currgn {"CD1_1" "CD2_1" "CRPIX1"} \
                                                      $direction \
                                                      [list [lindex $gemoList 0] \
                                                            [lindex $gemoList $naxisIdx] \
                                                            [lindex $refPixList 0]] \
                                                      $refPixList
                   }
                }
            }
            "Y" {
                set CDELTExist [powDetermineKeyWordExist $currgn "CDELT2"]
                set refPixValue [expr $powEditObject(height) - [lindex $refPixList 1] + 1]
                set refPixList [lreplace $refPixList 1 1 $refPixValue]

                # regardless if CDELT or CDn exist, this bit CDELT2 or CD2_2 has to be flipped
                set gemoValue [expr [lindex $gemoList $yPos] * -1]
                set gemoList [lreplace $gemoList $yPos $yPos $gemoValue]

                if { $CDnExist != "false" } {
                   set gemo2Value [expr [lindex $gemoList [expr $naxisIdx - 1]] * -1.0]
                   set gemoList [lreplace $gemoList [expr $naxisIdx - 1] \
                                                    [expr $naxisIdx - 1] $gemo2Value]
                }
                if { $CDELTExist != "false" } {
                   set CDELTExist [expr $CDELTExist * -1.0]
                }

                if { $setWCSFlag == "true" } {
                   set powWCS($currgn) [lreplace $powWCS($currgn) 1 1 $refPixList]
                   set powWCS($currgn) [lreplace $powWCS($currgn) 2 2 $gemoList]
                }

                incr yCount($currgn)
                incr yCount(${currgn}scope)
                if { $useWCS == "true" } {
                   if { $CDELTExist != "false" } {
                      powChangeFitsHeaderKeyWordValue $currgn {"CDELT2" "CRPIX2"} \
                                                      $direction \
                                                      [list $CDELTExist \
                                                            [lindex $refPixList 1]] \
                                                      $refPixList
                   }
                   if { $CDnExist != "false" } {
                      powChangeFitsHeaderKeyWordValue $currgn {"CD2_2" "CD1_2" "CRPIX2"} \
                                                      $direction \
                                                      [list [lindex $gemoList $yPos] \
                                                            [lindex $gemoList [expr $naxisIdx - 1]] \
                                                            [lindex $refPixList 1]] \
                                                      $refPixList
                   }
                }
            }
         }

#puts "CDnExit: $CDnExist, CDELTExist: $CDELTExist"

         if { $powPlotParam(graphType,$currgn) == "image" } {
            powCreateDataFlip $currgn $direction $powEditObject(height) $powEditObject(width)
         }
     }

     if { $inDirection == "U" } {
        set xCount($currgn) 0
        set yCount($currgn) 0
        set xCount(${currgn}scope) 0
        set yCount(${currgn}scope) 0
     }

#powDebugDataPrint "$currgn" $powFitsHeader($currgn)
     #powAdornGraph $currgn .pow.pow
     if { $inputCurrgnIsContour == "false" } {
        powRedrawGraphHandles $currgn
        powRedrawScopebox
        powEndROI 1
     } else {
        if { $currgn_contour != "" && [info exists powContourParam(separate)] && \
             $powContourParam(separate) == "yes" } {
	   powMakeContours $powContourParam(image) \
                           $powContourParam(list) \
                           $powContourParam(res)
           powRedrawScopebox
           #powEndROI 1
        }
     }
}

proc powDebugDataPrint { title string } {
     puts "$title"
     set k 0
     for {set i 0} {$i < [string length $string]} {incr i 80} {
        set currentStr [string range $string $i [expr $i + 79]]
        puts "<$currentStr>"
        incr k
     }
     puts "count: $k"
}

proc powDetermineKeyWordExist { img keyword } {
     global powFitsHeader 

     set str $powFitsHeader($img)
     set findFlag false
     set i 0
     while { 1 } {
        set currentStr [string range $str $i [expr $i + 79]]
        incr i 80
        if { [string trim $currentStr] == "" } {
           if { $i > [string length $str] } break
           continue
        }
        set currentStrToken [split $currentStr "=/"]
        set headerT  [string trim [lindex $currentStrToken 0]]
        set valueT   [string trim [lindex $currentStrToken 1]]
        
        if { [string tolower $headerT] == "end" } {
           break
        }
        if { $headerT == $keyword } {
           set findFlag $valueT
           break
        }
     }

     return $findFlag
}

proc powChangeFitsHeaderKeyWordValue { img keywordList direction changeList refPixList } {
     global powFitsHeader powWCS powFitsHeaderCnt
     global xCount yCount

#puts "powChangeFitsHeaderKeyWordValue: keywordList: $keywordList"
#puts "                               : changeList : $changeList"
     set changeListDone {}
     set str $powFitsHeader($img)
     set i 0
     set powFitsHeaderStrCnt 0
     set powFitsHeaderStr ""

     while { 1 } {
        set currentStr [string range $str $i [expr $i + 79]]
        incr i 80
        if { [string trim $currentStr] == "" } {
           if { $i > [string length $str] } break
           continue
        }
        set currentStrToken [split $currentStr "=/"]
        set headerT  [string trim [lindex $currentStrToken 0]]
        set valueT   [string trim [lindex $currentStrToken 1]]
        set header   [lindex $currentStrToken 0]
        set value    [lindex $currentStrToken 1]
        set comment  [lindex $currentStrToken 2]
        
        if { [string tolower $headerT] == "end" } {
           set endStr $currentStr
           break
        }
        set findFlag false
        set idx [lsearch -exact $keywordList $headerT]
        set keyword ""
        if { $idx < 0 } {
           set headerT [string range $headerT 0 [expr [string length $headerT] - 2]]
           set idx [lsearch -exact $keywordList $headerT]
           if { $idx >= 0 } {
              set findFlag true
              set keyword [lindex $keywordList $idx]
           }
        } else {
           set findFlag true
           set keyword [lindex $keywordList $idx]
        }
        
        if { $findFlag == "true" } {
           lappend changeListDone $keyword
           switch -glob $keyword {
                "CROTA2" -
                "CRPIX*" {
                   set newStr [format "%.10E " [lindex $changeList $idx]]
                }
                "CD*" {
                   set testStr [string trim $value]
                   if { [string range $testStr 0 0] == "-" } {
                      set testStr [string range $testStr 1 end]
                   } else {
                      set testStr [format " -%s" $testStr]
                   }
                   set newStr [format "%s " $testStr]
                }
                "CTYPE*" {
                   set newStr [format "'%s' " [lindex $changeList $idx]]
                }
           }

           if { [llength $currentStrToken] == 2 } {
              set newStr [format "%-s=%22s" $header $newStr]
              set newStr [format "%s%[expr 80 - [string length $newStr]]s" $newStr " "]
           } elseif { [llength $currentStrToken] == 3 } {
              set newStr [format "%-s=%[string length $value]s/%s" $header $newStr $comment]
           }

        } else {
           set newStr $currentStr
        }

        # make sure final card is 80 characters long
        set newStr [string range $newStr 0 79]
        if { $powFitsHeaderStrCnt == 0 } {
           set powFitsHeaderStr $newStr
        } else {
           set powFitsHeaderStr [format "%s%s" $powFitsHeaderStr $newStr]
        }
        incr powFitsHeaderStrCnt
     }

     # check to see if any keyword required by user is not in original header
     if { [llength $changeListDone] != [llength $keywordList] } {
        set restKeywordList {}
        set restKeywordValueList {}
        for { set i 0 } {$i < [llength $keywordList]} {incr i} {
           set idx [lsearch -exact $changeListDone [lindex $keywordList $i]]
           if { $idx < 0 } {
              lappend restKeywordList [lindex $keywordList $i]
              lappend restKeywordValueList [lindex $changeList $i]
           }
        }

        for { set i 0 } {$i < [llength $restKeywordList]} {incr i} {
            set newStr [format "%-8s=%21s" [lindex $restKeywordList $i] \
                                          [lindex $restKeywordValueList $i]]
            set newStr [format "%s%[expr 80 - [string length $newStr]]s" $newStr " "]
            set powFitsHeaderStr [format "%s%s" $powFitsHeaderStr $newStr]
            incr powFitsHeaderCnt($img) 
        }
     }

     # add end token string
     set powFitsHeaderStr [format "%s%s" $powFitsHeaderStr $endStr]

     if { $direction != "U" } {
        set powFitsHeader($img) $powFitsHeaderStr
        set powFitsHeader(${img}scope) $powFitsHeaderStr
     } else {
        set powFitsHeader(${img}scope) $powFitsHeader($img)
     }

     set powWCS(${img}scope) $powWCS($img)
     if [info exists powFitsHeader(c1_$img)] {
     #   set powWCS(c1_$img) $powWCS($img)
        set powFitsHeader(c1_$img) $powFitsHeader($img)
        set powFitsHeaderCnt(c1_$img) $powFitsHeaderCnt($img)
        set xCount(c1_$img) $xCount($img)
        set yCount(c1_$img) $yCount($img)
     }
     powResetWcsStructure -d $img $direction [lindex $refPixList 0] [lindex $refPixList 1]
}

proc powEndROI { zoomback {canvas .pow.pow}} {
    global saveROI powGUI
    global roi_xo roi_yo
    global roi_xn roi_yn r_staticYonG r_staticX0onG ROIbbox
    global roi_pixelxn roi_pixelyn 
    global ROIunits powZoomStart
    global g_magnification
    global xCount yCount
    global powWCS powFitsHeader powFitsHeaderCnt
    global currentGraphList
    global powWCSLabel
    global powDrawDone

    #if zoomback is true, we're restoring the "default size" of the graph
    #otherwise, this is the end of a user dragging an ROI box
    global powPlotParam currgn axisToChainHash chainToAxisHash powResizeMain powEditPlotParam
    global powGraphsTagRangeList powTagsColorMap currimg
    global powGraphsTagRectList

    if { [info exists roi_xn] && [info exist currimg] } {
       set result [powCanvasToPixel $currgn $currimg $roi_xn $roi_yn ".pow.pow"]
       set roi_pixelxn [lindex $result 0]
       set roi_pixelyn [lindex $result 1]
    }

    #Guard against the simple click.
    if { [info exists roi_xo] && [info exists roi_xn] &&
	 [info exists roi_yo] && [info exists roi_yn] } {
       if {$zoomback ==0 && $roi_xo == $roi_xn && $roi_yo == $roi_yn} {
	  if {$canvas != ".pow.scope" } {
	     $canvas delete ROI
             unset roi_xo roi_xn roi_yo roi_yn
	  }
	  return
       }
    }
    set zoomback [expr $zoomback % 2]
#puts "zoomback: $zoomback"

    if {$canvas == ".pow.scope"} {
	set currgraph ${currgn}scope
	# If graph is empty, scopebox will be empty, so don't draw ROI
	if { [.pow.scope find withtag ${currgraph}box]=="" } {
	   .pow.scope delete ROI
	   return
	}
    } else {
	set currgraph  $currgn
    }

    if {!($zoomback)} {
	set ROIbbox [$canvas coords ROI]
#puts "ROIbbox: $ROIbbox"
	set x0 [lindex $ROIbbox 0]
	set x1 [lindex $ROIbbox 2]
	set y0 [lindex $ROIbbox 3]
	set y1 [lindex $ROIbbox 1]

	if { $canvas==".pow.pow" } {
	   # Find which graph ROI overlaps, if any

	   set overlap_gn [powFindOverlapGraph $x0 $y1 $x1 $y0]
	   if { $overlap_gn != "" && $overlap_gn!=$currgn } {
	      powSelectGraph $overlap_gn
	      set currgraph $currgn
	   }
	}

#get "real" coordinates of ROIbbox
	set gcoords  [powCanvasToGraph $currgraph $x0 $y0 $canvas] 
	set llx [lindex $gcoords 0]
	set lly [lindex $gcoords 1]
	set gcoords  [powCanvasToGraph $currgraph $x1 $y1 $canvas] 
	set urx [lindex $gcoords 0]
	set ury [lindex $gcoords 1]

        set r_staticYonG [expr ($lly + $ury) / 2.0]
        set r_staticX0onG $llx
    } else {
        catch { powResetWcsStructure -r $currgn 0.0 0.0 }
    }

    set graphlist $currgn

    if {[array names axisToChainHash ${currgn}X] != ""} {
	set graphlist [concat $graphlist $chainToAxisHash($axisToChainHash(${currgn}X))]
    }

    if {[array names axisToChainHash ${currgn}Y] != ""} {
	set graphlist [concat $graphlist $chainToAxisHash($axisToChainHash(${currgn}Y))]
    }

    set currentGraphList $graphlist
    set principal 1
    
    foreach graph [concat $graphlist] {
        set graphIdx 0
        if [info exists currimg] {
           set graphIdx [lsearch -exact $powPlotParam(images,$graph) $currimg]
        }
	if {!$principal} {
           set axis [chopped $graph]
           set graph [chop $graph]
	}

	if {!$zoomback} {
	    if $principal {
#note rROIbbox is in "scientific" coordinate, other bboxs are in "X" coordinates (i.e. upper left origin)
		set rROIbbox [list $llx $lly $urx $ury]
		if { $canvas != ".pow.scope" } { $canvas delete ROI }
	    } else {
		if {$axis == "X"} {
		    set abox [.pow.pow coords ${graph}box]
		    set cllx $llx
		    set clly [lindex [powCanvasToGraph $graph \
			     [lindex $abox 0] [lindex $abox 3] .pow.pow] 1]
		    set curx $urx
		    set cury [lindex [powCanvasToGraph $graph \
			     [lindex $abox 2] [lindex $abox 1] .pow.pow] 1]
		    set rROIbbox [list $cllx $clly $curx $cury]
		} else {
		    set abox [.pow.pow coords ${graph}box]
		    set cllx [lindex [powCanvasToGraph $graph \
			     [lindex $abox 0] [lindex $abox 3] .pow.pow] 0]
		    set clly $lly
		    set curx [lindex [powCanvasToGraph $graph \
			     [lindex $abox 2] [lindex $abox 1] .pow.pow] 0]
		    set cury $ury
		    set rROIbbox [list $cllx $clly $curx $cury]
		}	    
	    }
	}

#get together everything you need for the next call to powCreateGraph
	set graph_position [.pow.pow coords ${graph}handle]
	set ROIcurves $powPlotParam(curves,$graph)
	set ROIimages $powPlotParam(images,$graph)

        set selection $powPlotParam(wcsName,$currgn)
        if { $selection == "WCS" } {
           set selection "DEFAULT"
        } else {
           set selection [string toupper [string range $selection end end]]
        }

        set powPlotParam(xunits,$graph) $powWCSLabel(xunit,$graph,$selection)
        set powPlotParam(yunits,$graph) $powWCSLabel(yunit,$graph,$selection)
       
#puts "selection: $selection, graph: $graph"
#puts "powPlotParam(xunits,$graph): $powWCSLabel(xunit,$graph,$selection)"
#puts "powPlotParam(yunits,$graph): $powWCSLabel(yunit,$graph,$selection)"
#puts "powPlotParam(xlabel,$graph): $powWCSLabel(xlabel,$graph,$selection)"
#puts "powPlotParam(ylabel,$graph): $powWCSLabel(ylabel,$graph,$selection)"

        if ![info exist powWCSLabel(xlabel,$graph,$selection)] {
           set powWCSLabel(xlabel,$graph,$selection) ""
        }
        if ![info exist powWCSLabel(ylabel,$graph,$selection)] {
           set powWCSLabel(ylabel,$graph,$selection) ""
        }
        set powPlotParam(xlabel,$graph) $powWCSLabel(xlabel,$graph,$selection)
        set powPlotParam(ylabel,$graph) $powWCSLabel(ylabel,$graph,$selection)

	set ROIunits [list $powPlotParam(xunits,$graph) \
			   $powPlotParam(yunits,$graph) \
			   $powPlotParam(xlabel,$graph) \
			   $powPlotParam(ylabel,$graph) ]

	set ROIgraphOptions [powGetGraphOptions $graph]

#puts "powEndROI powPlotParam(xmagstep,$currgn): $powPlotParam(xmagstep,$currgn)"
#puts "powEndROI powPlotParam(prev_magnification,$currgn): $powPlotParam(prev_magnification,$currgn)"
#puts "powEndROI powPlotParam(new_magnification,$currgn): $powPlotParam(new_magnification,$currgn)"
#puts "powEndROI powPlotParam(g_multiplier,$currgn): $powPlotParam(g_multiplier,$currgn)"
#puts "powEndROI powPlotParam(g_magnification,$currgn): $powPlotParam(g_magnification,$currgn)"
#puts "powEndROI g_magnification: $g_magnification"

#	powUnmapGraph $graph 0

#puts "powPlotParam(xBot,$currgn) $powPlotParam(xBot,$currgn)"
#puts "powPlotParam(xTop,$currgn) $powPlotParam(xTop,$currgn)"
#puts "powPlotParam(yBot,$currgn) $powPlotParam(yBot,$currgn)"
#puts "powPlotParam(yTop,$currgn) $powPlotParam(yTop,$currgn)"
#puts "powPlotParam(xdimdisp,$currgn): $powPlotParam(xdimdisp,$currgn)"
#puts "powPlotParam(zoomed,$currgn) $powPlotParam(zoomed,$currgn)"
	if {$zoomback} {
            if { ![info exists powZoomStart($currgn)] || $powZoomStart($currgn) != 1 } {
	       set powPlotParam(zoomed,$graph) 1
	       eval [concat powCreateGraph $graph \{$ROIcurves\} \{$ROIimages\} \
				       $ROIunits $powPlotParam(xdimdisp,$graph) \
				       $powPlotParam(ydimdisp,$graph)]
            } else {
               set powPlotParam(xBot,$graph) $powPlotParam(xBot,$currgn)
               set powPlotParam(xTop,$graph) $powPlotParam(xTop,$currgn)
               set powPlotParam(yBot,$graph) $powPlotParam(yBot,$currgn)
               set powPlotParam(yTop,$graph) $powPlotParam(yTop,$currgn)
	       set powPlotParam(zoomed,$graph) $powPlotParam(zoomed,$currgn)
            }
            set rROIbbox [list $powPlotParam(xBot,$graph) \
	   		       $powPlotParam(yBot,$graph) \
			       $powPlotParam(xTop,$graph) \
			       $powPlotParam(yTop,$graph) ]
	} else {
            set powPlotParam(zoomed,$currgn) 1

            if [info exists powPlotParam(zoomed,c1_$currgn)] {
               set powPlotParam(zoomed,c1_$currgn) 1
            }
           
	    eval [concat powCreateGraph $graph \{$ROIcurves\} \{$ROIimages\} \
				    $ROIunits $powPlotParam(xdimdisp,$graph) \
				    $powPlotParam(ydimdisp,$graph) $rROIbbox]
	}

	eval [concat powGraphOptions $graph $ROIgraphOptions]
	set principal 0
        if { [llength $ROIimages] > 1 } {
           # this is a movie
           powSelectImage $graph [lindex $powPlotParam(images,$graph) $graphIdx]
        }
    }

    if {$powGUI && $canvas != ".pow.scope"} {
	.pow.scope delete ROI
	powDrawScopeROI $rROIbbox
    }

    if {[info exists powGraphsTagRangeList($currgn)]} {
	foreach tagrangelist [concat $powGraphsTagRangeList($currgn)] {
	    set tag [lindex $tagrangelist 3]
	    eval [concat powColorRange $currgn $tagrangelist $powTagsColorMap($tag) 1]
	}
    }
    if {[info exists powGraphsTagRectList($currgn)]} {
	foreach tagrectlist [concat $powGraphsTagRectList($currgn)] {
	    set tag [lindex $tagrectlist 4]
	    eval [concat powColorRect $currgn $tagrectlist $powTagsColorMap($tag) 1]
	}
    }

    if { $zoomback == 1 } {
       # zoom back to original
       set powPlotParam(prev_magnification,$currgn) 1.0
       set powPlotParam(new_magnification,$currgn) 1.0
       set powPlotParam(g_multiplier,$currgn) 4.0
       set powPlotParam(g_magnification,$currgn) 1.0
       set g_magnification 1.0
    }

    # this is for editing the graph
    set powEditPlotParam(xBot,new) $powPlotParam(xBot,$currgn)
    set powEditPlotParam(yBot,new) $powPlotParam(yBot,$currgn)
    set powEditPlotParam(xTop,new) $powPlotParam(xTop,$currgn)
    set powEditPlotParam(yTop,new) $powPlotParam(yTop,$currgn)
    set powDrawDone 1
}


proc powDrawScopeROI { rROIbbox } {
#puts "powDrawScopeROI start"
    global currgn

    set gn ${currgn}scope

    # If graph is empty, scopebox will be empty, so don't draw ROI
    if { [.pow.scope find withtag ${gn}box]=="" } return

    set rllx [lindex $rROIbbox 0]
    set rlly [lindex $rROIbbox 1]
    set rurx [lindex $rROIbbox 2]
    set rury [lindex $rROIbbox 3]
    set ccoords [powGraphToCanvas $gn $rllx $rlly .pow.scope]
    set ulx [lindex $ccoords 0]
    set lry [lindex $ccoords 1]
    set ccoords [powGraphToCanvas $gn $rurx $rury .pow.scope]
    set lrx [lindex $ccoords 0]
    set uly [lindex $ccoords 1]
    .pow.scope create rectangle $ulx $uly $lrx $lry -tags ROI -outline blue 
}


#Select the graph if it is not the current graph. Then replot it.
proc powDrawOriginal {x y} {
#puts "powDrawOriginal start"
    global powDrawOriginalFlag
    global currgn

    set powDrawOriginalFlag true
    set gn [powWhereAmI $x $y]
    if {$gn != $currgn } {
               if { $gn != "NULL" } {
          set currgn $gn 
               } else {
                  # outside of graph, possible on scope
                  set gn $currgn
               }
       powSelectGraph $gn
    }
    powEndROI 1
}
   


proc powStretch {gn x y} {
#puts "powStretch start"
    global stretchX0 stretchY0
    global powPlotParam
    global fixedStretch new_xdim new_ydim
    global yellowLineWidth
    global ulx_yellow uly_yellow lrx_yellow lry_yellow
    global powHandX0 powHandY0
    global powGBWidth powGBHeight
    global powrootx powrooty

    # Calculate root bounding box of allowed area of the canvas (- a little)

    if { ![info exists powrootx] || ![info exists powrooty] } {
       return
    }
    set cx [.pow.pow canvasx [expr $x - $powrootx]]
    set cy [.pow.pow canvasy [expr $y - $powrooty]]
    

    set left [expr $ulx_yellow + 30]
    set top [expr $uly_yellow + 30]
    

    set right [.pow.pow canvasx [expr [winfo width  .pow.pow] - 20]]
    set bott  [.pow.pow canvasy [expr [winfo height .pow.pow] - 20]]
    
    # Check whether we have moved outside of the .pow.pow canvas
    # or past the upper left corner of the current_gn box
   
    if { $cx < $left } {
	set cx $left
    } elseif { $cx > $right } {
	set cx $right
    }

    if { $cy < $top } {
	set cy $top
    } elseif { $cy > $bott } {
	set cy $bott
    }

    .pow.pow delete current_gn


#how far have we moved the stretch-handle?

    set dx [expr $cx - $stretchX0]
    set dy [expr $cy - $stretchY0]



#check magstep


    

# calculate new xfactor from change in size of graphbox

    if { [expr $powGBWidth  + $dx] < 1 } { set dx [expr 1 - $powGBWidth]  }
    if { [expr $powGBHeight + $dy] < 1 } { set dy [expr 1 - $powGBHeight] }

    set xfactor [expr double($powGBWidth  + $dx) / double($powGBWidth) ]
    set yfactor [expr double($powGBHeight + $dy) / double($powGBHeight)]

    if { $fixedStretch == "yes" } {
	if { $xfactor < $yfactor } {
	    set yfactor $xfactor
	    set dy [expr double($powGBHeight) * ($yfactor - 1.0)]
	} else {
	    set xfactor $yfactor
	    set dx [expr double($powGBWidth)  * ($xfactor - 1.0)]
	}
    }



#Move the stretch-handle

    .pow.pow coords ${gn}shandle [expr $powHandX0 + $dx] [expr $powHandY0 +$dy]
    
#make new current_gn

    .pow.pow create rectangle  $ulx_yellow $uly_yellow \
	[expr $lrx_yellow + $dx] [expr $lry_yellow + $dy] \
	-tags  "current_gn graphDragable ${gn}yhandle handle ohandle" \
	-outline yellow -width $yellowLineWidth

    set new_xdim [expr $xfactor * $powGBWidth]
    set new_ydim [expr $yfactor * $powGBHeight]

#make magstep label   
    set sizeText [format "%4d x %4d" [expr round($new_xdim)] \
				     [expr round($new_ydim)]  ]
    .pow.ms[powCleanName ${gn}]handle configure -text "GraphSize: $sizeText" 

}

proc powBeginStretch {gn x y} {
#puts "powBeginStretch start"
	
    global powPlotParam ulx_yellow uly_yellow lrx_yellow lry_yellow
    global yellowLineWidth stretchX0 stretchY0 stretchGBWidth stretchGBHeight
    global powHandX0 powHandY0
    global powGBWidth powGBHeight
    global powrootx powrooty

    powSelectGraph $gn
    set bbox [.pow.pow coords current_gn]
    set ulx_yellow [lindex $bbox 0]
    set uly_yellow [lindex $bbox 1]
    set lrx_yellow [lindex $bbox 2]
    set lry_yellow [lindex $bbox 3]

    set handcoords [.pow.pow coords ${gn}shandle]
    
    set powHandX0 [lindex $handcoords 0]
    set powHandY0 [lindex $handcoords 1]

    set gbox [.pow.pow coords ${gn}box]
    
    set powGBWidth [expr [lindex $gbox 2] - [lindex $gbox 0]]
    set powGBHeight [expr [lindex $gbox 3] - [lindex $gbox 1]]

    set powrootx [winfo rootx .pow.pow]
    set powrooty [winfo rooty .pow.pow]

    set stretchX0 [.pow.pow canvasx [expr $x - $powrootx ]]
    set stretchY0 [.pow.pow canvasy [expr $y - $powrooty ]]




}


proc powEndStretch {gn} {
#puts "powEndStretch start"
    global powcursor powResizeMain powPlotParam 
    global fixedStretch new_xdim new_ydim

    powStretchGraphToSize $gn $new_xdim $new_ydim
    powSelectGraph $gn
    powReconfigureToplevel $powResizeMain
}


proc powStartNewRow { } {
#puts "powStartNewRow start"
    global powOpenAreaTop
 
#    update idletasks
    set powOpenAreaTop [lindex [.pow.pow bbox all] 3]
}

proc powInitGraph {gn xMin xMax yMin yMax xunits yunits xLabel yLabel \
		       {canvas ".pow.pow"} \
		       {xDim 600} {yDim 400} \
		       {xDimDisp 600} {yDimDisp 400} {aspect yes} \
		       {xmargin 60} {ymargin 60} } {

     # An array of plotting parameters
     global powPlotParam powOpenAreaTop powbg powScopeMargin powFontParam
     global powHeaderWcsKeyWord
     global powWCSList


catch { wm deiconify .pow }
#####################Plot
set powPlotParam(images,$gn) "NULL"
set powPlotParam(curves,$gn) "NULL"
#set powPlotParam(zoomed,$gn) 0

if ![info exists powPlotParam(zoomed,$gn)] {
   set powPlotParam(zoomed,$gn) 0
}

 if {![info exists powOpenAreaTop]} {set powOpenAreaTop 10}
 if { $canvas == ".pow.scope" } {
     set powPlotParam(xo,$gn) $powScopeMargin
     set powPlotParam(yo,$gn) $powScopeMargin
 } elseif {[info exists powPlotParam(xo,$gn)]} {
#   Do nothing, thereby keeping the xo/yo values intact
 } elseif {$canvas == ".pow.pow"} then {
#     update idletasks
     set bbox [.pow.pow bbox all]
     if {$bbox != ""} {
	 set leftSide [lindex $bbox 0]
	 .pow.pow addtag currentRow enclosed $leftSide $powOpenAreaTop \
	     [lindex $bbox 2]  [lindex $bbox 3]
#	 update idletasks
	 set bbox [.pow.pow bbox currentRow]
	 .pow.pow dtag currentRow
	 if {$bbox != ""} then {
	     set powPlotParam(xo,$gn) [expr [lindex $bbox 2] + $xmargin]
	 } else {
	     set powPlotParam(xo,$gn) [expr $leftSide + $xmargin]
	 } 
     } else {
	 set powPlotParam(xo,$gn) $xmargin
     }
     set powPlotParam(yo,$gn) [expr $powOpenAreaTop  + $ymargin ]
 }

 set powPlotParam(graphHeight,$gn) $yDim
 set powPlotParam(graphWidth,$gn) $xDim
 set powPlotParam(xBot,$gn) $xMin
 set powPlotParam(xTop,$gn) $xMax
 set powPlotParam(xunits,$gn) $xunits
 set powPlotParam(xlabel,$gn) $xLabel
 set powPlotParam(yBot,$gn) $yMin
 set powPlotParam(yTop,$gn) $yMax 
 set powPlotParam(yunits,$gn) $yunits
 set powPlotParam(ylabel,$gn) $yLabel
 
###################defaults for optional graph params handled by
#                  powGraphOptions
if {![info exists powPlotParam(bgcolor,$gn)]} {
    set powPlotParam(bgcolor,$gn) $powbg
}
if {![info exists powPlotParam(xmargin,$gn)]} {
    set powPlotParam(xmargin,$gn) $xmargin
}
if {![info exists powPlotParam(ymargin,$gn)]} {
    set powPlotParam(ymargin,$gn) $ymargin
}
if {![info exists powPlotParam(handletext,$gn)]} {
    set powPlotParam(handletext,$gn) "Select/Move: $gn"
}
if {![info exists powPlotParam(handleanchor,$gn)]} {
    set powPlotParam(handleanchor,$gn) "sw"
}
if {![info exists powPlotParam(handleposition,$gn)]} {
    set powPlotParam(handleposition,$gn) "tl"
}
if {![info exists powPlotParam(titleString,$gn)]} {
    set powPlotParam(titleString,$gn) "$gn"
}
if {![info exists powPlotParam(titlePosition,$gn)]} {
    set powPlotParam(titlePosition,$gn) "n"
}
if {![info exists powPlotParam(titleAnchor,$gn)]} {
    set powPlotParam(titleAnchor,$gn) "s"
}

#      Axis tick and grid options...

if {![info exists powPlotParam(GridLines,$gn)]} {
    set powPlotParam(GridLines,$gn) $powPlotParam(GridLines,powDef)
}
if {![info exists powPlotParam(GridColor,$gn)]} {
    set powPlotParam(GridColor,$gn) $powPlotParam(GridColor,powDef)
}
if {![info exists powPlotParam(GridDash,$gn)]} {
    set powPlotParam(GridDash,$gn) $powPlotParam(GridDash,powDef)
}
if {![info exists powPlotParam(xNumTicks,$gn)]} {
    set powPlotParam(xNumTicks,$gn) $powPlotParam(xNumTicks,powDef)
}
if {![info exists powPlotParam(yNumTicks,$gn)]} {
    set powPlotParam(yNumTicks,$gn) $powPlotParam(yNumTicks,powDef)
}
if {![info exists powPlotParam(xTickLength,$gn)]} {
    # order is [lft rgt top bot]
    set powPlotParam(xTickLength,$gn) $powPlotParam(xTickLength,powDef)
}
if {![info exists powPlotParam(yTickLength,$gn)]} {
    # order is [lft rgt top bot]
    set powPlotParam(yTickLength,$gn) $powPlotParam(yTickLength,powDef)
}
if {![info exists powPlotParam(xLabelTicks,$gn)]} {
    # order is [lft rgt top bot]
    set powPlotParam(xLabelTicks,$gn) $powPlotParam(xLabelTicks,powDef)
}
if {![info exists powPlotParam(yLabelTicks,$gn)]} {
    # order is [lft rgt top bot]
    set powPlotParam(yLabelTicks,$gn) $powPlotParam(yLabelTicks,powDef)
}
if {![info exists powPlotParam(tickLabels,$gn)]} {
    set powPlotParam(tickLabels,$gn) $powPlotParam(tickLabels,powDef)
}
if {![info exists powPlotParam(tickFormatCmdX,$gn)]} {
    set powPlotParam(tickFormatCmdX,$gn) $powPlotParam(tickFormatCmdX,powDef)
}
if {![info exists powPlotParam(tickFormatCmdY,$gn)]} {
    set powPlotParam(tickFormatCmdY,$gn) $powPlotParam(tickFormatCmdY,powDef)
}
if {![info exists powPlotParam(xTickScal,$gn)]} {
    set powPlotParam(xTickScal,$gn) $powPlotParam(xTickScal,powDef)
}
if {![info exists powPlotParam(yTickScal,$gn)]} {
    set powPlotParam(yTickScal,$gn) $powPlotParam(yTickScal,powDef)
}
if {![info exists powPlotParam(Notes,$gn)]} {
    set powPlotParam(Notes,$gn) {}
}

# WCS selection

if {![info exists powPlotParam(wcsName,$gn)]} {
    set powPlotParam(wcsName,$gn) $powPlotParam(wcsName,powDef)
}
set idx 3
foreach wcsName [list a b c d e f g h i j k l m n o p q r s t u v w x y z] {
    if { [llength $powWCSList($gn)] == 2 } {
       set found [lsearch -exact [lindex $powWCSList($gn) 1] [string toupper $wcsName]] 
       if { $found >= 0 } {
          .pow.mbar.edit.wcs entryconfigure $idx -state normal
       }
    }
    incr idx
}
update idletasks


#     Text Font Options...

foreach lbl $powFontParam(allTypes,powDef) {
    foreach opt $powFontParam(allOpts,powDef) {
	if { ![info exists powFontParam(${lbl}${opt},$gn)] } {
	    set powFontParam(${lbl}${opt},$gn) \
		  $powFontParam(${lbl}${opt},powDef)
	}
    }
}

#     Graph size

if { $xDimDisp == "NULL" } {
   if { ![info exists powPlotParam(xdimdisp,$gn)] } {
      set powPlotParam(xdimdisp,$gn) $powPlotParam(xdimdisp,powDef)
   }
   set xDimDisp $powPlotParam(xdimdisp,$gn)
} else {
   set powPlotParam(xdimdisp,$gn) $xDimDisp
}

if { $yDimDisp == "NULL" } {
   if { ![info exists powPlotParam(ydimdisp,$gn)] } {
      set powPlotParam(ydimdisp,$gn) $powPlotParam(ydimdisp,powDef)
   }
   set yDimDisp $powPlotParam(ydimdisp,$gn)
} else {
   set powPlotParam(ydimdisp,$gn) $yDimDisp
}


if {![info exists powPlotParam(FixedAspect,$gn)]} {
    set powPlotParam(FixedAspect,$gn) $aspect
}

#puts "powInitGraph Graph magstep, xDim: $xDim, yDim: $yDim, xDimDisp: $xDimDisp, yDimDisp, $yDimDisp"
set xmagstep  [expr double($xDimDisp)/$xDim]
set ymagstep  [expr double($yDimDisp)/$yDim]
set newaspect [expr $xmagstep/$ymagstep]

if { $powPlotParam(FixedAspect,$gn) } {
    if { [info exists powPlotParam(xmagstep,$gn)] } {
	set aspect [expr $powPlotParam(xmagstep,$gn) \
			 / $powPlotParam(ymagstep,$gn) ]
	if { $newaspect > $aspect } {
	    set xmagstep [expr $ymagstep*$aspect]
	} else {
	    set ymagstep [expr $xmagstep/$aspect]
	}
    } else {
	if { $xmagstep<$ymagstep } {
	   if { [expr $xmagstep*$yDim] < [expr $yDimDisp/15.0] } {
	      set ymagstep [expr $yDimDisp/15.0/$yDim]
	      set powPlotParam(yNumTicks,$gn) \
		    [expr $powPlotParam(yNumTicks,$gn)/2+1]
	   } else {
	      set ymagstep $xmagstep
	   }
	} else {
	   if { [expr $ymagstep*$xDim] < [expr $xDimDisp/15.0] } {
	      set xmagstep [expr $xDimDisp/15.0/$xDim]
	      set powPlotParam(xNumTicks,$gn) \
		    [expr $powPlotParam(xNumTicks,$gn)/2+1]
	   } else {
	      set xmagstep $ymagstep
	   }
	}
	# Handle special 1D case even better...
	if { $xDim==1 } {
	   set powPlotParam(xNumTicks,$gn) 0
	}
	if { $yDim == 1 } {
	   set powPlotParam(yNumTicks,$gn) 0
	}
    }
}

set powPlotParam(xmagstep,$gn) $xmagstep
set powPlotParam(ymagstep,$gn) $ymagstep
powSetGraphMagstep $gn $xmagstep $ymagstep

#######   End of powInitGraph   ########
}

proc powBuildGraph { gn images curves canvas } {
   global powPlotParam powResizeMain powGUI currgn powOrderedGraphList
   global powcursor powbg  powFirstTimeThroughFlag

   foreach el [list xo yo graphWidth graphHeight xmagstep ymagstep] {
      set $el $powPlotParam($el,$gn)
   }

# Clean the canvas if there was a previous version of this graph
   $canvas delete $gn

# Plot graph box and other niceties

   $canvas  create rectangle $xo $yo \
	 [expr $graphWidth  * $xmagstep + $xo] \
	 [expr $graphHeight * $ymagstep + $yo] \
	 -tags "$gn ${gn}box ${gn}line gbox" -outline black

   if {$canvas == ".pow.pow"} {
	
      # Can't have an image from the previous graph interfering with a new one
      powDeSelectImage

      powAdornGraph $gn $canvas

      .pow.pow delete ${gn}handles
      .pow.pow delete ${gn}shandle
      powMakeGraphHandles $gn

      if {$powFirstTimeThroughFlag} {
	 powReconfigureToplevel 1
	 set powFirstTimeThroughFlag 0
      } else {
	 powReconfigureToplevel $powResizeMain
      }

# Scroll to new graph
      set cbbox [.pow.pow cget -scrollregion]
      set bbox1 [.pow.pow bbox $gn]
      set xloc [expr double( [lindex $bbox1 0]+[lindex $bbox1 2]) \
	    / [lindex $cbbox 2] / 2.0 ]
      set yloc [expr double( [lindex $bbox1 1]+[lindex $bbox1 3]) \
	    / [lindex $cbbox 3] / 2.0 ]
      set xv [.pow.pow xview]
      if {$xloc<[lindex $xv 0] || $xloc>[lindex $xv 1]} {
	 .pow.pow xview moveto [expr double([lindex $bbox1 0]-30) \
	       / [lindex $cbbox 2] ]
      }
      set yv [.pow.pow yview]
      if {$yloc<[lindex $yv 0] || $yloc>[lindex $yv 1]} {
	 .pow.pow yview moveto [expr double([lindex $bbox1 1]-30) \
	       / [lindex $cbbox 3] ]
      }

      if { $gn != $currgn } {
	 # Place a "working" message on graph, update screen, then continue
	 set gMidX [expr 0.5*($graphWidth  * $xmagstep) + $xo]
	 set gMidY [expr 0.5*($graphHeight * $ymagstep) + $yo]
	 .pow.pow create text $gMidX $gMidY \
	       -anchor center -tags deleteMe -text "Building graph..."
	 update idletasks
	 .pow.pow delete deleteMe
      }

      .pow.pow bind $gn <ButtonPress-1> "powSelectGraph $gn"
      .pow.pow bind graphSelect_$gn <ButtonPress-1> "powSelectGraph $gn"
   }

   powPlotImages $gn $images $canvas
   powPlotCurves $gn $curves $canvas
   if { $canvas==".pow.pow" \
	 && [lsearch -exact $powOrderedGraphList $gn]==-1 } {
      lappend powOrderedGraphList $gn
   }

   [gNotifications default] postMessage $gn graphHasFinishedDrawing
}

proc powSetCursor { crsr } {
#puts "powSetCursor start"
   global powSaveCursor

   if { $crsr == "reset" } {
      set crsr [lindex $powSaveCursor end]
      set powSaveCursor [lreplace $powSaveCursor end end]
   } else {
      lappend powSaveCursor [.pow.pow cget -cursor]
   }
   .pow configure -cursor $crsr
   .pow.pow configure -cursor $crsr
   catch {.pow.scope configure -cursor $crsr}
}

proc powOverlapTest {id {canvas .pow.pow}} {
#puts "powOverlapTest start"
    set bb [$canvas bbox $id]
    if {$bb != ""} {
	set olap [eval $canvas find overlapping $bb]
    } else {
	return 0
    }
    foreach item $olap {
	set tags [$canvas gettags $item]
	if {$item != $id && !([string match "*handle*" $tags ])} {
	    return 1
	}
    }
    return 0
}


proc powRedrawBox {gn {canvas .pow.pow}} {
#puts "powRedrawBox start"
    global powPlotParam currimg
    
    foreach el  [array names powPlotParam]  {
	set p1 [lindex [split $el ,] 0]
	set p2 [lindex [split $el ,] 1]
	if { $p2 == $gn } {
	    set $p1 $powPlotParam($p1,$p2)
	}
    }
    
#find corners of new box
    set x0 [lindex [$canvas coords ${gn}box] 0]
    set y0 [lindex [$canvas coords ${gn}box] 3]
    set ccoords [powGraphToCanvas $gn $xTop $yTop $canvas] 
    set x1 [lindex $ccoords 0]
    set y1 [lindex $ccoords 1]
    
#remove previous box    
    $canvas delete ${gn}box

# plot the new box
    
    $canvas  create rectangle $x0 $y1 $x1 $y0 \
	    -tags "$gn gbox ${gn}box ${gn}line" -outline black

}


proc powChangeGrid { {redraw 0} } {
    global powPlotParam currgn

    if { $currgn=="powDef" } {return}

    if {$redraw} {
	powAdornGraph $currgn .pow.pow
    } else {
	.pow.pow itemconfig ${currgn}grid \
	      -fill $powPlotParam(GridColor,$currgn) \
	      -dash $powPlotParam(GridDash,$currgn)
    }
}

proc powContour { } {
#puts "powContour start"
    global currimg powRBmin powRBmax powbg powContourParam currgn
    global powDWP
    global g_titleFont

    if { ![info exists currimg] || $currimg=="NULL" || $currimg=="" } {
	tk_messageBox -message "Select a graph with an image first." \
		-title "No Image" -type ok -parent .pow
	return
    }

    set powContourParam(image)    $currimg
    set powContourParam(gn)       $currgn
    set powContourParam(res)      2
    set powContourParam(separate) no
    set powContourParam(nContrs)  10
    set lst [powGetTics $powRBmin($currimg) $powRBmax($currimg) 10 linear]
    set powContourParam(min)      [lindex $lst 0]
    set powContourParam(max)      [lindex $lst end]
    set powContourParam(scale)    linear

    if {[winfo exists ${powDWP}contour]} {destroy ${powDWP}contour}
    powToplevel ${powDWP}contour .pow "-bg $powbg"
    bind ${powDWP}contour <<CloseWindow>> "destroy ${powDWP}contour"

    catch {wm title ${powDWP}contour "Create Contours"}

    button ${powDWP}contour.help -text "Help" \
	    -command {powHelp Contours.html} \
	    -bg $powbg -takefocus 0 -font g_titleFont
    
    label ${powDWP}contour.image -bg $powbg -text "Image:" -font g_titleFont
    label ${powDWP}contour.currimg -bg yellow -fg black -text $currimg -font g_titleFont

    label ${powDWP}contour.imgrng -bg $powbg -text "Image Range:" -font g_titleFont
    frame ${powDWP}contour.imgfrm -bg $powbg
    label ${powDWP}contour.imgfrm.min -bg $powbg -width 10 \
	    -text "$powRBmin($currimg)" -font g_titleFont
    label ${powDWP}contour.imgfrm.dash -bg $powbg -text " - " -font g_titleFont
    label ${powDWP}contour.imgfrm.max -bg $powbg -width 10 \
	    -text "$powRBmax($currimg)" -font g_titleFont
    pack ${powDWP}contour.imgfrm.min -in ${powDWP}contour.imgfrm -side left \
	    -padx 4 -pady 1 -fill x -expand 1
    pack ${powDWP}contour.imgfrm.dash -in ${powDWP}contour.imgfrm -side left \
	    -padx 4 -pady 1
    pack ${powDWP}contour.imgfrm.max -in ${powDWP}contour.imgfrm -side left \
	    -padx 4 -pady 1 -fill x -expand 1

    label ${powDWP}contour.pixrng -bg $powbg -text "Contour Range:" -font g_titleFont
    frame ${powDWP}contour.pixfrm -bg $powbg
    entry ${powDWP}contour.pixfrm.min -bg $powbg -width 10 \
	    -textvariable powContourParam(min) -takefocus 1 -font g_titleFont
    label ${powDWP}contour.pixfrm.dash -bg $powbg -text " - " -font g_titleFont
    entry ${powDWP}contour.pixfrm.max -bg $powbg -width 10 \
	    -textvariable powContourParam(max) -takefocus 1 -font g_titleFont
    pack ${powDWP}contour.pixfrm.min -in ${powDWP}contour.pixfrm -side left \
	    -padx 4 -pady 1 -fill x -expand 1
    pack ${powDWP}contour.pixfrm.dash -in ${powDWP}contour.pixfrm -side left \
	    -padx 4 -pady 1
    pack ${powDWP}contour.pixfrm.max -in ${powDWP}contour.pixfrm -side left \
	    -padx 4 -pady 1 -fill x -expand 1

    label ${powDWP}contour.scale -bg $powbg -text "Scale:" -font g_titleFont
    frame ${powDWP}contour.sclbutt -bg $powbg
    radiobutton ${powDWP}contour.sclbutt.linear -bg $powbg -text Linear \
	    -variable powContourParam(scale) -value linear \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
    radiobutton ${powDWP}contour.sclbutt.sqrt -bg $powbg -text Sqrt \
	    -variable powContourParam(scale) -value sqrt \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
    radiobutton ${powDWP}contour.sclbutt.log -bg $powbg -text Log \
	    -variable powContourParam(scale) -value log \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
    pack ${powDWP}contour.sclbutt.linear -in ${powDWP}contour.sclbutt -side left \
	    -padx 4 -pady 1
    pack ${powDWP}contour.sclbutt.sqrt -in ${powDWP}contour.sclbutt -side left \
	    -padx 4 -pady 1
    pack ${powDWP}contour.sclbutt.log -in ${powDWP}contour.sclbutt -side left \
	    -padx 4 -pady 1

    label ${powDWP}contour.ncntrs -bg $powbg -text "# Contours:" -font g_titleFont
    frame ${powDWP}contour.ncntrsbutt -bg $powbg
    button ${powDWP}contour.ncntrsbutt.less -bg $powbg -text "<" \
	    -command { incr powContourParam(nContrs) -1 } -takefocus 0 -font g_titleFont
    entry  ${powDWP}contour.ncntrsbutt.numb -bg $powbg \
	    -textvariable powContourParam(nContrs) -width 5 -takefocus 1 -font g_titleFont
    button ${powDWP}contour.ncntrsbutt.more -bg $powbg -text ">" \
	    -command { incr powContourParam(nContrs) 1 } -takefocus 0 -font g_titleFont
    pack ${powDWP}contour.ncntrsbutt.less -in ${powDWP}contour.ncntrsbutt -side left \
	    -pady 1
    pack ${powDWP}contour.ncntrsbutt.numb -in ${powDWP}contour.ncntrsbutt -side left \
	    -pady 1
    pack ${powDWP}contour.ncntrsbutt.more -in ${powDWP}contour.ncntrsbutt -side left \
	    -pady 1
    trace variable powContourParam(nContrs) w { powSetContours }
    trace variable powContourParam(min) w { powSetContours }
    trace variable powContourParam(max) w { powSetContours }
    trace variable powContourParam(scale) w { powSetContours }
	    
    label ${powDWP}contour.clist -bg $powbg -text "Contours:" -font g_titleFont

    frame ${powDWP}contour.cntrs -bg $powbg
    scrollbar ${powDWP}contour.cntrs.scrolly -orient vertical -takefocus 0 \
	    -command {${powDWP}contour.cntrs.lst yview} -bg $powbg
    text ${powDWP}contour.cntrs.lst -bg $powbg -width 20 -height 5 \
	    -yscrollcommand {${powDWP}contour.cntrs.scrolly set } \
	    -takefocus 0 -font g_titleFont

    grid ${powDWP}contour.cntrs.lst     -in ${powDWP}contour.cntrs -row 1 -column 1 \
	    -sticky news
    grid ${powDWP}contour.cntrs.scrolly -in ${powDWP}contour.cntrs -row 1 -column 2 \
	    -sticky news
    grid rowconfigure    ${powDWP}contour.cntrs 1 -weight 1
    grid columnconfigure ${powDWP}contour.cntrs 1 -weight 1

    label ${powDWP}contour.res -bg $powbg -text "Resolution:" -font g_titleFont
    frame ${powDWP}contour.resbutt -bg $powbg
    radiobutton ${powDWP}contour.resbutt.high -bg $powbg -text High \
	    -variable powContourParam(res) -value 1 -highlightthickness 0 \
	    -takefocus 0 -font g_titleFont
    radiobutton ${powDWP}contour.resbutt.med -bg $powbg -text Medium \
	    -variable powContourParam(res) -value 2 -highlightthickness 0 \
	    -takefocus 0 -font g_titleFont
    radiobutton ${powDWP}contour.resbutt.low -bg $powbg -text Low \
	    -variable powContourParam(res) -value 3 -highlightthickness 0 \
	    -takefocus 0 -font g_titleFont
    pack ${powDWP}contour.resbutt.high -in ${powDWP}contour.resbutt -side left \
	    -padx 4 -pady 1
    pack ${powDWP}contour.resbutt.med -in ${powDWP}contour.resbutt -side left \
	    -padx 4 -pady 1
    pack ${powDWP}contour.resbutt.low -in ${powDWP}contour.resbutt -side left \
	    -padx 4 -pady 1

    checkbutton ${powDWP}contour.separate -bg $powbg \
	    -text "Place contours in separate graph" \
	    -variable powContourParam(separate) -onvalue yes -offvalue no \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont

    frame ${powDWP}contour.buttons -bg $powbg
    button ${powDWP}contour.buttons.make -text "Make Contours" -bg $powbg \
	    -command { 
	powMakeContours $powContourParam(image) \
			[${powDWP}contour.cntrs.lst get 1.0 end] \
			$powContourParam(res)
    } -font g_titleFont
    button ${powDWP}contour.buttons.exit -text "Exit" -bg $powbg \
	    -command {destroy ${powDWP}contour} -font g_titleFont
    pack ${powDWP}contour.buttons.make -in ${powDWP}contour.buttons -side left \
	    -padx 4 -pady 3
    pack ${powDWP}contour.buttons.exit -in ${powDWP}contour.buttons -side left \
	    -padx 4 -pady 3

    grid ${powDWP}contour.help -in ${powDWP}contour -row 0 -column 2 -sticky ne
    grid ${powDWP}contour.image -in ${powDWP}contour -row 0 -column 0 -sticky e \
	    -pady 8
    grid ${powDWP}contour.currimg -in ${powDWP}contour -row 0 -column 1 -sticky ew \
	    -pady 8 -padx 6

    grid ${powDWP}contour.imgrng -in ${powDWP}contour -row 2 -column 0 -sticky e
    grid ${powDWP}contour.imgfrm -in ${powDWP}contour -row 2 -column 1 -sticky ew \
	    -columnspan 2
    grid ${powDWP}contour.pixrng -in ${powDWP}contour -row 3 -column 0 -sticky e
    grid ${powDWP}contour.pixfrm -in ${powDWP}contour -row 3 -column 1 -sticky ew \
	    -columnspan 2
    grid ${powDWP}contour.ncntrs -in ${powDWP}contour -row 4 -column 0 -sticky e
    grid ${powDWP}contour.ncntrsbutt -in ${powDWP}contour -row 4 -column 1 -sticky w \
	    -padx 4
    grid ${powDWP}contour.scale -in ${powDWP}contour -row 5 -column 0 -sticky e
    grid ${powDWP}contour.sclbutt -in ${powDWP}contour -row 5 -column 1 -sticky w
    grid ${powDWP}contour.clist -in ${powDWP}contour -row 6 -column 0 -sticky e
    grid ${powDWP}contour.cntrs -in ${powDWP}contour -row 6 -column 1 -sticky news \
	    -padx 5 -columnspan 2
    grid ${powDWP}contour.res -in ${powDWP}contour -row 8 -column 0 -sticky e
    grid ${powDWP}contour.resbutt -in ${powDWP}contour -row 8 -column 1 -sticky w \
	    -pady 8
    grid ${powDWP}contour.separate -in ${powDWP}contour -row 10 -column 0 -sticky ew \
	    -padx 6 -pady 8 -columnspan 3

    grid ${powDWP}contour.buttons -in ${powDWP}contour -row 11 -column 0 -columnspan 3 \
	    -pady 8

    grid columnconfigure ${powDWP}contour 1 -weight 1
    grid rowconfigure ${powDWP}contour 0 -weight 1
    grid rowconfigure ${powDWP}contour 1 -minsize 10
    grid rowconfigure ${powDWP}contour 7 -minsize 10
    grid rowconfigure ${powDWP}contour 6 -weight 1
    grid rowconfigure ${powDWP}contour 11 -weight 1

    powSetContours 0 0 0
}

proc powGetScale { min max scale nlvls } {
#puts "powGetScale start"
    global powDWP
    set offset 0.0
    if { $min<0.0 } {
	set offset [expr -2.0*$min]
	set min [expr $min+$offset]
	set max [expr $max+$offset]
    }
    set min [expr double($min)]
    set max [expr double($max)]
    set list ""

    switch $scale {
	linear {
	    set step [expr ($max-$min) / ($nlvls-1) ]
	    set val $min
	    for {set i 0} {$i<$nlvls} {incr i} {
		lappend list [expr $val-$offset]
		set val [expr $val + $step]
	    }
	}
	sqrt {
	    set step [expr ( sqrt($max) - sqrt($min) ) / ($nlvls-1) ]
	    set val [expr sqrt($min)]
	    for {set i 0} {$i<$nlvls} {incr i} {
		lappend list [expr $val*$val-$offset]
		set val [expr $val + $step]
	    }
	}
	log {
	    if {$min==0.0} {set min [expr 0.001*$max]}
	    set step [expr log( $max / $min ) / ($nlvls-1) ]
	    set val [expr log($min)]
	    for {set i 0} {$i<$nlvls} {incr i} {
		lappend list [expr exp($val)-$offset]
		set val [expr $val + $step]
	    }
	}
	exp {}
    }
    return $list
}

proc powSetContours { a b c } {
#puts "powSetContours start"
    global powContourParam powDWP

    set nContrs $powContourParam(nContrs)
    if { $nContrs == "" } return
    if { !($nContrs > 2) } { set nContrs 2 }
    set powContourParam(list) ""

    set powContourParam(list) \
	    [powGetScale $powContourParam(min) $powContourParam(max) \
			 $powContourParam(scale) $nContrs]

    if { [winfo exists ${powDWP}contour.cntrs.lst] } {
	${powDWP}contour.cntrs.lst delete 1.0 end
	${powDWP}contour.cntrs.lst insert end [join $powContourParam(list) "\n"]
    }
}

proc powMakeContours { img list res } {
    global powContourParam powPlotParam
    global powWCS powCurveParam
    global powFitsHeader powFitsHeaderCnt xCount yCount
    global powWCSList powWCSLabel powWCSName
    global useWCSInfo
    global currgn

    set gn $powContourParam(gn)
    set cntr ${img}_contour
    catch { powDeleteGraph $cntr NOPROMPT }

    set useWCS false
    if { [info exists powWCS($img)] && $powWCS($img)!="" } {
	set powWCS($cntr) $powWCS($img)
        if { [lindex [lindex $powWCS($img) 0] 0] != 0.0 } {
           set useWCS true
        }
    }
   
    set powWCSList($cntr) $powWCSList($gn)
    set powWCSList(${cntr}scope) $powWCSList($gn)
    set powWCSName($cntr) $powWCSName($gn)
    set powWCSName(${cntr}scope) $powWCSName($gn)
    powCreateContour $cntr $img $list $res
    set powContourParam(list) $list
    set useWCSInfo($cntr) $fvPref::ifWCSInfo
    set useWCSInfo(${cntr}scope) $fvPref::ifWCSInfo
    set powWCSLabel(xlabel,$cntr,DEFAULT) $powWCSLabel(xlabel,$gn,DEFAULT)
    set powWCSLabel(ylabel,$cntr,DEFAULT) $powWCSLabel(ylabel,$gn,DEFAULT)
    set powWCSLabel(xunit,$cntr,DEFAULT)  $powWCSLabel(xunit,$gn,DEFAULT)
    set powWCSLabel(yunit,$cntr,DEFAULT)  $powWCSLabel(yunit,$gn,DEFAULT)

    if { $powContourParam(separate) == "yes" } {
	set graph $cntr
	set images NULL
	set curves $cntr
        set powFitsHeader($cntr) $powFitsHeader($gn)
        set powFitsHeader(${cntr}scope) $powFitsHeader($gn)
        set powFitsHeaderCnt($cntr) $powFitsHeaderCnt($gn)
        set powFitsHeaderCnt(${cntr}scope) $powFitsHeaderCnt($gn)
	set powWCS(${cntr}scope) $powWCS($cntr)
        if { $useWCS == "true" } {
           set powPlotParam(graphType,$cntr) $powPlotParam(graphType,$gn)
           set powPlotParam(graphType,${cntr}scope) $powPlotParam(graphType,$cntr)
        } else {
           set powPlotParam(graphType,$cntr) "binary"
           set powPlotParam(graphType,${cntr}scope) "binary"
           set powPlotParam(graphType,$gn) "binary"
        }

        set powPlotParam(zoomed,$cntr) $powPlotParam(zoomed,$gn)
        set powPlotParam(zoomed,${cntr}scope) $powPlotParam(zoomed,$cntr)
        set xCount($cntr) $xCount($gn)
        set yCount($cntr) $yCount($gn)
        set xCount(${cntr}scope) $xCount($cntr)
        set yCount(${cntr}scope) $yCount($cntr)
    } else {
	set graph $gn
	set images $powPlotParam(images,$gn)
	set curves $powPlotParam(curves,$gn)
	if {$curves=="NULL"} {
	    set curves $cntr
	} else {
	    lappend curves $cntr
	}
    }

    # Find the true width of the of the graph box
    set width  [tagXdim .pow.pow ${gn}box]
    set height [tagYdim .pow.pow ${gn}box]

    if { [lsearch -exact [powListGraphs] $graph]>=0 } {
       powUnmapGraph $graph
    }

    set powCurveParam(lStyle${cntr},$graph) " "
    set powCurveParam(lDisp${cntr},$graph) Yes
    set powCurveParam(pDisp${cntr},$graph) No

    set fixed $powPlotParam(FixedAspect,$gn)

    powCreateGraph $graph $curves $images \
	    $powPlotParam(xunits,$gn) $powPlotParam(yunits,$gn) \
	    $powPlotParam(xlabel,$gn) $powPlotParam(ylabel,$gn) \
	    $width $height \
	    $powPlotParam(xBot,$gn) $powPlotParam(yBot,$gn) \
	    $powPlotParam(xTop,$gn) $powPlotParam(yTop,$gn)

    set powPlotParam(FixedAspect,$graph) $fixed
}

proc powAdornGraph {gn {canvas ".pow.pow"}} {
   global powPlotParam

   if {$canvas != ".pow.pow" || $gn=="powDef" } {return}

   foreach par [list xNumTicks yNumTicks GridColor GridDash GridLines \
		     xTickScal yTickScal tickLabels] {
      set $par $powPlotParam($par,$gn)
   }

   if { $tickLabels=="degrees" && [powWCSexists $gn] } {
      # Convert "wcs" scaling to ra/dec to distinguish x/y axes
      set xTickScal "ra"
      set yTickScal "dec"
   }

   #  Make tick frequency non-linear
   set xNumTicks [expr $xNumTicks + int(exp($xNumTicks/3.0)) - 1]
   set yNumTicks [expr $yNumTicks + int(exp($yNumTicks/3.0)) - 1]

   .pow.pow delete ${gn}grid
   set powPlotParam(tickList,$gn) \
	 [powDrawGridLines $gn $canvas $xTickScal $yTickScal \
			   $GridColor $xNumTicks $yNumTicks $GridDash \
			   $GridLines ]

#puts "powPlotParam(tickList,$gn): $powPlotParam(tickList,$gn)"

   powDrawTicks $gn $canvas

   powMakeGraphLabels $gn
}

proc powDrawTicks { gn {canvas .pow.pow} } {
   global powPlotParam powFontParam
   global powTicksPerAxis
   global xCount yCount

   foreach par [list xTickLength xLabelTicks yTickLength yLabelTicks \
	 xmargin xTickScal yTickScal tickLabels tickFormatCmdX \
		   tickFormatCmdY] {
      set $par $powPlotParam($par,$gn)
   }
   set sideOrder [list lft rgt top bot]

   if { $tickLabels=="degrees" && [powWCSexists $gn] } {
      set xTickScal "ra"
      set yTickScal "dec"
   }

   .pow.pow delete ${gn}ticks ${gn}nums ${gn}label

   foreach axis [list x y] {
      foreach side [list top lft rgt bot none] {
	 set powTicksPerAxis($axis$side,$gn) 0
      }
   }

   ################################################################
   #
   #  Analyze tick values to identify required precision on labels
   #

   set xValues {}
   set yValues {}
   foreach {x y val axis side} $powPlotParam(tickList,$gn) {
      if { $axis=="x" } {
	 lappend xValues $val
      } elseif { $axis=="y" } {
	 lappend yValues $val
      }
   }

   set xValues [lsort -unique -real $xValues]
   set xLabelFmt [powBuildAxisFormat $xValues $xTickScal \
	 $powPlotParam(tickFormatCmdX,$gn)]

   set yValues [lsort -unique -real $yValues]
   set yLabelFmt [powBuildAxisFormat $yValues $yTickScal \
	 $powPlotParam(tickFormatCmdY,$gn)]

   #
   #
   ################################################################

   #set direction "U"
   set newTickList $powPlotParam(tickList,$gn)

   #foreach {x y val axis side} $powPlotParam(tickList,$gn)
   #   incr powTicksPerAxis($axis$side,$gn)

   foreach {x y val axis side} $powPlotParam(tickList,$gn) {
      incr powTicksPerAxis($axis$side,$gn)
      foreach {x y} [powGraphToCanvas $gn $x $y $canvas] {}
      if {$axis=="x"} {
	 if {$xTickScal=="ra"} {
	    set label [powHourRA $val $xLabelFmt]
	    if { [llength $xValues]<2 } {
	       # string will be of format xxhxxmxx.xxxxs
	       regsub {\.*0*s$} $label "s" label
	       regsub {00s$} $label "" label
	    } else {
	       regsub {X.*$} $label "" label
	    }
	 } elseif {$xTickScal=="log"} {
	    set label [eval $xLabelFmt [expr pow(10.0,$val)] ]
	 } else {
	    set label [eval $xLabelFmt $val ]
	 }
      } elseif {$axis=="y"} {
	 if {$yTickScal=="dec"} {
	    set label [powDegDec $val $yLabelFmt]
	    if { [llength $yValues]<2 } {
	       # string will be of format xx:xx:xx.xxxx
	       regsub {(:00)?\.*0*$} $label "" label
	    } else {
	       regsub {X.*$} $label "" label
	    }
	 } elseif {$yTickScal=="log"} {
	    set label [eval $yLabelFmt [expr pow(10.0,$val)] ]
	 } else {
	    set label [eval $yLabelFmt $val ]
	 }   
      }

      switch $side {
	 lft {
	    set tckLen [eval lindex \$${axis}TickLength 0]
	    set tckLab [eval lindex \$${axis}LabelTicks 0]
	    if { $tckLen != 0 } {
	       $canvas create line $x $y [expr $x - $tckLen] $y \
	      -tags "$gn ${gn}line ${gn}ticks ${gn}lftticks ${gn}${axis}ticks" \
	      -fill black
	    }
	    if { $tckLab } {
	       $canvas create text [expr $x - 5 - ($tckLen>0?$tckLen:0)] $y \
	      -text $label -anchor e -font [powGetFontList $gn tick] \
	      -fill $powFontParam(tickColor,$gn) \
	      -tags "$gn ${gn}text ${gn}nums ${gn}lftnums ${gn}${gn}nums"
	    }
	 }
	 rgt {
	    set tckLen [eval lindex \$${axis}TickLength 1]
	    set tckLab [eval lindex \$${axis}LabelTicks 1]
	    if { $tckLen != 0 } {
	       $canvas create line $x $y [expr $x + $tckLen] $y \
	      -tags "$gn ${gn}line ${gn}ticks ${gn}rgtticks ${gn}${axis}ticks" \
	      -fill black
	    }
	    if { $tckLab } {
	       $canvas create text [expr $x + 5 + ($tckLen>0?$tckLen:0)] $y \
	      -text $label -anchor w -font [powGetFontList $gn tick] \
	      -fill $powFontParam(tickColor,$gn) \
	      -tags "$gn ${gn}text ${gn}nums ${gn}rgtnums ${gn}${gn}nums"
	    }
	 }
	 top {
	    set tckLen [eval lindex \$${axis}TickLength 2]
	    set tckLab [eval lindex \$${axis}LabelTicks 2]
	    if { $tckLen != 0 } {
	       $canvas create line $x $y $x [expr $y - $tckLen] \
	      -tags "$gn ${gn}line ${gn}ticks ${gn}topticks ${gn}${axis}ticks" \
	      -fill black
	    }
	    if { $tckLab } {
	       $canvas create text $x [expr $y - 5 - ($tckLen>0?$tckLen:0)] \
	      -text $label -anchor s -font [powGetFontList $gn tick] \
	      -fill $powFontParam(tickColor,$gn) \
	      -tags "$gn ${gn}text ${gn}nums ${gn}topnums ${gn}${gn}nums"
	    }
	 }
	 bot {
	    set tckLen [eval lindex \$${axis}TickLength 3]
	    set tckLab [eval lindex \$${axis}LabelTicks 3]
	    if { $tckLen != 0 } {
	       $canvas create line $x $y $x [expr $y + $tckLen] \
	      -tags "$gn ${gn}line ${gn}ticks ${gn}botticks ${gn}${axis}ticks" \
	      -fill black
	    }
	    if { $tckLab } {
	       $canvas create text $x [expr $y + 5 + ($tckLen>0?$tckLen:0)] \
	      -text $label -anchor n -font [powGetFontList $gn tick] \
	      -fill $powFontParam(tickColor,$gn) \
	      -tags "$gn ${gn}text ${gn}nums ${gn}botnums ${gn}${axis}nums"
	    }
	 }
      }
   }

#   .pow.pow bind ${gn}nums <Enter> \
#         ".pow.pow itemconfigure ${gn}nums -fill yellow"
#   .pow.pow bind ${gn}nums <Leave> \
#         ".pow.pow itemconfigure ${gn}nums -fill black"
   .pow.pow bind ${gn}nums <<DblBtnPress>> \
	 "powEditGraphDlg $gn; powEditSelectPage Ticks"
}


proc powMakeGraphLabels { gn {canvas ".pow.pow"} } {
   global powPlotParam powFontParam
   global powTicksPerAxis

   foreach par [list xTickLength xLabelTicks yTickLength yLabelTicks \
	 xlabel ylabel xunits yunits titleString titlePosition titleAnchor \
	 xmargin xTickScal yTickScal tickLabels] {
      set $par $powPlotParam($par,$gn)
   }

   foreach [list lft top rgt bot] [$canvas coords ${gn}box] {}

# put the X and Y labels 

   if { $xunits=="" || [regexp -nocase NULL $xunits] } {
      set xString "$xlabel"     
   } else {
      set xString "$xlabel ($xunits)"     
   }
   if { $yunits=="" || [regexp -nocase NULL $yunits] } {
      set yString "$ylabel"     
   } else {
      set yString "$ylabel ($yunits)"     
   }

   # Should we swap the Axis labels?
   if { [powWCSisSwapped $gn] && \
	 $powTicksPerAxis(xlft,$gn) < $powTicksPerAxis(xbot,$gn) && \
	 $powTicksPerAxis(ylft,$gn) > $powTicksPerAxis(ybot,$gn) } {
      set tmp     $xString
      set xString $yString
      set yString $tmp
   } elseif { ![powWCSisSwapped $gn] && \
	 $powTicksPerAxis(xlft,$gn) > $powTicksPerAxis(xbot,$gn) && \
	 $powTicksPerAxis(ylft,$gn) < $powTicksPerAxis(ybot,$gn) } {
      set tmp     $xString
      set xString $yString
      set yString $tmp
   }
	 
   
   set lineSpace [font metrics [powGetFontList $gn axis] -linespace]
   incr lineSpace 5
   set topMarg [powMax [lindex $xTickLength 2] [lindex $yTickLength 2]]
   set botMarg [powMax [lindex $xTickLength 3] [lindex $yTickLength 3]]
   if { $botMarg<0 } {set botMarg 0}
   if { $topMarg<0 } {set topMarg 0}

   if [regexp {[^ ]} $xString] {
      $canvas create text [expr ($lft + $rgt)/2 ] \
	    [expr $bot + $botMarg + $lineSpace] -text $xString -anchor n \
	    -tags "$gn ${gn}label ${gn}xlabel ${gn}text" \
	    -font [powGetFontList $gn axis] \
	    -fill $powFontParam(axisColor,$gn)
   }

   if [regexp {[^ ]} $yString] {
      $canvas create text [expr $lft - $xmargin/2] \
	    [expr $top - $topMarg] -text $yString -anchor sw \
	    -justify left -tags "$gn ${gn}label ${gn}ylabel ${gn}text"\
	    -font [powGetFontList $gn axis] \
	    -fill $powFontParam(axisColor,$gn)
   }

# Now do the titleString

   # identical file name handler
   set titleStrToken [split $titleString "_"]
   if { [llength $titleStrToken] > 1 } {
      set titleStrToken [lreplace $titleStrToken end end]
      set titleString [lindex $titleStrToken 0]
      for {set i 1} {$i < [llength $titleStrToken]} {incr i} {
          set titleString [format "%s_%s" $titleString [lindex $titleStrToken $i]]
      }
   }

   if [regexp {[^ ]} $titleString] {

      if [string match "*w*" $titlePosition] {
	 set x $lft
      } elseif [string match "*e*" $titlePosition] {
	 set x $rgt
      } else {
	 set x [expr ($lft + $rgt)*0.5]
      }

      if [string match "*n*" $titlePosition] {
	 set y [expr $top - $topMarg]
	 if { [lindex $xLabelTicks 2] || [lindex $yLabelTicks 2] } {
	    set y [expr $y - $lineSpace]
	 }
	 if [regexp {[^ ]} $yString] {
	    set y [expr $y - $lineSpace]
	 }
      } elseif [string match "*s*" $titlePosition] {
	 set y [expr $bot + $botMarg]
	 if { [lindex $xLabelTicks 3] || [lindex $yLabelTicks 3] } {
	    set y [expr $y + $lineSpace]
	 }
	 if [regexp {[^ ]} $xString] {
	    set y [expr $y + $lineSpace]
	 }
      } else {
	 set y [expr ($top + $bot)*0.5]
      }

      $canvas create text $x $y -anchor $titleAnchor -text $titleString \
	    -tags "$gn graphDragable ${gn}label ${gn}tlabel ${gn}text" \
	    -font [powGetFontList $gn title] \
	    -fill $powFontParam(titleColor,$gn)
   }

#   $canvas bind ${gn}label <Enter> \
#         "$canvas itemconfigure ${gn}label -fill yellow"
#   $canvas bind ${gn}label <Leave> \
#         "$canvas itemconfigure ${gn}label -fill black"
   $canvas bind ${gn}label <<DblBtnPress>> \
	 "powEditGraphDlg $gn; powEditSelectPage Graph"

   # Now do any extra graph labels

   powRedrawNotes $gn
}


proc powDummyRangeCallback { gn x0 x1} {
#puts "powDummyRangeCallback start"
    puts "You have selected the ordered pair: ( $x0 , $x1) on the graph $gn"
}


proc powDragRange { x_or_y {tag highlight} {color red} {callback powDummyRangeCallback}} {
#puts "powDragRange start"
    global currgn powRangeX0 powRangeX1 powRangeX0C powRangeXC 
    global powRangeY0 powRangeY1 powRangeY0C powRangeYC powRangeTag powRangeColor
    global powRangeCallback  powRangeSaveBinding
    
    set powRangeCallback $callback
    set powRangeTag $tag
    set powRangeColor $color
    
    set powRangeSaveBinding(ButtonPress-1) [bind .pow.pow <ButtonPress-1>]
    set powRangeSaveBinding(B1-Motion) [bind .pow.pow <B1-Motion>]
    set powRangeSaveBinding(ButtonRelease-1) [bind .pow.pow <ButtonRelease-1>]
    
    bind .pow.pow <ButtonPress-1> {
	set gn [powWhereAmI %x %y]; 
	if {$gn == $currgn} {
	    set powRangeX0C [.pow.pow canvasx %x];
	    set powRangeY0C [.pow.pow canvasy %y];
	    set gcoords [powCanvasToGraph $currgn $powRangeX0C $powRangeY0C \
			     .pow.pow];
	    set powRangeX0 [lindex gcoords 0]
	    set powRangeY0 [lindex gcoords 1]
	    .pow.pow create line $powRangeX0C $powRangeY0C $powRangeX0C $powRangeY0C \
		-tags Range -fill $powRangeColor
	}
    }
    bind .pow.pow <B1-Motion> {
	set gn [powWhereAmI %x %y]; 
	if {$gn == $currgn} {
	    if {![info exists powRangeX0C]} {
		set powRangeX0C [.pow.pow canvasx %x];
		set powRangeY0C [.pow.pow canvasy %y];
		set gcoords [powCanvasToGraph $currgn $powRangeX0C $powRangeY0C \
			     .pow.pow];
		set powRangeX0 [lindex gcoords 0]
		set powRangeY0 [lindex gcoords 1]
	    } else {
		.pow.pow delete Range; 
	    }
	    set powRangeXC [.pow.pow canvasx %x];
	    set powRangeYC [.pow.pow canvasy %y];
	    .pow.pow create line $powRangeX0C $powRangeY0C $powRangeXC $powRangeYC \
		-tags Range -fill $powRangeColor
	}
    }
    if {$x_or_y == "X"} { 
	bind .pow.pow <ButtonRelease-1> {
	    if {[info exists powRangeX0C]} {
		set range_coords [.pow.pow coords Range]
		.pow.pow delete Range
		set powRangeXC [lindex $range_coords 0];
		set powRangeYC [lindex $range_coords 1];
		set powRangeCoords [powCanvasToGraph $currgn \
			       $powRangeXC $powRangeYC .pow.pow]
		set powRangeX0 [lindex $powRangeCoords 0]
		set powRangeY0 [lindex $powRangeCoords 1]
		set powRangeXC [lindex $range_coords 2];
		set powRangeYC [lindex $range_coords 3];
		set powRangeCoords [powCanvasToGraph $currgn \
			       $powRangeXC $powRangeYC .pow.pow]
		set powRangeX1 [lindex $powRangeCoords 0]
		set powRangeY1 [lindex $powRangeCoords 1]
		powColorRange $currgn X $powRangeX0 $powRangeX1 $powRangeY0 $powRangeY1 $powRangeTag $powRangeColor 0;
		$powRangeCallback $currgn $powRangeX0 $powRangeX1; 
		bind .pow.pow <ButtonPress-1> \
		    "$powRangeSaveBinding(ButtonPress-1)";
		bind .pow.pow <B1-Motion> \
		    "$powRangeSaveBinding(B1-Motion)";
		bind .pow.pow <ButtonRelease-1> \
		    "$powRangeSaveBinding(ButtonRelease-1)";
		#clear start point for next Drag
		unset powRangeX0C
	    }
	}
    } else {
	bind .pow.pow <ButtonRelease-1> {
	    if {[info exists powRangeX0C]} {
		set range_coords [.pow.pow coords Range]
		.pow.pow delete Range
		set powRangeXC [lindex $range_coords 0];
		set powRangeYC [lindex $range_coords 1];
		set powRangeCoords [powCanvasToGraph $currgn \
			       $powRangeXC $powRangeYC .pow.pow]
		set powRangeX0 [lindex $powRangeCoords 0]
		set powRangeY0 [lindex $powRangeCoords 1]
		set powRangeXC [lindex $range_coords 2];
		set powRangeYC [lindex $range_coords 3];
		set powRangeCoords [powCanvasToGraph $currgn \
			       $powRangeXC $powRangeYC .pow.pow]
		set powRangeX1 [lindex $powRangeCoords 0]
		set powRangeY1 [lindex $powRangeCoords 1]
		powColorRange $currgn Y $powRangeX0 $powRangeX1 $powRangeY0 $powRangeY1 $powRangeTag $powRangeColor 0;
		$powRangeCallback $currgn $powRangeY0 $powRangeY1; 
		bind .pow.pow <ButtonPress-1> \
		    "$powRangeSaveBinding(ButtonPress-1)";
		bind .pow.pow <B1-Motion> \
		    "$powRangeSaveBinding(B1-Motion)";
		bind .pow.pow <ButtonRelease-1> \
		    "$powRangeSaveBinding(ButtonRelease-1)";
		#clear start point for next Drag
		unset powRangeX0C
	    }
	}
    }
}

proc powColorRange { gn x_or_y x0 x1 y0 y1 {tag highlight} {color red} {redrawing 0}} {
#puts "powColorRange start"
    global powGraphsTagRangeList powTagsColorMap
    global chainToAxisHash axisToChainHash
#    puts "powColorRange: $gn $x_or_y $x0 $x1 $y0 $y1 $tag $color $redrawing"

    if {$x_or_y == "X"} {
	set a0 $x0
	set a1 $x1
    } else {
	set a0 $y0
	set a1 $y1
    }



    if {$a0 > $a1} {
	set tmp $a0
	set a0 $a1
	set a1 $tmp
    }
    if {!$redrawing} {
	set powTagsColorMap($tag) $color
	lappend powGraphsTagRangeList($gn) "$x_or_y $a0 $a1 $tag"
    }	

    set graphlist ${gn}$x_or_y


    if {$x_or_y =="X"} {
	if {[array names axisToChainHash ${gn}X] != ""} {
	    set graphlist [concat $graphlist $chainToAxisHash($axisToChainHash(${gn}X))]
	}
    } else {
	if {[array names axisToChainHash ${gn}Y] != ""} {
	    set graphlist [concat $graphlist $chainToAxisHash($axisToChainHash(${gn}Y))]
	}
    }
    

    foreach graph [concat $graphlist] {
	set axis [chopped $graph]
	set graph [chop $graph]
	powTagRange $graph  $x_or_y $x0 $x1 $y0 $y1 $tag
    }

    .pow.pow itemconfigure $tag -fill $color
}


proc powTagRange { gn x_or_y x0 x1 y0 y1 tag } {
#puts "powTagRange start"
#    puts "powTagRange: $gn $x_or_y $x0 $x1 $tag"
    set gbox [.pow.pow coords ${gn}box]
    set gx0 [lindex $gbox 0]
    set gx1 [lindex $gbox 2]
    set gy0 [lindex $gbox 1]
    set gy1 [lindex $gbox 3]
    if {$x_or_y == "X"} {
	set xa [lindex [powGraphToCanvas $gn $x0 $y0 .pow.pow] 0]
	set xb [lindex [powGraphToCanvas $gn $x1 $y1 .pow.pow] 0]
	if {($xa < $gx0 && $xb < $gx0) || ($xa > $gx1 && $xb > $gx1)} {
	    #range is entirely off the graph
	    return
	} 
	if {$xa < $gx0} {set xa $gx0 }
	if {$xb > $gx1} {set xb $gx1 }
	set ya $gy0
	set yb $gy1
    } else {
	set xa $gx0
	set xb $gx1
	set ya [lindex [powGraphToCanvas $gn $x1 $y1 .pow.pow] 1]
	set yb [lindex [powGraphToCanvas $gn $x0 $y0 .pow.pow] 1]
	if {($ya < $gy0 && $yb < $gy0) || ($ya > $gy1 && $yb > $gy1)} {
	    #range is entirely off the graph
	    return
	} 
	if {$ya < $gy0} {set ya $gy0 }
	if {$yb > $gy1} {set yb $gy1 }
    }
    .pow.pow addtag $tag enclosed $xa $ya $xb $yb
}

proc powTagRect { gn x0 y0 x1 y1 tag } {
#puts "powTagRect start"
    if {$x0 > $x1} {
	set tmp $x0
	set x0 $x1
	set x1 $tmp
    }
    if {$y0 > $y1} {
	set tmp $y0
	set y0 $y1
	set y1 $tmp
    }
    set ccoords [powGraphToCanvas $gn $x0 $y0 .pow.pow]
    set xa [lindex $ccoords 0]
    set yb [lindex $ccoords 1]
    set ccoords [powGraphToCanvas $gn $x1 $y1 .pow.pow]
    set xb [lindex $ccoords 0]
    set ya [lindex $ccoords 1]
    set gbox [.pow.pow coords ${gn}box]
    set gx0 [lindex $gbox 0]
    set gx1 [lindex $gbox 2]
    set gy0 [lindex $gbox 1]
    set gy1 [lindex $gbox 3]
    if {($xa < $gx0 && $xb < $gx0) || ($xa > $gx1 && $xb > $gx1) || \
	    ($ya < $gy0 && $yb < $gy0) || ($ya > $gy1 && $yb > $gy1)} {
#rect is entirely off the displayed graph
	return
    } 
    if {$xa < $gx0} {set xa $gx0 }
    if {$xb > $gx1} {set xb $gx1 }
    if {$ya < $gy0} {set ya $gy0 }
    if {$yb > $gy1} {set yb $gy1 }
    
    .pow.pow  addtag $tag enclosed $xa $ya $xb $yb
}





proc powDummyRectCallback { gn  x0 y0 x1 y1} {
#puts "powDummyRectCallback start"
    puts "You have selected the rectangle: ( $x0 , $y0 , $x1, $y1) on the graph "
}


proc powDragRect { {tag highlight} {color red} {callback powDummyRectCallback}} {
#puts "powDragRect start"
    global currgn powRectX0 powRectX1 powRectX0C powRectXC 
    global powRectY0 powRectY1 powRectY0C powRectYC powRectTag powRectColor
    global powRectCallback powRectSaveBinding

    set powRectCallback $callback
    set powRectTag $tag
    set powRectColor $color

    set powRectSaveBinding(ButtonPress-1) [bind .pow.pow <ButtonPress-1>]
    set powRectSaveBinding(B1-Motion) [bind .pow.pow <B1-Motion>]
    set powRectSaveBinding(ButtonRelease-1) [bind .pow.pow <ButtonRelease-1>]

    
    bind .pow.pow <ButtonPress-1> {
	set gn [powWhereAmI %x %y]; 
	if {$gn == $currgn} {
	    set powRectX0C [.pow.pow canvasx %x];
	    set powRectY0C [.pow.pow canvasy %y];
	    set gcoords [powCanvasToGraph $currgn $powRectX0C $powRectY0C .pow.pow];

	    set powRectX0 [lindex $gcoords 0]
	    set powRectY0 [lindex $gcoords 1]

	    .pow.pow create rectangle $powRectX0C $powRectY0C $powRectX0C $powRectY0C \
		-tags Rect -outline $powRectColor
	}
    }
    bind .pow.pow <B1-Motion> {
	set gn [powWhereAmI %x %y]; 
	if {$gn == $currgn} {
	    if {![info exists powRectX0C]} {
		set powRectX0C [.pow.pow canvasx %x];
		set powRectX0 [powCanvasToGraph $currgn X $powRectX0C .pow.pow];
		set powRectY0C [.pow.pow canvasy %y];
		set powRectY0 [powCanvasToGraph $currgn Y $powRectY0C .pow.pow];
	    } else {
		.pow.pow delete Rect; 
	    }
	    set powRectXC [.pow.pow canvasx %x];
	    set powRectYC [.pow.pow canvasy %y];
	    .pow.pow create rectangle $powRectX0C $powRectY0C $powRectXC $powRectYC \
		-tags Rect -outline $powRectColor
	}
    }
    bind .pow.pow <ButtonRelease-1> {
	if {[info exists powRectX0C]} {
	    set rect_coords [.pow.pow coords Rect]
	    .pow.pow delete Rect
	    set powRectXC [lindex $rect_coords 0]
	    set powRectYC [lindex $rect_coords 1];
	    set gcoords [powCanvasToGraph $currgn $powRectXC $powRectYC .pow.pow];
	    set powRectX0 [lindex $gcoords 0]
	    set powRectY0 [lindex $gcoords 1]

	    set powRectXC [lindex $rect_coords 2]
	    set powRectYC [lindex $rect_coords 3];
	    set gcoords [powCanvasToGraph $currgn $powRectXC $powRectYC .pow.pow]
	    set powRectX1 [lindex $gcoords 0]
	    set powRectY1 [lindex $gcoords 1]
	    powColorRect $currgn $powRectX0 $powRectY0 $powRectX1 $powRectY1 $powRectTag $powRectColor 0;
	    bind .pow.pow <ButtonPress-1> \
		"$powRectSaveBinding(ButtonPress-1)";
	    bind .pow.pow <B1-Motion> \
		"$powRectSaveBinding(B1-Motion)";
	    bind .pow.pow <ButtonRelease-1> \
		"$powRectSaveBinding(ButtonRelease-1)";
	    $powRectCallback $currgn $powRectX0 $powRectY0 $powRectX1 $powRectY1;
	}
    }
}

proc powColorRect { gn  x0 y0 x1 y1 {tag highlight} {color red} {redrawing 0}} {
#puts "powColorRect start"
    global powGraphsTagRectList powTagsColorMap
#    puts "powColorRect: $gn $x_or_y $x0 $x1 $tag $color $redrawing"
    if {$x0 > $x1} {
	set tmp $x0
	set x0 $x1
	set x1 $tmp
    }
    if {!$redrawing} {
	set powTagsColorMap($tag) $color
	lappend powGraphsTagRectList($gn) "$x0 $y0 $x1 $y1 $tag"
    }	
    powTagRect $gn $x0 $y0 $x1 $y1 $tag

# Unless someone can come up with a good reason, powColorRect doesn't follow
# linked axes because it's unclear what to do with the unlinked axis

    .pow.pow itemconfigure $tag -fill $color
}

proc powColorbar { } {
    global currimg currgn powResizeMain
    global powPlotParam powImageParam
    global powFitsHeader powFitsHeaderCnt powWCS xCount yCount
    global powWCSList powWCSLabel powWCSName

    if { [regexp {_colorbar$} $currgn] } {
       tk_messageBox -icon warning \
	     -message "Cannot create colorbar of\nanother colorbar" \
	     -parent .pow -title "Colorbar Warning" -type ok
       return
    }
    if { ![info exists currimg] || $currimg == "" } {
       tk_messageBox -icon warning \
	     -message "Select an image first." \
	     -parent .pow -title "Colorbar Warning" -type ok
       return
    }

    set saveimg $currimg
    set savegn $currgn

    if {[.pow.pow find withtag ${currimg}disp${currgn}] == ""} {
	puts "Your selected image must be on the selected graph to make a colorbar"
	return
    }

    set colorbarGn  ${currgn}_colorbar
    set colorbarImg ${currimg}_colorbar

    set width 2048.0
    set min $powImageParam(RBmin${currimg},$currgn)
    set max $powImageParam(RBmax${currimg},$currgn)
    set width [expr $max - $min]
    if { $min==$max } {
       if { $min==0.0 } {
	  set min -1
	  set max 1
       } else {
	  set min [expr $min-abs(0.1*$min)]
	  set max [expr $max+abs(0.1*$max)]
       }
    }
    set increment [expr ($max - $min) / ($width-1)]

    set x $min
    for {set j 0} {$j < $width} {incr j} {
	lappend color_list $x
	set x [expr $x + $increment]
    }

    powCreateDataFromList $colorbarImg $color_list
    
    set zunits [powGetImageUnits $currimg Z]
    set powPlotParam(zoomed,$colorbarImg) 0
    set powWCS($colorbarImg) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
    set powPlotParam(graphType,$colorbarImg) "image"
    set powFitsHeader($colorbarImg) ""
    set powFitsHeaderCnt($colorbarImg) 0
    set xCount($colorbarImg) 0
    set yCount($colorbarImg) 0

    set powPlotParam(zoomed,${colorbarImg}scope) 0
    set powWCS(${colorbarImg}scope) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
    set powPlotParam(graphType,${colorbarImg}scope) "image"
    set powFitsHeader(${colorbarImg}scope) ""
    set powFitsHeaderCnt(${colorbarImg}scope) 0
    set xCount(${colorbarImg}scope) 0
    set yCount(${colorbarImg}scope) 0

    set powWCSName($colorbarImg) 0

    powCreateImage $colorbarImg $colorbarImg 0 0 \
	    [expr int($width)] 1 $min $increment 0.5 1.0 \
	    $zunits " " $zunits
    
    if [info exists powWCSList($colorbarImg)] {
       foreach name [lindex $powWCSList($colorbarImg) 1] {
          $fFile assembleWcsLabel $colorbarImg $name
       }
    } else {
       set powWCSList($colorbarImg) {}
       lappend powWCSList($colorbarImg) 1
       lappend powWCSList($colorbarImg) {}
    }

    set powWCSList(${colorbarImg}scope) $powWCSList($colorbarImg)
    set powPlotParam(xo,$colorbarGn) [lindex [.pow.pow bbox ${savegn}box] 0]
    set powPlotParam(yo,$colorbarGn) \
	  [expr 20 + [lindex [.pow.pow bbox $savegn] 3] ]
    set powPlotParam(Colorbar${colorbarImg},$colorbarGn) ${currimg}disp${currgn}
    set powPlotParam(FixedAspect,$colorbarGn) No
    set powPlotParam(FixedAspect,${colorbarGn}scope) No

    set powPlotParam(handletext,$colorbarGn) "$savegn Colorbar"
    set powPlotParam(handleposition,$colorbarGn) bl
    set powPlotParam(handleanchor,$colorbarGn) nw
    set powPlotParam(titleString,$colorbarGn) "$savegn Colorbar"
    set powPlotParam(titlePosition,$colorbarGn) sw
    set powPlotParam(titleAnchor,$colorbarGn) nw
    set powPlotParam(GridLines,$colorbarGn) No
    set powPlotParam(yTickLength,$colorbarGn) [list 0 0 0 0]
    set powPlotParam(yLabelTicks,$colorbarGn) [list No No No No]
    set powPlotParam(zoomed,$colorbarGn) 0
    set powPlotParam(graphType,$colorbarGn) "binary"
    set powFitsHeader($colorbarGn) ""
    set powFitsHeaderCnt($colorbarGn) 0
    set powWCS($colorbarGn) ""
    set powWCS($colorbarGn) {{0.0 0.0} {0.0 0.0} {1.0 -0.0 0.0 1.0} {{} {}} {{} {}}}
    set xCount($colorbarGn) 0
    set yCount($colorbarGn) 0
    powCreateGraph $colorbarGn NULL $colorbarImg $zunits NULL " " " " \
	    [tagXdim .pow.pow ${currgn}box] 20

    powReconfigureToplevel $powResizeMain


#reselect original image    
    powSelectGraph $savegn
    powSelectImage $savegn $saveimg
}
    

#  axisValues need to contain unique values in ascending order

proc powBuildAxisFormat { axisValues axisScale defaultFmt } {
#puts "powBuildAxisFormat start"

   if { [llength $axisValues]<2 } {
      set axisDiff 0
   } else {
      set axisDiff [expr [lindex $axisValues 1] - [lindex $axisValues 0]]
      for { set i 2 } { $i<[llength $axisValues] } { incr i } {
	 set j [expr $i-1]
	 set diff [expr [lindex $axisValues $i] - [lindex $axisValues $j]]
	 if { $diff < $axisDiff } {
	    set axisDiff $diff
	 }
      }
   }
   switch $axisScale {
      "ra" {
	 set labelFmt "%dh"
	 if { $axisDiff==0 } {
	    append labelFmt "%02dm%07.4fs"
	 } else {
	    set axisDiff [expr $axisDiff * 3600.0 / 15.0]
	    if { $axisDiff < 3600.0 } {
	       # Need Minutes
	       append labelFmt "%02dm"
	       if { $axisDiff < 59.99 } {
		  # Need Seconds
		  if { $axisDiff < .99 } {
		     set axisDiffPrec  [expr int(-log10($axisDiff)+1)]
		     set axisDiffWidth [expr 3+$axisDiffPrec]
		     append labelFmt "%0${axisDiffWidth}.${axisDiffPrec}fs"
		  } else {
		     append labelFmt "%02.0fs"
		  }
	       } else {
		  append labelFmt "X%02fs"
	       }
	    } else {
	       append labelFmt "X%02dm%02fs"
	    }
	 }
      }
      "dec" {
	 set labelFmt "%d:%02d"
	 if { $axisDiff==0 } {
	    append labelFmt ":%07.4f"
	 } else {
	    set axisDiff [expr $axisDiff * 3600.0]
	    if { $axisDiff < 59.99 } {
	       # Need Seconds
	       if { $axisDiff < 0.99 } {
		  set axisDiffPrec [expr int(-log10($axisDiff)+1)]
		  set axisDiffWidth [expr 3+$axisDiffPrec]
		  append labelFmt ":%0${axisDiffWidth}.${axisDiffPrec}f"
	       } else {
		  append labelFmt ":%02.0f"
	       }
	    } else {
	       append labelFmt "X:%02.0f"
	    }
	 }
      }
      "log" {
	 set labelFmt $defaultFmt
      }
      default {
	 set labelFmt $defaultFmt
      }
   }

   return $labelFmt
}

# convert a decimal degree to HH MM SS.S
# the optional fmtStr needs to have 3 value placeholders (%'s) in h m s order

proc powHourRA { deciValue {fmtStr "%dh%02dm%05.2fs"} } {
#puts "powHourRA start"
#Written by J. Xu
    if { $deciValue < 0} {
	set deciValue [expr $deciValue + 360]
    }
    set hourValue [expr $deciValue/15.0 + 1e-13]
    set hour [expr int($hourValue)]
    set minuValue [expr ($hourValue - $hour)*60.0]
    set minu [expr int($minuValue)]
    set scndValue [expr ($minuValue - $minu)*60.0]
    set scnd $scndValue
    while {$hour >= 24} {set hour [expr $hour - 24]}
    while {$hour < 0} {set hour [expr $hour + 24]}

    # Check if we are rounding seconds to next value

    set scndFmt [lindex [split $fmtStr %] 3]
    set scndStr [format %$scndFmt $scnd]
    if { [regexp {^ *60} $scndStr] } {
	set scnd 0
	incr minu
	if {$minu == 60} {
	   set minu 0
	   incr hour
	   if { $hour==24 } {
	      set hour 0
	   }
	}
    }
    return [format $fmtStr $hour $minu $scnd]
}

# convert a decimal degree to DD MM SS.S
# the optional fmtStr needs to have 3 value placeholders (%'s) in h m s order

proc powDegDec { deciValue {fmtStr "%d:%02d:%05.2f"} } {
#puts "powDegDec start"
#Written by J. Xu
    if { $deciValue < 0} {
	set isNeg 1
    } else {
	set isNeg 0
    }
    set deciValue [expr abs($deciValue) + 1e-13]
    set deg [expr int($deciValue)]
    while {$deg > 360} {set deg [expr $deg - 360]}
    while {$deg < -360} {set deg [expr $deg + 360]}
    set minuValue [expr ($deciValue - $deg)*60.0]
    set minu [expr int($minuValue)]
    set scndValue [expr ($minuValue - $minu)*60.0]
    set scnd $scndValue

    # Check if we are rounding seconds to next value

    set scndFmt [lindex [split $fmtStr %] 3]
    set scndStr [format %$scndFmt $scnd]
    if { [regexp {^ *60} $scndStr] } {
	set scnd 0
	incr minu
	if {$minu == 60} {
	   set minu 0
	   incr deg
	   if { $deg==360 } {
	      set deg 0
	   }
	}
    }
    if { $isNeg } {
	return [format "-$fmtStr" $deg $minu $scnd]
    } else {
	return [format $fmtStr $deg $minu $scnd]
    }
}

proc powSwitch2NewWCSHeader {} {
    global powFitsHeader powPlotParam powWCSInfo powWCS
    global currgn coordSel powWCSList powWCSName
    global powWCSLabel 

    set powPlotParam(wcsName,$currgn) $powPlotParam(wcsName,powDef)
    set selection $powPlotParam(wcsName,$currgn)
    set dest "DEFAULT"
    if { $selection == "WCS" } {
       set powWCSName($currgn) 0
       #set powFitsHeader($currgn) [assembleWcsHeader $currgn]
       set powWCS($currgn) $powWCSInfo($currgn,DEFAULT)
    } else {
       set dest [string toupper [string range $selection 3 end]]
       set coordList [lindex $powWCSList($currgn) 1]
       set powWCSName($currgn) "[lsearch -exact $coordList [string toupper $dest]]"
       
       #set powFitsHeader($currgn) [assembleWcsHeader $currgn $dest]
       set powWCS($currgn) $powWCSInfo($currgn,$dest)
    }

    if [info exist powWCSName(imgobj_$currgn)] {
       set powWCSName(imgobj_$currgn) $powWCSName($currgn)
       set powWCS(imgobj_$currgn) $powWCS($currgn)
    }

    # next to initialize the pow wcs structure in C
    set tokenNew [lindex $powWCSInfo($currgn,$dest) 1]
    set tokenNewX [lindex $tokenNew 0]
    set tokenNewY [lindex $tokenNew 1]
    powResetWcsStructure -g $currgn $tokenNewX $tokenNewY
    catch { powEndROI 1 }
    powAdornGraph $currgn .pow.pow
    powRedrawGraphHandles $currgn
    powRedrawScopebox
}

proc assembleWcsHeader { img {selection "default"} } {
   global powHeaderWcsKeyWord

   # regular header
   return [format "%s%s%s" $powHeaderWcsKeyWord($img,NONE) \
                           $powHeaderWcsKeyWord($img,[string toupper $selection]) \
                           $powHeaderWcsKeyWord($img,END)]
}

proc powLoadFitsImage {url imagename} {
#puts "powLoadFitsImage start"
    global powWCS
#While POW, technically, should not know anything about FITS files, 
#loading an image using fitsTcl is *way* too complicated at the moment
#so this routine is provided as a public service

#This routine takes a url or filename and creates the POW Image object
#with the requested name.  The POW Data object is available under the
#name ${imagename}_data

#fitsTcl must be loaded to use this routine and an error will be thrown
#if it isn't

    if {[lsearch [info loaded] *Fitstcl*] == -1} {
#	error "You must load fitsTcl to use powLoadFitsImage"
    }


#if you're running under Windows or MacOS, you can't use
#a URL, just a local file name because cfitsio's network drivers don't work
#outside of UNIX.
#open the fits file (readonly)    
    if [catch {set infilehandle [fits open $url 0]}] {
	error "Couldn't open file: $url"
    }

#load the image data into memory
set imghandle [$infilehandle load image]
    
#get the dimensions of the image
set dims [$infilehandle info imgdim]
set n1 [lindex $dims 0]
set n2 [lindex $dims 1]

#get the data type of the image 
set data_type [lindex [lindex [$infilehandle get keyword BITPIX] 0] 1]

#Now a bit of Voodoo to deal with possible wierd file types:

#If the image has BZERO or BSCALE keywords in the header, fitsTcl will
#do the appropriate thing with them automatically, but the datatype returned
#will be floating point doubles (isn't FITS fun:)
if { ([catch {$infilehandle get keyword BZERO}] == 0) ||
     ([catch {$infilehandle get keyword BSCALE}] == 0) } {
    set data_type 4
}

#make a POW DATA object

powCreateData ${imagename}_data $imghandle $data_type [expr $n1 * $n2] 0
# powCreateDataFlip ${imagename}_data $imghandle $data_type [expr $n1 * $n2] 1 X $n1 $n2


#make a POW IMAGE object; the units (pixels, intensity) are arbitrary; since
#this is a general application, we don't know what they are

powCreateImage $imagename ${imagename}_data 0 0 $n1 $n2 0 1 0 1 pixels pixels intensity

        powCreateGraph $imagename NULL $imagename NULL NULL x_label y_label $n1 $n2


#This will setup POW to use the Astronomical coordinate information
#in the file (if there is any) 
global powWCS
#puts "wcsString: $wcsString"
if { ! [catch  {$infilehandle get imgwcs} wcsString] } {
    set powWCS($imagename) $wcsString
}

#we're done reading the file now
$infilehandle close


}

proc powReplotReset {} {
#puts "powReplotReset start"
     global currgn
     global powPlotParam

     set powPlotParam(g_magnification,$currgn) 1.0
     set powPlotParam(g_multiplier,$currgn)    0.5
     powEndROI 1
}

proc powSetMagnification {} {
#puts "powReplotReset start"
     global g_magnification
     global currgn

     set powPlotParam(g_magnification,$currgn) $g_magnification 
}

proc powExpr { outDataName inputExpression } {
#puts "powExpr start"
   #  Make sure fitsTcl is loaded first
   if {[lsearch [info loaded] *Fitstcl*] == -1} {
   # Pan Chai: comment out error message
#      error "You must load fitsTcl to use powExpr"
   }

   #  Evaluate the expression
   set res [vexpr -ptr -use powExprGetData $inputExpression]

   #  Create the powData item; have it create its own copy of the data
   eval powCreateData $outDataName $res 1

   #  Don't forget to free the pointer returned by vexpr
   fits free [lindex $res 0]
}

proc powDeleteMenuBarItem {} {
#puts "powDeleteMenuBarItem start"
     global menuBarDeleteFlag
     
     if { $menuBarDeleteFlag == "false" } {
	# .pow.mbar delete "Zoom"
	.pow.mbar delete "Replot"
	set menuBarDeleteFlag "true"
     }
}

proc _changeWinDirectoryToUnixFormat { dir } {
#puts "_changeWinDirectoryToUnixFormat start"
     set result ""

     for { set i 0 } { $i < [string length $dir] } {incr i} {
         set currentChar [string range $dir $i $i]
         if { $currentChar == "\\" } {
            set currentChar "/"
         }
         set result [format "%s%s" $result $currentChar]
     }
     return $result
}

##################
#
# Button Selection
#
##################

proc powButtonSelection { wndw1 wndw2 option { saveOp "SAVE" } } {
     global powLutButton
     global powROIButton
     global powbg

     if { $option == "Left" } {
        catch { ${wndw1} configure -bg $powbg }
        catch { ${wndw2} configure -bg yellow }
        set powLutButton 1
        set powROIButton 3
     } else {
        catch { ${wndw1} configure -bg yellow } err
        catch { ${wndw2} configure -bg $powbg }
        set powLutButton 3
        set powROIButton 1
     }
     if { $saveOp == "SAVE" } {
        powSaveConfig
     }
}

namespace eval powEvents {
    variable lastEventWndw ""
    
    proc generate { evt {evtWndw ""} } {
	variable lastEventWndw
        global CpowXRangeY0

	# This evtWndw messiness is necessary due to LinuxPPC's (and others?) problems
	# in tracking the focus when selecting menu items.  Can't tell if it is a
	# Window Manager problem or Tk problem.

	if { $evt == "<<PostMenus>>" } {
	    set whn "now"
	    set lastEventWndw $evtWndw
            catch { unset CpowXRangeY0 }
	} else {
	    set whn "tail"
	}
	if { $evtWndw=="" } {
	    if { $lastEventWndw=="" } {
		set evtWndw [focus]
	    } else {
		set evtWndw $lastEventWndw
	    }
	}
#puts "Evt: $evt in $evtWndw"
	if { $evtWndw != "" && [winfo exists $evtWndw] } {
	    event generate $evtWndw $evt -when $whn
	} else {
	    event generate . $evt -when $whn
	}
    }

    proc postMenus { w } {
#puts "Posting menus for $w"
       if { $w == ".pow" } {
	  .pow.mbar.file entryconfig "Close*" -label "Close POW"
       } else {
	  .pow.mbar.file entryconfig "Close*" -label "Close Window"
       }
    }

    proc ExitPOW { } {
        set tempList [::powListGraphs]
        foreach name $tempList {
           catch { powDeleteGraph $name NOPROMPT} err
        }
	set Pow_Done 1
	catch { destroy .pow.pow }
	catch { destroy .pow.scope }
        catch { event generate .pow <<powExit>> }
    }
}

