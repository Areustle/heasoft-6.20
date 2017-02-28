###########################################################################
#                                                                         #
#            Routines for new edit graph motif                            #
#                                                                         #
###########################################################################

proc powEditGraphDlg { gn } {
    global powbg currgn
    global powDWP
    global g_titleFont
    global powEditPlotParam
    
#puts "powEditGraphDlg gn: <$gn>"
    set w ${powDWP}gEdit
    if {[winfo exists $w]} {
       raise $w
       focus $w
       $w.btns.reset invoke
       return
    }

    powToplevel $w .pow "-bg $powbg"
    bind $w <<CloseWindow>> "powEditExitDlg"
    catch {wm title $w "Edit Graph"}

    label $w.graphCont -text "Graph Contents:" -fg black -bg yellow -font g_titleFont
    frame $w.graphList -bg $powbg
    scrollbar $w.graphList.scrolly -orient vertical -takefocus 0\
            -command {${powDWP}gEdit.graphList.lst yview} -bg $powbg
    listbox $w.graphList.lst -bg $powbg -width 25 -height 6 \
	    -selectmode browse -exportselection 0 -takefocus 0 \
	    -yscrollcommand "$w.graphList.scrolly set " -font g_titleFont
    bind $w.graphList.lst <ButtonRelease-1> \
	    { powEditBuildOptions }

    grid $w.graphList.lst     -in $w.graphList \
	  -row 1 -column 1 -sticky news
    grid $w.graphList.scrolly -in $w.graphList \
	  -row 1 -column 2 -sticky news
    grid rowconfigure    $w.graphList 1 -weight 1
    grid columnconfigure $w.graphList 1 -weight 1

    label $w.elemCont -text "Available Objects:" \
	  -fg black -bg yellow -font g_titleFont
    frame $w.elemList -bg $powbg
    scrollbar $w.elemList.scrolly -orient vertical -takefocus 0 \
	    -command {${powDWP}gEdit.elemList.lst yview} -bg $powbg
    listbox $w.elemList.lst -bg $powbg -width 25 -height 6 \
	    -selectmode browse -exportselection 0 -takefocus 0 \
	    -yscrollcommand "$w.elemList.scrolly set " -font g_titleFont

    grid $w.elemList.lst     -row 1 -column 1 -sticky news
    grid $w.elemList.scrolly -row 1 -column 2 -sticky news
    grid rowconfigure    $w.elemList 1 -weight 1
    grid columnconfigure $w.elemList 1 -weight 1

    button $w.help -bg $powbg -text "Help" \
	    -command {powHelp EditGraphs.html} -takefocus 0 -font g_titleFont

    grid $w.graphCont -row 1 -column 1 -sticky w    -padx 5 -pady 5
    grid $w.graphList -row 2 -column 1 -sticky news
    grid $w.help      -row 1 -column 2
    grid $w.elemCont  -row 1 -column 3 -sticky w    -padx 5 -pady 5
    grid $w.elemList  -row 2 -column 3 -sticky news

    frame  $w.elemButt -bg $powbg
    button $w.elemButt.add    -text "<-- Add" -bg $powbg -takefocus 0 \
	    -command {powEditAddElems} -font g_titleFont
    button $w.elemButt.remove -text "--> Remove" -bg $powbg -takefocus 0 \
	    -command {powEditRemoveElems} -font g_titleFont

    pack $w.elemButt.add    -fill x -padx 5 -pady 2		
    pack $w.elemButt.remove -fill x -padx 5 -pady 2
    grid $w.elemButt        -row 2 -column 2

    button $w.editObj -text "Edit Objects" -bg $powbg -takefocus 0 \
	    -command {powEditObjectDlg} -font g_titleFont
    grid $w.editObj -row 3 -column 3 -padx 5 -pady 5

    frame $w.btns -bg $powbg
    button $w.btns.apply -text "Apply" -bg $powbg \
          -command { powEditApplyToGraph } -font g_titleFont
    button $w.btns.reset -text "Reset" -bg $powbg \
          -command { powEditResetDialog } -font g_titleFont
    button $w.btns.exit -text "Exit"  -bg $powbg \
	    -command "powEditExitDlg" -font g_titleFont

    pack $w.btns.apply  -side left -padx 5 -pady 2
    pack $w.btns.reset  -side left -padx 5 -pady 2
    pack $w.btns.exit   -side left -padx 5 -pady 2
    grid $w.btns        -row 6 -column 1 -columnspan 3 -pady 5

    grid rowconfigure    $w 2 -weight 1
    grid rowconfigure    $w 5 -weight 1

    grid columnconfigure $w 1 -weight 1
    grid columnconfigure $w 3 -weight 1

    powEditResetDialog init
}

proc powEditExitDlg { } {
    global powDWP
    destroy ${powDWP}gEdit
}


proc powEditApplyToGraph { } {
   global powEditGraphName powPlotParam powEditGraphName
   global powLutButton powROIButton

   powEditUpdateVariables
   set gn $powEditGraphName
   powEraseGraph $gn 1

   powCreateGraph $gn $powPlotParam(curves,$gn) $powPlotParam(images,$gn) \
         $powPlotParam(xunits,$gn) $powPlotParam(yunits,$gn) \
         $powPlotParam(xlabel,$gn) $powPlotParam(ylabel,$gn) \
         $powPlotParam(xdimdisp,$gn) $powPlotParam(ydimdisp,$gn) \
         $powPlotParam(xBot,$gn) $powPlotParam(yBot,$gn) \
         $powPlotParam(xTop,$gn) $powPlotParam(yTop,$gn)

   # this is for Mouse. Will not be saved in POW preference
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


}

proc powEditResetDialog { {flag "reset" }} {
   global currgn powDWP powEditGraphName powEditPlotParam powPlotParam
   
#puts "powEditResetDialog: currgn: $currgn"
   set xdimdisp $powPlotParam(xdimdisp,powDef)
   set ydimdisp $powPlotParam(ydimdisp,powDef)

   if { [info exists powEditPlotParam(xdimdisp,new)] && $flag == "init" } {
      set xdimdisp $powEditPlotParam(xdimdisp,new)
      set ydimdisp $powEditPlotParam(ydimdisp,new)
   } 
   if { [winfo exists ${powDWP}gEdit] } {
      set powEditGraphName $currgn

# -------> FIX of selection of current graph

      powEditInitVariables init

      #set powEditPlotParam(xdimdisp,new) $xdimdisp
      #set powEditPlotParam(ydimdisp,new) $ydimdisp

      if { $currgn=="powDef" } {
         set powEditGraphName [powEditCreateNewGraphName]
      }
      powEditBuildOptions
   }
}


proc powEditCreateNewGraphName { } {
   set i 1
   set gn "powGraph_$i"
   while { [powListGraphs $gn] } {
      incr i
      set gn "powGraph_$i"
   }
   return $gn
}


proc powEditCreateNewGraph { gn } {
   global powPlotParam powEditGraphName
   global powWCS powFitsHeader powFitsHeaderCnt xCount yCount

   if { $gn!="" } {
      set powEditGraphName $gn
   } else {
      set powEditGraphName "powDef"
   }
   powEditInitVariables
   set powEditGraphName [powEditCreateNewGraphName]

   set powWCS($powEditGraphName) $powWCS($gn)
   set powFitsHeader($powEditGraphName) $powFitsHeader($gn)
   set powFitsHeaderCnt($powEditGraphName) $powFitsHeaderCnt($gn)
   set powWCS(${powEditGraphName}scope) $powWCS($gn)
   set powFitsHeader(${powEditGraphName}scope) $powFitsHeader($gn)
   set powFitsHeaderCnt(${powEditGraphName}scope) $powFitsHeaderCnt($gn)
   set xCount($powEditGraphName) 0
   set yCount($powEditGraphName) 0
   set xCount(${powEditGraphName}scope) 0
   set yCount(${powEditGraphName}scope) 0

   powEditApplyToGraph
}


proc powEditSelectPage { p } {
   global powDWP

   set note ${powDWP}gEdit.nBook
   Notebook:raise $note $p
}


########################################################################
#
#   Build Option Pages
#
########################################################################

proc powEditBuildOptions { } {
   global powDWP powbg

   set note ${powDWP}gEdit.nBook
   if { ![winfo exists $note] } {
      Notebook:create $note -pages {Graph Fonts Ticks Points Lines Image} \
	    -pad 4 -bg $powbg
      grid $note -in ${powDWP}gEdit -row 5 -column 1 \
	    -padx 15 -pady 0 -columnspan 3 -sticky nwes
   }

   set w [Notebook:frame $note Graph]
   powEditBuildGraphOptions $w

   set w [Notebook:frame $note Fonts]
   powEditBuildFontOptions $w

   set w [Notebook:frame $note Ticks]
   powEditBuildTickOptions $w

   set w [Notebook:frame $note Points]
   set itemNo [${powDWP}gEdit.graphList.lst curselection]

#puts "itemNo: $itemNo"

   if { $itemNo == "" } {
      set item "none none"
   } else {
      set item [${powDWP}gEdit.graphList.lst get $itemNo]
   }
   foreach {name type} [split $item { }] {}

   if { $type == "(curve)" } {
      powEditBuildCurveOptions1 $w $name
      set w [Notebook:frame $note Lines]
      powEditBuildCurveOptions2 $w $name
   } else {
      powEditBuildEmptyOptions $w "Curve Not Selected"
      set w [Notebook:frame $note Lines]
      powEditBuildEmptyOptions $w "Curve Not Selected"
   }

   set w [Notebook:frame $note Image]
   if { $type == "(image)" } {
      powEditBuildImageOptions $w $name
   } else {
      powEditBuildEmptyOptions $w "Image Not Selected"
   }

   Notebook:resize $note
}

##################
#
# Setup Page: Empty
#
##################

proc powEditBuildEmptyOptions { frame str } {
    global powbg
    global g_titleFont

    if {[winfo exists $frame]} {
       foreach i [winfo children $frame] {
	  destroy $i
       }
    } else {
       frame $frame -bg $powbg
    }
    
    label $frame.lab -bg $powbg -text $str -font g_titleFont

    # Use pack instead of grid to destroy any pre-existing grid
    # configuration from nonEmpty frames
    pack $frame.lab -side top -fill both -anchor center -expand 1
}


##################
#
# Setup Page: POW
#
##################

proc powEditBuildPOWOptions { frame } {
   global powEditAppParam powbg
   global g_titleFont

   if {[winfo exists $frame]} {
      foreach i [winfo children $frame] {
         destroy $i
      }
   } else {
      frame $frame -bg $powbg
   }
    
   set row 1

   grid rowconfigure $frame $row -minsize 5
   incr row

# Setup POW Application Options

   #
   # Cursor:
   #

   label $frame.cursor -bg $powbg -text "Cursor:" -font g_titleFont
   grid $frame.cursor  -row $row -column 2 -sticky ne

   set col 3
   foreach [list lab val] [list "Cross Hairs" crosshair \
         "Left Arrow" top_left_arrow "Right Arrow" right_ptr \
         "Gumby" gumby "Star Trek" trek] {
      if {$col==6} {set col 3; incr row}
      radiobutton $frame.cStyle$val -bg $powbg -text $lab \
            -variable powEditAppParam(cursor,new) -value $val \
            -highlightthickness 0 -takefocus 0 \
            -command "$frame configure -cursor $val" -font g_titleFont
      grid $frame.cStyle$val -row $row -column $col -sticky nw -padx 4
      incr col
   }
   incr row

   grid rowconfigure $frame $row -minsize 7
   incr row

   #
   # GUI position:
   #

   label $frame.gui -bg $powbg -text "GUI Position:" -font g_titleFont
   grid $frame.gui  -row $row -column 2 -sticky ne

   set col 3
   foreach [list lab val] [list Top top Left left Right right Bottom bottom \
         "Hidden" none] {
      if {$col==6} {set col 3; incr row}
      radiobutton $frame.gPos$val -bg $powbg -text $lab \
            -variable powEditAppParam(GUIposition,new) -value $val \
            -highlightthickness 0 -takefocus 0 -font g_titleFont
      grid $frame.gPos$val -row $row -column $col -sticky nw -padx 4
      incr col
   }
   incr row

   grid rowconfigure $frame $row -minsize 7
   incr row

   #
   # Scopebox Size:
   #

   label $frame.scope -bg $powbg -text "Scopebox Size:" -font g_titleFont
   grid $frame.scope  -row $row -column 2 -sticky ne

   set col 3
   foreach [list lab val] [list None [list 0 0] 100x100 [list 100 100] 150x150 [list 150 150]\
         200x200 [list 200 200] ] {
      if {$col==6} {set col 3; incr row}
      radiobutton $frame.sb$lab -bg $powbg -text $lab \
            -variable powEditAppParam(ScopeSize,new) -value $val \
            -highlightthickness 0 -takefocus 0 -font g_titleFont
      grid $frame.sb$lab  -row $row -column $col -sticky nw -padx 4
      incr col
   }
   incr row

   grid rowconfigure $frame $row -minsize 7
   incr row

   #
   # Resize Main:
   #

   label $frame.rszMain -bg $powbg -text "Resize Window:" -font g_titleFont
   grid $frame.rszMain  -row $row -column 2 -sticky ne

   set col 3
   foreach [list lab val] [list "To Fit Contents" 1 "Never" 0] {
      if {$col==6} {set col 3; incr row}
      radiobutton $frame.rs$val -bg $powbg -text $lab \
            -variable powEditAppParam(ResizeMain,new) -value $val \
            -highlightthickness 0 -takefocus 0 -font g_titleFont
      grid $frame.rs$val  -row $row -column $col -sticky nw -padx 4
      incr col
   }
   incr row

   grid rowconfigure $frame $row -minsize 7
   incr row

   #
   # Background Color:
   #

   label $frame.bg -bg $powbg -text "Background Color:" -font g_titleFont
   grid $frame.bg  -row $row -column 2 -sticky e

   powColorFrame $frame.bgColors powEditAppParam(bg,new)
   grid $frame.bgColors -row $row -column 3 -columnspan 3 \
         -sticky w -padx 8
   incr row

   grid rowconfigure $frame $row -minsize 7
   incr row


   grid columnconfigure $frame 0 -minsize 5
   grid columnconfigure $frame [list 3 4 5 6] -weight 1
}


##################
#
# Setup Page: Graph
#
##################

proc powEditBuildGraphOptions { frame } {
    global powbg powEditPlotParam powEditCurveParam powEditGraphName
    global powPlotParam 
    global currgn
    global g_titleFont
    global powLutButton
    global powROIButton
    global buttonWndw
    global buttonSelection

    if {[winfo exists $frame]} {
       foreach i [winfo children $frame] {
	  destroy $i
       }
    } else {
       frame $frame -bg $powbg
    }
    set buttonWndw $frame
    
    set row 1

    grid rowconfigure $frame $row -minsize 5
    incr row

    if { $powEditGraphName != "powDef" } {
       # Don't do this if we are editting defaults...
       # ... these data are not relvnt

       label $frame.title  -bg $powbg -text "Graph Title:" -font g_titleFont
       entry $frame.etitle -bg $powbg \
             -textvariable powEditPlotParam(titleString,new) -takefocus 1 -font g_titleFont
       grid $frame.title  -in $frame -row $row -column 1 -sticky e \
             -padx 5
       grid $frame.etitle -in $frame -row $row -column 2 -sticky ew \
             -padx 5 -columnspan 2
       incr row

       grid rowconfigure $frame $row -minsize 5
       incr row
    }

# Setup Bbox options

    label $frame.x -bg $powbg -text "X Axis" -font g_titleFont
    label $frame.y -bg $powbg -text "Y Axis" -font g_titleFont
    grid $frame.x  -in $frame -row $row -column 2 -sticky ew
    grid $frame.y  -in $frame -row $row -column 3 -sticky ew
    incr row

    if { $powEditGraphName == "powDef" } {
       set labelsAndValues [list Size: dimdisp]
    } else {
       # Don't do this if we are editting defaults...
       # ... these data are not relvnt
       set labelsAndValues [list Label: label  Min: Bot    Max: Top \
                               Units: units Size: dimdisp         ]
    }

    if { $powEditGraphName != "powDef" } {
       foreach {lbl val} $labelsAndValues {
          label $frame.l$val -bg $powbg -text $lbl -font g_titleFont
          entry $frame.ex$val -bg $powbg \
                -textvariable powEditPlotParam(x$val,new) -takefocus 1 -font g_titleFont
          entry $frame.ey$val -bg $powbg \
                -textvariable powEditPlotParam(y$val,new) -takefocus 1 -font g_titleFont
          grid $frame.l$val  -in $frame -row $row -column 1 -sticky e \
                -padx 5
          grid $frame.ex$val -in $frame -row $row -column 2 -sticky w \
                -padx 5
          grid $frame.ey$val -in $frame -row $row -column 3 -sticky w \
                -padx 5
          incr row
       }
    }

    # Axis Scaling

    label $frame.tickScale -bg $powbg -text "Scaling:" -font g_titleFont
    grid $frame.tickScale -in $frame -row $row -column 1 -sticky ne -padx 5

    frame $frame.scaleX -bg $powbg
    frame $frame.scaleY -bg $powbg
    radiobutton $frame.scaleX.log -bg $powbg -text Log \
	  -variable powEditPlotParam(xTickScal,new) -value log \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    radiobutton $frame.scaleX.lin -bg $powbg -text Linear \
	  -variable powEditPlotParam(xTickScal,new) -value linear \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    radiobutton $frame.scaleY.log -bg $powbg -text Log \
	  -variable powEditPlotParam(yTickScal,new) -value log \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    radiobutton $frame.scaleY.lin -bg $powbg -text Linear \
	  -variable powEditPlotParam(yTickScal,new) -value linear \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont

    pack $frame.scaleX.lin -padx 5 -side left -anchor w
    pack $frame.scaleX.log -padx 5 -side left -anchor w
    pack $frame.scaleY.lin -padx 5 -side left -anchor w
    pack $frame.scaleY.log -padx 5 -side left -anchor w

    grid $frame.scaleX -row $row -column 2 -sticky we -padx 5
    grid $frame.scaleY -row $row -column 3 -sticky we -padx 5
    incr row

    label $frame.mouse -bg $powbg -text "Mouse:" -font g_titleFont
    grid $frame.mouse  -in $frame -row $row -column 1 -sticky ne -padx 5 

    frame $frame.buttonSelection -bg $powbg
    set buttonWndw $frame.buttonSelection
    radiobutton $frame.buttonSelection.leftZoom \
           -font g_titleFont \
           -text "Normal: left zoom, right brightnesss/contrast" \
           -variable buttonSelection -value leftZoom \
           -command {powButtonSelection ${buttonWndw}.leftZoom ${buttonWndw}.leftBright Right DONT_SAVE}
    radiobutton $frame.buttonSelection.leftBright \
           -font g_titleFont \
           -text "Reversed: left  brightnesss/contrast, right zoom" \
           -variable buttonSelection -value leftBright \
           -command {powButtonSelection ${buttonWndw}.leftZoom ${buttonWndw}.leftBright Left DONT_SAVE}

    grid $frame.buttonSelection.leftZoom -row 0 -column 1 -sticky w -padx 5
    grid $frame.buttonSelection.leftBright -row 1 -column 1 -sticky w -padx 5
    grid $frame.buttonSelection -row $row -column 2 -sticky w -padx 5 -rowspan 3 -columnspan 2

    grid rowconfigure $frame $row -minsize 5
    incr row 3

    if { $powLutButton == 3 } {
       $frame.buttonSelection.leftZoom select
       powButtonSelection ${buttonWndw}.leftZoom ${buttonWndw}.leftBright Right DONT_SAVE 
    } else {
       $frame.buttonSelection.leftBright select
       powButtonSelection ${buttonWndw}.leftZoom ${buttonWndw}.leftBright Left DONT_SAVE 
    }

    if { $powEditGraphName != "powDef" } {
       # Don't do this if we are editting defaults...
       # ... these data are not relvnt

      checkbutton $frame.scaleData -bg $powbg -text "Scale curve data to axes" \
	    -variable powEditPlotParam(scalData,new) -onvalue Yes -offvalue No \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
      grid $frame.scaleData -row $row -column 2 -columnspan 2 -sticky we -padx 5
      incr row

      grid rowconfigure $frame $row -minsize 7
      incr row

    }

    # Reset Button

    if { $powEditGraphName != "powDef" } {
       # Don't do this if we are editting defaults...
       # ... these data are not relvnt

       button $frame.reset -bg $powbg -text "Reset Min/Max" -takefocus 0 \
	     -command {
	  foreach par [list xBot yBot xTop yTop] {
	     set powEditPlotParam($par,new) NULL
	  }
       } -font g_titleFont
       grid $frame.reset -in $frame -row $row -column 2 \
	     -columnspan 2 -sticky {} -padx 5 -pady 5
       incr row

    }

    grid rowconfigure $frame $row -minsize 5
    incr row

    grid columnconfigure $frame [list 0 4] -weight 1 -minsize 5
}

##################
#
# Setup Page: Fonts
#
##################

proc powEditBuildFontOptions { frame } {
    global powbg powEditFontParam powEditGraphName powFontParam
    global g_titleFont

    if {[winfo exists $frame]} {
       foreach i [winfo children $frame] {
	  destroy $i
       }
    } else {
       frame $frame -bg $powbg
    }
    
    set row 1

    grid rowconfigure $frame $row -minsize 5
    incr row

    label $frame.family -text "Font Family" -bg $powbg -font g_titleFont
    label $frame.size   -text "Size (pt)"   -bg $powbg -font g_titleFont
    label $frame.style  -text "Style"       -bg $powbg -font g_titleFont
    label $frame.color  -text "Color"       -bg $powbg -font g_titleFont
    grid $frame.family -row $row -column 2 -sticky s
    grid $frame.size   -row $row -column 3 -sticky s
    grid $frame.style  -row $row -column 4 -sticky s -columnspan 2
    grid $frame.color  -row $row -column 6 -sticky s
    incr row

    grid rowconfigure $frame $row -minsize 5
    incr row

    foreach {tLbl lbl} [list \
          "Title:" title "Axis Labels:" axis \
          "Tick Labels:" tick "Text Labels:" note] {

       label $frame.lbl$lbl -text $tLbl -bg $powbg -font g_titleFont
       checkbutton $frame.bld$lbl -text Bold -onvalue bold -offvalue normal \
             -bg $powbg -variable powEditFontParam(${lbl}Weight,new) \
             -highlightthickness 0 -font g_titleFont
       checkbutton $frame.itl$lbl -text Italic -onvalue italic -offvalue roman \
             -bg $powbg -variable powEditFontParam(${lbl}Slant,new) \
             -highlightthickness 0 -font g_titleFont
       set mnu [eval tk_optionMenu $frame.fnt$lbl \
             powEditFontParam(${lbl}Font,new) $powFontParam(allFonts,powDef)]
       $frame.fnt$lbl configure -bg $powbg -highlightthickness 0 -width 20 -font g_titleFont
       $mnu configure -bg $powbg -font g_titleFont

       set mnu [tk_optionMenu $frame.siz$lbl powEditFontParam(${lbl}Size,new) \
             7 9 12 14 16 18 24 32 40]
       $frame.siz$lbl configure -bg $powbg -highlightthickness 0 -width 3 -font g_titleFont
       $mnu configure -bg $powbg -font g_titleFont

       button $frame.clr$lbl -textvariable powEditFontParam(${lbl}Color,new) \
             -bg $powbg -highlightthickness 0 -width 7 \
             -command "powSelectColor powEditFontParam(${lbl}Color,new)" -font g_titleFont

       grid $frame.lbl$lbl -row $row -column 1 -padx 3 -sticky e
       grid $frame.fnt$lbl -row $row -column 2 -padx 3
       grid $frame.siz$lbl -row $row -column 3 -padx 3
       grid $frame.bld$lbl -row $row -column 4 -padx 3
       grid $frame.itl$lbl -row $row -column 5 -padx 3
       grid $frame.clr$lbl -row $row -column 6 -padx 3
       incr row

       grid rowconfigure $frame $row -minsize 5
       incr row
    }

}

##################
#
# Setup Page: Tick
#
##################

proc powEditBuildTickOptions { frame } {
    global powbg powEditPlotParam
    global g_titleFont

    if {[winfo exists $frame]} {
       foreach i [winfo children $frame] {
	  destroy $i
       }
    } else {
       frame $frame -bg $powbg
    }
    
    set row 1

    grid rowconfigure $frame $row -minsize 5
    incr row

# Setup Tick Options

    label $frame.nXTicks -bg $powbg -text "# of X Ticks:" -font g_titleFont
    grid $frame.nXTicks -in $frame -row $row -column 2 -sticky ne

    frame $frame.xTickSlide -bg $powbg
    label $frame.xTickSlide.few -text "None" -bg $powbg -font g_titleFont
    label $frame.xTickSlide.lots -text "Many" -bg $powbg -font g_titleFont
    scale $frame.xTickSlide.slide -from 0 -to 12 \
	    -orient horizontal -variable powEditPlotParam(xNumTicks,new) \
	    -highlightbackground $powbg -bg $powbg \
	    -showvalue 0 -takefocus 0 -font g_titleFont
    pack $frame.xTickSlide.few -in $frame.xTickSlide -side left
    pack $frame.xTickSlide.slide -in $frame.xTickSlide -side left \
	    -expand 1 -fill x
    pack $frame.xTickSlide.lots -in $frame.xTickSlide -side right
    grid $frame.xTickSlide -in $frame -row $row -column 3 -columnspan 3 \
	    -sticky ew -padx 5
    incr row

    label $frame.nYTicks -bg $powbg -text "# of Y Ticks:" -font g_titleFont
    grid $frame.nYTicks -in $frame -row $row -column 2 -sticky ne

    frame $frame.yTickSlide -bg $powbg
    label $frame.yTickSlide.few -text "None" -bg $powbg -font g_titleFont
    label $frame.yTickSlide.lots -text "Many" -bg $powbg -font g_titleFont
    scale $frame.yTickSlide.slide -from 0 -to 12 \
	    -orient horizontal -variable powEditPlotParam(yNumTicks,new) \
	    -highlightbackground $powbg -bg $powbg \
	    -showvalue 0 -takefocus 0 -font g_titleFont
    pack $frame.yTickSlide.few -in $frame.yTickSlide -side left
    pack $frame.yTickSlide.slide -in $frame.yTickSlide -side left \
	    -expand 1 -fill x
    pack $frame.yTickSlide.lots -in $frame.yTickSlide -side right
    grid $frame.yTickSlide -in $frame -row $row -column 3 -columnspan 3 \
	    -sticky ew -padx 5
    incr row

    label $frame.xTicks -bg $powbg -text "X Ticks:" -font g_titleFont
    grid $frame.xTicks -in $frame -row $row -column 2 -sticky ne
    frame $frame.xTickButt -bg $powbg
    radiobutton $frame.xTickButt.in -bg $powbg -text In \
	  -variable powEditPlotParam(xTickLength,new) \
	  -value [list -10 -10 -10 -10] \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    radiobutton $frame.xTickButt.out -bg $powbg -text Out \
	  -variable powEditPlotParam(xTickLength,new) \
	  -value [list 10 10 10 10] \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    checkbutton $frame.xTickButt.lab -bg $powbg -text "Labeled?"\
	  -variable powEditPlotParam(xLabelTicks,new) \
	  -onvalue [list Yes No No Yes] \
	  -offvalue [list No No No No] \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    pack $frame.xTickButt.in -side left -padx 4
    pack $frame.xTickButt.out -side left -padx 4
    pack $frame.xTickButt.lab -side left -padx 20
    grid $frame.xTickButt -row $row -column 3 -sticky w -padx 0 -columnspan 3
    incr row

    label $frame.yTicks -bg $powbg -text "Y Ticks:" -font g_titleFont
    grid $frame.yTicks -in $frame -row $row -column 2 -sticky ne
    frame $frame.yTickButt -bg $powbg
    radiobutton $frame.yTickButt.in -bg $powbg -text In \
	  -variable powEditPlotParam(yTickLength,new) \
	  -value [list -10 -10 -10 -10] \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    radiobutton $frame.yTickButt.out -bg $powbg -text Out \
	  -variable powEditPlotParam(yTickLength,new) \
	  -value [list 10 10 10 10] \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    checkbutton $frame.yTickButt.lab -bg $powbg -text "Labeled?"\
	  -variable powEditPlotParam(yLabelTicks,new) \
	  -onvalue [list Yes No No Yes] \
	  -offvalue [list No No No No] \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    pack $frame.yTickButt.in -side left -padx 4
    pack $frame.yTickButt.out -side left -padx 4
    pack $frame.yTickButt.lab -side left -padx 20
    grid $frame.yTickButt -row $row -column 3 -sticky w -padx 0 -columnspan 3
    incr row

    # Tick Labeling

    label $frame.tickLabel -bg $powbg -text "Tick Labels:" -font g_titleFont
    grid $frame.tickLabel -in $frame -row $row -column 2 -sticky ne

    set col 3
    foreach {lab val} [list Decimal decimal "Base 60 (deg)" degrees] {
	if {$col==5} {set col 3; incr row}
	radiobutton $frame.label$val -bg $powbg -text $lab \
		-variable powEditPlotParam(tickLabels,new) -value $val \
		-highlightthickness 0 -takefocus 0 -font g_titleFont
	grid $frame.label$val -in $frame \
		-row $row -column $col -sticky nw -padx 5
	incr col
    }
    incr row

    grid rowconfigure $frame $row -minsize 7
    incr row

    # Add a separator line

    frame $frame.sep -bg $powbg -relief ridge -height 2 -bd 2
    grid $frame.sep -row $row -column 3 -columnspan 3 -sticky ew
    grid rowconfigure $frame $row -minsize 10 -weight 1
    incr row

# Setup Grid Options

    checkbutton $frame.grid -bg $powbg -text "Grid Lines -" \
	    -variable powEditPlotParam(GridLines,new) \
	    -highlightthickness 0 -onvalue Yes -offvalue No -font g_titleFont
    grid $frame.grid -in $frame -row $row -column 1 -sticky w -columnspan 6
    incr row

    # Line Style

    label $frame.style -bg $powbg -text "Style:" -font g_titleFont
    grid $frame.style -in $frame -row $row -column 2 -sticky ne

    set col 3
    foreach {style val} \
	    {Solid " " "Sm Dash" 10 "Lg Dash" 20 \
	     "Dotted" "4 4" "Dot Dash" "15 10 4 10"} {
	regsub -all { } $style {_} cln
	if {$col==6} {set col 3; incr row}
	radiobutton $frame.lStyle$cln -bg $powbg -text $style \
		-variable powEditPlotParam(GridDash,new) -value $val \
		-highlightthickness 0 -takefocus 0 -font g_titleFont
	grid $frame.lStyle$cln -in $frame \
		-row $row -column $col -sticky nw -padx 4
	incr col
    }
    incr row

    grid rowconfigure $frame $row -minsize 7
    incr row

    # Color

    label $frame.gridColor -bg $powbg -text "Color:" -font g_titleFont
    grid $frame.gridColor -in $frame -row $row -column 2 -sticky e

    powColorFrame $frame.gColors powEditPlotParam(GridColor,new)
    grid $frame.gColors -in $frame -row $row -column 3 -columnspan 3 \
	  -sticky w -padx 8
    incr row

    grid rowconfigure $frame $row -minsize 5
    incr row

    grid columnconfigure $frame 0 -minsize 5
    grid columnconfigure $frame [list 3 4 5 6] -weight 1
}


##################
#
# Setup Page: Points
#
##################

proc powEditBuildCurveOptions1 { frame curve } {
    global powbg powEditCurveParam
    global g_titleFont

# If frame already exists delete its contents

    if {[winfo exists $frame]} {
       foreach i [winfo children $frame] {
	  destroy $i
       }
    } else {
       frame $frame -bg $powbg
    }
    
# Set default values if not defined for this curve

#puts "call powEditCurveDefOptions 1"
    powEditCurveDefOptions $curve

# Build widgets

    set row 1

    grid rowconfigure $frame $row -minsize 5
    incr row

#
# Setup Points Options
#

    # Point Display

    checkbutton $frame.pDisp -bg $powbg -text "Points -" \
          -variable powEditCurveParam(pDisp${curve},new) \
          -highlightthickness 0 -onvalue Yes -offvalue No -font g_titleFont
    grid $frame.pDisp -in $frame -row $row -column 1 -sticky w -columnspan 2
    incr row

    grid rowconfigure $frame $row -minsize 10
    incr row

    # Shape

    label $frame.shape -bg $powbg -text "Shape:" -font g_titleFont
    grid $frame.shape -in $frame -row $row -column 2 -sticky ne

    set col 3
    set cnt 1
    foreach shape {Dot Cross Diamond Box Octagon Triangle "Inv. Triangle"} {
	if {$col>=7} {set col 3; incr row}
	radiobutton $frame.pShape$cnt -bg $powbg -text $shape \
		-variable powEditCurveParam(pShape${curve},new) -value $shape \
		-highlightthickness 0 -takefocus 0 -font g_titleFont
	grid $frame.pShape$cnt -in $frame \
		-row $row -column $col -sticky nw -padx 4
	if { $shape=="Inv. Triangle" } {
	   grid configure $frame.pShape$cnt -columnspan 2
	   incr col
	}
	incr col
	incr cnt
    }
    incr row

    grid rowconfigure $frame $row -minsize 7
    incr row

    # Size

    label $frame.size -bg $powbg -text "Size:" -font g_titleFont
    grid $frame.size -in $frame -row $row -column 2 -sticky ne

    frame $frame.pSize -bg $powbg
    radiobutton $frame.pSize.fixed -bg $powbg -text "Fixed... " \
	    -variable powEditCurveParam(pSizeErr${curve},new) -value No \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
    label $frame.pSize.text -bg $powbg -width 2 \
	    -textvariable powEditCurveParam(pSize${curve},new) -font g_titleFont
    label $frame.pSize.pt -bg $powbg -width 2 -text pt -font g_titleFont
    scale $frame.pSize.slide -from 2 -to 12 -orient horizontal \
	    -variable powEditCurveParam(pSize${curve},new) \
	    -highlightbackground $powbg -bg $powbg \
	    -showvalue 0 -takefocus 0 -font g_titleFont

    grid $frame.pSize.fixed -in $frame.pSize -row 1 -column 1 -sticky nw -padx 3
    grid $frame.pSize.text -in $frame.pSize -row 1 -column 2 -sticky ne
    grid $frame.pSize.pt -in $frame.pSize -row 1 -column 3 -sticky nw
    grid $frame.pSize.slide -in $frame.pSize -row 1 -column 4 -sticky new \
	    -padx 4
    
    grid columnconfigure $frame.pSize 4 -weight 1
    grid $frame.pSize -in $frame -row $row -column 3 -columnspan 3 \
	    -sticky new

    incr row

    radiobutton $frame.pSizeError -bg $powbg -text Errorbars \
	    -variable powEditCurveParam(pSizeErr${curve},new) -value Yes \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
    grid $frame.pSizeError -in $frame \
	    -row $row -column 3 -sticky nw -padx 4
    incr row

    grid rowconfigure $frame $row -minsize 7
    incr row

    # Point Filling

    label $frame.fill -bg $powbg -text "Fill:" -font g_titleFont
    grid $frame.fill -in $frame -row $row -column 2 -sticky ne

    set col 3
    foreach {lab val} {Yes Yes No No} {
	if {$col==7} {set col 3; incr row}
	radiobutton $frame.pFill$val -bg $powbg -text $lab \
		-variable powEditCurveParam(pFill${curve},new) -value $val \
		-highlightthickness 0 -takefocus 0 -font g_titleFont
	grid $frame.pFill$val -in $frame \
		-row $row -column $col -sticky nw -padx 4
	incr col
    }
    incr row

    grid rowconfigure $frame $row -minsize 7
    incr row

    # Colors

    label $frame.pcolor -bg $powbg -text "Color:" -font g_titleFont
    grid $frame.pcolor -in $frame -row $row -column 1 -sticky e -columnspan 2

    powColorFrame $frame.pColors powEditCurveParam(pColor${curve},new)
    grid $frame.pColors -in $frame -row $row -column 3 -columnspan 3 \
	  -sticky w -padx 8
    incr row

    # Add a separator line

    frame $frame.sep -bg $powbg -relief ridge -height 2 -bd 2
    grid $frame.sep -row $row -column 3 -columnspan 4 -sticky ew
    grid rowconfigure $frame $row -minsize 10 -weight 1
    incr row

    # Data Transform

    label $frame.lLog -bg $powbg -text "Transform:" -font g_titleFont
    grid $frame.lLog -in $frame -row $row -column 2 -sticky ne

    checkbutton $frame.xLog -bg $powbg -text "Log X" \
	    -variable powEditCurveParam(logX${curve},new) \
	    -highlightthickness 0 -onvalue Yes -offvalue No -font g_titleFont
    checkbutton $frame.yLog -bg $powbg -text "Log Y" \
	    -variable powEditCurveParam(logY${curve},new) \
	    -highlightthickness 0 -onvalue Yes -offvalue No -font g_titleFont
    grid $frame.xLog -in $frame -row $row -column 3 -sticky w -padx 4
    grid $frame.yLog -in $frame -row $row -column 4 -sticky w -padx 4
    incr row

    grid rowconfigure $frame $row -minsize 5
    incr row

    grid columnconfigure $frame 1 -minsize 10
    grid columnconfigure $frame 0 -minsize 5
    grid columnconfigure $frame [list 3 4 5 6] -weight 1
}


##################
#
# Setup Page: Lines
#
##################

proc powEditBuildCurveOptions2 { frame curve } {
    global powbg powEditCurveParam
    global g_titleFont

# If frame already exists delete its contents

    if {[winfo exists $frame]} {
       foreach i [winfo children $frame] {
	  destroy $i
       }
    } else {
       frame $frame -bg $powbg
    }
    
# Set default values if not defined for this curve
#puts "powEditCurveDefOptions 2"
    powEditCurveDefOptions $curve

# Build widgets

    set row 1

    grid rowconfigure $frame $row -minsize 5
    incr row

#
# Setup Lines Options
#

    # Line Display

    checkbutton $frame.lDisp -bg $powbg -text "Lines -" \
          -variable powEditCurveParam(lDisp${curve},new) \
          -highlightthickness 0 -onvalue Yes -offvalue No -font g_titleFont
    grid $frame.lDisp -in $frame -row $row -column 1 -sticky w -columnspan 2
    incr row

    grid rowconfigure $frame $row -minsize 10
    incr row

    # Style

    label $frame.style -bg $powbg -text "Style:" -font g_titleFont
    grid $frame.style -in $frame -row $row -column 2 -sticky ne

    set col 3
    foreach {style val} \
	    {Solid " " "Sm Dash" 10 "Lg Dash" 20 \
	     "Dotted" "4 4" "Dot Dash" "15 10 4 10"} {
	regsub -all { } $style {_} cln
	if {$col==6} {set col 3; incr row}
	radiobutton $frame.lStyle$cln -bg $powbg -text $style \
		-variable powEditCurveParam(lStyle${curve},new) -value $val \
		-highlightthickness 0 -takefocus 0 -font g_titleFont
	grid $frame.lStyle$cln -in $frame \
		-row $row -column $col -sticky nw -padx 4
	incr col
    }
    incr row

    grid rowconfigure $frame $row -minsize 7
    incr row

    # Line Width

    label $frame.width -bg $powbg -text "Width:" -font g_titleFont
    grid $frame.width -in $frame -row $row -column 2 -sticky ne

    set col 3
    foreach {size val} {Thin 1 Medium 2 Thick 3} {
	if {$col==6} {set col 3; incr row}
	radiobutton $frame.lWidth$size -bg $powbg -text $size \
		-variable powEditCurveParam(lWidth${curve},new) -value $val \
		-highlightthickness 0 -takefocus 0 -font g_titleFont
	grid $frame.lWidth$size -in $frame \
		-row $row -column $col -sticky nw -padx 4
	incr col
    }
    incr row

    grid rowconfigure $frame $row -minsize 7
    incr row

    # Histogram???

    label $frame.step -bg $powbg -text "Connect:" -font g_titleFont
    grid $frame.step -in $frame -row $row -column 2 -sticky ne

    set col 3
    foreach {style val} {"Normal" No "Histogram" Yes} {
	if {$col==6} {set col 3; incr row}
	radiobutton $frame.lStep$val -bg $powbg -text $style \
		-variable powEditCurveParam(lStep${curve},new) -value $val \
		-highlightthickness 0 -takefocus 0 -font g_titleFont
	grid $frame.lStep$val -in $frame \
		-row $row -column $col -sticky nw -padx 4
	incr col
    }
    checkbutton $frame.lBoxFill -bg $powbg -text "Fill Boxes" \
	  -variable powEditCurveParam(lBoxFill${curve},new) \
	  -onvalue Yes -offvalue No \
	  -highlightthickness 0 -takefocus 0 -font g_titleFont
    grid $frame.lBoxFill -row $row -column $col -sticky nw -padx 4
    incr row
	  
    grid rowconfigure $frame $row -minsize 7
    incr row

    # Color

    label $frame.color -bg $powbg -text "Color:" -font g_titleFont
    grid $frame.color -in $frame -row $row -column 1 -sticky e -columnspan 2

    powColorFrame $frame.lColors powEditCurveParam(lColor${curve},new)
    grid $frame.lColors -in $frame -row $row -column 3 -columnspan 3 \
	  -sticky w -padx 8
    incr row

    # Add a separator line

    frame $frame.sep -bg $powbg -relief ridge -height 2 -bd 2
    grid $frame.sep -row $row -column 3 -columnspan 3 -sticky ew
    grid rowconfigure $frame $row -minsize 10 -weight 1
    incr row

    # Data Transform

    label $frame.lLog -bg $powbg -text "Transform:" -font g_titleFont
    grid $frame.lLog -in $frame -row $row -column 2 -sticky ne

    checkbutton $frame.xLog -bg $powbg -text "Log X" \
	    -variable powEditCurveParam(logX${curve},new) \
	    -highlightthickness 0 -onvalue Yes -offvalue No -font g_titleFont
    checkbutton $frame.yLog -bg $powbg -text "Log Y" \
	    -variable powEditCurveParam(logY${curve},new) \
	    -highlightthickness 0 -onvalue Yes -offvalue No -font g_titleFont
    grid $frame.xLog -in $frame -row $row -column 3 -sticky w -padx 4
    grid $frame.yLog -in $frame -row $row -column 4 -sticky w -padx 4
    incr row

    grid rowconfigure $frame $row -minsize 5
    incr row

    grid columnconfigure $frame 1 -minsize 10
    grid columnconfigure $frame 0 -minsize 5
    grid columnconfigure $frame [list 3 4 5] -weight 1
}


##################
#
# Setup Page: Images
#
##################

proc powEditBuildImageOptions { frame image } {
    global powbg powEditImageParam powImageParam powRBmin powRBmax
    global g_titleFont

# If frame already exists delete its contents

    if {[winfo exists $frame]} {
       foreach i [winfo children $frame] {
	  destroy $i
       }
    } else {
       frame $frame -bg $powbg
    }
    
# Set default values if not defined for this curve

    powEditImageDefOptions $image

# Build widgets

    set row 1

    grid rowconfigure $frame $row -minsize 5
    incr row

#
# Setup Image Options
#

    # Colormap

    label $frame.cmap -bg $powbg -text "Colormap:" -font g_titleFont
    grid $frame.cmap -in $frame -row $row -column 2 -sticky ne

    foreach colorGrp $powImageParam(allMaps,powDef) {
       set col 3
       foreach cmap [lrange $colorGrp 1 end] {
	  if {$col>=7} {set col 3; incr row}
	  radiobutton $frame.cmap$cmap -bg $powbg -text $cmap \
		-variable powEditImageParam(colormap${image},new) -value $cmap \
		-highlightthickness 0 -takefocus 0 -font g_titleFont
	  grid $frame.cmap$cmap -in $frame \
		-row $row -column $col -sticky nw -padx 4
	  incr col
       }
       incr row
    }

    grid rowconfigure $frame $row -minsize 7
    incr row

    # Inverted?

    label $frame.inv -bg $powbg -text "Invert:" -font g_titleFont
    grid $frame.inv -in $frame -row $row -column 2 -sticky ne

    radiobutton $frame.invOn -bg $powbg -text Yes \
	    -variable powEditImageParam(invert${image},new) -value Yes \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
    radiobutton $frame.invOff -bg $powbg -text No \
	    -variable powEditImageParam(invert${image},new) -value No \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
    grid $frame.invOn -in $frame \
	    -row $row -column 3 -sticky nw -padx 4
    grid $frame.invOff -in $frame \
	    -row $row -column 4 -sticky nw -padx 4
    incr row

    grid rowconfigure $frame $row -minsize 7
    incr row

    # Scaling

    label $frame.scale -bg $powbg -text "Scaling:" -font g_titleFont
    grid $frame.scale -in $frame -row $row -column 2 -sticky ne

    set col 3
    foreach {lab val} [list Linear linear Square-Root sqrt Logarithmic log \
            "Histo Equalize" histo] {
	if {$col>=7} {set col 3; incr row}
	radiobutton $frame.scale$val -bg $powbg -text $lab \
		-variable powEditImageParam(scale${image},new) -value $val \
		-highlightthickness 0 -takefocus 0 -font g_titleFont
	grid $frame.scale$val -in $frame \
		-row $row -column $col -sticky nw -padx 4
	incr col
    }
    incr row

    if { $image != "" } {

       grid rowconfigure $frame $row -minsize 7
       incr row

       # Intensity range

       label $frame.range -bg $powbg -text "Range:" -font g_titleFont
       grid $frame.range -in $frame -row $row -column 2 -sticky e

       frame $frame.rng -bg $powbg
       entry $frame.rng.min -relief sunken -bg $powbg -width 12 \
	     -textvariable powEditImageParam(RBmin${image},new) -font g_titleFont
       label $frame.rng.to -bg $powbg -text "-" -font g_titleFont
       entry $frame.rng.max -relief sunken -bg $powbg -width 12 \
	     -textvariable powEditImageParam(RBmax${image},new)  -font g_titleFont
       
       button $frame.rng.reset -bg $powbg \
	     -text "Reset to [format "%.6g-%.6g" $powRBmin($image) \
	                                         $powRBmax($image)]" \
             -command "
                   set powEditImageParam(RBmin${image},new) $powRBmin($image)
                   set powEditImageParam(RBmax${image},new) $powRBmax($image)
             " -font g_titleFont
#    label $frame.rng.orig -bg $powbg \
#	  -text "Orig:

       pack $frame.rng.min -side left
       pack $frame.rng.to -side left
       pack $frame.rng.max -side left
       pack $frame.rng.reset -side left -padx 10

       grid $frame.rng -in $frame -row $row -column 3 -columnspan 4\
	     -sticky news -padx 4

    }

    grid rowconfigure $frame $row -minsize 5
    incr row

    grid columnconfigure $frame 1 -minsize 10
    grid columnconfigure $frame 0 -minsize 5
    grid columnconfigure $frame [list 3 4 5 6] -weight 1
}


########################################################################
#
#  Listbox manipulation
#

proc powEditAddElems { } {
    global powEditGraphName
    global powEditPlotParam powEditCurveParam
    global powDWP
    

    set elemNo  [${powDWP}gEdit.elemList.lst  curselection]
    if { $elemNo != "" } {

	set item [${powDWP}gEdit.elemList.lst get $elemNo]
	${powDWP}gEdit.elemList.lst delete $elemNo
	if { [${powDWP}gEdit.elemList.lst size]<=$elemNo } {
	    ${powDWP}gEdit.elemList.lst selection set end
	} else {
	    ${powDWP}gEdit.elemList.lst selection set $elemNo
	}
	${powDWP}gEdit.graphList.lst insert end $item
	${powDWP}gEdit.graphList.lst selection clear 0 end
	${powDWP}gEdit.graphList.lst selection set end
	foreach {name type} [split $item { }] {}
	if { $type=="(curve)" } {
	    if { $powEditPlotParam(curves,new)=="NULL" } {
		set powEditPlotParam(curves,new) $name
	    } else {
		lappend powEditPlotParam(curves,new) $name
	    }
	} else {
	    if { $powEditPlotParam(images,new)=="NULL" } {
		set powEditPlotParam(images,new) $name
	    } else {
		lappend powEditPlotParam(images,new) $name
	    }
	}

    }
    powEditBuildOptions
}

proc powEditRemoveElems { } {
    global powEditGraphName
    global powEditPlotParam powEditCurveParam
    global powDWP
    

    set graphNo [${powDWP}gEdit.graphList.lst curselection]
    if { $graphNo=="" } return

    if { $graphNo != "" } {

	set item [${powDWP}gEdit.graphList.lst get $graphNo]
	${powDWP}gEdit.graphList.lst delete $graphNo
	if { [${powDWP}gEdit.graphList.lst size]<=$graphNo } {
	    ${powDWP}gEdit.graphList.lst selection set end
	} else {
	    ${powDWP}gEdit.graphList.lst selection set $graphNo
	}
	${powDWP}gEdit.elemList.lst insert end $item
	${powDWP}gEdit.elemList.lst selection clear 0 end
	${powDWP}gEdit.elemList.lst selection set end
	foreach {name type} [split $item { }] {}
	if { $type=="(curve)" } {
	    set i [lsearch -exact $powEditPlotParam(curves,new) $name]
	    if { [llength $powEditPlotParam(curves,new)]==1 } {
		set powEditPlotParam(curves,new) NULL
	    } else {
		set powEditPlotParam(curves,new) \
			[lreplace $powEditPlotParam(curves,new) $i $i]
	    }
	} else {
	    set i [lsearch -exact $powEditPlotParam(images,new) $name]
	    if { [llength $powEditPlotParam(images,new)]==1 } {
		set powEditPlotParam(images,new) NULL
	    } else {
		set powEditPlotParam(images,new) \
			[lreplace $powEditPlotParam(images,new) $i $i]
	    }
	}

    }
    powEditBuildOptions
}

proc powEditUpdateListboxes {  } {
    global powEditGraphName
    global powEditPlotParam powEditCurveParam
    global powDWP
    
#puts "call powEditUpdateListboxes"
    ${powDWP}gEdit.graphList.lst delete 0 end
    ${powDWP}gEdit.elemList.lst  delete 0 end

    set curves ""
    set images ""
    foreach item $powEditPlotParam(curves,new) {
	if {$item!="NULL"} {
	    ${powDWP}gEdit.graphList.lst insert end "${item} (curve)"
	    lappend curves $item
	}
    }
    foreach item $powEditPlotParam(images,new) {
	if {$item!="NULL"} {
	    ${powDWP}gEdit.graphList.lst insert end "${item} (image)"
	    lappend images $item
	}
    }

    foreach item [powListCurves] {
	if { [lsearch -exact $curves $item]==-1 } {
	    ${powDWP}gEdit.elemList.lst insert end "${item} (curve)"
	}
    }
    foreach item [powListImages] {
	if { [lsearch -exact $images $item]==-1 } {
	    ${powDWP}gEdit.elemList.lst insert end "${item} (image)"
	}
    }
    if { [${powDWP}gEdit.elemList.lst size]>0 } {
	${powDWP}gEdit.elemList.lst selection set 0
    }
    if { [${powDWP}gEdit.graphList.lst size]>0 } {
	${powDWP}gEdit.graphList.lst selection set 0
    }
}

########################################################################
#
#  Variable management
#

proc powEditInitVariables { {flag "init"} } {
    global powEditGraphName powDWP
    global powPlotParam powCurveParam powImageParam powFontParam
    global powEditPlotParam powEditCurveParam powEditImageParam
    global powEditFontParam

#puts "flag: $flag, powEditGraphName: $powEditGraphName"
    if { $flag == "reset" } {
       foreach el [array names powPlotParam]  {
         set p1 [lindex [split $el ,] 0]
         set p2 [lindex [split $el ,] 1]
         if { $p2 == $powEditGraphName } {
            if { $flag == "reset" } {
               if [info exists powEditPlotParam($p1,powDef)] {
                 set powPlotParam($p1,$p2) $powEditPlotParam($p1,powDef)
               }
            }
         }
       }
    }

    # Start fresh
    catch {unset powEditPlotParam}
    catch {unset powEditCurveParam}
    catch {unset powEditImageParam}
    catch {unset powEditFontParam}

    foreach el [array names powPlotParam]  {
      set p1 [lindex [split $el ,] 0]
      set p2 [lindex [split $el ,] 1]
      if { $p2 == $powEditGraphName } {
         set powEditPlotParam($p1,old) $powPlotParam($p1,$p2)
         set powEditPlotParam($p1,new) $powPlotParam($p1,$p2)
      }
    }
    set powEditPlotParam(scalData,new) No

    # Hide some plot parameters
    foreach el [list xo yo handletext FixedAspect regions] {
       catch {unset powEditPlotParam($el,new)}
    }

    foreach el [array names powCurveParam]  {
      set p1 [lindex [split $el ,] 0]
      set p2 [lindex [split $el ,] 1]
      if { $p2 == $powEditGraphName } {
          set powEditCurveParam($p1,old) $powCurveParam($p1,$p2)
          set powEditCurveParam($p1,new) $powCurveParam($p1,$p2)
        }
    }

    foreach el [array names powImageParam]  {
      set p1 [lindex [split $el ,] 0]
      set p2 [lindex [split $el ,] 1]
      if { $p2 == $powEditGraphName } {
          set powEditImageParam($p1,old) $powImageParam($p1,$p2)
          set powEditImageParam($p1,new) $powImageParam($p1,$p2)
        }
    }

    foreach el [array names powFontParam]  {
      set p1 [lindex [split $el ,] 0]
      set p2 [lindex [split $el ,] 1]
      if { $p2 == $powEditGraphName } {
          set powEditFontParam($p1,old) $powFontParam($p1,$p2)
          set powEditFontParam($p1,new) $powFontParam($p1,$p2)
        }
    }

    if { [winfo exists ${powDWP}gEdit] } {
       powEditUpdateListboxes
    }
}

proc powEditUpdateVariables { } {
    global powEditGraphName
    global powPlotParam powCurveParam powImageParam powFontParam
    global powEditPlotParam powEditCurveParam powEditImageParam
    global powEditFontParam
    global powWCSLabel powEditObject

    # Set default values of graph's curves if not already defined
    # Must do this before updating powPlotParam
    #   because powEditCurveDefOptions references it

#puts "powEditPlotParam(curves,new): $powEditPlotParam(curves,new)"
    foreach crv $powEditPlotParam(curves,new) {
#puts "           call powEditCurveDefOptions: $crv"
	powEditCurveDefOptions $crv
    }

    foreach img $powEditPlotParam(images,new) {
	powEditImageDefOptions $img
    }

    ####
    # Now update the global hashes for the modified graph
    ####

    foreach el [array names powEditFontParam]  {
	set p1 [lindex [split $el ,] 0]
	set p2 [lindex [split $el ,] 1]
	if { $p2 == "new" } {
	   set powFontParam($p1,$powEditGraphName) $powEditFontParam($p1,new)
	}
    }

    powEditConvertToAxes

    foreach el [array names powEditPlotParam]  {
	set p1 [lindex [split $el ,] 0]
	set p2 [lindex [split $el ,] 1]

	if { $p2 == "new" } {
           if { ![info exists powEditPlotParam($p1,powDef)] && \
                 [info exists powPlotParam($p1,$powEditGraphName)] } {
              set powEditPlotParam($p1,powDef) $powPlotParam($p1,$powEditGraphName)
           }

if { $p1 == "xdimdisp" } {
#puts "       B powPlotParam($p1,$powEditGraphName): $powPlotParam($p1,$powEditGraphName)"
}
	   set powPlotParam($p1,$powEditGraphName) $powEditPlotParam($p1,new)
	   set powPlotParam($p1,${powEditGraphName}scope) $powEditPlotParam($p1,new)

if { $p1 == "xdimdisp" } {
#puts "       A powPlotParam($p1,$powEditGraphName): $powPlotParam($p1,$powEditGraphName)"
}
           switch $p1 {
                "xunits" -
                "yunits" -
                "xlabel" -
                "ylabel" {
                    set target $p1
                    if { [string first "unit" $p1] >= 0 } {
                       set target [string range $p1 0 4]
                    }
                    set powWCSLabel($target,$powEditGraphName,DEFAULT) $powEditPlotParam($p1,new)
                }
           }
	}
    }

    unset powPlotParam(scalData,$powEditGraphName)

    foreach el [array names powEditCurveParam]  {
	set p1 [lindex [split $el ,] 0]
	set p2 [lindex [split $el ,] 1]
	if { $p2 == "new" } {
	   set powCurveParam($p1,$powEditGraphName) $powEditCurveParam($p1,new)
	   set powCurveParam($p1,${powEditGraphName}scope) \
		 $powEditCurveParam($p1,new)
	}
    }


    foreach img $powEditPlotParam(images,new) {
       if { $img == "NULL" } continue
       # Check for new colormap scaling
       if { [info exists powImageParam(lut$img,$powEditGraphName)] \
	     && $powImageParam(scale$img,$powEditGraphName) \
	     != $powEditImageParam(scale$img,new) } {
	  unset powImageParam(lut$img,$powEditGraphName)
       }
       foreach {gn2 img2} [powGetColorbarLink $powEditGraphName $img] {}
       foreach opt [eval list $powImageParam(allOpts,powDef) RBmin RBmax] {
	  set powImageParam(${opt}${img},$powEditGraphName) \
		$powEditImageParam(${opt}${img},new)
	  set powImageParam(${opt}${img},${powEditGraphName}scope) \
		$powEditImageParam(${opt}${img},new)
          if { $gn2 != "" } {
              set powImageParam(${opt}${img2},${gn2}) \
                      $powEditImageParam(${opt}${img},new)
              set powImageParam(${opt}${img2},${gn2}scope) \
                      $powEditImageParam(${opt}${img},new)
          }
       }
       if { $gn2 != "" } {
           powSetColorTable $gn2 $img2
           powReditherImage $gn2 $img2
       }
    }
}

########################################################################

proc powEditConvertToAxes {} {
   global powEditPlotParam powEditCurveParam

   if { $powEditPlotParam(scalData,new) } {

      foreach crv $powEditPlotParam(curves,new) {
	 if { $crv=="NULL" } continue
	 if { $powEditPlotParam(xTickScal,new)=="log" } {
	    set powEditCurveParam(logX${crv},new) Yes
	 } else {
	    set powEditCurveParam(logX${crv},new) No
	 }
	 if { $powEditPlotParam(yTickScal,new)=="log" } {
	    set powEditCurveParam(logY${crv},new) Yes
	 } else {
	    set powEditCurveParam(logY${crv},new) No
	 }
      }

   }
}

########################################################################

proc powEditCurveDefOptions { curve } {
    global powEditCurveParam powCurveParam powPlotParam

#puts "curve: $curve"
    if { $curve == "NULL" || $curve == "" } return

    set crvGraph ""
    foreach opt $powCurveParam(allOpts,powDef) {
	if {! [info exists powEditCurveParam(${opt}${curve},new)]} {
	    if { $crvGraph=="" } {
		set crvGraph powDef
		foreach grph [powListGraphs] {
		    if { [regexp "scope$" $grph] } continue
		    if { [lsearch -exact $powPlotParam(curves,$grph) $curve] \
			    != -1 } {
			set crvGraph $grph
		    }
		}
	    }
	    if { $crvGraph=="powDef" } {
		set powEditCurveParam(${opt}${curve},new) \
			$powCurveParam(${opt},powDef)
		set powEditCurveParam(${opt}${curve},old) \
			$powCurveParam(${opt},powDef)
	    } else {
		set powEditCurveParam(${opt}${curve},new) \
			$powCurveParam(${opt}${curve},$crvGraph)
		set powEditCurveParam(${opt}${curve},old) \
			$powCurveParam(${opt}${curve},$crvGraph)
	    }
	}
    }
}

proc powEditImageDefOptions { image } {
    global powEditImageParam powImageParam powPlotParam
    global powRBmin powRBmax

    if { $image == "NULL" || $image == "" } return

    set imgGraph ""
    foreach opt $powImageParam(allOpts,powDef) {
	if {! [info exists powEditImageParam(${opt}${image},new)]} {
	    if { $imgGraph=="" } {
		set imgGraph powDef
		foreach grph [powListGraphs] {
		    if { [regexp "scope$" $grph] } continue
		    if { [lsearch -exact $powPlotParam(images,$grph) $image] \
			    != -1 } {
			set imgGraph $grph
		    }
		}
	    }
	    if { $imgGraph=="powDef" } {
		set powEditImageParam(${opt}${image},new) \
			$powImageParam(${opt},powDef)
		set powEditImageParam(${opt}${image},old) \
			$powImageParam(${opt},powDef)
	    } else {
		set powEditImageParam(${opt}${image},new) \
			$powImageParam(${opt}${image},$imgGraph)
		set powEditImageParam(${opt}${image},old) \
			$powImageParam(${opt}${image},$imgGraph)
	    }
	}
    }

    # Must also worry about RBmin and RBmax which don't have powDef defaults

    if {! [info exists powEditImageParam(RBmin${image},new)]} {
       if { $imgGraph=="" || $imgGraph=="powDef" } {
	  set powEditImageParam(RBmin${image},new) \
		$powRBmin($image)
	  set powEditImageParam(RBmin${image},old) \
		$powRBmin($image)
	  set powEditImageParam(RBmax${image},new) \
		$powRBmax($image)
	  set powEditImageParam(RBmax${image},old) \
		$powRBmax($image)
       } else {
	  set powEditImageParam(RBmin${image},new) \
		$powImageParam(RBmin${image},$imgGraph)
	  set powEditImageParam(RBmin${image},old) \
		$powImageParam(RBmin${image},$imgGraph)
	  set powEditImageParam(RBmax${image},new) \
		$powImageParam(RBmax${image},$imgGraph)
	  set powEditImageParam(RBmax${image},old) \
		$powImageParam(RBmax${image},$imgGraph)
       }
    }
}

###########################################################################
#                                                                         #
#               Routines for editting Objects                             #
#                                                                         #
###########################################################################

proc powEditObjectDlg {  } {
    global powPlotParam powCurveParam powbg
    global powEditObject
    global powDWP

    global g_titleFont

    if {[winfo exists ${powDWP}object]} {destroy ${powDWP}object}
    powToplevel ${powDWP}object .pow "-bg $powbg"
    bind ${powDWP}object <<CloseWindow>> "powEditExitObjDlg"
    catch {wm title ${powDWP}object "Edit Objects"}

    label ${powDWP}object.objectCont -text "All Objects:" -fg black -bg yellow -font g_titleFont
    frame ${powDWP}object.objectList -bg $powbg
    scrollbar ${powDWP}object.objectList.scrolly -orient vertical -takefocus 0 \
	    -command {global powDWP ; ${powDWP}object.objectList.lst yview} -bg $powbg
    listbox ${powDWP}object.objectList.lst -bg $powbg -width 25 -height 6 \
	    -selectmode browse -exportselection 0 -takefocus 0 \
	    -yscrollcommand {global powDWP ; ${powDWP}object.objectList.scrolly set } -font g_titleFont
    bind ${powDWP}object.objectList.lst <ButtonRelease-1> \
	    { powEditUpdateObject }

    grid ${powDWP}object.objectList.lst     -in ${powDWP}object.objectList \
	    -row 1 -column 1 -sticky news
    grid ${powDWP}object.objectList.scrolly -in ${powDWP}object.objectList \
	    -row 1 -column 2 -sticky news
    grid rowconfigure    ${powDWP}object.objectList 1 -weight 1
    grid columnconfigure ${powDWP}object.objectList 1 -weight 1

    label ${powDWP}object.dataCont -text "All Data:" -fg black -bg yellow -font g_titleFont
    frame ${powDWP}object.dataList -bg $powbg
    scrollbar ${powDWP}object.dataList.scrolly -orient vertical -takefocus 0 \
	    -command {global powDWP ; ${powDWP}object.dataList.lst yview} -bg $powbg
    listbox ${powDWP}object.dataList.lst -bg $powbg -width 25 -height 6 \
	    -selectmode browse -exportselection 0 -takefocus 0 \
	    -yscrollcommand {global powDWP ; ${powDWP}object.dataList.scrolly set } -font g_titleFont

    grid ${powDWP}object.dataList.lst     -in ${powDWP}object.dataList -row 1 -column 1 \
	    -sticky news
    grid ${powDWP}object.dataList.scrolly -in ${powDWP}object.dataList -row 1 -column 2 \
	    -sticky news
    grid rowconfigure    ${powDWP}object.dataList 1 -weight 1
    grid columnconfigure ${powDWP}object.dataList 1 -weight 1

    button ${powDWP}object.help -bg $powbg -text "Help" \
	    -command {powHelp EditObjects.html} -takefocus 0 -font g_titleFont

    grid ${powDWP}object.objectCont -in ${powDWP}object -row 1 -column 1 -sticky w \
	    -padx 5 -pady 5
    grid ${powDWP}object.objectList -in ${powDWP}object -row 2 -column 1 -sticky news
    grid ${powDWP}object.help       -in ${powDWP}object -row 1 -column 2
    grid ${powDWP}object.dataCont   -in ${powDWP}object -row 1 -column 3 -sticky w \
	    -padx 5 -pady 5
    grid ${powDWP}object.dataList   -in ${powDWP}object -row 2 -column 3 -sticky news

    button ${powDWP}object.editData -text "Edit Data" -bg $powbg -takefocus 0 \
	    -command {
	if { [info exists powDontPush] } {
	    puts "I told you not to push that button again."
	} else {
	    puts "Don't push that button again."
	    set powDontPush 1
	}
    } -font g_titleFont
    #grid ${powDWP}object.editData -in ${powDWP}object -row 3 -column 3 -sticky n -pady 2

    frame ${powDWP}object.objectName -bg $powbg
    label ${powDWP}object.objectName.lab -bg yellow -fg black -text "Object Name:" -font g_titleFont
    entry ${powDWP}object.objectName.ent -textvariable powEditObject(name) \
	    -bg $powbg -font g_titleFont

    pack ${powDWP}object.objectName.lab -in ${powDWP}object.objectName -side left
    pack ${powDWP}object.objectName.ent -in ${powDWP}object.objectName -side left -padx 5
    grid ${powDWP}object.objectName     -in ${powDWP}object -row 3 -column 1 \
	    -columnspan 2 -sticky w -padx 18

    frame ${powDWP}object.objectType -bg $powbg -relief ridge -bd 3
    label ${powDWP}object.objectType.lab -bg yellow -fg black -text "Object Type:" -font g_titleFont
    radiobutton ${powDWP}object.objectType.curve -bg $powbg -text Curve \
	    -variable powEditObject(type) -value "(curve)" \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
    radiobutton ${powDWP}object.objectType.image -bg $powbg -text Image \
	    -variable powEditObject(type) -value "(image)" \
	    -highlightthickness 0 -takefocus 0 -font g_titleFont
    set powEditObject(type) "(curve)"

    pack ${powDWP}object.objectType.lab -in ${powDWP}object.objectType -side left
    pack ${powDWP}object.objectType.curve -in ${powDWP}object.objectType \
	    -side left -padx 10
    pack ${powDWP}object.objectType.image -in ${powDWP}object.objectType \
	    -side left -padx 10
    grid ${powDWP}object.objectType -in ${powDWP}object -row 4 -column 1 -sticky sew \
	    -columnspan 3 -padx 15

    frame ${powDWP}object.btns -bg $powbg
    button ${powDWP}object.btns.create -text "Create Object" -bg $powbg \
	    -command {powEditCreateObject} -font g_titleFont
    button ${powDWP}object.btns.reload -text "Reload Info" -bg $powbg \
	    -command {powEditLoadObjects; powEditBuildObject} -font g_titleFont
    button ${powDWP}object.btns.exit -text "Exit"  -bg $powbg \
	    -command powEditExitObjDlg -font g_titleFont

    pack ${powDWP}object.btns.create -in ${powDWP}object.btns -side left \
	    -padx 5 -pady 2
    pack ${powDWP}object.btns.reload -in ${powDWP}object.btns -side left \
	    -padx 5 -pady 2
    pack ${powDWP}object.btns.exit   -in ${powDWP}object.btns -side left \
	    -padx 5 -pady 2
    grid ${powDWP}object.btns        -in ${powDWP}object -row 6 -column 1 -columnspan 3 \
	    -pady 5

    grid rowconfigure    ${powDWP}object 2 -weight 1
    grid rowconfigure    ${powDWP}object 5 -weight 1

    grid columnconfigure ${powDWP}object 1 -weight 1
    grid columnconfigure ${powDWP}object 3 -weight 1

    powEditLoadObjects
    powEditBuildObject
    trace variable powEditObject(type) w powChangeBuildObject
}

proc powEditExitObjDlg { } {
    global powDWP
    destroy ${powDWP}object
}


proc powEditLoadObjects { {init 1} } {
    global powDWP
    
    ${powDWP}object.objectList.lst delete 0 end
    ${powDWP}object.dataList.lst delete 0 end

    set last "NULL (curve)"
    foreach itm [powListCurves] {
	set last "$itm (curve)"
	${powDWP}object.objectList.lst insert end $last
    }

    foreach itm [powListImages] {
	set last "$itm (image)"
	${powDWP}object.objectList.lst insert end $last
    }
    
    foreach itm [powListData] {
	${powDWP}object.dataList.lst insert end "$itm ([powFetchDataLength $itm])"
    }
    ${powDWP}object.dataList.lst insert end "NULL (0)"
    ${powDWP}object.dataList.lst selection set 0

    if {$init} {
	${powDWP}object.objectList.lst selection set end
	eval [concat powEditInitObject $last]
    }
}

proc powEditInitObject { obj typ } {
    global powEditObject powWCS
    global powRotation

    if {$typ == "(curve)"} {
	set loc [lsearch -exact [powListCurves] $obj]
    } else {
	set loc [lsearch -exact [powListImages] $obj]
    }

    set powEditObject(name) $obj
    set powEditObject(type) $typ

    # Initialize all the entries to standard default NULLS or values

    foreach el [list xdata xedata xunits ydata yedata yunits \
	             zdata zedata zunits] {
        set powEditObject($el) NULL
    }
    foreach el [list xdim ydim xorigin yorigin xinc yinc] {
        set powEditObject($el) 1
    }

    set powEditObject(wcs) 0
    foreach el [list xref yref xrefpix yrefpix xinc yinc rot ctype] {
	set powEditObject(wcs$el) " "
    }

    if ![info exists powRotation($obj)] {
       set powRotation($obj) 0.0
       #powChangeFitsHeaderKeyWordValue $obj {"CROAT2"} "X" $powRotation($obj)
    }

    set powEditObject(wcsrot) $powRotation($obj)

    # Now fill in the known quantities

    if { $obj!="NULL" && $loc!=-1 } {

	if { $typ=="(image)" } {
	    array set powEditObject [powFetchImageInfoHash $obj]
	    set powEditObject(xdata) $powEditObject(data)
	    set powEditObject(xdim) $powEditObject(width)
	    set powEditObject(ydim) $powEditObject(height)
	} else {
	    array set powEditObject [powFetchCurveInfoHash $obj]
	    foreach {vec lc} {X x Y y XE xe YE ye} {
		if { $powEditObject($vec) != "NULL" } {
		    array set tmpArray \
			    [powFetchVectorInfoHash $powEditObject($vec)]
		    set powEditObject(${lc}data) $tmpArray(data)
		    set powEditObject(${lc}units) $tmpArray(units)
		} else {
		    set powEditObject(${lc}data) NULL
		    set powEditObject(${lc}units) NULL
		}
	    }
	}

	if {[info exists powWCS($obj)] && $powWCS($obj) != "" } {
	    set powEditObject(wcs) 1
	    set wcslist $powWCS($obj)
            # wcsinfo : {xrefvalue yrefvalue} {xrefpix yrefpix} {cdelt} {unit} {type}
            set powEditObject(wcsxref) [lindex [lindex $wcslist 0] 0]
            set powEditObject(wcsyref) [lindex [lindex $wcslist 0] 1]
            set powEditObject(wcsxrefpix) [lindex [lindex $wcslist 1] 0]
            set powEditObject(wcsyrefpix) [lindex [lindex $wcslist 1] 1]
            set powEditObject(wcsxinc) [lindex [lindex $wcslist 2] 0]
            set powEditObject(wcsxrot_cal) [lindex [lindex $wcslist 2] 1]
            set powEditObject(wcsyrot_cal) [lindex [lindex $wcslist 2] 2]
            set powEditObject(wcsyinc) [lindex [lindex $wcslist 2] 3]
            set powEditObject(wcsxunit) [lindex [lindex $wcslist 3] 0]
            set powEditObject(wcsyunit) [lindex [lindex $wcslist 3] 1]
            set powEditObject(wcsctype) [lindex [lindex $wcslist 4] 0]
	}
    }
}

proc powEditCreateObject { } {
    global powEditObject powWCS powFitsHeader powFitsHeaderCnt
    global powDWP g_magification powPlotParam powRotation
    global xCount yCount

    set obj $powEditObject(name)
    set wcslist {}
    if { $powEditObject(wcs) } {
       set wcsxinc $powEditObject(wcsxinc)
       set wcsyinc $powEditObject(wcsyinc)
       if { $powRotation($obj) != 0.0 } {
          # restore xinc yinc back to rotation 0.0 degree
          set wcsxinc [expr $powEditObject(wcsxrot_cal) / cos($powRotation($obj))]
          set wcsyinc [expr $powEditObject(wcsyrot_cal) / sin($powRotation($obj))]
       }

       # get user input rotation
       set powRotation($obj) $powEditObject(wcsrot)
       set powEditObject(wcsxrot_cal) [expr $wcsxinc * cos($powRotation($obj))]
       set powEditObject(wcsyrot_cal) [expr $wcsyinc * sin($powRotation($obj))]

       lappend wcslist [list $powEditObject(wcsxref) $powEditObject(wcsyref)]
       lappend wcslist [list $powEditObject(wcsxrefpix) $powEditObject(wcsyrefpix)]
       lappend wcslist [list $powEditObject(wcsxinc) $powEditObject(wcsxrot_cal) \
                             $powEditObject(wcsyrot_cal) $powEditObject(wcsyinc)]
       lappend wcslist [list $powEditObject(wcsxunit) $powEditObject(wcsyunit)]
       lappend wcslist [list $powEditObject(wcsctype) $powEditObject(wcsctype)]

       set graphHandle $obj
       if { $powEditObject(type)=="(curve)" } {
          # get rid of "c1_" for curve
          set graphHandle [string range $obj 3 end]
       }

       powChangeFitsHeaderKeyWordValue $graphHandle {"CTYPE1" "CTYPE2" "CDELT1" "CDELT2" "CROTA2" } \
                                       "X" \
                                       [list [format "RA--%s" $powEditObject(wcsctype)] \
                                             [format "DEC-%s" $powEditObject(wcsctype)] \
                                             $powEditObject(wcsxinc) \
                                             $powEditObject(wcsyinc) \
                                             $powRotation($obj)] \
                                       [list $powEditObject(wcsxref) $powEditObject(wcsyref)]
                         
       set powFitsHeader($obj) $powFitsHeader($graphHandle)
       set powFitsHeaderCnt($obj) $powFitsHeaderCnt($graphHandle)
    }
    set powFitsHeader(${obj}scope) $powFitsHeader($obj)
    set powFitsHeaderCnt(${obj}scope) $powFitsHeaderCnt($obj)

    set powWCS($obj) $wcslist
    set powWCS(${obj}scope) $wcslist

    if { $powEditObject(type)=="(curve)" } {
        set powFitsHeaderStr $powFitsHeader($obj)
        set powFitsHeaderCntValue $powFitsHeaderCnt($obj)

        catch { powDeleteGraph $graphHandle NOPROMPT
                powDeleteImage $graphHandle $graphHandle
                powDeleteCurve $graphHandle curve }

        set powWCS($obj) $wcslist
        set powWCS($graphHandle) $wcslist
        set powWCS(${graphHandle}scope) $wcslist

        set powFitsHeader($obj) $powFitsHeaderStr
        set powFitsHeaderCnt($obj) $powFitsHeaderCntValue
        set powFitsHeader(${graphHandle}scope) $powFitsHeaderStr
        set powFitsHeaderCnt(${graphHandle}scope) $powFitsHeaderCntValue

        set powFitsHeader($graphHandle) $powFitsHeader($obj)
        set powFitsHeaderCnt($graphHandle) $powFitsHeaderCnt($obj)

        set powPlotParam(graphType,$obj) "binary"
        set powPlotParam(graphType,${obj}scope) "binary"
        set powPlotParam(graphType,$graphHandle) "binary"
        set powPlotParam(graphType,${graphHandle}scope) "binary"
        set powPlotParam(zoomed,${graphHandle}) 0
        set powPlotParam(zoomed,${graphHandle}scope) 0
        set powPlotParam(zoomed,${obj}) 0
        set powPlotParam(zoomed,${obj}scope) 0
        set xCount(${obj}) 0
        set yCount(${obj}) 0
        set xCount(${obj}scope) 0
        set yCount(${obj}scope) 0
        set xCount(${graphHandle}) 0
        set yCount(${graphHandle}) 0
        set xCount(${graphHandle}scope) 0
        set yCount(${graphHandle}scope) 0

	set vectors ""
	foreach vec { x xe y ye } {
	    if { $powEditObject(${vec}data) == "NULL" } {
		lappend vectors NULL
	    } else {
		powCreateVector ${obj}_${vec}data $powEditObject(${vec}data) 0 \
			NULL $powEditObject(${vec}units)
		lappend vectors ${obj}_${vec}data
	    }
	}
	eval [concat powCreateCurve $obj $vectors]
	eval [concat powCreateGraph $graphHandle $obj NULL \
                                    NULL NULL \
                                    $powPlotParam(xlabel,$graphHandle) \
                                    $powPlotParam(ylabel,$graphHandle) \
                                    [lindex $fvPref::graphDispSize 0] \
                                    [lindex $fvPref::graphDispSize 1]]

    } else {

	if { $powEditObject(wcs) } {
	    powCreateImage $obj $powEditObject(xdata) \
		    0 0 \
		    $powEditObject(xdim) $powEditObject(ydim) \
		    1 1 1 1 \
		    $powEditObject(xunits) $powEditObject(yunits) \
		    $powEditObject(zunits)
	} else {
	    powCreateImage $obj $powEditObject(xdata) \
		    0 0 \
		    $powEditObject(xdim) $powEditObject(ydim) \
		    $powEditObject(xorigin) $powEditObject(xinc) \
		    $powEditObject(yorigin) $powEditObject(yinc) \
		    $powEditObject(xunits) $powEditObject(yunits) \
		    $powEditObject(zunits)
	}
        powEndROI 1

    }
    powEditLoadObjects 0
    set elem "$obj $powEditObject(type)"
    set num [lsearch -exact [${powDWP}object.objectList.lst get 0 end] $elem]
    ${powDWP}object.objectList.lst selection set $num
    eval [concat powEditInitObject $elem]
}

proc powEditUpdateObject { } {
    global powDWP

    set itm [${powDWP}object.objectList.lst curselection]
    if {$itm==""} {
	powEditInitObject NULL "(curve)"
    } else {
	eval [concat powEditInitObject [${powDWP}object.objectList.lst get $itm]]
    }
    powEditBuildObject
}

proc powChangeBuildObject { a b c } {
    powEditBuildObject
}

proc powEditBuildObject { } {
    global powEditObject powbg
    global powDWP
    
    if { $powEditObject(type)=="(curve)" } {
	powEditBuildCurve ${powDWP}object.curveFrame
	${powDWP}object.curveFrame configure -borderwidth 3 -relief ridge
	grid ${powDWP}object.curveFrame -in ${powDWP}object -row 5 -column 1 \
		-padx 15 -pady 5 -columnspan 3 -sticky news
	catch {grid remove ${powDWP}object.imageFrame}
    } else {
	powEditBuildImage ${powDWP}object.imageFrame
	${powDWP}object.imageFrame configure -borderwidth 3 -relief ridge
	grid ${powDWP}object.imageFrame -in ${powDWP}object -row 5 -column 1 \
		-padx 15 -pady 5 -columnspan 3 -sticky news
	catch {grid remove ${powDWP}object.curveFrame}
    }
}

proc powEditUpdateDataButton { btn var } {
    global powEditObject
    global powDWP

    set elem [${powDWP}object.dataList.lst curselection]
    if { $elem=="" } return

    set data [lindex [${powDWP}object.dataList.lst get $elem] 0]
    set powEditObject($var) $data
    $btn configure -text $data
}

proc powEditBuildCurve { frame } {
    global powEditObject powbg
    global g_titleFont

    if { [winfo exists $frame] } {
	foreach {lbl var} [list Data: data Error: edata] {
	    $frame.bx$var configure -text $powEditObject(x$var)
	    $frame.by$var configure -text $powEditObject(y$var)
	}
	return
    }
    frame $frame -bg $powbg

    set row 1
    grid columnconfigure $frame 1 -minsize 10
    grid columnconfigure $frame 4 -minsize 10

    label $frame.x -bg $powbg -text X -font g_titleFont
    label $frame.y -bg $powbg -text Y -font g_titleFont
    grid $frame.x -in $frame -row $row -column 3 -sticky s
    grid $frame.y -in $frame -row $row -column 5 -sticky s
    incr row

    # Create data buttons

    foreach {lbl var} [list Data: data Error: edata] {
	label $frame.l$var -bg $powbg -text $lbl -font g_titleFont
	grid $frame.l$var -in $frame -row $row -column 2 -sticky e -padx 10
	button $frame.bx$var -bg $powbg -text $powEditObject(x$var) \
		-command "powEditUpdateDataButton $frame.bx$var x$var" -font g_titleFont
	button $frame.by$var -bg $powbg -text $powEditObject(y$var) \
		-command "powEditUpdateDataButton $frame.by$var y$var" -font g_titleFont
	grid $frame.bx$var -in $frame -row $row -column 3 -sticky news
	grid $frame.by$var -in $frame -row $row -column 5 -sticky news
	incr row
    }

    foreach {lbl var} [list Units: units] {
	label $frame.l$var -bg $powbg -text $lbl -font g_titleFont
	grid $frame.l$var -in $frame -row $row -column 2 -sticky e -padx 10
	entry $frame.ex$var -bg $powbg -textvariable powEditObject(x$var) \
		 -takefocus 1 -font g_titleFont
	entry $frame.ey$var -bg $powbg -textvariable powEditObject(y$var) \
		 -takefocus 1 -font g_titleFont
	grid $frame.ex$var -in $frame -row $row -column 3 -sticky news
	grid $frame.ey$var -in $frame -row $row -column 5 -sticky news
	incr row
    }

    grid rowconfigure $frame $row -minsize 10
    incr row

    powEditBuildWCS $frame $row
}

proc powEditBuildImage { frame } {
    global powEditObject powbg
    global g_titleFont

    if { [winfo exists $frame] } {
	$frame.bdata configure -text $powEditObject(xdata)
	return
    }
    frame $frame -bg $powbg

    set row 1
    grid columnconfigure $frame 1 -minsize 10
    grid columnconfigure $frame 4 -minsize 10

    label $frame.ldata -bg $powbg -text "Data:" -font g_titleFont
    grid $frame.ldata -in $frame -row $row -column 2 -sticky e -padx 10
    button $frame.bdata -bg $powbg -text $powEditObject(xdata) \
	    -command "powEditUpdateDataButton $frame.bdata xdata;
	              set powEditObject(ydim) 1;
                      set powEditObject(xdim) \
                            \[powFetchDataLength \$powEditObject(xdata)\]" -font g_titleFont
    grid $frame.bdata -in $frame -row $row -column 3 -sticky w -columnspan 3
    incr row

    label $frame.lzunits -bg $powbg -text "Units:" -font g_titleFont
    grid $frame.lzunits -in $frame -row $row -column 2 -sticky e -padx 10
    entry $frame.ezunits -bg $powbg -textvariable powEditObject(zunits) \
	     -takefocus 1 -font g_titleFont
    grid $frame.ezunits -in $frame -row $row -column 3 -sticky w
    incr row

    grid rowconfigure $frame $row -minsize 10
    incr row

    label $frame.x -bg $powbg -text X -font g_titleFont
    label $frame.y -bg $powbg -text Y -font g_titleFont
    grid $frame.x -in $frame -row $row -column 3 -sticky s
    grid $frame.y -in $frame -row $row -column 5 -sticky s
    incr row

    foreach {lbl var} \
	    [list Dimensions: dim Origin: origin "Pixel Size:" inc Units: units] {
	label $frame.l$var -bg $powbg -text $lbl -font g_titleFont
	grid $frame.l$var -in $frame -row $row -column 2 -sticky e -padx 10
	entry $frame.ex$var -bg $powbg -textvariable powEditObject(x$var) \
		 -takefocus 1 -font g_titleFont
	entry $frame.ey$var -bg $powbg -textvariable powEditObject(y$var) \
		 -takefocus 1 -font g_titleFont
	grid $frame.ex$var -in $frame -row $row -column 3 -sticky news
	grid $frame.ey$var -in $frame -row $row -column 5 -sticky news
	incr row
    }

    grid rowconfigure $frame $row -minsize 10
    incr row

    powEditBuildWCS $frame $row
}

proc powEditBuildWCS { frame row } {
    global powEditObject powbg
    global g_titleFont

# Build WCS entries

    checkbutton $frame.wcs -bg $powbg -text "WCS Info -" \
	    -highlightthickness 0 -takefocus 0 \
	    -variable powEditObject(wcs) -font g_titleFont
    grid $frame.wcs -in $frame -row $row -column 1 -columnspan 2 -sticky w
    incr row
			      
    foreach {lbl var} \
	    [list "Ref Value:" ref "Ref Pixel:" refpix "Pixel Scale:" inc] {
	label $frame.lw$var -bg $powbg -text $lbl -font g_titleFont
	grid $frame.lw$var -in $frame -row $row -column 2 -sticky e -padx 10
	entry $frame.ewx$var -bg $powbg -textvariable powEditObject(wcsx$var) \
		 -takefocus 1 -font g_titleFont
	entry $frame.ewy$var -bg $powbg -textvariable powEditObject(wcsy$var) \
		 -takefocus 1 -font g_titleFont
	grid $frame.ewx$var -in $frame -row $row -column 3 -sticky news
	grid $frame.ewy$var -in $frame -row $row -column 5 -sticky news
	incr row
    }

    label $frame.lrot -bg $powbg -text "Rotation:" -font g_titleFont
    grid $frame.lrot -in $frame -row $row -column 2 -sticky e -padx 10
    entry $frame.erot -bg $powbg -textvariable powEditObject(wcsrot) \
	     -takefocus 1 -font g_titleFont
    grid $frame.erot -in $frame -row $row -column 3 -sticky news
    incr row

    label $frame.lproj -bg $powbg -text "Projection:" -font g_titleFont
    grid $frame.lproj -in $frame -row $row -column 2 -sticky ne -padx 10

    set pcol 1
    set prow 1
    frame $frame.projType -bg $powbg
    foreach {lbl val} \
      {AZP "-AZP" SZP "-SZP" TAN "-TAN" STG "-STG" SIN "-SIN" ARC "-ARC" ZPN "-ZPN" \
       ZEA "-ZEA" AIR "-AIR" CYP "-CYP" CEA "-CEA" CAR "-CAR" MER "-MER" COP "-COP" \
       COE "-COE" COD "-COD" COO "-COO" SFL "-SFL" PAR "-PAR" MOL "-MOL" AIT "-AIT" \
       BON "-BON" PCO "-PCO" TSC "-TSC" CSC "-CSC" QSC "-QSC" HPX "-HPX" NCP "-NCP"} {

	 if {$pcol==5} {set pcol 1; incr prow}
	 radiobutton $frame.projType.p$lbl -bg $powbg -text $lbl \
		 -variable powEditObject(wcsctype) -value $val \
		 -highlightthickness 0 -takefocus 0 -font g_titleFont
	 grid $frame.projType.p$lbl -in $frame.projType \
		 -row $prow -column $pcol -sticky nw -padx 6
	 incr pcol
    }

    grid $frame.projType -in $frame -row $row -column 3 -columnspan 3 -sticky w
    incr row
}

########################################################################
#                                                                      #
#              Functions to set Default Display Options                #
#                                                                      #
########################################################################

proc powDefaultOptions { } {
    global currgn powbg
    global powPlotParam  powEditPlotParam  powEditGraphName
    global powCurveParam powEditCurveParam
    global powImageParam powEditImageParam
    global powDWP
    global g_titleFont

    if {[winfo exists ${powDWP}default]} {destroy ${powDWP}default}
    powToplevel ${powDWP}default .pow "-bg $powbg"
    bind ${powDWP}default <<CloseWindow>> "powEditExitDefDlg"
    catch {wm title ${powDWP}default "POW Preferences"}
    
    set powEditGraphName powDef
    powEditInitDefVariables

    set note ${powDWP}default.nBook
    Notebook:create $note -pages {POW Graph Fonts Ticks Points Lines Images} \
	  -pad 4 -bg $powbg
    set w [Notebook:frame $note POW]
    powEditBuildPOWOptions $w
    set w [Notebook:frame $note Graph]
    powEditBuildGraphOptions $w
    set w [Notebook:frame $note Fonts]
    powEditBuildFontOptions $w
    set w [Notebook:frame $note Ticks]
    powEditBuildTickOptions $w
    set w [Notebook:frame $note Points]
    powEditBuildCurveOptions1 $w ""
    set w [Notebook:frame $note Lines]
    powEditBuildCurveOptions2 $w ""
    set w [Notebook:frame $note Images]
    powEditBuildImageOptions $w ""

# Setup Buttons

    button ${powDWP}default.help -text "Help" \
	    -command {powHelp DefaultOptions.html} \
	    -bg $powbg -takefocus 0 -font g_titleFont
    
    frame ${powDWP}default.buttons -bg $powbg
    button ${powDWP}default.buttons.save -text "Save" -bg $powbg \
	  -command {powEditUpdateDefVariables; powSaveConfig} -font g_titleFont
    button ${powDWP}default.buttons.curr -text "Get Current" -bg $powbg \
	  -command {powEditGetCurrVariables} -font g_titleFont
    button ${powDWP}default.buttons.reset -text "Reset" -bg $powbg \
	  -command powEditInitDefVariables -font g_titleFont
    button ${powDWP}default.buttons.exit -text "Exit" -bg $powbg \
	  -command { powEditExitDefDlg } -font g_titleFont
    pack ${powDWP}default.buttons.save  -side left -padx 4 -pady 3
    pack ${powDWP}default.buttons.curr  -side left -padx 4 -pady 3
    pack ${powDWP}default.buttons.reset -side left -padx 4 -pady 3
    pack ${powDWP}default.buttons.exit  -side left -padx 4 -pady 3

    label ${powDWP}default.lab -bg $powbg \
	  -text "Default options for new graphs:" -font g_titleFont

# Place everything into dialog

    grid ${powDWP}default.help -in ${powDWP}default -row 1 -column 1 -sticky ne
    grid ${powDWP}default.lab -in ${powDWP}default -row 2 -column 1
    grid ${powDWP}default.nBook -in ${powDWP}default -row 3 -column 1 \
	    -sticky news -padx 15 -pady 10
    grid ${powDWP}default.buttons -in ${powDWP}default -row 4 -column 1 -pady 8

    Notebook:resize $note
}

proc powEditExitDefDlg { } {
    global powDWP
    powEditUpdateDefVariables
    destroy ${powDWP}default
}


proc powEditInitDefVariables {} {
   global powPlotParam  powEditPlotParam
   global powCurveParam powEditCurveParam
   global powImageParam powEditImageParam
   global powFontParam  powEditFontParam
   global powEditAppParam

#puts "call powEditInitDefVariables"
   foreach opt $powPlotParam(allOpts,powDef) {
      set powEditPlotParam(${opt},new) $powPlotParam(${opt},powDef)
   }
   foreach opt $powCurveParam(allOpts,powDef) {
      set powEditCurveParam(${opt},new) $powCurveParam(${opt},powDef)
   }
   foreach opt $powImageParam(allOpts,powDef) {
      set powEditImageParam(${opt},new) $powImageParam(${opt},powDef)
   }

   foreach opt $powFontParam(allOpts,powDef) {
      foreach lbl $powFontParam(allTypes,powDef) {
         set powEditFontParam(${lbl}${opt},new) \
               $powFontParam(${lbl}${opt},powDef)
      }
   }

   foreach opt [list bg cursor ResizeMain GUIposition ScopeSize ] {
      set powEditAppParam(${opt},new) [subst \$::pow$opt]
   }
}

proc powEditGetCurrVariables {} {
   global powPlotParam powEditPlotParam currgn currimg
   global powCurveParam powEditCurveParam
   global powImageParam powEditImageParam
   global powFontParam  powEditFontParam

#puts "call powEditGetCurrVariables, currgn: <$currgn>"
   if { ![info exists currgn] || $currgn=="powDef" } return

   foreach opt $powPlotParam(allOpts,powDef) {
      set powEditPlotParam(${opt},new) $powPlotParam(${opt},$currgn)
   }

   if { [info exists currimg] && $currimg != "" } {
      foreach opt $powImageParam(allOpts,powDef) {
	 set powEditImageParam(${opt},new) \
	       $powImageParam(${opt}${currimg},$currgn)
      }
   }

   set crv [lindex $powPlotParam(curves,$currgn) 0]
   if { $crv != "NULL" } {
      foreach opt $powCurveParam(allOpts,powDef) {
	 set powEditCurveParam(${opt},new) \
	       $powCurveParam(${opt}${crv},$currgn)
      }
   }

   foreach opt $powFontParam(allOpts,powDef) {
      foreach lbl $powFontParam(allTypes,powDef) {
         set powEditFontParam(${lbl}${opt},new) \
               $powFontParam(${lbl}${opt},$currgn)
      }
   }
}

proc powEditUpdateDefVariables {} {
   global powPlotParam  powEditPlotParam
   global powCurveParam powEditCurveParam
   global powImageParam powEditImageParam
   global powFontParam  powEditFontParam
   global powEditAppParam

   foreach opt $powPlotParam(allOpts,powDef) {
      set powPlotParam(${opt},powDef) $powEditPlotParam(${opt},new)
   }
   foreach opt $powCurveParam(allOpts,powDef) {
      set powCurveParam(${opt},powDef) $powEditCurveParam(${opt},new)
   }
   foreach opt $powImageParam(allOpts,powDef) {
      set powImageParam(${opt},powDef) $powEditImageParam(${opt},new)
   }
   foreach opt $powFontParam(allOpts,powDef) {
      foreach lbl $powFontParam(allTypes,powDef) {
         set powFontParam(${lbl}${opt},powDef) \
               $powEditFontParam(${lbl}${opt},new)
      }
   }
   foreach opt [list bg cursor ResizeMain GUIposition ScopeSize ] {
      if { [subst \$::pow$opt] != $powEditAppParam(${opt},new) } {
         powSetGlobal_$opt $powEditAppParam(${opt},new)
      }
   }
}

########################################################################
####
####  Color Frame "Widget"
####
########################################################################

proc powColorFrame { frame varName } {
   global powbg
   global g_titleFont
   upvar #0 $varName var

   set hex [list "00" "99" "FF"]
   set nClr [llength $hex]
   frame $frame -bg $powbg

   frame $frame.grid -relief ridge -bd 3 -bg $powbg
   for { set i 0 } { $i<$nClr } { incr i 1 } {
      for { set j 0 } { $j<$nClr } { incr j 1 } {
	 for { set k 0 } { $k<$nClr } { incr k 1 } {
	    set color "[lindex $hex $i][lindex $hex $j][lindex $hex $k]"
	    frame $frame.grid.c$color -width 24 -height 24 -bg "#$color" \
		  -bd 4 -relief flat -takefocus 0
	    grid $frame.grid.c$color -row $k -column [expr $j + $i*$nClr]
	    bind $frame.grid.c$color <ButtonPress-1> \
		  "powUpdateColorFrame $frame #$color $varName"
	 }
      }
   }

   button $frame.clrbtn -textvariable $varName \
         -bg $powbg -highlightthickness 0 -width 7 \
         -font g_titleFont \
         -command "powUpdateColorFrame $frame \
                      \[powSelectAndReturnColor \$$varName\] $varName"

   if { ![info exists var] } {
      # Set initial value to black
      set var "#000000"
   } 

   grid $frame.grid -row 1 -column 1
   grid $frame.clrbtn -row 1 -column 2 -padx 7

   powUpdateColorFrame $frame $var $varName
   trace variable ::$varName w "powColorVarHasChanged $frame"
}

proc powColorVarHasChanged { f varName varIndex op } {
   if { $varName=="var" } return
   if { $varIndex != "" } {
      set varName ${varName}($varIndex)
   }
   if { [winfo exists $f.grid] } {
      foreach c [winfo child $f.grid] {
         $c configure -relief flat
      }
   }
   powUpdateColorFrame $f [subst \$::$varName] $varName
}

proc powSelectAndReturnColor { c } {
   set newClr [tk_chooseColor -initialcolor $c]
   if { $newClr != "" } {
      set c $newClr
   }
   return $c
}


proc powUpdateColorFrame { f color varName } {
   upvar #0 $varName var

   set lvar [string range $var 1 end]
   if { [winfo exists $f.grid.c$lvar] } {
      $f.grid.c$lvar configure -relief flat
   }

   set var "$color"
   set lvar [string range $var 1 end]
   if { [winfo exists $f.grid.c$lvar] } {
      $f.grid.c$lvar configure -relief raised
   }
   if { [winfo exists $f.clrbtn] } {
      $f.clrbtn configure -bg $color
   }
}
