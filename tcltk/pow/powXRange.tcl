###################################################################
#
# TCL routines handling creation and manipulation of SAO XRanges
#
###################################################################

# variables used:
#     xRangeParam(rgns):  XRangeList object containing objects

proc powXRange { } {
    global heraQueryEntry g_fvHera
    global powPlotParam xRangeParam powbg currgn currimg
    global powFrameForTop
    global powDWP
    global g_titleFont
    global powLutButton powROIButton 
    global powLutButton_old powROIButton_old
    global CpowXRangeX0 CpowXRangeX1 CpowXRangeY0 CpowXRangeY1
    global TpowXRangeX0 TpowXRangeX1 TpowXRangeY0 TpowXRangeY1
    global staticY staticYonG availableColor

    set lineWidth 10.0
    set coord [.pow.pow coords ${currgn}box]

    set x0 [lindex $coord 0]
    set y0 [lindex $coord 3]
    set x1 [lindex $coord 2]
    set y1 [lindex $coord 1]

    set CpowXRangeX0 $x0
    set CpowXRangeX1 $x1
    set CpowXRangeY0 $y0
    set CpowXRangeY1 $y1

    set staticY    [expr ($CpowXRangeY0 + $CpowXRangeY1) / 2.0]
    set staticYonG [lindex [powCanvasToGraph $currgn $x0 $staticY .pow.pow] 1]

    set TpowXRangeX0 $x0
    set TpowXRangeX1 $x1
    set TpowXRangeY0 $y0
    set TpowXRangeY1 $y1

    if {[winfo exists ${powDWP}xRange]} {
	catch {raise ${powDWP}xRange}
	return
    }

    powToplevel ${powDWP}xRange .pow "-bg $powbg"
    bind ${powDWP}xRange <Leave> "powXRangeChangeValue all -1.0"
    bind ${powDWP}xRange <<CloseWindow>> "destroy ${powDWP}xRange"
    catch {wm title ${powDWP}xRange "Edit X Range"}
    catch {wm geometry ${powDWP}xRange +500+240}

    frame ${powDWP}xRange.head -bg $powbg
    label ${powDWP}xRange.head.title -bg yellow -fg black -font g_titleFont
    button ${powDWP}xRange.head.help -text "Help" \
	    -command {powHelp XRange.html} \
	    -bg $powbg -highlightthickness 0 -font g_titleFont
    pack ${powDWP}xRange.head.title -side left
    pack ${powDWP}xRange.head.help  -side right -padx 2 -pady 2

    frame ${powDWP}xRange.list -bg $powbg
    scrollbar ${powDWP}xRange.list.scrolly -orient vertical \
	    -command {${powDWP}xRange.list.rgns yview} -bg $powbg
    scrollbar ${powDWP}xRange.list.scrollx -orient horizontal \
	    -command {${powDWP}xRange.list.rgns xview} -bg $powbg
    listbox ${powDWP}xRange.list.rgns -width 30 -height 6 \
	    -selectmode browse -bg $powbg -exportselection 0 \
	    -yscrollcommand "${powDWP}xRange.list.scrolly set " \
	    -xscrollcommand "${powDWP}xRange.list.scrollx set " -font g_titleFont
    bind ${powDWP}xRange.list.rgns <ButtonRelease-1> \
	    {powSelectXRange [${powDWP}xRange.list.rgns curselection]}
    button ${powDWP}xRange.delete -text "Delete" -bg $powbg \
            -command {powDeleteCurrXRange} -highlightthickness 0 -font g_titleFont

    frame ${powDWP}xRange.property -relief groove

    button ${powDWP}xRange.propertyButton -text "Show Graph Utility" -font g_titleFont \
                                          -command { togglePropertyList }

    label ${powDWP}xRange.property.label -text "Change new graph property: " -font g_titleFont 
    tixControl ${powDWP}xRange.property.lineWidth -label "Line Thickness:" -integer false -min 1.0 \
                                         -option {
                                             font {Helvetica -11} \
                                             entry.background white \
                                             entry.width 5 \
                                             label.anchor w \
                                             entry.anchor e \
                                         } \
                                         -value 10.0 \
                                         -command { powXRangeChangeValue ${powDWP}xRange.property.lineWidth }

    set swLineWidthEntry [${powDWP}xRange.property.lineWidth subwidget entry]
    bind $swLineWidthEntry <Return> { powXRangeChangeValue ${powDWP}xRange.property.lineWidth -1.0 }

    set availableColor [list black blue green red yellow lightblue lightgreen lightyellow]
    tixComboBox ${powDWP}xRange.property.lineColor -label "Line Color:" -dropdown true -editable true \
                                         -option {
                                             font {Helvetica -11} \
                                             entry.width 7 \
                                             entry.background green \
                                             label.anchor w \
                                             entry.anchor e \
                                         } \
                                         -command { powXRangeChangeValue ${powDWP}xRange.property.lineColor }
    foreach color $availableColor {
           ${powDWP}xRange.property.lineColor insert end $color
    }

    tixSetSilent ${powDWP}xRange.property.lineColor green

    set swLineColor [${powDWP}xRange.property.lineColor subwidget entry]
    bind $swLineColor <Return> { powXRangeChangeValue ${powDWP}xRange.property.lineColor -1.0 }

    tixComboBox ${powDWP}xRange.property.boundaryColor -label "Boundary Color:" -dropdown true -editable true \
                                         -option {
                                             font {Helvetica -11} \
                                             entry.width 7 \
                                             entry.background blue \
                                             label.anchor w \
                                             entry.anchor e \
                                         } \
                                         -command { powXRangeChangeValue ${powDWP}xRange.property.boundaryColor }
    foreach color $availableColor {
           ${powDWP}xRange.property.boundaryColor insert end $color
    }

    tixSetSilent ${powDWP}xRange.property.boundaryColor blue

    set swBoundaryColor [${powDWP}xRange.property.boundaryColor subwidget entry]
    bind $swBoundaryColor <Return> { powXRangeChangeValue ${powDWP}xRange.property.boundaryColor -1.0 }

    tixComboBox ${powDWP}xRange.property.handleColor -label "Handle Color:" -dropdown true -editable true \
                                         -option {
                                             font {Helvetica -11} \
                                             entry.width 7 \
                                             entry.background red \
                                             label.anchor w \
                                             entry.anchor e \
                                         } \
                                         -command { powXRangeChangeValue ${powDWP}xRange.property.handleColor }
    foreach color $availableColor {
           ${powDWP}xRange.property.handleColor insert end $color
    }

    tixSetSilent ${powDWP}xRange.property.handleColor red

    set swBoundaryColor [${powDWP}xRange.property.handleColor subwidget entry]
    bind $swBoundaryColor <Return> { powXRangeChangeValue ${powDWP}xRange.property.handleColor -1.0 }
    button ${powDWP}xRange.property.reset -text "Reset" -font g_titleFont \
              -command { \
                   set swEntry [${powDWP}xRange.property.handleColor subwidget entry] ; \
                   $swEntry configure -background red ; \
                   $swEntry delete 0 end ; \
                   set swEntry [${powDWP}xRange.property.boundaryColor subwidget entry] ; \
                   $swEntry configure -background blue ; \
                   $swEntry delete 0 end ; \
                   set swEntry [${powDWP}xRange.property.lineColor subwidget entry] ; \
                   $swEntry configure -background green ; \
                   $swEntry delete 0 end ; \
                   ${powDWP}xRange.property.lineWidth configure -value 10.0 }

    grid ${powDWP}xRange.property.label         -column 0 -row 0 -sticky w
    grid ${powDWP}xRange.property.lineWidth     -column 0 -row 1 -sticky w
    grid ${powDWP}xRange.property.lineColor     -column 1 -row 1 -sticky w
    grid ${powDWP}xRange.property.boundaryColor -column 0 -row 2 -sticky w
    grid ${powDWP}xRange.property.handleColor   -column 1 -row 2 -sticky w
    grid ${powDWP}xRange.property.reset         -column 1 -row 3 -sticky e

    grid ${powDWP}xRange.list.rgns    -row 1 -column 1 -sticky news -columnspan 4
    grid ${powDWP}xRange.list.scrolly -row 1 -column 5 -sticky news
    # grid ${powDWP}xRange.list.scrollx -row 2 -column 1 -sticky news

    grid rowconfigure ${powDWP}xRange.list 1 -weight 1
    grid columnconfigure ${powDWP}xRange.list 1 -weight 1

    update idletasks
    ${powDWP}xRange.list.rgns configure -height 3 

    frame ${powDWP}xRange.rangeSelection -bg $powbg -bd 4 -relief groove 
    label ${powDWP}xRange.rangeSelection.title -text "Input maximum and minmum Y range:" -font g_titleFont
    label ${powDWP}xRange.rangeSelection.maxlabel -text "max:" -font g_titleFont
    entry ${powDWP}xRange.rangeSelection.maxentry -width 5 -font g_titleFont -bg white
    label ${powDWP}xRange.rangeSelection.minlabel -text "min:" -font g_titleFont
    entry ${powDWP}xRange.rangeSelection.minentry -width 5 -font g_titleFont -bg white
    button ${powDWP}xRange.rangeSelection.select -text "Create" -command {} -font g_titleFont


    grid ${powDWP}xRange.rangeSelection.title      -row 0 -column 0 -columnspan 6 -sticky news
    grid ${powDWP}xRange.rangeSelection.maxlabel   -row 1 -column 1 -sticky w
    grid ${powDWP}xRange.rangeSelection.maxentry   -row 1 -column 2 -sticky w
    grid ${powDWP}xRange.rangeSelection.minlabel   -row 1 -column 3 -sticky w
    grid ${powDWP}xRange.rangeSelection.minentry   -row 1 -column 4 -sticky w
    grid ${powDWP}xRange.rangeSelection.select     -row 1 -column 5 -sticky w

    frame ${powDWP}xRange.info -bg $powbg -relief groove -bd 2
    label ${powDWP}xRange.info.labelTitle -text "Mouse Button Functions Hints:" \
                                      -font [list Helvetica 10 bold]
    label ${powDWP}xRange.info.label1 -text "Drag left mouse button to select ranges; Hold 'Shift' key to unselect." \
                                      -font [list Helvetica 10]
    label ${powDWP}xRange.info.label2 -text "Select zoomed region with right mouse button; double click to unzoom." \
                                      -font [list Helvetica 10]

    grid ${powDWP}xRange.info.labelTitle -row 0 -column 1 -sticky w -columnspan 3
    grid ${powDWP}xRange.info.label1 -row 1 -column 1 -sticky w -columnspan 3
    grid ${powDWP}xRange.info.label2 -row 2 -column 1 -sticky w -columnspan 3

    frame ${powDWP}xRange.btns -bg $powbg
    button ${powDWP}xRange.btns.exit -text "Exit" \
            -command "destroy ${powDWP}xRange" \
	    -bg $powbg -highlightthickness 0 -font g_titleFont
    button ${powDWP}xRange.btns.clear -text "Clear All" -command powClearXRange \
	    -bg $powbg -highlightthickness 0 -font g_titleFont
    button ${powDWP}xRange.btns.save -text "Save..." -command powSaveXRangeFile \
	    -bg $powbg -highlightthickness 0 -font g_titleFont
    button ${powDWP}xRange.btns.open -text "Open..." -command powOpenXRangeFile \
	    -bg $powbg -highlightthickness 0 -font g_titleFont
    if { ![info exists heraQueryEntry] && !([info exists g_fvHera] && $g_fvHera > 0) } {
       pack ${powDWP}xRange.btns.open -in ${powDWP}xRange.btns -side left -padx 7
    }
    pack ${powDWP}xRange.btns.save -in ${powDWP}xRange.btns -side left -padx 7
    pack ${powDWP}xRange.btns.clear -in ${powDWP}xRange.btns -side left -padx 7
    pack ${powDWP}xRange.btns.exit -in ${powDWP}xRange.btns -side left -padx 7

    grid ${powDWP}xRange.head -in ${powDWP}xRange -column 1 -row 0 -sticky ew -columnspan 5
    grid ${powDWP}xRange.list -in ${powDWP}xRange -column 1 -row 1 -sticky news -columnspan 5

    grid ${powDWP}xRange.delete         -in ${powDWP}xRange -row 11 -column 1 -sticky e
    grid ${powDWP}xRange.propertyButton -in ${powDWP}xRange -row 11 -column 1 -sticky w
    # grid ${powDWP}xRange.property -in ${powDWP}xRange -row 12 -column 1 -sticky we
    # grid ${powDWP}xRange.rangeSelection -in ${powDWP}xRange -column 1 -row 11 -sticky news -columnspan 6 -rowspan 3
    grid ${powDWP}xRange.info -in ${powDWP}xRange -column 1 -row 15 -sticky news -columnspan 5 
    grid ${powDWP}xRange.btns -in ${powDWP}xRange -column 1 -row 17 -pady 5 -columnspan 5 

    grid columnconfigure ${powDWP}xRange 1 -weight 1
    grid rowconfigure ${powDWP}xRange 1 -weight 1
    grid rowconfigure ${powDWP}xRange 11 -minsize 5
    grid rowconfigure ${powDWP}xRange 15 -minsize 10
    # catch {wm minsize ${powDWP}xRange 300 400}

    #########
    #  Now setup bindings
    #########

    .pow.pow bind shape <Double-1> {powXRange}

    bind ${powDWP}xRange.list.rgns <Destroy> {powExitXRangeDlg}

    foreach wndw [list .pow ${powDWP}xRange] {
       bind $wndw <Shift-KeyPress-Up>      { powShiftXRange   0  -1 }
       bind $wndw <Shift-KeyPress-Left>    { powShiftXRange  -1   0 }
       bind $wndw <Shift-KeyPress-Right>   { powShiftXRange   1   0 }
       bind $wndw <Shift-KeyPress-Down>    { powShiftXRange   0   1 }

       bind $wndw <Control-KeyPress-Up>    { powShiftXRange   0 -10 }
       bind $wndw <Control-KeyPress-Left>  { powShiftXRange -10   0 }
       bind $wndw <Control-KeyPress-Right> { powShiftXRange  10   0 }
       bind $wndw <Control-KeyPress-Down>  { powShiftXRange   0  10 }
    }

    powSetupXRange $currgn
    $xRangeParam(rgns) activate
    set powLutButton_old $powLutButton
    set powROIButton_old $powROIButton
    powButtonSelection unknown unknown Left

    powUpdateXRangeDlg

    update idletasks

    scan [winfo geometry .pow] "%dx%d+%d+%d" Pw Ph Px Py

    set width [winfo reqwidth ${powDWP}xRange.head]
    scan [winfo geometry ${powDWP}xRange] "%dx%d+%d+%d" Rw Rh Rx Ry
    catch { wm geometry ${powDWP}xRange ${width}x$Rh+[expr $Px + $Pw - 15 ]+$Py } err

    [gNotifications default] addObserver \
          powXRangeNotify notify * graphHasBeenUnselected
    [gNotifications default] addObserver \
          powXRangeNotify notify * graphHasBeenSelected
    [gNotifications default] addObserver \
          powXRangeNotify notify * graphHasBeenDestroyed
}

proc togglePropertyList {} {
    global powDWP

    update idletasks
    scan [winfo geometry .pow] "%dx%d+%d+%d" Pw Ph Px Py

    set height [winfo reqheight ${powDWP}xRange.property]
    scan [winfo geometry ${powDWP}xRange] "%dx%d+%d+%d" Rw Rh Rx Ry

    if [winfo ismapped ${powDWP}xRange.property] {
       ${powDWP}xRange.propertyButton configure -text "Show Graph Utility"
       grid forget ${powDWP}xRange.property
       set width [winfo reqwidth ${powDWP}xRange.head]
       catch { wm geometry ${powDWP}xRange ${width}x[expr $Rh - $height] } err
       update idletasks
    } else {
       ${powDWP}xRange.propertyButton configure -text "Hide Graph Utility"
       grid ${powDWP}xRange.property -in ${powDWP}xRange -row 12 -column 1 -sticky we
       set width [winfo reqwidth ${powDWP}xRange.property]
       catch { wm geometry ${powDWP}xRange ${width}x[expr $Rh + $height] } err
       update idletasks
    }
}

proc powXRangeResetPanelColor { lineColor handleColor boundaryColor lineWidth } {
     global powDWP xRangeParam

     [${powDWP}xRange.property.handleColor subwidget entry] configure -background $handleColor
     [${powDWP}xRange.property.lineColor subwidget entry] configure -background $lineColor
     [${powDWP}xRange.property.boundaryColor subwidget entry] configure -background $boundaryColor
     [${powDWP}xRange.property.lineWidth subwidget entry] delete 0 end
     [${powDWP}xRange.property.lineWidth subwidget entry] insert end $lineWidth

     powXRangeChangeValue all -1.0
}

proc powXRangeChangeValue { wndw value } {
     global xRangeParam powDWP availableColor

     if { $wndw == "all" } {
        set swEntry [${powDWP}xRange.property.lineWidth subwidget entry]
        $xRangeParam(rgns) setLineWidth [$swEntry get]

        set swEntry [${powDWP}xRange.property.handleColor subwidget entry]
        $xRangeParam(rgns) setHandleColor [$swEntry cget -background]
        $swEntry delete 0 end
        set swEntry [${powDWP}xRange.property.boundaryColor subwidget entry]
        $xRangeParam(rgns) setBoundaryColor [$swEntry cget -background]
        $swEntry delete 0 end
        set swEntry [${powDWP}xRange.property.lineColor subwidget entry]
        $xRangeParam(rgns) setOutlineColor [$swEntry cget -background]
        $swEntry delete 0 end
        return
     }

     set swEntry [$wndw subwidget entry]

     if { $wndw == "${powDWP}xRange.property.lineWidth" } {
        if { $value == -1.0 } {
           set value [$swEntry get]
        }
        $xRangeParam(rgns) setLineWidth $value
     } else {
        $swEntry delete 0 end
        if { $value == -1.0 } {
           set value [$swEntry cget -background]
        } else {
           set idx [lsearch -exact $availableColor $value]
           if { $idx < 0 } {
              # not on the availableColor list
              $swEntry delete 0 end
              set value [$swEntry cget -background]
           } else {
              $swEntry configure -background $value
           }
        }
        $swEntry configure -background $value
        if { $wndw == "${powDWP}xRange.property.lineColor" } {
           $xRangeParam(rgns) setOutlineColor $value
        } elseif { $wndw == "${powDWP}xRange.property.boundaryColor" } {
           $xRangeParam(rgns) setBoundaryColor $value
        } elseif { $wndw == "${powDWP}xRange.property.handleColor" } {
           $xRangeParam(rgns) setHandleColor $value
        }
     }
}

proc powSetupXRange { gn } {
   global xRangeParam powPlotParam powDWP

   set xRangeParam(gn) $gn
   if { [info exists powPlotParam(XRanges,$gn)] } {
      set xRangeParam(rgns) $powPlotParam(XRanges,$gn)
      $xRangeParam(rgns) activate
   } else {
      set xRangeParam(rgns) [gRegionList $gn .pow.pow]
      $xRangeParam(rgns) setValueFormat "%.15g"
      $xRangeParam(rgns) setOwner powXRangeOwner
      $xRangeParam(rgns) changeShape "Line"
      set swEntry [${powDWP}xRange.property.handleColor subwidget entry]
      $xRangeParam(rgns) setHandleColor [$swEntry get]
      set swEntry [${powDWP}xRange.property.boundaryColor subwidget entry]
      $xRangeParam(rgns) setBoundaryColor [$swEntry get]
      set swEntry [${powDWP}xRange.property.lineWidth subwidget entry]
      $xRangeParam(rgns) setLineWidth [$swEntry get]
      set swEntry [${powDWP}xRange.property.lineColor subwidget entry]
      $xRangeParam(rgns) setOutlineColor [$swEntry get]
      $xRangeParam(rgns) setStaticFlag "y"
      set powPlotParam(XRanges,$gn) $xRangeParam(rgns)
   }
}

proc powUpdateXRangeDlg { { selectGn ""} } {
   global xRangeParam powDWP
   global currentRegionObj powPlotParam

   if { $selectGn != "" } {
      set xRangeParam(gn) $selectGn
   }

   set gn $xRangeParam(gn)

   #change rgn as well

   if [info exists powPlotParam(XRanges,$gn)] {
      set xRangeParam(rgns) $powPlotParam(XRanges,$gn)
      set xRangeParam(format) [$xRangeParam(rgns) getCoordSys]
      set currentRegionObj [$xRangeParam(rgns) getObj]
   }

   set xRangeParam(currShape) "Line"
   $xRangeParam(rgns) setCoordSys linear
   set xRangeParam(degreeFormat) "decimal"

   powUpdateXRangeList
   powUpdateXRangeTitle
}


proc powExitXRangeDlg { } {
    global xRangeParam powPlotParam
    global powDWP waitFlag staticY
    global powLutButton_old powLutButton powROIButton powROIButton_old
    global CpowXRangeY0 xrangeList_onG

    set waitFlag unsave
    destroy ${powDWP}xRange
    if { [$xRangeParam(rgns) count]==0 || ![winfo exists .pow.pow] } {

       itcl::delete object $xRangeParam(rgns)
       unset powPlotParam(XRanges,$xRangeParam(gn))

    } else {
       $xRangeParam(rgns) deleteAll
       $xRangeParam(rgns) deactivate
       itcl::delete object $xRangeParam(rgns)
       unset powPlotParam(XRanges,$xRangeParam(gn))
    }

    [gNotifications default] removeObserver \
          powXRangeNotify notify *

    if [info exists powLutButton_old] {
       set powLutButton $powLutButton_old
       set powROIButton $powROIButton_old
       powSaveConfig
    }

    catch { unset xrangeList_onG }
    catch { unset staticY }
    catch { unset CpowXRangeY0 }
}

proc powXRangeNotify { dmy obj msg args } {
   global powDWP xRangeParam currgn powPlotParam
   global TpowXRangeX0 TpowXRangeX1 TpowXRangeY0 TpowXRangeY1
   global CpowXRangeX0 CpowXRangeX1 CpowXRangeY0 CpowXRangeY1 currgraph
   global staticY r_staticYonG xrangeList_onG ROIbbox staticYonG r_staticX0onG

   if [info exists r_staticYonG] {
      set coord $ROIbbox
   } else {
      set coord [.pow.pow coords ${currgn}box]
   }
   set x0 [lindex $coord 0]
   set y0 [lindex $coord 3]
   set x1 [lindex $coord 2]
   set y1 [lindex $coord 1]
   set CpowXRangeX0 $x0
   set CpowXRangeX1 $x1
   set CpowXRangeY0 $y0
   set CpowXRangeY1 $y1

   if [info exists ROIbbox] {
      set staticY [lindex [powGraphToCanvas $currgn $r_staticX0onG $r_staticYonG .pow.pow] 1]
      unset ROIbbox
      unset r_staticYonG
   } else {
      set staticY [expr ($CpowXRangeY0 + $CpowXRangeY1) / 2.0]
   }

   set theRgns [$xRangeParam(rgns) regions]

   catch { unset xrangeList_onG }
   foreach rgn $theRgns {
       set coords [$rgn getCoords]
       set coords [lreplace $coords 1 1 $staticY]
       set coords [lreplace $coords 3 3 $staticY]
       if { [llength $coords] > 4 } {
          set coords [lreplace $coords 5 5 $staticY]
       }
       lappend xrangeList_onG [list $rgn $coords]
   }

   foreach regionList $xrangeList_onG {
       set rgn    [lindex $regionList 0]
       set coords [lindex $regionList 1]
       catch { $rgn setCoords $coords } err
       catch { $rgn draw } err
       catch { $rgn select } err
       if { ([lindex $coords 0] > $CpowXRangeX0 && [lindex $coords 0] < $CpowXRangeX1) || \
            ([lindex $coords 2] > $CpowXRangeX0 && [lindex $coords 2] < $CpowXRangeX1) } {
          catch { $rgn addBoundaryLine $coords } err
       }
       catch { $rgn finishModification } err
   }

   switch -- $msg {
      "graphHasBeenSelected" {
         if { [winfo exists ${powDWP}xRange] } {
            if { $xRangeParam(gn)==$obj } {
               # If graph hasn't actually changed, just call the activate
               # method so that all the shapes/drawables get raised to top
               $xRangeParam(rgns) activate
            } else {
               powSetupXRange $obj
            }
            powUpdateXRangeDlg
         }
         $xRangeParam(rgns) selectRegion [$xRangeParam(rgns) selected]

      }
      "graphHasBeenUnselected" {
         if { [info exists powPlotParam(XRanges,$obj)] } {
            set rgnList $powPlotParam(XRanges,$obj)
            $rgnList deactivate
            if { [$rgnList count]==0 } {
               unset powPlotParam(XRanges,$obj)
               itcl::delete object $rgnList
            }
         }
      }
      "graphHasBeenDestroyed" {
         if { [info exists powPlotParam(XRanges,$obj)] } {
            set rgnList $powPlotParam(XRanges,$obj)
            unset powPlotParam(XRanges,$obj)
            itcl::delete object $rgnList
            if { $xRangeParam(gn)==$obj } {
               set xRangeParam(gn) ""
            }
         }
      }
   }
}


proc powXRangeOwner { obj msg } {
   global xRangeParam

   switch $msg {
      "selectionHasChanged" {
         powUpdateSelectedXRange
      }
      "shapeIsBeingModified" {}
      "shapeHasChanged" {
         set idx [$xRangeParam(rgns) indexOfRgn $obj]
         if { $idx == -1 } return
         powUpdateXRangeList $idx
      }
      "regionsHaveChanged" {
         powUpdateXRangeList
      }
   }
}

proc powShiftXRange { dx dy } {
   global xRangeParam

   if { [info exists xRangeParam(rgns)] \
         && [itcl::find objects $xRangeParam(rgns)]!="" } {
      set idx [$xRangeParam(rgns) selected]
      [$xRangeParam(rgns) rgnAtIndex $idx] shift $dx $dy
   }
}

proc powUpdateXRangeTitle { {outputfile ""} } {
   global xRangeParam
   global powDWP

   if { $outputfile == "" } {
      set fName [$xRangeParam(rgns) filename]   
      if { $fName!="" } {
         ${powDWP}xRange.head.title configure \
               -text "X axis Range for $xRangeParam(gn) ([file tail $fName])"
      } else {
         ${powDWP}xRange.head.title configure -text "X axis Range for $xRangeParam(gn)"
      }
   } else {
      catch { ${powDWP}xRange.head.title configure -text "X axis Range for $xRangeParam(gn) ([file tail $outputfile])" }
   }
}

proc powSaveXRangeFile { } {
   global xRangeParam currimg powDWP
   global xrangeOutputFileName
   global heraClientObj heraClientUploadDirList

#  fvHera is defined in fvApp of fv and it indicates that POW is
#  used in a Hera client. 

   global heraQueryEntry g_fvHera
   global waitFlag g_backupDir

   set fName [$xRangeParam(rgns) filename]   
   if { $fName!="" } {
      set defFile [file tail $fName]
   } else {
      if { [info exists xrangeOutputFileName] && $xrangeOutputFileName != "" } {
         set defFile [file tail $xrangeOutputFileName]
      } else {
         set defFile pow.xrange
      }
   }

   if { [info exists heraQueryEntry] || ([info exists g_fvHera] && $g_fvHera > 0) } {
      dumpXRangeListToFile "$g_backupDir/[file tail $defFile]"

      set idx [lsearch $heraClientUploadDirList [list "*" [file tail $defFile]] ]
      set heraClientUploadDir [lindex [lindex $heraClientUploadDirList $idx] 0]

      eval $heraClientObj uploadFileVirtual {$g_backupDir/[file tail $defFile]} $heraClientUploadDir 
      eval $heraClientObj receiveOutput "refreshDir .$heraClientUploadDir"
      file delete -force $g_backupDir/[file tail $defFile]

      set waitFlag save
   } else {
      if { [info exists xrangeOutputFileName] && $xrangeOutputFileName != "" } {
         set filename $xrangeOutputFileName
      } else {
         set types {
            {{XRange Files}     {.xrange}        }
            {{All Files}        *             }
         }
         set filename [tk_getSaveFile -filetypes $types -initialfile "$defFile"]
      }
      if {$filename ==  "" } {
         set waitFlag unsave
         return
      }
      dumpXRangeListToFile $filename
      powUpdateXRangeTitle
      set waitFlag save
   }

}

proc dumpXRangeListToFile { fileName } {
     global xRangeParam

     set theRgns [$xRangeParam(rgns) regions]

     set f [open $fileName w+]
     foreach rgn $theRgns {
         foreach [list sign shape descr] \
               [$xRangeParam(rgns) buildRegionStr $rgn $xRangeParam(degreeFormat)]\
               {}
         #set descr "([join $descr {, }])"
         #set txtDescr "$sign${shape}$descr"
         set txtDescr "[format "%.15G" [lindex $descr 0]] [format "%.15G" [lindex $descr 2]]"
         puts $f $txtDescr
     }
     close $f
}

proc xrangeReadDataStr { str } {
     global powDWP
     global staticYonG
     global xRangeParam

     set dataStr [split [string trim $str] ";|\n"]
     set bufferedRegions {}
     for {set i 0} {$i < [llength $dataStr]} {incr i} {
         regsub -all "^%" [lindex $dataStr $i] "" result
         regsub -all "%" $result " " result
         ${powDWP}xRange.list.rgns insert $i $result
         set descr [split $result " "]
         set newDescr "+ Line \{[lindex $descr 0] $staticYonG [lindex $descr 1] $staticYonG\} unknown"
         lappend bufferedRegions $newDescr
     } 

     catch { $xRangeParam(rgns) flushBufferedRegions $bufferedRegions linear } err
     set bufferedRegions {}
}

proc powOpenXRangeFile { {fName "NONE"} } {
   global xRangeParam
   global powDWP

   set types {
      {{XRange Files}     {.xrange}        }
      {{All Files}        *             }
   }

   if { $fName == "NONE" } {
      set fName [$xRangeParam(rgns) filename]   
      if { $fName!="" } {
         set defFile [file tail $fName]
      } else {
         set defFile ""
      }
      set filename [tk_getOpenFile -filetypes $types -initialfile $defFile]
      if {$filename ==  "" } return

      if { $xRangeParam(nItems) } {
         set act [tk_dialog ${powDWP}xRangeInquiry "Open XRange File" \
               "XRange files already exist" warning 2 Cancel Overwrite Append]
         if { $act==-1 || $act==0 } {return}
         if { $act==1 } {
            $xRangeParam(rgns) deleteAll
         }
      }
   } else {
      set filename $fName
   }

   set f [open $filename r]
   set cnt [read $f [file size $filename]]
   close $f

   catch { xrangeReadDataStr $cnt} err
   catch { powUpdateXRangeTitle } err
}

		  
proc powUpdateXRangeList { {idx -1} } {
    global xRangeParam
    global powDWP currgn xrangeList_onG
    global CpowXRangeX0 CpowXRangeX1 CpowXRangeY0 CpowXRangeY1 staticY

   if { ![winfo exists ${powDWP}xRange] } { return }

   set xRangeParam(format) [$xRangeParam(rgns) getCoordSys]

   set currItm [$xRangeParam(rgns) selected]
   if { $idx==-1 } {
      ${powDWP}xRange.list.rgns delete 0 end
      set theRgns [$xRangeParam(rgns) regions]
      set n 0
   } else {
      set theRgns [$xRangeParam(rgns) rgnAtIndex $idx]
      set n $idx
      # this is the new region been created.. check to see if it overlapping other regions
      foreach rgn $theRgns {
         foreach [list sign shape descr] \
               [$xRangeParam(rgns) buildRegionStr $rgn $xRangeParam(degreeFormat)]\
               {}
         set currRgn [list [lindex $descr 0] [lindex $descr 2]]
      }
   }

   set totalRgns [$xRangeParam(rgns) regions]
   foreach rgn $totalRgns {
      foreach [list sign shape descr] \
            [$xRangeParam(rgns) buildRegionStr $rgn $xRangeParam(degreeFormat)]\
            {}
      lappend xrangeList [list [lindex $descr 0] [lindex $descr 2]]
   }

   set theRgns [$xRangeParam(rgns) regions]
   catch { unset xrangeList_onG }
   foreach rgn $theRgns {
       lappend xrangeList_onG [list $rgn [$rgn getCoords]]
   }

   # resort the list before populate at the GUI
   if [info exist xrangeList] {
      set pxrangeList $xrangeList
      set xrangeList [lsort -real -increasing -index 0 $xrangeList]

      ${powDWP}xRange.list.rgns delete 0 end
      set n 0
      set findIndex {}
      foreach descr $xrangeList {
         set txtDescr "[format "%.15G" [lindex $descr 0]] [format "%.15G" [lindex $descr 1]]"
         ${powDWP}xRange.list.rgns insert $n $txtDescr
         set selectTxt [lindex $xrangeList $n]
         set i [lsearch -exact $pxrangeList $selectTxt]
         lappend new_xrangeList_onG [lindex $xrangeList_onG $i]
         incr n
      }
      set xrangeList_onG $new_xrangeList_onG

      if { $currItm!=-1 } {
         # find the item index at prangeList
         set selectTxt [lindex $pxrangeList $currItm]
         set currItm [lsearch -exact $xrangeList $selectTxt]
         ${powDWP}xRange.list.rgns selection set $currItm
         ${powDWP}xRange.list.rgns see $currItm
      }
   }
}

proc powUpdateSelectedXRange { } {
   global xRangeParam powDWP

   set rgnIdx [$xRangeParam(rgns) selected]
   set rgn [$xRangeParam(rgns) rgnAtIndex $rgnIdx]

   if { [winfo exists ${powDWP}xRange] } {
      foreach [list sign shape descr] \
            [$xRangeParam(rgns) buildRegionStr $rgn $xRangeParam(degreeFormat)]\
            {}
      ${powDWP}xRange.list.rgns select clear 0 end
      ${powDWP}xRange.list.rgns select set $rgnIdx
      ${powDWP}xRange.list.rgns see $rgnIdx

      set xRangeParam(currSign)  $sign
      set xRangeParam(currShape) $shape
      $xRangeParam(rgns) setDefault $sign $shape
   }
}

proc powSelectXRange { itemNo } {
    global xRangeParam
    global powDWP xrangeList_onG

    if {$itemNo==""} {return}

    set regionName [lindex [lindex $xrangeList_onG $itemNo] 0]
    set theRgns [$xRangeParam(rgns) regions]
    set result [lsearch -exact $theRgns $regionName]
    $xRangeParam(rgns) selectRegion $result
    ${powDWP}xRange.list.rgns selection clear 0 end
    ${powDWP}xRange.list.rgns selection set $itemNo
    ${powDWP}xRange.list.rgns see $itemNo
}

proc powChangeFormat { newFormat } {
   global xRangeParam

   set format [string tolower [lindex $newFormat 0]]
   switch $format {

      #  Changes to degreeFormat

      "decimal" -
      "hhmmss" {
         set xRangeParam(degreeFormat) $format
      }

      #  Changes to format

      default {
         $xRangeParam(rgns) setCoordSys $newFormat
      }
   }

   powUpdateXRangeList
}

proc powGetCurrXRange {} {
     global xRangeParam  

     set theRgns [$xRangeParam(rgns) regions]
     return [lindex $theRgns [$xRangeParam(rgns) selected]]
}

proc powDeleteCurrXRange { {idx -1} } {
   global xRangeParam xrangeList_onG

   set rc [scan $idx "region%d" result]

   if { $rc > 0 } {
      set theRgns [$xRangeParam(rgns) regions]
      set result [lsearch -exact $theRgns $idx]
      catch { $xRangeParam(rgns) deleteRegion $result } err
   } else {
      if { $idx != -1 } {
         $xRangeParam(rgns) selectRegion $idx
      } else {
         set idx [$xRangeParam(rgns) selected]
      }
      $xRangeParam(rgns) deleteRegion [$xRangeParam(rgns) selected]

   }
}

proc powClearXRange { {mode "manual"} } {
    global xRangeParam powDWP xrangeList_onG
    global insideExistGraph

    catch { unset xrangeList_onG }
    if { $mode == "manual" } {
       set act [tk_messageBox -message "Delete All X Axis Ranges?" -type yesno \
	       -default no]

       if { $act=="yes" } {
           $xRangeParam(rgns) deleteAll
       }
    } else {
       $xRangeParam(rgns) deleteAll
    }

    catch {unset insideExistGraph}
}
