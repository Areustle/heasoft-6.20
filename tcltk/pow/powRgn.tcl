###################################################################
#
# TCL routines handling creation and manipulation of SAO regions
#
###################################################################

# variables used:
#     regionParam(rgns):  RegionList object containing objects
#     regionParam(currSign) : current sign (+/-)
#     regionParam(currShape): current shape (Circle, etc)
#     regionParam(supportedShapes): list of valid shapes
#     regionParam(format): "Linear", "FK5 ()", etc
#     regionParam(degreeFormat): "decimal" or "hhmmss"
#                 gn

proc powRegion { } {
#puts "powRegion start"
   global heraQueryEntry g_fvHera
   global powPlotParam regionParam powbg currgn currimg
   global powFrameForTop
   global powDWP availableColor
   global negSignLineColor negSignHandleColor posSignLineColor posSignHandleColor
   global d_negSignLineColor d_negSignHandleColor d_posSignLineColor d_posSignHandleColor
   global g_titleFont
   global powLutButton buttonWndw powROIButton
   global old_powLutButton old_powROIButton
   global propertyOrder

   set propertyOrder "Source"

    if {[winfo exists ${powDWP}region]} {
	catch {raise ${powDWP}region}
	return
    }

    set old_powLutButton $powLutButton
    set old_powROIButton $powROIButton

    set posSignLineColor blue
    set posSignHandleColor green
    set negSignLineColor red
    set negSignHandleColor yellow

    set d_posSignLineColor blue
    set d_posSignHandleColor green
    set d_negSignLineColor red
    set d_negSignHandleColor yellow

    powSetupRegions $currgn
    $regionParam(rgns) activate

    powToplevel ${powDWP}region .pow "-bg $powbg"
    bind ${powDWP}region <Leave> "powRegionChangeColor all -1.0"
    bind ${powDWP}region <<CloseWindow>> "destroy ${powDWP}region"
    catch {wm title ${powDWP}region "Edit Region"}
    catch {wm geometry ${powDWP}region +500+240}

    ${powDWP}region config -menu ${powDWP}region.mbar
    menu ${powDWP}region.mbar -postcommand "powEvents::generate <<PostMenus>>" -bg $powbg -font g_titleFont

    ${powDWP}region.mbar add cascade -menu ${powDWP}region.mbar.file -label "Property" -font g_titleFont
    ${powDWP}region.mbar add command -label "Help" -font g_titleFont -command { powHelp Regions.html }
 

    set m [menu ${powDWP}region.mbar.file]
    $m add radio -label Source -variable [itcl::scope propertyOrder] -value Source \
                 -command { powSetupRegionProperty }
    $m add radio -label Background -variable [itcl::scope propertyOrder] -value Background \
                 -command { powSetupRegionProperty }

    frame ${powDWP}region.list -bg $powbg
    scrollbar ${powDWP}region.list.scrolly -orient vertical \
	    -command {${powDWP}region.list.rgns yview} -bg $powbg
    scrollbar ${powDWP}region.list.scrollx -orient horizontal \
	    -command {${powDWP}region.list.rgns xview} -bg $powbg
    listbox ${powDWP}region.list.rgns -width 30 -height 6 \
	    -selectmode browse -bg $powbg -exportselection 0 \
	    -yscrollcommand "${powDWP}region.list.scrolly set " \
	    -xscrollcommand "${powDWP}region.list.scrollx set " -font g_titleFont
    bind ${powDWP}region.list.rgns <ButtonRelease-1> \
	    {powSelectRegion [${powDWP}region.list.rgns curselection]}

    grid ${powDWP}region.list.rgns    -row 1 -column 1 -sticky news -columnspan 4
    grid ${powDWP}region.list.scrolly -row 1 -column 5 -sticky news
    grid ${powDWP}region.list.scrollx -row 2 -column 1 -sticky news

    grid rowconfigure ${powDWP}region.list 1 -weight 1
    grid columnconfigure ${powDWP}region.list 1 -weight 1

    update idletasks
    ${powDWP}region.list.rgns configure -height 3 
    frame ${powDWP}region.currshape -bg $powbg -bd 4 -relief groove 
    frame ${powDWP}region.currshape.f1 -bg $powbg
    label ${powDWP}region.currshape.f1.lbl -text "Current Shape: " -bg $powbg -font g_titleFont
    button ${powDWP}region.currshape.f1.apply -text "Apply" -bg $powbg \
	    -command {powChangeRegion} -highlightthickness 0 -font g_titleFont
    button ${powDWP}region.currshape.f1.delete -text "Delete" -bg $powbg \
	    -command {powDeleteCurrRegion} -highlightthickness 0 -font g_titleFont

    pack ${powDWP}region.currshape.f1.lbl -in ${powDWP}region.currshape.f1 \
	    -side left
    pack ${powDWP}region.currshape.f1.apply -in ${powDWP}region.currshape.f1 \
	    -side right -padx 5
    pack ${powDWP}region.currshape.f1.delete -in ${powDWP}region.currshape.f1 \
	    -side right -padx 5

    frame ${powDWP}region.control -bg $powbg -bd 4 -relief groove 
    label ${powDWP}region.control.label -text "Set right mouse button to control:" -font g_titleFont
    radiobutton ${powDWP}region.control.zoom  -variable buttonSelection -text "Zoom/unZoom Region" \
                                      -font [list Helvetica 10] -value left \
                                      -command {powButtonSelection ${powDWP}region.control.brightness \
                                                                   ${powDWP}region.control.zoom Left}
    radiobutton ${powDWP}region.control.brightness -variable buttonSelection -text "Brightness/Contrast" \
                                      -font [list Helvetica 10] -value right  \
                                      -command {powButtonSelection ${powDWP}region.control.brightness \
                                                                   ${powDWP}region.control.zoom Right}

    grid ${powDWP}region.control.label      -row 0 -column 0 -columnspan 3
    grid ${powDWP}region.control.zoom       -row 1 -column 1 -sticky w
    grid ${powDWP}region.control.brightness -row 2 -column 1 -sticky w

    ${powDWP}region.control.zoom select
    powButtonSelection ${powDWP}region.control.brightness ${powDWP}region.control.zoom Left 

    frame ${powDWP}region.currshape.f2 -bg $powbg

    menu ${powDWP}region.shapemenu -tearoff 0 -bg $powbg
    set regionParam(supportedShapes) \
	    [list Box Circle Ellipse Polygon Line Point]
    foreach s $regionParam(supportedShapes) {
	    ${powDWP}region.shapemenu add command -label $s \
		-command "powChangeShape $s" -font g_titleFont
    }

    button ${powDWP}region.currshape.f2.shapebut \
	    -textvariable regionParam(currShape) -highlightthickness 0 \
	    -bg $powbg -relief raised -width 8 -font g_titleFont
    bind ${powDWP}region.currshape.f2.shapebut <ButtonPress-1> {
	tk_popup ${powDWP}region.shapemenu \
		[winfo pointerx ${powDWP}region.shapemenu] \
		[winfo pointery ${powDWP}region.shapemenu] \
		[lsearch -exact $regionParam(supportedShapes) \
		                $regionParam(currShape) ]
	${powDWP}region.currshape.f2.shapebut configure -relief raised
    }
    entry ${powDWP}region.currshape.f2.desc -bg $powbg -font g_titleFont

    button ${powDWP}region.currshape.f2.signbut \
          -textvariable regionParam(currSign) -highlightthickness 0 \
          -bg $powbg -relief raised -width 1 \
          -command { powToggleSign } -font g_titleFont

    set availableColor [list black blue green red yellow lightblue lightgreen lightyellow]
    tixComboBox ${powDWP}region.currshape.f2.lineColor -label "Line Color:" -dropdown true -editable true \
                                         -option {
                                             font {Helvetica -11} \
                                             entry.width 7 \
                                             entry.background blue \
                                             label.anchor w \
                                             entry.anchor e \
                                         } \
                                         -command { powRegionChangeColor ${powDWP}region.currshape.f2.lineColor }
    foreach color $availableColor {
           ${powDWP}region.currshape.f2.lineColor insert end $color
    }

    tixSetSilent ${powDWP}region.currshape.f2.lineColor blue
    [${powDWP}region.currshape.f2.lineColor subwidget label] configure -background $powbg

    set swLineWidget [${powDWP}region.currshape.f2.lineColor subwidget entry]
    bind $swLineWidget <Return> { powRegionChangeColor ${powDWP}region.currshape.f2.lineColor -1.0 }

    tixComboBox ${powDWP}region.currshape.f2.handleColor -label "Handle Color:" -dropdown true -editable true \
                                         -bg $powbg \
                                         -option {
                                             font {Helvetica -11} \
                                             entry.width 7 \
                                             entry.background green \
                                             label.anchor w \
                                             entry.anchor e \
                                         } \
                                         -command { powRegionChangeColor ${powDWP}region.currshape.f2.handleColor }
    foreach color $availableColor {
           ${powDWP}region.currshape.f2.handleColor insert end $color
    }

    tixSetSilent ${powDWP}region.currshape.f2.handleColor green
    [${powDWP}region.currshape.f2.handleColor subwidget label] configure -background $powbg

    set swHandleWidget [${powDWP}region.currshape.f2.handleColor subwidget entry]
    bind $swHandleWidget <Return> { powRegionChangeColor ${powDWP}region.currshape.f2.handleColor -1.0 }

    #pack ${powDWP}region.currshape.f2.signbut -in ${powDWP}region.currshape.f2 -side left -padx 1
    #pack ${powDWP}region.currshape.f2.shapebut -in ${powDWP}region.currshape.f2 -side left -padx 1
    #pack ${powDWP}region.currshape.f2.lineColor -in ${powDWP}region.currshape.f2 -side left -padx 1
    #pack ${powDWP}region.currshape.f2.desc -in ${powDWP}region.currshape.f2 -side left -padx 1 -fill x -expand 1

    grid ${powDWP}region.currshape.f2.signbut   -row 0 -column 0 -sticky w
    grid ${powDWP}region.currshape.f2.shapebut  -row 0 -column 1 -sticky w
    grid ${powDWP}region.currshape.f2.desc      -row 0 -column 2 -sticky we -columnspan 4
    grid ${powDWP}region.currshape.f2.lineColor   -row 1 -column 0 -sticky w -columnspan 2
    grid ${powDWP}region.currshape.f2.handleColor -row 1 -column 2 -sticky w -columnspan 2

    #  Build format controls

    frame ${powDWP}region.radiobtns -bg $powbg -bd 4 -relief groove
    label ${powDWP}region.radiobtns.lbl -text "Format:" -bg $powbg -font g_titleFont
      
    menu ${powDWP}region.radiobtns.formatmenu -tearoff 0 -bg $powbg -font g_titleFont
    set regionParam(supportedFormats) [$regionParam(rgns) getAllFormats]
    foreach s $regionParam(supportedFormats) {
       ${powDWP}region.radiobtns.formatmenu add command -label $s \
             -command [list powChangeFormat $s] -font [list Helvetica 10]
    }
    ${powDWP}region.radiobtns.formatmenu add separator
    ${powDWP}region.radiobtns.formatmenu add radiobutton \
          -label "Decimal Degrees" -variable regionParam(degreeFormat) \
          -command [list powChangeFormat "Decimal"] -value "decimal" -font [list Helvetica 10]
    ${powDWP}region.radiobtns.formatmenu add radiobutton \
          -label "HHMMSS Degrees" -variable regionParam(degreeFormat) \
          -command [list powChangeFormat "HHMMSS"]  -value "hhmmss" -font [list Helvetica 10]

    button ${powDWP}region.radiobtns.btn \
          -textvariable regionParam(format) -highlightthickness 0 \
          -bg $powbg -relief raised -width 10 -font [list Helvetica 10]
    bind ${powDWP}region.radiobtns.btn <ButtonPress-1> {
       tk_popup ${powDWP}region.radiobtns.formatmenu \
             [winfo pointerx ${powDWP}region.radiobtns.formatmenu] \
             [winfo pointery ${powDWP}region.radiobtns.formatmenu] \
             [lsearch -exact $regionParam(supportedFormats) \
                             $regionParam(format) ]
       ${powDWP}region.radiobtns.btn configure -relief raised -font [list Helvetica 10]
    }

    grid ${powDWP}region.radiobtns.lbl -column 0 -row 0 -sticky w
    grid ${powDWP}region.radiobtns.btn -column 0 -row 1

    #  Build Flux Panel

    frame ${powDWP}region.fluxprobe -bg $powbg -bd 4 -relief groove
    label ${powDWP}region.fluxprobe.label -text "Statistics:" -anchor w -font g_titleFont -bg $powbg
    iwidgets::labeledwidget  ${powDWP}region.fluxprobe.pixels -labeltext "N pixels:" \
                            -labelfont g_titleFont -background $powbg
    set childsite [${powDWP}region.fluxprobe.pixels childsite]
    label $childsite.c  -width 30 -relief sunken  -font g_titleFont -bg $powbg
    pack $childsite.c -side left
    iwidgets::labeledwidget  ${powDWP}region.fluxprobe.flux -labeltext "Total Flux:" \
                           -labelfont g_titleFont -background $powbg
    set childsite [${powDWP}region.fluxprobe.flux childsite]
    label $childsite.c  -width 30 -relief sunken  -font g_titleFont -bg $powbg
    pack $childsite.c -side left
    iwidgets::labeledwidget  ${powDWP}region.fluxprobe.mean -labeltext "Mean Flux:" \
                           -labelfont g_titleFont -background $powbg
    set childsite [${powDWP}region.fluxprobe.mean childsite]
    label $childsite.c  -width 30 -relief sunken  -font g_titleFont -bg $powbg
    label $childsite.label -text "+- error" -anchor w -font g_titleFont -bg $powbg
    pack $childsite.c $childsite.label -side left
    iwidgets::Labeledwidget::alignlabels  ${powDWP}region.fluxprobe.pixels \
                                          ${powDWP}region.fluxprobe.flux \
                                          ${powDWP}region.fluxprobe.mean

    grid ${powDWP}region.fluxprobe.label -column 0 -row 0 -columnspan 10 -sticky nsw
    grid ${powDWP}region.fluxprobe.pixels -column 0 -row 1 -columnspan 10 -sticky nsw
    grid ${powDWP}region.fluxprobe.flux -column 0 -row 2 -columnspan 10 -sticky nsw
    grid ${powDWP}region.fluxprobe.mean -column 0 -row 3 -columnspan 10 -sticky nsw

    #
    frame ${powDWP}region.btns -bg $powbg
    button ${powDWP}region.btns.exit -text "Exit" \
            -command "destroy ${powDWP}region" \
	    -bg $powbg -highlightthickness 0 -font g_titleFont
    button ${powDWP}region.btns.clear -text "Clear All" -command powClearRegions \
	    -bg $powbg -highlightthickness 0 -font g_titleFont
    button ${powDWP}region.btns.save -text "Save..." -command powSaveRegionFile \
	    -bg $powbg -highlightthickness 0 -font g_titleFont
    button ${powDWP}region.btns.open -text "Open..." -command powOpenRegionFile \
	    -bg $powbg -highlightthickness 0 -font g_titleFont
    if { ![info exists heraQueryEntry] && !([info exists g_fvHera] && $g_fvHera > 0) } {
       pack ${powDWP}region.btns.open -in ${powDWP}region.btns -side left -padx 7
    }
    pack ${powDWP}region.btns.save -in ${powDWP}region.btns -side left -padx 7
    pack ${powDWP}region.btns.clear -in ${powDWP}region.btns -side left -padx 7
    pack ${powDWP}region.btns.exit -in ${powDWP}region.btns -side left -padx 7

    grid ${powDWP}region.list -in ${powDWP}region -column 1 -row 1 -sticky news -columnspan 5

    grid ${powDWP}region.currshape -in ${powDWP}region -column 1 -row 5  -sticky ew -columnspan 5
    grid ${powDWP}region.currshape.f1 -in ${powDWP}region.currshape -column 1 -row 0  -sticky ew -padx 5 -pady 5
    grid ${powDWP}region.currshape.f2 -in ${powDWP}region.currshape -column 1 -row 1 -sticky ew  -padx 5 -pady 5

    grid ${powDWP}region.radiobtns -in ${powDWP}region -column 1 -row 11 -sticky news -rowspan 3 
    grid ${powDWP}region.control -in ${powDWP}region -column 2 -row 11 -sticky news -columnspan 4 -rowspan 3

    grid ${powDWP}region.fluxprobe -column 0 -row 15 -columnspan 5 -rowspan 4 -sticky news
    grid ${powDWP}region.btns -in ${powDWP}region -column 1 -row 19 -pady 5 -columnspan 5 

    grid columnconfigure ${powDWP}region 1 -weight 1
    grid columnconfigure ${powDWP}region.currshape 1 -weight 1
    grid columnconfigure ${powDWP}region.currshape.f2 2 -weight 1
    grid rowconfigure ${powDWP}region 1 -weight 1
    grid rowconfigure ${powDWP}region 11 -minsize 5
    grid rowconfigure ${powDWP}region 15 -minsize 10
    # catch {wm minsize ${powDWP}region 300 400}

    #########
    #  Now setup bindings
    #########

    .pow.pow bind shape <Double-1> {powRegion}

    bind ${powDWP}region.list.rgns <Destroy> {powExitRegionDlg}

    foreach wndw [list .pow ${powDWP}region] {
       bind $wndw <Shift-KeyPress-Up>      { powShiftRegion   0  -1 }
       bind $wndw <Shift-KeyPress-Left>    { powShiftRegion  -1   0 }
       bind $wndw <Shift-KeyPress-Right>   { powShiftRegion   1   0 }
       bind $wndw <Shift-KeyPress-Down>    { powShiftRegion   0   1 }

       bind $wndw <Control-KeyPress-Up>    { powShiftRegion   0 -10 }
       bind $wndw <Control-KeyPress-Left>  { powShiftRegion -10   0 }
       bind $wndw <Control-KeyPress-Right> { powShiftRegion  10   0 }
       bind $wndw <Control-KeyPress-Down>  { powShiftRegion   0  10 }
    }

    powUpdateRegionDlg

    update idletasks

    scan [winfo geometry .pow] "%dx%d+%d+%d" Pw Ph Px Py

    #set width [winfo reqwidth ${powDWP}region.control]
    #set width [expr [winfo reqwidth ${powDWP}region.radiobtns] + $width + 2]
    set width [expr [winfo reqwidth ${powDWP}region.fluxprobe] + 2]

    scan [winfo geometry ${powDWP}region] "%dx%d+%d+%d" Rw Rh Rx Ry
    catch { wm geometry ${powDWP}region ${width}x$Rh+[expr $Px + $Pw - 15 ]+$Py } err

    [gNotifications default] addObserver \
          powRegionNotify notify * graphHasBeenUnselected
    [gNotifications default] addObserver \
          powRegionNotify notify * graphHasBeenSelected
    [gNotifications default] addObserver \
          powRegionNotify notify * graphHasBeenDestroyed

}

proc powSetupRegionProperty { } {
   global regionParam
   global propertyOrder

   set rgnIdx [$regionParam(rgns) selected]
   if { $rgnIdx < 0 } return

   set rgn [$regionParam(rgns) rgnAtIndex $rgnIdx]

   $rgn setPropertyOrder $propertyOrder

   powSelectRegion $rgnIdx
}

proc powSetupRegions { gn } {
   global regionParam powPlotParam

   set regionParam(gn) $gn
   if { [info exists powPlotParam(regions,$gn)] } {

      set regionParam(rgns) $powPlotParam(regions,$gn)
      $regionParam(rgns) activate

   } else {

      set regionParam(rgns) [gRegionList $gn .pow.pow]
      $regionParam(rgns) setOwner powRegionOwner
      set powPlotParam(regions,$gn) $regionParam(rgns)

   }
}

proc powRegionResetPanelColor { lineColor handleColor } {
     global powDWP

     [${powDWP}region.currshape.f2.lineColor subwidget entry] configure -background $lineColor
     [${powDWP}region.currshape.f2.handleColor subwidget entry] configure -background $handleColor

     powRegionChangeColor all -1.0
}

proc powRegionChangeColor { wndw value } {
     global regionParam powDWP availableColor
     global negSignLineColor negSignHandleColor posSignLineColor posSignHandleColor

     if { $wndw == "all" } {
        set swEntry [${powDWP}region.currshape.f2.handleColor subwidget entry]
        $regionParam(rgns) setHandleColor [$swEntry cget -background]
        $swEntry delete 0 end
        set swEntry [${powDWP}region.currshape.f2.lineColor subwidget entry]
        $regionParam(rgns) setOutlineColor [$swEntry cget -background]
        $swEntry delete 0 end
     } else {
        set swEntry [$wndw subwidget entry]
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
        if { $wndw == "${powDWP}region.currshape.f2.lineColor" } {
           $regionParam(rgns) setOutlineColor $value
        } elseif { $wndw == "${powDWP}region.currshape.f2.handleColor" } {
           $regionParam(rgns) setHandleColor $value
        }
     }

     if { $regionParam(currSign) == "+" } {
        set posSignLineColor   [[${powDWP}region.currshape.f2.lineColor subwidget entry] cget -background]
        set posSignHandleColor [[${powDWP}region.currshape.f2.handleColor subwidget entry] cget -background]
     } else {
        set negSignLineColor   [[${powDWP}region.currshape.f2.lineColor subwidget entry] cget -background]
        set negSignHandleColor [[${powDWP}region.currshape.f2.handleColor subwidget entry] cget -background]
     }
}


proc powUpdateRegionDlg { { selectGn "" } } {
   global regionParam powDWP
   global currentRegionObj
   global powPlotParam

   if { $selectGn != "" } {
      set regionParam(gn) $selectGn
   }

   set gn $regionParam(gn)

   #change rgn as well

   if [info exists powPlotParam(regions,$gn)] {
      set regionParam(rgns) $powPlotParam(regions,$gn)
      set regionParam(format) [$regionParam(rgns) getCoordSys]
      set currentRegionObj [$regionParam(rgns) getObj]
   }

   if { [$regionParam(rgns) count]==0 } {
      set regionParam(currRgn)   ""
      set regionParam(currSign)  "+"
      set regionParam(currShape) "Circle"

      if { [powWCSexists $gn] } {
         # Dont set default regions
         # $regionParam(rgns) setCoordSys fk5
         set regionParam(degreeFormat) "hhmmss"
      } else {
         $regionParam(rgns) setCoordSys linear
         set regionParam(degreeFormat) "decimal"
      }
   }

   if { [powWCSexists $gn] } {
      set wcsState normal
      set linState disabled
   } else {
      set wcsState disabled
      set linState normal
   }
   foreach itm [list FK4 FK5 Gal Ecl IC Deg] {
      ${powDWP}region.radiobtns.formatmenu entryconfigure "${itm}*" \
            -state $wcsState
   }
   ${powDWP}region.radiobtns.formatmenu entryconfigure "Lin*" \
         -state $linState

   powUpdateRegionList
   powUpdateRegionTitle
}


proc powExitRegionDlg { } {
    global regionParam powPlotParam
    global powDWP waitFlag
    global old_powLutButton powLutButton powROIButton old_powROIButton

    set waitFlag unsave
    destroy ${powDWP}region

    if { [itcl::find objects $regionParam(rgns)] != "" } {
       if { [$regionParam(rgns) count]==0 || ![winfo exists .pow.pow] } {

          itcl::delete object $regionParam(rgns)
          unset powPlotParam(regions,$regionParam(gn))

       } else {
          $regionParam(rgns) deleteAll
          $regionParam(rgns) deactivate
          itcl::delete object $regionParam(rgns)
          unset powPlotParam(regions,$regionParam(gn))
       }
    }

    [gNotifications default] removeObserver \
          powRegionNotify notify *

    if [info exists old_powLutButton] {
       set powLutButton $old_powLutButton
       set powROIButton $old_powROIButton
       powSaveConfig
    }
}

proc powRegionNotify { dmy obj msg args } {
   global powDWP regionParam currgn powPlotParam

   switch -- $msg {
      "graphHasBeenSelected" {
         if { [winfo exists ${powDWP}region] } {
            if { $regionParam(gn)==$obj } {
               # If graph hasn't actually changed, just call the activate
               # method so that all the shapes/drawables get raised to top
               $regionParam(rgns) activate
            } else {
               powSetupRegions $obj
               powUpdateRegionDlg
            }
         }
      }
      "graphHasBeenUnselected" {
         if { [info exists powPlotParam(regions,$obj)] } {
            set rgnList $powPlotParam(regions,$obj)
            $rgnList deactivate
            if { [$rgnList count]==0 } {
               unset powPlotParam(regions,$obj)
               itcl::delete object $rgnList
            }
         }
      }
      "graphHasBeenDestroyed" {
         if { [info exists powPlotParam(regions,$obj)] } {
            set rgnList $powPlotParam(regions,$obj)
            unset powPlotParam(regions,$obj)
            itcl::delete object $rgnList
            if { $regionParam(gn)==$obj } {
               set regionParam(gn) ""
            }
         }
      }
   }
}


proc powRegionOwner { obj msg } {
   global regionParam
   global currentRegionObj

   switch $msg {
      "selectionHasChanged" {
         powUpdateSelectedRegion
      }
      "shapeIsBeingModified" {
      }
      "shapeHasChanged" {
         set idx [$regionParam(rgns) indexOfRgn $obj]
         if { $idx == -1 } return
         powUpdateRegionList $idx
      }
      "regionsHaveChanged" {
         powUpdateRegionList
      }
   }
}

proc powShiftRegion { dx dy } {
   global regionParam

   if { [info exists regionParam(rgns)] \
         && [itcl::find objects $regionParam(rgns)]!="" } {
      set idx [$regionParam(rgns) selected]
      [$regionParam(rgns) rgnAtIndex $idx] shift $dx $dy
   }
}

proc powUpdateRegionTitle { {outputfile ""} } {
   global regionParam
   global powDWP

   if { $outputfile == "" } {
      set fName [$regionParam(rgns) filename]   
      if { $fName!="" } {
     #    ${powDWP}region.head.title configure \
               -text "Regions for $regionParam(gn) ([file tail $fName])"
      } else {
     #    ${powDWP}region.head.title configure -text "Regions for $regionParam(gn)"
      }
   } else {
     # catch { ${powDWP}region.head.title configure -text "Regions for $regionParam(gn) ([file tail $outputfile])" }
   }
}

proc powSaveRegionFile { } {
   global regionParam currimg powDWP
   global regionOutputFileName
   global heraClientObj heraClientUploadDirList

#  g_fvHera is defined in fvApp of fv and it indicates that POW is
#  used in a Hera client. 

   global heraQueryEntry g_fvHera
   global waitFlag g_backupDir
   global newUploadFileName

   set filenameList [list "src.reg"]
   for {set i 0} {$i<[$regionParam(rgns) count]} {incr i} {
      set rgn   [$regionParam(rgns) rgnAtIndex $i]
      set propertyOrder [$rgn getPropertyOrder]
      if { $propertyOrder == "Background" } {
         lappend filenameList "back.reg"
         break
      }
   }

   for {set i 0} {$i < [llength $filenameList]} {incr i} {   
       if { $i == 0 } {
          # file list: source, background
          set fName [$regionParam(rgns) filename]   
          set property "Source"
       } else {
          set fName [$regionParam(rgns) bfilename]   
          set property "Background"
       }
       if { $fName!="" } {
          set defFile [file tail $fName]
       } else {
          if { [info exists regionOutputFileName] && $regionOutputFileName != "" } {
             set defFile [file tail $regionOutputFileName]
          } else {
             set defFile [lindex $filenameList $i]
          }
       }

       if { [info exists heraQueryEntry] || ([info exists g_fvHera] && $g_fvHera > 0) } {
          set newUploadFileName [file tail $defFile]
          powRenameFile $defFile
          vwait newUploadFileName
          set defFile $newUploadFileName
          $regionParam(rgns) writeToFile "$g_backupDir/[file tail $defFile]" $regionParam(degreeFormat) $property
          set idx [lsearch $heraClientUploadDirList [list "*" [file tail $defFile]] ]
          set heraClientUploadDir [lindex [lindex $heraClientUploadDirList $idx] 0]

          eval $heraClientObj uploadFileVirtual {$g_backupDir/[file tail $defFile]} $heraClientUploadDir 
          eval $heraClientObj receiveOutput "refreshDir .$heraClientUploadDir"
          file delete -force $g_backupDir/[file tail $defFile]
          set waitFlag save
       } else {
          if { [info exists regionOutputFileName] && $regionOutputFileName != "" } {
             set filename $regionOutputFileName
          } else {
             set filename [tk_getSaveFile -initialfile "$defFile"]
          }
          if {$filename ==  "" } {
             set waitFlag unsave
             return
          }
          $regionParam(rgns) writeToFile $filename $regionParam(degreeFormat) $property
          powUpdateRegionTitle
          set waitFlag save
       }
   }

}

proc powRenameFile { fileName } {
     global oldUploadFileName

     set oldUploadFileName $fileName 

     set top .renameFile
     toplevel .renameFile
     wm geometry $top +[winfo pointerx .]+[winfo pointery .]
     wm title .renameFile "Rename Upload File Name"

     label $top.label   -text "Rename file name if desired" -font g_titleFont
     label $top.entrylb -text "file name:" -font g_titleFont
     entry $top.entry   -text "" -width 30 -background white -font g_titleFont

     grid $top.label   -row 0 -column 0 -columnspan 5 -sticky nws
     grid $top.entrylb -row 1 -column 0 -sticky nws
     grid $top.entry   -row 1 -column 1 -columnspan 4  -sticky nws

     frame $top.actionFrame
     set actionFrame $top.actionFrame
     button $actionFrame.ok -text "Save File" -command { powUpdateFileName }
     grid $actionFrame.ok -row 0 -column 4
     grid $actionFrame -row 6 -column 0 -columnspan 10 -sticky news

     $top.entry delete 0 end
     $top.entry insert end [file tail $fileName]
     bind $top.entry <Return> { powUpdateFileName }

}

proc powUpdateFileName {} {
     global oldUploadFileName
     global newUploadFileName

     set fileDir  [file dirname $oldUploadFileName]
     set fileName [string trim [.renameFile.entry get]]

     set newUploadFileName [format "%s/%s" $fileDir $fileName]
     destroy .renameFile
}

proc powOpenRegionFile { {fName "NONE"} } {
   global regionParam
   global powDWP

   set types {
      {{Region Files}     {.reg}        }
      {{All Files}        *             }
   }

   if { $fName == "NONE" } {
      set fName [$regionParam(rgns) filename]   
      if { $fName!="" } {
         set defFile [file tail $fName]
      } else {
         set defFile ""
      }
      set filename [tk_getOpenFile -filetypes $types -initialfile $defFile]
      if {$filename ==  "" } return

      if { $regionParam(nItems) } {
         set act [tk_dialog ${powDWP}regionInquiry "Open Region File" \
               "Region files already exist" warning 2 Cancel Overwrite Append]
         if { $act==-1 || $act==0 } {return}
         if { $act==1 } {
            $regionParam(rgns) deleteAll
         }
      }
   } else {
      set filename $fName
   }

   catch { $regionParam(rgns) readFromFile $filename } err
   catch { powUpdateRegionTitle } err
}

		  
proc powUpdateRegionList { {idx -1} } {
   global regionParam
   global powDWP
   global convertToFormat powRotation currentRotationList currimg

   if { ![winfo exists ${powDWP}region] } { return }

   set regionParam(format) [$regionParam(rgns) getCoordSys]

   set currItm [$regionParam(rgns) selected]
   if { $idx==-1 } {
      ${powDWP}region.list.rgns delete 0 end
      set theRgns [$regionParam(rgns) regions]
      set n 0
   } else {
      set theRgns [$regionParam(rgns) rgnAtIndex $idx]
      set n $idx
   }

   foreach rgn $theRgns {
      foreach [list sign shape descr] \
            [$regionParam(rgns) buildRegionStr $rgn $regionParam(degreeFormat)]\
            {}
#puts "regionParam(degreeFormat): $regionParam(degreeFormat)"
#puts "readin descr: $descr"
      if [info exists convertToFormat] {
         if { ([string tolower $shape] == "box" || [string tolower $shape] == "ellipse") && [llength $descr] >= 5 } {
            switch $convertToFormat {
                "TO_SKY" {
                  # convert from Pixel value to Sky coordinates, minus powRotation
                  if { [info exists currimg] && [info exists powRotation($currimg)] } {
                     set convrtRot [lindex [lindex $currentRotationList 0] $n]
                     # set convrtRot [expr $convrtRot - $powRotation($currimg)]
                     set descr [lreplace $descr end end $convrtRot]
#puts "PIXEL_TO_SKY: descr: $descr"
                  }
                }
                "TO_PIXEL" {
                  # convert from Sky coordinates to Pixel value, add powRotation
                  if { [info exists currimg] && [info exists powRotation($currimg)] } {
                     set convrtRot [lindex [lindex $currentRotationList 0] $n]
                     set convrtRot [expr $convrtRot + $powRotation($currimg)]
                     set descr [lreplace $descr end end $convrtRot]
#puts "SKY_TO_PIXEL: descr: $descr"
                  }
                }
                default {
                }
            }
         }
         catch { unset convertToFormat }
         catch { unset currentRotationList }
      } elseif { [string first "(pixels)" [string tolower $regionParam(format)]] > 0 && \
                 [info exists currimg] && [info exists powRotation($currimg)] } {
         if { ([string tolower $shape] == "box" || [string tolower $shape] == "ellipse") && [llength $descr] >= 5 } {
            set rot [lindex $descr end]
            set descr [lreplace $descr end end [expr $rot + $powRotation($currimg)]]
         }
      }

      set descr "([join $descr {, }])"
#puts "final descr: $descr"
      set txtDescr "$sign${shape}$descr"
      ${powDWP}region.list.rgns insert $n $txtDescr
      if { $n==$currItm } {
         set regionParam(currSign)  $sign
         set regionParam(currShape) $shape
         set regionParam(currDescr) $descr
         ${powDWP}region.currshape.f2.desc delete 0 end
         ${powDWP}region.currshape.f2.desc insert 0 $descr
      }
      incr n
   }
   if { $idx!=-1 } {
      ${powDWP}region.list.rgns delete [expr $idx+1]
   }

   if { $currItm!=-1 } {
      ${powDWP}region.list.rgns selection set $currItm
      ${powDWP}region.list.rgns see $currItm
   }
   powCalculateImageFlux
}

proc powUpdateSelectedRegion { } {
   global regionParam powDWP

   set rgnIdx [$regionParam(rgns) selected]
   set rgn [$regionParam(rgns) rgnAtIndex $rgnIdx]

   if { [winfo exists ${powDWP}region] } {
      ${powDWP}region.currshape.f2.desc delete 0 end
      foreach [list sign shape descr] \
            [$regionParam(rgns) buildRegionStr $rgn $regionParam(degreeFormat)]\
            {}
      ${powDWP}region.currshape.f2.desc insert 0 "([join $descr {, }])"
      ${powDWP}region.list.rgns select clear 0 end
      ${powDWP}region.list.rgns select set $rgnIdx
      ${powDWP}region.list.rgns see $rgnIdx

      set regionParam(currSign)  $sign
      set regionParam(currShape) $shape
      $regionParam(rgns) setDefault $sign $shape
   }
   powCalculateImageFlux
}

proc powSelectRegion { itemNo } {
    global regionParam
    global powDWP
    global propertyOrder

    if {$itemNo==""} {return}
    $regionParam(rgns) selectRegion $itemNo

    set rgn [$regionParam(rgns) rgnAtIndex $itemNo]
    set propertyOrder [$rgn getPropertyOrder]
}

proc powChangeFormat { newFormat } {
   global regionParam
   global currimg powRotation convertToFormat currentRotationList

   set format [string tolower [lindex $newFormat 0]]
   switch $format {

      #  Changes to degreeFormat

      "decimal" -
      "hhmmss" {
         set regionParam(degreeFormat) $format
      }

      #  Changes to format

      default {
         set fromSys [$regionParam(rgns) getCoordSys]
         set theRgns [$regionParam(rgns) regions]
         set currentRotationList {}
         foreach rgn $theRgns {
            foreach [list sign shape descr] \
                  [$regionParam(rgns) buildRegionStr $rgn $regionParam(degreeFormat)]\
                  {}

            lappend currentRotationList [list [lindex $descr end] $fromSys]
         }

         $regionParam(rgns) setCoordSys $newFormat
         set toSys [$regionParam(rgns) getCoordSys]
         set convertToFormat "NONE"

         if { [string first "Pixel" $fromSys] >= 0 && [string first "Pixel" $toSys] < 0 } {
            # convert from Pixel value to Sky coordinates, minus powRotation
            set convertToFormat "TO_SKY"
         } elseif { [string first "Pixel" $fromSys] < 0 && [string first "Pixel" $toSys] >= 0 } {
            # convert from Sky coordinates to Pixel value, add powRotation
            set convertToFormat "TO_PIXEL"
         } elseif { [string first "Pixel" $fromSys] >= 0 && [string first "Pixel" $toSys] >= 0 } {
            # convert from Pixel value to Pixel value, since value of rotation is from
            # origine plane, add powRotation
            set convertToFormat "TO_PIXEL"
         }

      }
   }

   powUpdateRegionList
}

proc powChangeShape { newShape } {
   global regionParam
   global powDWP

   set d [${powDWP}region.currshape.f2.desc get]
   if { [catch {set descr [$regionParam(rgns) parseRegionStr $d]} errMsg] } {
      set regionParam(currShape) $newShape
      $regionParam(rgns) setDefault \
            $regionParam(currSign) $regionParam(currShape)
      ${powDWP}region.currshape.f2.desc delete 0 end
      return
   }

   foreach [list oldSign oldShape oldDescr oldUnits] $descr {}
   if {$oldShape==$newShape} {return}

   set rgn [gRegion $regionParam(gn) .pow.pow]
   $rgn setSign $oldSign
   $rgn setShape $oldShape
   $rgn setFunction $oldUnits $oldDescr
   set theta    [$rgn getRotation]
   set stdDescr [$rgn getCoords]

    if {$oldShape=="Polygon"} {
	set sumX 0
	set sumY 0
	set sumXX 0
	set sumYY 0
	set sumXY 0
	set cnt 0
	foreach {x y} $stdDescr {
	    set sumX  [expr $sumX +$x]
	    set sumY  [expr $sumY +$y]
	    set sumXX [expr $sumXX+$x*$x]
	    set sumYY [expr $sumYY+$y*$y]
	    set sumXY [expr $sumXY+$x*$y]
	    incr cnt
	}
	set x1 [expr $sumX/$cnt] 
	set y1 [expr $sumY/$cnt]
	set dx [expr sqrt($sumXX/$cnt-$x1*$x1)]
	set dy [expr sqrt($sumYY/$cnt-$y1*$y1)]
	set x2 [expr $x1+$dx]
	set y2 [expr $y1+$dy]
	set stdDescr [list $x1 $y1 $x2 $y2]
    }
    if {$newShape=="Polygon"} {
	set pts  [$rgn getPolygon]
	set npts [expr [llength $pts]-3]
        set theta 0
	set stdDescr [lrange $pts 0 $npts]
    }
    $rgn setRotation $theta
    $rgn setShape    $newShape
    $rgn setCoords   $stdDescr

    foreach [list sign shape descr] \
          [$regionParam(rgns) buildRegionStr $rgn $regionParam(degreeFormat)]\
          {}
    ${powDWP}region.currshape.f2.desc delete 0 end
    ${powDWP}region.currshape.f2.desc insert 0 "([join $descr {, }])"

    set regionParam(currShape) $shape
    $regionParam(rgns) setDefault $sign $shape

   itcl::delete object $rgn
}

proc powToggleSign { } {
   global regionParam powDWP
   global d_negSignLineColor d_negSignHandleColor d_posSignLineColor d_posSignHandleColor

   if {$regionParam(currSign)=="+"} {
      set regionParam(currSign) "-"
   } else {
      set regionParam(currSign) "+"
   }

   if { $regionParam(currSign) == "+" } {
      [${powDWP}region.currshape.f2.lineColor subwidget entry] configure -background $d_posSignLineColor
      [${powDWP}region.currshape.f2.handleColor subwidget entry] configure -background $d_posSignHandleColor
   } else {
      [${powDWP}region.currshape.f2.lineColor subwidget entry] configure -background $d_negSignLineColor
      [${powDWP}region.currshape.f2.handleColor subwidget entry] configure -background $d_negSignHandleColor
   }
   powRegionChangeColor all -1.0
   $regionParam(rgns) setDefault $regionParam(currSign) $regionParam(currShape)
}


proc powChangeRegion { } {
    global regionParam
    global powDWP

    $regionParam(rgns) setDefault $regionParam(currSign) $regionParam(currShape)
    set descr [${powDWP}region.currshape.f2.desc get]
    if { [catch {set newDescr [\
          $regionParam(rgns) parseRegionStr $descr \
          ]}] } {
       return
    }

    foreach {sign shape descr units} $newDescr {}
    $regionParam(rgns) modifyRegion $sign $shape $descr $units
}

proc powDeleteCurrRegion { } {
   global regionParam

   $regionParam(rgns) deleteRegion [$regionParam(rgns) selected]
}

proc powClearRegions { {mode "manual"} } {
    global regionParam powDWP

    if { $mode == "manual" } {
       set act [tk_messageBox -message "Delete All regions?" -type yesno \
	       -default no]

       if { $act=="yes" } {
           $regionParam(rgns) deleteAll
       }
    } else {
       $regionParam(rgns) deleteAll
    }
    catch { 
       set sta ${powDWP}region.fluxprobe
       set childsite [$sta.pixels childsite]
       $childsite.c  configure -text ""
       set childsite [$sta.flux childsite]
       $childsite.c  configure -text ""
       set childsite [$sta.mean childsite]
       $childsite.c  configure -text ""
    }
}

#########
#
#   Handle region clipping
#

proc powConvPoly { P } {

    set PolyGrid ""
    set x [lindex $P 0]
    set y [lindex $P 1]
    for {set i 2} {$i<[llength $P]} {incr i 2} {
	set nextX [lindex $P $i]
	set nextY [lindex $P [expr $i+1]]
	set dx [expr ($nextX-$x)]
	set dy [expr ($nextY-$y)]
	if { $dx || $dy } {
	    lappend PolyGrid [list $x $y $dx $dy]
	}
	set x $nextX
	set y $nextY
    }
    return $PolyGrid
}

proc powClipPolys { P1 P2 } {
#
# Find the intersect region of two polygons.  P1 and P2 *must* be closed
# (ie, P[0]==P[last]) without any adjacent duplicate entries (P[i]=P[i+1])
# If a polygon is irregular (eg, U-shaped), this routine may return a list
# of lists of coordinates, mapping out the disjointed regions.

    set TINY 1e-9
    set ONEPLUS [expr 1+$TINY]
    set ONEMNUS [expr 1-$TINY]

    set Poly1 [powConvPoly $P1]
    set Poly2 [powConvPoly $P2]
    set nPoly1 [llength $Poly1]
    set nPoly2 [llength $Poly2]
    set Ipts ""

    if { !$nPoly1 || !$nPoly2 } {
	if {$nPoly1 \
		&& [powPtInRgn [lindex P2 0] [lindex P2 1] $Poly1]} {
	    return $P2
	} elseif { $nPoly2 \
		&& [powPtInRgn [lindex P1 0] [lindex P1 1] $Poly2]} {
	    return $P1
	}
	return ""
    }

# Find the Intersections of the two regions

    for {set i1 0} {$i1 < $nPoly1} {incr i1} {
	set Seg1 [lindex $Poly1 $i1]
	foreach {a_x a_y a_dx a_dy} $Seg1 {}

	for {set i2 0} {$i2 < $nPoly2} {incr i2} {
	    set Seg2 [lindex $Poly2 $i2]
	    foreach {b_x b_y b_dx b_dy} $Seg2 {}
	    
	    set a $i1
	    set b $i2
	    set num [expr $a_dx*($a_y-$b_y) - $a_dy*($a_x-$b_x)]
	    set den [expr $a_dx*$b_dy - $b_dx*$a_dy]
	    if {$den!=0 || ($den==0 && $num==0)} {
		if {$den==0} {
		    if { [expr abs($b_dx)] > [expr abs($b_dy)] } {
			set fb [expr ($a_x-$b_x)/$b_dx]
		    } else {
			set fb [expr ($a_y-$b_y)/$b_dy]
		    }
		    if {$fb>=0 && $fb<$ONEPLUS} {
			if {$fb>$ONEMNUS} {
			    incr b
			    if {$b==$nPoly2} {set b 0}
			} elseif { $fb>$TINY } {
			    incr b
			    set dx [expr $fb*$b_dx]
			    set dy [expr $fb*$b_dy]
			    set x  [expr $b_x+$dx]
			    set y  [expr $b_y+$dy]
			    set Seg2 [list $b_x $b_y $dx $dy]
			    set newSeg [list $x $y [expr $b_dx-$dx] \
				    [expr $b_dy-$dy] ]
			    set Poly2 [lreplace $Poly2 $i2 $i2 $Seg2 $newSeg]
			    incr nPoly2
			    foreach {b_dx b_dy} "$dx $dy" {}
			    set tmp ""
			    foreach j $Ipts {
				foreach {j1 j2} $j {}
				if {$j2>$i2} {incr j2}
				lappend tmp [list $j1 $j2]
			    }
			    set Ipts $tmp
			}
			set newI [list $a $b]
			if { [lsearch -exact $Ipts $newI]==-1 } {
			    lappend Ipts $newI
			}
			set b $i2
		    }
		    set fb 0
		} else {
		    set fb [expr $num/$den]
		}
		if { $fb>=0 && $fb<$ONEPLUS } {
		    if { [expr abs($a_dx)] > [expr abs($a_dy)] } {
			set fa [expr ($b_dx*$fb+$b_x-$a_x)/$a_dx]
		    } else {
			set fa [expr ($b_dy*$fb+$b_y-$a_y)/$a_dy]
		    }
		    if { $fa>=0 && $fa<$ONEPLUS } {
			if { $fa>$ONEMNUS } {
			    incr a
			    if {$a==$nPoly1} {set a 0}
			} elseif { $fa>$TINY } {
			    incr a
			    set dx [expr $fa*$a_dx]
			    set dy [expr $fa*$a_dy]
			    set x  [expr $a_x+$dx]
			    set y  [expr $a_y+$dy]
			    set Seg1 [list $a_x $a_y $dx $dy]
			    set newSeg [list $x $y [expr $a_dx-$dx] \
				    [expr $a_dy-$dy] ]
			    set Poly1 [lreplace $Poly1 $i1 $i1 $Seg1 $newSeg]
			    incr nPoly1
			    foreach {a_dx a_dy} "$dx $dy" {}
			    set tmp ""
			    foreach j $Ipts {
				foreach {j1 j2} $j {}
				if {$j1>$i1} {incr j1}
				lappend tmp [list $j1 $j2]
			    }
			    set Ipts $tmp
			}
			if { $fb>$ONEMNUS } {
			    incr b
			    if {$b==$nPoly2} {set b 0}
			} elseif { $fb>$TINY } {
			    incr b
			    set dx [expr $fb*$b_dx]
			    set dy [expr $fb*$b_dy]
			    set x  [expr $b_x+$dx]
			    set y  [expr $b_y+$dy]
			    set Seg2 [list $b_x $b_y $dx $dy]
			    set newSeg [list $x $y [expr $b_dx-$dx] \
				    [expr $b_dy-$dy] ]
			    set Poly2 [lreplace $Poly2 $i2 $i2 $Seg2 $newSeg]
			    incr nPoly2
			    foreach {b_dx b_dy} "$dx $dy" {}
			    set tmp ""
			    foreach j $Ipts {
				foreach {j1 j2} $j {}
				if {$j2>$i2} {incr j2}
				lappend tmp [list $j1 $j2]
			    }
			    set Ipts $tmp
			    incr i2
			}
			set newI [list $a $b]
			if { [lsearch -exact $Ipts $newI]==-1 } {
			    lappend Ipts $newI
			}
		    }
		}
	    }
	}
    }
    
    set Ipts [lsort -command {powSortIntSects 0} $Ipts]
    set Jpts [lsort -command {powSortIntSects 1} $Ipts]
    set Npts [llength $Ipts]

#    powClipDump $Ipts $Poly1 $Poly2

# If there are no intersections or only 1, return:
#      P1 if P1 is inside P2
#      P2 if P1 encloses P2
#      empty if there is no overlap

# If there is only one intersection, make sure first point isn't lying
# right on the other Polygon.  If it is, use second point for test.

    set state [powPtInRgn [lindex $P1 0] [lindex $P1 1] $Poly2]
    if { $Npts==1 } {
	set i1 [lindex [lindex $Ipts 0] 0]
	set za [lindex [lindex $Ipts 0] 2]
	if { $i1==0 && $za==0 } {
	    set state [powPtInRgn [lindex $P1 2] [lindex $P2 3] $Poly2]
	}
	set Npts 0
    }

    if { ! $Npts } {
	if {$state} { return [list $P1] }
	set state [powPtInRgn [lindex $P2 0] [lindex $P2 1] $Poly1]
	if {$state} { return [list $P2] }
	return ""
    }

# Do loop over all the intersections and make sure they all make it
# into the clipped region... this allows for disjointed clip regions

  set Mclips ""
  for {set IntSects 0} {$IntSects<$Npts} {incr IntSects} {
    if { ![info exists Iused($IntSects)] } {

        set I $IntSects
	set J [lsearch -exact $Jpts [lindex $Ipts $I] ]
	set Idir 1
	set Jdir 1
	set endPt $I
	set errFlag 0

	foreach {ia1 ib1} [lindex $Ipts $I] {}
	set pt [lindex $Poly1 $ia1]
	foreach {x y dx dy} $pt {}
	set clipped "$x $y"
	set Iused($I) 1

    while { 1 } {

	foreach {ia1 ib1} [lindex $Ipts $I] {}
	foreach {x1 y1 dx1 dy1} [lindex $Poly1 $ia1] {}
	foreach {x2 y2 dx2 dy2} [lindex_wrap $Poly1 [expr $ia1-1] ] {}
	set posState [powPtInRgn [expr $x1+0.5*$dx1] [expr $y1+0.5*$dy1] $Poly2]
	set negState [powPtInRgn [expr $x2+0.5*$dx2] [expr $y2+0.5*$dy2] $Poly2]

	set doP1 1
	if { $posState && !$negState } {
	    set Idir 1
	} elseif { !$posState && $negState } {
	    set Idir -1
	} elseif { $posState } {
	    set prevI [lsearch -exact $Ipts [lindex_wrap $Jpts [expr $J-$Jdir]]]
	    set Idir [expr $I-$prevI]
	    if { $Idir<-1 } {set Idir  1}
	    if { $Idir> 1 } {set Idir -1}
	} else {
#                  Both directions outside region... 
#    If this wasn't first point tested, return to tracing other region.
#    Otherwise, it was a single point intersection, so just finish.
	    set doP1 0
	    if { $I!=$endPt } {
		set ia2 [lindex [lindex_wrap $Ipts [expr $I+1] ] 0]
		if { $ia1==$ia2 } {
		    incr I
		} else { incr I -1 }
		set J [lsearch -exact $Jpts [lindex_wrap $Ipts $I] ]
	    }
	}

	while { $doP1 } {

	    foreach {ia1 ib1} [lindex $Ipts $I] {}
	    incr I $Idir
	    if {$I==$Npts} {set I 0} elseif {$I<0} {set I [expr $Npts-1]}
	    
# Copy over all polygon1 segments inside polygon2 (ia1->ia2)

	    foreach {ia2 ib2} [lindex $Ipts $I] {}
	    while {$ia1!=$ia2} {
		incr ia1 $Idir
		if {$ia1==$nPoly1} {set ia1 0} \
			elseif {$ia1<0} {set ia1 [expr $nPoly1-1]}
		set pt [lindex $Poly1 $ia1]
		lappend clipped [lindex $pt 0]
		lappend clipped [lindex $pt 1]
	    }
	    set J [lsearch -exact $Jpts [lindex $Ipts $I] ]
	    set Iused($I) 1
	    if {$I==$endPt} {break}

	    if { [llength $clipped] > [expr 3*($nPoly1+$nPoly2)]} {
		set errFlag 1
		tk_messageBox -message "Got lost clipping regions!  Region is too complex."
		break
	    }
# Check whether we actually need to switch to P2... test whether the middle
# point of the next segment is still inside P2

	    if {$Idir==1} {
		foreach {x y dx dy} [lindex $Poly1 $ia2] {}
	    } else {
		foreach {x y dx dy} [lindex_wrap $Poly1 [expr $ia2-1]] {}
	    }
	    if { ![powPtInRgn [expr $x+0.5*$dx] [expr $y+0.5*$dy] $Poly2] } {
		break
	    }
	}
	if {$I==$endPt} {break}
	if {$errFlag} {break}

# Follow polygon2 around until re-intersect polygon1
# First, need to know what direction to go!

	foreach {ja1 jb1} [lindex $Jpts $J] {}
	foreach {x1 y1 dx1 dy1} [lindex $Poly2 $jb1] {}
	foreach {x2 y2 dx2 dy2} [lindex_wrap $Poly2 [expr $jb1-1] ] {}
	set posState [powPtInRgn [expr $x1+0.5*$dx1] [expr $y1+0.5*$dy1] $Poly1]
	set negState [powPtInRgn [expr $x2+0.5*$dx2] [expr $y2+0.5*$dy2] $Poly1]

	set doP2 1
	if { $posState && !$negState } {
	    set Jdir 1
	} elseif { !$posState && $negState } {
	    set Jdir -1
	} elseif { $posState } {
	    set prevJ [lsearch -exact $Jpts [lindex_wrap $Ipts [expr $I-$Idir]]]
	    set Jdir [expr $J-$prevJ]
	    if { $Jdir<-1 } {set Jdir  1}
	    if { $Jdir> 1 } {set Jdir -1}
	} else {
	    set jb2 [lindex [lindex_wrap $Jpts [expr $J+1] ] 1]
	    if { $jb1==$jb2 } {
		incr J
	    } else { incr J -1 }
	    set I [lsearch -exact $Ipts [lindex_wrap $Jpts $J] ]
	    set doP2 0
	}

	while { $doP2 } {

	    foreach {ja1 jb1} [lindex $Jpts $J] {}
	    incr J $Jdir
	    if {$J==$Npts} {set J 0} elseif {$J<0} {set J [expr $Npts-1]}

# Copy over all polygon2 segments inside polygon1 (jb1->jb2)

	    foreach {ja2 jb2} [lindex $Jpts $J] {}
	    while {$jb1!=$jb2} {
		incr jb1 $Jdir
		if {$jb1==$nPoly2} {set jb1 0} \
			elseif {$jb1<0} {set jb1 [expr $nPoly2-1]}
		set pt [lindex $Poly2 $jb1]
		lappend clipped [lindex $pt 0]
		lappend clipped [lindex $pt 1]
	    }
	    set I [lsearch -exact $Ipts [lindex $Jpts $J] ]
	    set Iused($I) 1
	    if {$I==$endPt} {break}

# Check whether we actually need to switch to P1... test whether the middle
# point of the next segment is still inside P1

	    if { [llength $clipped] > [expr 3*($nPoly1+$nPoly2)]} {
		set errFlag 1
		tk_messageBox -message "Got lost clipping regions!  Region is too complex."
		break
	    }

	    if {$Jdir==1} {
		foreach {x y dx dy} [lindex $Poly2 $jb2] {}
	    } else {
		foreach {x y dx dy} [lindex_wrap $Poly2 [expr $jb2-1]] {}
	    }
	    if { ![powPtInRgn [expr $x+0.5*$dx] [expr $y+0.5*$dy] $Poly1] } {
		break
	    }
	}
	if {$I==$endPt} {break}
	if {$errFlag} {break}

    }
    lappend Mclips $clipped
}
}
return $Mclips
}

proc powClipDump { Ints P1 P2 } {

    puts "\nPolygon1"
    set i 0
    foreach is $P1 {
 	puts "[eval [concat format \"%2d  %9.3f  %9.3f  %9.3f  %9.3f\" $i $is]]"
	incr i
    }
    puts "Polygon2"
    set i 0
    foreach is $P2 {
 	puts "[eval [concat format \"%2d  %9.3f  %9.3f  %9.3f  %9.3f\" $i $is]]"
	incr i
    }
    puts "Intersections: "
    set i 0
    foreach is $Ints {
 	puts "[eval [concat format \"%2d  %2d  %2d\" $i $is]]"
	incr i
    }
}

proc lindex_wrap { L i } {
    set n [llength $L]
    while { $i < 0 }   { incr i $n }
    while { $i >= $n } { incr i [expr -$n] }
    return [lindex $L $i]
}

proc powSortIntSects { elem a b } {
    set d [expr [lindex $a $elem] - [lindex $b $elem] ]
    if {$d==0} {
	set d [expr [lindex $a [expr 1-$elem] ] - [lindex $b [expr 1-$elem] ] ]
    }
    if { $d<0 } {return -1} elseif { $d>0 } {return 1} else {return 0}
}

proc powPtInRgn { x y Poly } {

    lappend Poly [lindex $Poly 0]
    set nPoly [llength $Poly]

    set next [lindex $Poly 0]
    foreach {n_x n_y n_dx n_dy} $next {}

    set flag 0
    for {set cnt 1} {$cnt < $nPoly} {incr cnt} {
	set nxt [list $n_x $n_y $n_dx $n_dy]
	foreach {b_x b_y b_dx b_dy} $nxt {}
	foreach {n_x n_y n_dx n_dy} [lindex $Poly $cnt] {}
	if { ($y>$b_y && $y>=$n_y) || ($y<$b_y && $y<=$n_y) \
		|| ($x>$b_x && $x>=$n_x) } {
	    continue
	}

# Check to see if x,y lies right on the segment

	if { $x>=$b_x || $x>$n_x } {
	    set dy [expr $y-$b_y]
	    if { [expr abs($b_dy)]<1e-10 } {
		if { [expr abs($dy)]<1e-10 } {
		    return 1
		} else {
		    continue
		}
	    }
	    set dx [expr $b_x + ($b_dx/$b_dy)*$dy - $x]
	    if { $dx < -1e-10 } {continue}
	    if { $dx < 1e-10 } {return 1}
	}

# There is an intersection! Make sure it isn't a V point.    

	if { $y!=$b_y } {
	    set flag [expr 1-$flag]
	} else {
	    set idx [expr $cnt-1]
	    while {1} {
		if {$idx} {incr idx -1} else {set idx [expr $nPoly-2]}
		set prevdy [lindex [lindex $Poly $idx] 3]
		if {$prevdy} {break}
	    }
	    if {$b_dy*$prevdy > 0} {
		set flag [expr 1-$flag]
	    }
	}
    }
    return $flag
} 

proc powCalculateImageFlux {} {
     global currimg
     global powbg
     global powDWP g_titleFont
     global regionParam
     global g_backupDir 
     global powRotation

     set probeFormat decimal

#   Use SAO Format

     set theRgns [$regionParam(rgns) regions]
     if {$theRgns == ""} {
         return
     }

     set regionFileName $g_backupDir/pow_[clock seconds].reg
     set f [open $regionFileName "w+"]

     set numberShape 0
     for {set i 0} {$i < [llength $theRgns]} {incr i} {
         set probeSelected [lindex $theRgns $i]

         set shape   [$probeSelected getShape ]
         set descr   [$probeSelected getFunction "pixels" ]
         set sign    [$probeSelected getSign]
         if { $regionParam(format) == "Physical (Pixels)" } {

            set new_descr ""
            if { $shape=="Line" || $shape=="Polygon" || $shape=="Point" } {

               #  These objects consist of just pairs of coordinates
   
               foreach [list phy_x phy_y] $descr { 
                  set result [powConvertPhysical2Image $phy_x $phy_y]
                  set img_x [lindex $result 0]
                  set img_y [lindex $result 1]
                  if { $new_descr == "" } {
                     set new_descr [format "%s %s" $img_x $img_y]
                  } else {
                     set new_descr [format "%s %s %s" $new_descr $img_x $img_y]
                  }
               }
   
            } else {
               set tokenList [split $descr " "]
               set phy_x [lindex $tokenList 0]
               set phy_y [lindex $tokenList 1]
               set result [powConvertPhysical2Image $phy_x $phy_y]
               set img_x [lindex $result 0]
               set img_y [lindex $result 1]
               set new_descr [format "%s %s" $img_x $img_y]

               if { $shape=="Circle" } {
                  set phy_radius [lindex $tokenList 2]
                  set img_radius [powConvertRadiusPhysical2Image $phy_x $phy_y $img_x $phy_radius]
                  set new_descr [format "%s %s" $new_descr $img_radius]
               } else {
                  set width  [lindex $tokenList 2]
                  set height [lindex $tokenList 3] 
                  set rot    [lindex $tokenList 4]
                  set phy_xn [expr $phy_x + $width]
                  set phy_yn [expr $phy_y + $height]
                  set result [powConvertPhysical2Image $phy_xn $phy_yn]
                  set img_xn [lindex $result 0]
                  set img_yn [lindex $result 1]
                  set new_width  [expr $img_xn - $img_x]
                  set new_height [expr $img_yn - $img_y]
                  set new_descr [format "%s %s %s %s" $new_descr $new_width $new_height $rot]
               }
            }

            set descr $new_descr 
         }

         if { [string tolower $shape] == "polygon" } {
            set token [split $descr " "]
            if { [llength $token] <= 4 } {
               continue
            }
         }

         if { ([string tolower $shape] == "box" || [string tolower $shape] == "ellipse") && [llength $descr] >= 5 } {
            set rot [lindex $descr end]
            if { [info exists currimg] && [info exists powRotation($currimg)] } {
               set rot [expr $rot + $powRotation($currimg)]
               set descr [lreplace $descr end end $rot]
            }
         }

         # update data
         set tempdescr "([join $descr {, }])"
         set rgnDescr "$sign[string tolower ${shape}]$tempdescr"

         puts $f $rgnDescr
         incr numberShape
     }
     close $f

     if { $numberShape <= 0 } return

     set results [powGetRegionStatistics $currimg $regionFileName $descr $shape $sign ]

     file delete -force $regionFileName
     set good  [lindex $results 0]
      if {$good == 1} {
          set probeCentX [lindex $results 1]
          set probeCentY [lindex $results 2]
          set probeStdX [lindex $results 3]
          set probeStdY [lindex $results 4]
          set probeFlux [lindex $results 5]
          set probeNPix [lindex $results 6]
          set probeMean [ format "%.10g" [lindex $results 7] ]
          set probeDMean [ format "%.10g" [lindex $results 8] ]
          set pixel [format "(%.2f, %.2f) +- (%.2f, %.2f)"  \
              $probeCentX  $probeCentY  $probeStdX $probeStdY ]
          foreach {graphx graphy} [powPixelToGraph $currimg \
              [expr $probeCentX - 1]  [expr $probeCentY - 1] ] {}
          foreach {gx1 gy1} [powPixelToGraph $currimg \
                [expr $probeCentX - 1 - $probeStdX ]  \
                [expr $probeCentY - 1 - $probeStdY ] ] {}
          foreach {gx2 gy2} [powPixelToGraph $currimg \
                [expr $probeCentX - 1 + $probeStdX ]  \
                [expr $probeCentY - 1 + $probeStdY ] ] {}
          set graphdx [expr abs($gx2 - $gx1)/2.0 ]
          set graphdy [expr abs($gy2 - $gy1)/2.0 ]
          if {$probeFormat == "decimal" } {
             set graphx [format %.6g $graphx]
             set graphy [format %.6g $graphy]
             set graphdx [format %.3g $graphdx]
             set graphdy [format %.3g $graphdy]
          } else {
             set graphx [powHourRA $graphx "%02d:%02d:%05.2f"]
             set graphy [powDegDec $graphy]
             set graphdx [powHourRA $graphdx "%02d:%02d:%05.2f"]
             set graphdy [powDegDec $graphdy]
         }
         set coord "($graphx, $graphy) +- ($graphdx, $graphdy)"
      } else {
          set probeCentX X
          set probeCentY Y
          set probeStdX ""
          set probeStdY ""
          set probeFlux 0.0
          set probeNPix 0
          set pixel  "(X,Y) +- (dX,dY)"
          set coord  "(X,Y) +- (dX,dY)"
          set probeMean   0.0
          set probeDMean   0.0
          if { $good == 504 } {
             tk_messageBox -message "Current projection is not supported." -type ok -icon error
          } else {
             tk_messageBox -message "Failed to get statistical values for region., err code: $good" -type ok -icon error
          }
      }
  
  #  Update the readout in dialog box.
     set sta ${powDWP}region.fluxprobe
     set childsite [$sta.pixels childsite]
     $childsite.c  configure -text $probeNPix
     set childsite [$sta.flux childsite]
     $childsite.c  configure -text $probeFlux
     set childsite [$sta.mean childsite]
     $childsite.c  configure -text "$probeMean +- $probeDMean"
}
