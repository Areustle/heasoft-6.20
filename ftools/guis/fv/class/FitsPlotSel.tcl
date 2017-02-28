# rewritten by Han Huang 05/23/01
#   more OOP-correct

itcl::class FitsPlotSel {
    inherit itk::Toplevel

    constructor {args} {}

    public method quitCmd { }
    public method turnsel {flag}
    public method returnAddMyCurve {} { return $cbCurrgn }

    private method _plotCmd {}
    private method _selAxis  {args}
    private method _clearCmd {}

    private variable colList {}
    private variable vecList {}
    private variable xAxis ""
    private variable yAxis ""
    private variable exAxis ""
    private variable eyAxis ""
    private variable rows   ""

    private variable execute ""

#Ziqin Pan
    private variable _plotselonly

    # Keep this common so that subsequent invocations will retain the setting
    private common cbCurrgn 0
 
    private variable id ""
}

itcl::body FitsPlotSel::constructor {args} {
# frame layout

    global g_titleFont
    option add *FitsPlotSel.font g_titleFont

    itk_component add  lbFrame {
        frame $itk_interior.flb 
    }
    pack $itk_component(lbFrame) -side left  -fill both -expand 1

    itk_component add  eFrame {
        frame $itk_interior.ef
    }
    pack $itk_component(eFrame) -side top  -fill x -expand 1

#button frame
    itk_component add  bFrame {
        frame $itk_interior.fb -borderwidth 2 -relief ridge
    }
    pack $itk_component(bFrame) -side top -fill both -expand 1

# listbox + scrollbr
           set itk_interior $itk_component(lbFrame)
    itk_component add  listbox {
        listbox $itk_interior.lb  -yscrollcommand \
               [itcl::code $itk_interior.sb set]
    }
    pack $itk_component(listbox) -side left -fill both  -expand 1 
    set id [lindex $args 0]
    set colList [lindex $args 1] 
    set vecList [lindex $args 2]
    set execute [lindex $args 3]

    #  Fill listbox

    for {set i 0} { $i < [llength $colList] } {incr i} {
         set tmpVec [lindex $vecList $i]
         if { [llength $tmpVec]>1 || [lindex $tmpVec 0]>1 } {
             set vecStr [join $tmpVec ,]
             $itk_component(listbox) insert end \
                 "[lindex $colList $i]\[$vecStr\]"
         } else {
             $itk_component(listbox) insert end [lindex $colList $i]
         }
    }

    itk_component add  scrollbar {
        scrollbar $itk_interior.sb -command \
            [itcl::code $itk_interior.lb yview]
    }
    pack $itk_component(scrollbar) -side left -fill y -expand 1
# 
    set itk_interior $itk_component(eFrame)

    itk_component add  axisl {
        label $itk_interior.asixl -text "Axis" 
    }
    itk_component add  columnl {
        label $itk_interior.columnl -text "Column name or expression to plot" 
    }

    itk_component add  xbutton {
        button $itk_interior.xb -text "X" \
           -command [itcl::code $this _selAxis X]
    }
    itk_component add  xEntry {
        entry $itk_interior.xe -textvariable [itcl::scope xAxis] 
    }
# 
    itk_component add  ybutton {
        button $itk_interior.yb -text "Y" \
	    -command [itcl::code $this _selAxis Y]
    }
	itk_component add  yEntry {
	    entry $itk_interior.ye -textvariable [itcl::scope yAxis]
    }
# 
    itk_component add  exbutton {
        button $itk_interior.exb -text "X Error" \
            -command [itcl::code $this _selAxis EX]
    }
    itk_component add  exEntry {
        entry $itk_interior.exe -textvariable [itcl::scope exAxis]
    }
#
    itk_component add  eybutton {
	button $itk_interior.eyb -text "Y Error" \
	    -command [itcl::code $this _selAxis EY]
    }
    itk_component add  eyEntry {
	entry $itk_interior.eye -textvariable [itcl::scope eyAxis] 
    }

    itk_component add rowEntry {
	entry $itk_interior.rowe -textvariable [itcl::scope rows] 
    }

    itk_component add rowLabel {
	label $itk_interior.rowl -text "Rows:"
    }

    itk_component add selonlyCheckButton {
	checkbutton $itk_interior.selonly -text "Use selected rows" \
                -variable [itcl::scope _plotselonly ] \
		 -selectcolor $fvPref::checkBBgColor  \
		 -activeforeground black \
		 -activebackground $fvPref::globalBgColor \
                 -state disabled

    } {
        usual
        ignore -selectcolor -activeforeground -activebackground
    }

    itk_component add cbCurrgn {
	checkbutton $itk_interior.cbCurrgn -variable [itcl::scope cbCurrgn] \
		 -text "Add my curve to current graph" \
		 -selectcolor $fvPref::checkBBgColor  \
		 -activeforeground black \
		 -activebackground $fvPref::globalBgColor
    } {
        usual
        ignore -selectcolor -activeforeground -activebackground
    }

    itk_component add help {
	label $itk_interior.help -text "Click on a column name then select the \n\
                     corresponding plot axis or error bar" \
	    -borderwidth 2 -relief ridge
    }
    grid configure  $itk_component(help)  -column 0 -row 0 \
	    -columnspan 2 -sticky "snew"
    grid configure  $itk_component(axisl) -column 0 -row 1 -sticky "snew"
    grid configure  $itk_component(columnl) -column 1 -row 1 -sticky "snew"

    grid configure  $itk_component(xbutton) -column 0 -row 2 -sticky "snew"
    grid configure  $itk_component(ybutton) -column 0 -row 3 -sticky "snew"
    grid configure  $itk_component(exbutton) -column 0 -row 4 -sticky "snew"
    grid configure  $itk_component(eybutton) -column 0 -row 5 -sticky "snew"
    grid configure  $itk_component(rowLabel) -column 0 -row 6 -sticky "snew"

    grid configure  $itk_component(xEntry)  -column 1 -row 2 -sticky "snew"
    grid configure  $itk_component(yEntry)  -column 1 -row 3 -sticky "snew"
    grid configure  $itk_component(exEntry) -column 1 -row 4 -sticky "snew"
    grid configure  $itk_component(eyEntry) -column 1 -row 5 -sticky "snew"
    grid configure  $itk_component(rowEntry) -column 1 -row 6 -sticky "snew"

    grid configure  $itk_component(selonlyCheckButton) -column 0 -row 7 \
              -sticky "snew" -columnspan 2 -padx 5
    grid configure  $itk_interior.cbCurrgn -column 0 -row 8 \
	      -sticky "snew" -columnspan 2 -padx 5
    grid columnconfigure $itk_component(eFrame) 1 -weight 5
#
    set itk_interior $itk_component(bFrame) 

    itk_component add  gobutton {
        button $itk_interior.gob -text "Plot"  -width 5\
	    -command [itcl::code $this _plotCmd]
    }
        
    itk_component add  clearbutton {
	button $itk_interior.clearb -text "Clear"  -width 5\
	    -command [itcl::code $this _clearCmd]
    }

    itk_component add  quitbutton {
	button $itk_interior.quitb -text "Close"  -width 5\
	    -command [itcl::code $this quitCmd]
    }

    itk_component add helpbutton {
	button $itk_interior.helpb -text "Help" -width 5 \
	    -command "hhelp plotDialog"
    }
    grid config $itk_component(gobutton) -padx 5\
	    -column 0 -row 1 -sticky "snew"
    grid config $itk_component(clearbutton) -padx 5\
	    -column 1 -row 1 -sticky "snew"
    grid config $itk_component(quitbutton) -padx 5\
	    -column 2 -row 1 -sticky "snew"
    grid config $itk_component(helpbutton) -padx 5\
	    -column 3 -row 1 -sticky "snew"
	
    set xAxis  ""
    set yAxis  ""
    set exAxis ""
    set eyAxis ""

    eval itk_initialize
}

itcl::body FitsPlotSel::_plotCmd {} {

    # check the element range
    if { ($xAxis == "") || ($yAxis == "")} {
        error "Need both X and Y axes"
        return
    }

    $execute $xAxis $exAxis $yAxis $eyAxis $cbCurrgn $rows $_plotselonly
    # destroy $itk_component(hull)
}

itcl::body FitsPlotSel::quitCmd { } {
    destroy $itk_component(hull)
}
itcl::body FitsPlotSel::turnsel {flag} {
    if {$flag == 1} {
        $itk_component(selonlyCheckButton) configure -state normal
    } else {
        $itk_component(selonlyCheckButton) configure -state disabled
    }
}

itcl::body FitsPlotSel::_selAxis  {args} {
    if { [$itk_component(listbox) curselection] == "" } {
        return
    }
    set column [lindex $colList [$itk_component(listbox) curselection]]
    set vecSize [lindex $vecList [$itk_component(listbox) curselection]]
    if { $column == ""} {
        puts "please select a column name"
	return
    }
    if { [string first " " $column] != -1 } {
        set bracket [string first \[ $column]
        if { $bracket == -1 } {
            set column "\$$column\$"
        } else {
            set column "\$[string range $column 0 [expr $bracket-1]]\$[string range $column $bracket end]"
        }
     }

    switch $args {
        X {
            set xAxis  $column
        }  Y {
            set yAxis  $column
        } EX {
            set exAxis $column
        } EY {
            set eyAxis $column
        }
    }

}
	
itcl::body FitsPlotSel::_clearCmd {} {
    set xAxis ""
    set yAxis ""
    set exAxis ""
    set eyAxis ""
    set cbCurrgn 0
#	$itk_component(vecmsg) configure -text " \n"
}
