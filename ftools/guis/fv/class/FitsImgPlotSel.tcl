itcl::class FitsImgPlotSel {
    inherit itk::Toplevel
    
    private common range 
    private common typeSelection
    private common cbCurrgn

    private variable id
    private variable _numRows
    private variable _numCols
    private variable entryWidth 15

    private variable _oneDim 0
    
    private method doneCmd {}
    private method cancelCmd {}
    private method helpCmd {}
    private method parseRange {} 
    private method selType {}
    

    constructor {args} {
	global activeBgColor
	global checkBBgColor
	global globalBgColor
        global g_titleFont

        option add *FitsImgPlotSel.font g_titleFont

	set id [lindex $args 0]
	set _numCols [lindex $args 1]
	set _numRows [lindex $args 2]

        # window looks and behaves differently when there's only one column
	# when there's only one column, user can only choose a range of rows (not columns)
        if { $_numCols == 1 } {
            set _oneDim 1
	}

	# labels
	itk_component add title {
            if { !$_oneDim } {
		message $itk_interior.mssg -text "Image Size: $_numCols X $_numRows" \
			-width 300 -relief ridge -borderwidth 4 -font g_titleFont
	    } else {
		message $itk_interior.mssg -text "Rows change 1-$_numRows" \
			-width 300 -relief ridge -borderwidth 4 -font g_titleFont
	    }
	}	
	
        if { !$_oneDim } {
	    set [itcl::scope typeSelection] row
	    itk_component add selRow {
		radiobutton $itk_interior.selrow -text "Plot Rows" \
			-variable [itcl::scope typeSelection] -value row \
			-selectcolor $fvPref::checkBBgColor  \
			-activeforeground black -activebackground $fvPref::globalBgColor \
			-command [itcl::code $this selType] -font g_titleFont
		
	    } 
	} else {
	    set [itcl::scope typeSelection] col
	}

	itk_component add selCol {
	    radiobutton $itk_interior.selcol -text "Plot Columns" \
		    -variable [itcl::scope typeSelection] -value col \
		    -selectcolor $fvPref::checkBBgColor  \
		    -activeforeground black -activebackground $fvPref::globalBgColor \
		    -command [itcl::code $this selType] -font g_titleFont
	} 

	if { !$_oneDim } {
	    itk_component add  rangeLabel {
		label $itk_interior.rangel -text "Range of rows to plot" \
			-width 25 -font g_titleFont
	    }
	    itk_component add  rangeEntry {
		entry $itk_interior.rangee  -textvariable [itcl::scope range($id)] \
			-width $entryWidth  -font g_titleFont
	    }
	} else {
	    itk_component add  rangeLabel {
		label $itk_interior.rangel -text "Range of columns to plot" \
			-width 25 -font g_titleFont
	    }
	    set range($id) 1
	    itk_component add  rangeEntry {
		entry $itk_interior.rangee  -textvariable [itcl::scope range($id)] \
			-width $entryWidth -font g_titleFont
	    }
	}
	bind $itk_component(rangeEntry) <Return> [itcl::code $this doneCmd]

	itk_component add sep {
	    frame $itk_interior.sep -height 4 -borderwidth 2 -relief raised
	}	
	itk_component add sep1 {
	    frame $itk_interior.sep1 -height 4 -borderwidth 2 -relief raised
	}	

	set cbCurrgn 0
	itk_component add cbCurrgn {
	  checkbutton $itk_interior.cbCurrgn -variable [itcl::scope cbCurrgn] \
		 -text "Add curve to current graph" \
		 -selectcolor $fvPref::checkBBgColor  \
		 -activeforeground black \
		 -activebackground $fvPref::globalBgColor -font g_titleFont
	} {
	  usual
	  ignore -selectcolor -activeforeground -activebackground
	}

	itk_component add  Button {
	    iwidgets::buttonbox $itk_interior.bb 
	} 

	$itk_component(Button) add make -text "Plot" \
	    -command [itcl::code $this doneCmd] -font g_titleFont
	$itk_component(Button) add cancel -text Cancel \
	    -command [itcl::code $this cancelCmd] -font g_titleFont
	$itk_component(Button) add help  -text Help \
	    -command [itcl::code $this helpCmd] -font g_titleFont

	grid configure $itk_component(title) -column 0 -row 0 \
	    -rowspan 2 -sticky "snew"
	if { !$_oneDim } {
	    grid configure $itk_component(selRow) -column 1 -row 0 \
		    -sticky "sw"
	}
	grid configure $itk_component(selCol) -column 1 -row 1 \
		-sticky "sw"
	grid configure $itk_component(sep1) -column 0 -row 2 \
	     -columnspan 4 -sticky "snew"
	grid configure $itk_component(rangeLabel) -column 0 -row 3 \
	     -sticky "snew" -pady 5
	grid configure $itk_component(rangeEntry) -column 1 -row 3 \
	     -sticky "snew" -pady 5
	grid configure $itk_component(sep) -column 0 -row 4 \
	     -columnspan 2 -sticky "snew"
	grid configure $itk_component(cbCurrgn) -column 0 -row 5 \
	     -columnspan 2 -sticky "snew"
	grid configure $itk_component(Button) -column 0 -row 6 \
	     -columnspan 2 -sticky "snew"
    }

    destructor {
    }
}

itcl::body FitsImgPlotSel::doneCmd {} {
    global g_GL


    if { $range($id) == " " | $range($id) == ""} {
	error "Please enter the range to plot"
	return
    }
    if { $typeSelection == "row" } {
	set rangeList [parseRange]
        set start [lindex $rangeList 0]
        set end   [lindex $rangeList 1]
        if { $start<1 || $start>$_numRows || $end<1 || $end>$_numRows } {
           error "Row range is out of range"
           return
        }
	lappend paramList "row"
    } else {
	set rangeList [parseRange]
        set start [lindex $rangeList 0]
        set end   [lindex $rangeList 1]
        if { $start<1 || $start>$_numCols || $end<1 || $end>$_numCols } {
           error "Column range is out of range"
           return
        }
	lappend paramList "column"
    }
    lappend paramList $start
    lappend paramList $end
    lappend paramList $cbCurrgn
    
    set g_GL($id,plotImg) [join $paramList]

    itcl::delete object $this
}

itcl::body FitsImgPlotSel::cancelCmd {} {
    global g_GL

    set g_GL($id,plotImg) ""
    itcl::delete object $this
}

itcl::body FitsImgPlotSel::helpCmd {} {
    hhelp imagePlot
}

itcl::body FitsImgPlotSel::parseRange {} {

    regsub -all " " $range($id) "" tmprange

    if { $tmprange == "" } {
	if { $typeSelection == "row" } {
	    return [list 1 $_numCols]
	} else {
	    return [list 1 $_numRows]
	}
    }
    
    set start 1
    set end 1
    set rangeCount [scan $tmprange "%d-%d" start end]
    if { $rangeCount == 1 } {
	set end $start
    } elseif { $rangeCount == 2} {
	;
    } else {
	error "Parse error: range"
	return
    }
    return [list $start $end]
}

itcl::body FitsImgPlotSel::selType {} {
    if { $typeSelection == "row" } {
	$itk_component(rangeLabel) configure -text "Range of rows to plot"
    } else {
	$itk_component(rangeLabel) configure -text "Range of columns to plot"
    }
}
