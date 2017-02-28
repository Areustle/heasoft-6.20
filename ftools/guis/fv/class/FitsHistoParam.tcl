itcl::class FitsHistoParam {
    inherit itk::Toplevel

    private variable xColName 
    private variable yColName 
    private variable cColName 
    private variable rowRange 
    private variable xMin 
    private variable yMin 
    private variable xMax 
    private variable yMax 
    private variable uxMin 
    private variable uyMin 
    private variable uxMax 
    private variable uyMax     
    private variable tlxMin 
    private variable tlyMin 
    private variable tlxMax 
    private variable tlyMax 
    private variable xBin 
    private variable yBin  
    private variable _colNames
    private variable _histselonly
    private variable fTable
    
    private variable fFile
    private variable type

    public  method insertDefaultValue {} 
    public  method getXMinMax {}
    public  method getYMinMax {}
    public  method getMinMax {}
    public  method turnsel {flag}

    private method quitCmd {} 
    private method _makeHistogram { closeFlag }
    private method enableEntry {}
    private method disableEntry {}
    private method getSelRange {}

    constructor {args} {
        global g_histoParam
        global g_titleFont
        option add *FitsHistoParam.font g_titleFont
        option add *FitsHistoParam.labelfont g_titleFont
 
# frame layout
	set fFile [lindex $args 0]
	set _listPreSelectedColNames [lindex $args 1]
	set _colNames $_listPreSelectedColNames

        set fTable [lindex $args 2]

	set xColName "" 
	set yColName  ""
	set cColName  ""
	set rowRange  ""
	set xMin  ""
	set yMin  ""
	set xMax  ""
	set yMax ""  
	set uxMin "" 
	set uyMin ""
	set uxMax "" 
	set uyMax ""     
	set tlxMin "" 
	set tlyMin "" 
	set tlxMax "" 
	set tlyMax "" 
	set xBin "" 
	set yBin ""  

        set g_histoParam ""
#
	# labels
	itk_component add title {
	    message $itk_interior.mssg -text \
                  "Make a 1D or 2D histogram by binning 1 or 2 table columns" \
                  -width 400 -relief ridge
	}

	itk_component add  colLabel {
	    label $itk_interior.coll -text "Column Name"
	}
	itk_component add  rowLabel {
	    label $itk_interior.rowl -text "Row Range"
	}

	itk_component add  tlminLabel {
	    label $itk_interior.tlminl -text "TLMin"
	}
	itk_component add  tlmaxLabel {
	    label $itk_interior.tlmaxl -text "TLMax"
	}
	itk_component add  uminLabel {
	    label $itk_interior.uminl -text "Min"
	}
	itk_component add  umaxLabel {
	    label $itk_interior.umaxl -text "Max"
	}
	itk_component add  minLabel {
	    label $itk_interior.minl -text "Data Min"
	}
	itk_component add  maxLabel {
	    label $itk_interior.maxl -text "Data Max"
	}

	itk_component add  binLabel {
	    label $itk_interior.binl -text "Bin Size"
	}

	itk_component add sep {
	    frame $itk_interior.sep -height 8 -borderwidth 3 -relief ridge
	}
#entry
	itk_component add  xcolEntry {
	     iwidgets::combobox $itk_interior.xcole  -labeltext X -labelpos n \
		 -textvariable [itcl::scope xColName] -width 14 \
		 -selectioncommand [itcl::code $this getXMinMax] 
	}
	eval $itk_component(xcolEntry) insert list 0 $_listPreSelectedColNames

	itk_component add  rowEntry {
	    entry $itk_interior.rowe -width 14 -textvariable [itcl::scope rowRange] 
	}
	bind  $itk_component(rowEntry) <Return> [itcl::code $this getXMinMax]
	bind  $itk_component(rowEntry) <Return> +[itcl::code $this getYMinMax]

	
	itk_component add  tlxminEntry {
	    entry $itk_interior.tlxmine -width 14 -textvariable [itcl::scope tlxMin] \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor \
		-state disabled -borderwidth 1  -relief raised 
	}	
	itk_component add  tlxmaxEntry {
	    entry $itk_interior.tlxmaxe -width 14  \
		-borderwidth 1  -relief raised \
                -textvariable [itcl::scope tlxMax] \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor \
		-state disabled  
	}	

	itk_component add  uxminEntry {
	    entry $itk_interior.uxmine -width 14 -textvariable [itcl::scope uxMin]  
	}	
	itk_component add  uxmaxEntry {
	    entry $itk_interior.uxmaxe -width 14 -textvariable [itcl::scope uxMax]  
	}
	itk_component add  xminEntry {
	    entry $itk_interior.xmine -width 14 \
		-borderwidth 1  -relief raised \
		-textvariable [itcl::scope xMin] -state disabled \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor
	}	
	itk_component add  xmaxEntry {
	    entry $itk_interior.xmaxe -width 14  \
		-borderwidth 1  -relief raised \
		-textvariable [itcl::scope xMax] -state disabled \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor
	}
	
	itk_component add  xbinEntry {
	    entry $itk_interior.xbine -width 14 -textvariable [itcl::scope xBin]  
	}	

######################
	itk_component add  ycolEntry {
            global g_titleFont
	    iwidgets::combobox $itk_interior.ycole  -labeltext Y -labelpos n \
		-textvariable [itcl::scope yColName] -width 14 \
		-selectioncommand [itcl::code $this getYMinMax] \
        } {
          keep -labelfont -textfont
        }

	eval $itk_component(ycolEntry) insert list end $_listPreSelectedColNames

	itk_component add  ccolEntry {
	    iwidgets::combobox $itk_interior.ccole  -labeltext Weight \
		-labelpos n \
		-textvariable [itcl::scope cColName] -width 14 
	}

	eval $itk_component(ccolEntry) insert list end $_listPreSelectedColNames

	itk_component add  tlyminEntry {
	    entry $itk_interior.tlymine -width 14  \
		-borderwidth 1  -relief raised \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor \
		-textvariable [itcl::scope tlyMin] -state disabled  
	}	
	itk_component add  tlymaxEntry {
	    entry $itk_interior.tlymaxe -width 14 \
		-borderwidth 1  -relief raised \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor \
		-textvariable  [itcl::scope tlyMax]   -state disabled  
	}	
	itk_component add  uyminEntry {
	    entry $itk_interior.uymine -width 14 -textvariable [itcl::scope uyMin]  
	}	
	itk_component add  uymaxEntry {
	    entry $itk_interior.uymaxe -width 14 -textvariable [itcl::scope uyMax]  
	}	

	itk_component add  yminEntry {
	    entry $itk_interior.ymine -width 14  \
		-borderwidth 1  -relief raised \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor \
		-textvariable [itcl::scope yMin]   -state disabled  
	}	
	itk_component add  ymaxEntry {
	    entry $itk_interior.ymaxe -width 14  \
		-borderwidth 1  -relief raised \
                -disabledforeground black \
                -disabledbackground $fvPref::globalBgColor \
		-textvariable [itcl::scope yMax]   -state disabled  
	}	
	
	itk_component add  ybinEntry {
	    entry $itk_interior.ybine -width 14 -textvariable [itcl::scope yBin]  
	}	


        itk_component add histselonlyCHK {
                checkbutton $itk_interior.histselonly -text "Use selected rows" \
                -variable [itcl::scope _histselonly ] \
                 -selectcolor $fvPref::checkBBgColor  \
                 -activeforeground black \
                 -activebackground $fvPref::globalBgColor \
                 -state disabled \
                 -command [itcl::code $this getMinMax] 
        } {
                usual
                ignore -selectcolor -activeforeground -activebackground
        }



	itk_component add  Button {
	    iwidgets::buttonbox $itk_interior.bb 
	} 
	$itk_component(Button) add makeclose -text "Make/Close" \
            -font g_titleFont \
	    -command [itcl::code $this _makeHistogram true]
	$itk_component(Button) add make -text "Make" \
            -font g_titleFont \
	    -command [itcl::code $this _makeHistogram false]
	$itk_component(Button) add cancel -text Close \
            -font g_titleFont \
	    -command [itcl::code $this quitCmd]
	$itk_component(Button) add help  -text Help \
            -font g_titleFont \
	    -command "hhelp 2D-Histogram"

	pack $itk_component(Button) -side bottom


	grid configure  $itk_component(title)  -column 0 -row 0 \
	    -columnspan 4 -sticky "new" -ipady 10


	grid configure  $itk_component(colLabel)  -column 0 -row 1 \
	    -sticky "se"
	grid configure  $itk_component(tlminLabel) -column 0 -row 2 -sticky "se"
	grid configure  $itk_component(tlmaxLabel) -column 0 -row 3 -sticky "se"
	grid configure  $itk_component(minLabel) -column 0 -row 4 -sticky "se"
	grid configure  $itk_component(maxLabel) -column 0 -row 5 -sticky "se"
	grid configure  $itk_component(uminLabel) -column 0 -row 6 -sticky "se"
	grid configure  $itk_component(umaxLabel) -column 0 -row 7 -sticky "se"
	grid configure  $itk_component(binLabel) -column 0 -row 8 -sticky "se"
	grid configure  $itk_component(rowLabel) -column 0 -row 9 -sticky "se"
	grid configure  $itk_component(histselonlyCHK) -column 0 -row 10 \
	    -columnspan 4 -sticky "snew"
	grid configure  $itk_component(sep) -column 0 -row 11 \
	    -columnspan 4 -sticky "snew"

	grid configure  $itk_component(xcolEntry) -column 1 -row 1 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(ycolEntry) -column 2 -row 1 \
	    -padx 4 -pady 4  -sticky "snew"
	grid configure  $itk_component(ccolEntry) -column 3 -row 1 \
	    -padx 4 -pady 4  -sticky "snew"
	grid configure  $itk_component(tlxminEntry) -column 1 -row 2 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(tlxmaxEntry)  -column 1 -row 3 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(tlyminEntry) -column 2 -row 2 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(tlymaxEntry)  -column 2 -row 3 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(xminEntry) -column 1 -row 4 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(xmaxEntry)  -column 1 -row 5 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(yminEntry)  -column 2 -row 4 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(ymaxEntry) -column 2 -row 5 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(uxminEntry)  -column 1 -row 6 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(uxmaxEntry) -column 1 -row 7 \
	    -padx 4 -pady 4 -sticky "snew"	
	grid configure  $itk_component(uyminEntry)  -column 2 -row 6 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(uymaxEntry) -column 2 -row 7 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(rowEntry) -column 1 -row 9 \
	     -padx 4 -pady 4  -sticky "snew"

	grid configure  $itk_component(xbinEntry) -column 1 -row 8 \
	    -padx 4 -pady 4 -sticky "snew"
	grid configure  $itk_component(ybinEntry)  -column 2 -row 8 \
	    -padx 4 -pady 4 -sticky "snew"

	grid configure $itk_component(Button) -column 0 -row 12 \
	    -columnspan 4  -sticky "snew"
#
        eval itk_initialize
    }

    destructor  {
    }

}

itcl::body FitsHistoParam::turnsel {flag} {
     if {$flag == 1} {
           $itk_component(histselonlyCHK) configure -state normal
     } else {
           $itk_component(histselonlyCHK) configure -state disabled
     }
}


itcl::body FitsHistoParam::insertDefaultValue {} {
     set token [split $_colNames " "]
     
     eval $itk_component(xcolEntry) selection set [lsearch -exact [string tolower $token] "x"]
     eval $itk_component(ycolEntry) selection set [lsearch -exact [string tolower $token] "y"]
     getXMinMax
     getYMinMax
}


itcl::body FitsHistoParam::getMinMax {} {
     getXMinMax
     getYMinMax
}
 
itcl::body FitsHistoParam::getXMinMax {} {
    if { $xColName == ""} return

    set selrange [$this getSelRange]

    set tlminmax [$fFile getTLMinMax $xColName]
#    set minmax [$fFile getColMinMax $xColName 1 $rowRange]
    set minmax [$fFile getColMinMax $xColName 1 $selrange]

    set xColType [lindex [lindex [$fFile getColInfo \
				      $xColName] 0] 4]
    if { [regexp {[duo]} $xColType] } {
	set colIsInt 1
	set tmpMin [expr int([lindex $minmax 0])]
	set tmpMax [expr int([lindex $minmax 1])]
    } else {
	set colIsInt 0
	set tmpMin [lindex $minmax 0]
	set tmpMax [lindex $minmax 1]
    }

    enableEntry

    set tlxMin [lindex $tlminmax 0]
    set tlxMax [lindex $tlminmax 1]
    set xMin $tmpMin
    set xMax $tmpMax

    if { $tlxMin == "" } {
	set uxMin $xMin
    } else {
	set uxMin $tlxMin
    }
    if { $tlxMax == "" } {
	set uxMax $xMax
    } else {
	set uxMax $tlxMax
    }

    set colnum [$fFile getColNum $xColName]
    if { [catch {set tmpbin [$fFile getKeyword "TDBIN$colnum"]}] } {
       # TDBIN keyword doesn't exist, so calculate one

       # default 2^n binsize

       set diff [expr $uxMax - $uxMin]
       set n [expr floor( log($diff/257.0)/log(2.0) )]
       set xBin [expr pow(2.0,$n)]

       if { $colIsInt } {
          if { $xBin < 1} {
             set xBin 1
          } else {
             set xBin [expr int($xBin)]
          }
       }
    } else {
       set xBin [lindex [lindex $tmpbin 0] 1]
    }

    disableEntry
}


itcl::body FitsHistoParam::getSelRange {} {
    if { $_histselonly == 1 } {
        set numRows [$fTable getnumRows]
        if { $rowRange == "" } {
           set tmpRange "1-$numRows"
        } else {
           set tmpRange $rowRange
        }
        set selrange ""
        set tmplist [split $tmpRange ","]
        set numlist [llength $tmplist]

        for { set k 0 } {$k <$numlist } {incr k} {
            set start_end  [lindex $tmplist $k]
            if { $selrange =="" } {
                append  selrange [$fTable _parseToRowRange $start_end]
            } else {
                append  selrange "," [$fTable _parseToRowRange $start_end]
            }
        }
        if {$selrange == "" } {  
           set selrange "1-1"
        }
      
     
     } else {
       set selrange $rowRange
     }

     return $selrange
}
    


itcl::body FitsHistoParam::getYMinMax {} {
    if { $yColName == ""} return

    set selrange [$this getSelRange]
 

    set tlminmax [$fFile getTLMinMax $yColName]
#    set minmax [$fFile getColMinMax $yColName 1 $rowRange]
    set minmax [$fFile getColMinMax $yColName 1 $selrange]

    set yColType [lindex [lindex [$fFile getColInfo \
				      $yColName] 0] 4]
    if { [regexp {[duo]} $yColType] } {
	set colIsInt 1
	set tmpMin [expr int([lindex $minmax 0])]
	set tmpMax [expr int([lindex $minmax 1])]
    } else {
	set colIsInt 0
	set tmpMin [lindex $minmax 0]
	set tmpMax [lindex $minmax 1]
    }

    enableEntry

    set tlyMin [lindex $tlminmax 0]
    set tlyMax [lindex $tlminmax 1]    
    set yMin $tmpMin
    set yMax $tmpMax

    if { $tlyMin == "" } {
	set uyMin $yMin
    } else {
	set uyMin $tlyMin
    }
    if { $tlyMax == "" } {
	set uyMax $yMax
    } else {
	set uyMax $tlyMax
    }

    set colnum [$fFile getColNum $yColName]
    if { [catch {set tmpbin [$fFile getKeyword "TDBIN$colnum"]}] } {
       # TDBIN keyword doesn't exist, so calculate one

       set diff [expr $uyMax - $uyMin]
       set n [expr floor( log($diff/257.0)/log(2.0) )]
       set yBin [expr pow(2.0,$n)]
       
       if { $colIsInt } {
          if { $yBin < 1} {
             set yBin 1
          } else {
             set yBin [expr int($yBin)]
          }
       }
    } else {
       set yBin [lindex [lindex $tmpbin 0] 1]
    }

    disableEntry
}

itcl::body FitsHistoParam::enableEntry {} {
    $itk_component(tlxminEntry) configure -state normal
    $itk_component(tlyminEntry) configure -state normal
    $itk_component(tlxmaxEntry) configure -state normal
    $itk_component(tlymaxEntry) configure -state normal
    $itk_component(xminEntry) configure -state normal
    $itk_component(yminEntry) configure -state normal
    $itk_component(xmaxEntry) configure -state normal
    $itk_component(ymaxEntry) configure -state normal
}

itcl::body FitsHistoParam::disableEntry {} {
    $itk_component(tlxminEntry) configure -state disabled
    $itk_component(tlyminEntry) configure -state disabled
    $itk_component(tlxmaxEntry) configure -state disabled
    $itk_component(tlymaxEntry) configure -state disabled
    $itk_component(xminEntry) configure -state disabled
    $itk_component(yminEntry) configure -state disabled
    $itk_component(xmaxEntry) configure -state disabled
    $itk_component(ymaxEntry) configure -state disabled
}


itcl::body FitsHistoParam::_makeHistogram { closeFlag } {
    global g_histoParam
    global g_backupDir
    global g_histoFileID

    set g_histoParam ""

    set selrange [$this getSelRange]

    if { $xColName == "" } {
	error "Please select a x column"
    }

    if { $xBin == "" } {
	error "Please provide x bin size"
    }

    if { $cColName != "" } {
       lappend g_histoParam -weight $cColName
    }

    if { $selrange != "" } {
	lappend g_histoParam -rows $selrange
    }
     
    incr g_histoFileID
    set histoFileName [file join ${g_backupDir} histo.tmp$g_histoFileID]
    lappend g_histoParam $histoFileName

    lappend g_histoParam [list $xColName $uxMin $uxMax $xBin]

    if { $yColName != "" } {
	if { $yBin == ""} {
	    error "Please provide y bin size"
	}  
        lappend g_histoParam [list $yColName $uyMin $uyMax $yBin]
    }

    if { $g_histoFileID > 0 } {
       if ![file exists $histoFileName] {
          eval $fFile makeHistogram $g_histoParam
       } else {
       }
       set fftmp [openFitsFile [file join $g_backupDir histo.tmp$g_histoFileID]]
       $fftmp changeFile
       $fftmp plotData 1
    }

    if { $closeFlag == "true" } {
       itcl::delete object $this
    }
}


itcl::body FitsHistoParam::quitCmd {} {
    global g_histoParam
#    global _fth

    set g_histoParam ""
#    set _fth 0
   
    itcl::delete object $this
}
