option add *FitsFileselectionbox.font "Helvetica 12 bold"

itcl::class FitsBaseCalculator {
    inherit itk::Toplevel

    constructor {theFather theCols} {}
    destructor  {}

    protected common resultFormula 
    protected common resultColumn 
    protected common resultForm
    protected variable numColBut
    protected variable numPage
    protected variable curPage 0
    protected variable _father
    protected variable _colNameList

#   Ziqin Pan Feb 18, 2004
#   Add variables to support row selection
    protected variable _calcselonly
    protected variable _calcselor
    protected variable _calcseland
    protected method addOrCheckButton {}
#

    protected method addBut {element isCol} 
    protected method butPage {page}
    protected method _pageUp {}
    protected method _pageDown {}
    protected method setResult {}
    protected method validation {}
    protected method init {}
    protected method _calculateCmd {}
    public method refresh {}
}


itcl::body FitsBaseCalculator::constructor {theFather theCols} {
    set _colNameList $theCols
    set _father $theFather
    init
    $_father addChild $this
}

itcl::body FitsBaseCalculator::destructor {} {
   $_father freeChild $this
}

itcl::body FitsBaseCalculator::init {} {
     global g_titleFont

     option add *FitsBaseCalculator.font g_titleFont

# frame layout
    itk_component add  outputFrame {
	frame $itk_interior.f1
    }
    itk_component add  sep1 {
	frame $itk_interior.s1 -height 4 -relief raised -bd 2
    }
    itk_component add  uslButFrame {
	frame $itk_interior.f2
    }
    itk_component add  sep2 {
	frame $itk_interior.s2 -width 4 -relief raised -bd 2
     }
    itk_component add  sep3 {
	frame $itk_interior.s3 -width 4 -relief raised -bd 2
     }
    itk_component add  colButFrame {
	frame $itk_interior.f3 
    }
    itk_component add  pageFrame {
	frame $itk_interior.f4 
    }
    itk_component add  pageButFrame {
	frame $itk_interior.f5 
    }
    
    
    grid configure $itk_component(outputFrame) -column 0 -row 0 \
	-columnspan 5 -sticky "snew"
    grid configure $itk_component(sep1)        -column 0 -row 1 \
	-columnspan 5 -sticky "snew" 
    grid configure $itk_component(uslButFrame) -column 0 -row 2 \
	-sticky "snew" -rowspan 2
    grid configure $itk_component(sep2)       -column 1 -row 2 \
	-sticky "snw" -rowspan 2
    grid configure $itk_component(colButFrame) -column 2 -row 2 \
	-sticky "snew" 
    grid configure $itk_component(pageButFrame) -column 2 -row 3 \
	-sticky "swe"
    grid configure $itk_component(sep3)        -column 3 -row 2 \
	-sticky "sne" -rowspan 2
    grid configure $itk_component(pageFrame) -column 4 -row 2 \
	-sticky "snew" -rowspan 2

    # the entry field
    set itk_interior $itk_component(outputFrame)

    # for different purposes(Add column and Delete rows) setResult is different
    setResult

    itk_component add eqSign {
	label $itk_interior.eqSign -text "=" -font g_titleFont
    }
    itk_component add result {
	entry $itk_interior.result -textvariable [itcl::scope resultFormula($this)] \
	    -width 50  -font g_titleFont
    }
    grid configure $itk_component(resultCol) -in $itk_component(outputFrame) \
	-column 0 -row 0 
    grid configure $itk_component(eqSign)    -in $itk_component(outputFrame) \
	-column 1 -row 0 
    grid configure $itk_component(result)    -in $itk_component(outputFrame) \
	-column 2 -row 0 -sticky "snew"
    # the button field
    set itk_interior $itk_component(uslButFrame)
    set uslButList [list sin  cos  tan "log e" exp    \
                         asin acos atan log10  sqrt   \
			 "<"  "==" ">"  rgn    gti    \
			 &&   ||   "("  ")"    "^"    \
                         PI   7    8    9      "/"    \
			 e    4    5    6      "*"    \
			 EE   1    2    3      "-"    \
			 CL   0    "."  #ROW   "+"    ]
    set i 0
    foreach elem $uslButList {
	itk_component add uslBut$i {
	    button $itk_interior.b$i -text $elem -padx 3 -pady 2 \
		-command [itcl::code $this addBut $elem 0] -font g_titleFont
	}
	set col [expr $i%5]
	set row [expr $i/5]
	grid configure $itk_component(uslBut$i) -in $itk_component(uslButFrame) \
	    -column $col -row $row -sticky "snew"
	incr i
    } 
    
    incr row
    itk_component add moreHelp {
	label $itk_interior.bhelp -text "(see Help for more functions)"  -font g_titleFont
    }
    grid configure $itk_component(moreHelp) -in $itk_component(uslButFrame) \
	    -column 0 -row $row -sticky "snew" -columnspan 5

   # the column button field
    set itk_interior $itk_component(colButFrame)

    itk_component add colLable {
	label $itk_interior.bl -text "Columns"  -font g_titleFont
    }
    grid configure $itk_component(colLable) -in $itk_component(colButFrame) \
	    -column 0 -columnspan 6  -row 0 -sticky "snew"

    set i 0

    foreach elem $_colNameList {
	itk_component add colBut$i {
	    button $itk_interior.b$i -text $elem -padx 3 -pady 2 \
		-command [itcl::code $this addBut $elem 1] -width 10 -font g_titleFont
	}
	incr i
    } 



    set numColBut $i
    set numPage [expr 1+$i/24]

    for {set j $numColBut} { $j < [expr $numPage*24]} {incr j} {
	itk_component add colBut$j {
	    button $itk_interior.b$j -text ""  -padx 3 -pady 2 \
		-width 10 -state disabled
	}	
    }



    butPage $curPage

    set itk_interior $itk_component(pageButFrame)
    itk_component add pageUp {
	button $itk_interior.up -text "Up" -command [itcl::code $this _pageUp]
    }
    itk_component add pageDown {
	button $itk_interior.down -text "Down" -command [itcl::code $this _pageDown]
    }
    itk_component add pageSep {
	frame $itk_interior.sep -height 4 -borderwidth 2 -relief \
	raised 
    }

# Ziqin Pan, Feb 18, 2004
# add to support row selection
    addOrCheckButton
#

    itk_component add selonlyCheckButton {
        checkbutton $itk_interior.selonly -text "Apply only to selected rows" \
                -variable [itcl::scope _calcselonly ]

    }

    pack $itk_component(pageSep) -in $itk_component(pageButFrame) \
	-fill x -side top
    pack $itk_component(orCheckButton) -in $itk_component(pageButFrame) \
	-fill x -side top
    pack $itk_component(selonlyCheckButton) -in $itk_component(pageButFrame) \
	-fill x -side top
    pack $itk_component(pageUp) -in $itk_component(pageButFrame) \
	-side left
    pack $itk_component(pageDown) -in $itk_component(pageButFrame) \
	-side right

    
    if { $numPage == 1} {
	$itk_component(pageUp) configure -state disabled
	$itk_component(pageDown) configure -state disabled
    }

    # 
    set itk_interior $itk_component(pageFrame)

    itk_component add calculate {
	button $itk_interior.calculate -text Calculate \
            -font g_titleFont \
	    -command  [itcl::code $this _calculateCmd] 
    }
    bind $itk_component(result) <Return> \
          [itcl::code $itk_component(calculate) invoke]

    itk_component add cancel {
	button $itk_interior.cancel -text Close -command "itcl::delete object $this" -font g_titleFont
    }
    itk_component add help {
	button $itk_interior.help -text Help -command {hhelp calculator} -font g_titleFont
    }

    grid configure $itk_component(calculate) -in $itk_component(pageFrame) \
	-column 0 -row 2 -sticky "snew"
    grid configure $itk_component(cancel) -in $itk_component(pageFrame) \
	-column 0 -row 3 -sticky "snew"
    grid configure $itk_component(help) -in $itk_component(pageFrame) \
	-column 0 -row 4 -sticky "snew"

# 

    eval itk_initialize
    focus $itk_component(result)
}

itcl::body FitsBaseCalculator::butPage {page} {

    for {set j 0} {$j < 24} {incr j} {
 	set col [expr $j/6]
	set row [expr $j%6 +1]
	set index [expr $page*24+$j]
	grid configure $itk_component(colBut$index) \
	      -in $itk_component(colButFrame) \
	      -column $col -row $row -sticky "snew"
    } 
}

itcl::body FitsBaseCalculator::addBut {element isCol} {

   if { $isCol } {
      if { [string is alnum $element] == 0 && [string is alpha $element] == 0 } {
         if { [regexp -nocase {[\$\+\-\*\/\.\<\>\%\^]} $element] == 1 } {
            set element "\$$element\$"
         }
      }
   } else {
      switch -exact -- $element {
	 sin  -
	 cos  -
	 tan  -
	 asin -
	 acos -
	 atan -
	 sqrt -
	 exp  -
	 log10  { set element "${element}(x)" ; set backup {2 1};   }
	"log e" { set element "log(x)"        ; set backup {2 1};   }
	 PI   { set element "#pi "                                  }
	 e    { set element "#e "                                   }
	 EE   { set element "E"                                     }
	 gti  { set element "gtifilter()"  ; set backup {1 0};      }
	 rgn  { set element "regfilter(\"rgnFile\")"  ; set backup {9 7};  }
	 CL   { set element ""; set [itcl::scope resultFormula($this)] "" }
      }
   }

   if { [$itk_component(result) selection present] } {
      $itk_component(result) delete sel.first sel.last
   }
   $itk_component(result) insert insert $element
   if { [info exists backup] } {
      set loc [$itk_component(result) index insert]
      set start [expr $loc-[lindex $backup 0] ]
      $itk_component(result) icursor $start
      $itk_component(result) selection range \
	    $start [expr $start+[lindex $backup 1]]
   }
}

itcl::body FitsBaseCalculator::_pageDown {} {
    if { $numPage == 1 } return
    if { $curPage == [expr $numPage-1]} return

    for {set j 0} {$j < 24} {incr j} {
	set index [expr $curPage*24+$j]
	grid forget $itk_component(colBut$index)	    
    }

    incr curPage
    butPage $curPage
}

itcl::body FitsBaseCalculator::_pageUp {} {
    if { $numPage == 1 } return
    if { $curPage == 0 } return

    for {set j 0} {$j < 24} {incr j} {
	set index [expr $curPage*24+$j]
	grid forget $itk_component(colBut$index)	    
    }    


    incr curPage -1
    butPage $curPage

}

itcl::body FitsBaseCalculator::addOrCheckButton {} {
    itk_component add orCheckButton {
      label $itk_interior.orCheckButton -text " "
    }
}

itcl::body FitsBaseCalculator::setResult {} {
    itk_component add resultCol {
	iwidgets::combobox  $itk_interior.resultCol -textvariable \
	    [itcl::scope resultColumn($this)] -completion 0 \
            -selectioncommand [::itcl::code $this validation]
    }
    eval $itk_component(resultCol) insert list end $_colNameList
}


itcl::body FitsBaseCalculator::validation {} {
    if { [string is alnum $resultColumn($this)] == 0 && \
         [string is alpha $resultColumn($this)] == 0 } {
       if { [string first "$" $resultColumn($this)] >= 0 ||
            [string first "-" $resultColumn($this)] >= 0 ||
            [string first "%" $resultColumn($this)] >= 0 ||
            [string first "^" $resultColumn($this)] >= 0 ||
            [string first "=" $resultColumn($this)] >= 0 ||
            [string first "+" $resultColumn($this)] >= 0 ||
            [string first "*" $resultColumn($this)] >= 0 ||
            [string first "^" $resultColumn($this)] >= 0 ||
            [string first "<" $resultColumn($this)] >= 0 ||
            [string first ">" $resultColumn($this)] >= 0 ||
            [string first "/" $resultColumn($this)] >= 0 } {
          #set resultColumn($this) "\$$resultColumn($this)\$"
       }
    }
}

itcl::body FitsBaseCalculator::refresh {} {

   set _colNameList [$_father getCalcCols]
   set newPages [expr 1+([llength $_colNameList]-1)/24]

   #  Update the number of buttons needed

   set itk_interior $itk_component(colButFrame)
   if { $numPage < $newPages } {
      # Have to create more pages
      for { set j [expr $numPage*24] } { $j < [expr $newPages*24] } {incr j} {
	 itk_component add colBut$j {
	    button $itk_interior.b$j -text ""  -padx 3 -pady 2 \
		  -width 10 -state disabled
	 }
      }
   } elseif { $numPage > $newPages } {
      # Delete excess buttons
      for { set j [expr $newPages*24] } { $j < [expr $numPage*24] } {incr j} {
	 destroy $itk_component(colBut$j)
      }
   }
   set numPage $newPages

   #  Update buttons to reflect current column names

   set i 0
   foreach elem $_colNameList {
      $itk_component(colBut$i) configure -text $elem -padx 3 -pady 2 \
	    -command [itcl::code $this addBut $elem 1] -width 10 \
	       -state normal
      incr i
   } 
   set numColBut $i

   while { $i < [expr $numPage*24]} {
      $itk_component(colBut$i) configure -text "" -padx 3 -pady 2 \
	    -width 10 -state disabled
      incr i
   }

   if { $numPage == 1} {
      $itk_component(pageUp) configure -state disabled
      $itk_component(pageDown) configure -state disabled
   } else {
      $itk_component(pageUp) configure -state normal
      $itk_component(pageDown) configure -state normal
   }

   if { $curPage >= $numPage } {
      # No need to "forget" buttons... they should have been deleted
      set curPage [expr $numPage-1]
      butPage $curPage
   }
}
