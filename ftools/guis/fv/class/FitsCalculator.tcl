itcl::class FitsCalculator {
    inherit FitsBaseCalculator

    constructor {theFather theCols} {
        FitsBaseCalculator::constructor $theFather $theCols
    } {}
    destructor  {}

    private method _calculateCmd {}
    private method setResult {}
    private method validation {}
    private method displayMsg { msg }
}

itcl::body FitsCalculator::_calculateCmd {} {
    global donotDisplayFlag

    if { ($resultColumn($this) == "") || ($resultFormula($this) == "") } {
	error "You need to give me both the result column name \n
and the formula to calculate"
        return 
    }

# try to tolerate = in the formula, just substitute with  ==  before
#    pass it to fitsTcl

    set tmp $resultFormula($this)

    # to see if the result is in an existing column 
    set cform "default"
    if ![regexp "^(.+)\\\((.+)\\\)$" $resultColumn($this) cdmy cname cform] {
       set cname $resultColumn($this)
    }

    set idx [lsearch -exact $_colNameList $cname]

    if { $idx >= 0 } {
       # result been put back to an existing column, check its type
       set cform [lindex [lindex [$_father getColInfo $cname] 0] 1]
    } 

    if { $cform != "default" } {
       if [regexp K $cform] {
          # this will be removed when cfitsio fully supports 64 bits integer arithmetic
          if { ![info exist donotDisplayFlag] || ( [info exist donotDisplayFlag] && $donotDisplayFlag == "no") } {
             displayMsg "Arithmetic function on 64 bits integer is not fully supported yet\nfor value that is larger than 10^15."
             tkwait window .thismsg
          }
       }
    }

    regsub -all {([^!<>=])=([^<>=])} $tmp {\1==\2} tmp

    set [itcl::scope resultFormula($this)] $tmp

    if { [regexp "^(.+)\\\((.+)\\\)$" $resultColumn($this) dmy name form] } {
       set [itcl::scope resultColumn($this)] $name
       set [itcl::scope resultForm($this)] $form
    } else {
       set [itcl::scope resultForm($this)] "default"
    }

# pass the formula back to _father object, and continue
    $_father calculateCols $resultColumn($this) $resultForm($this) \
	 $resultFormula($this) $_calcselonly
#    catch {itcl::delete object $this}
}

  
itcl::body FitsCalculator::setResult {} {
    itk_component add resultCol {
	iwidgets::combobox  $itk_interior.resultCol -textvariable \
	    [itcl::scope resultColumn($this)] -completion 0 \
            -selectioncommand [::itcl::code $this validation]
    }
    eval $itk_component(resultCol) insert list end $_colNameList
}

itcl::body FitsCalculator::validation {} {
    if { [string is alnum $resultColumn($this)] == 0 && \
         [string is alpha $resultColumn($this)] == 0 } {
       #set resultColumn($this) "\$$resultColumn($this)\$"
    }
}

itcl::body FitsCalculator::displayMsg { msg } {
    global donotDisplayFlag


    if [winfo exists .thismsg] {
       destroy .thismsg
    }

    toplevel .thismsg
    set top .thismsg
    wm geom .thismsg +[expr [winfo screenwidth .] / 3]+[expr [winfo screenheight .] / 2]
    wm title .thismsg "Warning"

    message $top.msg -text "$msg\n" -aspect 15000

    set lineCnt [expr [llength [split $msg \n]] + 1]
    checkbutton $top.display -text "Do not show this message again" -variable donotDisplayFlag \
                -onvalue yes -offvalue no
    button $top.done -text "done" -command { destroy .thismsg }   

    grid $top.msg -column 0 -row 0 -columnspan 7 -rowspan $lineCnt
    grid $top.display -column 0 -row [expr $lineCnt + 1] -columnspan 7
    grid $top.done -column 3 -row [expr $lineCnt + 2]
}

