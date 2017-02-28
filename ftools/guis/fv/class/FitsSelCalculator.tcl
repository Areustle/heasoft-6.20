#--------------------------------------------------------
# Modification History:
#
# Created by Ziqin Pan, Feb 18, 2004
# to support selection of row from expression
#
#--------------------------------------------------------

itcl::class FitsSelCalculator {
    inherit FitsBaseCalculator

    constructor {theFather theCols fRow nRows} {
        set fRow_ $fRow
        set nRows_ $nRows
        FitsBaseCalculator::constructor $theFather $theCols
    } {
        $itk_component(calculate) configure -text "Select" \
            -command [itcl::code $this selectCmd]
        $itk_component(help) configure -command {hhelp selectRows}
        $itk_component(selonlyCheckButton) configure -text "AND with previous selected rows"  \
              -variable [itcl::scope _calcseland] \
              -command [itcl::code $this ORoff]


    }
    destructor {}

    private variable fRow_
    private variable nRows_

    private method setResult {}
    private method addOrCheckButton {}
    private method selectCmd {}
    private method ANDoff {}
    private method ORoff {}
    
}

itcl::body FitsSelCalculator::ANDoff {  } {

    if { $_calcselor == 1} {
          $itk_component(selonlyCheckButton) deselect
    }
}
itcl::body FitsSelCalculator::ORoff {  } {

    if { $_calcseland == 1} {
          $itk_component(orCheckButton) deselect
    }
}
         


itcl::body FitsSelCalculator::selectCmd {  } {

   if { [regexp {[^0-9, -]} $resultFormula($this)] } {
      setWatchCursor [namespace tail $this] \
	    [itcl::code $_father selRowsWithCondition $resultFormula($this) $fRow_ $nRows_ $_calcseland $_calcselor]
   } else {
      $_father selRowsFromList $resultFormula($this) $_calcseland $_calcselor
   }
}

itcl::body FitsSelCalculator::addOrCheckButton {} {
    
    itk_component add orCheckButton {
	checkbutton  $itk_interior.orCheckButton -text "OR with previous selected rows  " \
              -variable [itcl::scope _calcselor] \
              -command [itcl::code $this ANDoff]
              
    }
}

itcl::body FitsSelCalculator::setResult {} {
    
    itk_component add resultCol {
	label  $itk_interior.resultCol -text "Rows to select or condition:"
    }
}
