itcl::class FitsDelCalculator {
    inherit FitsBaseCalculator

    constructor {theFather theCols} {
        FitsBaseCalculator::constructor $theFather $theCols
    } {
        $itk_component(calculate) configure -text "Delete" \
            -command [itcl::code $this deleteCmd]
        $itk_component(help) configure -command {hhelp deleteRows}
    }
    destructor {}

    private method setResult {}
    private method deleteCmd {}
}


itcl::body FitsDelCalculator::deleteCmd {  } {
   set rslt [promptMsg \
	 "The table rows will be permanently deleted.\nAre you sure?" \
	 return Yes No]
   if { $rslt=="CANCEL" || $rslt=="BREAK" } return

   if { [regexp {[^0-9, -]} $resultFormula($this)] } {
      setWatchCursor [namespace tail $this] \
	    [itcl::code $_father delRowsWithCondition $resultFormula($this)]
   } else {
#      setWatchCursor [namespace tail $this] \
#	    [itcl::code $_father delRowsFromList $resultFormula($this)]
      $_father delRowsFromList $resultFormula($this)
   }
}


itcl::body FitsDelCalculator::setResult {} {
    
    itk_component add resultCol {
	label  $itk_interior.resultCol -text "Rows to delete or condition:"
    }
}
