#
#  marith command implemented through remap/mop commands
#
rename marith omarith
proc marith2 {args} {

   parseparm [parmloc marith] $args

   set mexpr $parval(expr)
   if { $mexpr != "" && $cmdargc != 0 } {
      txwrite " Wrong number of arguments: $cmdargv" 5
      error {}
   }
   if { $mexpr == "" } { 
      if { $cmdargc == 0 } {
         txwrite " No expression given" 5
         error {}
      }
      set mexpr $cmdargv 
   } 
   if { $parval(size) != "" } {
      set szx $parval(size)
      set szy $parval(size)
   } else { 
      set szx $parval(szx)
      set szy $parval(szy)
   }

   global curmap
   set mapout $curmap
   if { $parval(mapout) != "" } { set mapout $parval(mapout) }
   set hdrid $parval(hdrid)
   set crota $parval(crota)
   if [isnull $crota] { set crota "" }
   set ra $parval(ra)
   set dec $parval(dec)
   set equinox $parval(equinox)

   set usercoord 0
   if { $hdrid != "" } { set usercoord 1 }
   if { $ra != "" && $dec != "" } { set usercoord 1 }
   if { $crota != "" } { set usercoord 1 }

   set op ""
   set arg1 ""
   set arg2 ""
   if [regexp -nocase \
{^([+-]?(?:[0-9]*\.?[0-9]+|[0-9]+\.?[0-9]*)(?:[eE][+-]?[0-9]+)?|[a-z0-9]+)\s*([+-/*])\s*([+-]?(?:[0-9]*\.?[0-9]+|[0-9]+\.?[0-9]*)(?:[eE][+-]?[0-9]+)?|[a-z0-9]+)$} \
        $mexpr match arg1 opsym arg2] {
#
#  Regexp matches expression of form 
#  number or map name [+-/*] number or map name
#
      if { $opsym == "+" } { set op add }
      if { $opsym == "-" } { set op sub }
      if { $opsym == "*" } { set op mult }
      if { $opsym == "/" } { set op div }
   } elseif [regexp -nocase {^(int|float)\s*\(\s*([a-z0-9]+)\s*\)$} \
             $mexpr match op arg1] {
#
#  Regexp matches expression of form 
#  int(map name) or float(map name)
#
#      Do nothing...

   } else {
      txwrite " Invalid expression, use: arg +-*/ arg or int(map) or float(map) where \"arg\" can be a map or a constant" 5
      error {}
   }
#
#  If both arguments are maps, remap to match image grids
#
   set isconst1 [regexp {^[+-]?(?:[0-9]*\.?[0-9]+|[0-9]+\.?[0-9]*)(?:[eE][+-]?[0-9]+)?$} $arg1]
   set isconst2 [regexp {^[+-]?(?:[0-9]*\.?[0-9]+|[0-9]+\.?[0-9]*)(?:[eE][+-]?[0-9]+)?$} $arg2]

   if { $isconst1 && $isconst2 } {
      txwrite " Must give at least one map in expression" 5
      error {}
   }
#
#  Remap to final coordinates 
#
   set tmp1 ""
   set tmp2 ""
   set remap_failure 0
#
   set remapcmd "remap"
   if { $hdrid != "" }   { lappend remapcmd "coorid=$hdrid" }
   if { $ra != "" }      { lappend remapcmd "ra=$ra" }
   if { $dec != "" }     { lappend remapcmd "dec=$dec" }
   if { $equinox != "" } { lappend remapcmd "equinox=$equinox" }
   if { $szx != "" }     { lappend remapcmd "szx=$szx" }
   if { $szy != "" }     { lappend remapcmd "szy=$szy" }

   txwrite "Case: $arg1 $op $arg2" 20
   if { $arg2 != "" && !$isconst1 && !$isconst2 } {
      #  Two map case

      set cmd $remapcmd
      if { $crota != "" } {
         set ocrota [chh map=$arg1 key=crota2]
         if [isnull $ocrota] { set ocrota 0.0 }
         set diff [expr $crota-$ocrota]
         if { $diff != 0 } {
            lappend cmd "rot=[expr $crota-$ocrota]"
         }
      }
      if { [llength $cmd] == 1 } { 
         set hdrid $arg1
         lappend cmd "coorid=$hdrid"
         lappend remapcmd "coorid=$hdrid"
      }
      # Don't bother remapping if output same coords
      if { ![map isequal $arg1 $hdrid] } {
         set tmp1 [map gettmp]
         lappend cmd in=$arg1 out=$tmp1
         txwrite " Processing $arg1 image" 10
         txwrite $cmd 20
         if [catch {eval $cmd}] {
            set remap_failure 1
            set tmp1 ""
         } else {
            set arg1 $tmp1
         }
      }

      set cmd $remapcmd
      if { $crota != "" } {
         set ocrota [chh map=$arg2 key=crota2]
         if [isnull $ocrota] { set ocrota 0.0 }
         set diff [expr $crota-$ocrota]
         if { $diff != 0 } {
            lappend cmd "rot=[expr $crota-$ocrota]"
         }
      }
      # Don't bother remapping if output same coords
      if { ![map isequal $arg2 $hdrid] } {
         set tmp2 [map gettmp]
         lappend cmd in=$arg2 out=$tmp2
         txwrite " Processing $arg2 image" 10
         txwrite $cmd 20
         if [catch {eval $cmd}] {
            set remap_failure 1
            set tmp2 ""
         } else {
            set arg2 $tmp2
         }
      }

   } elseif { $usercoord } {
      #  One map + user-specified coord case

      set cmd $remapcmd
      if { !$isconst1 } {
         if { $crota != "" } {
            set ocrota [chh map=$arg1 key=crota2]
            if [isnull $ocrota] { set ocrota 0.0 }
            set diff [expr $crota-$ocrota]
            if { $diff != 0 } {
               lappend cmd "rot=[expr $crota-$ocrota]"
            }
         }
         set tmp1 [map gettmp]
         lappend cmd in=$arg1 out=$tmp1
         txwrite " Processing $arg1 image" 10
         txwrite $cmd 20
         eval $cmd
         set arg1 $tmp1
      } else {
         if { $crota != "" } {
            set ocrota [chh map=$arg2 key=crota2]
            if [isnull $ocrota] { set ocrota 0.0 }
            set diff [expr $crota-$ocrota]
            if { $diff != 0 } {
               lappend cmd "rot=[expr $crota-$ocrota]"
            }
         }
         set tmp2 [map gettmp]
         lappend cmd in=$arg2 out=$tmp2
         txwrite " Processing $arg2 image" 10
         txwrite $cmd 20
         eval $cmd
         set arg2 $tmp2
      }

   } 
   #  else, one map no remap case
#
#  Perform operation
#
   if { $remap_failure } {
      txwrite " Could not align image due to missing sky coordinates" 10
      txwrite " Defaulting to simple map arithmetic (moper command)" 10
   }
   set mopcmd "mop"
   if { $hdrid != "" } { lappend mopcmd "hdrid=$hdrid" }
   lappend mopcmd "mapout=$mapout"
   lappend mopcmd $op 
   lappend mopcmd $arg1
   lappend mopcmd $arg2
   txwrite $mopcmd 20
   eval $mopcmd

   if { $tmp1 != "" } { map free $tmp1 }
   if { $tmp2 != "" } { map free $tmp2 }

}
