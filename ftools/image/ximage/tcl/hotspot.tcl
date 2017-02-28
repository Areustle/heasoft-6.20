#
#  Emulate old HOTSPOT command
#  Runs LABEL for EXOSAT/CMA1 or 2
#
proc hotspot {args} {

   parseparm [parmloc hotspot] $args
   if { $cmdargc != 0 } {
       txwrite " Wrong number of arguments: $cmdargv" 10
       error {}
   }

   set telescop [chheader key=TELESCOP]
   set instrume [chheader key=INSTRUME]
   if { ! ([regexp -nocase {^cma} $instrume] &&
           [regexp -nocase {^exosat$} $telescop]) } {
      error " HOTSPOT is only for the CMA detector"
   }

   if { [regexp -nocase {cma1} $instrume] } {
      set xpix 155
      set ypix 71
   } elseif { [regexp -nocase {cma2} $instrume] } {
      set xpix -11
      set ypix -20
   } else {
      error " Did not match CMA1 or CMA2"
   }
   txwrite " Hot Spot at $xpix,$ypix" 10
#
#  LABEL parameters
#
   lappend labelArgs "xpix=$xpix" "ypix=$ypix"
   if { $parval(color) != "" } { lappend labelArgs "color=$parval(color)" } 
   if { $parval(csize) != "" } { lappend labelArgs "csize=$parval(csize)" } 
   if { $parval(lwidth) != "" } { lappend labelArgs "lwidth=$parval(lwidth)" } 
   if { $parval(font) != "" }  { lappend labelArgs "font=$parval(font)" } 
   if { $parval(symbol) != "" } { lappend labelArgs "symbol=$parval(symbol)" } 
   if { $parval(symcolor) != "" } { 
      lappend labelArgs "symcolor=$parval(symcolor)" 
   } 
   if { $parval(symcsize) != "" } { 
      lappend labelArgs "symcsize=$parval(symcsize)" 
   } 
   if { $parval(symlwidth) != "" } { 
      lappend labelArgs "symlwidth=$parval(symlwidth)" 
   } 
   if { $parval(angle) != "" }  { lappend labelArgs "angle=$parval(angle)" } 
   if { $parval(just) != "" }  { lappend labelArgs "just=$parval(just)" } 
   lappend labelArgs "Hot Spot"
#
#  LABEL command
#
   txwrite "label $labelArgs" 20
   eval label $labelArgs
}
