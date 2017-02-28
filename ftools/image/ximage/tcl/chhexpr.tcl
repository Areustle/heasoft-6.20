namespace eval xan {

proc chhexpr {mapid inexpr} {
#
#  Evaluates an expression with capitalized keywords replaced
#  with their values in the mapid header and then evaluates
#
   regsub -all {[A-Z][A-Z0-9]*} $inexpr \
               {[::chheader mapid=$mapid key=&]} tmpexpr
   return [expr $tmpexpr]
}

} ;# End xan namespace
