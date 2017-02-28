proc listclean {str} {
#
#  Used primarily on parvals to allow entry of lists as standard Tcl
#  lists or as comma-delimited strings
#
#  Recommended usage: set x [listclean $x]
#
   if { [llength $str] == 1 } {
      return [split $str {,}]
   } else {
      return $str
   }

}
