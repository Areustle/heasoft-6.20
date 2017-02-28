#
#  Use string comparison to check for bad values, which is more
#  forgiving than numerical comparison.
#
proc isastbad {value} {
   global ASTBAD
   if { $value eq $ASTBAD } { return 1 }
   return 0
}
