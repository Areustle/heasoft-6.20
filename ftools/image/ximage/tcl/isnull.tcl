#
#  Use string comparison to check for null values, which is more
#  forgiving than numerical comparison.
#
proc isnull {value} {
   global NULL
   if { $value eq "NULL" } { return 1 }
   if { $value eq $NULL } { return 1 }
   return 0
}
