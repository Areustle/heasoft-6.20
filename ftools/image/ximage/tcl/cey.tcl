#
#  Change equinox year
#  Saved in 'default' array
#
proc cey {args} {

   global default

   if { [llength $args] > 1 } {
      txwrite " Usage: cey <equinox year>" 10
      error {}
   }

   if { [llength $args] == 1 } {
      set default(equinox) [lindex $args 0]
      if { $default(equinox) < 1900 ||
           $default(equinox) > 2050 } {
         txwrite " Allowed values are in the range of 1900-2050" 5
         txwrite " Equinox set to 2000" 5
         set default(equinox) 2000
      }
   } else {
      return $default(equinox)
   }
}
