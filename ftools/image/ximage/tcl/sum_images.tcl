#
#  Emulate SUM_IMAGES command
#  Runs more general MARITH command
#
proc sum_images {args} {

   global curmap savmode savmap

   parseparm [parmloc sum_images] $args

   if { $cmdargc != 0 } {
      txwrite " Wrong number of arguments: $cmdargv" 10
      error {}
   }

   if { !$savmode } {
      txwrite "Save/restore mode is currently off: Use MARITH" 5
      error {}
   }
#
# Build marith command
#
   set marithArgs {}
   if { $parval(ra) != "" } { lappend marithArgs ra=$parval(ra) }
   if { $parval(dec) != "" } { lappend marithArgs dec=$parval(dec) }
   if { $parval(equinox) != "" } { 
      lappend marithArgs equinox=$parval(equinox) 
   }
   lappend marithArgs mapout=$curmap
   lappend marithArgs hdrid=$savmap
   if { [chheader map=$savmap key=cdelt1] > 0 &&
        [chheader map=$savmap key=cdelt2] < 0 } {
      lappend marithArgs crota=-180
   } else {
      lappend marithArgs crota=0
   }
   lappend marithArgs $curmap+$savmap
#
# Run marith command
#
   txwrite "marith $marithArgs" 20
   eval marith $marithArgs
}
