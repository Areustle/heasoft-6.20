#
#  resize command implemented through remap command
#
rename resize oresize
proc resize2 {args} {

   parseparm [parmloc resize] $args

   if { $cmdargc != 0 } {
      txwrite " Wrong number of arguments: $cmdargv" 10
      error {}
   }

   if { [chh key=LOADED] == 0 } {
      txwrite "Map not loaded" 10
      error {}
   }

   if { $parval(pixel_size) == "" } {
      txwrite " Error: pixel size not set" 10
      error {}
   } 

   set npixsize $parval(pixel_size)

   set cdelt1 [chh key=CDELT1]
   set cdelt2 [chh key=CDELT2]
   set ctype [chh key=CTYPE1]
   if [string match {*TAN} $ctype] {
      txwrite " Assuming entered pixel in arcsecs" 10
      set opixsize [expr abs($cdelt1*3600.)]
   } else {
      txwrite " Assuming entered pixel in same units as CDELT" 10
      set opixsize [expr abs($cdelt1)]
   }
   if { $opixsize == 0 } { set opixsize 1. }
   txwrite " Current size: $opixsize" 20

   set zoom [expr $npixsize/$opixsize]

   global curmap
   remap rebin=$zoom inmap=$curmap outmap=$curmap
}
