#
#  rotate command implemented through remap command
#
rename rotate orotate
proc rotate2 {args} {

   parseparm [parmloc rotate] $args

   if { $cmdargc != 0 } {
      txwrite " Wrong number of arguments: $cmdargv" 10
      error {}
   }

   if { [chh key=LOADED] == 0 } {
      txwrite "Map not loaded" 10
      error {}
   }

   if { $parval(roll_angle) != "" } {
      set roll_angle $parval(roll_angle) 
   } else {
      txwrite " Rotating image to align with north" 10
      set roll_angle -90.
      txwrite " Default roll angle = $roll_angle" 15
   }

   global curmap
#  set rotangle [expr (-$roll_angle + 270.) - [chh key=crota2]]
   set crota2 [chh key=crota2]
   if [isnull $crota2] { set crota2 0.0 }
   set rotangle [expr ($roll_angle + 90.) - $crota2]
   txwrite "remap rotangle=$rotangle inmap=$curmap outmap=$curmap" 20
   remap rotangle=$rotangle inmap=$curmap outmap=$curmap
}
