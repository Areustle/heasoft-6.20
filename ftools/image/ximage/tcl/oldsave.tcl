#
#  Emulate old save, restore behavior with slots 1 and 2
#
proc save_image {} {
   global savmode savmap curmap

   if { !$savmode } {
      txwrite "Save/restore mode is currently off" 5
      error {}
   }
   return [map copy $curmap $savmap]
}

proc restore_image {} {
   global savmode savmap curmap

   if { !$savmode } {
      txwrite "Save/restore mode is currently off" 5
      error {}
   }
   return [map copy $savmap $curmap]
}

proc free_saved {} {
   global savmode savmap

   if { !$savmode } {
      txwrite "Save/restore mode is currently off" 5
      error {}
   }
   return [map free $savmap]
}
