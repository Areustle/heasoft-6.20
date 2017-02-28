#
#  Emulate old DISPLAY command
#  Runs MAP, LEVELS, VIEWPORT, PIMAGE depending on arguments
#
proc display {args} {

   global curmap savmode savmap maxmaps

   parseparm [parmloc display] $args

   if { $cmdargc != 0 } {
       txwrite " Wrong number of arguments: $cmdargv" 10
       error {}
   }
#
#  Set buffer map to last map that isn't saved map
#
   set i $maxmaps
   set found 0
   set bufmap ""
   while { $i > 0 && !$found } {
      if { !($savmode && [regexp -nocase "^map$i\$" $savmap]) } {
         set found 1
         set bufmap map$i
      }
      incr i -1
   }
   set mapin $parval(mapid)
   set lastmap $curmap
   if { $mapin != "" } { map set $mapin }
#
# Map command
#
   if { $parval(saved_image) } { 
      if { !$savmode } {
         error { Save/restore mode not enabled }
      }
      map set $savmap
   } elseif { $parval(div_saved) } { 
      if { $mapin != "" } {
         txwrite " DIV_SAVED and MAPID are incompatible" 5
         txwrite " Use MARITH, then DISPLAY result" 5
         map set $lastmap
         error {}
      }
      if { !$savmode } {
         error { Save/restore mode not enabled: Use MARITH }
      }
      if { $bufmap == "" } {
         error { Failed to set buffer map }
      } else {
         txwrite " Using $bufmap as buffer" 10
      }
      map set $bufmap
      txwrite "marith $savmap/$lastmap" 20
      if [catch "marith $savmap/$lastmap" result] {
         map set $lastmap
         error {}
      }
   } elseif { $parval(div_map) } { 
      if { $mapin != "" } {
         txwrite " DIV_MAP and MAPID are incompatible" 5
         txwrite " Use MARITH, then DISPLAY result" 5
         map set $lastmap
         error {}
      }
      if { !$savmode } {
         error { Save/restore mode not enabled: Use MARITH }
      }
      if { $bufmap == "" } {
         error { Failed to set buffer map }
      } else {
         txwrite " Using $bufmap as buffer" 10
      }
      map set $bufmap
      txwrite "marith $lastmap/$savmap" 20
      if [catch "marith $lastmap/$savmap" result] {
         map set $lastmap
         error {}
      }
   } elseif { $parval(sub_map) } { 
      if { $mapin != "" } {
         txwrite " SUB_MAP and MAPID are incompatible" 5
         txwrite " Use MARITH, then DISPLAY result" 5
         map set $lastmap
         error {}
      }
      if { !$savmode } {
         error { Save/restore mode not enabled: Use MARITH }
      }
      if { $bufmap == "" } {
         error { Failed to set buffer map }
      } else {
         txwrite " Using $bufmap as buffer" 10
      }
      map set $bufmap
      txwrite "marith $lastmap-$savmap" 20
      if [catch "marith $lastmap-$savmap" result] {
         map set $lastmap
         error {}
      }
   } elseif { $parval(exposure_map) } {
      set exmapid [chheader key=EXMAPID]
      if { $exmapid == "" } {
         txwrite " No exposure map" 10
         error {}
      }
      map set $exmapid
   } elseif { $parval(correct_exposure) } {
      if { $bufmap == "" } {
         error { Failed to set buffer map }
      } else {
         txwrite " Using $bufmap as buffer" 10
      }
      set exposure [chheader key=EXPOSURE]
      set exmapid [chheader key=EXMAPID]
      if { $exmapid == "" } {
         txwrite " No exposure map" 10
         error {}
      }
      txwrite " moper map=$bufmap div $exposure $exmapid" 10
      txwrite " moper map=$bufmap mult $bufmap $curmap" 10
      if [catch {
        moper map=$bufmap div $exposure $exmapid
        moper map=$bufmap mult $bufmap $curmap
      } result ] {
         error {}
      }
      map set $bufmap
   }
#
# Levels command
#
   set levmode histo
   if { $parval(histo) }    { set levmode histo }
   if { $parval(linear) }   { set levmode linear }
   if { $parval(log) }      { set levmode log }
   if { $parval(sqrt) }     { set levmode sqrt }

   set levelsArgs $levmode
   if { $parval(minlevel) != "" } { 
      append levelsArgs " minlevel=$parval(minlevel)" 
   }
   if { $parval(maxlevel) != "" } { 
      append levelsArgs " maxlevel=$parval(maxlevel)" 
   }
#
# Viewport command
#
   set viewportArgs ""
   if { $parval(v1) != "" && $parval(v2) != "" &&
        $parval(v3) != "" && $parval(v4) != "" } {
      set viewportArgs "v1=$parval(v1) v2=$parval(v2) \
                        v3=$parval(v3) v4=$parval(v4)"
   }

#  Left/right mode is in effect if pgtk::lfrt is defined
   if { $parval(left) || $parval(right) } { 
      if { $parval(left) } { set viewportArgs "number=1 1lef1rig" }
      if { $parval(right) } { set viewportArgs "number=2 1lef1rig" }
      set pgtk::lfrt 1
   } elseif [info exists pgtk::lfrt] {
      unset pgtk::lfrt
      viewport
      if { [string compare [file tail $viewport(file)] "1lef1rig.vpc"] == 0 } {
         viewport reset 
      }
   }
#
# PIMAGE command
#
   set plotArgs ""
   if { $parval(csize) != "" } { append plotArgs " csize=$parval(csize)" } 
   if { $parval(font) != "" }  { append plotArgs " font=$parval(font)" } 
   if { $parval(nobox) }       { append plotArgs " nobox" } 
   if { $parval(noframe) }     { append plotArgs " noframe" }
   if { $parval(overlay) }     { append plotArgs " overlay" }
   if { $parval(spectrogram) } { append plotArgs " nolabel" }
#
# Run the XIMAGE commands
#
   if { ! $parval(loaded) } {
      txwrite "levels $levelsArgs" 20
      if [catch "eval levels $levelsArgs" result] {
         map set $lastmap
         error {}
      }
   }
   if { $viewportArgs != "" } {
      eval viewport $viewportArgs
      txwrite "viewport $viewportArgs" 20
   }
   txwrite "pimage $plotArgs" 20
   if [catch "eval pimage $plotArgs" result] {
      map set $lastmap
      error {}
   }
   if { $parval(spectrogram) } {
      vplabel curvp margin=2.5 bottom Time
      vplabel curvp margin=2.5 left Energy
   }
   if { $viewportArgs != "" } {
      if { ![info exists pgtk::lfrt] } { viewport reset }
      txwrite "viewport reset" 20
   }
   if { $curmap != $lastmap } { map set $lastmap }
}
