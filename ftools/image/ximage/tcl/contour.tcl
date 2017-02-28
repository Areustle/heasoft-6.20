#
#  Emulate old CONTOUR command
#  Runs LEVELS, VIEWPORT, PCONTOUR depending on arguments
#
proc contour {args} {

   global curmap savmode savmap maxmaps tchat lchat default

   parseparm [parmloc contour] $args

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
   set overlay $parval(overlay)
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
         txwrite " Use MARITH, then CONTOUR result" 5
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
         txwrite " Use MARITH, then CONTOUR result" 5
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
   }
   if { $parval(sub_map) } { 
      if { $mapin != "" } {
         txwrite " SUB_MAP and MAPID are incompatible" 5
         txwrite " Use MARITH, then CONTOUR result" 5
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
   }
   if { $parval(exposure_map) } {
      set exmapid [chheader key=EXMAPID]
      if { $exmapid == "" } {
         txwrite " No exposure map" 10
         error {}
      }
      map set $exmapid
   }
   if { $parval(correct_exposure) } {
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
   set statistic $parval(statistic)
   if { [llength $parval(mult_levels)] !=0 } { set statistic 1 }
   if { [llength $parval(sigma_levels)] !=0 } { set statistic 1 }
   if { $statistic } {
#
# Bglevels command
#
      set levelsCmd bglevels

      if { [llength $parval(mult_levels)] !=0 } {
         lappend levelsCmd "mult_levels=$parval(mult_levels)"
      }
      if { [llength $parval(sigma_levels)] !=0 } {
         lappend levelsCmd "sigma_levels=$parval(sigma_levels)"
      }
      if { $parval(background) != "" } {
         lappend levelsCmd "background=$parval(background)"
      }
   } else {
#
# Levels command
#
      set levelsCmd levels
      set levmode histo
      if { $parval(histo) }    { set levmode histo }
      if { $parval(linear) }   { set levmode linear }
      if { $parval(log) }      { set levmode log }
      if { $parval(sqrt) }     { set levmode sqrt }
      lappend levelsCmd $levmode

      if { $parval(no_of_cont_levels) != "" } { 
         lappend levelsCmd number=$parval(no_of_cont_levels)
      }

      if { $parval(minlevel) != "" } { 
         lappend levelsCmd minlevel=$parval(minlevel)
      }
      if { $parval(maxlevel) != "" } { 
         lappend levelsCmd maxlevel=$parval(maxlevel)
      }
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
   if { $parval(left) } { 
      if { $overlay } {
         if {[catch {pgtk::switchvp 1} result]} {
            set viewportArgs "number=1 1lef1rig" 
         }
      } else {
         set viewportArgs "number=1 1lef1rig" 
      }
      set pgtk::lfrt 1
   } elseif { $parval(right) } { 
      if { $overlay } {
         if {[catch {pgtk::switchvp 2} result]} {
            set overlay 0
            set viewportArgs "number=2 1lef1rig" 
         }
      } else {
         set viewportArgs "number=2 1lef1rig" 
      }
      set pgtk::lfrt 1
   } elseif [info exists pgtk::lfrt] {
      unset pgtk::lfrt
      viewport
      if { [string compare [file tail $viewport(file)] "1lef1rig.vpc"] == 0 } {
         viewport reset 
      }
   }
#
# PCONTOUR command
#
   set plotArgs ""
   if { $parval(csize) != "" }  { append plotArgs " csize=$parval(csize)" } 
   if { $parval(font) != "" }   { append plotArgs " font=$parval(font)" } 
   if { $parval(nobox) }        { append plotArgs " nobox" } 
   if { $parval(noframe) }      { append plotArgs " noframe" }
   if { $overlay }              { append plotArgs " overlay" }
   if { $parval(spectrogram) }  { append plotArgs " nolabel" }
   if { $parval(color) != "" }  { append plotArgs " color=$parval(color)" } 
   if { $parval(lwidth) != "" } { append plotArgs " lwidth=$parval(lwidth)" } 
   if { $parval(lstyle) != "" } { append plotArgs " lstyle=$parval(lstyle)" } 
   if { !$statistic && $parval(first_contour_drawn) != "" } { 
      append plotArgs " first_contour_drawn=$parval(first_contour_drawn)"
   }
   if { $parval(nocoortrf) } { append plotArgs " trf={0.0 1.0 0.0 0.0 0.0 1.0}" }
#
# Run the XIMAGE commands
#
   if { ! $parval(loaded) } {
      set onumlevs $default(numlevs)
      txwrite "$levelsCmd" 20
      if [catch "eval $levelsCmd" result] {
         map set $lastmap
         error {}
      }
#
# Levels set to # messages can be confusing, so temporarily
# shut off chat
#
      set otchat $tchat
      set olchat $lchat
      if { $tchat <=10 && $lchat <=10 } { chatter 0 0 }
      levels number=$onumlevs
      chatter $otchat $olchat
   }
   if { $viewportArgs != "" } {
      eval viewport $viewportArgs
      txwrite "viewport $viewportArgs" 20
   }
   txwrite "pcontour $plotArgs" 20
   if [catch "eval pcontour $plotArgs" result] {
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
