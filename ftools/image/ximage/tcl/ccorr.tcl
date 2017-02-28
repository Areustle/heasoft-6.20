
proc ccorr {args} {
#
#  Takes a list of ra and dec, asks for a new location on 
#  the image to be selected for each and calculates a new
#  set of keywords to be applied
#
   parseparm [parmloc ccorr] $args

   if { $cmdargc != 0 } {
      txwrite " Wrong number of arguments: $cmdargv" 10
      error {}
   }

   global dismap curmap
   set map $dismap
   if { $dismap == "" } { set map $curmap }

   set lra0 $parval(ralist)
   set ldec0 $parval(declist)
   set srcfile $parval(filename)
   set disponly $parval(disponly)
   set color $parval(color)
   set symbol $parval(symbol)
   set symcsize $parval(symcsize)
   set symlwidth $parval(symlwidth)
   set csize $parval(csize)
   set savefile $parval(savefile)
   set matchfile $parval(matchfile)

   if { $matchfile != "" } {
      rdarray var=tmprd head={ra dec xpix ypix} $matchfile
   } elseif { $srcfile != "" } {
      rdarray var=tmprd head={ra dec} {expr=^\(([0-9]+)\)} match=equinox $srcfile
   }
   if [info exists tmprd] {
      set lra0 $tmprd(ra)
      set ldec0 $tmprd(dec)
   }
   set fmt {%d %02d %05.2f}

   set num [llength $lra0]
   if { $num != [llength $ldec0] } {
      txwrite "RA list must be of same length as Dec list" 5
      error {}
   }
   txwrite "Guide sources: $num" 10
   if { $num < 3 } { 
      error {At least 3 guide sources must be input for correction}
   }
#
#  Plot guide sources
#
   for { set i 0 } { $i < $num } { incr i } {
      set sra($i) [lindex $lra0 $i]
      set sdec($i) [lindex $ldec0 $i]
      coord map=$map ra=$sra($i) dec=$sdec($i)
      set sra($i) $coord(ra)
      set sdec($i) $coord(dec)
      set sx($i) $coord(xpix)
      set sy($i) $coord(ypix)
      if { $matchfile == "" } {
         label symbol=$symbol color=$color symcsize=$symcsize \
                              symlwidth=$symlwidth \
                              csize=$csize xpix=$sx($i) ypix=$sy($i) \
                              clip [expr $i+1]
      }
      set left($i) 1
#        txwrite "[expr $i+1]: [xan::HourRA $coord(ra) $fmt] \
#                              [xan::DegDec $coord(dec) $fmt]" 10
   }
   if { $disponly } { return }

   set idxlst {}
#
#  If matchfile, use selections read into tmprd array above
#
   if { $matchfile != "" } {
      for { set i 0 } { $i < $num } { incr i } {
         set nx($i) [lindex $tmprd(xpix) $i]
         set ny($i) [lindex $tmprd(ypix) $i]
         lappend idxlst $i
      }
   } else {
#
#  Select guide sources and actual location
#
      txwrite "Left-click plotted source, then actual source" 10
      txwrite "Middle-click deletes last selection and Right-click ends" 10
      txwrite "Note: At least 3 sources must be selected for correction" 10
      set done 0
      while { [llength $idxlst] < $num && !$done } {
         txwrite "\nSelect guide source: (Right-click to end)" 10
         set xy [select noerr]
         set btn1 $select(btn)
         switch $btn1 {
            "lf" {
               set x [lindex $xy 0]
               set y [lindex $xy 1]
               if [info exists mindist] { unset mindist }
               if [info exists mindidx] { unset minidx }
               for { set i 0 } { $i < $num } { incr i } {
                  if { $left($i) } {
                     set dist [expr pow($sx($i)-$x,2) + pow($sy($i)-$y,2)]
                     if { ![info exists mindist] || $dist < $mindist } {
                        set mindist $dist
                        set minidx $i
                     }
                  }
               }
               if { ![info exists minidx] } {
                 txwrite "No guide source left" 10
                 set done 1
                 continue
               }
               set i $minidx
               txwrite "[expr $i+1] => [xan::HourRA $sra($i) $fmt] \
                                       [xan::DegDec $sdec($i) $fmt] " 10
                                
               set xy [select xpix=$sx($i) ypix=$sy($i) noerr]
               set btn2 $select(btn)
               switch $btn2 {
                 "lf" {
                    lappend idxlst $i
                    set nx($i) [lindex $xy 0]
                    set ny($i) [lindex $xy 1]
                    draw arrow $sx($i) $sy($i) $nx($i) $ny($i)
                    set left($i) 0
                    txwrite "Offset: [expr $nx($i)-$sx($i)] \
                                     [expr $ny($i)-$sy($i)]" 10
                 }
                 "md" { txwrite "Cancelling current selection: [expr $i+1]" 10 }
                 "rt" { set done 1 }
               }
            }
            "md" { 
               if [llength $idxlst] {
                  set left([lindex $idxlst end]) 1
                  set idxlst [lreplace $idxlst end end] 
                  txwrite "Deleted [expr $i+1]: [xan::HourRA $sra($i) $fmt] \
                                                [xan::DegDec $sdec($i) $fmt]" 10
               } else {
                  txwrite "No source to delete" 10
               }
            }
            "rt" { set done 1 }
         }
      }
   } 
   if { [llength $idxlst] < 3 } {
      error {At least 3 guide sources must be selected for correction}
   }
#
#  Save selected guide sources, RA/Dec and image loc, to file
#
   if { $savefile != "" } {
      foreach i $idxlst {
         lappend savetmp(ra) $sra($i)
         lappend savetmp(dec) $sdec($i)
         lappend savetmp(xpix) $nx($i)
         lappend savetmp(ypix) $ny($i)
      }
      prarray file=$savefile order=ra,dec,xpix,ypix \
              comments="Saved session of XIMAGE ccorr command\nTo rerun: ccorr matchfile=$savefile" \
              savetmp 
   }
#
#  Calculate offsets from center for calculation of coord correction
#
   set xcen [chh key=DRPIX1 map=$map]
   set ycen [chh key=DRPIX2 map=$map]

   foreach i $idxlst {
      lappend xskew [expr $sx($i) - $xcen]
      lappend yskew [expr $sy($i) - $ycen]
      lappend xref [expr $nx($i) - $xcen]
      lappend yref [expr $ny($i) - $ycen]
   }
#
#  Iterate through calccor, which finds the delx,dely,theta that
#  minimizes differences between the reference and skewed points
#
   set pi [expr atan(1.)*4.]
   set itmax 10
   set itcnt 0
   set lastit 0
   set lastdx 0.
   set lastdy 0.
   set lastan 0.
   set initvec {}
   while {1} {

      set cmd [
         list calccor xskew=$xskew yskew=$yskew xref=$xref yref=$yref
      ]
      if { [llength $initvec] != 0 } {
         lappend cmd initvec=$initvec
      }
      if { $parval(norot) } {
         lappend cmd norot
      }
      txwrite $cmd 20
      eval $cmd
      set initvec [list $calccor(delx) $calccor(dely) $calccor(theta)]
#
# Compare results to jump out of loop
# when a calculation gives identical results (Assuming it
# converges within itmax iterations)
#
      if { $lastit == $calccor(iter) && $lastdx == $calccor(delx) && 
           $lastdy == $calccor(dely) && $lastan == $calccor(theta) } {
         break
      }
      incr itcnt
      if { $itcnt >= $itmax } {
         txwrite "Stopped before convergence" 5
         break 
      }
      set lastit $calccor(iter)
      set lastdx $calccor(delx)
      set lastdy $calccor(dely)
      set lastan $calccor(theta)
   }
   txwrite "\n Delx : $calccor(delx)" 5
   txwrite " Dely : $calccor(dely)" 5
   set rotdeg [expr $calccor(theta)*180./$pi]
   txwrite " Theta: $calccor(theta) (rad) $rotdeg (deg)" 5

   set ocrota2 [chh key=CROTA2 map=$map]
   set ocrval1 [chh key=CRVAL1 map=$map]
   set ocrval2 [chh key=CRVAL2 map=$map]
   txwrite "\n Old coordinates: " 10
   txwrite "  CROTA2 = $ocrota2" 10
   txwrite "  CRVAL1 = $ocrval1" 10
   txwrite "  CRVAL2 = $ocrval2" 10
#
#  Modify header to correct coordinates
#
   txwrite "\n Modifications: " 10
   chh key=CROTA2 val=[expr $ocrota2+$rotdeg] map=$map
   coord xp=[expr [chh key=DRPIX1]-$calccor(delx)] \
         yp=[expr [chh key=DRPIX2]-$calccor(dely)] map=$map
   chh key=CRVAL1 val=$coord(ra) map=$map
   chh key=CRVAL2 val=$coord(dec) map=$map
   wcs upwcs map=$map
#  txwrite "  CROTA2 = [chh key=CROTA2]" 10
#  txwrite "  CRVAL1 = [chh key=CRVAL1]" 10
#  txwrite "  CRVAL2 = [chh key=CRVAL2]" 10

   set ::ccorr(undo) "chh key=CROTA2 val=$ocrota2 map=$map; \
                       chh key=CRVAL1 val=$ocrval1 map=$map; \
                       chh key=CRVAL2 val=$ocrval2 map=$map; \
                       wcs upwcs map=$map"
   txwrite "\nTo undo coord changes: eval \$ccorr(undo)" 10
}

