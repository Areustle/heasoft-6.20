proc srcmrg {args} {
#
#  Takes sources from detect output files and merges them 
#  together keeping only unique sources
#
   parseparm [parmloc srcmrg] $args

   if { $cmdargc < 1 } {
      txwrite " Wrong number of arguments: At least one file required" 10
      error {}
   }

   global tchat lchat
   set debug 0

   set detdefault 1  ;# Variable that keeps track of the assumption
                     ;# that inputs are detect files, so we know
                     ;# whether it's safe to replace the first output
                     ;# column with a new source number

   set plot $parval(plot)                  ;# plot final source list?
   set outfile $parval(outfile)            ;# write merged list to file
   set format [listclean $parval(format)]  ;# column format of output
   set racol [listclean $parval(racol)]   ;# ra columns in file
   if { [llength $racol] == 0 } {         ;# Assume detect file
      set racol {6 7 8}                   ;#  if not given
   } else {
      set detdefault 0
   }
   set deccol [listclean $parval(deccol)] ;# dec columns in file
   if { [llength $deccol] == 0 } {        ;# Assume detect file
      set deccol {9 10 11}                ;#  if not given
   } else {
      set detdefault 0
   }
   if { $parval(tolerance) == "" } {      ;# tolerance in arcsec
      set toldeg 1e-5
   } else {
      set toldeg [expr $parval(tolerance)/3600.0]  ;# cnvt to degree
   }

   if { !$plot && $outfile == "" } {
      txwrite " The parameters plot and/or outfile are needed\
                to produce any result" 10
      error {}
   }
#
#  Read in all files and concatenate their columns into allcols array
#
   set first 1
   foreach file $cmdargv {

      if { $first } {
         rdarray var=allcols $file
         set filelen [llength $allcols(1)]
         set headers $rdarray(headers)    ;# Record order of columns
      } else {
         rdarray var=tmpcols $file
         lappend filelen [llength $tmpcols(1)]
         if { [llength [array names allcols]] != \
              [llength [array names tmpcols]] } {
            txwrite "Mismatch between number of columns in files" 5
            error {}
         }
         foreach i [array names allcols] {
            set allcols($i) [concat $allcols($i) $tmpcols($i)]
         }
      }
      set first 0
   }
#
#  Extract relevant RA/Dec columns
#
   set srcra {}
   set srcdec {}

   for { set i 0 } { $i < [llength $allcols(1)] } { incr i } {
      set rastr ""
      foreach col $racol {
         if { $rastr == "" } {
            set rastr [lindex $allcols($col) $i]
         } else {
            set rastr "$rastr [lindex $allcols($col) $i]"
         }
      }
      set decstr ""
      foreach col $deccol {
         if { $decstr == "" } {
            set decstr [lindex $allcols($col) $i]
         } else {
            set decstr "$decstr [lindex $allcols($col) $i]"
         }
      }
      lappend srcra $rastr
      lappend srcdec $decstr
   }

   set grplist {}

   # Turn down chat level
   set cleanup "chat $tchat $lchat"
   chat 5 5
#
#  Calculate distance from first source, sort into order
#  of increasing distance from the first source 
# 
#  Use srcord as special sorting list, which contains elements
#  of the form {distance original_index}
#
   set ra0 [lindex $srcra 0]
   set dec0 [lindex $srcdec 0]
   lappend srcord {0. 0}
   for { set i 1 } { $i < [llength $srcra] } { incr i } {
      set result [xan::srcoff $ra0 $dec0\
                           [lindex $srcra $i] [lindex $srcdec $i]]
      if { $result == "" } {
         eval $cleanup
         return
      }
      lappend srcord [list $result $i]
   }
#
#  Go through sources. If difference between successive sources are
#  within tolerance, check distance between them to see if they are the
#  same source
#
   set srcsort [lsort -real -index 0 $srcord]
   set lastdist [expr -$toldeg*2]  ;# Ensure first source isn't compared
                                    # with itself by setting the
                                    # distance to a value larger than
                                    # the tolerance
   set cmpgrp {}
   set lastgrp 0
   set n 0

   while { $n < [llength $srcsort] || [llength $cmpgrp] > 0 } {

      if { $n < [llength $srcsort] } {
         set item [lindex $srcsort $n]
         set diff [expr [lindex $item 0]-$lastdist]
         if { $debug } { puts "ITEM: $item DIFF: $diff" }
      } else {
         if { $debug } { puts "LAST GROUP CLEANUP: $cmpgrp" }
         set lastgrp 1
      }

      if { !$lastgrp && $diff < $toldeg } {

         lappend cmpgrp [lindex $item 1]
         if { $debug } { puts "WITHIN TOLERANCE: $diff $cmpgrp" }

      } else {

         while { [llength $cmpgrp] > 0 } {

            if { $debug } { puts "PROCESS GRP: $cmpgrp" }
            set tmpgrp {}
            set isrc [lindex $cmpgrp 0]
            lappend tmpgrp $isrc
            set ra1 [lindex $srcra $isrc]
            set dec1 [lindex $srcdec $isrc]

            set cmpgrp [lreplace $cmpgrp 0 0]
            set i 0
            while { $i < [llength $cmpgrp] } {
               set j [lindex $cmpgrp $i]
               set result [xan::srcoff $ra1 $dec1 [lindex $srcra $j]\
                                       [lindex $srcdec $j]]
               if { $result == "" } {
                  eval $cleanup
                  return
               }
               if { $result < $toldeg } {
                  lappend tmpgrp $j
                  set cmpgrp [lreplace $cmpgrp $i $i]
               } else {
                  incr i
               }
            }
            lappend grplist [lsort -integer $tmpgrp]
         }

         if { !$lastgrp } {
            lappend cmpgrp [lindex $item 1]
            if { $debug } { puts "ADD TO GRP: $cmpgrp" }
         }
      }
      set lastdist [lindex $item 0]
      incr n
   }

   # Restore chat level
   eval $cleanup

   txwrite " Unique sources: [llength $grplist]" 10

   if { $plot } {
#
#     Build label command that all points have in common
#
      set lblcmd "label"
      foreach qual {color csize lwidth font symcolor symcsize \
                    symlwidth angle just} {
         if { $parval($qual) != "" } { 
            lappend lblcmd "$qual=$parval($qual)"
         }
      }
      if { $parval(symbol) == "" } { 
         lappend lblcmd "symbol=3"
      } else {
         lappend lblcmd "symbol=$parval(symbol)"
      }
      lappend lblcmd "clip"

      foreach grp $grplist {
         set i [lindex $grp 0]
         set fipair [xan::fileidx $i $filelen]
#
#        Append specific parameters
#
         set cmd $lblcmd
         lappend cmd "ra=[lindex $srcra $i]"
         lappend cmd "dec=[lindex $srcdec $i]"
         if { [llength $filelen] == 1 } {  
            lappend cmd [lindex $fipair 1] ;# Source number only
         } else {
            lappend cmd [join $fipair "-"] ;# Filenum-Srcnum
         }
         eval $cmd
#        if { $debug } { puts $cmd }
      }
   }

   if { $outfile != "" } {
      txwrite " Output file: $outfile" 10
#
#     Default output format (mimic .det file)
#     Replace first column with incrementing detect number (#)
#
      if { [llength $format] == 0 } {
         set format [lreplace $headers 0 0 "#"]
      }
      set n 1
      set nawarn 0
      foreach grp $grplist {
         set i [lindex $grp 0]
         foreach col $format {
            if { $col == "#" } {
               lappend outary($col) $n 
            } elseif { $col == "old" } {
               set coltxt {}
               foreach j $grp {
                  lappend coltxt [join [xan::fileidx $j $filelen] "-"]
               }
               lappend outary($col) [join $coltxt {/}]
            } elseif { $col == "ra" } {
               lappend outary($col) [xan::raflt [lindex $srcra $i]]
            } elseif { $col == "dec" } {
               lappend outary($col) [xan::decflt [lindex $srcdec $i]]
            } else {
               if [info exists allcols($col)] {
                  lappend outary($col) [lindex $allcols($col) $i]
               } else {
                  if { !$nawarn } {
                     txwrite "WARNING: Column '$col' not available" 10
                     set nawarn 1
                  }
                  lappend outary($col) "N/A"
               }
            }
         }
         incr n
      }
      prarray file=$outfile order=$format noheader outary
   }
   if { $debug } { return $grplist }
}

namespace eval xan {

proc srcoff {ra1 dec1 ra2 dec2} {
#
#  Internal function that calls offset command and returns result
#
   set cmd "offset"
   lappend cmd "ra1=$ra1"
   lappend cmd "dec1=$dec1"
   lappend cmd "ra2=$ra2"
   lappend cmd "dec2=$dec2"
   if { [catch {eval $cmd}] == 0 } {
      return $offset(deg)
   } else {
      txwrite "ERROR: offset command failed" 5
      return ""
   }
}

proc fileidx {i filelen} {
#
#  Find file number and index within file
#  Given index (i) in appended list and list of file lengths
#
   set k [expr $i + 1]
   set j $k
   set ifile 1
   foreach len $filelen {
      set k [expr $k - $len]
      if { $k > 0 } {
         set j $k
         incr ifile
      } else {
         break
      }
   }
   return [list $ifile $j]
}

} ;# end xan namespace
