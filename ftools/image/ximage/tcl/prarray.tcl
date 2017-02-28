#
#  Display array nicely.  Particularly useful for arrays of lists
#  where lists are of equal length, although any array will work.
#  (Also writable to file)
#
proc prarray {args} {

   parseparm [parmloc prarray] $args

   if { $parval(varname) == "" } {
      txwrite " No array variable name given" 10
      error {}
   }
   set aryname $parval(varname)
   upvar 1 $aryname ary
   if { ![info exists ary] } {
      txwrite " Variable ($aryname) does not exist" 10
      error {}
   }

#
#  Construct list of maximum string lengths for each array element
#
   if { $parval(order) == "" } {
      set nmlst [lsort -dictionary [array names ary]]
   } else {
      set nmlst [listclean $parval(order)]
   }

   if { $parval(filename) == "" } {
      set screen 1
      set outchan "stdout"
   } else {
      set screen 0
      set outchan [open $parval(filename) w]
   }
   if { $parval(noheader) } {
      set prhead 0
   } else {
      set prhead 1
   }
      
#
#  Constant parameters
#
   set scrwid 80  ;# Screen width
   set minspc 2   ;# Minimum space between columns
   set debug 1    ;# Print debug messages?

   set spc [format "%-*s" $minspc ""]

   set maxnum 0
   foreach name $nmlst {
      set maxlen($name) [string length $name]
      foreach elm $ary($name) {
         set clen [string length $elm]
         if { $clen > $maxlen($name) } { set maxlen($name) $clen }
      }
      if { [llength $ary($name)] > $maxnum } { 
         set maxnum [llength $ary($name)]
      }
   }
#
# Write comments (prepend '#' to every line)
#
   if { $parval(comments) != "" } {
      set comments [split $parval(comments) "\n"]
      foreach line $comments {
         puts $outchan "# $line"
      }
   }
#
#  Group array elements to fit across page width
#
   set iwid 0
   if { $parval(index) } {
      set iwid [expr [string length [expr $maxnum - 1]] + 1]
   } elseif { $parval(iplus) } {
      set iwid [expr [string length $maxnum] + 1]
   }
   set i 0
   set grp {}
   if { $screen && $prhead } {
      set ahdr " ${aryname}:"
      set pad [expr 2+[string length $aryname]]
   } else {
      set ahdr "#"
      set pad 1
   }
   if { $iwid > $pad } {
      set ahdr "$ahdr[format "%-*s" [expr $iwid-$pad] ""]"
      set pad $iwid
   }
   set hdr $ahdr
   set len $pad
   set last 0
   while { $i < [llength $nmlst] || $len != $pad } {

      if { $i >= [llength $nmlst] } { ;# Printing remaining columns
         set last 1
         set add 0
      } else {
         set name [lindex $nmlst $i]
         set add [expr $minspc + $maxlen($name)]
      }
      if { (($screen && [expr $len + $add] > $scrwid) && 
             [llength $grp] > 0) || $last } {
#
#        Print builtup array elements
#
         if { $prhead } { puts $outchan $hdr }

         set mxrow 1
         foreach name $grp {
            set nrows [llength $ary($name)]
            if { $nrows > $mxrow } { set mxrow $nrows }
         }
         for { set j 0 } { $j < $mxrow } { incr j } {
#
#           Support for printing index
#
            if { $parval(index) } {
               set line " $j"
            } elseif { $parval(iplus) } {
               set line " [expr $j+1]"
            } else {
               set line ""
            }
            set tmppad [expr $pad-[string length $line]]
            set line "$line[format "%-*s" $tmppad ""]"

            foreach name $grp {
               if { $j < [llength $ary($name)] } {
                  set str [lindex $ary($name) $j]
               } else {
                  set str ""
               }

               set line "$line$spc[format "%-*s" $maxlen($name) $str]"
            }
            puts $outchan $line
            set line [format "%-*s" $pad ""]
         }
         if { !$last } { puts $outchan "" } ;# Separate groups w/ a blank line
         set hdr $ahdr
         set len $pad
         set grp {}
      } else {
#
#        Add on array element
#
         set hdr "$hdr$spc[format "%-*s" $maxlen($name) $name]"
         lappend grp $name
         incr len $add
         incr i
      }
   }
   if { !$screen } { close $outchan }
}

