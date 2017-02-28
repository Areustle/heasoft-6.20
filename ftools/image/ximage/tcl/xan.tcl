#
#  Common procs used internally by ximage's Tcl commands
#  Hide in xan:: namespace to avoid collisions with commands
#
#  HourRA - Returns formatted RA given decimal value
#  DegDec - Returns formatted Dec given decimal value
#  stripzero - Strip leading zeroes to avoid octal interpretation
#  raflt - Return float value of input RA (HH MM SS.SSS)
#  decflt - Return float value of input Dec (DD MM SS.SSS)
#  prlist - Print Tcl list in columnar format
#  htyread - Read history file into Tcl history
#  htywrite - Write Tcl history to file
#
namespace eval xan {

#
#  Were powHourRA and powDegDec, borrowed from POW
#

# convert a decimal degree to HH MM SS.S
# the optional fmtStr needs to have 3 value placeholders (%'s) in h m s order

proc HourRA { deciValue {fmtStr "%dh%02dm%05.2fs"} } {
#Written by J. Xu
    if { $deciValue < 0} {
	set deciValue [expr $deciValue + 360]
    }
    set hourValue [expr $deciValue/15.0 + 1e-13]
    set hour [expr int($hourValue)]
    set minuValue [expr ($hourValue - $hour)*60.0]
    set minu [expr int($minuValue)]
    set scndValue [expr ($minuValue - $minu)*60.0]
    set scnd $scndValue
    while {$hour >= 24} {set hour [expr $hour - 24]}
    while {$hour < 0} {set hour [expr $hour + 24]}

    # Check if we are rounding seconds to next value

    set scndFmt [lindex [split $fmtStr %] 3]
    set scndStr [format %$scndFmt $scnd]
    if { [regexp {^ *60} $scndStr] } {
	set scnd 0
	incr minu
	if {$minu == 60} {
           set minu 0
           incr hour
           if { $hour==24 } {
              set hour 0
           }
        }
    }
    return [format $fmtStr $hour $minu $scnd]
}

# convert a decimal degree to DD MM SS.S
# the optional fmtStr needs to have 3 value placeholders (%'s) in h m s order

proc DegDec { deciValue {fmtStr "%d:%02d:%05.2f"} } {
#Written by J. Xu
    if { $deciValue < 0} {
	set isNeg 1
    } else {
	set isNeg 0
    }
    set deciValue [expr abs($deciValue) + 1e-13]
    set deg [expr int($deciValue)]
    while {$deg > 360} {set deg [expr $deg - 360]}
    while {$deg < -360} {set deg [expr $deg + 360]}
    set minuValue [expr ($deciValue - $deg)*60.0]
    set minu [expr int($minuValue)]
    set scndValue [expr ($minuValue - $minu)*60.0]
    set scnd $scndValue

    # Check if we are rounding seconds to next value

    set scndFmt [lindex [split $fmtStr %] 3]
    set scndStr [format %$scndFmt $scnd]
    if { [regexp {^ *60} $scndStr] } {
	set scnd 0
	incr minu
	if {$minu == 60} {
           set minu 0
           incr deg
           if { $deg==360 } {
              set deg 0
           }
        }
    }
    if { $isNeg } {
	return [format "-$fmtStr" $deg $minu $scnd]
    } else {
	return [format $fmtStr $deg $minu $scnd]
    }
}

proc stripzero {value} {
#
#  To avoid Tcl interpretting numbers beginning with zero
#  as octal numbers, strip leading zeroes
#
   set retval [string trimleft $value 0]
   if { ![string length $retval] } {
      return 0
   }
   return $retval
}

#
#  Return float value of input RA
#  (i.e. hh mm ss.sss -> ddd.dddddd)
#  Note, if coordinate is already in decimal form, just return it
#
proc raflt { origval } {

   set vallist [split $origval " "]
   if { [llength $vallist] == 1 } {
      return $origval
   } elseif { [llength $vallist] == 3 } {
      set raval [stripzero [lindex $vallist 0]]
      set sign 1
      if { $raval < 0 } {
         set raval [expr -$raval]
         set sign -1
      }
      set raval [expr $raval + [stripzero [lindex $vallist 1]]/60. + \
                               [stripzero [lindex $vallist 2]]/3600.]
      return [expr $sign*$raval*15.]
   } else {
      txwrite "Bad format: $origval" 10
      error {}
   }

}

#
#  Return float value of input Dec
#  (i.e. dd mm ss.sss -> dd.dddddd)
#  Note, if coordinate is already in decimal form, just return it
#
proc decflt { origval } {

   set vallist [split $origval " "]
   if { [llength $vallist] == 1 } {
      return $origval
   } elseif { [llength $vallist] == 3 } {
      set decval [stripzero [lindex $vallist 0]]
      set sign 1
      if { $decval < 0 } {
         set decval [expr -$decval]
         set sign -1
      }
      set decval [expr $decval + [stripzero [lindex $vallist 1]]/60. + \
                                 [stripzero [lindex $vallist 2]]/3600.]
      return [expr $sign*$decval]
   } else {
      txwrite "Bad format: $origval" 10
      error {}
   }

}

#
#  Display list nicely
#
proc prlist {lst} {

   set scrwid 80  ;# Screen width
   set minspc 2   ;# Minimum space between columns
   set minwid 12  ;# Minimum column width
   set maxcol 5   ;# Maximum number of columns onscreen

   set success 0
   set ncol $maxcol
#
#  Construct list of string lengths for quick access
#
   foreach item $lst {
      lappend lenlst [string length $item]
   }
#
#  Look for optimum number of columns
#
   while { !$success } {

      set len [llength $lst]
#
#  Initialize columns with minimum width
#
      for { set i 0 } { $i < $ncol } { incr i } {
         set colwid($i) $minwid
      }
#
#  Expand columns based on contents
#
      set i 0
      while { $i < $len } {
         set icol [expr int(fmod($i,$ncol))]
         if { [lindex $lenlst $i] > $colwid($icol) } {
            set colwid($icol) [lindex $lenlst $i]
         }
         incr i
      }
#
#  Find output width
#
      set totwid 0
      for { set j 0 } { $j < $ncol } { incr j } {
         incr totwid [expr $colwid($j) + $minspc]
      }
#
#  If any overflows, try again with one less number of columns
#
      if { $totwid > $scrwid } {
         incr ncol -1
      } else {
         set success 1
      }
#
#  Special case when any one string exceeds screen width
#
      if { $ncol <=0 } { 
         set ncol 1
         set success 1
      }
            
   }
#
#  Write out list based on column widths
#
   set spc [format "%-*s" $minspc ""]
   set i 0
   set icol 0
   set line ""
   while { $i < $len } {
      set line "$line$spc[format "%-*s" $colwid($icol) [lindex $lst $i]]"
      incr icol
      if { $icol == $ncol } {
         txwrite $line 10
         set line ""
         set icol 0
      }
      incr i
   }
   if { $icol > 0 } { txwrite $line 10 }
}

#
#  Routines that implement history saving and restoring
#

proc htywrite {history_file} {
#
# Saves the last [history keep] commands to history file
#
   if [ string equal $history_file "" ] { return }
   set htychan [open $history_file w]
   set ilast [history nextid]
   set ifirst [expr $ilast-[history keep]]
   if { $ifirst < 1 } { set ifirst 1 }
   for {set i $ifirst} {$i < $ilast} {incr i} {
      puts $htychan [history event $i]
   }
   close $htychan
}

proc htyread {history_file} {
#
#  Adds the commands in history file to the Tcl history
#
   if [ string equal $history_file "" ] { return }
   if { ![file exists $history_file] } { return }
   set htychan [open $history_file r]
   while { ![eof $htychan] } {
      set numch [get $htychan line]
      if { $numch > 0 && ![regexp {^[0-9]} $line] } { history add $line }
   }
   close $htychan
}

} ;# End xan namespace
