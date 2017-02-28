#
#  Read file into Tcl array
#
proc rdarray {args} {

   set DEBUG 0 ;# If true, print diagnostics

   parseparm [parmloc rdarray] $args

   if { $cmdargc != 0 } {
       txwrite " Wrong number of arguments: $cmdargv" 10
       error {}
   }

# PARAMETERS
# filename  - Filename to read array from
# delim     - Column delimiter (default-> white space)
# columns   - List of columns to read in by name/number (First index:1)
# headers   - List of column header names (If blank, use numbers)
# readhead  - Get header names from file
# varname   - Name of output array (default-> inarray)
# expr      - Regular expression(s) to match lines for info
# matchlist - List of matches for () in expr
# sublist   - List of expression/substitution pairs

#
#  Open ASCII file and read in
#
   if { $parval(filename) == "" } {
      txwrite " rdarray: No filename given" 10
      error {}
   }
   set fp [open $parval(filename) r]
   set data [read $fp]
   close $fp
#
#  Clear out existing array if it exists
#
   if { $parval(varname) == "" } { 
      set varname inarray
      txwrite "Reading $parval(filename) into inarray variable" 10
   } else {
      set varname $parval(varname)
      txwrite "Reading $parval(filename)" 15
   }
   upvar 1 $varname outary
   if [info exists outary] { unset outary }
#
#  Build up user-entered regular expression command to 
#  retrieve information
#
   set matchlist [listclean $parval(matchlist)]
   if { $parval(expr) != "" } {
      set recmd [concat "regexp \{$parval(expr)\} \$line" tmp $matchlist]
   } else {
      set recmd "expr 0"  ;# Return no match every time
   }
#
#  Verify syntax of sublist (i.e. {regexp1 substitution1 regexp2 sub2})
#  Must be even number
#
   set rsexpr {}
   set rssub {}
   if { [expr int([llength $parval(sublist)]/2.0)] !=
        [expr int(([llength $parval(sublist)]/2.0)+0.5)] } {
      error {sublist usage: sublist="regexp1 sub1 regexp2 sub2"}
   }
   set i 0
   foreach val $parval(sublist) {
      if { !$i } {
         lappend rsexpr $val
         incr i
      } else {
         lappend rssub $val
         incr i -1
      }
   }
#
#  Load file
#
   set data [split $data "\n"]
   set headers [listclean $parval(headers)]
   set columns [listclean $parval(columns)]
#
#  Clear out matching expression variable
#
   foreach match $matchlist {
      upvar 1 $match up$match
      set up$match ""
   }

   foreach line $data {
      for { set i 0 } { $i < [llength $rsexpr] } { incr i } {
         if { $DEBUG } { puts "BEFORE:$line" }
         regsub -all [lindex $rsexpr $i] $line [lindex $rssub $i] line
         if { $DEBUG } { puts "AFTER: $line" }
      }
      if [eval $recmd] { 
         foreach match $matchlist {
            lappend up$match [subst $$match]
         }
#        set recmd "expr 0"  ;# uncomment for one-time only match
         continue 
      }          ;# Match expression and append submatches

      if [regexp {^\s*$} $line] { continue } ;# Throw out blank lines
      if [regexp {^[#!]} $line] { continue } ;# Throw out comments

      if { $parval(delim) != "" } {
         set tmpvals [split $line $parval(delim)]
      } else {
         set tmpvals $line
      }
#
#     If delimiters at beginning and/or end, throw out blank column
#
      if { [lindex $tmpvals 0] == "" && [lindex $tmpvals end] == "" } {
         set tmpvals [lrange $tmpvals 1 [expr [llength $tmpvals] - 2]]
      }
#
#     Trim leading and trailing whitespace
#
      set vals {}
      foreach val $tmpvals {
         set val [string trim $val]
         lappend vals $val
      }
#
#    If no headers are given, generate index list to match no. of values
#
      if { [llength $headers] == 0 } { 
         if { $parval(readhead) } { 
            set headers $vals
            continue
         } else {
            for { set i 1 } { $i <= [llength $vals] } { incr i } {
               lappend headers $i
            }
         }
      }
      if { [llength $vals] < [llength $headers] } {
         error {Number of column values < number of column headers}
      }
      for { set i 0 } { $i < [llength $vals] } { incr i } {
         if { $i < [llength $headers] } {
            set name [lindex $headers $i]
            if { [llength $columns] == 0 || 
                 [lsearch $columns $name] != -1 } {
               lappend outary($name) [lindex $vals $i]
            }
         }
      }
   }
#
#  Set informational variable rdarray
#
   upvar 1 rdarray rdarray
   set rdarray(varname) $varname
   if { [llength $columns] > 0 } {
      set rdarray(headers) $columns
   } else {
      set rdarray(headers) $headers
   }
   return {}
}
