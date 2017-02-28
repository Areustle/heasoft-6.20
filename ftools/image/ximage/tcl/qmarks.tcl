#
#  Replacements for ?? and ? (aka keywords) commands
#  (Formerly of GTCOM fame)
#
#  ?? - Lists summary of commands (from ximage.cmd)
#   ? - Lists summary of commands in keyword group (from ximage.kw)
#

proc ?? { {cmdlist {}} } {
#
#  Lists summary of commands in cmdlist with short description
#  If no argument given, list all commands
#
   global env
   set cmdfile "$env(XANADU)/image/ximage/manager/ximage.cmd"

   set cmdlen 10;  # Maximum command length to print
   set desclen 24; # Maximum description length to print
   set pagelen 24; # Maximum number of lines to a page

   if [catch "open $cmdfile r" fileId] {
      txwrite "Failed to open $cmdfile" 5
      error
   }
   set out ""
   set nlines 1
   txwrite " Characters before the * indicate the minimum abbreviation" 10
   foreach line [split [read $fileId] \n] {
      set i [string first " " $line]
      if { !([string index $line 0] == "*" || 
             [string index $line 0] == "!" ||
             $i < 0) } {
#
#  Test to see if command is in command list
#
         set cmd [string range $line 0 [expr $i-1]]
         if { [llength $cmdlist] != 0 && [lsearch $cmdlist $cmd] < 0 } {
            set prcmd 0
         } else {
            set prcmd 1
         }
         set desc [string trim [string range $line $i end] ]
#
#  Chop to max lengths
#
         set cmd [string range $cmd 0 [expr $cmdlen-1]]
         set desc [string range $desc 0 [expr $desclen-1]]
         
         if { $prcmd } {
            if { $out == "" } {
               set out [format " %-*s-> %-*s" $cmdlen $cmd $desclen $desc]
            } else {
               incr nlines
#
#  Do paging
#
               if { $nlines > $pagelen-1 } {
                   set answer [txread "<CR> continues, to exit type any character <CR> or <EOF>"]
                   if { $answer != "" } { return }
                   set nlines 1
               }
               txwrite "$out [format "%-*s-> %-*s" $cmdlen $cmd $desclen $desc]" 10
            set out ""
            }
         }
      }
   }
   if { $out != "" } { txwrite $out 10 }
}

proc keywords { {inkey ""} } {
#
#  Lists keyword groupings of commands and prompts for which
#  grouping should be listed
#
#  Behaves slightly different from GTCOM version.  Command
#  is modal like help.  Keywords are not commands in the
#  Tcl environment to avoid conflicts (e.g. info)
#
   global env
   set kwfile "$env(XANADU)/image/ximage/manager/ximage.kw"

   set kwlen 12; # Maximum keyword length to print
   set ncols 5;  # Number of columns to print keywords in
#
#  Read in keyword file
#
   if [catch "open $kwfile r" fileId] {
      txwrite "Failed to open $kwfile" 5
      error
   }
   foreach line [split [read $fileId] \n] {
      set entry [split $line ,]
      set keywd [string trim [lindex $entry 0]]
      set cmd [string trim [lindex $entry 1]]
      if { $keywd != "" && $cmd != "" } {
         lappend keyary($keywd) $cmd
      }
   }
   close $fileId
#
#  If inkey already set, print and exit
#
   if { $inkey != "" } {
      set match [array names keyary $inkey*]
      if { [llength $match] == 1 } {
         ?? $keyary($match)
      } elseif { [llength $match] == 0 } {
         txwrite " No $inkey keyword" 10
      } else {
         txwrite " Ambiguous keyword: $match" 10
      }
      return
   }
#
#  Print keywords
#
   txwrite "Enter one of the following keywords or ?? for a full command listing:" 5
   set out ""
   set kwcnt 0
   set keylist [lsort [array names keyary]]
   foreach key $keylist {
      set key [string range $key 0 [expr $kwlen-1]]
      incr kwcnt
      if { $kwcnt <= $ncols } {
         set out "$out [format "%-*s" $kwlen $key]"
      } else {
         txwrite $out 10
         set out " [format "%-*s" $kwlen $key]"
         set kwcnt 1
      }
   }
   if { $out != "" } { txwrite $out 10 }
#
#  Loop, asking for keywords
#
   set keepgoing 1
   while { $keepgoing } {
      set answer [txread "keywords> "]
      if { $answer == "" } {
         set keepgoing 0
      } elseif { $answer == "??" } {
         ??
      } else {
         set i [lsearch $keylist $answer*]
         if { $i < 0 } {
            txwrite " Invalid keyword" 5
         } else {
            ?? $keyary([lindex $keylist $i])
         }
      }
   }
}

interp alias {} ? {} keywords
