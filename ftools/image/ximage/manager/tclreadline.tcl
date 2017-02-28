namespace eval tclreadline {

#
#  Custom prompt
#

#variable prompt_string [file tail [info nameofexecutable]]

proc prompt1 {} {

#  variable prompt_string
#  return "$prompt_string\[[history nextid]\] "

   global xm_tcl_prompt
   if { ![info exists xm_tcl_prompt] } {
      set xm_tcl_prompt "> "
   }
   return $xm_tcl_prompt

}

#
#  Custom Loop to include log and script echoing 
#
proc Loop {args} {

    eval Setup ${args}

    uplevel #0 {

        while {1} {

            if [info exists tcl_prompt2] {
                set prompt2 $tcl_prompt2
            } else {
                set prompt2 "-> "
            }

            if {[catch {
                if {"" != [namespace eval ::tclreadline {info procs prompt1}]} {
                    set LINE [::tclreadline::readline read \
                    [::tclreadline::prompt1]]
                } else {
                    set LINE [::tclreadline::readline read %]
                }
                while {![::tclreadline::readline complete $LINE]} {
                    append LINE "\n"
                    append LINE [tclreadline::readline read ${prompt2}]
                }
            } ::tclreadline::errorMsg]} {
                puts stderr [list tclreadline::Loop: error. \
                $::tclreadline::errorMsg]
                continue
            }

            #
            #  Addition for ximage logging
            #
            if [info exists logchan] {
               puts $logchan "[::tclreadline::prompt1]$LINE"
            }
            if [info exists scriptchan] {
               # Don't echo "script none" command
               if { ![regexp -nocase {^scri[^ ]* none} $LINE] } {
                  puts $scriptchan $LINE
               }
            }

            # Magnus Eriksson <magnus.eriksson@netinsight.se> proposed
            # to add the line also to tclsh's history.
            #
            # I decided to add only lines which are different from
            # the previous one to the history. This is different
            # from tcsh's behaviour, but I found it quite convenient
            # while using mshell on os9.
            #
            if {[string length $LINE] && [history event 0] != $LINE} {
                history add $LINE
            }

            if [catch {
                set result [eval $LINE]
                if {$result != "" && [tclreadline::Print]} {
                    puts $result
                }
                set result ""
            } ::tclreadline::errorMsg] {
                puts stderr $::tclreadline::errorMsg
                #puts stderr [list while evaluating $LINE]
            }

        }
    }
}

#
#  Simple completer which returns nothing, so file completion
#  occurs every time
#

proc SimpleCompleter {part start end line} {

        return ""
}

#
#  Start readline loop
#

if { [info commands readline] != "" } {
   readline customcompleter ::tclreadline::SimpleCompleter
   readline builtincompleter 0
   if { ![file exists $ximage_history_file] } {
      close [open $ximage_history_file w]
   }
   Loop $ximage_history_file
} else {
   puts "tclreadline unavailable: reverting to standard Tcl command line"
}

} ;# namespace tclreadline
