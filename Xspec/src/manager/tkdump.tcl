## dump - outputs variables/procedure/widget info in source'able form.
## Accepts glob style pattern matching for the names
# ARGS:	type	- type of thing to dump: must be variable, procedure, widget
# OPTS: -nocomplain
#		don't complain if no vars match something
#	-filter pattern
#		specifies a glob filter pattern to be used by the variable
#		method as an array filter pattern (it filters down for
#		nested elements) and in the widget method as a config
#		option filter pattern
#	--	forcibly ends options recognition
# Returns:	the values of the requested items in a 'source'able form
## 
proc dump {type args} {
    set whine 1
    set code  ok
    if {![llength $args]} {
	## If no args, assume they gave us something to dump and
	## we'll try anything
	set args $type
	set type any
    }
    while {[string match -* [lindex $args 0]]} {
	switch -glob -- [lindex $args 0] {
	    -n* { set whine 0; set args [lreplace $args 0 0] }
	    -f* { set fltr [lindex $args 1]; set args [lreplace $args 0 1] }
	    --  { set args [lreplace $args 0 0]; break }
	    default {return -code error "unknown option \"[lindex $args 0]\""}
	}
    }
    if {$whine && ![llength $args]} {
	return -code error "wrong \# args: [lindex [info level 0] 0] type\
		?-nocomplain? ?-filter pattern? ?--? pattern ?pattern ...?"
    }
    set res {}
    switch -glob -- $type {
	c* {
	    # command
	    # outputs commands by figuring out, as well as possible, what it is
	    # this does not attempt to auto-load anything
	    foreach arg $args {
		if {[llength [set cmds [info commands $arg]]]} {
		    foreach cmd [lsort $cmds] {
			if {[lsearch -exact [interp aliases] $cmd] > -1} {
			    append res "\#\# ALIAS:   $cmd =>\
				    [interp alias {} $cmd]\n"
			} elseif {
			    [llength [info procs $cmd]] ||
			    ([string match *::* $cmd] &&
			    ([info tclversion] >= 8) &&
			    [llength [namespace eval [namespace qual $cmd]
				    info procs [namespace tail $cmd]]])
			} {
			    if {[catch {dump p -- $cmd} msg] && $whine} {
				set code error
			    }
			    append res $msg\n
			} else {
			    append res "\#\# COMMAND: $cmd\n"
			}
		    }
		} elseif {$whine} {
		    append res "\#\# No known command $arg\n"
		    set code error
		}
	    }
	}
	v* {
	    # variable
	    # outputs variables value(s), whether array or simple.
	    if {![info exists fltr]} { set fltr * }
	    foreach arg $args {
		if {![llength [set vars [uplevel 1 info vars [list $arg]]]]} {
		    if {[uplevel info exists $arg]} {
			set vars $arg
		    } elseif {$whine} {
			append res "\#\# No known variable $arg\n"
			set code error
			continue
		    } else { continue }
		}
		foreach var [lsort $vars] {
		    if {[info tclversion] >= 8} {
			set var [uplevel [list namespace which -variable $var]]
		    }
		    upvar $var v
		    if {[array exists v] || [catch {string length $v}]} {
			set nst {}
			append res "array set [list $var] \{\n"
			if {[array size v]} {
			    foreach i [lsort [array names v $fltr]] {
				upvar 0 v\($i\) __a
				if {[array exists __a]} {
				    append nst "\#\# NESTED ARRAY ELEM: $i\n"
				    append nst "upvar 0 [list $var\($i\)] __a;\
					    [dump v -filter $fltr __a]\n"
				} else {
				    append res "    [list $i]\t[list $v($i)]\n"
				}
			    }
			} else {
			    ## empty array
			    append res "    empty array\n"
			    append nst "unset [list $var](empty)\n"
			}
			append res "\}\n$nst"
		    } else {
			append res [list set $var $v]\n
		    }
		}
	    }
	}
	p* {
	    # procedure
	    foreach arg $args {
		if {
		    ![llength [set procs [info proc $arg]]] &&
		    ([string match *::* $arg] && ([info tclversion] >= 8) &&
		    [llength [set ps [namespace eval \
			    [namespace qualifier $arg] \
			    info procs [namespace tail $arg]]]])
		} {
		    set procs {}
		    set namesp [namespace qualifier $arg]
		    foreach p $ps {
			lappend procs ${namesp}::$p
		    }
		}
		if {[llength $procs]} {
		    foreach p [lsort $procs] {
			set as {}
			foreach a [info args $p] {
			    if {[info default $p $a tmp]} {
				lappend as [list $a $tmp]
			    } else {
				lappend as $a
			    }
			}
			append res [list proc $p $as [info body $p]]\n
		    }
		} elseif {$whine} {
		    append res "\#\# No known proc $arg\n"
		    set code error
		}
	    }
	}
	w* {
	    # widget
	    ## The user should have Tk loaded
	    if {![llength [info command winfo]]} {
		return -code error "winfo not present, cannot dump widgets"
	    }
	    if {![info exists fltr]} { set fltr .* }
	    foreach arg $args {
		if {[llength [set ws [info command $arg]]]} {
		    foreach w [lsort $ws] {
			if {[winfo exists $w]} {
			    if {[catch {$w configure} cfg]} {
				append res "\#\# Widget $w\
					does not support configure method"
				set code error
			    } else {
				append res "\#\# [winfo class $w]\
					$w\n$w configure"
				foreach c $cfg {
				    if {[llength $c] != 5} continue
				    ## Check to see that the option does
				    ## not match the default, then check
				    ## the item against the user filter
				    if {[string compare [lindex $c 3] \
					    [lindex $c 4]] && \
					    [regexp -nocase -- $fltr $c]} {
					append res " \\\n\t[list [lindex $c 0]\
						[lindex $c 4]]"
				    }
				}
				append res \n
			    }
			}
		    }
		} elseif {$whine} {
		    append res "\#\# No known widget $arg\n"
		    set code error
		}
	    }
	}
	a* {
	    ## see if we recognize it, other complain
	    if {[regexp {(var|com|proc|widget)} \
		    [set types [uplevel 1 what $args]]]} {
		foreach type $types {
		    append res "[uplevel 1 dump $type $args]\n"
		}
	    } else {
		set res "dump was unable to resolve type for \"$args\""
		set code error
	    }
	}
	default {
	    return -code error "bad [lindex [info level 0] 0] option\
		    \"$type\": must be variable, command, procedure,\
		    or widget"
	}
    }
    return -code $code [string trimright $res \n]
}
