# xspec.tcl
#
# XSPEC specific initialization of tcl.
#

#set NoTkCon 1

# Make a `system' command for sending things to the shell.

proc syscall args { 
    global spawn_args env
    if { [llength $args] == 0 } {
	return [uplevel exec >&@stdout <@stdin $env(SHELL) ]
    } else {
	set spawn_args "$args"
	return [uplevel exec >&@stdout <@stdin $env(SHELL) -c {$spawn_args} ] 
    }
}



proc null {} {
        return;
}

interp alias {} sys {} syscall
interp alias {} sy {} syscall
interp  alias {} ch {} chatter
interp  alias {} fr    {} freeze
interp  alias {} pl    {} plot
interp  alias {} fl    {} flux
interp  alias {} /    {} null
interp  alias {} local {} lmod

# Aliases needed to allow command abbreviations when filenames include
# (single backslash esacped) square brackets for HDU specification:
interp alias {} da {} data
interp alias {} dat {} data
interp alias {} bac {} backgrnd
interp alias {} back {} backgrnd
interp alias {} backg {} backgrnd
interp alias {} backgr {} backgrnd
interp alias {} backgrn {} backgrnd
interp alias {} res {} response
interp alias {} resp {} response
interp alias {} respo {} response
interp alias {} respon {} response
interp alias {} respons {} response
interp alias {} corf {} corfile
interp alias {} corfi {} corfile
interp alias {} corfil {} corfile


# Not yet implemented.
# make an index from scripts in xspec's tcl script directory, so that they will be 
# auto-loaded on initialization.
#
# Command to make a hardcopy


# temporary fix for directory problem with xs_continue

#source xs_continue.tcl


# add xspec12 addin directory to tcl's auto_path variable
set  libdir $env(HEADAS)/lib
if { [file isdirectory $env(HEADAS)/../spectral/scripts] } {
   set scriptDir $env(HEADAS)/../spectral/scripts
} else {
   set scriptDir $env(HEADAS)/spectral/scripts
}
lappend auto_path $libdir
lappend auto_path $scriptDir


# Set script echoing on, which is the backward compatible behaviour

set xs_echo_script 1

# If the first argument to XSPEC was a dash, the others should be
# script files to execute with the `@' command.

source $scriptDir/global_customize.tcl

if { ![string compare [lindex $argv 0]  "-" ] } {
   # Beginning with Tcl 8.5, the Xspec commands' tclIndex file
   #  doesn't seem to be sourced when the input script is run
   #  from batch mode.  This auto_load call will force it to
   #  look for tcloutr, and thereby source the tclIndex file.
   auto_load tcloutr
    for {set i 1} {$i < [llength $argv]} {incr i} {
	set init_file [lindex $argv $i]
	puts "\nExecuting script file \"$init_file\" ...\n"
	set xs_echo_script 1
	@$init_file
	set xs_echo_script 0
    }
}

::tclreadline::Print  yes

#           **** WARNING **** 
# Commands for system-wide customization should NOT 
# be placed here anymore.  Instead, they should go into
# the global_customize.tcl file.
