#
#  Primary system tcl initialization (sourced from ximage.c)
#  Sources $XANADU/image/ximage/tcl/*.tcl
#  Then, ~/.ximagerc
#  Then, ~/.ximage/*.tcl
#
set xm_tcl_prompt {[XIMAGE> }

#
#  Save/restore mode (On by default)
#  If true, copies first read into save position and allows
#    save,restore,etc.  Otherwise, save, restore, etc. disabled
#  Set saved map to be last map
#
set savmode 1
set savmap MAP$maxmaps
#
#  Default number of colors to allocate and size for /xtk device
#  Widget should be destroyed on next close if changed
#  Use trace to inform close operation of change
#
set xtkcols 255
set xtkwid 760
set xtkhgt 576
trace variable xtkcols w pgtk::tracetrigger
trace variable xtkwid w pgtk::tracetrigger
trace variable xtkhgt w pgtk::tracetrigger
#
#  Failsafe for default array if there is trouble with par file
#
if { ![info exists default] } {
   set default(equinox) 2000
   set default(numlevs) 16
   set default(device) "/xtk"
   set default(svra) ""
   set default(svdec) ""
   set default(svequinox) ""
}

proc alias {newname existname} {
   interp alias {} $newname {} $existname
}

#
#  Define exit command which cleans up (end pgplot, write defaults
#  to parameter file) and records history before exiting
#
proc exit {{code 0}} {
   global ximage_history_file
   xan::cleanup
   xan::htywrite $ximage_history_file
   log none
   tclexit $code
}

if { [info exists tk_version] } {
#
#  Hide root window (until needed)
#
   if [catch {tk::wm withdraw .} errmsg] {
      puts "Tk Error: $errmsg"
   }
#
#  Hide all commands beginning tk in tk:: namespace
#
   foreach cmd [info commands tk*] {
      rename $cmd tk::$cmd
   }
   unset cmd
#
#  Add aliases for some required tk commands
#  (Tk commands are renamed to tk:: in ximage.c)
#

alias bind tk::bind                    ;# Necessary
alias bindtags tk::bindtags            ;# Necessary
#alias button tk::button                ;# Not interfering
alias event tk::event                  ;# Necessary
alias focus tk::focus                  ;# Necessary
#alias frame tk::frame                  ;# Not interfering
alias grab tk::grab                    ;# Necessary
#alias grid tk::grid                   ;# Conflict with grid
#alias label tk::label                 ;# Conflict with label
alias menu tk::menu                    ;# Necessary
#alias option tk::option                ;# Not interfering
#alias pack tk::pack                    ;# Not interfering
#alias radiobutton tk::radiobutton     ;# Conflict with ra_dec_to_pix
#alias raise tk::raise                 ;# Conflict with ra_dec_to_pix
#alias scale tk::scale                 ;# Conflict with scale
#alias tkwait tk::tkwait                ;# Not interfering
alias toplevel tk::toplevel            ;# Not interfering
alias winfo tk::winfo                  ;# Necessary
alias wm tk::wm                        ;# Necessary

}
#
# Force user to use exec when running shell commands
# Prevents collision between ximage and shell commands
#
set auto_noexec 1
#
# Set directory for user addons
#
if { [info exists env(XIMAGE_HOME)] } {
   set ximage_home "$env(XIMAGE_HOME)"
} else {
   set ximage_home "$env(HOME)/.ximage"
}
#
# Set location of special ximage files
#
   set ximage_history_file "~/ximage.hty"
#
#  Aliases
#
alias rea read_image
alias rem remove_sources
alias polygon value
alias quit exit
alias bye exit
alias equinox cey
alias con contour
alias cont contour
alias cl close_pg_window
alias fi finding_chart
alias g grid

#
#  Now that all standard ximage aliases have been defined,
#  redefine alias command to something more useful to the
#  user, allowing arguments and multiple commands
#
proc alias {newname existname} {
   proc $newname {args} "eval $existname \$args"
}

proc syscall args { 
#
# Make a `system' command for sending things to the shell.
#
    global spawn_args env
    if { [llength $args] == 0 } {
        return [uplevel #0 exec >&@stdout <@stdin $env(SHELL) ]
    } else {
        set spawn_args $args
        set cmd "exec >&@stdout <@stdin $env(SHELL) -c {[join $spawn_args]}"
        return [uplevel #0 $cmd] 
    }
}

proc parmloc {cmdname} {
#
#  Find parameter file given command name
#  If already a parameter file just return it
#
   global ximage_home env

   if { [regexp {\.par$} $cmdname] } {
      set parfile $cmdname
   } else {
      set parfile "$ximage_home/$cmdname.par"
      if { ![file exists $parfile] } {
         set parfile "$env(XANADU)/image/ximage/pfiles/$cmdname.par"
      }
      if { ![file exists $parfile] } {
         txwrite "Could not find parameter file for $cmdname" 5
         set parfile ""
      }
   }
   return $parfile
}

proc parmhelp {command} {
#
#  Print help on parameters
#
   set parmlist "";
   set parfile [parmloc $command]
   if { [file exists $parfile] } {
       set fp [open $parfile r]
       set contents [split [read $fp] \n]
       close $fp

       foreach line $contents {
          if { [string index $line 0] != "#" } {
             set parsedlist [split $line ,]
             append parmlist "[lindex $parsedlist 0] "
          }
       }
    }
    txwrite " " 10
    txwrite " Allowed qualifiers are:" 10
    txwrite " " 10
    xan::prlist $parmlist
    txwrite " " 10
}

#
#  Source everything in tcl directory (Tcl-based commands)
#
foreach match [glob -nocomplain --  "$env(XANADU)/image/ximage/tcl/*.tcl"] {
   if {[catch {source $match} result]} {
      txwrite "Error in: $match" 5
   }
}
#
#  Source user's startup file
#
if [file exists ~/.ximagerc] { source ~/.ximagerc }
#
#  Source everything in user's ximage home directory (Tcl-based commands)
#
foreach match [glob -nocomplain --  "$ximage_home/*.tcl"] {
   if {[catch {source $match} result]} {
      txwrite "Error in user script: $match" 5
   }
}
unset match
#
#  Set maximum history to keep and show, then read in history file
#
history keep 200
set htyshow 20
xan::htyread $ximage_history_file
#
#  Execute commands given as arguments to ximage
#
if { $argc > 1 } {
   set startcmd [lreplace $argv 0 0]
   if { [catch "eval $startcmd" result] } {
      txwrite " Failed to execute startup command: $startcmd" 5
      if [info exists exit_on_startfail] { exit $exit_on_startfail }
   } 
   unset startcmd
   unset result
}

#
#  Error proc for Tk
#
proc bgerror {msg} {

   if { $msg == {} } {
      txwrite "Blank bgerror message" 30
      return
   }

   if { [pgtk::tkexists] } {
      pgtk::askdialog $msg err OK 0
   } else {
      txwrite "bgerror: $msg" 5
   }
}
