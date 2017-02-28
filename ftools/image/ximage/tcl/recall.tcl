#
#  Provide functionality similar to old RECALL command
#
proc recall args {

    global xm_tcl_prompt htyshow

    parseparm [parmloc recall] $args

    set shownum $htyshow

    if { $parval(max) != "" } {
       set shownum $parval(max)
    } elseif { $cmdargc == 1 && [regexp {^[0-9]+$} $cmdargv] } {
       txwrite "!$xm_tcl_prompt [history event $cmdargv]" 10
       return [eval history redo $cmdargv]
    } 
    set hlist [split [history] "\n"]
    set ibeg [expr [llength $hlist] - $shownum]
    foreach evt [lrange $hlist $ibeg end] {
       puts "$evt"
    }
}
