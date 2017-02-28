
proc grab_output {ohandle lhandle twidget} {
#This is a generic fileevent callback.  It reads from $ohandle and 
#writes the output to channel $lhandle and a text widget $twidget
    if {[gets $ohandle line] >= 0} {
	$twidget insert end "$line\n"
	puts $lhandle "$line"
    } else {
	close $ohandle
	close $lhandle
	exit
    }
}

proc bgerror {error} {
    global taskname lhandle ohandle twidget
    
    $twidget insert end $error
    puts $lhandle $error
    $twidget configure -bg pink

    wm deiconify .

#depending on the error, the ftool process could concievably not have finished
#but it probably has, so catch the following
    catch {close $ohandle}

#close log file
    close $lhandle
    
    button .cancel -text "Exit" -command "exit"
    grid configure .cancel -column 0 -row 1
    
    return
    
}

    
    
    
    
    

set TCLTK_LIBRARY $env(TCLTK_LIBRARY)
set FITSLAUNCHER_LIBRARY $env(FITSLAUNCHER_LIBRARY)


set len [ llength $argv ]
if {$len != 5} {
puts "There are $len elements in your command."
puts "TKTERM requires 5 parameters to function."
return
}

set window [lindex $argv 0]
set command_string [lindex $argv 1]
set log_file [lindex $argv 2]
set interactive [lindex $argv 3]
set taskname [lindex $argv 4]
set twidget .logtext

set win_name1 [winfo name .]

wm title . $log_file

text $twidget
grid configure  $twidget -row 0 -column 0 -sticky news


set ohandle [open "|$command_string"]
set lhandle [open $log_file w]

fileevent $ohandle readable "grab_output $ohandle $lhandle .logtext"

#tkterm $window $command_string $log_file $pause_window


set win_name [winfo name .]

if { $interactive == "no" } {
    wm iconify .
}

return
