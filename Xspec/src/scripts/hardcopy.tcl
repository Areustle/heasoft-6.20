proc hardcopy { {fName ""}  {setcol "mono"} } {
    global xs_return_result save_device chatter
    
    if { [string match $setcol "?"] } {
        puts "Syntax: hardcopy   \[filename\]  \[mono|color\]"
        puts "\tUsing the defaults (no arguments)  produces a monochrome landscape plot."
        puts "\te.g. hardcopy dataplot.ps color will produce a color plot saved in"
        puts "\tthe file \"dataplot.ps\". If the filename is specified the postscript"
        puts "\tfile will be saved."
        return
    }
    
    
# cpd returns the current plot device in its result string.

# Turn on the Tcl results, storing old value for later.

    set old_return $xs_return_result
    set xs_return_result 1

# Use setplot command to make a postscript file.
    
    set deviceName $fName
    if { [string match $setcol "mono"] || ![string match $setcol "color"] } {
          append deviceName "/ps" 
    } else {
           if { [string match $setcol "color"] }  {
                append deviceName "/cps"
           }
    }

    set comm_number [setplot command hardcopy $deviceName]
    plot
    setplot delete $comm_number

# Print and then delete the postscript file.
    if { ![string length $fName] } {
	exec lpr pgplot.ps
        exec rm pgplot.ps
    } else {
	exec lpr $fName
    }

    set xs_return_result $old_return
    return
}
