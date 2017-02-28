proc rescalecov { args }  {
# This rescales the entire covariance matrix used in the chain proposal command

# first check if the user input a ? for help

    if {![llength $args] || [lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
        puts "Usage: rescalecov scale"
        puts "Rescales the chain proposal distribution covariance matrix by the"
	puts "factor input as scale."
        puts " "
        puts "This script is obsolete. Use chain rescale instead."
        puts " "
        puts "kaa  v1.1  01/23/15"
        return
    }

    set factor [lindex $args 0]

# now just runs the chain rescale command

    chain rescale $factor

}
