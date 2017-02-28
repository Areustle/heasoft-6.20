package provide xsl_miss 1.0

#
# procedure to set the mission
# Use the variable length of the argument
# 1. mission
# 2. instrument
# 3. datamode
proc mset {args} { 
set len [ llength $args ]
set mess ""
if { $len >= 1 } { 
    xset mission [lindex $args 0] 
    append mess "Mission    ==> [lindex $args 0] \n"
} else {
    return 
}
if { $len >= 2 } { 
    xset instrument [lindex $args 1] 
    append mess "Instrument ==> [lindex $args 1] \n"
} else {
    return $mess
}
if { $len >= 3 } { 
    xset datamode [lindex $args 2] 
    append mess "Datamode   ==> [lindex $args 2] \n"
} else {
    return $mess
}
return $mess
}

 
