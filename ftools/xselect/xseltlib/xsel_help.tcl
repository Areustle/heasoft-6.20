#!/usr/bin/tclsh
package provide xsl_help 1.0

proc help_manual {} {
global manual hlist htitle
global env
set hlpfile  "$env(FTOOLS)/bin/txselect.hlp"
if [catch  "open $hlpfile r"  hlp ] {
    puts "errors in opening $hlpfile "
}
set manual(chap) ""
set hlist(chap) ""
while { [gets $hlp line] >=0} {
     if {[regexp {^1.*} $line ] == 1} {  
       set title "Overview"
     } elseif { [regexp {^2.*} $line ] == 1} { 
        regsub {^2} $line "" line
        set line [string trim $line ]
        set title [ split $line]
        set title [lindex $title 0]
        set title [string tolower $title]
        lappend hlist(chap) $title
        set htitle($title) $line
        set level1 $title
        set manual($title) $line\n\n
     } elseif { [regexp {^3.*} $line ] == 1} {
        regsub {^3} $line "" line
        set line [string trim $line ]
        set title [split $line]
        set title [lindex $title 0]
        set title [string tolower $title]
        set title "$level1-$title"
        set htitle($title) $line
        lappend hlist($level1) $title
        set level2 $title
        set manual($title) $line\n\n
     } elseif { [regexp {^4.*} $line ] == 1} {
        regsub {^4} $line "" line
        set line [string trim $line ]
        set c [split $line]
        set title [lindex $c 0]
        set a [string tolower $htitle($level2)]
        set a [string trim $a]
        if { [regexp $a $c] } { 
            set title [lindex $c 1]
        }
        set title [string tolower $title]
        set title "$level2-$title"
        set htitle($title) $line
        lappend hlist($level2) $title
        set manual($title) $line\n\n
    } else {
    append manual($title) "$line\n" 
    }
}
} 

proc xhelp {args} {
global manual hlist htitle
global xsl_std_channame
set demand [join $args "-"]
set demand [string tolower $demand]
puts $xsl_std_channame " "
if { [llength $args] == 0} {
    set line ""
    foreach item $hlist(chap) {
        append line "$htitle($item), "
    }
    puts $xsl_std_channame  "Available Topics: "
    puts  $xsl_std_channame $line
    return
}
if [info exists manual($demand)] {
    set title $htitle($demand)
    puts  $xsl_std_channame "          $title        \n"
    puts $xsl_std_channame $manual($demand)
    if [info exists hlist($demand) ] { 
         puts  $xsl_std_channame "Topic(s): "
         set line ""
         foreach item $hlist($demand) { 
            append line " $htitle($item),"
         }
         puts  $xsl_std_channame $line
    }
    return 
} elseif [info exists manual(commands-$demand)] {
    set title $htitle(commands-$demand)
    puts  $xsl_std_channame "          $title        \n"
    puts  $xsl_std_channame $manual(commands-$demand)
    if [info exists hlist(commands-$demand) ] { 
        puts  $xsl_std_channame "Topic(s): "
        set line ""
        foreach item $hlist(commands-$demand) { 
            append line " $htitle($item), "
        }
        puts  $xsl_std_channame "$line\n"
   }
   return 
} elseif [info exists manual(xpi-xpi-$demand)] {
    set title $htitle(xpi-xpi-$demand)
    puts  $xsl_std_channame "          $title        \n"
    puts  $xsl_std_channame $manual(xpi-xpi-$demand)
    if [info exists hlist(xpi-xpi-$demand) ] { 
         puts  $xsl_std_channame "Topic(s): "
         set line ""
         foreach item $hlist(xpi-xpi-$demand) { 
            append line " $htitle($item), "
         }
         puts  $xsl_std_channame $line
    }
    return
} else {
    puts  $xsl_std_channame "$args is not found"
    puts $xsl_std_channame  "Available Topics: "
    set line ""
    foreach item $hlist(chap) {
        append line "$htitle($item), "
    }
    puts  $xsl_std_channame $line
    return
}
}  
