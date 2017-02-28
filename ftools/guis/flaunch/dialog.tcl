# Dialog chapter
proc GetValue { string {string2 "Dialog box" }} {
    global prompt
    set f [toplevel .prompt -borderwidth 3]
    wm title .prompt $string2
    message $f.msg -text $string -aspect 1000
    entry $f.entry -textvariable prompt(result) -width 30
    set b [frame $f.buttons -bd 3]
    pack $f.msg $f.entry $f.buttons -side top -fill x
    
    button $b.ok -text OK -command {set prompt(ok) 1} \
	-underline 0
    button $b.cancel -text Cancel -command {set prompt(ok) 0} \
	-underline 0
    pack $b.ok -side left
    pack $b.cancel -side right
    
    foreach w [list $f.entry $b.ok $b.cancel] {
	bindtags $w [list .prompt [winfo class $w] $w all]
    }
    bind .prompt <Alt-o> "focus $b.ok ; break"
    bind .prompt <Alt-c> "focus $b.cancel ; break"
    bind .prompt <Alt-Key> break
    bind .prompt <Return> {set prompt(ok) 1}
    bind .prompt <Control-c> {set prompt(ok) 0}
    
    focus $f.entry
    grab $f
    tkwait variable prompt(ok)
    grab release $f
    destroy $f
    if {$prompt(ok)} {
	return $prompt(result)
    } else {
	return {}
    }
}
