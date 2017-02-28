## edit - opens a file/proc/var for reading/editing
## 
# Arguments:
#   type	proc/file/var
#   what	the actual name of the item
# Returns:	nothing
## 
;proc edit {args} {
    global XSCON

    array set opts {-find {} -type {} -attach {}}
    while {[string match -* [lindex $args 0]]} {
	switch -glob -- [lindex $args 0] {
	    -f*	{ set opts(-find) [lindex $args 1] }
	    -a*	{ set opts(-attach) [lindex $args 1] }
	    -t*	{ set opts(-type) [lindex $args 1] }
	    --	{ set args [lreplace $args 0 0]; break }
	    default {return -code error "unknown option \"[lindex $args 0]\""}
	}
	set args [lreplace $args 0 1]
    }
    # determine who we are dealing with
    if {[llength $opts(-attach)]} {
	foreach {app type} $opts(-attach) {break}
    } else {
	foreach {app type} [tkcon attach] {break}
    }

    set word [lindex $args 0]
    if {[string match {} $opts(-type)]} {
	if {[llength [tkConEvalOther $app $type info commands [list $word]]]} {
	    set opts(-type) "proc"
	} elseif {[llength [tkConEvalOther $app $type info vars [list $word]]]} {
	    set opts(-type) "var"
	} elseif {[tkConEvalOther $app $type file isfile [list $word]]} {
	    set opts(-type) "file"
	}
    }
    if {[string compare $opts(-type) {}]} {
	# Create unique edit window toplevel
	set w $XSCON(base).__edit
	set i 0
	while {[winfo exists $w[incr i]]} {}
	append w $i
	toplevel $w
	wm withdraw $w
	if {[string length $word] > 12} {
	    wm title $w "TkCon Edit: [string range $word 0 9]..."
	} else {
	    wm title $w "TkCon Edit: $word"
	}

	text $w.text -wrap none \
		-xscrollcommand [list $w.sx set] \
		-yscrollcommand [list $w.sy set] \
		-foreground $XSCON(color,stdin) \
		-background $XSCON(color,bg) \
		-insertbackground $XSCON(color,cursor) \
		-font $XSCON(font)
	scrollbar $w.sx -orient h -takefocus 0 -bd 1 \
		-command [list $w.text xview]
	scrollbar $w.sy -orient v -takefocus 0 -bd 1 \
		-command [list $w.text yview]

	if {[info tclversion] >= 8.0} {
	    set menu [menu $w.mbar]
	    $w configure -menu $menu
	} else {
	    set menu [frame $w.mbar -relief raised -bd 1]
	    grid $menu - - -sticky news
	}

	## File Menu
	##
	set m [menu [tkConMenuButton $menu File file]]
	$m add command -label "Save As..."  -underline 0 \
		-command [list tkConSave {} widget $w.text]
	$m add command -label "Append To..."  -underline 0 \
		-command [list tkConSave {} widget $w.text a+]
	$m add separator
	$m add command -label "Dismiss" -underline 0 -accel "Ctrl-w" \
		-command [list destroy $w]
	bind $w <Control-w>		[list destroy $w]
	bind $w <$XSCON(meta)-w>	[list destroy $w]

	## Edit Menu
	##
	set text $w.text
	set m [menu [tkConMenuButton $menu Edit edit]]
	$m add command -label "Cut"   -under 2 -command [list tkConCut $text]
	$m add command -label "Copy"  -under 0 -command [list tkConCopy $text]
	$m add command -label "Paste" -under 0 -command [list tkConPaste $text]
	$m add separator
	$m add command -label "Find" -under 0 \
		-command [list FindBox $text]

	## Send To Menu
	##
	#set m [menu [tkConMenuButton $menu "Send To..." send]]
	#$m add command -label "Send To $app" -underline 0 \
	#	-command "tkConEvalOther [list $app] $type \
	#	eval \[$w.text get 1.0 end-1c\]"
	#set other [tkcon attach]
	#if {[string compare $other [list $app $type]]} {
	#    $m add command -label "Send To [lindex $other 0]" \
	#	    -command "tkConEvalOther $other \
	#	    eval \[$w.text get 1.0 end-1c\]"
	#}

	grid $w.text - $w.sy -sticky news
	grid $w.sx - -sticky ew
	grid columnconfigure $w 0 -weight 1
	grid columnconfigure $w 1 -weight 1
	grid rowconfigure $w 0 -weight 1
    } else {
	return -code error "unrecognized type '$word'"
    }
    switch -glob -- $opts(-type) {
	proc*	{
	    $w.text insert 1.0 [tkConEvalOther $app $type dump proc [list $word]]
	}
	var*	{
	    $w.text insert 1.0 [tkConEvalOther $app $type dump var [list $word]]
	}
	file	{
	    $w.text insert 1.0 [tkConEvalOther $app $type eval \
		    [subst -nocommands {set __tkcon(fid) [open $word r]
	    set __tkcon(data) [read \$__tkcon(fid)]
	    close \$__tkcon(fid)
	    after 2000 unset __tkcon
	    return \$__tkcon(data)}]]
	}
	error*	{
	    $w.text insert 1.0 [join $args \n]
	    tkConErrorHighlight $w.text
	}
	default	{
	    $w.text insert 1.0 [join $args \n]
	}
    }
    wm deiconify $w
    focus $w.text
    if {[string compare $opts(-find) {}]} {
	tkConFind $w.text $opts(-find) -case 1
    }
}



## tkConFind - searches in text widget $w for $str and highlights it
## If $str is empty, it just deletes any highlighting
# ARGS: w	- text widget
#	str	- string to search for
#	-case	TCL_BOOLEAN	whether to be case sensitive	DEFAULT: 0
#	-regexp	TCL_BOOLEAN	whether to use $str as pattern	DEFAULT: 0
##
;proc tkConFind {w str args} {
    $w tag remove find 1.0 end
    set truth {^(1|yes|true|on)$}
    set opts  {}
    foreach {key val} $args {
	switch -glob -- $key {
	    -c* { if {[regexp -nocase $truth $val]} { set case 1 } }
	    -r* { if {[regexp -nocase $truth $val]} { lappend opts -regexp } }
	    default { return -code error "Unknown option $key" }
	}
    }
    if {![info exists case]} { lappend opts -nocase }
    if {[string match {} $str]} return
    $w mark set findmark 1.0
    while {[string compare {} [set ix [eval $w search $opts -count numc -- \
	    [list $str] findmark end]]]} {
	$w tag add find $ix ${ix}+${numc}c
	$w mark set findmark ${ix}+1c
    }
    global XSCON
    $w tag configure find -background $XSCON(color,blink)
    catch {$w see find.first}
    return [expr {[llength [$w tag ranges find]]/2}]
}
