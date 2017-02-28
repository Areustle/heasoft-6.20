set TCLTK_LIBRARY $env(TCLTK_LIBRARY)
set FITSLAUNCHER_LIBRARY $env(FITSLAUNCHER_LIBRARY)
set EXPECTDIR $env(EXPECTDIR)

source $FITSLAUNCHER_LIBRARY/dialog.tcl
source $FITSLAUNCHER_LIBRARY/list_it.tcl
source $FITSLAUNCHER_LIBRARY/compress_tools.tcl
source $FITSLAUNCHER_LIBRARY/tkterm.tcl
source $FITSLAUNCHER_LIBRARY/ftools_query.tcl

# The following are for general fileselection information.
#----------------------------------------------------------------------
proc fileselectResources {} {
  # path is used to enter the file name
    option add *Fileselect*path.relief		sunken	startup
    option add *Fileselect*path.background	ivory	startup
    option add *Fileselect*path.foreground	black	startup

    # Text for the label on pathname entry
    option add *Fileselect*l.text		File:	startup
    option add *Fileselect*l.background	        ivory	startup

    # Text for the OK and Cancel buttons
    option add *Fileselect*ok*text		Ok	startup
    option add *Fileselect*ok*underline		0	startup
    option add *Fileselect*cancel.text		Cancel	startup
    option add *Fileselect*cancel.underline 	0	startup
    option add *Fileselect*fv.text              Fv      startup
    option add *Fileselect*fv.underline 	0	startup
    option add *Fileselect*copy.text            "Make list"    startup
    option add *Fileselect*copy.underline 	0	startup
    option add *Fileselect*help*text		Help	startup
    option add *Fileselect*help*underline	0	startup


    option add *Fileselect*.background	        ivory	startup

    # Size of the listbox
    option add *Fileselect*list.width		30	startup
    option add *Fileselect*list.height		10	startup
    option add *Fileselect*list.background	ivory	startup
}

#----------------------------------------------------------------------
# fileselect returns the selected file, or {}
proc fileselect { {why "File Selection Window"} {default {}} {mustExist 1} } {
    global fileselect
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue
    global fileselect_uncompress_setup dir_only

    # Since the File Selection window and Directory Selection window
    # are mutually exclusive, we delete the Directory Selection Window 
    # before proceeding.
    if { [winfo exists .fileselect] == 1 } {
	if [info exists dir_only] {
	    destroy .fileselect
	    tkwait window .fileselect
	} else {
	    raise .fileselect
	    focus .fileselect
	    return
	}
    }

    set t [toplevel .fileselect -bg ivory -bd 4 -class Fileselect]
    fileselectResources
    wm title .fileselect $why 
    $t config -bg ivory

    frame $t.buttons -bg ivory
    pack $t.buttons -side top -fill both


    # Create a read-only entry for the current directory
    set fileselect(dirEnt) [entry $t.dir -width 15 \
				-bg ivory -fg black \
				-relief flat -state disabled]
    pack $t.dir -side top -fill x
    
    # Create an entry for the pathname
    # The value is kept in fileselect(path)
    frame $t.top
    label $t.top.l -padx 0 -bg ivory
    set e [entry $t.top.path \
	       -bg ivory -fg black \
	       -textvariable fileselect(path)]
    pack $t.top -side top -fill x
    pack $t.top.l -side left
    pack $t.top.path -side right -fill x -expand true

    # Create a listbox to hold the directory contents
    set lb [listbox $t.list \
		-yscrollcommand [list $t.scroll set]]
    scrollbar $t.scroll \
	-background azure1\
	-activebackground lightpink -activerelief groove \
	-troughcolor azure2 \
    -command [list $lb yview]

    set file [menubutton $t.buttons.file -text "File" \
		  -activeforeground $blue1 -activebackground $ivory \
		  -borderwidth 5 -relief raised -bg $blue1 -fg $ivory \
		  -height 1 -underline 0 \
		  -menu $t.buttons.file.menu]

    set file_but [menu $t.buttons.file.menu -tearoff 1 \
		      -activebackground $ivory \
		      -activeforeground $blue1 \
		      -borderwidth 2 \
		      -relief raised -bg $blue1 \
		      -fg $ivory ]

    if ![info exists dir_only] {
	$file_but add command -label "Make a list of files?       " \
	    -underline 0 -accelerator "Ctrl+m"\
	    -command "ListSelect .fileselect $lb .copy  {directory} ; \
                      tkwait window .fileselect ; fileselectOK"
	bind $t <Control-m> "ListSelect .fileselect $lb .copy {directory}; \
                      tkwait window .fileselect ; fileselectOK"

	$file_but add separator
	
    }

    $file_but add command -label "Cancel?" \
	-underline 0 -accelerator "Ctrl+c"\
	-command {fileselectCancel}
    bind $t <Control-c> {fileselectCancel}

    if ![info exists dir_only] {
	set ok [button $t.buttons.ok \
		    -bg yellow -fg black \
		    -borderwidth 5 \
		    -padx 3 -pady 3 \
		    -relief raised -activebackground forestgreen \
		    -activeforeground white \
		    -command fileselectOK]
	bind $t <Control-o> {fileselectOK}
    } else {
	set ok [button $t.buttons.ok \
		    -bg yellow -fg black \
		    -borderwidth 5 \
		    -padx 3 -pady 3 \
		    -relief raised -activebackground forestgreen \
		    -activeforeground white \
		    -command dirselectOK]
	bind $t <Control-o> {dirselectOK}
    }

    set help [menubutton $t.buttons.help -text "Help" \
		  -activeforeground $blue1 -activebackground $ivory \
		  -borderwidth 5 -relief raised -bg $blue1 -fg $ivory \
		  -height 1 -underline 0 \
		  -menu $t.buttons.help.menu]

    set help_but [menu $t.buttons.help.menu -tearoff 1 \
		      -activebackground $ivory \
		      -activeforeground $blue1 \
		      -borderwidth 2 \
		      -relief raised -bg $blue1 \
		      -fg $ivory ]

    if ![info exists dir_only] {
	$help_but add command -label "About File selection   " \
	    -accelerator "Ctrl+h"\
	    -command "delay_hhelp $lb File_selection"
	bind $t <Control-h> "delay_hhelp .fileselect File_selection "
    } else {
	$help_but add command -label "About Directory selection   " \
	    -accelerator "Ctrl+h"\
	    -command "delay_hhelp $lb Directory_selection"
	bind $t <Control-h> "delay_hhelp .fileselect Directory_selection "
    }

    $help_but add command -label "All Topics available" \
	-underline 0 -accelerator "Ctrl+a"\
	-command "delay_hhelp $lb Topic_list"
    bind $t <Control-a> "delay_hhelp $lb Topic_list"

    if ![info exists dir_only] {
    # Create the Fv Launch button
	set fv [button $t.buttons.fv \
		    -bg turquoise -fg black \
		    -borderwidth 5 \
		    -padx 3 -pady 3 \
		    -relief raised -activebackground white \
		    -activeforeground blue \
		    -command fileselect_fv ]
	bind $t <Control-f> {fileselect_fv}
    }

    pack $t.list -side left -fill both -expand true
    pack $t.scroll -side left -fill y

    if ![info exists dir_only] {
	pack $t.buttons.file $t.buttons.fv $t.buttons.ok \
	    -side left -padx 0 -pady 0
	FindFont $t.buttons.fv 14 bold
    } else {
	pack $t.buttons.file $t.buttons.ok \
	    -side left -padx 0 -pady 0
    }

    pack $t.buttons.help -side right -padx 0 -pady 0
    FindFont $t.buttons.help 14 bold    

    FindFont $t.buttons.ok 14 bold
    FindFont $t.buttons.file 14 bold    

    fileselectBindings $t $e $lb 
    
    # Initialize variables and list the directory
    if {[string length $default] == 0} {
	set fileselect(path) {}
	set dir [pwd]
    } else {
	set fileselect(path) [file tail $default]
	set dir [file dirname $default]
    }
    set fileselect(dir) {}
    set fileselect(done) 0
    set fileselect(mustExist) $mustExist
    
    # Wait for the listbox to be visible so
    # we can provide feedback during the listing 
    tkwait visibility .fileselect.list
    fileselectList $dir
    
    tkwait variable fileselect(done)

    if [winfo exists $t] {
	destroy $t
    }

    return $fileselect(path)

}

proc fileselectBindings { t e lb } {

    global fileselect_double_fv_setup

    # t - toplevel
    # e - name entry
    # lb - listbox
    
    # Elimate the all binding tag because we
    # do our own focus management
    foreach w [list $e $lb ] {
	bindtags $w [list $t [winfo class $w] $w]
    }

    # Dialog-global cancel binding
    bind $t <Control-c> fileselectCancel
    
    # Entry bindings
    bind $e <Return> fileselectOK
    bind $e <space> fileselectComplete
    
    # A single click, or <space>, puts the name in the entry
    # A double-click, or <Return>, selects the name
    bind $lb <space> "fileselectTake $%W ; focus $e"
    bind $lb <Button-1> \
	"fileselectClick %W %y ; focus $e"
    bind $lb <Return> "fileselectTake %W ; fileselectOK"

    if { $fileselect_double_fv_setup == 1 } {
	bind $lb <Double-Button-1> \
	    "fileselectClick %W %y ; fileselect_fv"
    } else {
	bind $lb <Double-Button-1> \
	    "fileselectClick %W %y ; fileselectOK"
    }
    
    # Focus management.  	# <Return> or <space> selects the name.
    bind $e <Tab> "focus $lb ; $lb select set 0"
    bind $lb <Tab> "focus $e"
    
    # Button focus.  Extract the underlined letter
    # from the button label to use as the focus key.
    #foreach but [list $fv $ok ] {
#	set char [string tolower [string index  \
\#				      [$but cget -text] [$but cget -underline]]]
#	bind $t <Alt-$char> "focus $but ; break"
#    }

#    bind $fv <Tab> "focus $ok"
#    bind $ok <Tab> "focus $fv"
    
    # Set up for type in
    focus $e
}

proc fileselectList { dir {files {}} } {
    global fileselect directory fileselect_uncompress_setup
    global dir_only 

    if { [ regsub {//} $dir {/} value ] != 0} {
	set dir $value
    }

    # Since many systems have system directories at the beginning
    # of path names like /local and /tmp_mnt which really aren't 
    # part of the path, we shall strip off the first /directory and test
    # to see if what remains is actually a directory, if so then 
    # we will use that as the directory.

    if { [ string match ~* $dir ] == 0 } {
	set separate [split $dir \/ ]
	set seplen [llength $separate]

	if { $seplen > 1 } {
	    set together [join [lrange $separate 2 end] \/]
	    set together "/$together"
	    if [file isdirectory $together] {
		set dir $together
	    }
	}
    }

    set directory "$dir/"
    if { [ regsub {//} $directory {/} value ] != 0} {
	set directory $value
    }


    # Update the directory display
    set e $fileselect(dirEnt)
    $e config -state normal
    $e delete 0 end
    $e insert 0 $dir
    $e config -state disabled

    # scroll to view the tail end
    $e xview moveto 1
    
    .fileselect.list delete 0 end
    set fileselect(dir) $dir

    if ![file isdirectory $dir] {
	.fileselect.list insert 0 "Bad Directory"
	return
    }

    .fileselect.list insert 0 Listing...
    update idletasks
    .fileselect.list delete 0
    if {[string length $files] == 0} {

	# List the directory and add an
	# entry for the parent directory
	set files [glob -nocomplain $fileselect(dir)/*]
	.fileselect.list insert end ../
    }

    # Sort the directories to the front
    set dirs {}
    set others {}
    foreach f [lsort $files] {
	if [file isdirectory $f] {
	    lappend dirs [file tail $f]/
	} else {
	    if ![info exists dir_only] {
		lappend others [file tail $f]
	    }
	}
    }
    foreach f [concat $dirs $others] {
	.fileselect.list insert end $f
    }
}

proc fileselect_fv {} {
    global fileselect executed_tasks_index spawned_tasks
    global term_spawn_id
    global parameter_pause_window_setup

    set pause_window "no"

    if { $parameter_pause_window_setup == 1 } {
	set pause_window "yes"
    }
    set interactive "no"
    set task "fv" 

    set string "$fileselect(dir)/$fileselect(path)"
    
    if { [ regsub {//} $string {/} value ] != 0} {
	set string $value
    }

    if { [file isfile $string] == 1 } {
	set executed_tasks_index [spawn_task $task $string \
				      $executed_tasks_index \
				      $interactive $pause_window]
    } else {
	fileselectOK
	# error "You must first select a file."
    }
}

proc fileselectOK {} {
    global fileselect dir_only
    
    # Handle the parent directory specially
    if {[regsub {^\.\./?} $fileselect(path) {} newpath] != 0} {
	set fileselect(path) $newpath
	set fileselect(dir) [file dirname $fileselect(dir)]
	fileselectOK
	return
    }
    
    set path $fileselect(dir)/$fileselect(path)
    
    if [file isdirectory $path] {
	set fileselect(path) {}
	fileselectList $path
	return
    }

    if [file exists $path] {
	set fileselect(path) $path
	set fileselect(done) 1
	return
    }

    # Neither a file or a directory.
    # See if glob will find something
    if [catch {glob $path} files] {

	# No, perhaps the user typed a new
	# absolute pathname
	if [catch {glob $fileselect(path)} path] {

	    # Nothing good
	    if {$fileselect(mustExist)} {

		# Attempt completion
		fileselectComplete
	    } elseif [file isdirectory \
			  [file dirname $fileselect(path)]] {

		# Allow new name
		set fileselect(done) 1
	    }
	    return
	} else {

	    # OK - try again
	    set fileselect(dir) [file dirname $fileselect(path)]
	    set fileselect(path) [file tail $fileselect(path)]
	    fileselectOK
	    return
	}
    } else {

	# Ok - current directory is ok,
	# either select the file or list them.
	if {[llength [split $files]] == 1} {
	    set fileselect(path) $files
	    fileselectOK
	} else {
	    set fileselect(dir) [file dirname [lindex $files 0]]
	    fileselectList $fileselect(dir) $files
	}
    }
}

proc fileselectCancel {} {
    global fileselect
    set fileselect(done) 1
    set fileselect(path) {}
}

proc fileselectClick { lb y } {

    # Take the item the user clicked on
    global fileselect
    set fileselect(path) [$lb get [$lb nearest $y]]
}

proc fileselectTake { lb } {

    # Take the currently selected list item
    global fileselect
    set fileselect(path) [$lb get [$lb curselection]]
}

proc fileselectComplete {} {
    global fileselect
    
    # Do file name completion
    # Nuke the space that triggered this call
    set fileselect(path) [string trim $fileselect(path) \t\ ]
    
    # Figure out what directory we are looking at
    # dir is the directory
    # tail is the partial name
    if {[string match /* $fileselect(path)]} {
	set dir [file dirname $fileselect(path)]
	set tail [file tail $fileselect(path)]
    } elseif [string match ~* $fileselect(path)] {
	if [catch {file dirname $fileselect(path)} dir] {

	    return	;# Bad user
	}
	set tail [file tail $fileselect(path)]
    } else {
	set path $fileselect(dir)/$fileselect(path)
	set dir [file dirname $path]
	set tail [file tail $path]
    }

    # See what files are there
    set files [glob -nocomplain $dir/$tail*]
    if {[llength [split $files]] == 1} {

	# Matched a single file
	set fileselect(dir) $dir
	set fileselect(path) [file tail $files]
    } else {
	if {[llength [split $files]] > 1} {

	    # Find the longest common prefix
	    set l [expr [string length $tail]-1]
	    set miss 0

	    # Remember that files has absolute paths
	    set file1 [file tail [lindex $files 0]]
	    while {!$miss} {
		incr l
		if {$l == [string length $file1]} {

		    # file1 is a prefix of all others
		    break
		}
		set new [string range $file1 0 $l]
		foreach f $files {
		    if ![string match $new* [file tail $f]] {
			set miss 1
			incr l -1
			break
		    }
		}
	    }
	    set fileselect(path) [string range $file1 0 $l]
	}
	fileselectList $dir $files
    }
}

proc dirselectOK {} {
    global fileselect dir_only
    
    # Handle the parent directory specially
    if {[regsub {^\.\./?} $fileselect(path) {} newpath] != 0} {
	set fileselect(path) $newpath
	set fileselect(dir) [file dirname $fileselect(dir)]
	fileselectOK
	return
    }
    
    set path $fileselect(dir)/$fileselect(path)
    
    if [file isdirectory $path] {
	set fileselect(path) $path
	set fileselect(done) 1
	return
    }

    # Neither a file or a directory.
    # See if glob will find something
    if [catch {glob $path} files] {

	# No, perhaps the user typed a new
	# absolute pathname
	if [catch {glob $fileselect(path)} path] {

	    # Nothing good
	    if {$fileselect(mustExist)} {

		# Attempt completion
		fileselectComplete
	    } elseif [file isdirectory \
			  [file dirname $fileselect(path)]] {

		# Allow new name
		set fileselect(done) 1
	    }
	    return
	} else {

	    # OK - try again
	    set fileselect(dir) [file dirname $fileselect(path)]
	    set fileselect(path) [file tail $fileselect(path)]
	    fileselectOK
	    return
	}
    } else {

	# Ok - current directory is ok,
	# either select the file or list them.
	if {[llength [split $files]] == 1} {
	    set fileselect(path) $files
	    fileselectOK
	} else {
	    set fileselect(dir) [file dirname [lindex $files 0]]
	    fileselectList $fileselect(dir) $files
	}
    }
}