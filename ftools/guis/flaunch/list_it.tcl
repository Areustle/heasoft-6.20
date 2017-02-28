
# The following are generic filebuilding procedures
#----------------------------------------------------------------------
# Listbox chapter
proc ScrolledListbox { parent args } {

	# Create listbox attached to scrollbars, pass thru $args
	frame $parent
	eval {listbox $parent.list \
		-yscrollcommand [list $parent.sy set] \
		-xscrollcommand [list $parent.sx set]} $args

	# Create scrollbars attached to the listbox
	scrollbar $parent.sx -orient horizontal \
	    -background azure1\
	    -activebackground lightpink -activerelief groove \
	    -troughcolor azure2 \
	    -command [list $parent.list xview]
	scrollbar $parent.sy -orient vertical \
	    -background azure1\
	    -activebackground lightpink -activerelief groove \
	    -troughcolor azure2 \
	    -command [list $parent.list yview]

	# Arrange them in the parent frame
	pack $parent.sx -side bottom -fill x
	pack $parent.sy -side right -fill y
	pack $parent.list -side left -fill both -expand true
}

# Listbox chapter
proc ScrolledListbox2 { parent args } {
	frame $parent

	# Create listbox attached to scrollbars, pass thru $args
	eval {listbox $parent.list \
		-yscrollcommand [list $parent.sy set] \
		-xscrollcommand [list $parent.sx set]} $args
	scrollbar $parent.sy -orient vertical \
	    -background azure1\
	    -activebackground lightpink -activerelief groove \
	    -troughcolor azure2 \
	    -command [list $parent.list yview]

	# Create extra frame to hold pad and horizontal scrollbar
	frame $parent.bottom
	scrollbar $parent.sx -orient horizontal \
	    -background azure1\
	    -activebackground lightpink -activerelief groove \
	    -troughcolor azure2 \
	    -command [list $parent.list xview]

	# Create padding based on the scrollbar width and border
	set pad [expr [$parent.sy cget -width] + 2* \
		([$parent.sy cget -bd] + \
		 [$parent.sy cget -highlightthickness])]
	frame $parent.pad -width $pad -height $pad

	# Arrange everything in the parent frame
	pack $parent.bottom -side bottom -fill x
	pack $parent.pad -in $parent.bottom -side right
	pack $parent.sx -in $parent.bottom -side bottom -fill x
	pack $parent.sy -side right -fill y
	pack $parent.list -side left -fill both -expand true
	return $parent.list
}
if 0 {
	frame $parent
	eval {listbox $parent.list \
		-yscrollcommand [list $parent.sy set] \
		-xscrollcommand [list $parent.pad.sx set]} $args
	scrollbar $parent.sy -orient vertical \
		-command [list $parent.list yview]

	# Create extra frame to hold pad and horizontal scrollbar
	frame $parent.pad
	scrollbar $parent.pad.sx -orient horizontal \
		-command [list $parent.list xview]

	# Create padding based on the scrollbar's width
	set pad [expr [$parent.sy cget -width] + \
	    2*([$parent.sy cget -bd] + [$parent.sy cget -highlightthickness])]
	frame $parent.pad.it -width $pad -height $pad

	# Arrange everything in the parent frame
	pack $parent.pad -side bottom -fill x
	pack $parent.pad.it -side right
	pack $parent.pad.sx -side bottom -fill x
	pack $parent.sy -side right -fill y
	pack $parent.list -side left -fill both -expand true
}

proc ListSelect { host lb parent {type "filename" } \
		      {destroy "all"} \
		      {prepend_val No_global_variable_set} \
		      {append_val No_global_variable_set} } {

    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue

    # Create list to be written to a file.
    if {[winfo exists $parent] == 1 } {
	raise $parent
	focus $parent
	return
    }

    toplevel $parent -bg ivory 
    wm title $parent "List of Selected Items"
    frame $parent.menubar

    set file [menubutton $parent.file -text "File" \
		  -activeforeground $blue1 -activebackground $ivory \
		  -borderwidth 5 -relief raised -bg $blue1 -fg $ivory \
		  -height 1 -underline 0 \
		  -menu $parent.file.menu]

    set file_but [menu $parent.file.menu -tearoff 1 \
		      -activebackground $ivory \
		      -activeforeground $blue1 \
		      -borderwidth 2 \
		      -relief raised -bg $blue1 \
		      -fg $ivory ]
    $file_but add command -label "Save list to File?       " \
	-underline 0 -accelerator "Ctrl+s"\
	-command "ListSelect_Save $type $parent.picked.list ask $destroy $host $parent"
    bind $parent <Control-s> "ListSelect_Save $type $parent.picked.list ask $destroy $host $parent"

    $file_but add separator

    $file_but add command -label "Cancel?" \
	-underline 0 -accelerator "Ctrl+c"\
	-command "destroy $parent"
    bind $parent <Control-c> "destroy $parent"

    set help [menubutton $parent.help -text "Help" \
		  -activeforeground $blue1 -activebackground $ivory \
		  -borderwidth 5 -relief raised -bg $blue1 -fg $ivory \
		  -height 1 -underline 0 \
		  -menu $parent.help.menu]

    set help_but [menu $parent.help.menu -tearoff 1 \
		      -activebackground $ivory \
		      -activeforeground $blue1 \
		      -borderwidth 2 \
		      -relief raised -bg $blue1 \
		      -fg $ivory ]

    $help_but add command -label "About making a list   " \
	-accelerator "Ctrl+h"\
	-command "delay_hhelp $parent Making_a_list"
    bind $parent <Control-h> "delay_hhelp $parent Making_a_list "

    $help_but add command -label "All Topics available" \
	-underline 0 -accelerator "Ctrl+a"\
	-command "delay_hhelp $parent Topic_list"
    bind $parent <Control-a> "delay_hhelp $parent Topic_list"

    ScrolledListbox2 $parent.picked -width 40 -height 10 \
	-background ivory -foreground black \
	-setgrid 1

    pack $parent.file -in $parent.menubar \
	-side left -padx 0 -pady 0
    pack $parent.help -in $parent.menubar \
	-side right -padx 0 -pady 0

    pack $parent.menubar $parent.picked -side top \
	-expand true -fill both

    # Selecting in choice moves items into picked
    bind $lb <ButtonPress-3> \
	{ListSelectStart %W %y}
    bind $lb <B3-Motion> \
	{ListSelectExtend %W %y}
    bind $lb <ButtonRelease-3> \
        [list ListSelectEnd %W %y $parent.picked.list $lb \
	     $prepend_val $append_val ] 

    # Selecting in picked deletes items
    bind $parent.picked.list <ButtonPress-1> \
	{ListSelectStart %W %y}
    bind $parent.picked.list <B1-Motion> \
	{ListSelectExtend %W %y}
    bind $parent.picked.list <ButtonRelease-1> \
	{ListDeleteEnd %W %y}

    return 

}

proc ListSelectStart { w y } {
    $w select anchor [$w nearest $y]
    $w select set anchor [$w nearest $y]
}

proc ListSelectExtend { w y } {
    $w select set anchor [$w nearest $y]
}

proc ListSelectEnd {w y list lb prepend_val append_val} {

    # Here we read in the value associated with the global variable name
    # which is stored in the variable prepend_val, since this is the 
    # information that is to be prepended to the string read out of
    # the associated listbox. If they are set to their default value
    # of "No_global_variable_set"
    if ![ string match "No_global_variable_set" $prepend_val ] {
	upvar \#0 $prepend_val string_beg
    }

    if ![ string match "No_global_variable_set" $append_val ] {
	upvar \#0 $append_val string_end
    }

    $w select set anchor [$w nearest $y]

    foreach i [$w curselection] {
	set total_file [$w get $i]

	if [info exists string_beg] {
	    set total_file $string_beg$total_file
	}

	if [info exists string_end] {
	    set total_file $total_file$string_end
	}

	if { [ regsub {//} $total_file {/} value ] != 0} {
	    set total_file $value
	}
	$list insert end $total_file
    }
    selection clear $lb
}

proc ListDeleteEnd {w y} {
    $w select set anchor [$w nearest $y]
    foreach i [lsort -decreasing [$w curselection]] {
	$w delete $i
    }
}

#----------------------------------------------------------------------
# This procedure takes as input a name of a list box and will 
# prompt the user (via a Dialog box) for the name of the file to
# create. Once this is done the procedure will move through the 
# specified list box saving each row to the specified file. 
proc ListSelect_Save { {type "row"} { parent_list "none" } \
			   {dialog "ask"} {destroy "all"} {parent "none"} {host "none"} } {

    # type can take on the values of "row", "script", "filename", 
    #      or "task" and 
    #      will based upon the type of list selected perform different
    #      actions, e.g., if type=filename, then the code will atttempt 
    #      to uncompress, unzip, untar, etc. the selected files. 

    global fileselect append_at FLAUNCH_CONFIG

    set type [ string trim $type ]

    if [string match "none" $parent_list] {
	return 
    } else {

	set get_list [ $parent_list get 0 end ]
	set list_length [ llength $get_list ]
	if { $list_length <= 0 } {
	    return
	}

	set suffix ".fil"

	if [ string match "filename" $type ] {
	    set output_file [GetValue \
			 "Root-filename of the file to create?" \
			 "Input Filename" ]
	    set name_length [ llength $output_file ]
	    if { $name_length <= 0 } {
		return
	    }

	} elseif [ string match "task" $type ] {	    

	    set output_file "$FLAUNCH_CONFIG/custom"
	    set suffix "_tasks"

	} elseif [ string match "row" $type ] {
	    
	    set suffix ".col"
	} elseif [ string match "script" $type ] {

	    set suffix ".tcl_script"
	} else {
	    
	}	

	set output_file [string trim $output_file]
	set output_file "$output_file$suffix"
	set output_file [string trim $output_file]

	# Since Tcl does strange things if the file all ready exists
	# and you are essentially just overwriting the file, 
	# let's delete it if it is there, and then recreate it. 
	# This should always work properly.
	catch { exec rm $output_file }

	# Check to see if the configuration file directory 
	# actually exists. If it doesn't than create it since we
	# will be writing files to this directory
	if { [file isdirectory $FLAUNCH_CONFIG] != 1 } {
	    catch { exec mkdir $FLAUNCH_CONFIG }
	    if { [file isdirectory $FLAUNCH_CONFIG] != 1 } {
		tkerror "Cannot create FLAUNCH configuration directory.\nExecute a chmod command and try this again.\n"
		return
	    }
	}
	
	# Let's open the file for writing. 
	set fileId [ open $output_file {RDWR CREAT}]

	foreach i $get_list {

	    if [ string match "filename" $type ] {
		# If the list is a list of files, let's see if they
		# have to be uncompressed. 
		set tempfile [type_and_uncompress_file $i]

	    } elseif [ string match "row" $type ] {
		set tempfile $i

	    } elseif [ string match "task" $type ] {
		set tempfile $i

	    } elseif [ string match "script" $type ] {
		set tempfile $i
		
	    } else {
		set tempfile $i
	    }

	    puts $fileId $tempfile

	}
	
	close $fileId

    }

    set append_at 1

    set fileselect(dir) "."
    set fileselect(path) "$output_file"

    if [ string match "first" $destroy ] {
	if [winfo exists $parent] {
	    destroy $parent
	}
    }

    if [ string match "second" $destroy ] {
	if [winfo exists $host] {
	    destroy $host
	}
    }

    if [ string match "first" $destroy ] {
	if [winfo exists $parent] {
	    destroy $parent
	}

	if [winfo exists $host] {
	    destroy $host
	}
    }
    
    return

}
