#!expectk -f
#

set HOME $env(HOME)
set TCLTK_LIBRARY $env(TCLTK_LIBRARY)
set FITSLAUNCHER_LIBRARY $env(FITSLAUNCHER_LIBRARY)
set LHEASOFT $env(LHEASOFT)
set LHEA_HELP $env(LHEA_HELP)
set FLAUNCH_CONFIG $HOME/.flaunch

global LHEASOFT FITSLAUNCHER_LIBRARY HOME FLAUNCH_CONFIG
source $FITSLAUNCHER_LIBRARY/filestuff.tcl
source $FITSLAUNCHER_LIBRARY/ftools_query.tcl

set tcl_precision 17

#----------------------------------------------------------------------
# Set up some defaults so that we may add an option to change them in
# a future version. 
option add *borderWidth 0
option add *relief raised 
option add *Scrollbar.borderWidth 3
option add *background ivory
option add *foreground black
option add *activeBackground ivory
option add *selectColor blue
option add *troughColor black

#----------------------------------------------------------------------
# This procedure allows the user to change the default directory that 
# they are viewing in flaunch. A pop-up directory selection window
# is created and the user must select a directory from the directories 
# that are displayed. 
proc Change_default_directory {} {
    global dir_only

    # Since the File Selection window and Directory Selection window
    # are mutually exclusive, we delete the File Selection Window 
    # and then we will re-create it.
    if { [winfo exists .fileselect] == 1 } {
	destroy .fileselect
	tkwait window .fileselect
    }


    # The "File Selection" window can be made to function for directories.
    # This is accomplished by setting the variable "dir_only" before 
    # calling the File Selection window. 
    set dir_only 1

    # Call the "Directory Select" window - which is a special behavior
    # for the File Selection window. 
    set directory [fileselect "Directory Select" ]

    # Now that we have selected a directory lets "cd" into it.
    cd $directory

    # After chosing a directory, unset the dir-only variable so that 
    # the File Selection windoww reverts to its default behaviour. 
    unset dir_only

    # Okay, we are finished so let's return the directory, 
    # just incase the user wants to do something with it. 
    return $directory
}

#----------------------------------------------------------------------
# This procedure allows the user to choose a shell for executing non-ftool
# commands. This allows the user to specify C-shell, or any other and 
# have his .file read and any aliases that he has defined will be valid 
# within flaunch. 
proc choose_a_shell { {action {}} } {
    global FLAUNCH_CONFIG
    global flaunch_shell_setup shell
  

    if { $action != "force"} {

	# If we turn off the use of a shell, then go back
	# to the default, and use the Bourne shell. 
	if { $flaunch_shell_setup == 0 } {
	    set shell sh
	    return
	}

	# If we turn on the use of a shell, and a shell choice
	# file exists than read that and return.
	if [file exists $FLAUNCH_CONFIG/shell_choice] {
	    set shell [read_file $FLAUNCH_CONFIG/shell_choice ]
	    return
	}
    }

    # Check to see if the configuration file directory 
    # actually exists. If it doesn't than create it since we
    # will be writing files to this directory
    if { [file isdirectory $FLAUNCH_CONFIG] != 1 } {
	catch { exec mkdir $FLAUNCH_CONFIG }
	if { [file isdirectory $FLAUNCH_CONFIG] != 1 } {
	    tkerror "Cannot create FLAUNCH configuration directory. Execute a chmod command and try this again.\n"
	    return
	}
    }

    # so the user can make a choice 
    file_line_entry .choose_a_shell {Choose a Shell} \
		    /etc/shells $FLAUNCH_CONFIG/shell_choice 

    # Now that we have written the information out to a file, let's 
    # read it back in and set the necessary global variable. 
    set shell [read_file $FLAUNCH_CONFIG/shell_choice ]

    # If we choose a shell, then tell the code to use it. 
    if { $flaunch_shell_setup != 0 } {
	set flaunch_shell_setup 1
	config_file write
    }

    return

}

#----------------------------------------------------------------------
proc  file_line_entry { {window .window} {title_name "Choose an Entry"} \
			    {input_file "null" } {output_file "null" } } {

    global backgnd relef
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue

    set ff $window

    if [catch {set f [open $input_file r] } result] {
	return
    } else {
	set i 0

	if [catch {toplevel $ff} result] {
	    puts "ERROR window exists... \n"
	} else {
	    wm title $ff $title_name

	    frame $ff.full_help_top
	    frame $ff.full_help_bottom
	    button $ff.close_it -text "Close" \
		-padx 3 -pady 3 \
		-activeforeground $red \
		-activebackground $ivory -relief raised \
		-borderwidth 5 -underline 0 \
		-background $red -foreground $ivory \
		-command "clean_up $ff {} {} {}"

	    button $ff.write_it -text "OK" \
		-padx 3 -pady 3 \
		-underline 0 \
		-background yellow -foreground black \
		-activebackground forestgreen \
		-borderwidth 5 \
		-command "write_to_file $output_file $window"
	    FindFont $ff.write_it 14 bold

	    listbox  $ff.full_help -height 10 -width 30 \
		-relief ridge \
		-background $ivory -foreground black\
		-yscrollcommand " $ff.scrolly_help set" \
		-xscrollcommand " $ff.scrollx_help set"
	    scrollbar $ff.scrolly_help -background $azure1\
		-activebackground $lightpink -activerelief raised \
		-troughcolor $azure2 \
		-command " $ff.full_help yview"
	    scrollbar $ff.scrollx_help -background $azure1\
		-activebackground $lightpink -activerelief raised \
		-troughcolor $azure2 \
		-command "$ff.full_help xview" -orient horizontal 
	}

	while {[gets $f line] >= 0} {
	    $ff.full_help insert end $line
	}
	close $f

	pack $ff.full_help_top -side top -fill x
	pack $ff.close_it $ff.write_it -in $ff.full_help_top -side left
	
	pack $ff.scrollx_help -side bottom -fill x
	pack $ff.scrolly_help -side right -fill y
	pack $ff.full_help -side left -fill both -expand true
    }
    
    bind $ff <Control-c> "clean_up $ff {} {} {}"
    bind $ff <Control-o> "write_to_file $output_file $window " 
    bind $ff <Double-1> "write_to_file $output_file $window "
    FindFixedFont $ff.full_help 16 demibold

    focus $ff

}

#----------------------------------------------------------------------
# This procedure allows the user to set a log-directory to use, 
# rather than 
proc choose_a_log_directory { {action {}} } {
    global FLAUNCH_CONFIG
    global log_dir flaunch_logdir_setup dir_only 

    if { $action != "force"} {
  
	# If we turn off the use of a log_file, then go back
	# to the default, and use FLAUNCH_CONFIG
	if { $flaunch_logdir_setup == 0 } {
	    set log_dir $FLAUNCH_CONFIG
	    return
	}

	# If we turn on the use of a log_directory, and a log_choice
	# file exists than read that and return.
	if [file exists $FLAUNCH_CONFIG/log_file] {
	    set log_dir [read_file $FLAUNCH_CONFIG/log_file ]
	    return
	}
    }

    # Check to see if the configuration file directory 
    # actually exists. If it doesn't than create it since we
    # will be writing files to this directory
    if { [file isdirectory $FLAUNCH_CONFIG] != 1 } {
	catch { exec mkdir $FLAUNCH_CONFIG }
	if { [file isdirectory $FLAUNCH_CONFIG] != 1 } {
	    tkerror "Cannot create FLAUNCH configuration directory. Execute a chmod command and try this again.\n"
	    return
	}
    }

    # Since the File Selection window and Directory Selection window
    # are mutually exclusive, we delete the File Selection Window 
    # and then we will re-create it.
    if { [winfo exists .fileselect] == 1 } {
	destroy .fileselect
	tkwait window .fileselect
    }

    # The "File Selection" window can be made to function for directories.
    # This is accomplished by setting the variable "dir_only" before 
    # calling the File Selection window. 
    set dir_only 1

    # Call the "Directory Select" window - which is a special behavior
    # for the File Selection window. 
    set log_dir [fileselect "Directory Select" ]

    # After chosing a directory, unset the dir-only variable so that 
    # the File Selection windoww reverts to its default behaviour. 
    unset dir_only

    # Since Tcl does strange things if the file all ready exists
    # and you are essentially just overwriting the file, 
    # let's delete it if it is there, and then recreate it. 
    # This should always work properly.
    catch { exec rm $FLAUNCH_CONFIG/log_file }

    # Let's open the file for writing. 
    set fileId [ open $FLAUNCH_CONFIG/log_file {RDWR CREAT}]
    puts $fileId $log_dir
    close $fileId

    # If we choose a shell, then tell the code to use it. 
    if { $flaunch_logdir_setup != 1 } {
	set flaunch_logdir_setup 1
	config_file write
    }

    return

}


#----------------------------------------------------------------------
proc Input_file_name { {window .window} {title_name "Input a File"} \
			   {input_file "null" } {output_file "null" } } {
    set t $window

    if { [ file exists $input_file ] == 1 } {
	toplevel $window  -bg ivory
	wm title $window $title_name

	frame $t.button_bar -bg ivory

	button $t.button_bar.help_app -text "Help" -background blue \
	    -foreground ivory \
	    -padx 3 -pady 3 \
	    -underline 0 \
	    -borderwidth 5 \
	    -command {  }
	bind $t <Alt-h> { }
	
	button $t.button_bar.run_it -text "OK?" \
	    -padx 3 -pady 3 \
	    -underline 0 \
	    -borderwidth 5 \
	    -background turquoise \
	    -activebackground pink \
	    -relief raised\
	    -command {test_selected_task}

	bind $t <Control-g> {test_selected_task}

	button $t.button_bar.close_it -text "Close" \
	    -background red -foreground ivory \
	    -padx 3 -pady 3 \
	    -underline 0 \
	    -borderwidth 5 \
	    -command "clean_up $t {} {} {}"
	bind $t <Control-c> "clean_up $t {} {} {}"

	label $t.label -text Task: -bg ivory
	FindFont $t.label 14 bold

	entry $t.tool -width 20 -relief sunken \
	    -textvariable select_task_tool -bg ivory -fg black \
	    -justify left

	FindFont $t.tool 14 demibold
	bind Entry <Enter> {%W config -bg turquoise -fg black \
				-cursor left_side}
	bind Entry <Leave> {%W config -bg ivory -fg black}
	bind Entry <FocusIn> {%W config -bg turquoise -fg black \
				  -cursor left_side}
	bind Entry <FocusOut> {%W config -bg ivory -fg black}	
	bind $t.tool <Return> { }

	pack $t.button_bar.close_it -in $t.button_bar -anchor nw
	pack $t.button_bar -anchor nw
	pack $t.tool $t.label -side right

    } else {

	raise $t
	pack $t.button_bar.close_it $t.button_bar.run_it \
	    $t.button_bar.help_app -in $t.button_bar -side left
	pack $t.button_bar -anchor nw
	pack $t.tool $t.label -side right

    }

focus $t.tool  

}
#----------------------------------------------------------------------
proc write_to_file { {output_file "null"} {window .window} {action "file"}} {

    catch { set shell_temp [selection get] }
    
    if ![ info exists shell_temp ] {
	tkerror " You did not choose an entry.\n Please select an entry from the choices provided.\n Then select OK."
	return
    }

    set shell_temp [string trim $shell_temp]

    if [ string match "file" $action] {

	if { [ file exists $shell_temp ] != 1 } {
	    tkerror " The file, $shell_temp, does exist.\n Choose another.\n"
	    return
	}
    }

    set output_file [string trim $output_file]
	
    # Since Tcl does strange things if the file all ready exists
    # and you are essentially just overwriting the file, 
    # let's delete it if it is there, and then recreate it. 
    # This should always work properly.
    catch { exec rm $output_file }

    # Let's open the file for writing. 
    set fileId [ open $output_file {RDWR CREAT}]
    puts $fileId $shell_temp
    close $fileId

    destroy $window

    return
}

#----------------------------------------------------------------------
# This procedure actually reads the file $FLAUNCH_CONFIG/custom_tasks and 
# displays it into the listbox associated with the list of tasks. 
proc read_file { file_to_read } {
    global FLAUNCH_CONFIG
    global task_button task_button_list
    global ftools_call_package 
    global backgnd relef
    global selected_package
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue

    # Let's check to see if there is actually a list of tasks in
    # a custom file for this person. 
    if { [ file exists $file_to_read ] != 1 } {
	return
    }

    set f [open $file_to_read r]
    set i 0
    while {[gets $f line] >= 0} {
	# Now assign the proper short help to the task listed in
	# the user's personal custom_tasks file. 
	set shell $line
    }
    close $f

    return $shell

}

#----------------------------------------------------------------------
# This procedure actually reads the file $FLAUNCH_CONFIG/custom_tasks and 
# displays it into the listbox associated with the list of tasks. 
proc Read_Custom_list {} {
    global FLAUNCH_CONFIG
    global task_button task_button_list
    global ftools_call_package 
    global backgnd relef
    global selected_package
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue

    # Call the procedure that looks up ALL possible FTOOLS and 
    # defines the short-help for each. We do this by providing the
    # package name FTOOLS.
    get_ftools_hash_no_update ftools

    # Since we set the package name to "ftools" above, let's reset it
    # to the proper value of "custom". 
    set selected_package "custom"

    # Let's check to see if there is actually a list of tasks in
    # a custom file for this person. 
    if { [ file exists $FLAUNCH_CONFIG/custom_tasks ] != 1 } {
	destroy .custom
	return
    }

    set f [open $FLAUNCH_CONFIG/custom_tasks r]
    set i 0
    while {[gets $f line] >= 0} {
	# Now assign the proper short help to the task listed in
	# the user's personal custom_tasks file. 
	set temp_index ftools_hash($line)

	# Redefine the index and hash table for that task
	set ftools_index($i) temp_index
	set ftools_hash($line) temp_index
    }
    close $f

    if {$ftools_call_package > 0} {
	.tasks delete 0 $ftools_call_package
    }

    foreach tool [lsort [array names ftools_hash]] {
	incr ftools_call_package +1
	.tasks insert end "$tool" 
    }

    pack .scrollx -side bottom -in .short_help -fill x
    pack .help_short -side top -in .short_help -fill both \
	-expand true
    pack .scrolly -in .task -side right -fill y
    pack .tasks -in .task -side left -fill both -expand true

}

#----------------------------------------------------------------------
# This procedure is associated with the main "flaunch" window and allows
# the user to create a file (custom_tasks ) which contains a list of 
# tasks that the user have selected as being those that he is most likely 
# to use. This allows the user to simply "click" on the correct task 
# to activate it without having to search through a long list of tasks.
proc Create_Custom_task_list {} {
    global flaunch_custom_list_setup

    # If we are in the middle of creating custom-task-list then
    # just return without doing anything. 
    if { [winfo exists .custom_tasks] == 1 } {
	return
    }

    if { [winfo exists .all] !=1 } { 
	restore_delete_packages 
    }  

    # Set the package to ALL
    .all select

    if { [winfo exists .tasks] != 1 } {
	list_tasks ftools 
	tkwait visibility .tasks
    }

    # Let's call the generic file builder from a list. Here we are
    # associated .tasks in the . window, and creating .custom_task
    # we are telling it that it is a "task" we are wriiting to file, in
    # case there are any "special" things that have to be done to lists 
    # of type task. Then the "second" says that we are to delete
    # the .custom_task window when we are finished. 
    ListSelect . .tasks .custom_task task second {} {} 

    # Here we wait till we are finished building the list
    tkwait window .custom_task

    # Set the variable which will display the package list.
    set flaunch_custom_list_setup 1

    # Now that we are finished creating a "custom" list for our
    # user, we "update" the package list by removing it and 
    # recreating it, so that there is now a "custom" package.
    restore_delete_packages
    restore_delete_packages
    .custom select
    list_tasks custom
    return
}

#----------------------------------------------------------------------
# This procedure takes a window and gets its position in 
# coordinates for width, height, xpos, ypo. It returns 
# a 1 if the window specified exists (is mapped to the screen)
# and 0 if it fails... 

proc store_window_position { orig_win } {
    
    set height 0
    set width 0
    set xpos 0
    set ypos 0

    if [winfo ismapped $orig_win] {
	if [winfo vrootheight $orig_win] {
	    set height [winfo vrootheight $orig_win] 
	    puts "In vroot height $height"
	} else {
	    set height [winfo height $orig_win]
	    puts "in root height $height"
	}
	
	if [winfo vrootwidth $orig_win] {
	    set width [winfo vrootwidth $orig_win] 
	    puts "In vroot width $width"
	} else {
	    set width [winfo width $orig_win]
	    puts "in root width $width"
	}

	if [winfo vrootx $orig_win] {
	    set xpos [winfo vrootx $orig_win] 
	    puts "In vroot x $xpos"
	} else {
	    set xpos [winfo rootx $orig_win]
	    puts "in root x $xpos"
	}

	if [winfo vrooty $orig_win] {
	    set ypos [winfo vrooty $orig_win] 
	    puts "In vroot y $ypos"

	} else {
	    set ypos [winfo rooty $orig_win]
	    puts "in root y $ypos"
	}
	
	return 1
    } else {
	return 0
    }

}


#----------------------------------------------------------------------
# This procedure is not normally used, but I wanted to keep
# it just in case... It checks to see if Netscape is running and
# if it is then it issues a "remote" command so that the help is 
# displayed in the already running Netscape window. 
proc check_netscape_and_override { {topic aboutFlaunch} } {
    global FITSLAUNCHER_LIBRARY netpid

    return 

    # Unfortunately, there is no system independent way to find 
    # out what the process id is associated with a code executing. So
    # while we can correctly pop-up another window the first time this
    # is called, there is no way of knowing if the new-window is associated
    # with the Tcl process, or if that window was closed. So this
    # procedure is not useful until there is s system independant 
    # way to find out what the id is associated with a given process. 

    # If the user wants to use Netscape to display the help than several 
    # checks must be performed. Earlier we ensured that Netscape is actually 
    # in the user's path. But what if the user has another version of Netscape
    # running, which is NOT associated with Flaunch? If we have a version 
    # which is associated with flaunch, then we have the variable 
    # "netpid" defined which contains the process id associated with the 
    # process which was spawned. But if Netscape was started independently 
    # we have to do a check to see that it exists. 
    catch { exec rm /tmp/netscape_test }
    catch { set window_info [exec xwininfo -name "Netscape" \
				 2> /tmp/netscape_test] }
	
    # If the error file exists, that does not necessarily mean that Netscape
    # is NOT running, we have to do some farther checks first. 
    if { [ file exists /tmp/netscape_test ] == 1 } {	
	# If the file exists but is zero length - then the command
	# failed and we display an error message to the screen. 

	puts "Netscape is running"
	if { [ file size /tmp/netscape_test ] == 0 } {
	    # The file exists, but has zero length, so Netscape is 
	    # presently running so we can execute this as a remote
	    # command. 
	    puts "Attempting to spawn remote in first"
	    set netstring "netscape -remote 'OpenFile(${FITSLAUNCHER_LIBRARY}/doc/${topic}.html,new-window)' "
	    set netpid [exec sh -c $netstring &]

	    set delay 3000
	    return $delay

	} else {
	    # The file exists and has a size, i.e., it contains information
	    # thus Netscape is NOT presently running.
	    puts "Attempting to spawn in second" 
	    set netstring "netscape 'file:${FITSLAUNCHER_LIBRARY}/doc/${topic}.html"
	    set netpid [ exec sh -c $netstring & ]

	    puts "The Net pid is $netpid"

	    set delay 15000
	    return $delay

	}
    } else {
	# The error file does NOT exist, so Netscape is running. so we can
	# take command via the "remote" option.
	puts "Attempting to spawn remote in third"
	set netstring "netscape -remote 'OpenFile(${FITSLAUNCHER_LIBRARY}/doc/${topic}.html,new-window)' "
	exec sh -c $netstring &

	set delay 3000
	return $delay

    }
}


#----------------------------------------------------------------------
proc config_file { {action read} } {
    global FITSLAUNCHER_LIBRARY HOME
    global FLAUNCH_CONFIG
    global flaunch_shell_setup  \
	flaunch_logdir_setup \
	flaunch_command_call_setup \
	flaunch_custom_list_setup flaunch_output_log_setup \
	flaunch_command_line_setup \
	parameter_hidden_setup \
	parameter_help_parm_setup \
	parameter_comment_setup \
	parameter_control_setup parameter_iconify_setup \
	parameter_remove_edit_window_setup \
	parameter_pause_window_setup online_help_setup \
	fileselect_uncompress_setup fileselect_double_fv_setup

    set file_exists [file exists $HOME/.flaunchrc]

    # Check to see if the configuration file directory 
    # actually exists. If it doesn't than create it since we
    # will be writing files to this directory
    if { [file isdirectory $FLAUNCH_CONFIG] != 1 } {
	catch { exec mkdir $FLAUNCH_CONFIG }
    }

    if { $action == "write" } {

	# Since Tcl does strange things if the file all ready exists
	# and you are essentially just overwriting the file, 
	# let's delete it if it is there, and then recreate it. 
	# This should always work properly.
	catch { exec rm $HOME/.flaunchrc }

	set fileId [open $HOME/.flaunchrc {RDWR CREAT}]
	puts $fileId "flaunch_shell_setup,$flaunch_shell_setup"
	puts $fileId "flaunch_logdir_setup,$flaunch_logdir_setup"
	puts $fileId "flaunch_command_call_setup,$flaunch_command_call_setup"
	puts $fileId "flaunch_custom_list_setup,$flaunch_custom_list_setup"
	puts $fileId "flaunch_output_log_setup,$flaunch_output_log_setup"
	puts $fileId "flaunch_command_line_setup,$flaunch_command_line_setup"
	puts $fileId "parameter_hidden_setup,$parameter_hidden_setup"
	puts $fileId "parameter_help_parm_setup,$parameter_help_parm_setup"
	puts $fileId "parameter_comment_setup,$parameter_comment_setup"
	puts $fileId "parameter_control_setup,$parameter_control_setup"
	puts $fileId "parameter_iconify_setup,$parameter_iconify_setup"
	puts $fileId "parameter_remove_edit_window_setup,$parameter_remove_edit_window_setup"
	puts $fileId "parameter_pause_window_setup,$parameter_pause_window_setup"
	puts $fileId "online_help_setup,$online_help_setup"
	puts $fileId "fileselect_uncompress_setup,$fileselect_uncompress_setup"
	puts $fileId "fileselect_double_fv_setup,$fileselect_double_fv_setup"
	close $fileId
	return 
    }

    # Define the default values for various configuration parameters
    #
    set flaunch_shell_setup 0
    set flaunch_logdir_setup 0
    set flaunch_command_call_setup 1
    set flaunch_custom_list_setup 0
    set flaunch_output_log_setup 0
    set flaunch_command_line_setup 0
    set parameter_hidden_setup 0
    set parameter_help_parm_setup 1
    set parameter_comment_setup 0 
    set parameter_control_setup 0
    set parameter_iconify_setup 0
    set online_help_setup 0
    set parameter_remove_edit_window_setup 0
    set parameter_pause_window_setup 0
    set fileselect_uncompress_setup 1
    set fileselect_double_fv_setup 0


    if { $action == "reset" } {
	return 
    }

    # If a configuration file exists then open it and read out the proper
    # information. 
    if { $file_exists == 1 } {

	set fileId [open $HOME/.flaunchrc {RDWR}]
	foreach line [split [read $fileId] \n] {

	    # Use the comma as the line separator.
	    set separate [split $line , ]
	    set seplen [llength $separate]
	    if { $seplen > 2 } {
		puts "ERROR - initialization file corrupted!!!"
		puts "Delete $HOME/.flaunchrc to proceed!!!"
		exit
	    }
	    set config_name [lindex [split $line , ] 0]
	    set config_value [lindex [split $line , ] 1]
	    set $config_name $config_value
	}
	close $fileId
	return
    }
}

#----------------------------------------------------------------------
# This procedure changes the cursor while an action is being taken
# this is hoped to cut down on the number of unintentional duplicate 
# button clicks. If this procedure is called with "on" then the cursor
# is changed to a "watch" in the specified window, if called with "off" 
# the curson is changed to a "top_left_arrow" in the specified window.
# If called with any other value the cursor is changed to that symbol. 
proc ch_cursor {w symbol} {

    # Let's test to see if the specified window exists first.
    if [ winfo exists $w ] {

	if { [string match "on" $symbol] == 1 } {
	    $w configure -cursor watch
	} elseif { [string match "off" $symbol] == 1 } {
	    $w configure -cursor top_left_arrow
	} else {
	    $w configure -cursor $symbol
	}

	update idletasks
    }

    return
}

#----------------------------------------------------------------------
# This procedure operates in conjunction with hhelp, and changes the
# cursor to a watch before the calle, and if Netscape is called, 
# delays 15 seconds before changing it back, although updating
# an existing Netscape window only takes about 3 seconds.
proc delay_hhelp { { winval . } { topic aboutFlaunch } } {
    global netpid

    ch_cursor $winval on
    update idletasks

    set delay [ hhelp $topic ]

    after $delay " ch_cursor $winval off "
    return

}
  
#----------------------------------------------------------------------
# This is the Hyperhelp function that itkwish uses to create interactive
# helps. I just copied an example and am modifying it to suit my needs. 
proc hhelp { {topic aboutFlaunch} } {

    global  FITSLAUNCHER_LIBRARY 
    global netpid
    global parameter_hidden_setup \
	parameter_comment_setup \
	parameter_control_setup parameter_iconify_setup \
	parameter_remove_edit_window_setup \
	parameter_pause_window_setup online_help_setup \
	fileselect_uncompress_setup

    # Set a delay variable, this is used in conjunction with 
    # delay_hhelp and sets that amount of time to delay before
    # changing the cursor back from a watch to normal. We initialize
    # it with a 15 second delay.
    set delay 15000

    # If the user has selected to use Netscape to view the on-line help
    # then we have to check to see if Netscape is actually in his path.
    # To do this we execute the command "Netscape -v" to display the 
    # version (which for some reason is sent to stderr rather than stdout).
    # So we redirect stderr to the temporary file /tmp/netscape_test and 
    # then we check to see if it exists, and if so if is is larger than 
    # 0 length. Only in the case that the file both exists and is greater 
    # than zero length can we be sure that Netscape actually exists in 
    # the user's path.

    # Unfortunately, since different systems behave differently only 
    # when both the temporary file /tmp/netscape_test exists and is 
    # greater than zero length do we know that Netscape actually exists 
    # in the user's path. So only than can we proceed using Netscape. 
    # In the other cases - the file exists but is zero length, or it does 
    # not exists we print a warning to the screen and will use the 
    # default Hyperhelp facility. 
    if { $online_help_setup == 1 } {
	catch { exec rm /tmp/netscape_test }
	catch { exec netscape -v 2> /tmp/netscape_test }
	
	if { [ file exists /tmp/netscape_test ] == 1 } {	

	    # If the file exists but is zero length - then the command
	    # failed and we display an error message to the screen. 
	    if { [ file size /tmp/netscape_test ] == 0 } {
		puts " "
		puts "*******************************************"
		puts "      Netscape is not in your path!"
		puts "     Check the location of netscape"
		puts "      and/or modify your .flaunchrc"
		puts "     Using Itkwish Hyperhelp instead."
		puts "*******************************************"
		
		set online_help_setup 0
	    } 
	} else {
	    # If the file does NOT exist than Netscape is also not
	    # in the path so the same error message is printed. 
	    puts " "
	    puts "*******************************************"
	    puts "      Netscape is not in your path!"
	    puts "     Check the location of netscape"
	    puts "      and/or modify your .flaunchrc"
	    puts "     Using Itkwish Hyperhelp instead."
	    puts "*******************************************"
		
	    set online_help_setup 0
	}
	    
    }

    # If the user wants to use Hyperhelp than we don't have to do 
    # any special tests.
    if { $online_help_setup == 0 } {
	# If the window doesn't exist than we have to create it and set it up
	# properly. 
	if { [winfo exist .hyperHelp] == 0} {
	    hyperhelp .hyperHelp -title "flaunch: Hyperhelp" \
		-topics {aboutFlaunch Helpful_hints \
			     Choose_a_shell \
			     Custom_package_creation \
			     Directory_selection \
			     Executed_tasks \
			     Extended_help  \
			     File_selection \
			     Flaunch \
			     Fl_menubuttons \
			     Interactive \
			     List_of_selected_items \
			     Par_edit_window \
			     Par_menubuttons \
			     Task_selection \
			     Terminal \
			     Topic_list } \
		-helpdir ${FITSLAUNCHER_LIBRARY}/doc -padx 10
	}
    
	catch {.hyperHelp activate}
	.hyperHelp showtopic $topic
	set delay 1

    } elseif { $online_help_setup == 1} {

	# If we want to run Netscape instead then we have to do some
	# tests. Unfortunately, people want different things. There
	# is a procedure 'check_netscape_and_override' which will
	# check for another Netscape and take control if it exists.
	# But that is not implemented at this time. 

	# Instead we will always attempt to spawn a new Netscape for
	# the on-line help if there is not one all ready associated with
	# with flaunch.
	if { [ info exists netpid ] != 1 } {
	    # If a netpid variable is not defined, then we will attempt 
	    # to spawn a new Netscape.
	    set netstring "netscape file:${FITSLAUNCHER_LIBRARY}/doc/${topic}.html"
	    set netpid [ exec sh -c $netstring & ]

	} else {

	    # If the netpid variable is defined by a previous spawned
	    # Netscape, test to see if the process still exists.
	    if { [test_pid $netpid] == 1} {
		# If that process exists, use that window to display
		# any additional requested helps.
		set netstring "netscape -remote 'openFile(${FITSLAUNCHER_LIBRARY}/doc/${topic}.html)' "
		exec sh -c $netstring &
		set delay 3000

	    } else {
		# If that process doesn't exist, then spawn a new 
		# Netscape for viewing the on-line help.
		set netstring "netscape file:${FITSLAUNCHER_LIBRARY}/doc/${topic}.html"
		set netpid [ exec sh -c $netstring & ]
	    }

	}

    }
    
    catch { exec rm /tmp/netscape_test }
    return $delay
} 

#----------------------------------------------------------------------
# Color chapter
proc Color { color } {
	set rgb [winfo rgb . $color]
	return [format "#%03x%03x%03x" \
		[expr round([lindex $rgb 0] * 1.00)] \
		[expr round([lindex $rgb 1] * 1.00)] \
		[expr round([lindex $rgb 2] * 1.00)]]
}

#----------------------------------------------------------------------
# Color chapter
proc ColorDarken { w color } {
	set rgb [winfo rgb $w $color]
	return [format "#%03x%03x%03x" \
		[expr round([lindex $rgb 0] * 0.95)] \
		[expr round([lindex $rgb 1] * 0.95)] \
		[expr round([lindex $rgb 2] * 0.95)]]
}

#----------------------------------------------------------------------
# Font chapter
proc FindFont { w {sizes 14} {weight medium} {slant r}} {
	foreach family {times courier helvetica} {
		foreach size $sizes {
			if {[catch {$w config -font \
				-*-$family-$weight-$slant-*-*-$size-*}] == 0} {
				return -*-$family-$weight-$slant-*-*-$size-*
			}
		}
	}
	$w config -font fixed
	return fixed
}

proc FindFixedFont { w {sizes 14} {weight medium} {slant r}} {
	foreach family {fixed times courier helvetica} {
		foreach size $sizes {
			if {[catch {$w config -font \
				-*-$family-$weight-$slant-*-*-$size-*}] == 0} {
				return -*-$family-$weight-$slant-*-*-$size-*
			}
		}
	}
    $w config -font fixed
    return fixed
}

#----------------------------------------------------------------------
# This procedure sets the selected_log variable
proc set_selected_log {} {
    global selected_log

    catch {set selected_temp [selection get]}
    set temp [lindex [ split $selected_temp . ] 1]
    set value [string trim $temp]
    if [string match "log" $value] {
	set selected_log $selected_temp
    }


}


#----------------------------------------------------------------------
# This procedure reads the system one-line help file to 
# figure and stores it in two arrays - one by task name and the other 
# by number so that it can be looked up. 
proc get_ftools_hash_no_update {fhelp_file } {
    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index
    global backgnd relef 

    if {[winfo exists .label_tasks] != 1} {

	if [string match "custom" $fhelp_file] {
	    Read_Custom_list
	    return
	}
	
	catch {unset ftools_hash}
	catch {unset ftools_index}
	set fpackage "$fhelp_file"
	set fhelp_file "$fhelp_path/$fhelp_file.txt"

	set f [open $fhelp_file r]
	set i 0
	while {[gets $f line] >= 0} {
	    if [regexp {(.*) - (.*)} $line weeble tool short_help] {
		set tool [string trim $tool]
		set short_help [string trim $short_help]
		set ftools_index($i) $short_help
		set ftools_hash($tool) $short_help
		incr i +1
	    }
	}
	close $f
	
    } else {

	list_tasks $fhelp_file

    }

}

#----------------------------------------------------------------------
# This procedure actually tests to see if the input characters
# in the Select a Task window match any task name. Several things
# are tested for and examined. It is easiest to try this option 
# and use standard globbing characters, i.e., *'s and ?'s in the
# input string and see what happens....

# Basically this procedure is a good way to search all of the ftools
# to find one that does what you want. So you could either enter a 
# taskname, e.g., fdump, or a description, e.g., *hist* and look at
# a list of all tasknames with "hist" in their name and in their 
# one-line short help which describes them. 
proc test_selected_task {} {
    global select_task_tool selected_package
    global select_task_tool_temp
    global FLAUNCH_CONFIG flaunch_shell_setup shell

    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index packages


    # The global variable select_task_tool contains the string which
    # input by the user into the Select a Task window for us to use.

    # Unset some variables that will be used. 
    # task_list contains all tasks with the input string in their
    #           name. 
    if [info exists task_list] {
	unset task_list
    }

    # task_list2 contains all tasks with the input string in their
    #           one-line short_help description.. 
    if [info exists task_list2] {
	unset task_list2
    }

    # Remove any blanks that were in the input string. And assign
    # that to be the "task" which is being searched for. 
    set task [string trim $select_task_tool]

    # If nothing was input, then don't waste time doing anything.
    # Just wait for him to input something useful.
    if { [string length $task] < 1 } {
	return
    }

    # Unset some more global variables. 
    catch {unset ftools_hash}
    catch {unset ftools_index}
    set i 0

    # We are going to scan through all ftools packages for matches
    # to the input string. 
    foreach package $packages {
	set fhelp_file $package
	set fpackage "$fhelp_file"
	set fhelp_file "$fhelp_path/$fhelp_file.txt"

	# Okay, we are going to option the above files and read 
	# out what they contain. 
	set f [open $fhelp_file r]

	set j 0

	while {[gets $f line] >= 0} {
	    if [regexp {(.*) - (.*)} $line weeble tool short_help] {
		set tool [string trim $tool]
		set short_help [string trim $short_help]

		# Test to see if the input "task" matches any
		# taskname exactly. If it does than we 
		# execute it and get out of here without any
		# more testing.
		if { [string compare $task $tool]  == 0 } {
		    set perfectmatch $tool
		    set select_task_tool $perfectmatch
		    run_selected_task
		    return
		}
		    
		# If the input "task" is a "string" inside of
		# any other taskname then store that taskname in
		# the variable "task_list", and don't do any checking
		# to see if "task" is contained in the one-line
		# description of this task.
		if { [string match $task $tool] == 1 && $j == 0} {
		    if { [info exists task_list] } {
			lappend task_list "$tool"
			set j 1
		    } else {
			set task_list "$tool"
			set j 1
		    }
		    set ftools_index($i) $short_help
		    set ftools_hash($tool) $short_help
		    incr i +1

		}

		# If the input "task" did not match any part of the
		# taskname for this task, then let's see if it is
		# contained in the one-line short help describing this
		# task. If so than store the name of the task
		# in the list "task_list2"
		if { [string match $task $short_help] == 1 && $j == 0 } {
		    if { [info exists task_list2] } {
			lappend task_list2 "$tool"
			set j 1
		    } else {
			set task_list2 "$tool"
			set j 1
		    }
		    set ftools_index($i) $short_help
		    set ftools_hash($tool) $short_help
		    incr i +1

		}

		# Set up for reading the next group of tasks. 
		set j 0
	    }
	}
	close $f
    }
    
    # if [info exists perfectmatch] {
    #	set select_task_tool $perfectmatch
    #	run_selected_task
    #	return
    # }

    # If we had no perfect matches, but had a match on either the
    # tasknames or the one-line short help description then 
    # continue. 
    if { [ info exists task_list ] || [ info exists task_list2 ] }  {

	set selected_package "ftools"

	if { [winfo exists .all] != 1 } {
	    set close_it 1
	    restore_delete_packages
	}

	.all select

	if { [winfo exists .tasks] != 1 } {
	    restore_delete_list_tasks
	    tkwait visibility .tasks
	}

	if {$ftools_call_package > 0} {
	    .tasks delete 0 $ftools_call_package
	}

	if [info exists task_list] {
	    set number_of_tasks_in_list [llength $task_list]
	} else {
	    set number_of_tasks_in_list 0
	}

	if [info exists task_list2] {
	    set number_of_tasks_in_list2 [llength $task_list2]
	} else {
	    set number_of_tasks_in_list2 0
	}

	set total_number_of_tasks [expr $number_of_tasks_in_list \
				       + $number_of_tasks_in_list2 ]

	# puts "The total number is $total_number_of_tasks"
	# puts "With the values being  $number_of_tasks_in_list"
	# puts "and  $number_of_tasks_in_list2" 
	# puts "Number of tools in task is $ftools_call_package"

	if { $total_number_of_tasks  >= 1 } {
	    set ftools_call_package 0 

	    if { $number_of_tasks_in_list  >= 1 } {
		foreach tool $task_list {
		    incr ftools_call_package +1
		    .tasks insert end "$tool" 
		}
		
	    }

	    if { $number_of_tasks_in_list2  >= 1 } {
		foreach tool $task_list2 {
		    incr ftools_call_package +1
		    .tasks insert end "$tool" 
		}
		
	    }

	    pack .scrollx -side bottom -in .short_help -fill x
	    pack .help_short -side top -in .short_help -fill both \
		-expand true
	    pack .scrolly -in .task -side right -fill y
	    pack .tasks -in .task -side left -fill both -expand true

	    if { $total_number_of_tasks == 1 } {
		# puts "The total number of tasks is ONE"

		if { $number_of_tasks_in_list > 0 } {
		    foreach i $task_list {
			set select_task_tool $i
		    }
		}

		if { $number_of_tasks_in_list2 > 0 } {
		    foreach i $task_list2 {
			set select_task_tool $i
		    }
		}

		# puts "Calling run_selected_task "
		# puts "$select_task_tool"
		# run_selected_task

	    }

	} else { 

	    if { [ string first "*" $task] == -1 } {
		set select_task_tool $task
		run_selected_task
	    }
	    
	    if { [ string first "?" $task] == -1 } {
		set select_task_tool $task
		run_selected_task
	    }

	    return
	}

    } else {
	#puts "We are in the else routine\n"
	# set temp_task $task

	if { $flaunch_shell_setup == 1 } {
	    if [file exists $FLAUNCH_CONFIG/shell_choice] {
		set shell [read_file $FLAUNCH_CONFIG/shell_choice ]
	    } else {
		tkerror "You have no shell defined. Setting shell to sh"
		set flaunch_shell_setup 0
		set shell sh
		config_file write
	    }
	} else {
	    set shell sh
	}

	# This cludge is necessary so that the "Select a Task" window
	# doesn't have the "sh -c" or the quotes show up in it. 
	# So we set this temporary variable, and this is checked 
	# at the appropriate place before the call is done. If 
	# this variable is set than it uses it rather than what is stored
	# in the variable "select_task_tool".
	#
	# Oh, the reason for the "sh -c" is so that this command is 
	# executed inside of a shell which is spawned specifically for 
	# this command. 

	# This will be "unset" in the proper calling routine.
	set select_task_tool_temp " $shell -c \"$task\" "
	run_selected_task

	return 
    }

}

#----------------------------------------------------------------------
# This procedure actually runs a task.
proc run_selected_task {} {
    global select_task_tool

    ch_cursor .select_task "on" 
    ch_cursor .select_task.tool "on" 
    set task_area [winfo exists .tasks]
    set val [set_package_label $select_task_tool] 
    set task_label [winfo exists .label_tasks]
    get_ftools_hash_no_update $val

    if { $task_area == 0 && $task_label == 1} {
	restore_delete_list_tasks 
    } 

    list_tasks $val 

    if { $task_area == 0 } {
	# If there was no task area, and we created one
	# then we may want to create a task_list, but
	# only if all of the package buttons are displayed.

	# If they aren't displayed, then we do not want to display
	# the task list, so we will close it if it appears.
	if { [winfo exists .all] != 1 } {
	    restore_delete_list_tasks
	}
    }

    update_short_help $select_task_tool 
    select_task
    task_query $select_task_tool
    ch_cursor .select_task "off" 
    ch_cursor .select_task.tool "left_side"

}

#----------------------------------------------------------------------
# This procedure just pops up a small dialog box for input. 
proc select_task {} {

    global select_task_tool
    global FLAUNCH_CONFIG flaunch_shell_setup shell

    set t ".select_task"

    if { $flaunch_shell_setup == 1 } {
	if [file exists $FLAUNCH_CONFIG/shell_choice] {
	    set shell [read_file $FLAUNCH_CONFIG/shell_choice ]
	} else {
	    tkerror "You have no shell defined. Setting shell to sh"
	    set flaunch_shell_setup 0
	    set shell sh
	    config_file write
	}
    } else {
	set shell sh
    }

    if { [winfo exists .select_task ] == 0 } {
	toplevel .select_task  -bg ivory
	wm title .select_task "Select a Task"

	frame $t.button_bar -bg ivory

	button $t.button_bar.help_app -text "Help" -background blue \
	    -foreground ivory \
	    -padx 3 -pady 3 \
	    -underline 0 \
	    -borderwidth 5 \
	    -command { extended_help $select_task_tool }
	bind $t <Alt-h> { extended_help $select_task_tool }
	
	button $t.button_bar.run_it -text "Go" \
	    -padx 3 -pady 3 \
	    -underline 0 \
	    -borderwidth 5 \
	    -background turquoise \
	    -activebackground pink \
	    -relief raised\
	    -command {test_selected_task}

	bind $t <Control-g> {test_selected_task}

	button $t.button_bar.close_it -text "Close" \
	    -background red -foreground ivory \
	    -padx 3 -pady 3 \
	    -underline 0 \
	    -borderwidth 5 \
	    -command "clean_up $t {} {} {}"
	bind $t <Control-c> "clean_up $t {} {} {}"

	label $t.label -text Task: -bg ivory
	FindFont $t.label 14 bold

	entry $t.tool -width 20 -relief sunken \
	    -textvariable select_task_tool -bg ivory -fg black \
	    -justify left

	FindFont $t.tool 14 demibold
	bind Entry <Enter> {%W config -bg turquoise -fg black \
				-cursor left_side}
	bind Entry <Leave> {%W config -bg ivory -fg black}
	bind Entry <FocusIn> {%W config -bg turquoise -fg black \
				  -cursor left_side}
	bind Entry <FocusOut> {%W config -bg ivory -fg black}	
	bind $t.tool <Return> {test_selected_task}

	pack $t.button_bar.close_it -in $t.button_bar -anchor nw
	pack $t.button_bar -anchor nw
	pack $t.tool $t.label -side right

    } else {

	raise $t
	pack $t.button_bar.close_it $t.button_bar.run_it \
	    $t.button_bar.help_app -in $t.button_bar -side left
	pack $t.button_bar -anchor nw
	pack $t.tool $t.label -side right

    }

focus $t.tool  

}

#----------------------------------------------------------------------
# This routine does a "ps" for a process id number to see if that
# process still exists. If it does not it returns a 0 if the process
# is still running it returns a 1. This is used to place KILL
# buttons in certain windows such that processes that are running
# in the background can be killed. 
proc test_pid {pid_value} {
    if [catch {exec ps $pid_value} result] {
	return 0
    } else {
	return 1
    }
}

#----------------------------------------------------------------------
# This procedure kills a process ID if it exists
proc kill_pid {pid_value} {
    if { [test_pid $pid_value] == 1 } {
	catch {exec kill -9 $pid_value}
    } 
    return
}

#----------------------------------------------------------------------
# This procedure kills everything which was spawned and still might be 
# running... 
proc kill_all_processes { } {
    global spawned_tasks executed_tasks_index netpid

    for {set i 0} {$i < $executed_tasks_index} {incr i +1} {
	kill_pid $spawned_tasks($i)
    }

    if [ info exists netpid ] {
	kill_pid $netpid
	kill_pid [expr $netpid + 1]
    }
    return
}

#----------------------------------------------------------------------
proc kill_and_cleanup { l1 l2 l3 } {
    global log_dir

    if { $l1 == 1 } {
	kill_all_processes
    }
    
    if { $l2 == 1 } {
	catch {[eval exec rm -rf [glob $log_dir/*_tcl_*.log] ]}
    }

    if { $l3 ==1 } {
	exit 
    }
}

#----------------------------------------------------------------------
#
proc reinitialize_logs { } {
    global executed_tasks executed_tasks_index 


    set etl ".executed_task_list"
    if { [winfo exists .executed_task_list] == 1 } {
	raise $etl
	$etl.tasks delete 0 $executed_tasks_index
    }

    catch {[eval exec rm -rf [glob *_tcl_*.log] ]}

    set executed_tasks_index 0
    clean_up $etl {} {} {}


}

#----------------------------------------------------------------------
# This procedure reads the system one-line help file to 
# figure and stores it in two arrays - one by task name and the other 
# by number so that it can be looked up. 
proc get_ftools_hash {fhelp_file } {
    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index

    # If the user has opted for a `custom' list then read it. 
    if [string match "custom" $fhelp_file] {
	Read_Custom_list
	return
    }

    catch {unset ftools_hash}
    catch {unset ftools_index}
    set fpackage "$fhelp_file"
    set fhelp_file "$fhelp_path/$fhelp_file.txt"
    set f [open $fhelp_file r]
    set i 0
    while {[gets $f line] >= 0} {
	if [regexp {(.*) - (.*)} $line weeble tool short_help] {
	    set tool [string trim $tool]
	    set short_help [string trim $short_help]
	    set ftools_index($i) $short_help
	    set ftools_hash($tool) $short_help
	    incr i +1
	}
    }
    close $f

    if {$ftools_call_package > 0} {
	.tasks delete 0 $ftools_call_package
    }

    foreach tool [lsort [array names ftools_hash]] {
	incr ftools_call_package +1
	.tasks insert end "$tool" 
    }

    pack .scrollx -side bottom -in .short_help -fill x
    pack .help_short -side top -in .short_help -fill both \
	-expand true
    pack .scrolly -in .task -side right -fill y
    pack .tasks -in .task -side left -fill both -expand true

}

#----------------------------------------------------------------------
# This procedure actually lists the tasks that are part of a specific
# package in a frame that is displayed in the root window.
# It tasks as input one of the "package" names, or "all" to display
# all tasks. 
proc list_tasks { package } {
    global task_button task_button_list
    global ftools_call_package 
    global backgnd relef 
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue
    global selected_package

    set selected_package "$package"

    if {[winfo exists .label_tasks] != 1} {
	button .label_tasks -text "TASKS" -background $backgnd \
	    -borderwidth 5 -padx 3 -pady 3 \
	    -command {restore_delete_list_tasks}

	pack .label_tasks -in .task -side top -anchor w
	if { [winfo exists .all] == 1 } {
	    pack .all -in .package -side top -fill x
	    pack .package -in .flaunch -side left -fill both 
	}
	pack .task -in .flaunch -side right -expand true -fill both
    }
    
    if {[winfo exists .tasks] != 1} {
	listbox .tasks -width 15 -height 1 \
	    -relief ridge \
	    -background $whitesmoke -foreground $blue1 \
	    -yscrollcommand ".scrolly set" 
	
	scrollbar .scrolly -background $azure1\
	    -activebackground $lightpink -activerelief $relef \
	    -troughcolor $azure2 \
	    -command ".tasks  yview"

	bind .tasks <ButtonRelease-1> \
	    {update_short_help [selection get] }
	
	bind .tasks <Double-1> \
	    {task_query $selected_tool}

    } else {

    }

    get_ftools_hash $package
}


#----------------------------------------------------------------------
# This procedure either removes the list of tasks or redisplays it
# based upon whether the "TASKS" button is depressed, this helps to 
# "clean" up the root-window if the user is busy doing other things. 
proc restore_delete_list_tasks {} {
    global task_button task_button_list
    global ftools_call_package 
    global backgnd relef
    global selected_package
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue

    # If we are presently trying to create a custom task list 
    # then do not allow the "tasks" window to disappear. 
    if {[winfo exists .custom_task] == 1} {
	return
    }

    set package $selected_package
    set ftools_call_package 0

    if {[winfo exists .label_tasks] != 1} {
	button .label_tasks -text "TASKS" -background $backgnd \
	    -borderwidth 5 -padx 3 -pady 3 \
	    -command {restore_delete_list_tasks}
	pack .label_tasks -in .task -side top -anchor w
    }
    
    if {[winfo exists .tasks] != 1} {

	listbox .tasks -width 15 -height 1 \
	    -relief ridge \
	    -background $whitesmoke -foreground $blue1 \
	    -yscrollcommand ".scrolly set" 
	
	scrollbar .scrolly -background $azure1\
	    -activebackground $lightpink -activerelief $relef \
	    -troughcolor $azure2 \
	    -command ".tasks  yview"

	bind .tasks <ButtonRelease-1> \
	    {update_short_help [selection get] }

	bind .tasks <Double-1> \
	    {task_query $selected_tool}

	get_ftools_hash $package
	set task_button_list 1

    } else {
	destroy .tasks
	destroy .scrolly
    }
       
}

#----------------------------------------------------------------------
# This procedure modifies or updates the one-line short help that is 
# displayed at the bottom of the root-window. It updates this based upon
# the user "clicking" on one of the task-names in the task-list display.
# It also "pops-up" the "EDIT" button which allows the user to procede to 
# edit that particular task. 
proc update_short_help {tool} {
    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index selected_tool extended_help main_help
    global backgnd relef 
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue

    set main_help [string trim $tool]
    set selected_tool [string trim $tool] 

    if { [winfo exists .run_it] != 1 } {
	# button .run_it -text "Submit" -background $turquoise \
	\#    -activebackground $pink \
	\#    -command {task_query $selected_tool}
	#    FindFont .run_it 14 bold

	button .run_it -text "Go" \
	    -padx 3 -pady 3 \
	    -activeforeground blue \
	    -activebackground white -relief raised \
	    -borderwidth 5 -bg turquoise -fg black \
	    -underline 0 \
	    -command {task_query $selected_tool}
	FindFont .run_it 14 bold
	pack .run_it -in .button_bar -side left 
	bind . <Control-g> {task_query $selected_tool}
        bind . <Control-G> {task_query $selected_tool}
    }

    .help_short delete 0

    if [catch {.help_short insert end \
		  "$tool - $ftools_hash($selected_tool)"} result ] {
	.help_short insert end "$tool - No one-line description exists for this tool"
    }
}

#----------------------------------------------------------------------
proc clean_up {close var1 var2 var3 } {
    global $var1 $var2 $var3
    set $var1 0
    set $var2 0
    set $var3 0
    if { $close != "." } {
	focus .
    }
    destroy $close
}
#----------------------------------------------------------------------
proc extended_parameter_help {selected_task parameter} {
    global backgnd relef
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue

    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index selected_tool extended_help main_help
    global ff

    catch {exec fparhelp $selected_task $parameter > /tmp/fparhelp_out 2>&1  }

    if [catch {set f [open "/tmp/fparhelp_out" r] } result] {
	return
    } else {
	set i 0
	set ff .full_help_frame

	if [catch {toplevel $ff} result] {
	    $ff.full_help delete 0 $extended_help
	} else {
	    wm title $ff "Extended Help Frame"
	    frame $ff.full_help_top
	    frame $ff.full_help_bottom
	    button $ff.close_it -text "Close" \
		-padx 3 -pady 3 \
		-activeforeground $red \
		-activebackground $ivory -relief raised \
		-borderwidth 5 -underline 0 \
		-background $red -foreground $ivory \
		-command "clean_up $ff extended_help {} {}"

	    button $ff.run_it -text "Go" \
		-padx 3 -pady 3 \
		-activeforeground blue \
		-activebackground white -relief raised \
		-borderwidth 5 -bg turquoise -fg black \
		-underline 0 \
		-command {task_query $selected_tool}
	    FindFont $ff.run_it 14 bold

	    listbox  $ff.full_help -height 15 -width 75 \
		-relief ridge \
		-background $ivory -foreground $firebrick4\
		-yscrollcommand " $ff.scrolly_help set" \
		-xscrollcommand " $ff.scrollx_help set"
	    scrollbar $ff.scrolly_help -background $azure1\
		-activebackground $lightpink -activerelief raised \
		-troughcolor $azure2 \
		-command " $ff.full_help yview"
	    scrollbar $ff.scrollx_help -background $azure1\
		-activebackground $lightpink -activerelief raised \
		-troughcolor $azure2 \
		-command "$ff.full_help xview" -orient horizontal 
	}

	set extended_help 0
	while {[gets $f line] >= 0} {
	    $ff.full_help insert end $line
	    incr extended_help +1
	}
	close $f

	pack $ff.full_help_top -side top -fill x
	# pack $ff.close_it $ff.run_it -in $ff.full_help_top -side left
	pack $ff.close_it -in $ff.full_help_top -side left
	
	pack $ff.scrollx_help -side bottom -fill x
	pack $ff.scrolly_help -side right -fill y
	pack $ff.full_help -side left -fill both -expand true
   
    }
    
    bind $ff <Control-c> "clean_up $ff extended_help {} {}"
    bind $ff <Control-g> {task_query $selected_tool}

    catch { exec rm /tmp/fparhelp_out }

}

#----------------------------------------------------------------------
proc extended_help {selected_task} {
    global backgnd relef
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue

    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index selected_tool extended_help main_help
    global ff
    set fhelp_file "$fhelp_path/$selected_task.txt"

    if [catch {set f [open $fhelp_file r] } result] {
	return
    } else {
	set i 0
	set ff .full_help_frame

	if [catch {toplevel $ff} result] {
	    $ff.full_help delete 0 $extended_help
	} else {
	    wm title $ff "Extended Help Frame"
	    frame $ff.full_help_top
	    frame $ff.full_help_bottom
	    button $ff.close_it -text "Close" \
		-padx 3 -pady 3 \
		-activeforeground $red \
		-activebackground $ivory -relief raised \
		-borderwidth 5 -underline 0 \
		-background $red -foreground $ivory \
		-command "clean_up $ff extended_help {} {}"

	    button $ff.run_it -text "Go" \
		-padx 3 -pady 3 \
		-activeforeground blue \
		-activebackground white -relief raised \
		-borderwidth 5 -bg turquoise -fg black \
		-underline 0 \
		-command {task_query $selected_tool}
	    FindFont $ff.run_it 14 bold

	    listbox  $ff.full_help -height 15 -width 75 \
		-relief ridge \
		-background $ivory -foreground $firebrick4\
		-yscrollcommand " $ff.scrolly_help set" \
		-xscrollcommand " $ff.scrollx_help set"
	    scrollbar $ff.scrolly_help -background $azure1\
		-activebackground $lightpink -activerelief raised \
		-troughcolor $azure2 \
		-command " $ff.full_help yview"
	    scrollbar $ff.scrollx_help -background $azure1\
		-activebackground $lightpink -activerelief raised \
		-troughcolor $azure2 \
		-command "$ff.full_help xview" -orient horizontal 
	}

	set extended_help 0
	while {[gets $f line] >= 0} {
	    $ff.full_help insert end $line
	    incr extended_help +1
	}
	close $f

	pack $ff.full_help_top -side top -fill x
	# pack $ff.close_it $ff.run_it -in $ff.full_help_top -side left
	pack $ff.close_it -in $ff.full_help_top -side left
	
	pack $ff.scrollx_help -side bottom -fill x
	pack $ff.scrolly_help -side right -fill y
	pack $ff.full_help -side left -fill both -expand true

   
    }
    
    bind $ff <Control-c> "clean_up $ff extended_help {} {}"
    bind $ff <Control-g> {task_query $selected_tool}
}
#----------------------------------------------------------------------
proc iconify_deiconify_task {task_name} {
    global spawned_tasks spawned_tasks_name
    
    set window_file [lindex [ split $task_name . ] 0]
    set task_number [lindex [split $window_file _ ] 2]
    set exists [test_pid $spawned_tasks($task_number)]
    set window_name [$spawned_tasks_name($task_number)]

    if { $exists == 1 } {
	send $window_name {wm deiconify .}
    }


}

#----------------------------------------------------------------------
proc view_task_result {file_to_examine} {
    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index selected_tool extended_help main_help
    global fte extended_output selected_log
    global backgnd relef
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue
    global spawned_tasks spawned_tasks_name
    global flaunch_output_log_setup log_dir

    if { $flaunch_output_log_setup == 1 } {
	set temp_value $file_to_examine
	set temp_value [split $temp_value .]
	set temp_value [join $temp_value _]
	set temp $temp_value

    } else {
	set temp "Output_Log"
    }

    set fte .full_output_frame_$temp
    set fhelp_file "$log_dir/$file_to_examine"

    if { [file exists $fhelp_file] == 1} {
	set f [open $fhelp_file r]
	set i 0

	if { [winfo exists $fte] == 0 } {
	    toplevel $fte
	    wm title $fte $temp
	    frame $fte.full_output_top
	    frame $fte.full_output_bottom
	    button $fte.close_it -text "Close" \
		-padx 3 -pady 3 \
		-underline 0 \
	        -borderwidth 5 \
		-background $red -foreground $ivory \
		-command "clean_up $fte extended_output {} {}"

	    listbox  $fte.full_output -height 15 -width 80 \
		-relief ridge \
		-background $ivory -foreground black\
		-yscrollcommand " $fte.scrolly_output set" \
		-xscrollcommand " $fte.scrollx_output set"
	    scrollbar $fte.scrolly_output -background $azure1\
		-activebackground $lightpink -activerelief raised \
		-troughcolor $azure2 \
		-command " $fte.full_output yview"
	    scrollbar $fte.scrollx_output -background $azure1\
		-activebackground $lightpink -activerelief raised \
		-troughcolor $azure2 \
		-command "$fte.full_output xview" -orient horizontal 

	    bind $fte <Control-c> "clean_up $fte extended_output {} {}"


	} else {
	    raise .full_output_frame_$temp
	    $fte.full_output delete 0 $extended_output
	}

	set extended_output 0

	# We are trying to get the task number, so we remove the .log 
	# that we concatenated after we tacked on the task number.
	set window_file [lindex [ split $file_to_examine . ] 0]

	# Let's split the name of the selected log file on underscores
	# since the log file is kept specific by using _tcl_ in its name.
	set temp_list [split $window_file _]

	# Now count the number of elements in the list created above, 
	# we we split the name on underscore's
	set temp_number [llength $temp_list]

	# Since we have one element to start with, we decrement the
	# number in the list by one to get the element in the list
	# that we want.
	set temp_number [ expr $temp_number - 1 ]

	# Now we construct the tasknumber by extracting that 
	# element from the list.
	set task_number [lindex [split $window_file _ ] $temp_number]

	set exists [test_pid $spawned_tasks($task_number)]

	if { $exists == 1 } {
	    if { [winfo exists $fte.update_it ] == 0 } {
		button $fte.update_it -text "Update" \
		    -padx 3 -pady 3 \
		    -underline 0 \
		    -borderwidth 5 \
		    -background $turquoise \
		    -activebackground $pink \
		    -command {view_task_result $selected_log}
		bind $fte <Control-u> {view_task_result $selected_log}

		button $fte.kill_it -text "Kill" \
		    -padx 3 -pady 3 \
		    -underline 0 \
		    -borderwidth 5 \
		    -background $red \
		    -command { set kill_task \
				   [lindex [ split $selected_log . ] 0] ; \
				   set id [lindex [split $kill_task _ ] 2] ; \
				   kill_pid $spawned_tasks($id) ; \
				   view_task_result $selected_log } 
		bind $fte <Control-k> { set kill_task \
				   [lindex [ split $selected_log . ] 0] ; \
				   set id [lindex [split $kill_task _ ] 2] ; \
				   kill_pid $spawned_tasks($id) ; \
				   view_task_result $selected_log }

		if { [winfo exists $fte.kill_it ] == 1 } { 
		    pack $fte.update_it $fte.kill_it \
			-in $fte.full_output_top -side right
		}
	    }

	    $fte.full_output insert end "***** Process still executing *****"
	    $fte.full_output insert end "   This will be a partial log      "
	    $fte.full_output insert end " Press the Update button to update."
	    $fte.full_output insert end "-----------------------------------"
	    incr extended_output +4
	} else {
	    if { [winfo exists $fte.update_it] == 1} {
		destroy $fte.update_it
	    }
	    if { [winfo exists $fte.kill_it] == 1} {
		destroy $fte.kill_it
	    }
	}

	while {[gets $f line] >= 0} {
	    $fte.full_output insert end $line
	    incr extended_output +1
	}
	close $f

	pack $fte.full_output_top -side top -fill x
	pack $fte.close_it -in $fte.full_output_top -side left
    
	pack $fte.scrollx_output -side bottom -fill x
	pack $fte.scrolly_output -side right -fill y
	pack $fte.full_output -side left -fill both -expand true
    }
    FindFixedFont $fte.full_output 16 demibold
    # ListSelect  $fte.full_output .copy 
}

#----------------------------------------------------------------------
proc insert_button {} {

    global backgnd relef 
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue
    global selected_log

    set etl ".executed_task_list"
    set_selected_log

    if {[winfo exists $etl.run_it] != 1} {
	button $etl.run_it -text "View log" \
	    -padx 3 -pady 3 \
	    -underline 0 \
	    -background $turquoise \
	    -activebackground $pink \
	    -borderwidth 5 \
	    -command { set_selected_log ;
		view_task_result $selected_log }
	
	#button $etl.deiconify -text "Deiconify" \
	\#    -padx 3 -pady 3 \
	\#   -underline 0 \
	\#  -background yellow -foreground black \
	\#    -activebackground forestgreen \
	\#    -borderwidth 5 \
	\#    -command {iconify_deiconify_task [selection get]}

#	pack $etl.close_it \
\#	    -in $etl.tasks_top -side right
	pack $etl.file_app $etl.run_it \
	    -in $etl.tasks_top -side left

	bind $etl <Control-v> { set_selected_log ;
	    view_task_result $selected_log}
	# bind $etl <Control-d> {iconify_deiconify_task [selection get]}

    } 
    
}
#----------------------------------------------------------------------
proc update_executing_task_list {task_executing} {
    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index selected_tool extended_help main_help

    global selected_task
    global executed_tasks_button executed_tasks_index
    global executed_tasks spawned_tasks
    global executed_tasks_listbox 
    global backgnd relef
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue
    set etl ".executed_task_list"

    if { [winfo exists .executed_task_list] == 1 } {

	$etl.tasks insert end "$task_executing"

	pack $etl.tasks_top -side top -fill x
	
	if { [winfo exists .executed_task_list.run_it ] == 1} {
	    pack $etl.run_it  \
		-in $etl.tasks_top -side left
	} else {
	    pack $etl.file_app -in $etl.tasks_top -side left
	}
    
	pack $etl.scrolly_help -side right -fill y
	pack $etl.tasks -side left -fill both -expand true

	bind $etl.tasks <Double-1> \
	    {view_task_result [selection get]}

    }

}

#----------------------------------------------------------------------
proc executing_tasks_query { } {
    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index selected_tool extended_help main_help

    global selected_task selected_log
    global executed_tasks_button executed_tasks_index
    global executed_tasks spawned_tasks
    global executed_tasks_listbox 
    global backgnd relef
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue
    set etl ".executed_task_list"

    if { [winfo exists .executed_task_list.run_it ] == 1 } {
	destroy .executed_task_list.run_it
    }

    if { [winfo exists .executed_task_list] != 1 } {
	toplevel $etl
	wm title $etl "Executed Tasks"
	frame $etl.tasks_top
	frame $etl.tasks_bottom
	label $etl.label_package -text "Output files created:" \
	    -background $backgnd

#	button $etl.close_it -text "Close" \
\#	    -padx 3 -pady 3 \
\#	    -underline 0 \
\#	    -borderwidth 5 \
\#	    -background $red -foreground $ivory \
\#	    -command "clean_up $etl {} {} {}"

	menubutton $etl.file_app -text "Action" \
	    -activeforeground $blue1 -activebackground $ivory \
	    -borderwidth 5 -relief raised -bg $blue1 -fg $ivory \
	    -height 1 -underline 0 \
	    -menu $etl.file_app.menu

	set file_ap [menu $etl.file_app.menu -tearoff 1 \
			 -activebackground $ivory \
			 -activeforeground $blue1 \
			 -borderwidth 2 \
			 -relief raised -bg $blue1 \
			 -fg $ivory ]
	$file_ap add command -label "Delete logs, reinitialize count   " \
	    -underline 0 -accelerator "Ctrl+d"\
	    -command { reinitialize_logs }
	bind $etl <Control-d> {reinitialize_logs}

	$file_ap add command -label "Close this window." \
		-underline 0 -accelerator "Ctrl+c" \
		-command "clean_up $etl {} {} {}"
	bind $etl <Control-c> "clean_up $etl {} {} {}"


	listbox  $etl.tasks -height 5 -width 25 \
	    -relief ridge \
	    -background $ivory -foreground $firebrick4\
	    -yscrollcommand " $etl.scrolly_help set" 
	scrollbar $etl.scrolly_help -background $azure1\
	    -activebackground $lightpink -activerelief raised \
	    -troughcolor $azure2 \
	    -command " $etl.tasks yview"
	bind $etl.tasks <ButtonRelease-1> \
	    {insert_button}
    } else {
	raise $etl
	$etl.tasks delete 0 $executed_tasks_index
   }

# The extended_help variable keeps track of how many lines in the
# extended help window actually contains data - so lets initialize 
# it only once if is is undefined. 
   if [ info exists extended_help ] {

   } else {
       set extended_help 0
   }

    for {set i 0} {$i < $executed_tasks_index} {incr i +1} {
	$etl.tasks insert end "$executed_tasks($i).log"
    }

    pack $etl.tasks_top -side top -fill x
    pack $etl.file_app -in $etl.tasks_top -side left    

    pack $etl.scrolly_help -side right -fill y
    pack $etl.tasks -side left -fill both -expand true

    bind $etl.tasks <Double-1> \
	{set_selected_log ; 
	    view_task_result $selected_log }

}

#----------------------------------------------------------------------
# This procedure actuall takes a tool name and searches all known
# packages for that name. 
proc set_package_label { tool {update "yes"} } {
    global packages

    set value [ find_task_in_package $tool ]

    set close_it 0

    if { [winfo exists .all] != 1 } {
	set close_it 1
	restore_delete_packages
    }

    if [string match "yes" $update] {
	foreach package $packages {
	    .$package deselect
	}
    
	.all deselect
	.$value select
    }

    if { $close_it == 1 } {
	restore_delete_packages
    }
    
    if { $value == "all" } {
	return "ftools"
    } else {
	return $value
    }

}

#----------------------------------------------------------------------
# Given a specific task, search all packages for that task, and return
# the package in which that task resides - default to "all"
proc find_task_in_package { task } {
    global ftools_hash fhelp_path ftools_call_package tools \
	short_help ftools_index packages

    set package_for_task "all"


    foreach package $packages {
	set fhelp_file $package
	catch {unset ftools_hash}
	catch {unset ftools_index}
	set fpackage "$fhelp_file"
	set fhelp_file "$fhelp_path/$fhelp_file.txt"
	set f [open $fhelp_file r]
	set i 0
	while {[gets $f line] >= 0} {
	    if [regexp {(.*) - (.*)} $line weeble tool short_help] {
		set tool [string trim $tool]
		set short_help [string trim $short_help]
		set ftools_index($i) $short_help
		set ftools_hash($tool) $short_help
		incr i +1
		if { [string compare $tool $task] == 0 } {
		    set package_for_task $package
		}
	    }
	}
	close $f
    }
    
    return $package_for_task

}

#----------------------------------------------------------------------
# This procedure lists all of the possible FTOOLS packages that a 
# user can select, as well as creates the generic "all" radiobutton. 
proc list_packages {} {
global HOME FLAUNCH_CONFIG
global backgnd relef packages
global flaunch_custom_list_setup
global ivory blue1 seashell red firebrick4 lightpink azure2 \
    azure1 turquoise pink whitesmoke lightskyblue

destroy .label_package

button .label_package -text "PACKAGES" -background $backgnd \
    -borderwidth 5 -padx 3 -pady 3 \
    -command {restore_delete_packages }

pack .label_package -in .package -side top -fill x

foreach package $packages {
    radiobutton .$package -text $package -variable curr_package \
	-background $backgnd -activebackground $lightskyblue \
	-relief $relef -selectcolor $red \
	-borderwidth 2 -padx 0 -pady 0\
	-value $package -anchor w -command "list_tasks $package"
    pack .$package -in .package -side top -fill x
}

if { $flaunch_custom_list_setup == 1 } {
    if { [ file exists $FLAUNCH_CONFIG/custom_tasks ] == 1 } {
	radiobutton .custom -text custom -variable curr_package \
	    -value custom \
	    -background $backgnd -activebackground $lightskyblue \
	    -relief $relef -selectcolor $red \
	    -borderwidth 2 -padx 0 -pady 0\
	    -anchor w -command "list_tasks custom"
	pack .custom -in .package -side top -fill x
    }
}

radiobutton .all -text all -variable curr_package -value ftools \
    -background $backgnd -activebackground $lightskyblue \
    -relief $relef -selectcolor $red \
    -borderwidth 2 -padx 0 -pady 0\
    -anchor w -command "list_tasks ftools"

pack .all -in .package -side top -fill x
pack .package -in .flaunch -side left -fill both
pack .task -in .flaunch -side right -expand true -fill both

}

#----------------------------------------------------------------------
# This procedure either removes the list of packages or redisplays it
# based upon whether the "Packages" button is depressed, this helps to 
# "clean" up the root-window if the user is busy doing other things. 
proc restore_delete_packages {} {
    global HOME FLAUNCH_CONFIG
    global backgnd relef packages
    global flaunch_custom_list_setup
    global ivory blue1 seashell red firebrick4 lightpink azure2 \
	azure1 turquoise pink whitesmoke lightskyblue

    # If we are presently trying to create a custom task list 
    # then do not allow the "packages" window to disappear. 
    if {[winfo exists .custom_task] == 1} {
	return
    }

    if {[winfo exists .all] != 1} {

	foreach package $packages {
	    radiobutton .$package -text $package -variable curr_package \
		-background $backgnd -activebackground $lightskyblue \
		-relief $relef -selectcolor $red\
		-borderwidth 2 -padx 0 -pady 0\
		-value $package -anchor w -command "list_tasks $package"
	    pack .$package -in .package -side top -fill x
	}

	if { $flaunch_custom_list_setup == 1 } {
	   if { [ file exists $FLAUNCH_CONFIG/custom_tasks ] == 1 } {
	       radiobutton .custom -text custom \
		   -variable curr_package -value custom \
		   -background $backgnd -activebackground $lightskyblue \
		   -relief $relef -selectcolor $red \
		   -borderwidth 2 -padx 0 -pady 0\
		   -anchor w -command "list_tasks custom"
	       pack .custom -in .package -side top -fill x
	   }
	}

	radiobutton .all -text all -variable curr_package -value ftools \
	    -background $backgnd -activebackground $lightskyblue \
	    -relief $relef -selectcolor $red\
	    -borderwidth 2 -padx 0 -pady 0\
	    -anchor w -command "list_tasks ftools"

	pack .all -in .package -side top -fill x
	pack .package -in .flaunch -side left -fill both
	pack .task -in .flaunch -side right -expand true -fill both
	
    } else {
	
	foreach package $packages {
	    destroy .$package
	}

	if [winfo exists .custom] {
	    destroy .custom
	}

	destroy .all
	
    }
}

# Now we are finally to the "guts" of the code that sets up the 
# basic root window.

#----------------------------------------------------------------------
global env
global ftools_hash ftools_index curr_package fhelp_path \
    ftools_call_package selected_tool extended_help main_help
global selected_package
global executed_tasks_button executed_tasks_index 
global executed_edits_index
global executed_tasks spawned_tasks
global extended_output
global packages_button packages_button_list
global task_button task_button_list
global flaunch_command_call_setup \
    flaunch_shell_setup \
    flaunch_logdir_setup \
    flaunch_custom_list_setup flaunch_output_log_setup \
    flaunch_command_line_setup \
    parameter_hidden_setup \
    parameter_help_parm_setup \
    parameter_comment_setup \
    parameter_control_setup parameter_iconify_setup \
    parameter_remove_edit_window_setup \
    parameter_pause_window_setup online_help_setup \
    fileselect_uncompress_setup fileselect_double_fv_setup
global packages
global shell log_dir

#----------------------------------------------------------------------
# Let's set up all of the colors and button descriptions so that we can 
# adjust them simply in case of a problem. 
global backgnd relef
global ivory blue1 seashell red firebrick4 lightpink azure2 \
    azure1 turquoise pink whitesmoke lightskyblue


#set ivory [Color ivory]
#set blue1 [Color blue]
#set seashell [Color seashell]
#set red [Color red]
#set firebrick4 [Color firebrick4]
#set lightpink [Color lightpink]
#set azure2 [Color azure2]
#set azure1 [Color azure1]
#set lightskyblue [Color lightskyblue]
#set turquoise [Color turquoise]
#set pink [Color pink]
#set whitesmoke [Color whitesmoke]

set ivory       "ivory"        
set blue1       "blue"       
set seashell    "seashell"     
set red         "red"    
set firebrick4  "firebrick4"   
set lightpink   "lightpink"    
set azure2      "azure2"       
set azure1      "azure1"       
set lightskyblue "lightskyblue" 
set turquoise   "turquoise"    
set pink        "pink"         
set whitesmoke  "whitesmoke"    

# We have to set up a variable to keep track of the tasks executed.
set executed_tasks_button 0
set executed_tasks_index 0
set executed_edits_index -1
set extended_output 0
set packages_button 0
set packages_button_list 0
set task_button 0
set task_button_list 0
set yes "1"
set no "0"

set selected_package "ftools"
set etl ".executed_task_list"
set backgnd "$seashell"
set relef "groove"

set fhelp_path "$env(LHEA_HELP)"

if {[file isdirectory "$fhelp_path"] == 0} {
 tkerror "Cannot find HELP directory: $fhelp_path \nCheck the value of the \nenvironment variable LHEA_HELP!\n\nAborting."
    exit
}

set ipackages 0

if [file exists "$fhelp_path/asca.txt"] {
    lappend packages "asca"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: asca"
    puts "Missing file: $fhelp_path/asca.txt"
}

if [file exists "$fhelp_path/caltools.txt"] {
    lappend packages "caltools"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: caltools"
    puts "Missing file: $fhelp_path/caltools.txt"
}

if [file exists "$fhelp_path/einstein.txt"] {
    lappend packages "einstein"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: einstein"
    puts "Missing file: $fhelp_path/einstein.txt"
}

#if [file exists "$fhelp_path/examples.txt"] {
#    lappend packages "examples"
#    incr ipackages
#} else {
#    puts " "
#    puts "Cannot find package: examples"
#    puts "Missing file: $fhelp_path/examples.txt"
#}

if [file exists "$fhelp_path/fimage.txt"] {
    lappend packages "fimage"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: fimage"
    puts "Missing file: $fhelp_path/fimage.txt"
}

if [file exists "$fhelp_path/futils.txt"] {
    lappend packages "futils"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: futils"
    puts "Missing file: $fhelp_path/futils.txt"
}

if [file exists "$fhelp_path/gro.txt"] {
    lappend packages "gro"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: gro"
    puts "Missing file: $fhelp_path/gro.txt"
}

if [file exists "$fhelp_path/heasarc.txt"] {
    lappend packages "heasarc"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: heasarc"
    puts "Missing file: $fhelp_path/heasarc.txt"
}

if [file exists "$fhelp_path/rosat.txt"] {
    lappend packages "rosat"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: rosat"
    puts "Missing file: $fhelp_path/rosat.txt"
}

if [file exists "$fhelp_path/time.txt"] {
    lappend packages "time"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: time"
    puts "Missing file: $fhelp_path/time.txt"
}

if [file exists "$fhelp_path/vela5b.txt"] {
    lappend packages "vela5b"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: vela5b"
    puts "Missing file: $fhelp_path/vela5b.txt"
}

if [file exists "$fhelp_path/xronos.txt"] {
    lappend packages "xronos"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: xronos"
    puts "Missing file: $fhelp_path/xronos.txt"
}

if [file exists "$fhelp_path/xte.txt"] {
    lappend packages "xte"
    incr ipackages
} else {
    puts " "
    puts "Cannot find package: xte"
    puts "Missing file: $fhelp_path/xte.txt"
}

if { $ipackages <= 0 } {
    tkerror "No packages were found!\nCannot continue!\nAborting."
    exit
} elseif { $ipackages < 12 } {
    puts " "
    puts "============================================================"
    puts "Some FTOOLS packages are missing in your installation."
    puts "Continuing without them." 
    puts "Check to see if your FTOOLS build completed properly,"
    puts "examine your make.log file for errors, and your HELP"
    puts "directory: $fhelp_path"
    puts "============================================================"
    puts " "
    puts "Continuing with the tasks found."
}

# set packages [list asca caltools einstein examples fimage \
\#		  futils gro heasarc \
\#		  rosat time vela5b xronos xte]



set packages


# Read the configuration file, if there is one to be read.
config_file read

frame .button_bar -background $backgnd
frame .flaunch -background $backgnd
frame .package -background $backgnd
frame .task -background $backgnd
frame .short_help

set extended_help 0
set ftools_call_package 0
set selected_tool "flaunch"

set curr_package "all"

# Create the Packages button which can display or remove the list of
# packages that are available. 
button .label_package -text "PACKAGES" -background $backgnd \
    -borderwidth 5 -padx 3 -pady 3 \
    -command {restore_delete_packages }

FindFont .label_package 14 bold

pack .label_package -in .package -side top -fill x

#Set up initial list of Packages that are supported.
list_packages
restore_delete_packages

set main_help "flaunch"

# Create the "HELP" menubutton which displays an extended help window.
menubutton .help_app -text "Help" \
    -activeforeground $blue1 -activebackground $ivory \
    -borderwidth 5 -relief raised -bg $blue1 -fg $ivory \
    -height 1 -underline 0 \
    -menu .help_app.menu

set help_ap [menu .help_app.menu \
		-activebackground $ivory \
		-activeforeground $blue1 \
		-borderwidth 2 \
		-relief raised -bg $blue1 \
		-fg $ivory ]

$help_ap add command -label "About chosen task    " \
    -accelerator "Ctrl+h" \
    -command {extended_help $main_help}
bind . <Control-h> {extended_help $main_help}

$help_ap add command -label "About flaunch" \
    -underline 6 -accelerator "Ctrl+f" \
    -command {delay_hhelp . aboutFlaunch}
bind . <Control-f> {delay_hhelp . aboutFlaunch}
# bind .help_app <Return> {delay_hhelp . aboutFlaunch}
# bind .help_app.menu <Return> {delay_hhelp . aboutFlaunch}


$help_ap add command -label "All topics available" \
    -underline 0 -accelerator "Ctrl+a" \
    -command {delay_hhelp . Topic_list}
bind . <Control-a> {delay_hhelp . Topic_list}

FindFont .help_app 14 bold
FindFont .help_app.menu 14 bold

# bind . <KeyPress> {puts stdout {%%K=%k %%A=%A}}

menubutton .file_app -text "File" \
    -activeforeground $blue1 -activebackground $ivory \
    -borderwidth 5 -relief raised -bg $blue1 -fg $ivory \
    -height 1 -underline 0 \
    -menu .file_app.menu

set file_ap [menu .file_app.menu -tearoff 1 \
		-activebackground $ivory \
		-activeforeground $blue1 \
		-borderwidth 2 \
		-relief raised -bg $blue1 \
		-fg $ivory ]
$file_ap add command -label "Select a Task       " \
    -underline 9 -accelerator "Ctrl+t"\
    -command { select_task }
bind . <Control-t> {select_task}

# $file_ap add command -label "Information on FTOOLS parameters     " \
\#    -underline 0 -accelerator "Ctrl+i "\
\#   -command {View_information_on_ftools}
# bind . <Control-i> {View_information_on_ftools}

$file_ap add command -label "Browse files      " \
    -underline 0 -accelerator "Ctrl+b "\
    -command {fileselect}
bind . <Control-b> {fileselect}

$file_ap add command -label "Create a `Custom' package       " \
    -underline 11 -accelerator "Ctrl+u "\
    -command { Create_Custom_task_list }
bind . <Control-u> { Create_Custom_task_list }

$file_ap add command -label "Change default directory       " \
    -underline 7 -accelerator "Ctrl+d "\
    -command {Change_default_directory }
bind . <Control-d> { Change_default_directory }

$file_ap add separator

$file_ap add cascade -label "Quit?" \
    -underline 0 \
    -menu $file_ap.sub1


set q [menu $file_ap.sub1 \
	    -activeforeground blue \
	    -activebackground white -borderwidth 2 \
	    -relief raised \
	    -bg red -fg white ]
$q add command -label "Exit all?" \
    -underline 1 -accelerator "Ctrl+x or Ctrl+c" \
    -command "kill_and_cleanup $yes $yes $yes"

bind . <Control-x> {kill_and_cleanup $yes $yes $yes}
bind . <Control-c> {kill_and_cleanup $yes $yes $yes}

$q add command -label "Close windows, delete log-files (leave processes)?   " \
    -underline 39 \
    -accelerator "Ctrl+p" \
    -command "kill_and_cleanup $no $yes $yes"
bind . <Control-p> "kill_and_cleanup $no $yes $yes"

$q add command -label "Close windows, kill processes (leave logs)" \
    -underline 36 \
    -accelerator "Ctrl+l" \
    -command "kill_and_cleanup $yes $no $yes"
bind . <Control-l> "kill_and_cleanup $yes $no $yes"

$q add command -label "Close windows (leave logs and processes)?" \
    -accelerator "Ctrl+w" \
    -command "kill_and_cleanup $no $no $yes"
bind . <Control-w> "kill_and_cleanup $no $no $yes"

$q add command -label "Iconify this window?" \
    -underline 0 \
    -accelerator "Ctrl+i" \
    -command "wm iconify ."

bind . <Control-i> {wm iconify .}

FindFont .file_app 14 bold
FindFont .file_app.menu 14 bold
FindFont $file_ap.sub1 14 bold


# Create an "Option" button which pops up a Meno so that 
# the user can select ftools without having to "click" on them. 
menubutton .option_it -text "Options" \
    -activeforeground $blue1 -activebackground $ivory \
    -borderwidth 5 -relief raised -bg $blue1 -fg $ivory \
    -height 1 -underline 1 \
    -menu .option_it.menu

set option_ap [menu .option_it.menu -tearoff 1 \
		   -activebackground turquoise \
		   -activeforeground black \
		   -borderwidth 2 \
		   -relief raised -bg turquoise\
		   -fg black ]

$option_ap add checkbutton -label \
    "Use specified shell rather than Bourne shell?" \
    -variable flaunch_shell_setup \
    -command { choose_a_shell }

$option_ap add checkbutton -label \
    "Use specified log-directory?" \
    -variable flaunch_logdir_setup \
    -command { choose_a_log_directory }


$option_ap add checkbutton -label \
    "Turn off 'flaunch taskname' special behavior?" \
    -variable flaunch_command_call_setup

$option_ap add checkbutton -label \
    "Read in `custom' package list if one exists?" \
    -variable flaunch_custom_list_setup

$option_ap add checkbutton -label \
    "Associate each Executed Task name to an Output Log?" \
    -variable flaunch_output_log_setup

$option_ap add checkbutton -label \
    "Read/write/use command-line construction information?" \
    -variable flaunch_command_line_setup

$option_ap add separator

$option_ap add checkbutton -label "Show Hidden parameters?" \
    -variable parameter_hidden_setup

$option_ap add checkbutton -label "Create Help button for each parameter?" \
    -variable parameter_help_parm_setup

$option_ap add checkbutton -label "Show Comments on right?" \
    -variable parameter_comment_setup

$option_ap add checkbutton -label "Control command-line construction?" \
    -variable parameter_control_setup

$option_ap add checkbutton -label "Do NOT iconify task_windows?" \
    -variable parameter_iconify_setup

$option_ap add checkbutton -label "Keep Parameter Edit window open after executing?" \
    -variable parameter_remove_edit_window_setup

$option_ap add checkbutton -label "Pause terminal window before deletion?" \
    -variable parameter_pause_window_setup

$option_ap add separator

$option_ap add checkbutton -label \
    "Use Netscape to display on-line help?" \
    -variable online_help_setup

# $option_ap add checkbutton -label "Use previously running Netscape window?" \
\#    -variable parameter_help_netscape_setup

$option_ap add separator

$option_ap add checkbutton -label \
    "Attempt to uncompress files automatically?" \
    -variable fileselect_uncompress_setup

$option_ap add checkbutton -label \
    "Double-1 in File Selection window calls fv?" \
    -variable fileselect_double_fv_setup

$option_ap add separator

$option_ap add command -label "Choose a new shell for command execution." \
    -command { choose_a_shell "force" }

$option_ap add command -label "Specify a new log-file directory." \
    -command { choose_a_log_directory "force" }

$option_ap add separator

$option_ap add command -label "Reset all options to their initial defaults." \
    -command { config_file reset }

$option_ap add command -label "Restore options from configuration file." \
    -command { config_file read }

$option_ap add command -label "Save settings to configuration file?" \
    -command { config_file write }

FindFont .option_it 14 bold
FindFont .option_it.menu 14 bold

pack .file_app -in .button_bar -side left
pack .help_app -in .button_bar -side right 
pack .option_it -in .button_bar -side left 
pack .button_bar -in .flaunch -side top -fill x

tk_menuBar .button_bar .file_app .option_it .help_app 
focus .button_bar

pack .package -in .flaunch -side left -fill both
pack .flaunch -side top -fill both -expand true
pack .short_help -side bottom -fill x

# Create a generic list box for the one-line help to be displayed.
listbox .help_short -height 1 \
    -relief ridge \
    -background $whitesmoke -foreground black\
    -selectbackground $pink \
    -xscrollcommand ".scrollx set" 

scrollbar .scrollx -orient horizontal -background $azure1\
    -activebackground $lightpink -activerelief $relef \
    -troughcolor $azure2 \
    -command ".help_short xview"

bind .help_short <ButtonRelease-1>\
    {extended_help $selected_tool}


# Here we check to see if the user wants to use a different "shell"
# and if so we either read it in, or we call the procedure to define
# one and let the user define the shell to use. 

if { $flaunch_shell_setup == 1 } {
    if [file exists $FLAUNCH_CONFIG/shell_choice] {
	set shell [read_file $FLAUNCH_CONFIG/shell_choice ]
    } else {
	tkerror "You have no shell defined. Setting shell to sh"
	set flaunch_shell_setup 0
	set shell sh
	config_file write
    }
} else {
    set shell sh
}

if { $flaunch_logdir_setup == 1 } {
    if [file exists $FLAUNCH_CONFIG/log_file] {
	set log_dir [read_file $FLAUNCH_CONFIG/log_file ]
    } else {
	tkerror "You have no Log directory defined. Using default"
	set flaunch_logdir_setup 0
	set log_dir $FLAUNCH_CONFIG
	config_file write
    } 
} else {
    set log_dir $FLAUNCH_CONFIG
}

set len [llength $argv ]

if { $flaunch_command_call_setup == 0 } {
    if { $len >= 1 } {
	tkwait visibility .
	set select_task_tool [lindex $argv 0]
	set sel_package [ set_package_label $select_task_tool ]
	get_ftools_hash_no_update $sel_package
	set win_tasks [winfo exists .tasks] 
	list_tasks $sel_package
	if {$win_tasks != 1 } {restore_delete_list_tasks}
	update_short_help $select_task_tool
	task_query $select_task_tool yes
    }
    
} else {
    if { $len >= 1 } {
	tkwait visibility .
	set select_task_tool [lindex $argv 0]
	test_selected_task
   }
}
    
