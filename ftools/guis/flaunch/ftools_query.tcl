#----------------------------------------------------------------------
# This procedure works with the fileselect script and the 3rd Mouse
# button to receive file information, and set the value in a specific
# entry box to the value returned to the calling routine. "Fileselect" is
# a procedure that does file handling. 
proc set_file_name { { l 0 } dsp} {
    global deval append_at active_box
    
    set append_at 0
    
    if { $l < 1 } {
	# No L value was given, so we will have to try to figure out
	# where to place the output of the file selection box.
	
	# For now let's just make it go to the first selection box... 
	if { $active_box($dsp) > 0 } {
	    set l $active_box($dsp)
	} else {
	    set l 1
	}
    }
    
    set str_length 0
    set file_selected [fileselect]

    set tempfile [type_and_uncompress_file $file_selected]
    
    set tempfile [string trim $tempfile]
    set str_length [string length $tempfile]
    
    if { $append_at == 1 } {
	set at_sign \@
	set tempfile $at_sign$tempfile
    } else {
    }
    
    if { $str_length != 0 } {
	set deval($l$dsp) $tempfile
    }

    set display .$dsp
    set top $display
    set t1 $top.c.canvas
    set t $t1.f
    $t.row${l}.deval${l} xview moveto 1

}

#----------------------------------------------------------------------
# This is vestigial, but I wanted to keep it so that I didn't have to
# reinvent the wheel the next time that I want to do something similiar. 
proc display_close_menu {window} {
    # ---------------------------------------------------
    global ret_val
    set ret_val 0
    
    if { [winfo exists .menu] != 1} {
	set m [menu .menu -tearoff 1]
	$m add check -label Save? -variable save \
	    -command {set ret_val 1}
	$m add check -label Exit? -variable end_it \
	    -command {set ret_val 2}
	$m add check -label "Save \& Exit?" -variable both \
	    -command {set ret_val 3}
    }
    
    set geom [wm geometry $window]
    set xpos [lindex [split $geom x] 0] 
    set other  [lindex [split $geom x] 1] 
    set ypos  [lindex [split $other +] 0] 
    tk_popup $m $xpos $ypos
    
    destroy .menu
    return ret_val
    
    
}

#----------------------------------------------------------------------
proc hidden_proc { task dsp {command_call no} {comment_setup 0 } } {
    
    # We have to set callval (or string) to "cycle" so that the 
    # procedure ftools_query knows NOT to reread the parameter file, but
    # rather to use the internally stored global variables for to get
    # this information.
    set callval "cycle"
    
    # Destroy the original window, we will recreate it again once
    # we call ftools_query. 
    destroy .$dsp
    
    # Call the ftools_query procedure to generate a screen (with 
    # the same name as the one destroyed, but show all parameters
    # this time, i.e., no "hidden" parameters.
    ftools_query $task $callval 3 $dsp $command_call $comment_setup
    
    return
} 

#----------------------------------------------------------------------
# This procedure provides several levels of submitting a task to run
# either in the background or as an interactive process. This 
# procedure always creates and Expect window to run the process. 
proc submit_proc {task dsp interactive} {
    global executed_tasks_index \
	parameter_pause_window_setup \
	parameter_remove_edit_window_setup
    
    set pause_window "no"
    
    if { $parameter_pause_window_setup == 1 } {
	set pause_window "yes"
    }
    
    if { $interactive == "no"} {
	# Set the action that gen_param_sting is to take - i.e., generate
	# a string ready for input into "spawn_task"
	set action "command"
	
	#set command_string to be the actual returned string
	set command_string [gen_param_string $task $dsp $action]
	
	# Destroy the original window.
	if { $parameter_remove_edit_window_setup == 0 } {
	    destroy .$dsp
	}
	
    	# puts "Non-interactive command string is $command_string "
	set executed_tasks_index [ spawn_task $task \
				       $command_string \
				       $executed_tasks_index \
				       $interactive $pause_window]

    } elseif { $interactive == "yes" } {
	# Set the action that gen_param_sting is to take - i.e., generate
	# a string ready for input into "spawn_task"
	
	set action "command_interactive"
	
	#set command_string to be the actual returned string
	set command_string [gen_param_string $task $dsp $action]
	
	# Destroy the original window.
	if { $parameter_remove_edit_window_setup == 0 } {
	    destroy .$dsp
	}
	# puts "Interactive command string is $command_string "
	
	set executed_tasks_index [ spawn_task $task \
				       $command_string \
				       $executed_tasks_index \
				       $interactive $pause_window ]
    } else {
	
	gen_param_string $task $dsp run
	
    }
    
    
}

#----------------------------------------------------------------------
proc spawn_task {task string executed_tasks_index interactive pause_window} {
    global executed_tasks spawned_tasks spawned_tasks_name
    global term_spawn_id SYSPFILES  FITSLAUNCHER_LIBRARY
    global shell log_dir LHEASOFT
    
    # string = The command string to be passed to the spawned shell - this
    #          will simply be the task name for interactive processes. 
    #
    # execute_tasks_index   = The number tasks that have been executed so far.
    # interactive (boolean) = 
    #           Should this task be spawned with a terminal that is
    #           ready for interactive communication, or spawn it in its
    #           iconified state such that it runs quietly in the background
    #           put allows the user to inspect what is happening with each
    #           spawned task. 


    set leftbracket \[
    set rightbracket \]
    
    if { [winfo exists .tasks_executing] != 1 } {
	if { [winfo exists .executed_task_list] != 1 } {
	    button .tasks_executing -text "Output" \
		-padx 3 -pady 3 \
		-underline 0 \
		-background yellow -foreground black \
		-activebackground forestgreen \
		-activeforeground white \
		-borderwidth 5 \
		-command {executing_tasks_query}
	    bind . <Control-o> {executing_tasks_query}
	    pack .tasks_executing -in .button_bar -side left 
	}
    }
    
    # Check the string for various characters that have special meaning
    # to Tcl or any of the Tcl routines. 
    # Check the string for multiple ['s and replace them with a /[
    set string_temp [split $string \[ ]
    set string_num [llength $string_temp]
    
    if { $string_num > 1 } {
	set string [join $string_temp \\\[ ]
    }
    
    # Check the string for multiple ]'s and replace them with a /[
    set string_temp [split $string \] ]
    set string_num [llength $string_temp]
    
    if { $string_num > 1 } {
	set string [join $string_temp \\\] ]
    }
    
    set tclunderscore "_tcl_"
    set back \&
    
    # Remove and trailing spaces that the user may have entered when
    # they input a task name to execute. 
    set task [string trim $task]
    set temptask $task

    # Since it is possible for a task to have spaces and dots in it,
    # we remove any of those when defining the name of the log file
    # which we will create to store the output.
    set temptask [lindex [ split $temptask . ] 0]
    set temptaskfirst [lindex [ split $temptask \ ] 0]

    # Since a user can enter a command which we spawn as a Bourne shell
    # we don't want the name of the task to be just "sh" but rather 
    # its command name, so we move to the command is temptask matches "sh".
    if { [string compare $temptaskfirst $shell ] == 0 } {
	set temptask [lindex [ split $temptask \ ] 2]
	set temptask [lindex [ split $temptask \" ] 1]
    } else {
	set temptask $temptaskfirst
    }

    # Specify the name of the window
    set head ".spawn_$temptask$tclunderscore$executed_tasks_index"
    
    # Specify the name of the log file.
    set log "$temptask$tclunderscore$executed_tasks_index.log"

    set ex [ concat $task $string ]
    
    set temp_pid [exec $LHEASOFT/bin/wish $FITSLAUNCHER_LIBRARY/term_spawn.tcl -name $head $head $ex $log_dir/$log $interactive $task &]

    set executed_tasks($executed_tasks_index) \
	$temptask$tclunderscore$executed_tasks_index
    
    set spawned_tasks($executed_tasks_index) \
	$temp_pid
    
    set spawned_tasks_name($executed_tasks_index) \
	$head
    
    update_executing_task_list $log
    
    incr executed_tasks_index +1
    
    return $executed_tasks_index
}

#----------------------------------------------------------------------
proc change_boolean_value {dsp orig_num winval } {
    global deval
    
    if { $deval($orig_num$dsp) == "no"} {
	set deval($orig_num$dsp) "yes"
    } else {
	set deval($orig_num$dsp) "no"
    }
    $winval config -text $deval($orig_num$dsp)
    
}

#----------------------------------------------------------------------
proc gen_param_string {task dsp action} {
    global depar deone demode defilled deval demin demax \
        defin prompt number2 mode query use
    global FLAUNCH_CONFIG flaunch_command_line_setup

    # action can take on the follow values
    #   action   - generate the system call and save to the parameter file
    #   string   - generate a string ready to be input into the ftools_query
    #              routine - this is mostly for the "Hidden" key
    #   command  - generate the command string ready for "spawn_task"
    
    # Let's set the cursor in the calling window to a watch
    ch_cursor .$dsp on
    update idletasks
    after 8000 "ch_cursor .$dsp off"
    
    set j 1
    set k 1
    set d 0
    set number ""
    set char \"
    set charagain \ 
    
    set equal =
    set blank \ 
    set comma ,
    set yes yes
    set no no
    set string3 $blank
    set string2 " "
    
    set task2 [string toupper $task]
    
    # IN THIS FOREACH, SET STRING2 TO BE A LIST OF VARIABLES AND
    # THEIR VALUES.  THEN SAVE TO THE TASK PFILE THOSE LINES THAT
    # ARE INDICATED TO BE LEARNED.
    
    set ex " "
    
    # This checks to see if the user has set the
    # option which will read/write/use any
    # command-line information which may be stored
    # about the associated task. 
    if { $flaunch_command_line_setup == 1 } {
	# Since Tcl does strange things if the file all ready exists
	# and you are essentially just overwriting the file, 
	# let's delete it if it is there, and then recreate it. 
	# This should always work properly.
	catch { exec rm $FLAUNCH_CONFIG/$task.clf }
	set fileId_clf [ open $FLAUNCH_CONFIG/$task.clf {RDWR CREAT}]

	foreach l $number2($dsp) {
	    puts $fileId_clf "$depar($l$dsp),$use($l$dsp)"
	}
	close $fileId_clf
    }


    foreach l $number2($dsp) {

	# This "if" statement checks to see if any of the parameter 
	# control boxes have been deselected, i.e., the user is 
	# specifically stating NOT to include that parameter in 
	# the command line string that is passed to the system for execution. 
	if { $use($l$dsp) == 0 } {
	    continue
	} 
	
	set query($dsp) 1

	# This was the original command:
	# set detemp($l$dsp) $deval($l$dsp)

	# This is s "fix" to "catch" user error - like \n or \r...
	set tempvalue [string trim $deval($l$dsp)]
	set tempvalue [string trim $tempvalue \n ]
	set tempvalue [string trim $tempvalue \r ]

	# Okay, now let's set the value back to the parameter
	# to the string that is left after removing carriage returns
	# and line feeds.
	set detemp($l$dsp) $tempvalue
	
	set departemp [ string trim $depar($l$dsp) ]
	if { $departemp == "mode" } {
	    set string4 $string3
	}
	
	if {0 == [string length [string trim $detemp($l$dsp)]]} {
	    set temp "$depar($l$dsp)$equal\" \""
	    
	} else {
	    if {$deone($l$dsp) == "s"} {
		set temp "$depar($l$dsp)$equal\"$detemp($l$dsp)\""
	    } else {
		set temp "$depar($l$dsp)$equal$detemp($l$dsp)"
	    }
	}
	
	set string2 [concat $string2 $temp]
	
	# For those parameters which have more than one value,
	# (and thus have several input values for this one
	# parameter) these lines separate them, if they are
	# separated by commas, and passes them along. 
	
	# PDW 12/29/98: This causes problems for fselect/fcalc expressions
        #               which can contain functions with comma-separated
        #               parameters.  Tools should be able to interpret either
        #               comma- or space-separated lists, so leave it alone

	# set detemp($l$dsp) [split $detemp($l$dsp) ,]
	# set detemp($l$dsp) [join $detemp($l$dsp) \ ]
	set deval($l$dsp) $detemp($l$dsp)
	
	# If the description of the parameter is that it is a string
	# then we will pad it with a blank (that pset will handle)
	# so that the parameter is set to a blank, and insert quotes
	# around the string. 
	
	if {$deone($l$dsp) == "s"} {
	    if {0 == [string length [string trim $detemp($l$dsp)]]} {
		set string3 [concat $string3 $depar($l$dsp)$equal$charagain$char$blank$charagain$char]
		set detempit " "
		set detemp($l$dsp) $char$detempit$char
	    } else {
		set string3 [concat $string3 $depar($l$dsp)$equal$charagain$char$detemp($l$dsp)$charagain$char]
		set detemp($l$dsp) $char$detemp($l$dsp)$char
	    }
	} else {
	    set string3 [concat $string3 $depar($l$dsp)$equal$charagain$char$detemp($l$dsp)$charagain$char]
	    set detemp($l$dsp) $char$detemp($l$dsp)$char
	    
	}
	
	set ex [concat $ex $depar($l$dsp)$equal$detemp($l$dsp)]
    }
    
    if { $action == "string" } {
	
	return $ex
    } elseif { $action == "command" } {
	
	# This is used to return a string ready to be given as a 
	# string to be executed to a shell. 
	set tempit mode=hl
	
	set string2 [ concat $string4 $tempit ]
	
	# puts "string2 is $string2"
	
	return $string2
	
    } elseif { $action == "command_interactive" } {
	
	# This is used to return a string ready to be given as a 
	# string to be executed to a shell. 
	set tempit mode=ql
	# set tempit mode=hl
	set string2 [ concat $string4 $tempit ]
	# puts "string2 is $string2"
	return $string2
	
    } elseif { $action == "run" } {
	
	set tempit \;
	set sleep_val "sleep 1 "
	set sleep_val [ concat $sleep_val $tempit ]
	
	set string2 $string4
	
	# set string2 [ concat $string4 $tempit]
	# set tempit [ pid ]
	# set tempit [ concat "kill -9 " $tempit ]
	# set ampit \&
	# set tempit [ concat $tempit " 2> /dev/null 1>/dev/null " ]
	# set tempit "exit"
	
	# set string2 [ concat $string2 $tempit ]
	
	# Now tack the task being called onto the front of the string
	# containing the command-line string that is executed. 
	set temp "$task"
	set temp [ concat $temp $string2 ]
	set temp [ concat $sleep_val $temp ]
	
	# Send all output to the standard device terminal so that all
	# output shows up on the screen that originally called flaunch.
	# That is where all output will be redirected to. 
	exec sh -c $temp > /dev/tty
	
	# Return a string which is ready to be input into "spawn_task"
	return $string2
	
    } else {
	
	# Since TCL executed its own shell, which does strange things to 
	# the line passed to it, we execute a Borne shell so that we know
	# what is being intercepted by the shell and what isn't. 
	
	# Normally Tcl uses its own brain-damaged shell when attempting to
	# execute commands, unfortunately, it places double quotes all over
	# the place so there is no telling what the called codes are getting
	# as input. To avoid the majority of this we spawn a NORMAL shell
	# in which to execute the "pset" command to update the parameter file.
	
	# This is necessary since Tcl cannot output a file properly, so
	# if we attempt to simply write the parameter file ourselves, it is
	# corrupt due to extraneous characters being inserted. Hopefully this
	# bug will be fixed in an update of Tcl, but as of 7.5b1 and Tk4.1
	# this problem is present for all parameter files which require 
	# more than 25 input parameters. So we have to use this "workaround" 
	# which is generally more robust, but unfortunately slower as well. 
	
	# To compensate for this we will only write out the paratemter file
	# before actually calling the program to be executed. Although I 
	# will probably add a button to write out the parameter file, just
	# incase the user really wants to do so. 
	
	set tempit mode=hl
	set string2 [ concat $string4 $tempit ]
	
	# Call pset to set all parameters for the task
	set temp "pset $task"
	set temp [ concat $temp $ex ]
	
	exec sh -c $temp
	
	# Return a string which is ready to be input into "spawn_task"
	return $string2
    }
    
}

#----------------------------------------------------------------------
proc ftools_query {task { string none } { hidval 0 } { dsp display } \
		       { command_call no } { comment_setup 0 } } {
    
    # task = task name to be operated upon/called.
    # string = input string if called from command prompt.
    #          Default value is none
    # hidden = Display hidden parameters and query for input??? 
    #          Default is to only query for non-hidden parameters.
    
    # We will populate several lists from the parameter file:
    # depar()  = Name of the input parameter being prompted for
    # deone()  = Type of parameter that is expected: s=string;
    #            i=integer, r=real, d=double, b=boolean (yes, no)
    # demode() = Hidden parameter, or "asked", i.e., mode set
    # deval()  = Value that was read from the parameter file, 
    #            or input field
    # demin()  = Minimum value given in parameter file - usually blank
    # demax()  = Maximum value given in parameter file - usually blank
    # defin()  = Prompting string to be displayed if queried. 
    # 
    
    # active_box = The Box that is active - from the last time a
    #              box was selected. 

    global FLAUNCH_CONFIG

    global parameter_hidden_setup parameter_help_parm_setup \
	parameter_control_setup parameter_iconify_setup \
	parameter_remove_edit_window_setup \
	flaunch_command_line_setup
    
    global depar deone demode defilled deval demin demax \
        defin string2 prompt t number2 mode query hidden \
	d use active_box
    
    # THE FOLLOWING SETS THE PATH TO THE USER'S PFILES
    # DIRECTORY. NEED A GENERIC NAME FOR THE LOCATION OF
    # THE USERS PFILES AND SET THE VARIABLE PATH TO IT.
    
    # puts "Value of call is $hidval and $parameter_hidden_setup"
    
    #
    
    # Initialize the active_box, to be the first.
    set active_box($dsp) 0

    if {$hidval < 2 } {
	if { $parameter_hidden_setup == 1 } {
	    set hidval 1
	}
    } elseif { $hidval == 2 } {
	set hidval 0
    } elseif { $hidval == 3 } {
	set hidval 1
    }
    
    if { $parameter_iconify_setup == 1 } {
	set interactive yes
    } else {
	set interactive no 
    }
    
    # puts "The value of interactive is $interactive"
    
    set locpfile [copypfiles $task]
    set black "ivory"
    set yellow "black"
    set white "black"
    set display .$dsp
    set hidden($dsp) $hidval
    set save "save"
    
    # THE FOLLOWING LINES COUNT THE NUMBER OF PARAMETERS
    # PASSED WITHIN THE STRING VARIABLE AND SET VARIOUS
    # OTHER CONTROL PARAMETERS. IF STRING IS EMPTY THEN
    # I WILL EQUAL ZERO.
    
    if {$string == "none"} {
	set i 0
    } elseif { $string == "hidden" } {
	set i 0
    } else {
	set i [llength $string]
    }
    
    set j 1
    set k 1
    set d($dsp) 0
    set number ""
    set char \"
    set charagain \\
	
    set string2 $task
    set equal =
    set comma ,
    set yes "yes"
    set no "no"
    
    set task2 [string toupper $task]
    
    if {$string != "cycle" } {

	# Initialize the index variable for the command-line-file.
	set k_clf 0
	
	# This checks to see if the user has set the
	# option which will read/write/use any
	# command-line information which may be stored
	# about the associated task. 
	if { $flaunch_command_line_setup == 1 } {

	    # Check to see a CLF file exists for this task,
	    # if so than read it and use it. If not than 
	    # use the default of using all parameters on
	    # the command line. 
	    if { [file exists $FLAUNCH_CONFIG/$task.clf] == 1 } {
		# If a clf file exists then assign a file ID and
		# open it.
		set fileId_clf [open $FLAUNCH_CONFIG/$task.clf {RDWR}]

		foreach line_clf [split [read $fileId_clf] \n] {
	    
		    # Let's remove all blanks from the string - in case some
		    # idiot screwed up their command-line file. 
		    set line_clf [string trim $line_clf]

		    # Not let's search for #'s incase said idiot put comments
		    # lines into the command-line file...
		    set value_clf [string first \# $line_clf]

		    # If no #'s were found in the FIRST position then we have 
		    # a command-line file that was constructed properly and 
		    # we can procede normally.
		    if { $value_clf != 0 } {
			incr k_clf

			set line2_clf($k_clf) $line_clf
	    
			# We have to check to see if commas were used in the 
			# prompting string, i.e., are there more than 7 commas?
			set separate_clf [split $line_clf , ]
			set seplen_clf [llength $separate_clf]
	    
			# If there were more than 7 commas then we join all the
			# ones that occurred at the end to get the prompt string.
			if {$seplen_clf <= 2} {
			    set depar_clf($k_clf) [lindex [split $line_clf , ] 0]
			    set use_clf($k_clf) [lindex [split $line_clf , ] 1]
			}
			# incr k_clf
		    }
		}

		# Close the command-line file, since Tcl cannot write the file
		# out correctly. 
		close $fileId_clf
		# set k_clf [ expr $k_clf - 1 ]
	    }
	}

	# THESE LINES ASSIGN ARRAYS INPAR(J) TO EACH PARAMATER WITHIN
	# STRING AND INPAR(J) TO EACH PARAMATER'S VALUE. IF STRING
	# EMPTY (I = 0) THE WHILE LOOP WILL NOT EXECUTE.
	
	while {$i != 0} {
	    set inpar($j) [lindex [lindex $string [expr $j -1]] 0]
	    set inval($j) [lindex [lindex $string [expr $j -1]] 1]
	    incr j
	    incr i -1
	}
	incr j -1
	
	# THIS LINE OPENS THE USER'S "TASK".PAR FILE WITHIN THE
	# PFILES/RELEASE DIRECTORY FOR READING AND WRITING.
	
	set fileId [open $locpfile {RDWR}]
	
	# THIS FOREACH LOOP USES ARRAYS TO ASSIGN THE PARAMETER
	# NAMES AND THEIR VALUES TO VARIABLES. THE QUOTES IN THE
	# PFILES ARE REMOVED AND THE VARIABLES UPDATED WITH THOSE
	# VALUES PASSED BY STRING IF ANY.
	
	foreach line [split [read $fileId] \n] {
	    
	    # Let's remove all blanks from the string - in case some
	    # idiot screwed up their parameter file. 
	    set line [string trim $line]

	    # Not let's search for #'s incase said idiot put comments
	    # lines into the parameter file...
	    set value [string first \# $line]

	    # If no #'s were found in the FIRST position then we have 
	    # a parameter file that was constructed properly and 
	    # we can procede.
	    if { $value != 0 } {

		set line2($k) $line
	    
		# We have to check to see if commas were used in the 
		# prompting string, i.e., are there more than 7 commas?
		set separate [split $line , ]
		set seplen [llength $separate]
	    
		# If there were more than 7 commas then we join all the
		# ones that occurred at the end to get the prompt string.
		if {$seplen > 7} {
		    set defin($k$dsp) [join [lrange $separate 6 end] ,]
		} else {
		    # If not we just get the 7th element.
		    set defin($k$dsp) [lindex [split $line , ] 6]
		}
		
		set depar($k$dsp) [lindex [split $line , ] 0]
		set deone($k$dsp) [lindex [split $line , ] 1]
		set demode($k$dsp) [lindex [split $line , ] 2]
		set defilled($k$dsp) 0
		set deval($k$dsp) [lindex [split $line , ] 3]
		set demin($k$dsp) ""
		set demax($k$dsp) ""
		set use($k$dsp) 1

		# puts "Read in $depar($k$dsp)\nK is $k"

		# This checks to see if the user has set the
		# option which will read/write/use any
		# command-line information which may be stored
		# about the associated task. 
		if { $flaunch_command_line_setup == 0 } {
		    # set use($k$dsp) 1
		} else {
		    
		    # Check to see a CMD file exists for this task,
		    # if so than read it and use it. If not than 
		    # use the default of using all parameters on
		    # the command line. 
		    if { [file exists $FLAUNCH_CONFIG/$task.clf] == 1 } {

			# Ninety-nine percent of the time there should be
			# one-to-one correspondence between written paramters
			# in the CLF file and in the parameter file, so let's 
			# use that to our advantage, if not we will have to
			# search through ALL parameters in the CLF for a match.

			if { $k <= $k_clf } {
			    # puts "K is $k and k_clf is $k_clf\n"

			    # if k_clf actually exists for k then check:
			    if [ string match $depar_clf($k) $depar($k$dsp) ] {
				# puts "Matched $depar_clf($k) with $depar($k$dsp)\n"
				set use($k$dsp) $use_clf($k) 
			    } else {
				# If no match than scan for a match
				set m 1
				# puts "No match - searching - k < k_clf\n"

				# Let's search for a pattern match on parameters
				while {$m < $k_clf} {
				    # If we find a match set the use value properly.
				    if [ string match $depar_clf($m) $depar($k$dsp) ] {
					set use($k$dsp) $use_clf($m)
				    }
				    incr m
				}
			    }

			} else {
			    # if k is bigger than k_clf scan for a match as well... 
			    set m 1

			    # Let's search for a pattern match on parameters
			    while {$m < $k_clf} {
				# If we find a match set the use value properly.
				if [ string match $depar_clf($m) $depar($k$dsp) ] {
				    set use($k$dsp) $use_clf($m)
				}
				incr m
			    }
			}	
			
		    } else {
			set use($k$dsp) 1
		    }
		}
		
		set char1($k) [string index $deval($k$dsp) 0]
		
		if {$char1($k) == $char} { 
		    set deval($k$dsp) [string trim $deval($k$dsp) $char]
		}
	    
		if { [string match "mode" $depar($k$dsp) ] == 1 } {
		    set mode($dsp) $deval($k$dsp)
		}
	    
		if { $comment_setup == 1 } {
		    set par_length [string length $depar($k$dsp)]
		    for {set i $par_length} {$i < 12} {incr i 1} {
			set depar($k$dsp) "$depar($k$dsp) "
		    }
		}
	    
		set demin($k$dsp) [lindex [split $line , ] 4]
		set demax($k$dsp) [lindex [split $line , ] 5]
		
		set char2($k) [string index $defin($k$dsp) 0]
		
		if {$char2($k) == $char} { 
		    set defin($k$dsp) [string trim $defin($k$dsp) $char]
		}
		
		set p $j
		
		while {$p != 0} {
		    if { [string match $inpar($p) $depar($k$dsp)] == 1 } {
			set deval($k$dsp) $inval($p)
			set defilled($k$dsp) 1
		    }
		    incr p -1
		}
		
		lappend number $k
		incr k
	    } else {
		# This is what is executed if #'s are found in the first
		# space, i.e., the zero value.
		# Nothing is done... 

	    }
	}
	    
	incr k -2
	    
	    # Close the parameter file, since Tcl cannot write the file
	    # out correctly. 
	
	close $fileId
	
	# Number is simply a string (1 2 3 ...) with one numerical
	# value for each line in the task parameter file.
	
	# Number2 equals the total number with the last element removed
	# since the loop iterates one to many times.
	set number2($dsp) [lreplace $number $k $k]
	
    }
    
    # do we need to really do anything?  If dodisplay is still 0 then there
    # is nothing to do
    set dodisplay 0
    foreach l $number2($dsp) {
	set query($dsp) 0
	if {$hidden($dsp)} {
	    set query($dsp) 1
	}
	set dodisplay 1
    }
    
    # THE FOLLOWING SECTION SETS UP THE TK INTERFACE IF NEEDED
    # USING A TOPLEVEL WINDOW.
    
    # set todo($dsp) "NOTHING"
    
    if {$dodisplay} {
	
	if { [winfo exists $display] == 1 } {
	    destroy $display
	}
	
	# Create and name a toplevel display ready for input.
	toplevel $display -bg $black
	
	set top $display
	wm title $display $task
	
	# ---------------------------------------------------
	
	# Create two frames - one for buttons and the other for
	# all of the entries which will be placed on the canvas. 
	set t2 [frame $top.d]
	pack $top.d -side top -fill both
	
	frame $top.c
	
	canvas $top.c.canvas  -width 100 -height 100 \
	    -yscrollcommand [list $top.c.yscroll set]
	scrollbar $top.c.yscroll -orient vertical  \
	    -background azure1\
	    -activebackground lightpink -activerelief groove \
	    -troughcolor azure2 \
	    -command [list $top.c.canvas yview]
	pack $top.c.yscroll -side right -fill y
	pack $top.c.canvas -side left -fill both -expand true
	pack $top.c -side top -fill both -expand true
	
	set t1 $top.c.canvas
	
	frame $t1.f 
	set t $t1.f
	# $t1 create window 0 0 -anchor nw -window $t1.f
	
	# ---------------------------------------------------------
	
	frame $t2.submit -bg $black 
	pack $t2.submit -fill x
	
	# this section creates entry and radio-button widgets
	# for those variables which need user input.
	
	foreach l $number2($dsp) {
	    
	    set holdval $l
	    set query($dsp) 0
	    
	    if {$hidden($dsp)} {
		set query($dsp) 1
	    }
	    
	    if {($demode($l$dsp) == "a") && ($mode($dsp) == "ql")} {
		set query($dsp) 1
	    } elseif {[string index $demode($l$dsp) 0] == "q"} {
		set query($dsp) 1
	    }
	    
	    if {$query($dsp) && $defilled($l$dsp)} {
		set query($dsp) 0
	    }
	    
	    if {$query($dsp)} {
		frame $t.row${l}
		$t.row${l} config -bg $black
		
		label $t.row${l}.defin${l} -bg $black -fg $yellow\
		    -text $defin($l$dsp) 
		FindFont $t.row${l}.defin${l} 14 demibold
		
		# This little conditional if sets hidden paramters
		# to be displayed in a "red" font and non-hidden 
		# parameters to be displayed in "black"
		if {($demode($l$dsp) == "a") && ($mode($dsp) == "ql")} {
		    label $t.row${l}.depar${l} -bg ivory -fg black\
			-text "$depar($l$dsp)"
		} else {
		    label $t.row${l}.depar${l} -bg ivory -fg red\
			-text "$depar($l$dsp)"
		}

		# This changes the Font from proportional to fixed
		# if the user wants to have the parameters come before
		# the description as to what to input - some users
		# one in particular wanted this and since he was
		# someone here and high-up this option was inserted
		# and made configurable by the user. The majority of users
		# do not want this - but it is now possible... 
		if { $comment_setup == 1 } {
		    FindFixedFont $t.row${l}.depar${l} 14 bold
		} else {
		    FindFont $t.row${l}.depar${l} 14 bold
		}

		# Create the button to display help on this
		# particular parameter - it is a small 
		# question mark before the parameter.
		button $t.row${l}.dehelp${l} \
			-text "?" \
			-height 1 \
			-padx 0 -pady 0 \
			-bg turquoise -fg black \
			-relief sunken -activebackground white \
			-activeforeground blue \
			-command "extended_parameter_help $task $depar($l$dsp)"
		
		# This series of tests adds the "type" of parameter
		# to be input to the screen. So we have to test
		# and define a human readable form to this. 
		# So "s" is a "string" (str), "r" is "real", 
		# "g" is a "list" (list), "d" is
		# a "double" (dbl), "i" is an "integer" (int),
		# "b" is a "boolean" (bool), and anything else is
		# just displayed as it is given. 
		if { $comment_setup == 1 } {
		    
		    if { $deone($l$dsp) == "s"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "  (str):" 
		    } elseif { $deone($l$dsp) == "r"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text " (real):" 
		    } elseif { $deone($l$dsp) == "g"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text " (list):" 
		    } elseif { $deone($l$dsp) == "d"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "  (dbl):" 
		    } elseif { $deone($l$dsp) == "i"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "  (int):" 
		    } elseif { $deone($l$dsp) == "b"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text " (bool):" 
		    } else {
			
			set par_length [string length $deone($l$dsp)]
			for {set i $par_length} {$i < 8} {incr i 1} {
			    set deone($l$dsp) " $deone($l$dsp)"
			}
			
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "($deone($l$dsp)):" 
		    }
		    
		    
		} else {
		    
		    if { $deone($l$dsp) == "s"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "(str):" 
		    } elseif { $deone($l$dsp) == "r"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "(real):" 
		    } elseif { $deone($l$dsp) == "g"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "(list):" 
		    } elseif { $deone($l$dsp) == "d"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "(dbl):" 
		    } elseif { $deone($l$dsp) == "i"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "(int):" 
		    } elseif { $deone($l$dsp) == "b"} {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "(bool):" 
		    } else {
			label $t.row${l}.deone${l} -bg ivory -fg black\
			    -text "($deone($l$dsp)):" 
		    }
		    
		}
		
		FindFont $t.row${l}.deone${l} 14 demibold
		
		
		# if the value of the parameter is boolean then a 
		# toggle-button with only a yes/no response is set up.
		if {$deone($l$dsp) == "b"} {
		    
		    button $t.row${l}.deval${l} \
			-text $deval($l$dsp) -width 25 \
			-height 1 \
			-padx 0 -pady 0 \
			-bg ivory -fg black \
			-relief sunken -activebackground blue \
			-activeforeground white \
			-command "change_boolean_value $dsp $holdval $t.row${l}.deval${l}"
		    checkbutton $t.row${l}.use${l} -selectcolor firebrick4 \
			-bg ivory \
			-padx 0 -pady 0 \
			-onvalue 1 \
			-offvalue 0 \
			-variable use($l$dsp) 
		    
		    FindFont $t.row${l}.deval${l} 14 demibold
		    
		    bind button <FocusIn> {%W config -bg blue -fg white}
		    bind button <FocusOut> {%W config -bg ivory -fg blue}
		    bind button <Key-y> {%W select}
		    
		    if { $comment_setup == 0 } {
			if { $parameter_control_setup == 1 } {
			    pack $t.row${l}.use${l} $t.row${l}.deval${l}  \
				-side right -padx 5 -fill y
			} else {
			    pack $t.row${l}.deval${l}  \
				-side right -padx 5 -fill y
			}
		    }
		    
		    # If it isn't a boolean create an "entry" widget. 
		} else {
		    
		    entry $t.row${l}.deval${l} -width 25 -relief sunken \
			-textvariable deval($l$dsp) -bg ivory -fg black \
			-justify center
		    $t.row${l}.deval${l} xview moveto 1
		    FindFont $t.row${l}.deval${l} 14 demibold
		    
		    checkbutton $t.row${l}.use${l} -selectcolor firebrick4 \
			-onvalue 1 \
			-offvalue 0 \
			-padx 0 -pady 0 \
			-bg ivory \
			-variable use($l$dsp) 
		    
		    bind $t.row${l}.deval${l} <Button-3> \
			"set_file_name $l $dsp"

		    bind $t.row${l}.deval${l} <Button-1> \
			"set active_box($dsp) $l"
		    
		    bind Entry <Enter> {%W config -bg blue -fg white \
					    -cursor left_side}
		    bind Entry <Leave> {%W config -bg ivory -fg black}
		    bind Entry <FocusIn> {%W config -bg blue -fg white \
					      -cursor left_side}
		    bind Entry <FocusOut> {%W config -bg ivory -fg black}
		    
		    
		    if { $comment_setup == 0 } {
			if { $parameter_control_setup == 1 } {
			    pack $t.row${l}.use${l} $t.row${l}.deval${l}  \
				-side right -padx 5 -fill y
			} else {
			    pack $t.row${l}.deval${l}  \
				-side right -padx 5 -fill y
			}
		    }
		    
		}
		
		
		if { $comment_setup == 1 } {
		    pack $t.row${l}.depar${l} \
			-side left -fill both 
		    pack $t.row${l}.deone${l} \
			-side left  -fill y 

		    if { $parameter_help_parm_setup == 1 } {
			pack $t.row${l}.dehelp${l} \
			    -side left -fill y
		    }
		    
		    if { $parameter_control_setup == 1 } {
			pack $t.row${l}.deval${l} $t.row${l}.use${l} \
			    -side left -padx 5 -fill y
		    } else {
			pack $t.row${l}.deval${l}  \
			    -side left -padx 5 -fill y
		    }
		    
		    pack $t.row${l}.defin${l} \
			-side left
		    pack $t.row${l} -side top -fill both
		    
		    
		} else {
		    
		    # Pack everything into the frame. Unfortunately, since
		    # this is all placed in a canvas the packing order does
		    # NOT affect resizing the window.
		    
		    if { $parameter_help_parm_setup == 1 } {
			pack $t.row${l}.dehelp${l} \
			    -side right -fill y
		    }

		    pack $t.row${l}.deone${l} \
			-side right -fill y 
		    pack $t.row${l}.depar${l} \
			-side right -fill both 
		    pack $t.row${l}.defin${l} \
			-side left
		    pack $t.row${l} -side top -fill both
		    
		}
		incr d($dsp)
		
	    }
	}
	
	
	# if the tk interface is created then the following
	# menubuttons are created.
	
	if {$d($dsp) != 0} {
	    
	    menubutton $t2.submit.file -text "File" \
		-activeforeground blue \
		-activebackground white -relief raised \
		-borderwidth 5 -bg blue -fg white \
		-underline 0 \
		-height 1 \
		-menu $t2.submit.file.menu
	    set s [menu $t2.submit.file.menu \
		       -activeforeground blue \
		       -activebackground white \
		       -relief raised \
		       -borderwidth 2 -bg blue -fg white ]
	    
	    if {$hidden($dsp) == 0} {
		$s add command -label "Show Hidden parameters" \
		    -command "ftools_query $task cycle 3 $dsp $command_call $comment_setup"
	    } else {
		$s add command -label "Hide Hidden parameters" \
		    -command "ftools_query $task cycle 2 $dsp $command_call $comment_setup"
	    }
	    
	    $s add command -label "File selection" \
		-accelerator "Ctrl+f" \
		-command "set_file_name 0 $dsp"
	    bind $display <Control-f> "set_file_name 0 $dsp"
	    
	    $s add command -label "Create a File" \
		-accelerator "Ctrl+i" \
		-command "FileCreate"
	    
	    $s add command -label "Close this window." \
		-accelerator "Ctrl+c" \
		-command "destroy $display"
	    bind $display <Control-c> "destroy $display"
	    $s add command -label "Save parameter file." \
		-accelerator "Crtl+s" \
		-command "gen_param_string $task $dsp $save"
	    FindFont $t2.submit.file 14 bold
	    FindFont $t2.submit.file.menu 14 bold
	    bind $display <Control-s> "gen_param_string $task $dsp $save"
	    
	    if { $command_call == "yes" } {
		button $t2.submit.submit -text "Run" \
		    -padx 3 -pady 3 \
		    -underline 0 \
		    -activeforeground blue \
		    -activebackground white -relief raised \
		    -borderwidth 5 -bg turquoise -fg black \
		    -command "wm withdraw $display ; wm withdraw . ; update ; submit_proc $task $dsp window ; exit "
		
		FindFont $t2.submit.submit 14 bold
		bind $display <Control-r> "wm withdraw $display ; wm withdraw . ; update ; submit_proc $task $dsp window ; exit "
		bind $display <Control-g> "wm withdraw $display ; wm withdraw . ; update ; submit_proc $task $dsp window ; exit "
	    } else {
		menubutton $t2.submit.submit -text "Run" \
		    -activeforeground blue \
		    -activebackground white -relief raised \
		    -borderwidth 5 -bg blue -fg white \
		    -underline 0 \
		    -height 1 \
		    -menu $t2.submit.submit.menu
		set s [menu $t2.submit.submit.menu \
			   -activeforeground blue \
			   -activebackground white \
			   -relief raised \
			   -borderwidth 2 -bg blue -fg white ]
		$s add command -label "Execute (default)?" \
		    -accelerator "Ctrl+r" \
		    -command "ch_cursor $dsp on ; submit_proc $task $dsp $interactive ; ; ch_cursor $dsp off"
		$s add command -label "Execute in open-window?" \
		    -command "ch_cursor $dsp on ; submit_proc $task $dsp $yes ; ch_cursor $dsp off"
		$s add command -label "Execute in iconified window?" \
		    -command "ch_cursor $dsp on ; submit_proc $task $dsp $no ; ; ch_cursor $dsp off"
		bind $display <Control-r> "ch_cursor $dsp on ; submit_proc $task $dsp $interactive ; ch_cursor $dsp off"
	    }
	    
	    set help [menubutton $t2.submit.help -text "Help" \
			  -activeforeground blue -activebackground white \
			  -borderwidth 5 -relief raised -bg blue -fg white \
			  -height 1 -underline 0 \
			  -menu $t2.submit.help.menu]
	    
	    set help_but [menu $t2.submit.help.menu -tearoff 1 \
			      -activebackground white \
			      -activeforeground blue \
			      -borderwidth 2 \
			      -relief raised -bg blue \
			      -fg white ]
	    
	    $help_but add command -label "About the selected task   " \
		-accelerator "Ctr+h"\
		-command "extended_help $task"
	    bind $display <Control-h> "extended_help $task"
	    
	    $help_but add command -label "Parameter edit window help" \
		-underline 0 -accelerator "Ctrl+p"\
		-command "delay_hhelp $display Par_edit_window"
	    bind $display <Control-p> "delay_hhelp $display Par_edit_window"
	    
	    $help_but add command -label "Other Topics available" \
		-underline 0 -accelerator "Ctrl+o"\
		-command "delay_hhelp $display Topic_list"
	    bind $display <Control-o> "delay_hhelp $display Topic_list"
	    
	    FindFont $t2.submit.help 14 bold
	}
	
	bind Entry <Up>  {tk_focusPrev $t}
	bind Entry <Down> {tk_focusNext $t}
	bind Entry <Tab> {tk_focusNext $t}
	bind Entry <Shift-Tab> {tk_focusPrev $t}
	bind $t2.submit <Key-s> {set prompt($dsp) 1}
	
	# ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
	
	$t1 create window 0 0 -anchor nw -window $t1.f
	
	set child [lindex [pack slaves $t] 0]
	
	tkwait visibility $child
	set incr [winfo height $child]
	set width [winfo width $t]	
	set height [winfo height $t]
	
	set truewidth [expr $width + 25 ]
	set trueheight [ expr $height + $incr +18 ]
	
	$t1 config -scrollregion "0 0 $width $height"
	$t1 config -yscrollincrement $incr
	
	$t1 find closest \
	    [$t1 canvasx $width] \
	    [$t1 canvasy 0] 
	
	if {$height > 10 * $incr} {
	    set height [expr 10 * $incr]
	}
	
	$t1 config -width $width -height $height
	
	pack $top.d -side top 
	
	pack $t2.submit.help -in $t2.submit -side right
	pack $t2.submit.file -in $t2.submit -side left
	pack $t2.submit.submit -in $t2.submit -anchor center
	
	pack $top.c -side bottom -expand true -fill both
	
	set height_imp [winfo height $t1]
	
	wm minsize $display $truewidth $height_imp
	wm maxsize $display $truewidth $trueheight
	
    }
    
    return 
}

#----------------------------------------------------------------------
# This code has been modified to make use of the "plist" command since
# the way that PFILES is defined has changed dramatically, and this 
# is the only way to ensure that this section of code will remain 
# accurate and appropriate as XPI changes.

proc copypfiles {task} {
    
    set temp_string "plist $task"

    if [catch {set d [exec sh -c $temp_string | grep "Parameters for " ] }] {
	return 
    }

    regsub {Parameters for /} $d {/} locpfile

    # otherwise, things look good!
    return $locpfile

}

#----------------------------------------------------------------------
proc copypfiles_old {task} {
    
    global env
    
    if [catch {set pfclobber $env(PFCLOBBER)}] {
	set pfclobber 0
    } 
    set pfiles $env(PFILES)
    
    # parse apart the pfiles
    
    set locpfiles [lindex [split $pfiles ";"] 0]
    set syspfiles [lindex [split $pfiles ";"] 1]
    
    set syspfile $syspfiles/$task.par
    set locpfile $locpfiles/$task.par
    
    if {[file exists $locpfile] == 1 && $pfclobber == 0} {
	# we're done, the local pfile exists and we don't want
	# to overwrite things
	return $locpfile
    }
    
    if {[file exists $syspfile] == 0} {
	error "Parameter file $syspfile doesn't exist!\n"
    }
    
    if {[file exists $locpfile] == 0} {
	# locpfile doesn't exist, copy it
	exec cp $syspfile $locpfile
	return $locpfile
    }
    
    if {[file mtime $syspfile] > [file mtime $locpfile]} {
	# locpfile exists, but is older than syspfile
	exec cp $syspfile $locpfile
	return $locpfile
    }
    
    # otherwise, things look good!
    return $locpfile
    
}

#----------------------------------------------------------------------
proc task_query {task {command_call no} } {
    
    # Unfortunately not all FTOOLS have parameter files, so we have to check
    # for a parameter file, and if it doesn't exist then we call a procedure
    # which will spawn a new xwindow to run the task interactively. 
    
    global env executed_tasks_button executed_tasks_index 
    global executed_edits_index 
    global executed_tasks spawned_tasks
    global log input command but
    global parameter_pause_window_setup parameter_comment_setup
    global select_task_tool_temp

    ch_cursor . "on"
    
    set pause_window "no"

    if [info exists select_task_tool_temp] {
	set task $select_task_tool_temp
	catch { unset select_task_tool_temp }
    }
    
    # Check to see if the user wants to "pause" any 
    # interactive task windows that are opened
    if { $parameter_pause_window_setup == 1 } {
	set pause_window "yes"
    }

    # Here we reproduce what is done within copypfiles, since
    # there is no EASY way to show that the function errored out, it
    # is faster to execute the command and be done with it.

    set temp_string "plist $task"

    if [catch {set d [exec sh -c $temp_string | grep "Parameters for " ] }] {
    # If no parameter file exists then we must spawn an
    # interactive shell which will allow the user to communicate 
    # interactively with the spawned task - this is most commonly 
    # the case when the user is running a Perl script
    # which by definition must be interactive, or one of the growing 
    # Tcl scripts which also require interactive communication. 

	set interactive "yes"
	set executed_tasks_index [ spawn_task $task {} \
				       $executed_tasks_index \
				       $interactive $pause_window ]
	ch_cursor . "off"
	return
    }

    regsub {Parameters for /} $d {/} locpfile

    # Check to see if a parameter file exists for the selected task. If this
    # task has a parameter file, then we will call the "ftools_query" 
    # procedure which allows the user to edit the parameter file and 
    # then submit the task (with the modified parameter values) 
    # for execution. 
    if {[file exists $locpfile] == 1 } {
	incr executed_edits_index +1
	ftools_query $task none 0 dsp_$executed_edits_index \
	    $command_call $parameter_comment_setup
	
	ch_cursor . "off"
	return
    }
    
    # puts "If you are seeing this you have no local pfile...\n"

    # This part will execute if no local pfile exists or
    # pfclobber is allowed - i.e., we can overwrite the locpfiles.
	
    incr executed_edits_index +1	
    set ex [ ftools_query $task none 0 dsp_$executed_edits_index \
		 $command_call $parameter_comment_setup ]
    
    ch_cursor . "off"
    return
	
}
