# LightSteelBlue1 == {202, 225, 255} == #cae1ff
#tk_setPalette LightSteelBlue1
tk_setPalette #dcf0ff
#tk_setPalette #e4f0ff
#
set appname xsut
set version 1.02
set revdate "08 April 1998"
wm title . "XTE Selective UnTar"
wm iconname . $appname
#
set obsidregexp "^.....-..-..-*"
#
# Make sure that we don't orphan the placard window...
#
bind . <Destroy> placard_destroy
#
# Here come de menus...
#
menu .menu -tearoff 0
#
set m .menu.file
menu $m -tearoff 0
.menu add cascade -label Functions -menu $m -underline 1
$m add command -label "Apply ObsId/AppId Criteria" -command siftfiles -state disabled
$m add check -label "Save filelist?" -variable svlist
$m add command -label "Exit $appname" -command {wm withdraw . ; exit}
#
set m .menu.obsid
menu $m -tearoff 0
set obsel 0
.menu add cascade -label "ObsId" -menu $m -underline 0
$m add radio -label "All obsids" -variable obsel -value 0 -command {select_obs}
$m add radio -label "All non-slews" -variable obsel -value 1 -command {select_obs}
$m add radio -label "All slews" -variable obsel -value 2 -command {select_obs}
$m add radio -label "Selected" -variable obsel -value 3
#
set m .menu.appid
menu $m -tearoff 0
set filechoice all
.menu add cascade -label "Files" -menu $m -underline 0
$m add radio -label "All Files" -variable filechoice -value all -command {file_sanity all}
$m add radio -label "Selected Files ONLY" -variable filechoice -value sel -command {file_sanity selected}
$m add cascade -label "Specific AppIds" -menu $m.specific
#
set m .menu.appid.specific
menu $m -tearoff 1 -postcommand {set filechoice sfc}
$m add check -label "PCA Standard Modes" -variable app_pcastd -command {file_sanity specific}
$m add check -label "HEXTE Standard (Archive) Modes" -variable app_hxtstd -command {file_sanity specific}
$m add check -label "PCA Custom Modes" -variable app_pcacus -command {file_sanity specific}
$m add check -label "HEXTE Custom Modes" -variable app_hxtcus -command {file_sanity specific}
$m add check -label "Standard Products" -variable app_stdp -command {file_sanity specific}
$m add check -label "Housekeeping (pca/hexte/acs)" -variable app_hk -command {file_sanity specific}
$m add check -label "Calibration Data" -variable app_cal -command {file_sanity specific}
#
set m .menu.help
menu $m -tearoff 0
.menu add cascade -label Help -menu $m -underline 0
$m add command -label Help -command "display_help"
$m add command -label About -command "splashscreen ok"

#
# Misc buttons, listboxes and scrollbars...
#
button .apply -text "Apply\nSelections" -command siftfiles
#
label .labobs -text "ObsId List"
scrollbar .yscrobs -command ".lstobs yview"
listbox .lstobs \
    -yscrollcommand [list Scroll_Set .yscrobs [list grid .yscrobs -row 1 -column 1 -sticky ns]] \
    -setgrid 1 -height 6 -width 17 -selectmode extended
#
label .labfil -text "File List"
scrollbar .yscrfil -command ".lstfil yview"
scrollbar .xscrfil -command ".lstfil xview" -orient horizontal
listbox .lstfil \
    -yscrollcommand [list Scroll_Set .yscrfil [list grid .yscrfil -row 1 -column 3 -sticky ns -rowspan 2]] \
    -xscrollcommand [list Scroll_Set .xscrfil [list grid .xscrfil -row 3 -column 2 -sticky ew]] \
    -setgrid 1 -height 12 -width 63 -selectmode extended

#
# This frame contains entry boxes plus associated labels 
# and buttons as well as the "Quit" button
#
frame .entry
#
label .entry.sclab -justify left -text "Archive device or file"
entry .entry.scebx -width 12
bind .entry.scebx <Return> maketoc
button .entry.scbut -text "Scan" -command "maketoc"
#
set extbyt 0
set exlab [list Size: [expr $extbyt / 1024] kb]
label .entry.exlab -width 16 -anchor e -textvariable exlab
button .entry.exbut -text "Extract" -command "unpack"
#
button .entry.quit -text Quit -command {wm withdraw . ; exit}

#
grid .entry.sclab -row 0 -column 0 -sticky w
grid .entry.scebx -row 0 -column 1 -sticky w
grid .entry.scbut -row 0 -column 2 -sticky w
grid columnconfigure .entry 3 -weight 1 -minsize 30
grid .entry.exlab -row 0 -column 4 -sticky e
grid .entry.exbut -row 0 -column 5 -sticky w
grid columnconfigure .entry 6 -weight 1 -minsize 30 ;# keeps "Quit" pushed east
grid .entry.quit -row 0 -column 7 -sticky e

#
# Grid everything into the toplevel (including .entry frame)
#
. configure -menu .menu
grid .labobs -row 0 -column 0 
grid .labfil -row 0 -column 2
grid .lstobs -row 1 -column 0 -sticky ns
grid .yscrobs -row 1 -column 1 -sticky ns
grid .lstfil -row 1 -column 2 -sticky news -rowspan 2
grid .yscrfil -row 1 -column 3 -sticky ns -rowspan 2
grid rowconfigure . 2 -weight 1 ;# -minsize 70
grid .xscrfil -row 3 -column 2 -sticky ew
grid .entry -row 4 -sticky ew -columnspan 4
grid columnconfigure . 2 -weight 1
grid rowconfigure . 1 -weight 1
grid .apply -row 2 -rowspan 2 -column 0 -columnspan 2

#
# Bindings
#
bind .lstobs <ButtonRelease-1> {
    set obsel 3
}
bind .lstfil <ButtonRelease-1> {
    set filechoice sel
}

proc select_obs {} {
    global obsel

    switch -exact -- $obsel {
	0 { ;# all
	    .lstobs selection clear 0 end
	}
	1 { ;# non-slews
	    set obsids [.lstobs get 0 end]
	    for {set i 0} {$i < [.lstobs size]} {incr i} {
		if {![string match *\[AZ\] [lindex $obsids $i]]} {
		.lstobs selection set $i
		} else {
		    .lstobs selection clear $i
		}
	    }
	}
	2 { ;# slews
	    set obsids [.lstobs get 0 end]
	    for {set i 0} {$i < [.lstobs size]} {incr i} {
		if {[string match *\[AZ\] [lindex $obsids $i]]} {
		    .lstobs selection set $i
		} else {
		    .lstobs selection clear $i
		}
	    }
	}
		default {puts "this should never happen!"}
    }
}

proc select_files {} {
    global app_stdp ; global app_hk ; global app_cal
    global app_pcastd ; global app_hxtstd 
    global app_pcacus ; global app_hxtcus
    global obsidregexp

    set i -1
    foreach name [.lstfil get 0 end] {
	incr i
	set subsysidx [expr [lsearch -regexp [split $name "/"] $obsidregexp] + 1]
	set subsysnam [lindex [split $name "/"] $subsysidx]
	# always keep index files and 00README
	if {[string match F* $subsysnam] || [string match 00* $subsysnam]} {
	    .lstfil selection set $i
	}
	set filename [lindex [split $name "/"] [expr $subsysidx + 1]]
	if {$app_cal && ([string compare $subsysnam "cal"] == 0)} {
	    .lstfil selection set $i
	    continue
	}
	if {$app_stdp && [string match FP* $filename]} {
	    .lstfil selection set $i
	    continue
	}
	if {$app_stdp && ([string compare $subsysnam "stdprod"] == 0)} {
	    .lstfil selection set $i
	    continue
	}
	if {$app_hk && \
		([string match FS5\[45\]* $filename] || [string match FH* $filename])} {
	    .lstfil selection set $i ;# including hexte appids 84 and 85 with Hk 
	    continue
	}
	if {$app_pcastd && \
		([string match FS46* $filename] || [string match FS4a* $filename])} {
	    .lstfil selection set $i
	    continue
	}
	if {$app_hxtstd && \
		([string match FS52* $filename] || [string match FS58* $filename])} {
	    .lstfil selection set $i
	    continue
	}
	if {$app_pcacus && \
		([string match FS37* $filename] || [string match FS3b* $filename] || \
		     [string match FS3f* $filename] || [string match FS4f* $filename])} {
	    .lstfil selection set $i
	    continue
	}
	if {$app_hxtcus && \
		([string match FS50* $filename] || [string match FS56* $filename])} {
	    .lstfil selection set $i
	    continue
	}
    }
}

proc maketoc {} {
    global tapetoc
    global toc
    global target
    global tarprog
    global obsel; global filechoice ; global exlab
    global fnam
#
# If this routine has been called we assume that a new tar
# file (or tape) is going to be read so we reset important stuff
#
# But first let's confirm that they really want to rescan if the
# archive device hasn't changed...
#
    set choice ok
    set targnew [string trimright [string trimleft [.entry.scebx get]]]
    if {[array exists toc] && ($target == $targnew)} {
	set choice [tk_messageBox -type okcancel -default ok \
			-message "Really rescan $targnew?" \
			-icon question]
	if {$choice == "ok"} {
	    unset toc
	    .lstobs delete 0 end
	    .lstfil delete 0 end
	    set extbyt 0
	    set exlab [list Size: [expr $extbyt / 1024] Kb]
	}
    }
    if {$choice == "cancel"} {return}
    if [array exists toc] {unset toc} ;# user has entered a new device/file
    set obsel 0
    set filechoice all ; file_sanity all

    set target [string trimright [string trimleft [.entry.scebx get]]]
    
#
# Let's find GNU tar (required for remote device access and other options)
#
    set tars {gnutar gtar tar}
    foreach candidate $tars {
	if [catch {exec which $candidate} spud] {
	    continue
# on some systems "which" returns 1 even when nothing is found
# (counterintuitively, I think) but tartest handles it anyway
	} else {
	    if [tartest $candidate] {
		set tarprog $candidate
		break
	    }
	}
    }
    if {![info exists tarprog]} {
	working_cursor off
	set errmsg "GNU tar could not be found."
	set errmsg "$errmsg Consult documentation for further details."
	error $errmsg
    }

#
# Do we have a valid target device or file?
# If so, let's get a table of contents...
# (allowing HOSTNAME:F without check!)
#
    if {[file exists $target] || [string match *:* $target]} {
	working_cursor on
	placard_display "Scanning Archive File or Tape\n\nThis may take a while...."
	switch -glob -- $target {
	    *\.tgz -
	    *\.tar.gz {set taropts tvzf}
	    default {set taropts tvf}
	}
#	if [catch {exec $tarprog $taropts $target} tapetoc] {
#	    working_cursor off
#	    error "error in reading $target; check filename: $tapetoc"
#	}
	set tocfid [open "|$tarprog $taropts $target" r]
	while {![eof $tocfid]} {
	    update
	    gets $tocfid line
	    if {![eof $tocfid]} {
		append tapetoc "$line\n"
		if {[placard_present]} {
		    parse_file_name $line
		    placard_eval "set dyntxt $fnam"
		}
	    }
	}
	siftfiles ;# display the output
	.menu.file entryconfigure "Apply ObsId/AppId Criteria" -state normal
	working_cursor off
    } else {
	switch -glob -- $target {
	    "/dev*" { error "Device \"$target\" not found"}
	    "?*" { error "File \"$target\" not found"}
	    default { error "No tape device or filename specified"}
	}
    }
}

#
# This takes the sifted table of contents (toc) and extracts
# the remaining files
#
proc unpack {} {
    global tapetoc
    global toc
    global tarprog
    global target
    global svlist

    if {![llength [array names toc]]} {
	error "File List empty: nothing to extract"
    } ;# an empty file list causes all files to be extracted!

    siftfiles ;# in case they didn't apply criteria first...

    working_cursor on
    placard_display "Extracting files\n\nThis may take a while..."
    set tmpfile filelist_[pid]
    if [catch {open $tmpfile w} fileid] {
	working_cursor off
	error "Cannot open $filelist: $fileid"
    } else { ;# write the filelist in same order as on tape (does this help?)
#	foreach item [split $tapetoc "\n"] {
#	    set file [lindex $item 7]
#	    foreach name [array names toc] { ;# selected files only
#		if [string match $name $file] {
#		    puts $fileid $name
#		}
#	    }
#	}
	foreach name [array names toc] { ;# selected files only
	    puts $fileid $name
	}
	close $fileid
	switch -glob -- $target {
	    *\.tgz -
	    *\.tar.gz {
		set tarcmd [list $tarprog --extract --ungzip --verbose --files-from=$tmpfile --file=$target]
	    }
	    default {
		set tarcmd [list $tarprog --extract --verbose --files-from=$tmpfile --file=$target]
	    }
	}
	# can't catch the following for some reason, let's hope it works...
#       eval exec $tarcmd
	set extrfid [open "|$tarcmd" r]
	while {![eof $extrfid]} {
	    update
	    gets $extrfid line
	    if {![eof $extrfid] && [placard_present]} {
		placard_eval "set dyntxt {[list Extracting: [lindex $line end]]}"
	    }
	}
	if {!$svlist} {exec rm -f $tmpfile}
	working_cursor off
    }
}

proc siftfiles {} {
    global obsel
    global selobs ; global selfil
    global filechoice
    global app_stdp ; global app_hk ; global app_cal
    global app_pcastd ; global app_hxtstd 
    global app_pcacus ; global app_hxtcus
    global tapetoc
    global exlab
    global toc
    global obsidregexp fnam fsiz

    
#
# Make sure something has been read in first
#
    if {![info exists tapetoc]} {
	error "Cannot apply selections: nothing scanned yet!"
    }

    working_cursor on
    placard_display "Applying selection criteria\n\nPlease stand by..."
#
# Fill file name and size arrays from the t.o.c.
#
    if {[placard_present]} {
	placard_eval "set dyntxt {Reading table of contents...}"
    }
    foreach item [split $tapetoc "\n"] {
	if {[llength $item] > 0} {
	    parse_file_name $item
	    set toc($fnam) $fsiz
	}
    }

#
# Eliminate zero-length files
#
#   foreach {name size} [array get toc] {
#	if {($size == 0)} {
#	    unset toc($name)
#	}
#    }
#
# Eliminate directory entries to avoid multiple 
#  extraction since we're going to use a list of files...
#
    if {[placard_present]} {
	placard_eval "set dyntxt {Evaluating files...}"
    }
    foreach name [array names toc] {
	if {[string match *\/ $name]} {
	    unset toc($name)
	}
    }

#
# Pick up the selected obsids if needed
# (Need to do this before clearing listbox!)
# selobs is now global so check for it too...
#
    if {$obsel == 3} {
	if {[llength [.lstobs curselection]] == 0} {
	    if {([info exists selobs] == 0) || ([llength $selobs] == 0)} {
		working_cursor off
		error "Nothing selected from ObsId List"
	    }
	} else {
	    if {[info exists selobs]} {unset selobs}
	    foreach index [.lstobs curselection] {
		lappend selobs [.lstobs get $index]
	    }
	}
    }
    
#
# Strip out ObsId names and display unique ObsId List
#
    if {[placard_present]} {
	placard_eval "set dyntxt {Identifying ObsIds...}"
    }
    foreach {name size} [array get toc] {
	lappend obstmp [lindex [split $name "/"] \
			    [lsearch -regexp [split $name "/"] $obsidregexp]]
    }
    #nifty Perl trick to simulate "uniq" follows (thanks to Larry)...
    foreach i $obstmp {
	set a($i) 1
    }

    .lstobs delete 0 end
    foreach obsid [lsort [array names a]] {
	switch -- $obsel {
	    0 { ;# all
		.lstobs insert end $obsid
	    }
	    1 { ;# non-slews only
		if {![string match *\[AZ\] $obsid]} {
		    .lstobs insert end $obsid
		}
		foreach name [array names toc] {
		    set testobs [lindex [split $name "/"] \
				     [lsearch -regexp [split $name "/"] $obsidregexp]]
		    if {[string match *\[AZ\] $testobs]} {
			unset toc($name)
		    }
		}
	    }
	    2 { ;# slews only
		if {[string match *\[AZ\] $obsid]} {
		    .lstobs insert end $obsid
		}
		foreach name [array names toc] {
		    set testobs [lindex [split $name "/"] \
				     [lsearch -regexp [split $name "/"] $obsidregexp]]
		    if {![string match *\[AZ\] $testobs]} {
			unset toc($name)
		    }
		}
	    }
	    3 { ;# selected obsid(s)
		if {[lsearch -exact $selobs $obsid] == -1} {
		    foreach name [array names toc] {
			set testobs [lindex [split $name "/"] \
					 [lsearch -regexp [split $name "/"] $obsidregexp]]
			if {![string compare $testobs $obsid]} {
			    unset toc($name)
			}
		    }
		} else {
		    .lstobs insert end $obsid
		}
	    }
	    default {puts "this should never happen!"}
	}
    }
    
#
# Apply AppId criteria
#
    if {[placard_present]} {
	placard_eval "set dyntxt {Identifying AppIds...}"
    }
    switch -exact -- $filechoice {
	sfc {
	    foreach name [array names toc] {
		set subsysidx [expr [lsearch -regexp [split $name "/"] $obsidregexp] + 1]
		set subsysnam [lindex [split $name "/"] $subsysidx]
		if {[string match F* $subsysnam]} continue ;# always keep index files
		switch -exact -- $subsysnam { ;# always discard these subsystem directories
		    ace -
		    eds -
		    fds -
		    gsace -
		    ifog -
		    ipsdu -
		    pse -
		    spsdu { 
			unset toc($name)
			continue
		    }
		    default {}
		}
		set filename [lindex [split $name "/"] [expr $subsysidx + 1]]
		if {!$app_cal && ([string compare $subsysnam "cal"] == 0)} {
		    unset toc($name)
		    continue
		}
		if {!$app_stdp && [string match FP* $filename]} {
		    unset toc($name)
		    continue
		}
		if {!$app_stdp && !$app_hk && ([string compare $subsysnam "stdprod"] == 0)} {
		    unset toc($name)
		    continue
		}
		if {!$app_hk && \
			([string match FS5\[45\]* $filename] || [string match FH* $filename])} {
		    unset toc($name) ;# including hexte appids 84 and 85 with Hk 
		    continue
		}
		if {!$app_pcastd && \
			([string match FS46* $filename] || [string match FS4a* $filename])} {
		    unset toc($name)
		    continue
		}
		if {!$app_hxtstd && \
			([string match FS52* $filename] || [string match FS58* $filename])} {
		    unset toc($name)
		    continue
		}
		if {!$app_pcacus && \
			([string match FS37* $filename] || [string match FS3b* $filename] || \
			     [string match FS3f* $filename] || [string match FS4f* $filename])} {
		    unset toc($name)
		    continue
		}
		if {!$app_hxtcus && \
			([string match FS50* $filename] || [string match FS56* $filename])} {
		    unset toc($name)
		    continue
		}
	    }
	}
	sel {
	    foreach index [.lstfil curselection] {
		lappend selfil [.lstfil get $index]
	    }
	    foreach name [array names toc] {
		set match 0
		foreach file $selfil {
		    if {[string match $file $name]} {
			set match 1
		    }
		}
		if {!$match} {
		    unset toc($name)
		} 
	    }
	}
	all {
	}
    }
    
#
# Compute total size and display File List
#
    if {[placard_present]} {
	placard_eval "set dyntxt {Computing Size...}"
    }
    set extbyt 0
    .lstfil delete 0 end
    foreach name [lsort [array names toc]] {
	set extbyt [expr $extbyt + $toc($name)]
	.lstfil insert end $name
    }
    set exlab [list Size: [expr $extbyt / 1024] Kb]

    working_cursor off
}

#
# from Welch (2nd ed.) p.347
#
proc Scroll_Set {scrollbar geoCmd offset size} {
    if {$offset != 0.0 || $size != 1.0} {
	eval $geoCmd ;# Make sure it's visible
	$scrollbar set $offset $size
    } else {
	set manager [lindex $geoCmd 0]
	$manager forget $scrollbar ;# hide it
    }
}

#
# This ensures that if any of the appid checkbuttons (besides "All")
# are selected then "All" will be deselected. If none of the others
# are selected then "All" will be selected (confusing, huh?)
#
# The argument to the procedure is either "all" (if called from the
# "ALL Files" button) or else "other" (if any of the others).
#
proc file_sanity {x} {
    global filechoice
    global app_stdp ; global app_hk ; global app_cal
    global app_pcastd ; global app_hxtstd 
    global app_pcacus ; global app_hxtcus

    switch -- $x {
	specific {
	    set filechoice all
	    if {$app_stdp || $app_hk || $app_cal || $app_pcastd || \
		    $app_hxtstd || $app_pcacus || $app_hxtcus} {
		set filechoice sfc
		select_files
	    }
	}
	all {
	    if {$filechoice != "all"} {
		set filechoice all
	    } else {
		set filechoice all 
		set app_stdp 0 ; set app_hk 0 ; set app_cal 0
		set app_pcastd 0 ; set app_hxtstd 0
		set app_pcacus 0 ; set app_hxtcus 0
	    }
	}
	selected {
	    if {$filechoice != "sel"} {
		set filechoice sel
	    } else {
		set filechoice sel 
		set app_stdp 0 ; set app_hk 0 ; set app_cal 0
		set app_pcastd 0 ; set app_hxtstd 0
		set app_pcacus 0 ; set app_hxtcus 0
	    }
	}
    }
}

#
# tests to see if progname is, in fact, GNU tar
#
proc tartest {progname} {
#    if [catch {exec $progname --version} result] {
#	return 0
#    } else {
#
# the above clause is just too clever -- for example,
# GNU tar v1.11.2 helpfully returns its version number
# on stderr instead of stdout! Just grab result (whether
# it's stderr or stdout) and check it...
#
    catch {exec $progname --version} result
    if {[lsearch [list $result] *GNU*] != -1} {
	return 1
    } else {
	return 0
    }
#   }
}

#
# Turn the cursor to a watch face when busy
#  and back to normal when done
#
proc working_cursor {status} {

    switch -- $status {
	on {
	    .entry.scebx configure -cursor watch
	    . configure -cursor watch
	    .menu configure -cursor watch
	    update idletasks
	}
	off {
	    .entry.scebx configure -cursor xterm
	    . configure -cursor {}
	    .menu configure -cursor {}
	    update idletasks
	    placard_destroy
	}
	default {}
    }
}

#
# "placard" concepts lifted from Harrison & McLennan
#
proc placard_create {script} {
    global placard

    if {![placard_present]} {
	set placard [open "|wish8.0" w]
	puts $placard {
	    after idle {
		update idletasks
		set maxw [winfo screenwidth .]
		set maxh [winfo screenheight .]
		set x0 [expr ($maxw-[winfo reqwidth .])/2]
		set y0 [expr ($maxh-[winfo reqheight .])/2]
		wm geometry . "+$x0+$y0"
		wm title . "XTE Selective UnTar (2)"
		wm iconname . xsut(2)
	    }
	}
	puts $placard $script
	flush $placard
    }
}
					 
proc placard_destroy {} {
    global placard

    update
    catch {puts $placard "exit"}
    catch {flush $placard}
    catch {close $placard}
}

proc placard_display {msg} {
    global env placard

    set script {
	frame .info -relief ridge -borderwidth 3
	label .info.lab -textvariable dyntxt -width 60 -anchor w
	pack .info.lab -side left -anchor sw
	catch {.info.lab configure \
		   -font -*-helvetica-medium-r-normal--*-120-*}
	pack .info -fill x -side bottom -anchor sw
	set file [file join $env(XSUT_LIBRARY) xte_sm.gif]
	set imh [image create photo -file $file]
	label .pic -image $imh
	pack .pic -side left -padx 8 -pady 8
	label .title -text "XTE Selective Untar Tool"
	pack .title -fill x -padx 8 -pady 8
	catch {.title configure \
		   -font -*-helvetica-bold-o-normal--*-140-*}
    }
    append script "label .status -text {$msg}"
    append script {
	pack .status -fill x -pady 8
	catch {.status configure \
		   -font -*-helvetica-medium-r-normal--*-120-*}
    }
    placard_create $script
}

proc placard_eval {script} {
    global placard

    catch {puts $placard $script}
    catch {flush $placard}
}

proc placard_present {} {
    global placard

    update      
    if ![catch {pid $placard}] {
    	return 1
    } else {
	return 0
    }
}

proc splashscreen {button} {
    global version appname revdate

    toplevel .splashscreen
    wm title .splashscreen "About $appname"

    after idle {
	update idletasks
	set x [expr [winfo rootx .]+50]
	set y [expr [winfo rooty .]+50]
	wm geometry .splashscreen "+$x+$y"
    }

    label .splashscreen.version -text "$appname version - $version"
    label .splashscreen.date -text "Last revision: $revdate"

    grid .splashscreen.version -padx 60
    grid .splashscreen.date

    if {$button == "ok"} {
        button .splashscreen.ok -text "OK" -command "destroy .splashscreen"
        grid .splashscreen.ok
    }
}

proc parse_file_name {line} {
    global fnam fsiz
#
# different versions of GNU tar give
# differently parsed output but we're counting
# on the third (blank-delimited) field being
# the file size and the file name being either
# last, or in the case of symbolic links, third
# from the end
#
    switch -glob -- $line {
	"*->*" {set fnam [lindex $line [expr [llength $line] - 3]]}
	default {set fnam [lindex $line end]}
    }
    set fsiz [lindex $line 2]
}

proc display_help {} {
    global appname env

    toplevel .help
    wm title .help "[string toupper $appname] Help"

    after idle {
	update idletasks
	set x [expr [winfo rootx .]+50]
	set y [expr [winfo rooty .]+50]
	wm geometry .help "+$x+$y"
    }

    button .help.dismiss -text Done -command "destroy .help"
    pack .help.dismiss -side bottom

    text .help.text -relief raised -bd 2 \
	-yscrollcommand ".help.scroll set"
    scrollbar .help.scroll -command ".help.text yview"
    pack .help.scroll -side right -fill y
    pack .help.text -side left


    proc loadFile file {
	.help.text delete 1.0 end
	set f [open $file]
	while {![eof $f]} {
	    .help.text insert end [read $f 1000]
	}
	close $f
    }
    loadFile [file join $env(XSUT_LIBRARY) xsut_help.txt]
}