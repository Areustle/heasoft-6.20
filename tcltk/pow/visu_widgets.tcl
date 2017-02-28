if { [info exists env(BLT_LIBRARY)] == 0 } {
    set has_blt 0
} else {
    set has_blt 1
}

set _x_ 0
set _y_ 0
set _wid_ 0
set _len_ 0
set _DispMax_ 0
set _DispMin_ 0
set _userDispMax_ 0
set _userDispMin_ 0
set _thres_incr_ 1
set _thres_res_ 1
set _gc_ "or"
set _index_ 0
set _init_index_ 0
set _toggle_gc_ 0
set zoomx 2
set zoomy 2
set fromx1 0
set fromy1 0
set fromx2 0
set fromy2 0
set ONodeX(0) 0
set ONodeY(0) 0
set nb 0
set win_nb 0
set curr_nb_slices 0
set curr_filename {}
set nb_slices 0
set scale_slice 0
set slice_nb 0
set img_nb 0
set palette_button 0
set palette_mapped 0
set cmap gray
set curr_min 0
set curr_max 255
set curr_color "intensity"
foreach k {intensity red green blue} {
    set col_npoints($k) 2
}
unset k
set Private_Colormap 1 
# set def_font "-*-helvetica-*-r-normal-*-14-*-*-*-*-*-*-*"
#set visu_help_file $visu_library/../../man/html/tutorial.html
set dir "."
set file ""
set pattern "*.sdt"
set showdot 0
# option add *font $def_font


proc destroywin win {
    global img_array win_array curr_img title_array color_bar_img


    if { [info exists img_array($win.c)] } {
	unset title_array($win)
	if { $curr_img == $img_array($win.c) } {
	    unset win_array($img_array($win.c))
	    unset img_array($win.c)
	    set new_img [lindex [array names win_array] 0]
	    if { $new_img != {} } {
		set curr_img $new_img
		set_curr_defaults $curr_img
		set_curr_event_loop [winfo toplevel $win_array($curr_img)]
		set_active_title [winfo toplevel $win_array($curr_img)]
	    } else {
		if { [info exists color_bar_img] } {
		    set curr_img $color_bar_img
		}   
	    }
	} else {
	    unset win_array($img_array($win.c))
	    unset img_array($win.c)
	}
	catch {destroy $win}
	update idletasks
    }
}

proc delete_img img {
    upvar $img img1
    global win_array img_array filename_array nb_slices_array 
   
    if { [info exists win_array($img1)] } {
	unset img_array($win_array($img1).c)
	destroy $win_array($img1)
	unset win_array($img1)
    }
    image delete $img1
  
    unset filename_array($img1)
    unset nb_slices_array($img1)
    unset img1

    set new_img [lindex [array names win_array] 0]
    if { $new_img != {} } {
	set curr_img $new_img
	set_curr_defaults $curr_img
	set_curr_event_loop [winfo toplevel $win_array($curr_img)]
	set_active_title [winfo toplevel $win_array($curr_img)]
    } else {
	if { [info exists color_bar_img] } {
	    set curr_img $color_bar_img
	}   
    }
}

proc topConfig { win w h } {
	global _wid_ _len_ sbwidth

    if { [catch {pack slaves $win} msg] > 0} {
	puts "error in topConfig : $msg"
	return
    }
    if { [lsearch [pack slaves $win] "$win.xscroll"] == -1 } {
	set hasX 0
    } else  {
	set hasX 1
    }
    if { [lsearch [pack slaves $win] "$win.yscroll"] == -1 } {
	set hasY 0
    } else  {
	set hasY 1
    }
    if {$w >= $_wid_} {
	if {$hasX} {
	    catch {pack forget $win.xscroll}
	    set hasX 0
	}
    } else {
	if {!$hasX} {
	    catch {pack $win.xscroll -before $win.c -side bottom -expand 1 -fill x}
	    set hasX 1
	}
    }
    if {$h >= $_len_+$hasX*$sbwidth} {
	if {$hasY} {
	    catch {pack forget $win.yscroll}
	    set hasY 0
	}
    } else {
	if {!$hasY} {
	    catch {pack $win.yscroll -before $win.c -side right -expand 1 -fill y}
	    set hasY 1
	}
    }
    #
    # Check again since we now might need the vertical scrollbar
    # after all.
    #
    if {$w >= $_wid_+$hasY*$sbwidth} {
	if {$hasX} {
	    catch {pack forget $win.xscroll}
	    set hasX 0
	}
    } else {
	if {!$hasX} {
	    catch {pack $win.xscroll -before $win.c -side bottom -expand 1 -fill x}
	    set hasX 1
	}
    }
}

proc snap2pict {img1 img2} {
    upvar $img1 in
    upvar $img2 out
    global scale_slice _wid_ _len_ win_array curr_img filename_array curr_filename nb_slices_array nb_slices curr_nb_slices curr_slice_array seq_array sbwidth

    if { ![info exists in] } {
	puts "source Pict image does not exists"
	return
    }
    if { ![info exists out] } {
	set out [image create pict]
    }
    $in snap2pict $out
    set filename_array($out) "snap2pict"
    set curr_filename "snap2pict"
    set nb_slices_array($out) 1
    set seq_array($out) 0
    set curr_slice_array($out) 0
    set curr_nb_slices 1
    set curr_img $out

    if { [ info exists win_array($out)] } { 
	set _wid_ [image width $out]
	set _len_ [image height $out]
	$win_array($out).c configure -width $_wid_ -height $_len_ 
	$win_array($out).c configure -scrollregion "0 0 $_wid_ $_len_" -confine 1
	wm maxsize $win_array($out) $_wid_ $_len_
	wm geometry $win_array($out) [format "%dx%d" $_wid_ $_len_]
	set sbwidth [winfo reqwidth $win_array($out).yscroll]
	topConfig $win_array($out) $_wid_ $_len_
    }
}

proc snap2photo {img1 img2} {
    upvar $img1 in
    upvar $img2 out
   
    if { ![info exists in] } {
	puts "source Pict image does not exists"
	return
    }
    if { ![info exists out] } {
	set out [image create photo -palette 10/10/10]
    }
    set g [image create photo]
    $in snap2photo $out
}

proc disp_photo {img1} {
    upvar $img1 in
    global win_nb

    if { ![info exists in] } {
	puts "Pict image does not exists"
	return
    }
    set win_nb [expr $win_nb+1]
    set win ".win$win_nb"
    catch {toplevel $win  -cursor {crosshair green}}
   
    canvas $win.c -width [image width $in] -height [image height $in]
    $win.c create image 0 0 -image $in -anchor nw
    pack $win.c -expand 1 -fill both -anchor nw
}


proc disp_priv img {
    upvar $img img1
    global Private_Colormap 

    if { [info exists img1] } {
	set Private_Colormap 3
	disp img1
    }
}

proc disp img {
    upvar $img img1
    global win_array win_nb filename_array title_array _wid_ _len_ sbwidth img_array curr_img 
   
    if { ![ info exists win_array($img1)] } {
	set win_nb [expr $win_nb+1]
	set win ".win$win_nb"
	catch {toplevel $win  -cursor {crosshair green}}
	set title_array($win) "<$win_nb>: "
	
	set _wid_ [image width $img1]
	set _len_ [image height $img1]
	
	wm maxsize $win $_wid_ $_len_
	canvas $win.c -width $_wid_ -height $_len_ -borderwidth 0 -highlightthickness 0 -cursor {crosshair green}
	$win.c create image 0 0 -anchor nw -image $img1
	pack $win.c -side top -expand 0 -fill none -in $win

	scrollbar $win.yscroll -orient vertical \
		-command [list $win.c yview] -width 12
	scrollbar $win.xscroll -orient horizontal \
		-command [list $win.c xview] -width 12

	set sbwidth [winfo reqwidth $win.yscroll]

	pack $win.yscroll -side right -fill y -expand 0 -after $win.c
	pack $win.xscroll -side bottom -fill x -expand 0 -after $win.c

	$win.c configure -yscrollcommand "$win.yscroll set" \
		-xscrollcommand "$win.xscroll set" 
	$win.c config -scrollregion "0 0 $_wid_ $_len_" -confine 1
	

	set img_array($win.c) $img1
	set win_array($img1) $win
	
    }
    if {[info exists curr_img] && [info exists win_array($curr_img)]} {
	set_curr_event_loop [winfo toplevel $win_array($curr_img)]
    }
    set_curr_defaults $img1
    set_curr_event_loop $win_array($img1)
    set_active_title $win_array($img1)
    
}



proc set_active_title win {
    global title_array img_array filename_array seq_array curr_slice_array 

    foreach l [array names img_array] {
	set w [winfo toplevel $l]
	if { $seq_array($img_array($w.c)) == 1 } {
	    wm title $w "$title_array($w) [file tail $filename_array($img_array($w.c))]$curr_slice_array($img_array($w.c)).sdt"
	} else {
	    wm title $w "$title_array($w) [file tail $filename_array($img_array($w.c))]:$curr_slice_array($img_array($w.c))"
	}
    }
    if { $seq_array($img_array($win.c)) == 1 } {
	wm title $win "<***> [file tail $filename_array($img_array($win.c))]$curr_slice_array($img_array($win.c)).sdt ACTIVE"
    } else {
	wm title $win "<***> [file tail $filename_array($img_array($win.c))]:$curr_slice_array($img_array($win.c)) ACTIVE"
    }    
}

proc update_active_title win {
    global title_array img_array filename_array seq_array curr_slice_array 

    if { $seq_array($img_array($win.c)) == 1 } {
	wm title $win "<***> [file tail $filename_array($img_array($win.c))]$curr_slice_array($img_array($win.c)).sdt ACTIVE"
    } else {
	wm title $win "<***> [file tail $filename_array($img_array($win.c))]:$curr_slice_array($img_array($win.c)) ACTIVE"
    }    
}


proc set_curr_defaults img {
    global curr_img img_array curr_filename filename_array curr_nb_slices nb_slices_array scale_slice curr_slice_array _wid_ _len_ _DispMin_ _DispMax_

    set curr_img $img
    set curr_filename $filename_array($curr_img)
    set curr_nb_slices $nb_slices_array($curr_img)
    set scale_slice $curr_slice_array($curr_img) 
    if { $scale_slice > $curr_nb_slices } {
	set scale_slice $curr_nb_slices
    }
    set _len_ [image height $curr_img]
    set _wid_ [image width $curr_img]
    set code [catch {.slice configure -to [expr $nb_slices_array($curr_img)-1]} string]
    if {$code>0} {
	return
    }
    set _DispMin_ [$curr_img getmin]
    set _DispMax_ [$curr_img getmax]
    update_scale
}

proc set_curr_event_loop win {
    global curr_img _wid_ _len_ _x_ _y_ _pix_ img_array

    bind $win <Destroy> {
	set win [winfo toplevel %W]
	bind $win <Destroy> {} 
	bind $win.c <Destroy> {}
	destroywin $win 
    }
    bind $win.c <Destroy> {
	set win [winfo toplevel %W]
	bind $win <Destroy> {}	
	bind $win.c <Destroy> {}
	destroywin $win 
    }

    bind $win <Configure> "topConfig $win %w %h"
    
    bind $win <Enter> {
	set win [winfo toplevel %W]
	if { ![info exists curr_img] } {
	    set_curr_defaults $img_array($win.c)
	    set_active_title $win
	}
    }

    bind $win.c <Enter> {
	set win [winfo toplevel %W]
	if { ![info exists curr_img] } {
	    set_curr_defaults $img_array($win.c)
	    set_active_title $win
	}
    }
    bind $win.c <Button-1> {
	# reset the events in curr_img if we are in overlay paint mode
	if { [info exists curr_img] && [info exists  win_array($curr_img)] } {
	    set_curr_event_loop  [winfo toplevel $win_array($curr_img)]
	}
	set can %W
	set win [winfo toplevel %W]
	set_curr_defaults $img_array($win.c)
	set_active_title [winfo toplevel %W]
	set _x_ [expr int([$can canvasx %x])]
	set _y_ [expr int([$can canvasy %y])]
	if {  ($_x_>0) && ($_x_<$_wid_) && ($_y_>0) && ($_y_<$_len_) } {
	    set _pix_ [$curr_img get $_x_ $_y_]
	}
    }
    bind $win.c <B1-Motion> {
	set can %W
	set _x_ [expr int([$can canvasx %x])]
        set _y_ [expr int([$can canvasy %y])]
        if { ($_x_>0) && ($_x_<$_wid_) && ($_y_>0) && ($_y_<$_len_) } {
           set _pix_ [$curr_img get $_x_ $_y_] 
        }
    }      
    bind $win.c <Motion> {
        set _x_ [expr int([%W canvasx %x])] 
	set _y_ [expr int([%W canvasy %y])]
        update idletasks
        if { ($_x_>0) && ($_x_<$_wid_) && ($_y_>0) && ($_y_<$_len_) } {
           set _pix_ [$curr_img get $_x_ $_y_] 
        }
    } 
    bind $win.c <ButtonRelease-1> {}
    bind $win.c <Button-2> {}
    bind $win.c <B2-Motion> {} 
    bind $win.c <ButtonRelease-2> {}
    bind $win.c <Button-3> {}
}


proc rdfile { file img {args {}} } {
    upvar $img img1
   
    global scale_slice _wid_ _len_ win_array filename_array slice_nb \
	    nb_slices_array nb_slices curr_nb_slices curr_slice_array seq_array sbwidth
  
    if { $file == {} } {
	return
    }
    if { ![info exists img1] } {
	set new 1
    } else { set new 0 }
    
    set seqbase [string range $file 0 [string last _ $file]]
    set seqend [string range $file [string last _ $file] end]
    
    if {$new == 1} { 
	set img1 [image create pict]
    }
	
    if { $args != {} } {
	eval { $img1 read $file -shrink } $args
    } else {
	$img1 read $file -shrink
    }
    if { [scan $seqend "_%d.sdt" seqnum]==1 } {
	set slice_nb 0
	set scale_slice $seqnum
	set filename_array($img1) $seqbase
	set nb_slices_array($img1) [llength [glob [format "%s*.sdt" $seqbase]]]
	set seq_array($img1) 1
	set curr_slice_array($img1) $seqnum
    } else {
	set slice_nb 0
	set scale_slice 0
	set filename_array($img1) $file
	set nb_slices_array($img1) $nb_slices
	set seq_array($img1) 0
	set curr_slice_array($img1) 0
    }

    if { [ info exists win_array($img1)] } {
	set _wid_ [image width $img1]
	set _len_ [image height $img1]
	$win_array($img1).c configure -width $_wid_ -height $_len_ 
	$win_array($img1).c configure -scrollregion "0 0 $_wid_ $_len_" -confine 1
	wm maxsize $win_array($img1) $_wid_ $_len_
	wm geometry $win_array($img1) [format "%dx%d" $_wid_ $_len_]
	set sbwidth [winfo reqwidth $win_array($img1).yscroll]
	topConfig $win_array($img1) $_wid_ $_len_
	# reset the events in curr_img if we are in overlay paint mode
	if { [info exists curr_img] && [info exists win_array($curr_img)] } {
	    set_curr_event_loop  [winfo toplevel $win_array($curr_img)]
	}
	set_curr_defaults $img1
    }
}

proc rdbinary {img access file  {args {}} } {
    global scale_slice _wid_ _len_ win_array filename_array slice_nb nb_slices_array nb_slices curr_nb_slices curr_slice_array seq_array sbwidth
    
    upvar $img img1
    if { $file == {} || $access == {} } {
	puts "Usage rdbinary img -file|channel name \[-width -height -skip\]"
	return
    }
    if { ![info exists img1] } {
	set new 1
    } else { 
	set new 0 
    }
    
    if { [string compare "-channel" $access] } {
	set has_file 1
    } elseif  { [string compare "-file" $access] } {
	set has_file 0
    } else {
	puts  "Usage rdbinary img -file|channel name \[-width -height -skip\]"
	return
    }

    if {$has_file} {
	set seqbase [string range $file 0 [string last _ $file]]
	set seqend [string range $file [string last _ $file] end]
    }

    if {$new == 1} { 
	set img1 [image create pict]
    }

    if { $args != {} } {
	eval { $img1 rdbinary $access $file } $args
    } else {
	$img1 rdbinary $access $file
    }

    if {$has_file} {
	if { [scan $seqend "_%d.sdt" seqnum]==1 } {
	    set slice_nb 0
	    set scale_slice $seqnum
	    set filename_array($img1) $seqbase
	    set nb_slices_array($img1) [llength [glob [format "%s*.sdt" $seqbase]]]
	    set seq_array($img1) 1
	    set curr_slice_array($img1) $seqnum
	} else {
	    set slice_nb 0
	    set scale_slice 0
	    set filename_array($img1) $file
	    set nb_slices_array($img1) $nb_slices
	    set seq_array($img1) 0
	    set curr_slice_array($img1) 0
	}
    } else {
	set slice_nb 0
	set scale_slice 0
	set filename_array($img1)  $file
	set nb_slices_array($img1) $nb_slices
	set seq_array($img1) 0
	set curr_slice_array($img1) 0
    }
   
    if { [ info exists win_array($img1)] } {
	set _wid_ [image width $img1]
	set _len_ [image height $img1]
	$win_array($img1).c configure -width $_wid_ -height $_len_ 
	$win_array($img1).c configure -scrollregion "0 0 $_wid_ $_len_" -confine 1
	wm maxsize $win_array($img1) $_wid_ $_len_
	wm geometry $win_array($img1) [format "%dx%d" $_wid_ $_len_]
	set sbwidth [winfo reqwidth $win_array($img1).yscroll]
	topConfig $win_array($img1) $_wid_ $_len_ 
	# reset the events in curr_img if we are in overlay paint mode
	if { [info exists  curr_img] && [info exists  win_array($curr_img)] } {
	    set_curr_event_loop  [winfo toplevel $win_array($curr_img)]
	}
	set_curr_defaults $img1
    }
}

proc upslice {} {
    global curr_img scale_slice

    if { [info exists curr_img] } {
	set nb [expr $scale_slice+1]
	rdslice curr_img $nb
    }
}

proc downslice {} {
    global curr_img scale_slice

    if { [info exists curr_img] } {
	set nb [expr $scale_slice-1]
	rdslice curr_img $nb
    }
}

proc rdslice { img nb } {
    upvar $img img1

    global scale_slice slice_nb nb_slices_array _wid_ _len_ filename_array curr_img seq_array curr_slice_array win_array
    
    if { ![info exists img1] } { 
        #Create variable first
	return
    }   
    if { $nb<0 } {
	return
    }
    if { $nb>=$nb_slices_array($img1)} {
	return
    }
    set scale_slice $nb
    set curr_slice_array($img1) $nb
    if { $seq_array($img1)== 0 } {
	# now just read image slice $scale_slice
	set slice_nb $scale_slice
	$img1 read $filename_array($img1)
	# using rdbinary is faster, but can cause problems if the
	# images have been converted to a different type
	# comment it out if you want it
	# $img1 rdbinary -file $filename_array($img1)
	
    } else {
	set slice_nb 0
	set filenm [format "%s%d.sdt" $filename_array($img1) $nb]
	$img1 read $filenm
	# using rdbinary is faster, but can cause problems if the
	# images have been converted to a different type
	# comment it out if you want it
	# $img1 rdbinary -file $filenm
    }
    # reset the events in curr_img if we are in overlay paint mode
    if { [info exists  curr_img] && [info exists  win_array($curr_img)] } {
	set_curr_event_loop  [winfo toplevel $win_array($curr_img)]
    }
    if { [info exists win_array($img1)] } { 
	set_curr_defaults $img1
	update_active_title $win_array($img1)
    }
}

proc make_img {img} {
    upvar $img img1
    
    global filename_array nb_slices_array curr_slice_array seq_array 
    
    if { ![info exists img1] } {
	set img1 [image create pict]
	set nb_slices_array($img1) 1
	set seq_array($img1) 0
	set curr_slice_array($img1)  0
	set filename_array($img1) "make_img"
    }
}

proc savefile {} {
    global curr_img

    if { [info exists curr_img] } {
	wrfile curr_img
    }
}

proc wrfile {{img1 {}} {name {}} {args {}} } {
     global filename_array nb_slices_array curr_slice_array seq_array dir file pattern showdot 
    upvar $img1 img

    if { [info exists img] } { 
	if { $name=={} } {
	    if { $seq_array($img) == 1 } {
		set name "$filename_array($img)$curr_slice_array($img)"
	    } else {
		set name $filename_array($img)
	    }
	}
	set text [format "Do you really want \n to save file \n %s ?" $name]
	set sure [tk_dialog .d {Save to disk} $text warning 0 {cancel} {save file as} {save file} ]
	if { $sure==0 } {
	    return
	}
	if { $sure==1 } {
	    set name [fileselect .fsel "Select A File" dir file pattern showdot]
	    if { $name == {} } {
		return
	    }
	}
	if { [file exists $name] } {
	    set text [format "Overwrite file \n %s ?" $name]
	    set sure [tk_dialog .d {Save to disk} $text warning 0 {cancel} {overwrite} ]
	    if { $sure==0 } {
		return
	    }
	}
	if { $args=={} } {
	    $img write $name
	} else {
	    eval {$img write $name} $args
	}
    }
}  

proc zoom { img {zx 1} {zy 1} {subx 1} {suby 1} } {
    upvar $img img1
    global _wid_ _len_ nb_slices_array filename_array curr_img seq_array curr_slice_array
    
    if { ![info exists img1] } { 
        #Create variable first
       return
    }   
    
    set g [image create pict]
    $g copy $img1 -zoom $zx $zy -subsample $subx $subx
    set nb_slices_array($g) $nb_slices_array($img1)
    set filename_array($g) $filename_array($img1)
    set seq_array($g) $seq_array($img1)
    set curr_slice_array($g) $curr_slice_array($img1)
    unset nb_slices_array($img1)
    unset filename_array($img1)
    unset seq_array($img1)
    unset curr_slice_array($img1)
    unset img1
    set img1 $g
    unset g
    if { [ info exists win_array($img1)] } {
	set _wid_ [image width $img1]
	set _len_ [image height $img1]
	$win_array($img1).c configure -width $_wid_ -height $_len_ 
	$win_array($img1).c configure -scrollregion "0 0 $_wid_ $_len_" -confine 1
	wm maxsize $win_array($img1) $_wid_ $_len_
	wm geometry $win_array($img1) [format "%dx%d" $_wid_ $_len_]
	set sbwidth [winfo reqwidth $win_array($img1).yscroll]
	topConfig $win_array($img1) $_wid_ $_len_ 
	set_curr_defaults $img1
    } 
   
}

proc colorbar {} {
    global color_bar_img curr_img win_color_bar

    if { ![info exists color_bar_img] } {
	if { [info exists curr_img] } {
	    set _wid_ [image width $curr_img]
	    set _len_ 30
	} else {
	    set _wid_ 400
	    set _len_ 30
	}
	set color_bar_img [image create pict -width $_wid_ -height $_len_]
	$color_bar_img colorbar
    } else {
	if { [info exists curr_img] && $color_bar_img != $curr_img} {
	    if { [image width $color_bar_img] != [image width $curr_img] } {
		if { [info exists win_color_bar] } {
		    destroy .win_color_bar
		} else {
		    delete_colorbar
		}
		set _wid_ [image width $curr_img]
		set _len_ 30
		set color_bar_img [image create pict -width $_wid_ -height $_len_]
		$color_bar_img colorbar
	    }
	}
    }
    disp_colorbar 
    colormap
}



proc delete_colorbar {} {
    global color_bar_img win_color_bar curr_img

    if { $color_bar_img == $curr_img } {
	image delete $color_bar_img
	unset color_bar_img
	unset win_color_bar
	unset curr_img
    } else {
	image delete $color_bar_img
	unset color_bar_img
	unset win_color_bar
    }
}

proc disp_colorbar { } {
    global color_bar_img win_color_bar curr_img Private_Colormap
   
    if { ![ info exists win_color_bar] } {
	set win_color_bar 1
	set win ".win_color_bar"
	catch {toplevel $win}
	
	set _wid_ [image width $color_bar_img]
	set _len_ [image height $color_bar_img]
	set Private_Colormap 0
	wm maxsize $win $_wid_ [expr $_len_+30]
	wm title $win "Colorbar"
	canvas $win.c -width $_wid_ -height [expr $_len_+30] -borderwidth 0 -highlightthickness 0
	$win.c create image 0 0 -anchor nw -image $color_bar_img
	pack $win.c -side top -expand 0 -fill none -in $win
	if { ![info exists curr_img] } {
	    set curr_img $color_bar_img
	}
	$win.c create line 0 $_len_ $_wid_ $_len_
	for {set i 0} {$i<10} {incr i} {
	    set _x_ [expr $_wid_/10.0*$i]
	    set _y_ $_len_
	    $win.c create line $_x_ $_y_ $_x_ [expr $_y_+5]
	} 
	set min [$curr_img getmin]
	set max [$curr_img getmax]
	
	$win.c create line 0 $_len_ 0 [expr $_len_+10]
	$win.c create text 5 [expr $_len_+30] -text $min -anchor sw
	set _x_ [expr $_wid_/2]
	$win.c create line $_x_ $_len_ $_x_ [expr $_len_+10]
	$win.c create text $_x_ [expr $_len_+30] -text [expr ($max-$min)/2] -anchor s

	$win.c create line $_wid_ $_len_ $_wid_ [expr $_len_+10]
	$win.c create text [expr $_wid_-5] [expr $_len_+30] -text $max -anchor se
	bind $win.c <Destroy> {
	    if { $win_color_bar == 1 } {
		delete_colorbar
	    }
	}
    }
}

proc set_cmap_level {} {
    global curr_img color_bar_img Private_Colormap

    if { [info exists curr_img] } {
	if { [info exists color_bar_img] } {
	    if {$color_bar_img != $curr_img } {
		$curr_img cmap_level $Private_Colormap
	    } else {
		set Private_Colormap [$curr_img cmap_level]
	    }
	} else {
	    $curr_img cmap_level $Private_Colormap
	}
    }
}

proc movie { img {first 0} {nb 0} {offset 1}} {
    upvar $img img1
    global nb_slices_array slice_nb scale_slice filename_array win_array title_array curr_slice_array
    
    if { ![info exists img1] } { 
	#puts "Create variable first"
	return
    }   
    if {  $nb_slices_array($img1) == 0 } {
	#puts "just one image "
	return
    }
    
    if { $first > $nb_slices_array($img1) } {
	return
    }
    
    for {set i $first} {$i<$nb} {set i [expr $i+$offset]} {
	if { $i > $nb_slices_array($img1) } {
	    set scale_slice $nb_slices_array($img1)
	    set slice_nb $nb_slices_array($img1)
	} else { 
	    set scale_slice $i
	    set slice_nb $i
	}
	#read image slice $scale_slice
	$img1 rdbinary -file $filename_array($img1)
	set curr_slice_array($img1) $slice_nb
	disp img1
	update idletasks
    }
}



proc make_palette { {pal 0} } {
    global curr_img curr_color loval loval_ent hival hival_ent midthresval diffthresval coledit _cwid_ _clen_ cx cy plist pedge col_npoints palette_button curr_min curr_max _thres_incr_ _thres_res_ palette_mapped has_plb_segment keypress
	   
    if { $pal != 0 } {
	if {[catch {set pal [toplevel .pal]}] > 0} {
	    catch {wm deiconify .pal}
	    if { [catch {raise .pal}] > 0} {
		puts "window already exists and needs to be raised"
	    }   
	    return
	}
    } else {
	set pal .
    }
    wm geometry $pal +580+30
    wm title $pal "Palette operations"
    set palette_mapped 1

    set fcolor [frame $pal.f0 -relief sunken -borderwidth 1]
    set cmap "gray"
    set color [frame $fcolor.f0]
    
    set color1 [frame $color.f1]    
    radiobutton $color1.gray -text gray -variable cmap -value gray -command colormap 
    radiobutton $color1.ct -text ct -variable cmap -value ct -command colormap 
    radiobutton $color1.hot -text hot -variable cmap -value hot -command colormap 
    radiobutton $color1.cold -text cold -variable cmap -value cold -command colormap 
    pack $color1.gray $color1.ct $color1.hot $color1.cold -side top  -anchor w -in $color1

    set color2 [frame $color.f2]
    radiobutton $color2.hls -text hls -variable cmap -value hls -command colormap 
    radiobutton $color2.spectrum -text spectrum -variable cmap -value spectrum -command colormap 
    radiobutton $color2.invert -text invert -variable cmap -value invert -command colormap 
    radiobutton $color2.rand -text random -variable cmap -value random -command colormap 
    pack $color2.hls $color2.spectrum $color2.invert $color2.rand  -side top  -anchor w -in $color2
    pack $color1 $color2 -side left -expand 1 -fill both -in $color
    
    set lut [frame $fcolor.f1]
    set lut1 [frame $lut.f1]
    set rread [button $lut1.read  -text "Read" -command "select_input_lut_file" ]
    set n_add [button $lut1.nadd -text "Add" -command add_color_node]
    set rset [button $lut1.rsetpal  -text "Reset" -command "reset_lut" ]
    pack $rread $n_add $rset -side top -expand 1 -fill x

    set lut2 [frame $lut.f2]
    set save [button $lut2.save -text "Save" -command "select_output_lut_file" ]
    set n_remove [button $lut2.remove -text "Remove" -command remove_color_node]
    set rapply [button $lut2.rapplypal  -text "Apply" -command "colormap" ]
    pack $save $n_remove $rapply -expand 1 -fill x 
    pack $lut1 $lut2 -side left -fill both -expand 1
    pack $color $lut -side top -fill both -expand 1 

    set level [frame $fcolor.f2]
    set cbar [button $level.colorbar -text Colorbar -command "colorbar" ]
    if {$palette_button==1} {
	set cmap_level [menubutton $level.cmap  -text "Colormap Level" -menu $level.cmap.menu -relief raised]
	pack $cmap_level -side top -fill both -expand 1 
	menu $level.cmap.menu -postcommand [list fillCmapMenu $level.cmap.menu "set_cmap_level" ]
	if {$has_plb_segment==1} {
	    set bclip [button $level.bclip -text "Clip data" -command "clip"]
	    set stretch [button $level.str -text "Stretch" -command "stretch_colormap"]
	    pack $bclip $stretch -side top -fill both -expand 1
	}
    }
    pack $cbar -side top -fill both -expand 1
    
    pack $level -side bottom -fill both -expand 1
    pack $fcolor -side right -anchor ne 
   
   
    set thresframe [frame $pal.f1]
    set loval $curr_min
    set lothres [frame $thresframe.f1]
    label $lothres.lothrestext -text "lo thresh"  
    entry $lothres.ent -width 7 -textvariable loval
    scale $lothres.lothres -from $curr_min -to $curr_max -orient horizontal \
        -variable loval -length 6c -resolution $_thres_res_ 
    pack $lothres.lothrestext $lothres.ent $lothres.lothres -side left -expand 1 -fill x -in $lothres
    pack $lothres -side top

    set hival $curr_max
    set hithres [frame $thresframe.f2]
    label $hithres.hithrestext -text "hi thresh" 
    entry $hithres.ent -width 7 -textvariable hival
    scale $hithres.hithres -from $curr_min -to $curr_max -orient horizontal \
        -variable hival -length 6c -resolution  $_thres_res_ 
    pack $hithres.hithrestext $hithres.ent $hithres.hithres -side left -expand 1 -fill x -in $hithres
    pack $hithres -side top
  
    set diffthresval [expr $curr_max-$curr_min]
    set midthresval [expr $curr_min+($diffthresval/2)]

    set midthres [frame $thresframe.f3]
    label $midthres.midthrestext -text "mean threshold  "  
    scale $midthres.midthres -from $curr_min -to $curr_max -orient horizontal \
        -variable midthresval -length 6c -resolution $_thres_res_ 
    pack $midthres.midthrestext $midthres.midthres -side left -expand 1 -fill x -in $midthres
    pack $midthres -side top

    set diffthres [frame $thresframe.f4]
    label $diffthres.diffthrestext -text "threshold range  " 
    scale $diffthres.diffthres -from 0.0 -to [expr $curr_max-$curr_min] -orient horizontal \
        -variable diffthresval -length 6c -resolution $_thres_res_ 
    pack $diffthres.diffthrestext $diffthres.diffthres -side left -expand 1 -fill x -in $diffthres
    pack $diffthres -side top
    
    set bframe [frame $pal.f2]
    set _cwid_ 100
    set _clen_ 100
    set intframe [frame $pal.in -width $_cwid_ -height $_clen_ -relief sunken -borderwidth 1]
    set title [frame $intframe.ti -width $_cwid_ -height 10  -relief raised -borderwidth 1]
    label $title.lab -text Intensity -fg black
    pack $title.lab
    set coledit(intensity) [canvas $intframe.c -width $_cwid_ -height $_clen_ -background gray]
    pack $title $coledit(intensity) -fill both -expand 1
    set redframe [frame $pal.red -width $_cwid_ -height $_clen_ -relief sunken -borderwidth 1]
    set title [frame $redframe.ti -width $_cwid_ -height 10  -relief raised -borderwidth 1]
    label $title.lab -text Red -fg red
    pack $title.lab
    set coledit(red) [canvas $redframe.c -width $_cwid_ -height $_clen_ -background white]
    pack $title $coledit(red) -fill both -expand 1
    set greenframe [frame $pal.green -width $_cwid_ -height $_clen_ -relief sunken -borderwidth 1] 
    set title [frame $greenframe.ti -width $_cwid_ -height 10  -relief raised -borderwidth 1]
    label $title.lab -text Green -fg green
    pack $title.lab
    set coledit(green) [canvas $greenframe.c -width $_cwid_ -height $_clen_ -background white]
    pack $title $coledit(green) -fill both -expand 1
    set blueframe [frame $pal.blue -width $_cwid_ -height $_clen_ -relief sunken -borderwidth 1]
    set title [frame $blueframe.ti -width $_cwid_ -height 10  -relief raised -borderwidth 1]
    label $title.lab -text Blue -fg blue
    pack $title.lab
    set coledit(blue) [canvas $blueframe.c -width $_cwid_ -height $_clen_ -background white]
    pack $title $coledit(blue) -fill both -expand 1
    pack $intframe $redframe $greenframe $blueframe  -side left -expand 1 -in $bframe
    pack $thresframe $bframe -side top -fill x -expand 1 -anchor nw

    set_color_binds 
   
    bind $diffthres.diffthres <B1-Motion> {
	if { ![info exists curr_img] } {
	    return
	}
	set loval [expr $midthresval-$diffthresval/2]
	set hival [expr $midthresval+$diffthresval/2] 
	$curr_img cmap_threshold $loval $hival
    }

    bind $midthres.midthres <B1-Motion> {
	if { ![info exists curr_img] } {
	    return
	}
	set loval [expr $midthresval-$diffthresval/2]
	set hival [expr $midthresval+$diffthresval/2]
	$curr_img cmap_threshold $loval $hival
    }

    bind $lothres.lothres <B1-Motion> {
	if { ![info exists curr_img] } {
	    return
	}
	if { $loval >= $hival } {
	    set loval [expr $hival -$_thres_incr_] 
	}
	set midthresval [expr ($loval+$hival)/2]
	set diffthresval [expr $hival-$loval]
	$curr_img cmap_threshold $loval $hival
    }
    bind $hithres.hithres <B1-Motion> {
	if { ![info exists curr_img] } {
	    return
	}
	if { $loval >= $hival } {
	    set hival [expr $loval+$_thres_incr_]
	}
	set midthresval [expr ($loval+$hival)/2]
	set diffthresval [expr $hival-$loval]
	$curr_img cmap_threshold $loval $hival
    }
	
    bind $lothres.lothres <ButtonRelease-1> {
	if { ![info exists curr_img] } {
	    return
	}
	if { $loval >= $hival } {
	    set loval [expr $hival -$_thres_incr_]   
	} 
	set midthresval [expr ($loval+$hival)/2]
	set diffthresval [expr $hival-$loval]
	$curr_img cmap_threshold $loval $hival
    }

    
    bind $hithres.hithres <ButtonRelease-1> {
	if { ![info exists curr_img] } {
	    return
	}
	if { $loval >= $hival } {
	    set hival [expr $loval+$_thres_incr_]
	}
	set midthresval [expr ($loval+$hival)/2]
	set diffthresval [expr $hival-$loval]
	$curr_img cmap_threshold $loval $hival
    }
    bind $midthres.midthres <ButtonRelease-1> {
	if { ![info exists curr_img] } {
	    return
	}
	if { $loval == $curr_min } {
	    set midthresval [expr $curr_min+$diffthresval/2]
	    set hival [expr $curr_min+$diffthresval]
	} elseif { $hival == $curr_max } {
	    set midthresval [expr $curr_max-$diffthresval/2]
	    set loval [expr $curr_max-$diffthresval]   
	}
	$curr_img cmap_threshold $loval $hival
    }
    bind $diffthres.diffthres <ButtonRelease-1> {
	if { ![info exists curr_img] } {
	    return
	}
	if { $loval == $curr_min } {
	    set diffthresval [expr ($midthresval-$curr_min)*2]
	    set hival [expr $curr_min+$diffthresval]
	} elseif { $hival == $curr_max } {
	    set diffthresval [expr ($curr_max-$midthresval)*2]
	    set loval [expr $curr_max-$diffthresval]
	}
	$curr_img cmap_threshold $loval $hival
    }

    bind $lothres.ent <Leave> {
	if { ![info exists curr_img] } {
	    return
	}
	%W configure -textvariable loval
	if {$keypress==1} {
	    set keypress 0
	} else {
	    return
	}
	if { [string compare $loval_ent ""] == 0 } {
	    set loval_ent [expr $curr_max-$diffthresval]
	}
	set loval $loval_ent
	
	if { $loval >= $hival } {
	    set loval [expr $hival -$_thres_incr_]
	}
	set midthresval [expr ($loval+$hival)/2]
	set diffthresval [expr $hival-$loval]
	$curr_img cmap_threshold $loval $hival
    }
    bind $hithres.ent <Leave> {
	if { ![info exists curr_img] } {
	    return
	}
	%W configure -textvariable hival
	if {$keypress==1} {
	    set keypress 0
	} else {
	    return
	}
	if { [string compare $hival_ent ""] == 0 } {
	    set hival_ent [expr $curr_min+$diffthresval]
	}
	set hival $hival_ent
	
	if { $loval >= $hival } {
	    set hival [expr $loval+$_thres_incr_]
	}
	set midthresval [expr ($loval+$hival)/2]
	set diffthresval [expr $hival-$loval]
	$curr_img cmap_threshold $loval $hival
    }
    bind $lothres.ent <Return> {
	if { ![info exists curr_img] } {
	    return
	}
	if {$keypress==1} {
	    set keypress 0
	}
	if { [string compare $loval_ent ""] == 0 } {
	    set loval_ent [expr $curr_max-$diffthresval]
	}
	set loval $loval_ent
	%W configure -textvariable loval
	if { $loval >= $hival } {
	    set loval [expr $hival -$_thres_incr_]
	}
	set midthresval [expr ($loval+$hival)/2]
	set diffthresval [expr $hival-$loval]
	$curr_img cmap_threshold $loval $hival
    }
    bind $hithres.ent <Return> {
	if { ![info exists curr_img] } {
	    return
	}
	if {$keypress==1} {
	    set keypress 0
	} 
	if { [string compare $hival_ent ""] == 0 } {
	    set hival_ent [expr $curr_min+$diffthresval]
	}
	set hival $hival_ent
	%W configure -textvariable hival
	if { $loval >= $hival } {
	    set hival [expr $loval+$_thres_incr_]
	}
	set midthresval [expr ($loval+$hival)/2]
	set diffthresval [expr $hival-$loval]
	$curr_img cmap_threshold $loval $hival
    }
    foreach evnt {<Enter>} {
	bind $lothres.ent $evnt {
	    if { ![info exists curr_img] } {
		return
	    }
	    set keypress 0
	    set loval_ent $loval
	    %W configure -textvariable loval_ent
	}
	bind $hithres.ent $evnt {
	    if { ![info exists curr_img] } {
		return
	    }
	    set keypress 0
	    set hival_ent $hival
	    %W configure -textvariable hival_ent
	}
    }
    foreach evnt {<KeyPress>} {
	bind $lothres.ent $evnt {
	    if { ![info exists curr_img] } {
		return
	    }
	    if {$keypress==0} {
		set keypress 1 
		set loval_ent $loval
		%W configure -textvariable loval_ent
	    }
	}
	bind $hithres.ent $evnt {
	    if { ![info exists curr_img] } {
		return
	    } 
	    if {$keypress==0} {
		set keypress 1 
		set hival_ent $hival
		%W configure -textvariable hival_ent
	    }
	}
    }

    bind $pal <Destroy> {
	set palette_mapped 0
	if { $palette_button == 1 } {
	    bind .pal <Destroy> {}
	}
    }
}

proc normalize {{a {0}} {b {0}}} {
    global curr_img _DispMax_ _DispMin_ _userDispMin_ _userDispMax_

    if { [info exists curr_img] } {
	set _userDispMin_ $a
	set _userDispMax_ $b
	$curr_img range $a $b
	set _DispMin_ [$curr_img getmin]
	set _DispMax_ [$curr_img getmax]
    }
}

proc stretch_colormap {} {
    global curr_img curr_color plist cx cy _cwid_ _clen_ cmap col_npoints

    if { ![info exists curr_img] } {
	return
    }
    $curr_img cmap_stretch
}

proc colormap {} {
    global curr_img curr_color plist cx cy _cwid_ _clen_ cmap col_npoints
    global currgn currimg

    if { ![info exists curr_img] } {
	return
    }
    set old_color $curr_color
    set clist {intensity red green blue}
    foreach k $clist {
	set curr_color $k
	set l {}
	for {set i 0} {$i<$col_npoints($curr_color)} {incr i} {
	    lappend l [expr int(floor($cx($plist($i,$curr_color),$curr_color)))]
	    lappend l [expr int(floor($cy($plist($i,$curr_color),$curr_color)))]
	}
#	eval {$curr_img cmap_stretch $curr_color $_cwid_ $_clen_} $l
    }
#    $curr_img colormap $cmap
    catch {set curr_color $old_color}
#    set powCurrentLut($curr_img) $l
    powCmapStretchIntensity $_cwid_ $_clen_ $l
    powReditherImages $currgn $currimg
}

proc reset_lut {} {
    global coledit curr_img curr_color plist pedge cx cy _cwid_ _clen_ col_npoints 

    set npts [expr $col_npoints($curr_color) -1]
    for {set i 0} {$i<$col_npoints($curr_color)} {incr i} {
	set _x_ [expr $_cwid_.0/$npts.0*$i]
	set _y_ [expr $_clen_*($npts-$i)/$npts.0]
	set p $plist($i,$curr_color)
	$coledit($curr_color) coords $p [expr $_x_-3] [expr $_y_-3] [expr $_x_+3] [expr $_y_+3]
	set cx($p,$curr_color) $_x_
	set cy($p,$curr_color) $_y_
    }
    for {set i 1} {$i<$col_npoints($curr_color)} {incr i} {
	set old_i [expr $i-1]
	$coledit($curr_color) coords $pedge($i,$curr_color) \
		$cx($plist($old_i,$curr_color),$curr_color) \
		$cy($plist($old_i,$curr_color),$curr_color) \
		$cx($plist($i,$curr_color),$curr_color)	\
		$cy($plist($i,$curr_color),$curr_color)	
	$coledit($curr_color) lower $pedge($i,$curr_color)
    }
    colormap 
}

proc image_open value {
    global img_nb curr_img color_bar_img dir file pattern showdot 
    
    if { $value == 1 } {
	incr img_nb
	set file [fileselect .fsel "Select A File" dir file pattern  showdot]
	if { $file != {} } {
	    rdfile $file img$img_nb
	    disp img$img_nb
	}
    } else {
	if { ![ info exists curr_img] } {
	    return
	}
	if { [info exists color_bar_img] && $curr_img==$color_bar_img} {
	    return
	}
	set file [fileselect .fsel "Select A File" dir file pattern showdot]
	if { $file != {} } {
	    rdfile $file curr_img
	    disp curr_img
	}
    }
}

proc copy { src dest args } {
    upvar $src src1
    upvar $dest dest1
    global scale_slice _wid_ _len_ win_array curr_img filename_array curr_filename nb_slices_array \
nb_slices curr_nb_slices curr_slice_array seq_array sbwidth

    if { [info exists src1] } {
	if { ![info exists dest1] } {
	    set dest1 [image create pict]
	}
	eval {$dest1 copy $src1} $args
	set slice_nb 0
	set scale_slice 
	set filename_array($dest1) "copy"
	set curr_filename "copy"
	set nb_slices_array($dest1) $nb_slices
	set seq_array($dest1) 0
	set curr_slice_array($dest1) 0
	set curr_nb_slices $nb_slices
	set curr_img $dest1
	if { [ info exists win_array($dest1)] } {
	    set _wid_ [image width $dest1]
	    set _len_ [image height $dest1]
	    $win_array($dest1).c configure -width $_wid_ -height $_len_ 
	    $win_array($dest1).c configure -scrollregion "0 0 $_wid_ $_len_" -confine 1
	    wm maxsize $win_array($dest1) $_wid_ $_len_
	    wm geometry $win_array($dest1) [format "%dx%d" $_wid_ $_len_]
	    set sbwidth [winfo reqwidth $win_array($dest1).yscroll]
	    topConfig $win_array($dest1) $_wid_ $_len_
	}
    } else {
	puts "source image $src1 does not exist"
    }
}

proc get_min_max {} {
    global curr_img curr_min curr_max _thres_res_ _thres_incr_
    
    if { [info exists curr_img] } {
	set max [$curr_img getmax]
	set min [$curr_img getmin]
	if {$max==$min} {
	    return
	}
	set t [format "%.1g" [expr ($max-$min)/255.0]] 
	set div [expr $max/$t]
	set h [expr ceil($div)]
	set curr_max [expr $t*$h]
	set div [expr $min/$t]
	set h [expr floor($div)]
	set curr_min [expr $t*$h]
	set _thres_res_ $t
	set _thres_incr_ $t
    }
}

proc update_scale {} {
    global curr_min curr_max _thres_res_ pal_window palette_button palette_mapped
  
    if { ($palette_button == 1) && ($palette_mapped == 1) } {
	set old_min $curr_min
	set old_max $curr_max
	
	set x [.pal.f1.f1.lothres get]
	set x1 [lindex [.pal.f1.f1.lothres coords $curr_min] 0]
	set x2 [lindex [.pal.f1.f1.lothres coords $curr_max] 0]
	
	set y [.pal.f1.f2.hithres get]
	set z [.pal.f1.f3.midthres get]
	set t [.pal.f1.f4.diffthres get]
	
	get_min_max
	
	set fac [expr ($curr_max-$curr_min)/($old_max-$old_min)]
	.pal.f1.f1.lothres config -from $curr_min -to $curr_max -resolution $_thres_res_
	.pal.f1.f1.lothres set [expr ($x-$old_min)*$fac+$curr_min]
	
	.pal.f1.f2.hithres config -from $curr_min -to $curr_max -resolution $_thres_res_
	.pal.f1.f2.hithres set [expr ($y-$old_min)*$fac+$curr_min]
	
	.pal.f1.f3.midthres config -from $curr_min -to $curr_max -resolution $_thres_res_
	.pal.f1.f3.midthres set [expr ($z-$old_min)*$fac+$curr_min]
	
	.pal.f1.f4.diffthres config -from 0.0 -to [expr $curr_max-$curr_min] -resolution $_thres_res_
	.pal.f1.f4.diffthres  set [expr $t*$fac]
    } else {
	get_min_max
    }
}


proc fillCmapMenu {topmenu cmd} {
    global Private_Colormap curr_img win_array color_bar_img

    if { [string compare "image_open 1" $cmd] != 0 } {
	if { [info exists curr_img] && [info exist win_array($curr_img)] } {
	    set Private_Colormap [$curr_img cmap_level]
	}
    }
    catch {$topmenu delete 0 last}
    $topmenu add radiobutton -label "Shared Colormap" -variable Private_Colormap -value 0   -command $cmd
    $topmenu add radiobutton -label "Default Colormap" -variable Private_Colormap -value 1  -command $cmd
    $topmenu add radiobutton -label "Shared Private Cmap" -variable Private_Colormap -value 2  -command $cmd
    $topmenu add radiobutton -label "Private Colormap" -variable Private_Colormap -value 3  -command  $cmd

}

proc clip {{lo {}} {hi {}} } {
    global curr_img color_bar_img loval hival

    if { [info exists curr_img] } {
	if { [info exists color_bar_img] } {
	    if {$color_bar_img != $curr_img } {
		if {$hi=={}} {
		    set hi $hival 
		}
		if {$lo=={}} {
		    set lo $loval
		}
		$curr_img clip $lo $hi
	    }
	} else {
	    if {$hi=={}} {
		set hi $hival 
	    }
	    if {$lo=={}} {
		set lo $loval
	    }
	    $curr_img clip $lo $hi
	}
	get_min_max
	update_scale
    }
}

proc quit {} { 
    exit 
}


proc control_panel {} {
    global  curr_img  scale_slice nb_slices_array curr_slice_array palette_button Private_Colormap visu_help_file has_plb_segment has_blt _DispMin_ _DispMax_ _userDispMin_ _userDispMax__x_ _y_ _pix_ _wid_ _len_
	  

    set pixtop .
    wm geometry . 530x140+10+30
    wm title $pixtop "[wm title $pixtop]: Control Panel"
    set f0 [frame $pixtop.f0]
  
    menubutton .files -text Files -menu .files.menu -relief raised 
    menu .files.menu
    .files.menu add cascade -label "New" -menu .files.menu.new  
    .files.menu add cascade -label "Open" -menu .files.menu.open
    .files.menu add command -label "Save" -command "savefile" 
    menu .files.menu.new -postcommand [list fillCmapMenu .files.menu.new {image_open 1}]
    menu .files.menu.open -postcommand [list fillCmapMenu .files.menu.open {image_open 0;set_cmap_level}]
    
    button .palette -text Palette -command "make_palette 1" 
    button .broi -text ROI -command "make_roi" 
    if { $has_blt == 1} {
	menubutton .prof -text " 1D " -menu .prof.m  -relief raised
	menu .prof.m -postcommand fillGraphMenu
    }
    if { $has_plb_segment == 1} {
	button .filter -text Filter -command "make_filter" 
    }
    button .overlay -text Overlays -command "make_overlay" 
    button .help -text Help -command "exec netscape $visu_help_file &"
    button .quit -text Quit -command exit 
    pack .files .palette .overlay .broi  -side left -fill x -expand 1 -in $f0
    if {$has_blt == 1} {
	 pack .prof  -side left -fill x -expand 1 -in $f0
    }
    if {$has_plb_segment == 1} {
	 pack .filter  -side left -fill x -expand 1 -in $f0
    }
    pack .help .quit -side left -fill x -expand 1 -in $f0
    pack $f0 -side top -fill both -expand 1 
    set palette_button 1
    set f1 [frame $pixtop.f1]

    set f2 [frame $pixtop.f2 -width 4c]
    entry $f2.x -width 4 -textvariable _x_
    label $f2.label1 -text "x" 
    entry $f2.y -width 4 -textvariable _y_
    label $f2.label2 -text " = " 
    label $f2.pix -textvariable _pix_ 
    pack $f2.x $f2.label1 $f2.y $f2.label2 $f2.pix -side left -in $f2
    pack $f2 -side left -fill y -expand 0 -in $f1
   
    set f3 [frame $pixtop.f3]
    label $f3.text -text "rows" 
    label $f3.value -textvariable _len_ 
    pack $f3.text $f3.value -side left -expand 0 -fill none -in $f3
   
    
    set f4 [frame $pixtop.f4]
    label $f4.text -text "cols" 
    label $f4.value -textvariable _wid_ 
    pack $f4.text $f4.value -side left -expand 0 -fill none -in $f4

    set f5 [frame $pixtop.f5]
    label $f5.text -text "nb_slices" 
    label $f5.value -textvariable curr_nb_slices 
    pack $f5.text $f5.value -side left -expand 0 -fill none -in $f5

    pack $f5 -side right -expand 0 -fill none -anchor e -in $f1
    pack $f4 -side right -expand 0 -fill none -anchor e -in $f1
    pack $f3 -side right -expand 0 -fill none -anchor e -in $f1

    pack $f1 -side top -fill both -expand 1
    
    set f6 [frame $pixtop.f6]
    label $f6.labelumin -text "user min " 
    entry $f6.umin -width 7 -textvariable _userDispMin_ 
    label $f6.labelumax -text " user max " 
    entry $f6.umax -width 7 -textvariable _userDispMax_ 
    label $f6.labelmin -text " min " 
    label $f6.min -textvariable _DispMin_ 
    label $f6.labelmax -text " max " 
    label $f6.amax -textvariable _DispMax_ 
    pack $f6.labelumin $f6.umin $f6.labelumax $f6.umax $f6.labelmin $f6.min $f6.labelmax $f6.amax -side left -expand 0 -fill x -in $f6
    pack $f6 -side top -fill both -expand 1

    set slice [frame $pixtop.f7]
    label .slicetext -text "slice number" 
    button .plus -text ">>" -command "upslice" 
    button .minus -text "<<" -command "downslice" 
    scale .slice -from 0 -to 9 -orient horizontal \
        -variable scale_slice -length 8c
    pack .slicetext .plus .minus .slice -side left -expand 1 -fill x -in $slice
    pack $slice -side top
    
    bind .slice <ButtonRelease> {
	if { ![info exists curr_img] } {
	    return
	}
	if {  $nb_slices_array($curr_img) == 1 } {
	    #just one image 
	    set scale_slice 0
	}
	if { $scale_slice > $nb_slices_array($curr_img) } {
	    set scale_slice $nb_slices_array($curr_img)
	} 
	set curr_slice_array($curr_img) $scale_slice
        rdslice curr_img $scale_slice
    }


    bind $f2.x <Leave> {
	if { ![info exists curr_img] } {
	    return
	}
	if { ($_x_>0) && ($_x_<$_wid_) && ($_y_>0) && ($_y_<$_len_) } {
	    set _pix_ [$curr_img get $_x_ $_y_]	
	}
    }	
    bind $f2.x <Return> {
	if { ![info exists curr_img] } {
	    return
	}
	if { ($_x_>0) && ($_x_<$_wid_) && ($_y_>0) && ($_y_<$_len_) } {
	    set _pix_ [$curr_img get $_x_ $_y_]	
	}
    }	 
    bind $f2.y <Leave> {
	if { ![info exists curr_img] } {
	    return
	}
	if { ($_x_>0) && ($_x_<$_wid_) && ($_y_>0) && ($_y_<$_len_) } {
	    set _pix_ [$curr_img get $_x_ $_y_] 
	}
    }	
    bind $f2.y <Return> {
	if { ![info exists curr_img] } {
	    return
	}
	if { ($_x_>0) && ($_x_<$_wid_) && ($_y_>0) && ($_y_<$_len_) } {
	    set _pix_ [$curr_img get $_x_ $_y_] 
	}
    }	
    
    bind $f6.umin <Return> {
	normalize $_userDispMin_ $_userDispMax_
    }
   
    bind $f6.umax <Return> {
	normalize $_userDispMin_ $_userDispMax_
    }

}



proc mkedges {} {
    global coledit cx cy plist pedge _cwid_ _clen_ curr_color col_npoints

    for {set i 1} {$i<$col_npoints($curr_color)} {incr i} {
	set old_i [expr $i-1]
	set pedge($i,$curr_color) [$coledit($curr_color) create line \
		$cx($plist($old_i,$curr_color),$curr_color) \
		$cy($plist($old_i,$curr_color),$curr_color) \
		$cx($plist($i,$curr_color),$curr_color) \
		$cy($plist($i,$curr_color),$curr_color) -tag edge]
	$coledit($curr_color) lower $pedge($i,$curr_color)
    }
}


proc mknodes {} {
    global coledit cx cy plist _cwid_ _clen_ curr_color col_npoints

    set npts [expr $col_npoints($curr_color)-1]
    for {set i 0} {$i<$col_npoints($curr_color)} {incr i} {
	set x [expr $_cwid_.0/$npts.0*$i]
	set y [expr $_clen_*($npts-$i)/$npts.0]
	set plist($i,$curr_color) [$coledit($curr_color) create oval [expr $x-3] [expr $y-3] [expr $x+3] [expr $y+3] -outline black -fill white -tags node]
	set cx($plist($i,$curr_color),$curr_color) $x
	set cy($plist($i,$curr_color),$curr_color) $y
    }
}

proc mvnode {node x y} {
    global coledit curr_img curr_color plist pedge cx cy _cwid_ _clen_ col_npoints

    if { $x < 0 } {
	set x 0
    } elseif { $x > $_cwid_ } {
	set x $_cwid_ 
    }
    if {$y < 0} {
	set y 0
    } elseif {$y > $_clen_} {
	set y $_clen_
    }
	
    if { $node == $plist(0,$curr_color) } {	
	set p $plist(0,$curr_color)
	set p1 $plist(1,$curr_color)
	if { $x>$cx($p1,$curr_color) } {
	    set x $cx($p1,$curr_color)
	}
	if { $x<($_clen_-$y)}  {
	    set x 0
	} else {
	    set y $_clen_ 
	}   
	set cx($p,$curr_color) $x
	set cy($p,$curr_color) $y
	$coledit($curr_color) coords $p [expr $x-3] [expr $y-3] [expr $x+3] [expr $y+3]
	$coledit($curr_color) coords $pedge(1,$curr_color) $cx($p,$curr_color) $cy($p,$curr_color) $cx($p1,$curr_color) $cy($p1,$curr_color)

    } elseif { $node == $plist([expr $col_npoints($curr_color)-1],$curr_color) } {
	set p  $plist([expr $col_npoints($curr_color)-2],$curr_color)
	set p1 $plist([expr $col_npoints($curr_color)-1],$curr_color)
	
	if { $x<$cx($p,$curr_color) } {
	   set x $cx($p,$curr_color)
	}
	if { $x<($_clen_-$y)}  {
	    set y 0
	    
	} else {
	    set x $_cwid_
	}
	set cx($p1,$curr_color) $x 
	set cy($p1,$curr_color) $y
	 
	$coledit($curr_color) coords $p1 [expr $x-3] [expr $y-3] [expr $x+3] [expr $y+3]
	$coledit($curr_color) coords $pedge([expr $col_npoints($curr_color)-1],$curr_color) $cx($p,$curr_color) $cy($p,$curr_color) $cx($p1,$curr_color) $cy($p1,$curr_color)
    } else {
	set i 1
	while {$node != $plist($i,$curr_color) } {
	    incr i
	}
	set p $plist($i,$curr_color)
	set pm $plist([expr $i-1],$curr_color)
	set pp $plist([expr $i+1],$curr_color)

	if { $x<$cx($pm,$curr_color) } {
	    set x $cx($pm,$curr_color)
	} elseif {$x>$cx($pp,$curr_color) } {
	    set x $cx($pp,$curr_color)
	}
	$coledit($curr_color) move $node [expr $x-$cx($p,$curr_color)] [expr $y-$cy($p,$curr_color)]
	set cx($p,$curr_color) $x
	set cy($p,$curr_color) $y
	$coledit($curr_color) coords $pedge($i,$curr_color) $cx($pm,$curr_color) $cy($pm,$curr_color) $cx($p,$curr_color) $cy($p,$curr_color)
	$coledit($curr_color) coords $pedge([expr $i+1],$curr_color) $cx($p,$curr_color) $cy($p,$curr_color) $cx($pp,$curr_color) $cy($pp,$curr_color)
    
    }
  
    if { [info exists curr_img] } {
	set l {}
	for {set i 0} {$i<$col_npoints($curr_color)} {incr i} {
	    lappend l [expr int(floor($cx($plist($i,$curr_color),$curr_color)))]
	    lappend l [expr int(floor($cy($plist($i,$curr_color),$curr_color)))]
	}
	eval {$curr_img cmap_stretch $curr_color $_cwid_ $_clen_} $l
    }
}

proc set_curr_color col {
    global coledit curr_color

    set bgcol [$coledit($col) cget -bg]
    $coledit($curr_color) configure -bg $bgcol
    set curr_color $col
    $coledit($curr_color) configure -bg gray
}

proc set_node_number {numnodes} {
    global coledit curr_img curr_color plist pedge cx cy _cwid_ _clen_ col_npoints

    #delete existing nodes
    for {set i 0} {$i<$col_npoints($curr_color)} {incr i} {
	$coledit($curr_color) delete $plist($i,$curr_color)
    }
    for {set i 1} {$i<$col_npoints($curr_color)} {incr i} {
	$coledit($curr_color) delete $pedge($i,$curr_color)
    }

    set col_npoints($curr_color) $numnodes

    mknodes
    mkedges
}

proc add_color_node {} {
    global col_npoints curr_color

    set num $col_npoints($curr_color)
    set_node_number [incr num]
    colormap
}

proc remove_color_node {} {
    global col_npoints curr_color
    
    set num $col_npoints($curr_color)
    if {$num==2} {
	return
    } else {
	set num [expr $num-1]
	set_node_number $num
    } 
    colormap
}

proc save_color_nodes {filename} {
    global curr_color col_npoints cx cy plist _cwid_ _clen_

    set f [open $filename w]
    set l {red green blue intensity}
    foreach k  $l {
	puts $f "$col_npoints($k) \#$k"
	for {set i 0} {$i<$col_npoints($k)} {incr i} {
	    set x [expr $cx($plist($i,$k),$k)/double($_cwid_)]
	    set y [expr (double($_clen_)-$cy($plist($i,$k),$k))/double($_clen_)]
	    puts $f "$x $y"
	}
	puts $f ""
    }
    close $f
}
	

proc read_color_nodes {filename} {
    global coledit curr_color col_npoints cx cy plist pedge _cwid_ _clen_

    set f [open $filename r]
    set l {red green blue intensity}
    foreach k  $l {
	set_curr_color $k
	for {set i 0} {$i<$col_npoints($k)} {incr i} {
	    $coledit($k) delete $plist($i,$k)
	} 
	for {set i 1} {$i<$col_npoints($k)} {incr i} { 
	    $coledit($k) delete $pedge($i,$k)
	}
	gets $f line 
	if { [eof $f] } {
	    close $f
	    return
	}
	scan $line "%d" col_npoints($k) 
	mknodes
	mkedges 
	for {set i 0} {$i<$col_npoints($k)} {incr i} {
	    gets $f line 
	    if { [eof $f] } {
		close $f
		return
	    }
	    scan $line "%g %g" x y 
	    mvnode $plist($i,$k) [expr $x*$_cwid_] [expr $_clen_*(1-$y)]
	} 
	gets $f line 
	if { [eof $f] } {
	    close $f
	}
    }
    colormap
}
	    

proc select_input_lut_file {} {
    global dir file pattern showdot 

    set oldpattern $pattern
    set pattern "*.tr.dat"
    set name [fileselect .fsel "Select A File" dir file pattern showdot]
    if { $name == {} } {
	set pattern $oldpattern
	return
    }
    read_color_nodes $name
    set pattern $oldpattern
}

proc select_output_lut_file {} {
    global dir file pattern showdot 

    set oldpattern $pattern
    set pattern "*.tr.dat"
    set name [fileselect .fsel "Select A File" dir file pattern showdot]
    if { $name == {} } {
	set pattern $oldpattern
	return
    }
    if { [file exists $name] } {
	set text [format "Overwrite file \n %s ?" $name]
	set sure [tk_dialog .d {Save to disk} $text warning 0 {cancel} {overwrite} ]
	if { $sure==0 } {
	    set pattern $oldpattern
	    return
	}
    }
    save_color_nodes $name
    set pattern $oldpattern
}

proc set_color_binds {} {
    global coledit curr_color plist pedge col_npoints oldx oldy _slope_
 
    set old_color $curr_color
    set l {intensity red green blue}
    foreach k  $l {
	set_curr_color $k
	mknodes
	mkedges 
	
	bind $coledit($curr_color) <Button-1> {
	    set can %W
	    set l {intensity red green blue}
	    foreach k  $l {
		if {$coledit($k) == $can } {
		    set_curr_color $k
		}
	    }
	}

	$coledit($curr_color) bind node <B1-Motion> {
	    set p [$coledit($curr_color) find withtag current]
	    mvnode $p %x %y 
	}
   
	$coledit($curr_color) bind edge <Button-2> { 
	    set can %W
	    set l {intensity red green blue}
	    foreach k  $l {
		if {$coledit($k) == $can } {
		    set_curr_color $k
		}
	    }
	    set e  [$coledit($curr_color) find withtag current]
	    for {set i 1} {$i<$col_npoints($curr_color)} {incr i} {
		if {$e==$pedge($i,$curr_color)} {
		    set oldx %x
		    set oldy %y
		    set p $plist([expr $i-1],$curr_color)
		    set p1 $plist($i,$curr_color)
		    set x1 $cx($p,$curr_color)
		    set x2 $cx($p1,$curr_color)
		    set y1 $cy($p,$curr_color)
		    set y2 $cy($p1,$curr_color) 
		    if {abs(double($x1-$x2))>0.01} {
			set _slope_ [expr double($y2-$y1)/double($x2-$x1)]
		    } else {
			set _slope_ "NaN"
		    }
		}
	    }
	}

	$coledit($curr_color) bind edge <B2-Motion> {
	    set e  [$coledit($curr_color) find withtag current]
	    if {$col_npoints($curr_color)==2} {
		mvramp [expr $oldx-%x] [expr $oldy-%y]
		set oldx %x
		set oldy %y
	    } else {
		
		for {set i 1} {$i<$col_npoints($curr_color)} {incr i} {
		    if {$e==$pedge($i,$curr_color)} {
			set p $plist([expr $i-1],$curr_color)
			set x [expr $cx($p,$curr_color)-$oldx+%x]
			set y [expr $cy($p,$curr_color)-$oldy+%y]
			mvnode $p $x $y
			set p $plist($i,$curr_color)
			set x [expr $cx($p,$curr_color)-$oldx+%x]
			set y [expr $cy($p,$curr_color)-$oldy+%y]
			mvnode $p $x $y
			set oldx %x
			set oldy %y
		    }
		}
	    }
	}
    }
    set_curr_color $old_color
}

proc mvramp {dx dy} {
    global coledit curr_img curr_color plist pedge cx cy _cwid_ _clen_ col_npoints _slope_ 

    set p $plist(0,$curr_color)
    set p1 $plist(1,$curr_color)
    
    set x1 $cx($p,$curr_color)
    set x2 $cx($p1,$curr_color) 
    set y1 $cy($p,$curr_color)
    set y2 $cy($p1,$curr_color)
   
    set x [expr $x1-$dx]
    set y [expr $y1-$dy]

    if { $x<($_clen_-$y)}  {
	set x 0
    } else {
	set y $_clen_ 
    }   
     
    if { [string compare $_slope_ "NaN"] != 0} {
	set a $_slope_
	set b [expr ($y-$a*$x)]
	
	set newy  [expr $a*$_cwid_+$b]
	if { $newy < 0 } {
	    if {$a!=0} {
		set xp [expr -$b/$a]
	    } else {
		set xp $x
	    }
	    set yp 0
	} else {
	    set xp $_cwid_
	    set yp $newy
	}
	mvnode $p $x $y
	mvnode $p1 $xp $yp
    } else {
	set x2 $x
	set y2 [expr $y2-$dy]
	#update point in most logical order
	if {$dx<0} {
	    mvnode $p1 $x2 $y2
	    mvnode $p $x $y
	} else {
	    mvnode $p $x $y
	    mvnode $p1 $x2 $y2
	}
    }
    update idletasks
}


























# set global variables 
# --------------------
if { ![info exists dir] }  {set dir "."}
if { ![info exists file] } {set file ""}
if { ![info exists pattern] } {set pattern "*.sdt"}
if { ![info exists showdot] } {set showdot 0}


# file selector box
# -----------------

proc fileselect {w title dir1 file1 pattern1 showdot1} {
    upvar $dir1 dir
    upvar $file1 file
    upvar $pattern1 pattern
    upvar $showdot1 showdot
    global fileselect


    # local functions
    # ---------------

    proc settabstops {toplevel list} {
	for {set i 0} {$i < [llength $list]} {incr i} {
	    set tab1 [lindex $list $i]
	    set tab2 [lindex $list [expr ( $i + 1 ) % [llength $list]]]
	    bindtags $tab1 [list $toplevel [winfo class $tab1] $tab1]
	    bind $tab1 <Tab> "focus $tab2"
	    bind $tab2 <Shift-Tab> "focus $tab1"
	}
    }

    proc dirnormalize {dir} {
	if {[string compare $dir ""] == 0} {set dir "."}
	set list [split $dir "/"]
	switch [lindex $list 0] {
	    ""      { set d [list ""] }
	    default { set d [split [pwd] "/"] }
	}
	foreach i $list {
	    switch $i {
		""      -
		"."     { }
		".."	{ if {[string compare [lindex $d end] ""] != 0} {
		             set d [lrange $d 0 [expr [llength $d] - 2]]
		          }
		        }
		default { lappend d $i }
	    }
	}
	set s ""
	foreach i $d { append s $i/ }
	return $s
    }

    proc readdir {w dir file pattern} {
	global fileselect

	set dir [dirnormalize $dir]
	if {![file readable $dir]} {
	    bell
	    return
	}
	set file [file tail $file]

	if {[string compare $pattern ""] == 0} { set pattern "*" }

	set fileselect(path) $dir
	set fileselect(file) $file
	set fileselect(pattern) $pattern
	set fileselect(selection) $dir$file
	$w.filter.entry delete 0 end
	$w.filter.entry insert 0 "$dir$pattern"

	
	set file_list [lsort [glob -nocomplain "$dir{.,}*"]]
	$w.lists.frame1.list delete 0 end
	if {$fileselect(showdot)} {
	    foreach i $file_list {
		if {[file isdirectory $i]} { $w.lists.frame1.list insert end $i }
	    }
	} else {
	    $w.lists.frame1.list insert end "$dir."
	    $w.lists.frame1.list insert end "$dir.."  
	    foreach i $file_list {
		if {[file isdirectory $i]} {
		    if { ![string match "$dir\.*" $i] } {
			$w.lists.frame1.list insert end $i
		    }
		}
	    }
	}
	
	if {[$w.lists.frame1.list size] > 0} {
	    $w.lists.frame1.list xview \
		    [string last "/" [$w.lists.frame1.list get 0]]
	}

	if {$fileselect(showdot)} {
	    set file_list [lsort [glob -nocomplain "$dir{.,}$pattern"]]
	} else {
	    set file_list [lsort [glob -nocomplain "$dir$pattern"]]
	}
	$w.lists.frame2.list delete 0 end
	foreach i $file_list {
	    if {![file isdirectory $i]} {
		$w.lists.frame2.list insert end [file tail $i]
	    }
	}
	if {[$w.lists.frame2.list size] > 0} {
	    $w.lists.frame2.list xview \
		    [string last "/" [$w.lists.frame2.list get 0]]
	}
    }


    # event actions
    # -------------

    proc cmd_showdot {w} {
	global fileselect

	readdir $w $fileselect(path) $fileselect(file) $fileselect(pattern)
    }

    proc cmd_filesel_filter {w} {
	set dir [file dirname [$w.filter.entry get]]
	set pattern [file tail [$w.filter.entry get]]
	readdir $w $dir "" $pattern
    }

    proc cmd_filesel_left_dblb1 {w list} {
	global fileselect

	if {[$list size] > 0 } {
	    readdir $w "[$list get [$list curselection]]/" "" \
		    $fileselect(pattern)
	}
    }

    proc cmd_filesel_right_b1 {w list} {
	global fileselect

	if {[$list size] > 0} {
	    set fileselect(file) [$list get [$list curselection]]
	    set fileselect(selection) "$fileselect(path)$fileselect(file)"
	}
    }

    proc cmd_filesel_right_dblb1 {w list} {
	if {[$list size] > 0} {
	    $w.buttons.ok flash
	    $w.buttons.ok invoke
	}
    }


    # GUI
    # ---

    catch {destroy $w}
    toplevel $w
    wm protocol $w WM_DELETE_WINDOW { }
    wm transient $w [winfo toplevel [winfo parent $w]]
    wm title $w $title

    frame $w.title -relief raised -borderwidth 1
    frame $w.top -relief raised -borderwidth 1
    frame $w.bottom -relief raised -borderwidth 1
    pack $w.title $w.top $w.bottom -fill both -expand 1

    label $w.title.label -text $title
    pack $w.title.label -in $w.title -expand 1 -pady 5

    frame $w.filter
    label $w.filter.label -text "Filter" -anchor w
    checkbutton $w.filter.showdot -text "Show Dot Files" \
	    -command "cmd_showdot $w" -variable fileselect(showdot)
    entry $w.filter.entry
    pack $w.filter.entry -side bottom -fill x
    pack $w.filter.label -side left
    pack $w.filter.showdot -side right

    frame $w.selection
    label $w.selection.label -text "Selection" -anchor w
    entry $w.selection.entry -textvariable fileselect(selection)
    pack $w.selection.label $w.selection.entry -fill x

    frame $w.lists

    frame $w.lists.frame1
    label $w.lists.frame1.label -text "directories" -anchor w
    scrollbar $w.lists.frame1.yscroll -relief sunken \
	    -command "$w.lists.frame1.list yview"
    scrollbar $w.lists.frame1.xscroll -relief sunken -orient horizontal \
	    -command "$w.lists.frame1.list xview"
    listbox $w.lists.frame1.list -width 30 -height 10 \
	    -yscroll "$w.lists.frame1.yscroll set" \
	    -xscroll "$w.lists.frame1.xscroll set" \
	    -relief sunken -setgrid 1 -selectmode single
    pack $w.lists.frame1.label -side top -fill x
    pack $w.lists.frame1.yscroll -side right -fill y
    pack $w.lists.frame1.xscroll -side bottom -fill x
    pack $w.lists.frame1.list -expand yes -fill y

    frame $w.lists.frame2
    label $w.lists.frame2.label -text "Files" -anchor w
    scrollbar $w.lists.frame2.yscroll -relief sunken \
	    -command "$w.lists.frame2.list yview"
    scrollbar $w.lists.frame2.xscroll -relief sunken -orient horizontal \
	    -command "$w.lists.frame2.list xview"
    listbox $w.lists.frame2.list -width 30 -height 10 \
	    -yscroll "$w.lists.frame2.yscroll set" \
	    -xscroll "$w.lists.frame2.xscroll set" \
	    -relief sunken -setgrid 1 -selectmode single
    pack $w.lists.frame2.label -side top -fill x
    pack $w.lists.frame2.yscroll -side right -fill y
    pack $w.lists.frame2.xscroll -side bottom -fill x
    pack $w.lists.frame2.list -expand yes -fill y

    frame $w.lists.fill
    pack $w.lists.frame1 -side left
    pack $w.lists.frame2 -side right
    pack $w.lists.fill -padx 10

    frame $w.buttons
    button $w.buttons.ok -text "Ok" -width 10
    button $w.buttons.filter -text "Filter" -width 10
    button $w.buttons.cancel -text "Cancel" -width 10
    pack $w.buttons.ok $w.buttons.filter $w.buttons.cancel -side left -expand 1

    pack $w.filter $w.lists $w.selection -side top -padx 20 -fill x -pady 5 \
	    -in $w.top
    pack $w.buttons -expand 1 -fill both -pady 10 -in $w.bottom


    # event bindings
    # --------------

    $w.buttons.ok config -command "set fileselect(button) 0"
    $w.buttons.cancel config -command "set fileselect(button) 2"
    $w.buttons.filter config -command "cmd_filesel_filter $w"

    bind $w.lists.frame1.list <Double-Button-1> "cmd_filesel_left_dblb1 $w %W"
    bind $w.lists.frame1.list <Double-space> "cmd_filesel_left_dblb1 $w %W"
    bind $w.lists.frame2.list <Button-1> "cmd_filesel_right_b1 $w %W"
    bind $w.lists.frame2.list <space> "cmd_filesel_right_b1 $w %W"
    bind $w.lists.frame2.list <Double-Button-1> "cmd_filesel_right_dblb1 $w %W"
    bind $w.lists.frame2.list <Double-space> "cmd_filesel_right_dblb1 $w %W"

    bind $w.filter.entry <Return> \
	    "$w.buttons.filter flash; $w.buttons.filter invoke"
    bind $w.selection.entry <Return> \
	    "$w.buttons.ok flash; $w.buttons.ok invoke"
    bind $w <Escape> "$w.buttons.cancel flash; $w.buttons.cancel invoke"

    settabstops $w [list $w.filter.showdot $w.filter.entry \
	    $w.lists.frame1.list $w.lists.frame2.list $w.selection.entry \
	    $w.buttons.ok $w.buttons.filter $w.buttons.cancel]


    # initialization
    # --------------

    set fileselect(path) $dir
    set fileselect(file) ""
    set fileselect(pattern) $pattern
    set fileselect(showdot) $showdot
    set fileselect(button) 0
    set fileselect(selection) ""

    readdir $w $dir $file $pattern
    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
            - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
            - [winfo vrooty [winfo parent $w]]]
    wm geom $w +$x+$y
    wm deiconify $w
    set old_focus [focus]
    grab $w
    focus $w.selection.entry
    tkwait variable fileselect(button)
    grab release $w
    catch {focus $old_focus}
    destroy $w
    if {$fileselect(button) == 0} {
	set dir $fileselect(path)
	set file $fileselect(file)
	set pattern $fileselect(pattern)
	set showdot $fileselect(showdot)
	return $fileselect(selection)
    } else {
	return {}
    }
}



