namespace eval pgtk {
#
#  Copy of tk_dialog with command conflicts replaced 
#
proc dialog {w title text bitmap default args} {

    global tkPriv tcl_platform

    # Check that $default was properly given
    if {[string is int $default]} {
	if {$default >= [llength $args]} {
	    return -code error "default button index greater than number of buttons specified for tk_dialog"
	}
    } elseif {[string equal {} $default]} {
	set default -1
    } else {
	set default [lsearch -exact $args $default]
    }

    # 1. Create the top-level window and divide it into top
    # and bottom parts.

    catch {tk::destroy $w}
    tk::toplevel $w -class Dialog
    tk::wm title $w $title
    tk::wm iconname $w Dialog
    tk::wm protocol $w WM_DELETE_WINDOW { }

    # Dialog boxes should be transient with respect to their parent,
    # so that they will always stay on top of their parent window.  However,
    # some window managers will create the window as withdrawn if the parent
    # window is withdrawn or iconified.  Combined with the grab we put on the
    # window, this can hang the entire application.  Therefore we only make
    # the dialog transient if the parent is viewable.
    #
    if { [winfo viewable [winfo toplevel [winfo parent $w]]] } {
	tk::wm transient $w [winfo toplevel [winfo parent $w]]
    }    

    if {[string equal $tcl_platform(platform) "macintosh"]} {
	unsupported1 style $w dBoxProc
    }

    tk::frame $w.bot
    tk::frame $w.top
    if {[string equal $tcl_platform(platform) "unix"]} {
	$w.bot configure -relief raised -bd 1
	$w.top configure -relief raised -bd 1
    }
    tk::pack $w.bot -side bottom -fill both
    tk::pack $w.top -side top -fill both -expand 1

    # 2. Fill the top part with bitmap and message (use the option
    # database for -wraplength and -font so that they can be
    # overridden by the caller).

    tk::option add *Dialog.msg.wrapLength 3i widgetDefault
    if {[string equal $tcl_platform(platform) "macintosh"]} {
	tk::option add *Dialog.msg.font system widgetDefault
    } else {
	tk::option add *Dialog.msg.font {Times 12} widgetDefault
    }

    tk::label $w.msg -justify left -text $text
    tk::pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 3m -pady 3m
    if {[string compare $bitmap ""]} {
	if {[string equal $tcl_platform(platform) "macintosh"] &&  [string equal $bitmap "error"]} {
	    set bitmap "stop"
	}
	tk::label $w.bitmap -bitmap $bitmap
	tk::pack $w.bitmap -in $w.top -side left -padx 3m -pady 3m
    }

    # 3. Create a row of buttons at the bottom of the dialog.

    set i 0
    foreach but $args {
	tk::button $w.button$i -text $but -command [list set tkPriv(button) $i]
	if {$i == $default} {
	    $w.button$i configure -default active
	} else {
	    $w.button$i configure -default normal
	}
	tk::grid $w.button$i -in $w.bot -column $i -row 0 -sticky ew -padx 10
	tk::grid columnconfigure $w.bot $i
	# We boost the size of some Mac buttons for l&f
	if {[string equal $tcl_platform(platform) "macintosh"]} {
	    set tmp [string tolower $but]
	    if {[string equal $tmp "ok"] || [string equal $tmp "cancel"]} {
		tk::grid columnconfigure $w.bot $i -minsize [expr {59 + 20}]
	    }
	}
	incr i
    }

    # 4. Create a binding for <Return> on the dialog if there is a
    # default button.

    if {$default >= 0} {
	bind $w <Return> "
	[list $w.button$default] configure -state active -relief sunken
	update idletasks
	after 100
	set tkPriv(button) $default
	"
    }

    # 5. Create a <Destroy> binding for the window that sets the
    # button variable to -1;  this is needed in case something happens
    # that destroys the window, such as its parent window being destroyed.

    bind $w <Destroy> {set tkPriv(button) -1}

    # 6. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    tk::wm withdraw $w
    update idletasks
    set x [expr {[winfo screenwidth $w]/2 - [winfo reqwidth $w]/2  - [winfo vrootx [winfo parent $w]]}]
    set y [expr {[winfo screenheight $w]/2 - [winfo reqheight $w]/2  - [winfo vrooty [winfo parent $w]]}]
    tk::wm geom $w +$x+$y
    tk::wm deiconify $w

    # 7. Set a grab and claim the focus too.

    set oldFocus [focus]
    set oldGrab [grab current $w]
    if {[string compare $oldGrab ""]} {
	set grabStatus [grab status $oldGrab]
    }
    grab $w
    if {$default >= 0} {
	focus $w.button$default
    } else {
	focus $w
    }

    # 8. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    tk::tkwait variable tkPriv(button)
    catch {focus $oldFocus}
    catch {
	# It's possible that the window has already been destroyed,
	# hence this "catch".  Delete the Destroy handler so that
	# tkPriv(button) doesn't get reset by it.

	bind $w <Destroy> {}
	tk::destroy $w
    }
    if {[string compare $oldGrab ""]} {
      if {[string compare $grabStatus "global"]} {
	    grab $oldGrab
      } else {
          grab -global $oldGrab
	}
    }
    return $tkPriv(button)

}

}  ;# End of namespace pgtk
