itcl::class NewExtension {
    inherit  itk::Toplevel

    private variable fileName ""
    private variable _ext_type IMAGE
    private variable _selected 0

    private method _select_type {} 
    private method _go {} 
    private method _close {} {
	itcl::delete object $this
    }
    
    constructor {args} {
	global checkBBgColor
	global g_titleFont

        option add *NewExtension.font g_titleFont

# to fix a bug in iwidgets. The radiobox does not get cleaned when being 
# created. if an earlier radiobox exist, then the selection is still
# valid even the radio box got distroyed
	catch {unset ::iwidgets::Radiobox::_modes}

#
	if { [llength $args] == 0 } {
	    itcl::delete object $this
	    error "Empty argument list"
	}

# to fix a bug in iwidgets. The radiobox does not get cleaned when being 
# created. if an earlier radiobox exist, then the selection is still
# valid even the radio box got distroyed
	catch {unset ::iwidgets::Radiobox::_modes}

#
	if { [llength $args] == 0 } {
	    itcl::delete object $this
	    error "Empty argument list"
	}
	set fileName [lindex $args 0]
        set args [lrange $args 1 end]

	component hull configure -borderwidth 0
	
	iwidgets::radiobox $itk_interior.exttype \
		-labeltext "Extension Type" \
		-selectcolor $fvPref::checkBBgColor  
	# pack $itk_component(exttype) -fill x -expand 1 -padx 2
	# $itk_component(exttype) configure -labelfont g_titleFont
     
	# $itk_component(exttype) add IMAGE -text "Image"
	# $itk_component(exttype) add TABLE -text "Table"
	# $itk_component(exttype) configure -command [itcl::code $this _select_type] 

	pack $itk_interior.exttype -fill x -expand 1 -padx 2
	$itk_interior.exttype configure -labelfont g_titleFont
     
	$itk_interior.exttype add IMAGE -text "Image"
	$itk_interior.exttype add TABLE -text "Table"
	$itk_interior.exttype configure -command [itcl::code $this _select_type] 

	itk_component add workspace {
	    frame $itk_interior.workspace -height 50
	} 
	pack $itk_component(workspace) -fill x -expand 1

	itk_component add sep {
	    frame $itk_interior.sep -height 3 -relief raised -borderwidth 1
	} 
	pack $itk_component(sep) -fill x -expand 1

	itk_component add bbox {
	    iwidgets::buttonbox $itk_interior.bbox 
	} 
	$itk_component(bbox) add OK -text Create -command [itcl::code $this _go] -font g_titleFont
	$itk_component(bbox) add Cancel -text Cancel -command \
	    [itcl::code $this _close] -font g_titleFont
	$itk_component(bbox) add Help -text Help -command \
	    {hhelp createNewFITS} -font g_titleFont
	pack $itk_component(bbox) -fill x -expand 1

        eval itk_initialize $args
    }

    destructor {}

}

itcl::body NewExtension::_go {} {
    $itk_interior.workspace.child go
    _close
}

itcl::body NewExtension::_select_type {} {
    set _ext_type [$itk_interior.exttype get]

    if { $_selected == 1} {
	catch {itcl::delete object $itk_interior.workspace.child}
    } 

    if { $_ext_type == "IMAGE" } {
	NewImage $itk_interior.workspace.child $fileName
    } else {
	NewTable $itk_interior.workspace.child $fileName
    }
    pack $itk_interior.workspace.child -fill x -expand 1
    set _selected 1
}

