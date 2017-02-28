itcl::class NewFITS {
    inherit  itk::Toplevel
    private common fileName ""

    private method _close {} {
	itcl::delete object $this
	
    }
    private method _go {} {
	if { [file exist $fileName ] == 1 } {
	    promptMsg "File $fileName exists\n Do you want to overwrite?" \
		[itcl::code $this _save] Yes No
	} else {
	    _save
	}
    }
    
    private method _save {} {
	$itk_interior.image setFileName $fileName
	$itk_interior.image go
	if { [file exist $fileName ] } {
	    openFitsFileWM $fileName 0
	}
	_close
    }
    
    constructor {args} {
        global g_titleFont
	component hull configure -borderwidth 0

       if { [llength $args]>0 } {
	  set fileName [lindex $args 0]
          set args [lrange $args 1 end]
       }

       iwidgets::entryfield $itk_interior.fname -labeltext "File name:" \
                -labelfont g_titleFont \
                -textfont g_titleFont \
		-labelpos w -textvariable [itcl::scope fileName]
	pack $itk_interior.fname -fill x -expand 1

	NewImage $itk_interior.image $fileName
	pack $itk_interior.image -fill x -expand 1

	itk_component add bbox {
	    iwidgets::buttonbox $itk_interior.bbox 
	}  
	$itk_component(bbox) add OK -font g_titleFont -text Create -command [itcl::code $this _go]
	$itk_component(bbox) add Cancel  -font g_titleFont -text Cancel -command \
	    [itcl::code $this _close]
	$itk_component(bbox) add Help  -font g_titleFont -text Help -command \
	    {hhelp createNewFITS}
	pack $itk_component(bbox) -fill x -expand 1

        eval itk_initialize $args
    }

    destructor {}

}

















