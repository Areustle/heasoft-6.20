itcl::class NewImage {
    inherit  itk::Widget

    private variable fileName
    private variable g_isNewFile 1
    private variable _naxes     0
    private variable _axis_list {}

    private method create_new_fits_image {}
    private method _close {}

    public method go {} 
    public method setFileName {fname}

    constructor {args} {
        global g_titleFont
	
	if { [llength $args] !=0 } {
	    set fileName $args
	}

	if { $fileName != "" && [file exist $fileName] } {
	    set g_isNewFile 0
	} 

	itk_component add data_type {
	    iwidgets::optionmenu $itk_interior.datatype \
		-labeltext "Image data type:" -labelpos w \
                -font g_titleFont -labelfont g_titleFont
	}
	pack $itk_component(data_type) -fill x -expand 1
        $itk_component(data_type) insert 0 \
              {Signed Byte (8)} {Unsigned Byte (8)} \
              {Signed Integer (16)} {Unsigned Integer (16)} \
              {Signed Long (32)} {Unsigned Long (32)} \
              {Signed Long Long (64)} \
              {Float (-32)} {Double (-64)}
        $itk_component(data_type) select 0
	
	itk_component add axis_list {
	    iwidgets::entryfield $itk_interior.axis_list \
		-labeltext "Image dimensions:" -labelpos w \
                -labelfont g_titleFont -textfont g_titleFont \
		-textvariable [itcl::scope _axis_list] 
	}
	pack $itk_component(axis_list) -fill x -expand 1

    }

    destructor {}
}

itcl::body NewImage::setFileName {fname} {
    set fileName $fname
}

itcl::body NewImage::create_new_fits_image {} {
    global isMac
    
    if { $g_isNewFile ==1 } {
	set filemode 2
    } else {
	set filemode 1
    }

    if { [catch {set newfits [fits open $fileName $filemode]} err] } {
	error $err
	return
    }
    
    set data_type [$itk_component(data_type) get]

    if { ![regexp {^([^ ]*) .*\((.*)\)} $data_type dmy option bitpix] } {
       error "Bad data type value"
    }

    if { $g_isNewFile == 0} {
       $newfits move [$newfits info nhdu] 
       $newfits put ihd $bitpix $_naxes $_axis_list
    } else {
       $newfits put ihd -p $bitpix $_naxes $_axis_list
    }

    if { ($option=="Unsigned" && $bitpix!=8) \
          || ($option=="Signed" && $bitpix==8) } {
       # Write keywords to offset values
       switch $bitpix {
          8 {
             set bzero -128
          }
          16 {
             set bzero 32768
          }
          32 {
             set bzero 2147483648
          }
       }
       $newfits put keyword "BZERO  $bzero  Make values $option"
       $newfits put keyword "BSCALE 1       Make values $option"
    }
    $newfits close
    if { $isMac && $g_isNewFile } {
        file attributes $fileName -creator "fvEd" -type "FITS"
    }
}

itcl::body NewImage::_close {} {
    itcl::delete object $this
}


itcl::body NewImage::go {} {
    if { $fileName == ""} {
	error "Please give me a name for the file"
    }
    
    set _axis_list [join [split $_axis_list {, }]]
    set _naxes [llength $_axis_list]
    create_new_fits_image 
    _close 
}
