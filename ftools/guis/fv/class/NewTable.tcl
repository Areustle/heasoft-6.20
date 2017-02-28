itcl::class NewTable {
    inherit  itk::Widget

    private variable fileName
    private variable g_isNewFile 1
    private variable _table_type BINARY

    private method _close {}
    private method create_new_fits_table {type}
    public method go {}

    constructor {args} {
	global checkBBgColor
        global g_titleFont

	if { [llength $args] != 1} {
	    error "Too many args in NewTable"
	    _close
	}
	set fileName [lindex $args 0]

	if { [file exist $fileName] == 1 } {
	    set g_isNewFile 0
	} 

	iwidgets::radiobox $itk_interior.type -labeltext "Table Type" \
                -labelfont g_titleFont \
		-labelpos nw -selectcolor $fvPref::checkBBgColor 
	pack $itk_interior.type -fill x -expand 1
	$itk_interior.type add BINARY -text "Binary"
	$itk_interior.type add ASCII  -text "ASCII"

        $itk_interior.type select BINARY

	#$itk_component(tabletype) add BINARY -text "Binary"
	#$itk_component(tabletype) add ASCII  -text "ASCII"

        #$itk_component(tabletype) select BINARY
    }

    destructor {}
}

itcl::body NewTable::go {} {
    set _table_type [$itk_interior.type get]
    
    create_new_fits_table $_table_type 
    _close 
}

itcl::body NewTable::create_new_fits_table {type} {

    if { $g_isNewFile ==1 } {
	set filemode 2
    } else {
	set filemode 1
    }
    if { [catch {set fitscmd [fits open $fileName $filemode]} err] == 1 } {
	error $err
	return
    }

    if { $g_isNewFile == 0} {
	$fitscmd move [$fitscmd info nhdu] 
    }

    if { $_table_type == "ASCII" } {
	$fitscmd put ahd 1 0 {} {} {} {} "ASCIITable" 0
    } else {
	$fitscmd put bhd 1 0 {} {} {} "BinTable" 
    }
    $fitscmd close
}

itcl::body NewTable::_close {} {
    itcl::delete object $this
}







