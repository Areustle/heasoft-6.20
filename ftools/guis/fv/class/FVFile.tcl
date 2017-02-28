itcl::class FVFile {

    constructor {args} {}
    destructor {}

    private variable fileName
    private variable extNum 1
    private variable openMode 0 
    private variable fFileName 
    private variable isOpen 0
    private variable colList {}
    private variable xcolumn {}
    private variable ycolumn {}
    private variable xecolumn {}
    private variable yecolumn {}

    
    public method setFileName {fname} 
    public method setFileMode {fmode}
    public method setExtension {extnum}
    public method displayTable {}
    public method displayHeader {}
    public method displayImage {}
    public method plotTable {}
    public method setColumnList {args}
    public method setXColumn {x} 
    public method setYColumn {y} 
    public method setXErrorColumn {xe} 
    public method setYErrorColumn {ye} 

    private method openFVFile {} 
}

itcl::body FVFile::constructor {} {

}

itcl::body FVFile::setFileName {fname} {
    set fileName $fname
}

itcl::body FVFile::setFileMode {fmode} {
# rw =0 , r = 1, new = 2
    
    switch $fmode {
	"new" {
	    set file_mode 2
	}
	"r" {
	    set file_mode 1
	}
	"wr" -
	"rw" {
	    set file_mode 0
	}
	default {
	    set file_mode $fmode
	}
    }
    set openMode $file_mode
}

itcl::body FVFile::setExtension {extnum} {
# primary array extnum = 1
    set extNum $extnum
}

itcl::body FVFile::openFVFile {} {
    if { $isOpen == 1} return
# _father is only for displaying the highlight. open as read-only
    set fFileName [openFitsFileWM $fileName $openMode]
    set isOpen 1
}

itcl::body FVFile::displayTable {} {
    openFVFile

   $fFileName openTable $extNum - 1
}

itcl::body FVFile::plotTable {} {
    openFVFile

    if { $xcolumn == {} || $ycolumn == {} } {
	puts "You need to specify X and Y column names for plotting"
	return 
    }
    $fFileName plotData $extNum [list $xcolumn $xecolumn $ycolumn $yecolumn 0]
}

itcl::body FVFile::displayHeader {} {
    openFVFile

    $fFileName openHeader $extNum
}

itcl::body FVFile::displayImage {} {
    openFVFile

    $fFileName plotData $extNum
}


itcl::body FVFile::setColumnList {args} {
    set colList [join $args]
}


itcl::body FVFile::setXColumn {x} {
    set xcolumn $x
}

itcl::body FVFile::setYColumn {y} {
    set ycolumn $y
}

itcl::body FVFile::setXErrorColumn {xe} {
    set xecolumn $xe
}

itcl::body FVFile::setYErrorColumn {ye} {
    set yecolumn $ye
}
