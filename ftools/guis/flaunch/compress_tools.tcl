#----------------------------------------------------------------------
# This file contains a group of tools which can be uses to un/compress, 
# un/zip, or g(un)zip a file, or a group of input files. 

#----------------------------------------------------------------------
# This procedure acts as the first level screening for any files 
# that are selected. Thus this procedure looks at the filename 
# selected and checks to see if 
proc type_and_uncompress_file { file_input } {
    global fileselect_uncompress_setup

    # Remove any white space around the filename.
    set file_selected [string trim $file_input]

    # Reinitialize all variables that tell us what the file type is.
    set index_str 0
    set tar_file 0
    set file_not_compressed 0
    
    # puts "Attempting to uncompress file"
    # puts "Input file is $file_selected"

    if { $fileselect_uncompress_setup == 0 } {
	return $file_selected
    }

    set strlen [ string length $file_selected ]
    if { $strlen < 1 } {
	return $file_selected
    }

    
    if ![file exists $file_selected] {
	return $file_input
    }

    set strindex 0

    set strindex [ string first ".gz" $file_selected ]
    if { $strindex > 0 } {
	# puts "Attempting to decompress $file_selected"
	set temp_filename [ strip_filename $file_selected $strindex ]
	set index_str $strindex
	set strindex 0
	set temp "gzip -d $file_selected"
	exec sh -c $temp

    }

    set strindex [ string first ".z" $file_selected ]
    if { $strindex > 0 } {
	# puts "Attempting to decompress $file_selected"
	set temp_filename [ strip_filename $file_selected $strindex ]
	set index_str $strindex
	set strindex 0
	set temp "gzip -d $file_selected"
	exec sh -c $temp

    }

    set strindex [ string first "-z" $file_selected ]
    if { $strindex > 0 } {
	# puts "Attempting to decompress $file_selected"
	set temp_filename [ strip_filename $file_selected $strindex ]
	set index_str $strindex
	set strindex 0
	set temp "gzip -d $file_selected"
	exec sh -c $temp

    }

    set strindex [ string first ".Z" $file_selected ]
    if { $strindex > 0 } {
	# puts "Attempting to decompress $file_selected"
	set temp_filename [ strip_filename $file_selected $strindex ]
	set index_str $strindex
	set strindex 0
	set temp "gzip -d $file_selected"
	exec sh -c $temp

	if ![ file exists $temp_filename ] {
	    set temp "uncompress $file_selected"
	    exec sh -c $temp
	} 

    }

    set strindex [ string first ".tgz" $file_selected ]
    if { $strindex > 0 } {
	# puts "Attempting to decompress $file_selected"
	set temp_filename [ strip_filename $file_selected $strindex ]
	set index_str $strindex
	set strindex 0
	set temp "gzip -d $file_selected"
	exec sh -c $temp

	set file_selected_temp [concat $temp_filename ".tar"]
	if [ file exists $file_selected_temp ] {
	    set file_selected $file_selected_temp
	}

    }

    set strindex [ string first ".taz" $file_selected ]
    if { $strindex > 0 } {
	# puts "Attempting to decompress $file_selected"
	set temp_filename [ strip_filename $file_selected $strindex ]
	set index_str $strindex
	set strindex 0
	set temp "gzip -d $file_selected"
	exec sh -c $temp

	set file_selected_temp [concat $temp_filename ".tar"]
	if [ file exists $file_selected_temp ] {
	    set file_selected $file_selected_temp
	}

    }

    set strindex [ string first ".zip" $file_selected ]
    if { $strindex > 0 } {
	set temp_filename [ strip_filename $file_selected $strindex ]

	set index_str $strindex
	set strindex 0

	exec sh -c $temp

    }

    set strindex [ string first ".tar" $file_selected ]
    if { $strindex > 0 } {
	set temp_filename [ strip_filename $file_selected $strindex ]
	
	set index_str $strindex
	set tar_file 1
	set strindex 0
	# puts "Trying to untar tar file"

    }

    if [ info exists temp_filename ] {
	if [ file exists $temp_filename ] {
	    return $temp_filename
	} else {
	    # puts "Decompress attempt failed, returning original file"
	}

    }

    return $file_selected

}

#----------------------------------------------------------------------
proc strip_filename { file_selected index_str } {

    set index_str [expr $index_str - 1 ]
    set matchit [ string match ".gz" $file_selected ]
    set filename [ string range $file_selected 0 $index_str ]
    return $filename

}

#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------