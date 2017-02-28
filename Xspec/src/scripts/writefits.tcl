proc writefits { args } {
# writes a bunch of information to a single line of a FITS file

# first check if the user input a ? for help

    if {![llength $args] || [lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
       puts "Usage: writefits FITSfile"
       puts "Writes information to a single line a FITS file."
       puts "This script requires only one dataset to have been read in. The"
       puts "error command should have been run for any parameters for which"
       puts "error output is required"
       puts " "
       puts "kaa  v1.0  11/06/08"
       return
    }

# now check that the user actually has data read in and a model set up

    set prompt "XSPEC12>"

    if {[tcloutr modcomp] == "0"} {
       puts "You must set up a model first"
       return
    }
    if {[tcloutr datasets] == "0"} {
       puts "You must set read in data first"
       return
    }

# parse the arguments - FITS outfile

    set FITSfile "out.fits"
    if {[llength $args] > 0} {
       set FITSfile [lindex $args 0]
    }

    set FITSfileexists [file exists $FITSfile]

# get the names of the files

    set sourcefile [tcloutr filename]
    set index [string last / $sourcefile]
    if { $index == "-1" } {
	set path ""
    } else {
	set path [string range $sourcefile 0 $index]
        set index [expr $index+1]
	set last [expr [string length $sourcefile]-1]
        set sourcefile [string range $sourcefile $index $last]
    }

    set backfile [string trim [tcloutr backgrnd]]
    if {$backfile eq ""} {set backfile "none"}

    set respfile [string trim [tcloutr response]]
    if {$respfile eq ""} {set respfile "none"}

    set arffile [string trim [tcloutr arf]]
    if {$arffile eq ""} {set arffile "none"}

# get the exposure time

    set time [tcloutr expos]

# we will need to know the number of parameters

    set numpar [tcloutr modpar]

# get the parameter information

    for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
	if {[scan [tcloutr param $ipar] "%f %f" tmp1 tmp2] == 2} {
            set sparval($ipar) $tmp1
            set spardel($ipar) $tmp2
	    scan [tcloutr error $ipar] "%f %f" sparerrlow($ipar) sparerrhi($ipar)
        } else {
            set sparval($ipar) $tmp1
            set spardel($ipar) -1
        }
    }

# get the statistic value

    set statvalue [tcloutr stat]

# open a text version of the output file

    set txtfile $FITSfile
    append txtfile "-txt"
    rm -f $txtfile
    set fileid [open $txtfile w]

# write the text output

    set outstr "$path $sourcefile $backfile $respfile $arffile $time $statvalue "
    for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
	if { $spardel($ipar) > 0 } {
	    append outstr "$sparval($ipar) $sparerrlow($ipar) $sparerrhi($ipar) "
	}
    }
    
    puts $fileid $outstr

# close the output text file

    close $fileid

# create a temporary file for the column descriptors

    set cdfile $FITSfile
    append cdfile "-cd"
    rm -f $cdfile
    set fileid [open $cdfile w]

    puts $fileid "DIRPATH 72A"
    puts $fileid "PHAFILE 72A"
    puts $fileid "BACKFILE 72A"
    puts $fileid "RESPFILE 72A"
    puts $fileid "ARFFILE 72A"
    puts $fileid "EXPOSURE E seconds"
    puts $fileid "STATISTIC E"
    for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
	if { $spardel($ipar) > 0 } {
	    set punit " "
	    scan [tcloutr pinfo $ipar] "%s %s" pname punit
	    puts $fileid [concat $pname$ipar " E " $punit]
	    puts $fileid [concat E$pname$ipar " 2E " $punit]
	}
    }
    close $fileid

# create a FITS file with the parameter and stat output - if the file exists
# append the row onto the current file

    if { $FITSfileexists == 1 } {
	set tmpfile $FITSfile
	append tmpfile "-tmp"
	ftcreate cdfile=$cdfile datafile=$txtfile outfile=$tmpfile
	set tmpfile2 $FITSfile
	append tmpfile2 "-tmp2"
	ftmerge infile="$FITSfile,$tmpfile" outfile=$tmpfile2
	mv $tmpfile2 $FITSfile
	rm -r $tmpfile
    } else {
	ftcreate cdfile=$cdfile datafile=$txtfile outfile=$FITSfile
    }

# tidy up the temporary files

    rm -f $cdfile $txtfile

}
