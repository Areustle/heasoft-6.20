proc multifake { args } {
# runs multiple fakeits - assume that source, background, response etc.
# have all been set up correctly - and saves the results in a file

# first check if the user input a ? for help

    if {![llength $args] || [lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
       puts "Usage: multifake time niter outfile"
       puts "Runs <niter> iterations of fakeit with exposure of <time> and "
       puts "writes the results to <outfile>"
       puts " "
       puts "Before running this procedure you must have read in the datasets"
       puts "along with their response files and optional background"
       puts "and arf files. You must also have defined the model."
       puts " "
       puts "The output file is a FITS binary table with the columns being the"
       puts "value fit for each parameter in each iteration. The final column"
       puts "is the statistic value."
       puts " "
       puts "Note that if an error occurs during the fit of a faked spectrum"
       puts "then -999 is written for all parameters and the statistic value"
       puts "for that iteration."
       puts " "
       puts "kaa  v1.1  3/22/13"
       return
    }

# now check that the user actually has data read in and a model set up

    set prompt "XSPEC12>"

    if {[tcloutr modcomp] == "0"} {
       puts "You must set up a model first"
       return
    }
    set numdata [tcloutr datasets]
    if {$numdata == "0"} {
       puts "You must read in data first"
       return
    }

# parse the arguments - time, niter, and outfile

    set time 1000
    if {[llength $args] > 0} {
       set time [lindex $args 0]
    }
    set niter 100
    if {[llength $args] > 1} {
       set niter [lindex $args 1]
    }
    set outfile "fakeout.fits"
    if {[llength $args] > 2} {
       set outfile [lindex $args 2]
    }

# save the query setting and set to yes for this procedure

    set query [tcloutr query]
    query yes

# save the chatter level then set to a low chatter

    set chatlevel [scan [tcloutr chatter] "%d"]
    chatter 0

# save the names of the files

    for {set iset 1} {$iset <= $numdata} {incr iset} {

	set sourcefile($iset) [tcloutr filename $iset]

	set backfile($iset) [string trim [tcloutr backgrnd $iset]]
	if {$backfile($iset) eq ""} {set backfile($iset) "none"}

	set respfile($iset) [string trim [tcloutr response $iset]]
	if {$respfile($iset) eq ""} {set respfile($iset) "none"}

	set arffile($iset) [string trim [tcloutr arf $iset]]
	if {$arffile($iset) eq ""} {set arffile($iset) "none"}

# save the noticed channels

	set noticed($iset) [tcloutr noticed $iset]

    }

# open a text version of the output file

    set txtfile $outfile
    append txtfile "-txt"
    rm -f $txtfile
    set fileid [open $txtfile w]

# we will need to know the number of parameters

    set numpar [tcloutr modpar]

# save the parameter information so we can reset it for each fit.

    for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
	scan [tcloutr param $ipar] "%f %f" sparval($ipar) spardel($ipar)
    }

# loop round fakeit iterations

    for {set iter 1} {$iter <= $niter} { incr iter} {

# reset the parameter values to the original input

       set outstr " "       
       for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
	  append outstr "& $sparval($ipar) $spardel($ipar) "
       }
       newpar 1-$numpar $outstr

# fake the spectra

       set outstr " nowrite & y & & "
       for {set iset 1} {$iset <= $numdata} {incr iset} {
	   append outstr "junk$iset.fak & $time & "
       }
       fakeit $outstr

# ignore the appropriate channels

       for {set iset 1} {$iset <= $numdata} {incr iset} {
	   ignore $iset:1-**
	   notice $iset:$noticed($iset)
       }

# do a fit - if an error results then write the value of -999 to all columns

       set outstr " "
       if { [catch { fit 500 }] } {

          for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
	     if { $spardel($ipar) > 0 } { append outstr "-999 " }
          }
          append outstr "-999"

       } else {

# write the free parameter values and fit statistic to the file. Note
# can use the saved parameter deltas to know which parameter should
# be written

          for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
	     tclout param $ipar
	     if { $spardel($ipar) > 0 } { append outstr [scan $xspec_tclout "%f"] " " }
          }
          tclout stat
          append outstr [scan $xspec_tclout "%f"] 
      }

       puts $fileid $outstr

# now restore original background file because we need to use this on
# each iteration

       for {set iset 1} {$iset <= $numdata} {incr iset} {
	   if { $backfile($iset) ne "none" } { 
	       back $iset $backfile($iset)
	   }
       }

# and set the channels

       for {set iset 1} {$iset <= $numdata} {incr iset} {
	   ignore $iset:1-**
	   notice $iset:$noticed($iset)
       } 

# end loop over fakeit iterations

    }

# close the output file

    close $fileid

# create a temporary file for the column descriptors

    set cdfile $outfile
    append cdfile "-cd"
    rm -f $cdfile
    set fileid [open $cdfile w]

    for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
       set punit " "
       scan [tcloutr pinfo $ipar] "%s %s" pname punit
       if { $spardel($ipar) > 0 } { puts $fileid [concat $pname$ipar " E " $punit] }
    }
    puts $fileid "stat E"
    close $fileid

# create a header file with information about the response and model used
# to run this simulation

    set headfile $outfile
    append headfile "-head"
    rm -f $headfile
    set fileid [open $headfile w]
    for {set iset 1} {$iset <= $numdata} {incr iset} {
	puts $fileid [concat "SRC$iset " $sourcefile($iset)]
	puts $fileid [concat "BACK$iset " $backfile($iset)]
	puts $fileid [concat "RESP$iset " $respfile($iset)]
	puts $fileid [concat "ARF$iset " $arffile($iset)]
	puts $fileid [concat "CHAN$iset " $noticed($iset)]
    }
    puts $fileid [concat "EXPOSURE " $time]
    tclout model
    puts $fileid [concat "MODEL    " '[string trim $xspec_tclout]']

    for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
       puts $fileid [concat "PARAMV$ipar" $sparval($ipar)]
       puts $fileid [concat "PARAMD$ipar" $spardel($ipar)]
    }

    close $fileid


# create a FITS file with the parameter and stat output

    rm -f $outfile
    fcreate cdfile=$cdfile datafile=$txtfile outfile=$outfile headfile=$headfile

# tidy up the temporary files

    rm -f $cdfile $txtfile $headfile

# reset the query option

    query $query

# reset the model parameters

    set outstr " "
    for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
       append outstr "& $sparval($ipar) $spardel($ipar) "
    }
    newpar 1-$numpar $outstr

# and the data, response, and background - slightly convoluted so as to avoid
# a reread of the response (which might be slow)

    for {set iset 1} {$iset <= $numdata} {incr iset} {
	data [expr $numdata+$iset] $sourcefile($iset)
	response [expr $numdata+$iset] $respfile($iset)
	if { $backfile($iset) ne "none" } { 
	    back [expr $numdata+$iset] $backfile($iset)
	}
	if { $arffile($iset) ne "none" } { 
	    arf [expr $numdata+$iset] $arffile($iset)
	}
    }
    for {set iset 1} {$iset <= $numdata} {incr iset} {
	data none /
    }

# and set the channels

    for {set iset 1} {$iset <= $numdata} {incr iset} {
	ignore $iset:1-**
	notice $iset:$noticed($iset)
    } 

# reset the chatter level

    chatter $chatlevel

}
