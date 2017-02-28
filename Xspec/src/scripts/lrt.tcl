proc lrt { args } {
# test for model model1 relative to model0. Note that the user must have created
# command files defining the two models.

# first check if the user input a ? for help

    if {![llength $args] || [lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
	puts "Usage: lrt niter model0_name model1_name filename chatter"
	puts "Runs <niter> sets of simulated datasets based on <model0_name>, calculates"
        puts "the likelihood ratio for <model1_name> relative to <model0_name> (calculated"
	puts "by the statistic for model0 minus the statistic for model1), and outputs"
        puts "the fraction of iterations with the likelihood ratio smaller than that for the"
        puts "data. If the optional filename is given then the iteration results are written"
        puts "to the file. The first line of the file contains the results for the data,"
	puts "the other lines for the simulations. Each line comprises the statistic value"
	puts "for model0, the statistic value for model1, and the difference. If chatter is"
	puts "this is used as the chatter level. The default is zero so no information will"
	puts "written to the screen."
	puts " "
	puts "Before running this procedure you must have created command files called"
        puts "<model1_name>.xcm and <model0_name>.xcm which define the two models. A"
        puts "good way to do this is to set up the model then use save model to make"
        puts "the command file"
        puts " "
	puts "kaa  v1.3  07/26/16"
	return
    }

    set prompt "XSPEC12>"

# parse the arguments - niter

    set niter 100
    if {[llength $args] > 0} {
       set niter [lindex $args 0]
    }

# model0 name

    set model0 "model0"
    if {[llength $args] > 1} {
       set model0 [lindex $args 1]
    }
    append model0 ".xcm"

# model1 name
    
    set model1 "model1"
    if {[llength $args] > 2} {
       set model1 [lindex $args 2]
    }
    append model1 ".xcm"

# output filename

    set filename "none"
    if {[llength $args] > 3} {
       set filename [lindex $args 3]
    }

# chatter level

    set chatlevel 0
    if {[llength $args] > 4} {
       set chatlevel [lindex $args 4]
    }


# now check that the user actually has data read in

    set numdata [tcloutr datasets]
    if {$numdata == "0"} {
       puts "You must read in data first"
       return
    }

# save the filenames

    for {set iset 1} {$iset <= $numdata} {incr iset} {

	set sourcefile($iset) [tcloutr filename $iset]

	set datagroup($iset) [tcloutr datagrp $iset]

	set backfile($iset) [string trim [tcloutr backgrnd $iset]]
	if {$backfile($iset) eq ""} {set backfile($iset) "none"}

	set respfile($iset) [string trim [tcloutr response $iset]]
	if {$respfile($iset) eq ""} {set respfile($iset) "none"}

	set arffile($iset) [string trim [tcloutr arf $iset]]
	if {$arffile($iset) eq ""} {set arffile($iset) "none"}

# save the noticed parameters

	set noticed($iset) [tcloutr noticed $iset]

    }

# save the chatter level then set to a low chatter

    set savechatlevel [scan [tcloutr chatter] "%d"]
    chatter $chatlevel

# save the parameter information for the two models

    @$model1
    fit
    fit
    fit

    set numpar1 [tcloutr modpar]
    for {set ipar 1} {$ipar <= $numpar1} {incr ipar} {
	scan [tcloutr param $ipar] "%f" sparval1($ipar)
	set sparislink1($ipar) [string range [tcloutr plink $ipar] 0 0]
	set sparlink1($ipar) [string trimleft [tcloutr plink $ipar] ?TF?]
    }

    set stat1 [tcloutr stat]

    @$model0
    fit
    fit
    fit

    set numpar0 [tcloutr modpar]
    for {set ipar 1} {$ipar <= $numpar0} {incr ipar} {
	scan [tcloutr param $ipar] "%f" sparval0($ipar)
	set sparislink0($ipar) [string range [tcloutr plink $ipar] 0 0]
	set sparlink0($ipar) [string trimleft [tcloutr plink $ipar] ?TF?]
    }

# calculate the likelihood ratio statistic for the input data

    set stat0 [tcloutr stat]
    set lrt_obs [expr $stat0 - $stat1]

# if required save this information to the first line of filename

    if {$filename ne "none"} {
	set fileid [open $filename w]
	set outstr "observed: $stat0 $stat1 $lrt_obs"
	puts $fileid $outstr
    }

# set up list to save the likelihood ratio

    set likelihoodratio [list]

# create array of strings containing random sets of parameters. do this first to avoid having to
# continually reload the original data to reset the correlation matrix

    for {set i 0} {$i < $niter} { incr i} {
	tclout simpars
	set simpararray($i) $xspec_tclout
    }

# loop round iterations

    for {set iter 0} {$iter < $niter} { incr iter} {

	set simparstr $simpararray($iter)

# turn the string of parameters into a Tcl list

	regsub -all { +} [string trim $simparstr] { } cpars
	set lpars [split $cpars]

# set the parameters to these values

	set outstr " "
	for {set ipar 0} {$ipar < $numpar0} {incr ipar} {
	    append outstr "& [lindex $lpars $ipar] "
	}
	newpar 1-$numpar0 $outstr

# fake the spectra

	set outstr " nowrite & y & & "
	for {set iset 1} {$iset <= $numdata} {incr iset} {
	    append outstr "junk$iset.fak & & "
	}
	fakeit $outstr

# ignore the appropriate channels

	model none
	for {set iset 1} {$iset <= $numdata} {incr iset} {
	    ignore $iset:1-**
	    notice $iset:$noticed($iset)
	}

# do a fit to model1 - need to decide how to handle errors

	@$model1
	fit
	fit
	fit

# save the fit statistic value

	set model1stat [tcloutr stat]

# now switch to model0

        @$model0

# and do a fit

	fit
	fit
	fit

# add to the next member of the likelihoodratio list

	set model0stat [tcloutr stat]
	set lrt_sim [expr $model0stat - $model1stat]
	lappend likelihoodratio $lrt_sim

	if {$filename ne "none"} {
	    set outstr "$model0stat $model1stat $lrt_sim"
	    puts $fileid $outstr
	}

# now restore original background file because we need to use this on
# each iteration

        for {set iset 1} {$iset <= $numdata} {incr iset} {
	    if { $backfile($iset) ne "none" } { 
	        back $iset $backfile($iset)
	    }
        }

# end loop over iterations

    }

# now restore the original data, response, and background
# - this is slightly convoluted so as to avoid a reread of the response
# (which might be slow)

    for {set iset 1} {$iset <= $numdata} {incr iset} {
	data $datagroup($iset):[expr $numdata+$iset] $sourcefile($iset)
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
    for {set iset 1} {$iset <= $numdata} {incr iset} {
	ignore $iset:1-**
	notice $iset:$noticed($iset)
    }

# restore model0 with its original parameters

    @$model0
    fit
    fit
    fit

# close the output file if necessary

    if {$filename ne "none"} {
        close $fileid
    }

# sort the likelihood ratios into increasing numerical order

    set sorted_ltr [lsort -increasing -real $likelihoodratio]

# find the position in the list occupied by the observed dataset

    if {$lrt_obs < [lindex $sorted_ltr 0]} {
	set fraction 0
    }

    if {$lrt_obs > [lindex $sorted_ltr [expr $niter-1]]} {
	set fraction 1
    }


# Note the 1.0 in the expr calculation to force floating point

    for {set iter 0} {$iter < $niter} { incr iter} {

	if {$lrt_obs > [lindex $sorted_ltr $iter]} {
	    set fraction [expr 1.0*($iter+1)/$niter]
	}

    }


# reset the chatter level

    chatter $savechatlevel

# return the fraction of simulated LRTs less than the observed.

    return $fraction

}
