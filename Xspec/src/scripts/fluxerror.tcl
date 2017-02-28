proc fluxerror { args } {
# this estimates the confidence region on the flux for the current model assuming
# with the model parameters listed set to zero. Useful for calculating the error
# on the unabsorbed flux or the flux of a component in the model


# first check if the user input a ? for help

    if {![llength $args] || [lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
	puts "Usage: fluxerror niter param-list confidence-level"
	puts "Runs <niter> sets of simulated parameters, calculates the flux with"
	puts "the parameters in <param-list> set to zero then writes out the"
        puts "<confidence-level> confidence range"
	puts " "
	puts "Before running this procedure you must have fit the model"
	puts " "
	puts "kaa  v1.1  07/20/07"
	return
    }

# now check that the user actually has data read in and a model set up

    if {[tcloutr modcomp] == "0"} {
       puts "You must set up a model first"
       return
    }
    set numdata [tcloutr datasets]
    if {$numdata == "0"} {
       puts "You must set read in data first"
       return
    }

# parse the arguments - niter, param-list, and confidence-level

    set niter 100
    if {[llength $args] > 0} {
       set niter [lindex $args 0]
    }
    set paramlist [list]
    if {[llength $args] > 1} {
       set paramlist [lindex $args 1]
    }
    set confidencelevel [list]
    if {[llength $args] > 2} {
       set confidencelevel [lindex $args 2]
    }


# save the chatter level then set to a low chatter

    set chatlevel [scan [tcloutr chatter] "%d"]
    chatter 0

# we will need to know the number of parameters

    set numpar [tcloutr modpar]

# save the parameter information so we can reset it each time.

    for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
	scan [tcloutr param $ipar] "%f" sparval($ipar)
	set sparislink($ipar) [string range [tcloutr plink $ipar] 0 0]
	set sparlink($ipar) [string trimleft [tcloutr plink $ipar] ?TF?]
    }

# set up lists to save the fluxes

    for {set idata 1} {$idata <= $numdata} {incr idata} {
	set fluxes($idata) [list]
    }

# loop round iterations

    for {set iter 1} {$iter <= $niter} { incr iter} {

# get the random set of parameters

       tclout simpars

# turn the string of parameters into a Tcl list

       regsub -all { +} [string trim $xspec_tclout] { } cpars
       set lpars [split $cpars]

# set the parameters in paramlist to zero

       foreach p $paramlist {
          set lpars [lreplace $lpars [expr $p-1] [expr $p-1] 0.0]
       }

# set the parameters to these values

       set outstr " "
       for {set ipar 0} {$ipar < $numpar} {incr ipar} {
	   append outstr "& [lindex $lpars $ipar] "
       }
       newpar 1-$numpar $outstr

# calculate the flux

       flux

# save the flux output in the list

       for {set idata 1} {$idata <= $numdata} {incr idata} {
	   lappend fluxes($idata) [scan [tcloutr flux $idata] "%f"]
       }

# reset the model parameters

       set outstr " "
       for {set ipar 1} {$ipar <= $numpar} {incr ipar} {
           if {$sparislink($ipar) == "F"} {
	       append outstr "& $sparval($ipar) "
	   } else {
	       append outstr "& $sparlink($ipar) "
	   }
       }
       newpar 1-$numpar $outstr

# run a fit (which should converge instantly)

       fit

# end loop over iterations

    }

# sort the list of fluxes into increasing numerical order

    for {set idata 1} {$idata <= $numdata} {incr idata} {
	set sorted_fluxes($idata) [lsort -real -increasing $fluxes($idata)]
    }

# and write out the percentage points for the requested confidence level

    set low [expr ($niter*(100-$confidencelevel)/200)-1]
    set high [expr $niter-$low-1]

    puts " "
    puts "$confidencelevel% confidence range on unabsorbed flux : "
    if {$numdata == "1"} {
	puts "  [lindex $sorted_fluxes(1) $low] [lindex $sorted_fluxes(1) $high]"
    } else {
	for {set idata 1} {$idata <= $numdata} {incr idata} {
	    puts "  [lindex $sorted_fluxes($idata) $low] [lindex $sorted_fluxes($idata) $high] for dataset $idata"
	}
    }
    puts " "
    
# reset the chatter level to that on input

    chatter $chatlevel

}
