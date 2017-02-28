proc modid { args } {
# guesses IDs of lines in the model
# kaa 1/14/99
# kaa 1/5/00  modified for xspec v11

# first check if the user input a ? for help

    if {![llength $args] || [lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
       puts "Usage: modid delta|conf"
       puts "Writes out possible IDs for lines in the model"
       puts " "
       puts "For each gaussian or lorentzian line in the model run"
       puts "the identify command. If a number is given as an argument"
       puts "then that is used as the delta energy for identify. If"
       puts "the string conf is given as the argument then the last"
       puts "calculated confidence regions are searched for possible"
       puts "line IDs. If no argument is given then conf is assumed"
       return
    }

# now check that the user actually has a model set up

    tclout modcomp
    set number_models $xspec_tclout
    if { $number_models == 0} {
       puts "You must set up a continuum model first"
       return
    }

# parse the argument. check whether it is the string conf. If no argument
# then assume conf.

    set qconf 0 
    if { [llength $args] > 0 && [lindex $args 0] != "conf" } {
       set qconf 1
       set deltaE [lindex $args 0]
    }

# loop over the model components

    for {set imodel 1} {$imodel<=$number_models} {incr imodel} {

# get the model name and check whether it is one of the line models

       tclout compinfo $imodel
       scan $xspec_tclout "%s %d %d" compname startparam numparam

       if { $compname == "gaussian" || $compname == "zgauss" || $compname == "lorentz" } {

# if we are not using the confidence region then just get the energy

	  if { $qconf == 1 } {

             tclout param $startparam
             scan $xspec_tclout "%f" energy

          } else {

# otherwise get the confidence region information and calculate the energy
# and delta to pass to the identify command

             tclout error $startparam
             scan $xspec_tclout "%f %f" low_limit high_limit
             set energy [ expr 0.5*($high_limit + $low_limit) ]
             set deltaE [ expr 0.5*($high_limit - $low_limit) ]

          }

# and run the identify command

          puts [format "Doing model component %d (%s)..." $imodel $compname]

          puts [format "XSPEC> identify %f %f" $energy $deltaE]
          identify $energy $deltaE

       }

# end loop over model components

    }

    return

}
