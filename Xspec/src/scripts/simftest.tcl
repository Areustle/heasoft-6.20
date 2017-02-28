proc simftest { args } {
# performs an F-test - built on top of the lrt procedure.

    if {![llength $args] || [lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
	puts "Usage: simftest model_comp niter filename chatter"
	puts "Runs <niter> sets of simulated datasets to determine F-test probability for"
        puts "the model component <model_comp>. If <filename> is written then passes this"
	puts "to lrt.tcl to save likelihood ratio simulation information. The first line"
	puts "of the file contains the results for the data, the other lines for the"
	puts "simulations. Each line comprises the statistic value for the model without"
	puts "model_comp, the statistic value for the model with model_comp, and the"
	puts "difference. If chatter is set then this chatter level will be used. The"
        puts "default is zero so no information will be written to the screen."
	puts " "
	puts "kaa  v1.2  07/26/16"
	return
    }

# parse the arguments

    set model_comp [lindex $args 0]

    set niter 100
    if {[llength $args] > 1} {
       set niter [lindex $args 1]
    }

    set filename "none"
    if {[llength $args] > 2} {
       set filename [lindex $args 2]
    }

    set chatlevel 0
    if {[llength $args] > 3} {
       set chatlevel [lindex $args 3]
    }

# save the current chatter level and set low

    set savechatlevel [scan [tcloutr chatter] "%d"]
    if {[catch {
       chatter $chatlevel

   # First save the current model in model_with_comp.xcm.

       fit 500
       save model model_with_comp.xcm

   # Save the model with the <model_comp> component missing.

       delcomp $model_comp
       fit 500
       save model model_without_comp.xcm

   # run the likelihood ratio test script

       set probability [expr 1.0 - [lrt $niter model_without_comp model_with_comp $filename $chatlevel]]

       puts "Probability that data are consistent with model without extra component = $probability"

   # now get rid of the temporary model files

       rm model_with_comp.xcm model_without_comp.xcm
    } result]} {
       chatter $savechatlevel
       rm model_with_comp.xcm model_without_comp.xcm
       return -code error 
    }
# restore the chatter level

    chatter $savechatlevel

    return $probability
}
