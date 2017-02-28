proc addline { args } {
# adds lines to the model at energies of largest residuals
# kaa 1/6/99   
# kaa 1/5/00  modified for xspec v11

# first check if the user input a ? for help

    if {![llength $args] || [lindex $args 0] == "?" || [lindex $args 0] == "-h"} {
       puts "Usage: addline nlines modeltype fit|nofit"
       puts "Adds to the model nlines additional lines of type modeltype"
       puts " "
       puts "The nlines additional lines are added one at a time."
       puts "Line energies are set to that of the largest residual"
       puts "between the data and the model. For each line a fit is"
       puts "performed with the line width and normalization as the only"
       puts "free parameters."
       puts " "
       puts "The default options are one line and a gaussian. The other"
       puts "model type that can be used is lorentz. If no third argument"
       puts "is given then the sigma and normalization of each line are"
       puts "fit. If nofit is specified then the fit is not performed but"
       puts "if fit is specified then all free parameters are fit."
       puts " "
       puts "kaa  1/5/00"
       return
    }

# now check that the user actually has data read in and a model set up

    set prompt "XSPEC12>"

    tclout modcomp
    if {$xspec_tclout == "0"} {
       puts "You must set up a continuum model first"
       return
    }
    tclout datagrp
    if {$xspec_tclout == "0"} {
       puts "You must set read in data first"
       return
    }

# parse the arguments - first is the number of lines and the second
# the line type. The defaults are 1 and gauss, respectively. The
# third argument can be either nofit in which case no fitting at all
# is performed, fit in which case the total model is fit. If no
# third argument is given then just the sigma and norm of each line
# are fit.

    set nlines 1
    if {[llength $args] > 0} {
       set nlines [lindex $args 0]
    }
    set linetype gauss
    if {[llength $args] > 1} {
       set linetype [lindex $args 1]
    }
    set fit 1
    set nofit 1
    if {[llength $args] > 2} {
	if {[lindex $args 2] == "fit"} {
           set fit 0
        }
	if {[lindex $args 2] == "nofit"} {
           set nofit 0
        }
    }
    
# get the current number of datagroups

    tclout datagrp
    set ngroups $xspec_tclout

# loop over the lines required

    for {set iline 0} {$iline<$nlines} {incr iline} {

# get the current number of model components

      tclout modcomp
      set ncomp $xspec_tclout

# get the current number of parameters

      tclout modpar
      set numparam $xspec_tclout

# work out where the next model component and model parameter must go

      set nextcomp [expr $ncomp/$ngroups + 1]
      set nextparam [expr $numparam/$ngroups + 1]

# and the number of parameters per datagroup

      set parpergroup [expr $numparam/$ngroups]

# set up an array of the parameter deltas

      for {set impar 1} {$impar<=$numparam} {incr impar} {

         tclout param $impar
         scan $xspec_tclout "%f %f" value delta($impar)

      }

# get the peak residual energy and strength

      tclout peak
      scan $xspec_tclout "%f %f" peake peakn

      puts " "
      puts [format "New peak at %f keV..." $peake]
      puts " "

# add the new component. first we have to set up the model parameters
# string.

      set pstring " "
      append pstring "& $peake  1.e-4 "
      append pstring "& 0.0     1.e-4 "
      append pstring "& $peakn  1.e-4 "

      for {set igroup 2} {$igroup<=$ngroups} {incr igroup} {
         append pstring "& = $nextparam "
         append pstring "& = [expr $nextparam + 1] "
         append pstring "& = [expr $nextparam + 2] "
      }

      puts [format "%s addcomp %d %s %s" $prompt $nextcomp $linetype $pstring]
      addcomp $nextcomp $linetype $pstring

# if the standard fit option is required (fit=1, nofit=1) then
# freeze all parameters except for the line sigma and norm

      if {$nofit == 1 && $fit == 1} {
         for {set igroup 1} {$igroup<=$ngroups} {incr igroup} {
            set istart [expr ($igroup-1)*($parpergroup+3) + 1] 
            set iend [expr $istart + $parpergroup]
	    puts [format "%s freeze %d-%d" $prompt $istart $iend]
            freeze $istart-$iend
         }
      }

# if the nofit option has not been set (nofit=0) then
# fit for the sigma and normalization

      if {$nofit == 1} {
#         puts "$prompt fit 100"
         fit 100
      }

# if the standard fit option is required (fit=1, nofit=1) then
# thaw all the parameters that were originally variable

      if {$nofit == 1 && $fit == 1} {
         for {set igroup 1} {$igroup<=$ngroups} {incr igroup} {
            for {set impar 1} {$impar<=$parpergroup} {incr impar} {

               set thispar [expr ($igroup-1)*($parpergroup+3) + $impar]
   	       if {$delta($thispar)>0.} {
                  puts [format "%s thaw %d" $prompt $thispar]
                  thaw $thispar
               }

   	    }
         }

# and thaw the energy of the new line
      
         puts [format "%s thaw %d" $prompt $nextparam]
         thaw $nextparam

      }

    }

   return

}
