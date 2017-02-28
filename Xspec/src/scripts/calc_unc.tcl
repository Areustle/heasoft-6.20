proc calc_unc {fit_param delta_stat deltaCrit direction tolerance ntries} {
# Calculates the uncertainty on a parameter by searching for the parameter
# value that give a statistic of best-fit+delta_stat. If direction is +1 then
# calculate the upper bound and if -1 then the lower. Essentially the same
# as the Fortran routine clcunc.f
# returns [list flag parameter_value delta_statistic] where flag has the 
# following options
#     0        OK
#     1        parameter is frozen
#     2        new minimum found
#     3        parameter reached hard limit before delta_stat was obtained
#     4        number of trials exceeded before delta_stat obtained

# Save the current chatter and reset to a lower value

tclout chatter
set save_chat [lindex $chatter 0]
chatter [expr $save_chat-6]


# Save the current fit parameter values since we will have to reset them
# at the end.

tclout modpar
set modpar $xspec_tclout
set number_params $modpar
for {set ifpar 1} {$ifpar <= $number_params} {incr ifpar} {
   tclout param $ifpar
   eval set paramn \$param$ifpar
   set save_param($ifpar) [lindex $paramn 0]
}


# Set the current best fit value and sigma for the chosen parameter.
# If sigma is zero then use parameter delta.

tclout param $fit_param
eval set paramn \$param$fit_param
set best_value [lindex $paramn 0]
set delta [lindex $paramn 1]
if { $delta < 0 } {
   chatter $save_chat
   return [list 1 $best_value 0.]
}
set sigma [lindex $paramn 6]
if { $sigma == 0 } {
   set sigma $delta
}

# Set the hard limit.

if { $direction == +1 } {
   set hard_value [lindex $paramn 5]
   set hard_limit [expr $hard_value - $best_value]
} else {
   set hard_value [lindex $paramn 2]
   set hard_limit [expr $best_value - $hard_value]
}

if { $hard_limit <= 0 } {
   chatter $save_chat
   return [list 3 $best_value 0.]
}

# Set the current fit statistic value and critical value

tclout stat
#set statistic $xspec_tclout
set save_stat $statistic
# Freeze the parameter of interest
freeze $fit_param
# Start the main loop.

set done 0
set trials 0
set bracket 0
set first 1
set qfintp 1

while {!$done} {

   incr trials

# Find the next parameter value to test. If we have not bracketed yet
# then step out else do an interpolation.

   if {!$bracket} {

# Set the initial trial guess.

      if { $first } {
         set xtrial $sigma
         if { $xtrial > $hard_limit } {
            set xtrial $hard_limit
         }
         set first 0

      } else {

         set xtrial [expr 2*$xtrial]
         if {$xtrial > $hard_limit } {
            set xtrial $hard_limit
         }

      }

#      puts "Extrapolated xtrial = $xtrial"

   } else {

# If the first iteration then set all the interpolation internal variables

      if { $qfintp } {
         set qfintp 0
         set coef(1) 0.
         set coef(2) 0.
         set coef(3) [expr 1./($sigma * $sigma)]
         set xval(1) 0.
         set xval(2) 0.
         set xval(3) 0.
         set yval(1) 0.
         set yval(2) -1.0e20
         set yval(3) -1.0e20
         set old_del -1.0e20
         set ier3pt 0
      }

      set interp_out [unc_interpolate $delta_stat $cur_delta $hard_limit $xtrial $xlimit(1) $xlimit(2) $ylimit(1) $ylimit(2) $coef(1) $coef(2) $coef(3) $xval(1) $xval(2) $xval(3) $yval(1) $yval(2) $yval(3) $ier3pt $old_del]
    
      set icerr [lindex $interp_out 0]
      set xtrial [lindex $interp_out 1]
      set xlimit1 [lindex $interp_out 2]
      set xlimit2 [lindex $interp_out 3]
      set ylimit1 [lindex $interp_out 4]
      set ylimit2 [lindex $interp_out 5]
      set coef1 [lindex $interp_out 6]
      set coef2 [lindex $interp_out 7]
      set coef3 [lindex $interp_out 8]
      set xval1 [lindex $interp_out 9]
      set xval2 [lindex $interp_out 10]
      set xval3 [lindex $interp_out 11]
      set yval1 [lindex $interp_out 12]
      set yval2 [lindex $interp_out 13]
      set yval3 [lindex $interp_out 14]
      set ier3pt [lindex $interp_out 15]
      set old_del [lindex $interp_out 16]

      if { $icerr == 1 } {
         set done 1
         puts "Parameter pegged at hard limit $hard_value"
         puts " with delta fit statistic = $cur_delta"
      } elseif { $icerr == 2 } {
         set xtrial [expr ($xlimit(1)+$xlimit(2))/2.]
      }

#      puts "Interpolated xtrial = $xtrial"

  }
  

# Set the new parameter and reset the other parameters to their start
# values.

   for {set ifpar 1} {$ifpar <= $number_params} {incr ifpar} {
      newpar $ifpar $save_param($ifpar)
   }
   newpar $fit_param [expr $best_value + $xtrial*$direction]

#   puts "New parameter value [expr $best_value + $xtrial*$direction]"

# Do a fit.

   if {[catch {fit 100} result]} {
#   If user breaks during a fit with ctrl-C, it should end up in here.
      chatter $save_chat
      thaw $fit_param
      return -code error
   }

# Check for a new minimum. If so exit.

   tclout stat
   set fit_stat $statistic

   if { [expr $save_stat - $fit_stat] > $deltaCrit } {
      puts "New minimum found"
      chatter $save_chat
      thaw $fit_param
      return [list 2 [expr $direction*$xtrial+$best_value] [expr $fit_stat - $save_stat]]
   }

   set cur_delta [expr $fit_stat - $save_stat]
   if { $cur_delta < 0 } {
      set cur_delta 0.
   }

# if we have not yet bracketed the target then see whether this attempt
# managed it. If it did then set the brackets and go onto the step of
# converging on the solution. Otherwise test whether we are at a hard limit
# in which case give up.

   if { !$bracket } {
       if { $fit_stat > [expr $save_stat + $delta_stat] } {
          set bracket 1
          set xlimit(1) 0.
          set xlimit(2) $xtrial
          set ylimit(1) 0.
          set ylimit(2) $cur_delta
#          puts "Successful bracket : $direction, $xtrial, $cur_delta"

       } else {
          if { $xtrial >= $hard_limit } {
             set done 1
             puts "Parameter pegged at hard limit $hard_limit"
          }
       }

# if the target has already been bracketed then we are in the convergence
# phase so check for convergence and too many iterations.

   } else {

      if {[expr abs($cur_delta-$delta_stat)] <= $tolerance } {
         set done 1
      } else {
#         puts "$trials $direction $xtrial $cur_delta"
#         puts "$xlimit(1) $ylimit(1) $xlimit(2) $ylimit(2)"

# If the calculated statistic for the trial value exceeds the upper limit,
# ylimit(2), then there is probably something wrong and the user should
# use steppar to investigate the situation. Return a value midway between
# the bracketing trials.

         if { $cur_delta > $ylimit(2) } {
            puts "Apparent non-monotonicity in statistic space detected"
            puts "Current bracket values [expr $direction*$xlimit(1)+$best_value], [expr $direction*$xlimit(2)+$best_value]"
            puts "and delta statistic $ylimit(1), $ylimit(2)"
            puts "but latest trial [expr $direction*$xtrail+$best_value] gives $cur_delta"
            puts "Suggest that you check this result using the steppar command"
            set xtrial [expr ($xlimit(1)+xlimit(2))/2.]
            set cur_delta [expr ($ylimit(1)+ylimit(2))/2.]
            set done 1
         }

# If the tolerance has not been achieved but the top and bottom of the xlimit
# range are equal then we cannot calculate the delta to the required tolerance
# so have to give up.

         if { [expr abs($xlimit(1)-$xlimit(2))/$xlimit(2)] < 1.0e-5 } {
            puts "Warning: identical values of the parameter give different"
            puts "values of the statistic. Please check your result"
            if { $direction == -1 } {
              puts "for the low end of the confidence range"
            } else {
              puts "for the high end of the confidence range"
            }
            set done 1
	 }

# if too many times round the loop give the user a chance to get out

         if { $trials >= $ntries } {
            puts "Number of trials exceeded before convergence"
            puts "Current trial values [expr $direction*$xlimit(1)+$best_value], [expr $direction*$xlimit(2)+$best_value]"
            puts "and delta statistic $ylimit(1), $ylimit(2)"
            chatter $save_chat
            thaw $fit_param
            return [list 4 [expr $direction*$xtrial+$best_value] $cur_delta]
         }
      }
   }   

# End loop over iterations

}

# the answer has converged so save the value and reset the values of the
# fit parameters to the values at invocation

for {set ifpar 1} {$ifpar <= $number_params} {incr ifpar} {
   newpar $ifpar $save_param($ifpar)
}
thaw $fit_param
if {[catch {fit 100} result]} {
#   If user breaks during a fit with ctrl-C, it should end up in here.
   chatter $save_chat
   return -code error
}

chatter $save_chat
return [list 0 [expr $direction*$xtrial+$best_value] $cur_delta]

}
