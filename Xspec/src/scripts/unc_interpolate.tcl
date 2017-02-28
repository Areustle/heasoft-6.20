proc unc_interpolate {delta_stat cur_delta hard_limit xtrial xlimit1 xlimit2 ylimit1 ylimit2 coef1 coef2 coef3 xval1 xval2 xval3 yval1 yval2 yval3 ier3pt old_del} {

# Routine called by calc_unc to calculate the trial offset for the parameter.
# The basic algorithm is to use the last three calculated values of delta stat.
# for a given offset to make a new estimate of the offset for the target delta
# stat. A quadratic fit is used unless has failed previously or has no real
# roots within the current range of calculated offsets.

# if there is no change between current fit statistic and last then
# parameter is insensitive to a change of this magnitude


if { $cur_delta == $old_del } {
   return [list 2 $xtrial $xlimit1 $xlimit2 $ylimit1 $ylimit2 $coef1 $coef2 $coef3 $xval1 $xval2 $xval3 $yval1 $yval2 $yval3 $ier3pt $old_del]
}

# if current delta statistic greater than previous min but less than
# target then reset min.

if { $cur_delta > $ylimit1 && $cur_delta < $delta_stat } {
   set ylimit1 $cur_delta
   set xlimit1 $xtrial
   if { $xlimit1 == $hard_limit } {
   return [list 1 $xtrial $xlimit1 $xlimit2 $ylimit1 $ylimit2 $coef1 $coef2 $coef3 $xval1 $xval2 $xval3 $yval1 $yval2 $yval3 $ier3pt $old_del]
   }
}

# if current delta statistic less than previous max but greater than
# target then reset max.

if { $cur_delta < $ylimit2 && $cur_delta > $delta_stat } {
   set ylimit2 $cur_delta
   set xlimit2 $xtrial
}

# replace in the 3pt array the point farthest away

if { $yval2 > 0 } {
   set dif 0.
   set idif 0
   for {set jdif 1} {$jdif <= 3} {incr jdif} {
      set cdif [expr |$yval$jdif-$delta_stat|]
      if {$cdif > $dif } { 
         set idif $jdif
         set dif $cdif
      }
   }
   set yval$idif $cur_delta
   set xval$idif $xtrial
   set ft3pt_out [ft3pt $xval $yval]
   set ier3pt [lindex ft3pt_out 0]

   if { $ier3pt != 0 } {
      puts "* 3 Point fit failed "
   } else {
      set coef1 [lindex ft3pt_out 1]
      set coef2 [lindex ft3pt_out 2]
      set coef3 [lindex ft3pt_out 3]
   }

} else {

# this is the first trial, so explicitly fit to a simple quadratic

   set xval2 $xtrial
   set yval2 $cur_delta
   set coef3 [expr $cur_delta/($xtrial*$xtrial)]
}

# save the current trial

set old_delta $cur_delta


# If a quadratic fit is to be used as the estimator then solve. Use positive
# root unless it lies outside the valid range in which case try negative root.
# If this is outside range too then punt and go for a linear estimator.

if { $ier3pt == 0 && $coef3 != 0 } {

   set radix [expr $coef2*$coef2 - 4*$coef3*($coef1-$delta_stat)]

   if {$radix >= 0} {

# Calculate positive root

      set radix [expr sqrt($radix)]
      set xtrial [expr ($radix-$coef2)/(2*$coef3)]
#      puts "3 pt interpolation guess : $xtrial"

# If the positive root wasn't in the range then use the negative

      if { $xtrial < $xlimit1 || $xtrial > $xlimit2 } {
         set xtrial [expr (-$radix-$coef2)/(2*$coef3)]
#         puts "Revised 3 pt guess : $xtrial"
      }

# If neither root is in the range then use a linear interpolation

      if { $xtrial >= $xlimit1 && $xtrial <= $xlimit2 } {

# If we get to here then xtrial is a valid quadratic estimate so return

         return [list 0 $xtrial $xlimit1 $xlimit2 $ylimit1 $ylimit2 $coef1 $coef2 $coef3 $xval1 $xval2 $xval3 $yval1 $yval2 $yval3 $ier3pt $old_del]

      }
   }
}

c if a quadratic estimator was not requested or could not be found then
c try linear interpolation. if the linear interpolation guess is less
c than 10% of the current interval then there is probably something going
c wrong so go for the conservative solution and use the midpoint

set xtrial [expr $xlimit1 + ($delta_stat-$ylimit1)*($xlimit2-$xlimit1)/($ylimit2-$ylimit1)]
if { [expr abs($xtrial-$xlimit1)] < [expr 10.*abs($xlimit2  -$xlimit1) ] } {
   set xtrial [expr ($xlimit2+$xlimit1)/2]
   puts "Mid-point trial : $xtrial"
} else {
   puts "Linear interpolation trial : $xtrial"
}

return [list 0 $xtrial $xlimit1 $xlimit2 $ylimit1 $ylimit2 $coef1 $coef2 $coef3 $xval1 $xval2 $xval3 $yval1 $yval2 $yval3 $ier3pt $old_del]

}
