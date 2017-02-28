proc ft3pt {xval yval} {

# Routine to put a quadratic exactly through 3 points.

if { $xval3 == $xval2 } {
   return [list 1 0. 0. 0.]
}

set delta [expr 1/($xval3-$xval2)]

set x1 $xval1
set x2 $xval2
set y1 $yval1
set y2 $yval2
set ft2pt_out [ft2pt $x1 $x2 $y1 $y2]
set ierr [lindex ft2pt_out 0]
if {$ierr != 0} {
   return [list $ierr 0. 0. 0.]
}
set coef11 [lindex ft2pt_out 1]
set coef12 [lindex ft2pt_out 2]

set x2 $xval3
set y2 $yval3
set ft2pt_out [ft2pt $x1 $x2 $y1 $y2]
set ierr [lindex ft2pt_out 0]
if {$ierr != 0} {
   return [list $ierr 0. 0. 0.]
}
set coef21 [lindex ft2pt_out 1]
set coef22 [lindex ft2pt_out 2]

set coef1 [expr $delta*($coef11*$xval3-$coef21*$xval2)]
set coef2 [expr $delta*($coef12*$xval3-$coef22*$xval2-$coef11+$coef21)]
set coef3 [expr $delta*($coef22-$coef12)]


return [list 0 $coef1 $coef2 $coef3]

}
