proc ft2pt {x1 x2 y1 y2} {

# Routine to put a quadratic exactly through 3 points.

if {$x1 == $x2} {
   return [list 1 0. 0.]
}

set delta [expr 1/($x2-$x1)]
set c1 [expr $delta*($y1*$x2-$y2*$x1)]
set c2 [expr $delta*($y2-$y1)]

return [list 0 $c1 $c2]

}
