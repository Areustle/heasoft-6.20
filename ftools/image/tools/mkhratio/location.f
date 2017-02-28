       integer function location(xx,n,x)
       INTEGER n
       REAL x, xx(n)

c **************************************************
c  A function version of the locate subroutine
c  from Numerical Recipes in FORTRAN (Section 3.4)
c
c  Given an array, its length, and a value,
c  returns the index j at which the value x
c  falls between xx(j) and xx(j+1)
c
c  Unlike the Recipes version, out of range values
c  return the closest j
c **************************************************

       INTEGER jl,jm,ju

       jl=0
       ju=n+1
   10  if (ju-jl .gt. 1) then
          jm=(ju+jl)/2
          if ((xx(n) .gt. xx(1)) .eqv. (x .gt. xx(jm))) then
             jl=jm
          else
             ju=jm
          endif
       goto 10
       endif

c Original routine, jl is the result
c Don't want any out-of-bound answers
c      location=min(jl,n-1)
       location=min(jl,n)
       return
       end
