***********************************************************************

C Given an array xx of length n, and given a value x, returns a value j
C such that x is between xx(j) and xx(j+1).  xx must be monotonic, either
C increasing or decreasing.  j=0 or j=n is returned to indicate that x is
C out of range.
C Copied from Numerical Recipes, Press et al.

C This is the double-precision version

*  @(#) locate8.f 1.2@(#)

	subroutine locate8(xx,n,x,j)

	double precision xx(n),x

	jl = 0
	ju = n+1
10	if (ju-jl.gt.1) then
	  jm = (ju+jl)/2
	  if ((xx(n).gt.xx(1)).eqv.(x.ge.xx(jm))) then
	    jl = jm
	  else
	    ju = jm
	  end if
	  go to 10
	end if
	j = jl

	return

	end

