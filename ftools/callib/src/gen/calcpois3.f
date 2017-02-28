*+ CALCPOIS3
	function calcpois3(N)

	IMPLICIT NONE
	integer N
	real calcpois3

c Description
c This routine calculates and returns a 1 sigma error approximation for
c N counts assuming Poissonian statistics. 
c  - The algorithm used is the MEAN of the +ve & -ve errors given by
c    Gehrels 1986, Ap.J. 303, 336:
c		calcpois = 0.5 * (1.0 + SQRT(N+0.75)) + (SQRT(N - 0.25))
c
c Passed Parameters
c  N		i   : No. counts for which error is to be estimated
c  CALCPOIS3	  r : Estimated mean 1-sigma error on N
c
c Called routines
c  None
c 
c Author/Modification History
c  Ian M George  (1.0.0:95 Aug 01), original
c
*-
c internals
	real calcpois, calcpois2

	calcpois3 = 0.5*(calcpois(N) + calcpois2(N))

	return
	end
