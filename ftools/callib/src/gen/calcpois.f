*+ CALCPOIS
	function calcpois(N)

	IMPLICIT NONE
	integer N
	real calcpois

c Description
c This routine calculates and returns a 1 sigma error approximation for
c N counts assuming Poissonian statistics. 
c  - The algorithm used is that of Gehrels 1986, Ap.J. 303, 336.
c		calcpois = 1.0 + SQRT(N + 0.75)
c    The returned value is that of the (larger) +ve error, thus 
c    allowing for conservatism if the returned value is used to 
c    assign symetric +ve and -ve errors in the main routine.
c    For small N, the returned error is GREATER than that obtained by
c    SQRT(N), but the difference reduces as one moves to larger N
c    (and the Poissonian distribution becomes more like a Gaussian).
c  - Error propagtion is problematic in the low N regieme, 
c    In principle one can not propagate Poisson errors in the square 
c    root of sum of squares way.  It is true that the variances of the Poisson
c    distribution are combined in this normal way, but confindence limit error
c    bars are not simply related to the variance like they are for Gaussian
c    statistics.
c    HOWEVER, if you are not too fussy about small inaccuracies, I think you can
c    use the normal method even with Poisson error bars.  This works best, of 
c    course, if you have more counts, but even for n=5 or so, it is not too bad.
c    For example, 
c      for n=5 counts, the 95% confidence interval is 13.11 - 1.279 = 11.83.  
c      for n=10 counts, the 95% interval is           20.14 - 4.13  = 16.01.  
c      If one adds two datasets, each with have 5 counts in a given bin.  
c      Then, the total number of counts in the bin = 10
c	Propagating errors in the normal way gives sqrt(2) * 11.83 = 16.73 
c                                             compare to the exact = 16.01.
c (PROS uses estsigma when POISSERR=TRUE, & uses SQRT(N) when POISSERR=FALSE)
c
c Passed Parameters
c  N		i   : No. counts for which error is to be estimated
c  CALCPOIS	  r : Estimated (+ve) 1-sigma error on N
c
c Called routines
c  None
c 
c Author/Modification History
c  T.Jane Turner (1.0.0:94 Mar 11), original
c  Ian M George  (2.0.0:95 Aug 01), trapped -ve N
c
*-
	if(n.lt.0) then
	   calcpois = 0.0
	else
	   calcpois = 1.0 + SQRT(N + 0.75)
	endif

	return
	end
