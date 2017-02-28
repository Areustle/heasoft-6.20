C ***************************************************************************
C FUNCTION:
C      poiss
C
C DESCRIPTION:      
C      compute a random number based on a Poisson distribution.
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      Routine computes Guassian distributed noise with sig = sqrt(a)
C        if input mean is > 50
C
C USEAGE:
C      FUNCTION poiss(a,iseed)
C      
C ARGUMENTS:
C      a     - mean for the Poisson distribution
C      iseed - seed for random number generator
C      
C PRIMARY LOCAL VARIABLES:
C      n    - number of counts
c      p    - (a**n)/n
c      x    - uniformly distributed random value
c      y    - probability of getting n counts for a mean = a.
c      ex   - exp(-a)
c      sum  - cumulative probability of n or fewer counts
C      
C CALLED ROUTINES:
C      function gauss    - computes a Gaussian distributed random number 
C      function ft_ran2  - returns uniformly distributed random numbers
C      
C **************************************************************************

      function poiss(a,iseed)
	
	integer n, poiss, iseed	
	real a, ex, sum, p, x, y, ft_ran2, gauss

        if (a .lt. 0.) then
           poiss = a
           
	else if (a .le. 50) then
           ex = exp(-a)
5          n = 0
           sum = 0.
           p = 1.
           x = ft_ran2(iseed)
           
c      p = (value**n)/n !, and y = probability of getting n counts for a
c      mean = value
10         y = p * ex
           sum = sum + y
           if (x .lt. sum) go to 20
           n = n + 1
           p = p*a/n
           if (p .lt. 1.e-20) go to 5
           goto 10
c      
20         poiss = n
           
	else
           poiss = int(gauss(a,sqrt(a),iseed))
           
	endif
        
	return
	end
