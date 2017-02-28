C ***************************************************************************
C FUNCTION:
C      gauss
C
C DESCRIPTION:      
C    compute a random number based on a Gaussian distribution
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C       routine utilizes the central limit theorem to obtain a Gaussian
C        distributed random variable from the sum of twelve uniformly 
C        distributed random variables.
C
C USEAGE:
C	function gauss(a, sigma, iseed)
C      
C ARGUMENTS:
C      a     - mean of the Guassian distribution
C      sigma - width of Gaussian distribution
C      iseed - seed for random number generator
C
C PRIMARY LOCAL VARIABLES:
C      t - sum of 12 uniformly distributed random values
C      
C CALLED ROUTINES:
C      function ft_ran2 - returns uniformly distributed random numbers
C
C *******************************************************************

	function gauss(a, sigma, iseed)
	
	real a, sigma, t, gauss, ft_ran2
	integer i, iseed

	t = 0.
	do 40 i = 1,12
40	  t = t + ft_ran2(iseed)
	gauss = (t-6.)*sigma + a

	return
	end
