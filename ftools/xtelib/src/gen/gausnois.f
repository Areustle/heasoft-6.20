C ***************************************************************************
C SUBROUTINE:
C      gausnois
C
C DESCRIPTION:      
C      adds Gaussian noise to the sum of two input values, and computes resulting
C        uncertainty.
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:
C      call gausnois(sigma, comp, rate, rate2, error, error2, iseed)
C      
C ARGUMENTS:
C      sigma     - width of Gaussian distribution
C      comp      - noise-free intensity value to be added to RATE; in units of RATE
C      rate      - 2nd, possibly noisy, intensity value
c      rate2     - output intensity value
c      error     - uncertainty associated with RATE value
c      error2    - output error value
c      iseed     - seed for random number generator
C
C PRIMARY LOCAL VARIABLES:
C      
C CALLED ROUTINES:
C      function gauss - generate a Gaussian distributed random value
C
C *******************************************************************

	subroutine gausnois(sigma, comp, rate, rate2,
     &				error, error2, iseed)

	real sigma, comp, gauss
	real rate, rate2, error, error2
        integer iseed
        
	comp = gauss(comp, sigma, iseed) 
	rate2 = rate + comp
    	error2 = sqrt(error**2 + sigma**2)
 	

	return
	end
