C ***************************************************************************
C SUBROUTINE:
C      poisnois
C
C DESCRIPTION:      
C      adds poisson noise to the sum of two input values.
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C      Assumes input RATE value can be put in terms of counts (i.e. binning,
C         bgnd subtraction, collimator correction et al can be removed.)
C      This routine can convert from count/s to counts. 
C
C USEAGE:
C      call poisnois(bin, comp, rate, runit, rate2, error, error2, iseed)
C      
C ARGUMENTS:
C      bin       - bin size of the data
C      comp      - noise-free intensity value to be added to rate; in units of runit
C      rate      - 2nd, possibly noisy, intensity value
c      runit     - units associated with the RATE value
c      rate2     - output intensity value
c      error     - uncertainty associated with RATE value
C      error2    - output error value
C      iseed     - seed for random number generator
C
C PRIMARY LOCAL VARIABLES:
C      sumcts    - simple sum of comp and rate
C      
C CALLED ROUTINES:
C      function poiss - generate a poisson distributed random value
C
C *******************************************************************

      subroutine poisnois(bin, comp, rate, runit, rate2, error, error2,
     &     iseed)

        double precision bin
	real comp, rate, rate2, error, error2
        real sumcts
	integer poiss, iseed
        character*(*) runit

        sumcts = (rate + comp)
        if (runit .eq. 'count/s') sumcts = sumcts * bin
        rate2 = poiss(sumcts,iseed)
	error2 = sqrt(rate2)
        
        if (runit .eq. 'count/s') then
           rate2 = rate2 / bin
           error2 = error2 / bin
        endif
        

	return
	end
