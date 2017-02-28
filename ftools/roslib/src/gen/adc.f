*+ADC
c    ------------------------------------------------------------
       subroutine adc(dx,dy,pha,pi_cor,yprime,gy,
     &         aprime1,bin_low,
     &         bin_high,errflg,chatter)	
c    ------------------------------------------------------------
      IMPLICIT NONE
c --- DESCRIPTION -----------------------------------------------------
c
C Comments from original sass software
CG  This routine returns a corrected real value for the measured
CG  pulse-height bin.  This correction routine exists since the analog
CG  to digital conversion for PSPC event pulse heights is not completely
CG  linear.  The ADC bins do not have equal widths though on average
CG  they are linear.   
CG  A random number generator is used to find a real value
CG  with equal probability in the range covered by the pulse-height
CG  channel.  For instance, channel 10 covers the range 9.02 - 10.12.
CG  adc.for therefore returns a real value between 9.02 and 10.12.  
C	input file has PHA(int), PI(int), dx(r), dy(r), x'(r),GY, GA
C     
C OUTPUT file has  PHA, PI,A',dx,dy,Y'
c
c --- AUTHORS/MODIFICATION HISTORY ------------------------------------
c
c Jane Turner (Sept 1995) 1.0.0;
c Rehana Yusaf (Sept 19 1995) 1.0.1; minor cosmetic changes,
c                                    the input is no longer read from a
c                                    file but is passed as arguments to
c                                    this routine.
c Rehana Yusaf (Oct 23 1995) 1.0.2; replace RAN (non-standard fortran)
c                                   with ft_ran2
c
      character(5) version
      parameter (version = '1.0.2')
c ---------------------------------------------------------------------
*-
c
c --- VARIABLES ---
c
      character(30) errstr
      character(80) desc
      integer len,clenact

      real gy,yprime,aprime1,bin_width
      real bin_low(*),bin_high(*),ft_ran2
      integer chatter,errflg, iseed, pi_cor,pha,dx,dy
c
c --- USER INFO ---
c
      errstr = ' ERROR: ADC Ver '//version
      len = clenact(errstr)
      IF (chatter.GE.40) THEN
        desc = ' ... using adc Ver '//version
        call fcecho(desc)
      ENDIF

      iseed = 127773345

C  ADC nonlinearity correction

      bin_width = bin_high(pha) - bin_low(pha) 
      aprime1 = bin_low(pha)+ft_ran2(iseed)*bin_width

C		APRIME1=BIN_LOW(PHA)

      return
50    END
