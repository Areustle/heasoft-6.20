*+GAIN_SAT

c    -------------------------------------------------------
      subroutine gain_sat(dx,dy,pha,pi,yprime,aprime1,
     &         aprime2,aprime3,mean_pha,
     &         errflg,chatter)	
c    -------------------------------------------------------
      IMPLICIT NONE
c --- DESCRIPTION ---------------------------------------------------
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------
c
c Jane Turner (Sept 1995)     1.0.0;
c Rehana Yusaf (Sept 20 1995) 1.0.1; 
c Rehana Yusaf (1995 Oct 23) 1.0.2; bug-fix dx, dy should be int NOT real
c
      character(5) version
      parameter (version = '1.0.2')
*-
c -------------------------------------------------------------------
*-

C INPUT file has  PHA, PI,A', dx,dy,Y'
C OUTPUT file has  PHA, PI, A', A'',A''',Y' 

        real aprime1, aprime2, yprime,  GAIN
	real aprime3
        integer  pha,pi,dx,dy
        real mean_pha
        REAL*4 BINEXP, BINOFF, BINSCA, TEMP

        integer chatter,errflg
 
CP  CAL                 R4  I  time corrected
C 			 mean value of Al-Kalpha Prescott fit
CP  APRIME1             R4  I   input pulse-height channel
CP  APRIME2             sat corrected bin value
CJT	APRIME3		sat & time corrected value
CP  GAIN                R4    O gain correction factor
C
C
	binexp=1.74
	binsca=0.0055

C INPUT file has PHA, PI,A'dx,dy,Y'

                BINOFF = 0.62777*mean_pha + 4.453
                GAIN = 4.543085 - 5.135878E-2*mean_pha + 
     &                 2.518324E-4*mean_pha * mean_pha
     &               - 4.562700E-7*mean_pha*mean_pha*mean_pha

            IF(aprime1.GT. BINOFF) THEN
C
C  Gain saturation Case
C  Correct for the gain saturation
C
                TEMP = aprime1 - BINOFF
                aprime2 = aprime1 + BINSCA*(TEMP**BINEXP)
            ELSE

C Linear case

                aprime2 = aprime1 
            ENDIF

C Do temporal gain correction

		aprime3 = GAIN*aprime2
          return
50        END
