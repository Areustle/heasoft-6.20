
        SUBROUTINE NONLIN1 (TIME,PIIN,PIOUT)
C
C  NONLIN1 uses the results of Prieto, Hasinger, and Snowden to correct
C  the observed PI channel for the nonlinear gain decay with time.  There
C  are a couple shortcuts in this code.  First, the Prieto et al. result
C  shows how the gain decay affects the original channel while this code
C  has to reverse that.  Second, some the Prieto et al. result probably
C  include some of the affect of the gain hole in the center of the 
C  detector.  Both of these effects should be small.
C
C  INPUT:   SCC        S/C second of event
C           PIIN       PI channel
C  OUTPUT:  PIOUT      Scaled PI channel
C
        IMPLICIT NONE
C
        REAL*4 DELPI, NORM, PCEN, PIIN, PIOUT, PWIDTH, YEAR
        REAL*8 TIME
C
        DATA NORM, PCEN, PWIDTH /1.045, 68.2, 39.9/
C
        IF(PIIN .GT. 200.) THEN
            PIOUT = PIIN
        ELSE
            YEAR = TIME/31557600.
            DELPI = YEAR*NORM*EXP(-(PIIN - PCEN)**2/PWIDTH**2)
C
C  Scale the channel
C
            PIOUT = PIIN + DELPI
        ENDIF
C
        RETURN
        END

