      SUBROUTINE EINSTEIN_VIGN(A,E,Vig,Ierr)
      IMPLICIT NONE
c
c get the Einstein vignetting
c
c Taken from a routine written by Xiaoyi Wu at Columbia University
c Author N. White 8/20/93
c
c in:
c A is off axis angle in arc min
c E is the energy (not used)
c out:
c vig - the fractional vignetting
c ierr - something went wrong
c
      INCLUDE '../include/io.inc'
      INTEGER*4 Ierr
      REAL*4 A , E , Vig
C
      Ierr = 0
C
C  Check the lower bounds for the values of A and E
C
      IF ( A.LT.0. ) THEN
         Ierr = 1
         WRITE (ZWRite,99001) A
         CALL XWRITE(ZWRite,10)
         Vig = 1.
         RETURN
      ENDIF
 
      IF ( A.GT.100. ) THEN
         CALL XWRITE(' Warning: vignetting radius > 100 min',10)
         Ierr = 1
      ENDIF
C
C  Get the value for vignetting
C
      IF ( A.LT.12.0 ) THEN
         Vig = 1.0 - A*(0.00825+A/3200.0)
      ELSEIF ( A.GE.12.0 .AND. A.LT.40.0 ) THEN
         Vig = 1.11232 - A*0.02136
      ELSE
         Vig = 0.25792
      ENDIF
      RETURN
C
99001 FORMAT (' WARNING : ANGLE < 0. !',1X,E12.5,
     &        ' VIGNETTING FUNCTION SET TO 1')
      END
