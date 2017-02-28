      SUBROUTINE ROSAT_VIGN(A,kE,Vig,Ierr)
      IMPLICIT NONE
c      SUBROUTINE OFF_AX_A(A,E,VIG,IERR)
C
CC  Returns the normalized value of the vignetting function
C
C************************ FFORM VERSION 1.0 ************  9-SEP-88 08:50
C
CA  author : SLS               date: 9-SEP-1988 08:00
CU  update : CRI               date: 7-DEC-1988 15:13
CU  update : CRI        date: 18-DEC-1989 09:33
CU  update : CRI        date: 19-MAR-1990 09:30 message 902 removed
C
CT  status: not tested
C
C   general description
CG  This routine calculates the normalized vignetting function of the
CG  ROSAT/PSPC combination for a given off-axis angle and energy.
CG  The effect of the window support structure has not been included
CG  for this calculation.  The values come fom an analytic fit to
CG  the vignetting function.
C
C   call_var.          type I/O description
CP  IERR                I4    O = 0 no error
CP                              = 1 angle or energy greater than the
CP                                  specified range, calculated value
CP                                  returned limited to GE 0.
CP                              = 1 angle or energy less than 0., value
CP                                  returned of 1.
CP  A                   R4  I   angle in arc minutes (0. - 60.)
CP  kE                  R4  I   energy in keV (0. - 2.)
CP  VIG                 R4    O analytic vignetting function value
C
C   include_block name          description
C
C   variables   meaning
c
      INCLUDE '../include/io.inc'
      INTEGER*4 Ierr
      REAL*4 A , aa , kE, E , ee , f , g , Vig
C
C  Convert into expected units (keV)
CP  E                   R4  I   energy in eV (0. - 2000.)
      E = kE*1000.

      Ierr = 0
C
C  Check the lower bounds for the values of A and E
C
      IF ( (E.LT.0.) .OR. (A.LT.0.) ) THEN
         Ierr = 1
         WRITE (ZWRite,99001) E , A
         CALL XWRITE(ZWRite,10)
         Vig = 1.
      ELSE
         IF ( A.GT.60. ) Ierr = 1
         IF ( E.GT.2000. ) THEN
            Ierr = 1
            WRITE (ZWRite,99002) E
            CALL XWRITE(ZWRite,10)
         ENDIF
         aa = A/60.
         ee = E/2000.
C
C  Get the value for G(A)
C
         IF ( aa.LE.0.2333 ) THEN
            g = 1.
         ELSEIF ( aa.LE.0.8885 ) THEN
            g = 1.1065 - 0.4565*aa
         ELSE
            g = 5.9287 - 5.8837*aa
         ENDIF
C
C  Get the value for F(A,E)
C
         f = 1. + 0.4244*aa*ee - 0.3313*aa*aa*ee - 0.8248*aa*ee*ee
C
C  Set the value for the vignetting function
C
         Vig = g*f
         IF ( Vig.LT.0. ) Vig = 0.
      ENDIF
      RETURN
C
99001 FORMAT (' WARNING : ENERGY OR ANGLE < 0. !',2(1X,E12.5),
     &        ' VIGNETTING FUNCTION SET TO 1')
C see comment above
C 902   FORMAT(' WARNING : ANGLE VALUE > 60 ! ',E12.5)
99002 FORMAT (' WARNING : ENERGY VALUE > 2000 ! ',E12.5)
      END
