**==angle.spg  processed by SPAG 4.50J  at 13:07 on 31 Jul 1998
      SUBROUTINE ANGLE(Ra1,Dec1,Ra2,Dec2,Ang)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL*8 a , ab , b , bc , ca 
      REAL*4 Ang, Dec1 , Dec2 , Ra1 , Ra2
      character(255) string

C*** End of declarations inserted by SPAG
      ab = 90.0d0 - DBLE(Dec1)
      a = DBLE(Ra2) - DBLE(Ra1)
      ca = 90.0d0 - DBLE(Dec2)
      CALL CAAAB(ab,a,ca,bc,b)
      Ang = SNGL(bc)
      write(string,10) ab,a,ca,bc,b
 10   format("ANGLE ab,a,ca,bc,b ",5(F18.12,1X))
      CALL XWRITE(string,55)
      RETURN
      END
