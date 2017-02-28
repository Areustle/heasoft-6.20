**==caaab.spg  processed by SPAG 4.50J  at 12:32 on 15 May 1998
      SUBROUTINE CAAAB(Ab,A,Ca,Bc,B)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL*8 A , Ab , abr , ar , B , Bc , Ca , car , cosab , cosb , 
     &     cosbc , cosca , dtor , pi , r90 , rtod , sinab , sinb , 
     &     sinbc , sinca
      character(255) string
C*** End of declarations inserted by SPAG
      DATA pi/3.141592654d0/ , dtor/.1745329D-1/ , rtod/57.2957795d0/ , 
     &     r90/1.570796327d0/
      car = Ca*dtor
      ar = A*dtor
      abr = Ab*dtor
      cosca = DCOS(car)
      sinca = DSIN(car)
      cosab = DCOS(abr)
      sinab = DSIN(abr)
      cosbc = cosca*cosab + sinca*sinab*DCOS(ar)
      IF ( cosbc.GE.1.0d0 ) THEN
         cosbc = 1.0d0
         Bc = 0.0d0
C     SINBC=1.0E-5
         B = 0.0d0
         RETURN
      ELSEIF ( cosbc.LE.-1.0d0 ) THEN
         cosbc = -1.0d0
         Bc = 2.0d0*r90
C     SINBC=1.0E-5
         B = 0.0d0
         RETURN
      ENDIF
      sinbc = DSQRT(1.0d0-cosbc*cosbc)
c      Bc = r90 - ATAN(cosbc/sinbc)
      Bc = r90 - DATAN2(cosbc,sinbc)
      sinb = sinca*DSIN(ar)/sinbc
      cosb = (cosca-(cosab*cosbc))/(sinab*sinbc)
      B = pi - DATAN2(sinb,-cosb)
      B = B*rtod
      Bc = Bc*rtod
      write(string,10) cosbc, sinbc, DATAN2(cosbc,sinbc), sinb,cosb,
     &     DATAN2(sinb,-cosb)
 10   format("cosbc,sinbc,DATAN2(cosbc,sinbc),sinb,cosb,
     & DATAN2(sinb,-cosb)",6(F18.12,1X))
      CALL XWRITE(string,55)
      RETURN
      END
