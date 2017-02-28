      FUNCTION IPOSS(Sum,Seed)
c
c  poisson numb generator <880421.1100>
C
C***** function to return numbers that follow a poisson
C***** distribution with mean = sum
c
c  for vax version one has to pass down seed for random
c  number generator
c  8/2/90   gth
c
      INCLUDE '../include/io.inc'
      INTEGER*4 Seed , IPOSS
c
      INTEGER*4 isu , k
      REAL*4 Sum , esp , expm , ex , am , GETRAN , y
      DATA isu/0/
      SAVE isu
c
      IF ( Sum.LE.0. ) THEN
         IPOSS = 0
         RETURN
      ENDIF
      expm = EXP(-Sum)
      y = GETRAN(Seed)
      k = 0
      esp = expm
      ex = 1.
      am = 1.
      DO WHILE ( y.GE.esp )
         k = k + 1
         am = am/k*Sum
         ex = ex + am
         esp = ex*expm
         IF ( ex.GT.1.E38 ) THEN
            isu = isu + 1
            WRITE (ZWRite,99001) isu
            CALL XWRITE(ZWRite,10)
            GOTO 100
         ENDIF
      ENDDO
 100  IPOSS = k
      RETURN
99001 FORMAT (' Overflows in iposs ! ',I3)
      END
