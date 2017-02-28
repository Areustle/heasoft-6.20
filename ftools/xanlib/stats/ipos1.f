C***** function to return numbers that follow a poisson
C***** distribution with mean = sum
C *    this version makes use of qran instead of uran for the quasi
C *    random number generator
      integer*4 FUNCTION ipos1(sum) 
c poisson numb generator <880421.1100>
c
      integer*4 irrr, k, isu
      real*4 y, sum, esp, expm, am, ex
      COMMON /ip1   / irrr
      IF ( sum.GT.88. ) sum = 88.
99001 FORMAT (' SUM : ',1PE10.4)
      expm = exp(-sum)
      CALL qran(y,irrr)
      k = 0
      esp = expm
      ex = 1.
      am = 1.
      isu = 0
 100  CONTINUE
      IF ( y.GE.esp ) THEN
        k = k + 1
        am = am/k*sum
        ex = ex + am
        esp = ex*expm
        IF ( ex.GT.1.E38 ) THEN
          isu = isu + 1
          WRITE (*,99002) isu
99002     FORMAT ('+overflows in ipos1 ! ',I3)
          GO TO 200
        END IF
        GO TO 100
      END IF
 200  CONTINUE
      ipos1 = k
      RETURN
      END
