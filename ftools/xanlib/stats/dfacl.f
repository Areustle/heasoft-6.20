C
      real*8 FUNCTION dfacl(i)   
c computes factorial (log version) <880421.1100>
c      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      integer*4 i, n
      real*8 par, an
      IF ( i.LT.0 ) THEN
        WRITE (*,99001) i
99001   FORMAT (' WARNING I IS NEGATIVE ! , I = ',I5)
        STOP
      ELSE IF ( i.EQ.0 ) THEN
        dfacl = 0.
        RETURN
      ELSE
        par = 0.
        DO n = 1, i
          an = n
          par = par + dlog(an)
        END DO
        dfacl = par
        RETURN
      END IF
      END
