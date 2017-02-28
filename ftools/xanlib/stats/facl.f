C
      real*4 FUNCTION facl(i)   
c computes factorial (log version) <880421.1100>
      integer*4 i, n
      real*4 par, an
c
      IF ( i.LT.0 ) THEN
        WRITE (*,99001) i
99001   FORMAT (' WARNING I IS NEGATIVE ! , I = ',I5)
        STOP
      ELSE IF ( i.EQ.0 ) THEN
        facl = 0
        RETURN
      ELSE
        par = 0.
        DO n = 1, i
          an = n
          par = par + alog(an)
        END DO
        facl = par
        RETURN
      END IF
      END
