C
      real*4 FUNCTION fac(i)    
c returns factorial of i <880421.1100>
      integer*4 i, n
      real*4 par

      IF ( i.LT.0 ) THEN
        WRITE (*,99001) i
99001   FORMAT (' WARNING I IS NEGATIVE ! , I = ',I5)
	fac = 0.
        RETURN
      ELSE IF ( i.EQ.0 ) THEN
        fac = 1.
        RETURN
      ELSE IF ( i.GE.34 ) THEN
        WRITE (*,*) ' Warning, overflow in subroutine fac '
        fac = 1.E37
        RETURN
      ELSE
        par = 1.
        DO n = 1, i
          par = par*n
        END DO
        fac = par
        RETURN
      END IF
      END
