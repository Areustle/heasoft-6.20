**==POST0R.spg  processed by SPAG 3.09I  at 09:46 on 20 Aug 1992
*- post0r - post real parameter values into a bin
      INTEGER FUNCTION POST0R(X,Nx,X1,X2,Status)
 
      INCLUDE 'status.codes'
* Import :
      REAL X                                  ! real value
      INTEGER Nx                              ! no of bins
      REAL X1(*) , X2(*)                      ! bin limits
* Status :
      INTEGER Status
* Local variables :
      INTEGER j
*-
      IF ( Status.NE.OK__ ) THEN
	POST0R = 0
	RETURN
      ENDIF
      j = 1
      DO WHILE ( (j.LE.Nx) .AND. .NOT.((X.GE.X1(j)) .AND. (X.LE.X2(j)))
     &           )
         j = j + 1
      ENDDO
 
      IF ( j.LE.Nx ) THEN
         POST0R = j
      ELSE
         Status = ERROR__
      ENDIF
 
      END
