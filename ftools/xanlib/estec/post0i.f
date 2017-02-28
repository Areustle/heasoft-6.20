**==POST0I.spg  processed by SPAG 3.09I  at 09:46 on 20 Aug 1992
*- post0i - post integer parameter values into a bin
      INTEGER FUNCTION POST0I(X,Nx,X1,X2,Status)
 
      INCLUDE 'status.codes'
* Import :
      INTEGER X                               ! integer value
      INTEGER Nx                              ! no of bins
      INTEGER X1(*) , X2(*)                   ! bin limits
* Status :
      INTEGER Status
* Local variables :
      INTEGER j
*-
      IF ( Status.NE.OK__ ) THEN
	POST0I = 0
	RETURN
      ENDIF
      j = 1
      DO WHILE ( (j.LE.Nx) .AND. .NOT.((X.GE.X1(j)) .AND. (X.LE.X2(j)))
     &           )
         j = j + 1
      ENDDO
 
      IF ( j.LE.Nx ) THEN
         POST0I = j
      ELSE
         Status = ERROR__
      ENDIF
 
      END
