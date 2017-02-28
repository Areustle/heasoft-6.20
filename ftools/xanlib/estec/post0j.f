**==POST0J.spg  processed by SPAG 3.09I  at 09:46 on 20 Aug 1992
*- post0j - post integer*2 parameter values into a bin
      INTEGER FUNCTION POST0J(X,Nx,X1,X2,Status)
 
      INCLUDE 'status.codes'
* Import :
      INTEGER*2 X                             ! integer*2 value
      INTEGER Nx                              ! no of bins
      INTEGER*2 X1(*) , X2(*)                 ! bin limits
* Status :
      INTEGER Status
* Local variables :
      INTEGER j
*-
      IF ( Status.NE.OK__ ) THEN
	POST0J = 0
	RETURN
      ENDIF
      j = 1
      DO WHILE ( (j.LE.Nx) .AND. .NOT.((X.GE.X1(j)) .AND. (X.LE.X2(j)))
     &           )
         j = j + 1
      ENDDO
 
      IF ( j.LE.Nx ) THEN
         POST0J = j
      ELSE
         Status = ERROR__
      ENDIF
 
      END
