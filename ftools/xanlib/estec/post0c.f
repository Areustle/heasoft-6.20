**==POST0C.spg  processed by SPAG 3.09I  at 09:46 on 20 Aug 1992
*- post0c - post character parameter values into a bin
      INTEGER FUNCTION POST0C(X,Nx,X1,X2,Status)
 
      INCLUDE 'status.codes'
* Import :
      CHARACTER*(*) X                         ! character value
      INTEGER Nx                              ! no of bins
      CHARACTER*(*) X1(*) , X2(*)             ! bin limits
* Status :
      INTEGER Status
* Local variables :
      INTEGER j
*-
      IF ( Status.NE.OK__ ) then
        POST0C = -1
	RETURN
      ENDIF
      j = 1
      DO WHILE ( (j.LE.Nx) .AND. .NOT.((X.GE.X1(j)) .AND. (X.LE.X2(j)))
     &           )
         j = j + 1
      ENDDO
 
      IF ( j.LE.Nx ) THEN
         POST0C = j
      ELSE
         Status = ERROR__
      ENDIF
 
      END
