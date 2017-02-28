**==POST1D.spg  processed by SPAG 3.09I  at 09:46 on 20 Aug 1992
*- post1d - post sorted list of real*8 parameter values into sorted bins
      SUBROUTINE POST1D(Nvals,X,Nx,X1,X2,Jv,Nv,Status)
 
      INCLUDE 'status.codes'
* Import :
      INTEGER Nvals                          ! no of values
      REAL*8 X(*)                            ! list of values
      INTEGER Nx                             ! no of bins
      REAL*8 X1(*) , X2(*)                   ! bin limits
* Export :
      INTEGER Jv(*)                          ! index of first value in b
      INTEGER Nv(*)                          ! no of values in bin
* Status :
      INTEGER Status
* Local variables :
      REAL*8 xx
      INTEGER i , j
*-
      IF ( Status.NE.OK__ ) RETURN
 
      DO 100 j = 1 , Nx
         Jv(j) = 0
         Nv(j) = 0
 100  CONTINUE
 
      IF ( (X(Nvals).LT.X1(1)) .OR. (X(1).GT.X2(Nx)) ) THEN
         RETURN
      ENDIF
 
      i = 1
      j = 1
      DO WHILE ( (i.LE.Nvals) .AND. (X(i).LE.X2(Nx)) .AND. (j.LE.Nx) )
         DO WHILE ( X(i).LT.X1(j) )
            i = i + 1
         ENDDO
         xx = X(i)
         DO WHILE ( (j.LT.Nx) .AND. (xx.GT.X2(j)) )
            j = j + 1
         ENDDO
         IF ( (xx.GE.X1(j)) .AND. (xx.LE.X2(j)) ) THEN
            Nv(j) = Nv(j) + 1
            IF ( Nv(j).EQ.1 ) Jv(j) = i
         ENDIF
         i = i + 1
      ENDDO
 
      END
