      SUBROUTINE DISRO(Pixpro,Pixval,Np,Seed,Maptype,Npix)
      implicit none
C
C     Divide value up evenly for real case.  For integer,
C     use probability for low counts
C
C  I  pixpro  (r)  probability that a photon will go in pixel i
C  I  pixval  (r)  pixel value to be distributed
C  I  np      (i)  # of pixels where photons have to be distributed
C  I  seed    (i)  Seed value for GETRAN
C  I  maptype (s)  Data type (I=integer, R=real)
C  O  npix    (r)  # of photons in pixel i
C
      integer Np, Seed
      real*4 Pixpro(Np), Pixval, Npix(Np)
      character*(*) Maptype

      REAL*4 sum , rrr , GETRAN
      INTEGER*4 i , j , n, npmu
      LOGICAL negative
c
      IF ( Maptype(1:1).eq.'R' .or. abs(Pixval).GT.50 ) THEN
         sum = 0
         DO 50 i = 1 , Np - 1
            if ( Maptype(1:1).eq.'I' ) then
               Npix(i) = NINT(Pixval*Pixpro(i))
            else
               Npix(i) = Pixval*Pixpro(i)
            endif
            sum = sum + Npix(i)
 50      CONTINUE
         Npix(Np) = Pixval - sum
         RETURN
      ENDIF
c
      Npix(1) = 0.
      npmu = Np - 1
      DO 100 j = 1 , Np
         Npix(j) = 0.
 100  CONTINUE
c
c     Shortcut... no need to do any calcs if zero
      if ( Pixval.eq.0. ) return
      negative = .FALSE.
      if ( Pixval.lt.0. ) negative = .TRUE.

      sum = 0.
      DO 200 i = 1 , Np
         sum = sum + Pixpro(i)
 200  CONTINUE
      Pixpro(1) = Pixpro(1)/sum
      DO 300 i = 2 , Np
         Pixpro(i) = Pixpro(i-1) + Pixpro(i)/sum
 300  CONTINUE
      n = nint(abs(Pixval))
      DO 400 i = 1 , N
         rrr = GETRAN(Seed)
         j = 0
         DO WHILE ( .TRUE. )
            j = j + 1
            IF ( j.GT.Np ) WRITE (*,99001) j
            IF ( rrr.LT.Pixpro(j) ) THEN
               Npix(j) = Npix(j) + 1
               GOTO 400
            ENDIF
         ENDDO
 400  CONTINUE
      if ( negative ) then
         do i = 1, Np
            Npix(i) = -Npix(i)
         enddo
      endif
      RETURN
99001 FORMAT (' J > NP ,  J =',I3)
      END
