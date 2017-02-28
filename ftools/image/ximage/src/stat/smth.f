      SUBROUTINE SMTH(Map,Mapbuf,Szx,Szy,S,D,Bufsz,Sigpix,
     &                Scaling_factor,Anrm,Smooth_x,Smooth_y,
     &                Wavesmth)
      IMPLICIT NONE
c
c  smth performs a two-dimensional smoothing of data
c  vax/vms version  13/2/90  gth
c
c  I  map            (r)  Image map
c  I  mapbuf         (r)  Image map buffer
c  I  szx/y          (i)  Size of maps
c  -  s,d            (r)  Buffers for smooth calculation
c  I  bufsz          (i)  Size of buffers
c  I  sigpix         (r)  Sigma of the gaussian in image pixels
c  I  scaling_factor (r)  Scale factor used to increase precision
c  O  anrm           (r)  Normalisation factor (i.e. the area under the
c                         convolution kernel)
c  I  smooth_x       (l)  Whether to smooth x direction
c  I  smooth_y       (l)  Whether to smooth y direction
c  I  wavesmth       (l)  Whether to use wavelet smoothing
c
      integer*4 Szx, Szy, Bufsz
      real*4 Map(Szx,Szy), Mapbuf(Szx,Szy), S(Bufsz), D(Bufsz)
      REAL*4 Sigpix, Scaling_factor, Anrm
      logical Smooth_x , Smooth_y , Wavesmth
c
c  Local variable
c
      INTEGER*4 i , j , m , inum , k , kk
      REAL*4 r(51) , FUNCT
      REAL*4 wavelet
      LOGICAL isrnull
c
      Anrm = 1.
C
C  initialize convolution kernel
C
      inum = Sigpix*5. + 0.5
      m = MAX0(1,inum)
      m = MIN0(m,25)
c
      IF( wavesmth ) THEN
         DO i = -m , m
            r(i+m+1) = wavelet(i,Sigpix)
         ENDDO
      ELSE
        DO i = -m , m
            r(i+m+1) = FUNCT(i,Sigpix)
        ENDDO
      ENDIF
c
      Anrm = 0.
      DO i = -m , m
         DO j = -m , m
            Anrm = Anrm + r(i+m+1)*r(j+m+1)
         ENDDO
      ENDDO
C
C   first go:
c   =========
C   store columns of map in s, do the convolution and restore
c   s in columns of mapout
c
      DO 400 k = 1 , Szy
         DO 250 kk = 1 , Szx
            if ( isrnull(Map(kk,k)) ) then
               S(kk) = 0.
            else
               S(kk) = Map(kk,k)*Scaling_factor
            endif
 250     CONTINUE
         IF ( Smooth_x ) THEN
            CALL CNVL(S,D,Szx,r,m)
         ELSE
            DO kk = 1 , Szx
               d(kk) = S(kk)
            ENDDO
         ENDIF
         DO kk = 1 , Szx
            Mapbuf(kk,k) = d(kk)
         ENDDO
 400  CONTINUE
C
C   second go:
c   ==========
c   store rows of mapout in s, do the convolution and restore
c   s in rows of map
C
      DO 600 k = 1 , Szx
         DO kk = 1 , Szy
            S(kk) = Mapbuf(k,kk)
         ENDDO
         IF ( Smooth_y ) THEN
            CALL CNVL(S,D,Szy,r,m)
         ELSE
            DO kk = 1 , Szy
               d(kk) = S(kk)
            ENDDO
         ENDIF
         DO 500 kk = 1 , Szy
            Map(k,kk) = D(kk)/Anrm
 500     CONTINUE
 600  CONTINUE
      RETURN
      END
