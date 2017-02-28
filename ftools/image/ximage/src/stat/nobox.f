      SUBROUTINE NOBOX(Sfa,Opixback,Scts,Opixbox)
      IMPLICIT NONE
c
C subroutine to calculate optimum box size for maximizing signal to
C noise ratio
C
C  I  sfa       (r) ?area
C  I  opixback  (r) Background in cnts/original pixel (This is
C                   the bg calculated before boxes are checked
C                   for bad statistics)
C  I  scts      (r) Source counts
C  O  opixbox   (i) Size of optimum cell for count rate estimation
C                   units are in original pixels
C   Input from common:
C            FRAc  vector containing source point spread function
C
      real*4 Sfa, Opixback, Scts
      integer*4 Opixbox

      INCLUDE 'ximpsf.inc'
c
c  Local variables
c
      REAL*4 zoom_psf , radius98
      REAL*4 src_counts , ak , ssq , sigmin , sig
      INTEGER*4 kmin , k , krad
      kmin = -1
c
      src_counts = MAX(0., Scts)
      sigmin = 1.E37
      sig = sigmin
      zoom_psf = SQRT(Sfa)
      radius98 = MAXPSF*zoom_psf
      DO 100 k = 0 , MAXPSF
         ak = zoom_psf*k
         ssq = (2.*ak+1.)*(2.*ak+1.)
         sigmin = sig
         IF ( FRAc(k).EQ.0 ) THEN
            if ( k.eq.0 ) then
               call XWRITE (' divide by zero [Frac(0)] in nobox', 5)
            else
               call XWRITE (' divide by zero in nobox', 25)
            endif
            sig = 1.0
         ELSE
            sig = SQRT(src_counts*FRAc(k)+Opixback*ssq)/FRAc(k)
         ENDIF
         IF ( sig.LT.sigmin ) THEN
            sigmin = sig
            kmin = k*zoom_psf
         ENDIF
         IF ( FRAc(k).GT.0.98 ) THEN
c
c  radius98 unused.  Should it still jump out of loop if > 0.98?
c
            radius98 = k*zoom_psf
            GOTO 200
         ENDIF
 100  CONTINUE
 200  IF ( kmin.EQ.-1 ) kmin = (MAXPSF-1)*zoom_psf
      krad = kmin
      Opixbox = krad*2 + 1
      RETURN
      END
