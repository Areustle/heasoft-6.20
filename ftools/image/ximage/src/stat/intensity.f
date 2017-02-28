      SUBROUTINE INTENSITY(Srccnts,Srcpix,Baccnts,Bacpix,Opixbox,
     &                     Unknown,Stonr)
      IMPLICIT NONE
c
c fixed it so that it cannot take the sqrt of a -ve number  LA 9/15/92
c
c  I  srccnts  (r)  Counts in source box
c  I  srcpix   (r)  Image pixels in source box
c  I  baccnts  (r)  Counts in local background box
c  I  bacpix   (r)  Image pixels in local background box
c  I  opixbox  (i)  Size of source box in original pixels
c  I  unknown  (i)  TRUE if using unknown psf
c  O  stonr    (r)  Signal to noise ratio
c
c common changes
c sint - corrected for psf and bgnd
c
      real*4 Srccnts,Srcpix,Baccnts,Bacpix, Stonr
      integer*4 Opixbox
      logical Unknown

      include '../include/io.inc'
      include 'backgd.inc'
      include 'detect.inc'
      include 'ximpsf.inc'
c
c  Local variables
c
      INTEGER*4 iisi , iress, ipo
      REAL*4 var, locback, bbba, XPOLOG

      iisi = (FLOAT(Opixbox)-1.)/2.
      IF ( iisi.GT.MAXPSF ) iisi = MAXPSF
c
c scale background to source size box
c   locback - Background counts per image pixel
c
      IF ( Bacpix.GT.0.0 ) THEN
         locback = Baccnts/Bacpix
      ELSE
         call XWRITE (' Local background is zero in intensity', 10)
         locback = 0.0
      ENDIF
      bbba = locback*Srcpix
 
      iress = Srccnts
      BASo(NUMdet) = Locback
c added divide by zero trap.... Nick 6/9/93
      IF ( Unknown ) THEN
         PSFco(NUMdet) = 1.0
      ELSE IF ( FRAc(Iisi).GT.0.0 ) THEN
         PSFco(NUMdet) = 1./FRAc(MIN(Iisi,MAXPSF))
      ELSE
         CALL XWRITE ('divide by zero in intensity', 25)
         PSFco(NUMdet) = 1.0
      ENDIF
      write(ZWRite,*) NUMdet, ' - local bg (cnts/img pix) ', 
     &                         locback, ' Imgpix in box ', Srcpix
      call XWRITE(ZWRite, 20)
      write(ZWRite,*) NUMdet, ' - PSFco ', PSFco(NUMdet), 
     &                 ' Back ', bbba, ' Tot Cnts ', iress
      call XWRITE(ZWRite, 20)
c
c correct for psf and subtract the background
c
      IF ( IZEro.NE.1 ) THEN
         SINt(NUMdet) = (Srccnts-Locback*Srcpix)*PSFco(NUMdet)
      ELSE
         SINt(NUMdet) = (Srccnts-BNEw*Srcpix)*PSFco(NUMdet)
      ENDIF
c
c get the error on the source count rate (but what about the bgnd?)
c
      var = Srccnts
      IF ( var.GT.0.0 ) THEN
         ERR(NUMdet) = SQRT(var)*PSFco(NUMdet)
      ELSE
         ERR(NUMdet) = 0.0
      ENDIF
c
c signal to noise ratio
c
      IF ( ERR(NUMdet).NE.0 ) THEN
         Stonr = SINt(NUMdet)/ERR(NUMdet)
      ELSE
         Stonr = 1.0
         call XWRITE (' warning: zero source counts in INTENSITY', 10)
      ENDIF
c
c probablity its a bgnd fluct
c
      IF ( Stonr.LT.5. .AND. bbba.GT.0.0 ) THEN
         ipo = 1
         PROb(NUMdet) = XPOLOG(iress,bbba,ipo)
c        print*,'xpolog',PROb(NUMdet),iress,bbba,ipo,stonr
      ELSE
         PROb(NUMdet) = 0.
c        print*,'xpolog',PROb(NUMdet),stonr,bbba
      ENDIF

      RETURN
      END
