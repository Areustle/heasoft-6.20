      SUBROUTINE inxypix(Cursor,Xpix,Ypix)
      IMPLICIT NONE
c
c Input coordinate in original pixels from cursor or command line
c
c  I  cursor   (l)  if cursor required
c  O  xpix     (r)  value in original pixel coordinates
c  O  ypix     (r)  ypixel value in image array coordinates
c
      logical Cursor
      real*4 Xpix, Ypix

      INCLUDE '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 idelim, iflag , nret , n, status
      REAL*4 rrd(2)
      character(80) descr(2)
      LOGICAL isdisplay, ismouse
c
      IF ( isdisplay() .AND. ismouse() .AND. Cursor ) THEN
         CALL XWRITE(' Cursor is active ',10)
c
c  read the cursor position
c
         status = 0
         call tclreslr('select noerr', rrd, n, 2, status)
         xpix = rrd(1)
         ypix = rrd(2)
c
c print the x and y pixel
         CALL XWRITE('              Pixel coordinates',10)
         CALL XWRITE(' ',10)
         WRITE (ZWRite,99001) xpix , ypix
         CALL XWRITE(ZWRite,10)
      ELSE
c
c from comand line 
         rrd(1) = Xpix
         rrd(2) = Ypix
         idelim = 0
         iflag = 0
         descr(1) = ' X Pixel coordinate  '
         descr(2) = ' Y Pixel coordinate '
         n = 2
         ZSTring = ' '
         ZPArse = 0
         CALL XPXGTR(ZSTring,ZPArse,n,descr,n,rrd,nret,iflag,idelim)
         Xpix = rrd(1)
         Ypix = rrd(2)
      ENDIF
      RETURN
99001 FORMAT (10x,'X=',F9.2,4X,'Y=',F9.2)
      END
