      SUBROUTINE OFFBARY(Map,Szx,Szy,Xpos,Ypos,Optbox,Xdif,Ydif,Status)
      IMPLICIT NONE
c
c  Given box center and size returns barycentric shift
c
c  I  map    (r)  Image map
c  I  szx/y  (i)  Size of image map
c  I  x/ypos (r)  Box center
c  I  optbox (i)  Optimal box for highest S/N (image coordinates)
c  O  x/ydif (r)  Difference from start center
c  O  status (i)  Error flag (0=OK)
c
      integer*4 Szx, Szy, Optbox, Status
      real*4 Map(Szx,Szy)
      real*4 Xpos, Ypos, Xdif, Ydif

      include '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 tmpbox, kx1 , kx2, ky1 , ky2 , i , j
      REAL*4 oxpos, oypos, tmpbxh, xfrac, yfrac, xloc, yloc
      REAL*4 xparz , yparz , total , val
      REAL*4 xmin, xmax, ymin, ymax
      LOGICAL isrnull
      
      Status = 0
      oxpos = Xpos
      oypos = Ypos
      tmpbox = Optbox

      write(ZWRite,*) ' Bary box size, box center: ', tmpbox, oxpos,
     &                                                           oypos
      call xwrite(ZWRite, 25)
      tmpbxh = float(tmpbox)/2.
      xmin = oxpos - tmpbxh
      xmax = oxpos + tmpbxh
      ymin = oypos - tmpbxh
      ymax = oypos + tmpbxh
      kx1 = NINT(xmin)
      kx2 = NINT(xmax)
      ky1 = NINT(ymin)
      ky2 = NINT(ymax)
      xparz = 0.
      yparz = 0.
      total = 0.
      write(ZWRite,*) ' Bary box xmin/max, ymin/max ', kx1, kx2,
     &                                                 ky1, ky2
      call xwrite(ZWRite, 25)
c
c  This algorithm considers every pixel in the box, Map(i,j)
c
c   The sum of weighted distance is made by adding the value of the
c    considered pixel, Map(ii,jj), multiplied by its location
c    from the starting center (oxpos,oypos)
c
c   At the end, the sum of weighted distances is divided by the
c    sum of the Map values for all the pixels in the box, 
c    xparz/total and yparz/total.  Adding these to kx and ky,
c    respectively yields the location of the barycenter in 
c    image coordinates.
c
      DO 50 i = kx1 , kx2
         IF ( i.LE.0 .OR. i.GT.Szx ) THEN
c           Source near image edge
            Status = -1
            write(ZWRite,*) ' barycenter: box outside image (x)',
     &                      i,j
               call xwrite(ZWRite, 25)
            GOTO 50
         ENDIF
         DO 20 j = ky1 , ky2
            IF ( j.LE.0 .OR. j.GT.Szy ) THEN
c              Source near image edge
               Status = -1
               write(ZWRite,*) ' barycenter: box outside image (y)',
     &                         i,j
               call xwrite(ZWRite, 25)
               GOTO 20
            ENDIF
            if ( abs(oxpos - (float(i)-0.5)).gt.tmpbxh ) then
               xfrac = abs(xmin - (float(i)+0.5))
               xloc = (float(i)+0.5) - xfrac/2.
            elseif ( abs(oxpos - (float(i)+0.5)).gt.tmpbxh ) then
               xfrac = abs(xmax - (float(i)-0.5))
               xloc = (float(i)-0.5) + xfrac/2.
            else
               xfrac = 1.0
               xloc = float(i)
            endif
            if ( abs(oypos - (float(j)-0.5)).gt.tmpbxh ) then
               yfrac = abs(ymin - (float(j)+0.5))
               yloc = (float(j)+0.5) - yfrac/2.
            elseif ( abs(oypos - (float(j)+0.5)).gt.tmpbxh ) then
               yfrac = abs(ymax - (float(j)-0.5))
               yloc = (float(j)-0.5) + yfrac/2.
            else
               yfrac = 1.0
               yloc = float(j)
            endif

            if ( .not.isrnull(Map(i,j)) ) then
               val = Map(i,j)*xfrac*yfrac
               xparz = xparz + (xloc - xmin)*val
               yparz = yparz + (yloc - ymin)*val
               total = total + val
            endif

 20      CONTINUE
 50   CONTINUE
      IF ( total.LE.0.0 ) THEN
         Status = -1
         call xwrite(' barycenter: total <=0', 15)
      ELSE
         Xdif = (xmin + xparz/total) - Xpos
         Ydif = (ymin + yparz/total) - Ypos
      ENDIF

      RETURN
      END
