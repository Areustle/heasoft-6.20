      SUBROUTINE RAW_COUNTS(Map,Szx,Szy,Xpos,Ypos,Srccnts,Srcpix,
     &                      Optbox,Optbxh,Good)
      IMPLICIT NONE
c
c find the raw number of counts in the box
c
c  I  map     (r) image map
c  I  szx/y   (i) size of image map
c  I  x/ypos  (r) location of current box in image pixels
c  O  srccnts (r) number of counts in box
c  O  srcpix  (r) number of image pixels in box
c  I  optbox  (i) full size of box in image pixels
c  I  optbxh  (r) half size of box in image pixels
c I/O good    (l) Whether current box is good
c
      integer*4 Szx, Szy, Optbox
      real*4 Map(Szx,Szy)
      real*4 Xpos, Ypos, Optbxh, Srccnts, Srcpix
      logical Good
c
c  Local variables
c
      INTEGER*4 ixmin, iymin, jjx, jjy 
      logical isrnull

      ixmin = NINT(Xpos - Optbxh)
      iymin = NINT(Ypos - Optbxh)
      Srccnts = 0
      Srcpix = 0
      DO 100 jjx = ixmin , ixmin + Optbox
         IF ( jjx.LE.0 .OR. jjx.GT.Szx ) THEN
c           Source too close to image edge
            Good = .FALSE.
            GOTO 100
         ELSE
            DO 20 jjy = iymin , iymin + Optbox
               IF ( jjy.LE.0 .OR. jjy.GT.Szy ) THEN
c                 Source too close to image edge
                  Good = .FALSE.
                  GOTO 20
               ENDIF
               if ( .not.isrnull(MAP(jjx,jjy)) ) then
                  Srccnts = Srccnts + NINT(MAP(jjx,jjy))
                  Srcpix = Srcpix + 1
               endif
 20         CONTINUE
         ENDIF
 100  CONTINUE
      RETURN
      END
