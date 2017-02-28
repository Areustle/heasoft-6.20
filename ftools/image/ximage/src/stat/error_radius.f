      FUNCTION ERROR_RADIUS(Xpix,Ypix,Snrat,Itel)
c   this function returns an estimate of the cma  90% error radius
c   in units of seconds of arc for any given position Xpix,Ypix
c   (units of 4 arcsecs linearized image pixels)
c   itel=telescope number  (Special calc for EXOSAT)
c
      INTEGER*4 Itel
      REAL*4 Xpix , Ypix , Snrat

      include '../include/startup.inc'
c
c  Local variables
c
      integer*4 sav_itel
      REAL*4 ERROR_RADIUS , sav_err, dist , a , b , c
      real*8 dd, xoff, yoff, xcen, ycen

      data sav_err/-1./ , sav_itel/-999/
      save sav_err, sav_itel, xoff, yoff

      xcen = 0.
      ycen = 0.

      if ( Itel.le.0 ) then
         ERROR_RADIUS = -1.
         RETURN
      endif

      if ( .not.(ZTElescop(Itel).eq.'EXOSAT' .and.
     &           ZINstrume(Itel)(1:3).eq.'CMA') ) then
         if ( sav_itel.ne.Itel ) then
            call gmdbd(Itel, 'ERRRAD', dd, 0, status)
            sav_err = dd
            sav_itel = Itel
         endif
         ERROR_RADIUS = sav_err
         RETURN
      endif
      if ( sav_itel.ne.Itel ) then
         call get_optax(itel, xcen, ycen, xoff, yoff)
         sav_itel = Itel
      endif
            
CMA1  dist = SQRT((Xpix-135.)**2.+(Ypix-60)**2.)/15.
CMA1         MDB:       ^^^ = 134.      ^^ = 61.

      dist = SQRT((Xpix-xoff)**2.+(Ypix-xoff)**2.)/15.

      IF ( dist.GT.55. ) THEN
         CALL XWRITE(' position is outside CMA image ',10)
         CALL XWRITE(' check xpix and ypix ',10)
         CALL XWRITE(' sub error_radius ',10)
         ERROR_RADIUS = -1.
         RETURN
      ENDIF
      IF ( dist.LT.15. ) THEN
         ERROR_RADIUS = 15.
      ELSE
         a = 1.556E-2
         b = 0.900
         c = -2.00
         ERROR_RADIUS = a*dist*dist + b*dist + c
      ENDIF
      IF ( ERROR_RADIUS.LT.30 .AND. Snrat.LT.5. ) ERROR_RADIUS = 30.
      RETURN
      END
