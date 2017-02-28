      subroutine get_optax(itel, xcen, ycen, xoff, yoff) 
      implicit none
c
c  Retrieve offset for specified mission
c
c  I  itel     (i)  Telescope index
c  I  x/ycen   (d)  Telescope center
c  O  x/yoff   (d)  Telescope optical axis
c
      integer itel
      real*8 xcen, ycen, xoff, yoff

      logical ISDNULL
      real*8 DNULL
c
c  Local variables
c
      integer status

      status = 0
c
c  For defined mission:
c     If DOPTIC not set, use DRPIX from mdb
c     If DRPIX from mdb not set, use center of loaded image
c
c  For unknown mission:
c     Use center of loaded image
c
      if ( itel.gt.0 ) then
         call gmdbd(itel, 'DOPTIC1', xoff, 0, status)
         if ( status.ne.0 ) xoff = DNULL()
         call gmdbd(itel, 'DOPTIC2', yoff, 0, status)
         if ( status.ne.0 ) yoff = DNULL()

         if ( ISDNULL(xoff) .or. ISDNULL(yoff) ) then
            call gmdbd(itel, 'DRPIX1', xoff, 0, status)
            if ( status.ne.0 ) xoff = DNULL()
            call gmdbd(itel, 'DRPIX2', yoff, 0, status)
            if ( status.ne.0 ) yoff = DNULL()
         endif
      else
         xoff = DNULL()
         yoff = DNULL()
      endif
         
      if ( ISDNULL(xoff) .or. ISDNULL(yoff) ) then
         xoff = xcen
         yoff = ycen
      endif

      return
      end
          
