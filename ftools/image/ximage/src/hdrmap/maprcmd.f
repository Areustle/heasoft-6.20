      SUBROUTINE maprcmd(mode)
      implicit none
c
c  Make recommendation based on current map setup
c
c  I  mode  (i)  1=restore displayed map to current map
c
      integer mode

      include '../include/io.inc'
      include '../include/maxvals.inc'
      include '../include/mapdef.inc'
c
c  Local variables
c
      integer LENACT
      character*(MAX_IDSTR) curmap, dismap


      if ( mode.eq.1 ) then

         if ( IDIsmap.gt.0 ) then
            dismap = MAPids(IDIsmap)
            curmap = MAPids(ICUrmap)
            write(ZWRite,'(4a)') ' Display map can overwrite '//
     &                           'current with: map copy ',
     &                           dismap(:LENACT(dismap)),' ',
     &                           curmap(:LENACT(curmap))
            call xwrite(ZWRite, 10)
         endif

      endif
      
      return
      end
