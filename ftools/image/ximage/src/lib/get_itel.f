      subroutine get_itel(mapid, itel)
      implicit none
c
c  Get telescope index for specified map
c
c  I  mapid  (c)  Header to retrieve from
c  O  itel   (i)  Telescope index
c
      character*(*) mapid
      integer*4 itel
c
c  Local variables
c
      character(80) telescop, instrume, detnam
      integer*4 detidx, status

      telescop = ' '
      instrume = ' '

      call gheads(mapid, 'TELESCOP', telescop, 0, status)
      call gheads(mapid, 'INSTRUME', instrume, 0, status)
      call gheads(mapid, 'DETNAM', detnam, 0, status)
      itel = detidx(telescop, instrume, detnam)

      return
      end
