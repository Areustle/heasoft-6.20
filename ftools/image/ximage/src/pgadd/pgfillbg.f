      subroutine pgfillbg (Min_level)
      implicit none
      integer Min_level

c     Sets all color indices up to Min_level to the "background" level,
c     assumed to be color index zero
 
      integer i, minci, maxci
      real bgR, bgG, bgB
      
      call PGQCIR(minci, maxci)
      call PGQCR (minci, bgR, bgG, bgB)
      do i = minci, minci+Min_level
          call PGSCR (i, bgR, bgG, bgB)
      enddo
      call PGUPDT
      
      return
      end
