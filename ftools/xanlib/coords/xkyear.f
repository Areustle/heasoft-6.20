*- xkyear - decimal year from shf key
      real*8 function xkyear(shf)
* Author :
*  Andy Pollock
* History :
*  3 October 1991 : original
* Import :
      integer*4 shf
* Local variables :
      integer*4 t(5)
      real*8 yd
*-
      call timka(shf,t)
      if(((t(1)/4)*4).eq.t(1))then
         yd=366d0
      else
         yd=365d0
      endif

      xkyear=dble(t(1))+(dble(t(2))
     &                   +(dble(t(3))
     &                     +(dble(t(4))
     &                       +dble(t(5))/60d0)/60d0)/24d0)/yd

      return

      end
