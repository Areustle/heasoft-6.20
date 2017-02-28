*- xkjd - JD from shf key
      real*8 function xkjd(shf)
* Author :
*  Andy Pollock
* History :
*  3 October 1991 : original
* Import :
      integer*4 shf
* Local constant :
*  jd0 - JD at shf's origin
      real*8 jd0
      parameter (jd0=2444238.5d0)
*-
      xkjd=jd0+dble(shf)/86400d0

      return

      end
