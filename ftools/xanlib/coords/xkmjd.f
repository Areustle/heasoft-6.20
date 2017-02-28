*- xkmjd - MJD from shf key
      real*8 function xkmjd(shf)
* Author :
*  Andy Pollock
* History :
*  3 October 1991 : original
* Import :
      integer*4 shf
* Local constant :
*  mjd0 - MJD at shf's origin
      real*8 mjd0
      parameter (mjd0=44238d0)
*-
      xkmjd=mjd0+dble(shf)/86400d0

      return

      end
