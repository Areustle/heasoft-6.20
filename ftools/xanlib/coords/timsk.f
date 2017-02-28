*- timsk.for - convert string "yyyy ddd hh:mm:ss" to shf key
      subroutine timsk(string,shf)
* Author :
*  Andy Pollock
* History :
*  6 October 1992 : original

* Import :
      character*(*) string
* Export :
      integer*4 shf
* Local variable :
      integer*4 it(5)
*-
      call timsa(string,it)
      call timak(it,shf)

      return

      end
