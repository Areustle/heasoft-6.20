*- xfill - fill storage with byte data
      subroutine xfill(nbytes,a,b)
* Author :
*  Andy Pollock
* History :
*  5 July 1990 : original
      implicit none
* Import :
      integer nbytes                 ! no of bytes to move
      character(1) a                         ! filling character
* Export :
      character(1) b(*)                      ! destination of bytes
*-
      call filbyt(nbytes,a,b)
      return
      end
