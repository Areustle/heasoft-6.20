*- xmove - move bytes from A to B
      subroutine xmove(nbytes,a,b)
* Author :
*  Andy Pollock
* History :
*  5 July 1990 : original
      implicit none
* Import :
      integer nbytes                 
c! no of bytes to move
      character(1) a(*)                      
c! source of bytes
* Export :
      character(1) b(*)                      
c! destination of bytes
*-
      call movbyt(nbytes,a,b)
      return
      end
