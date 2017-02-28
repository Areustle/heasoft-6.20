      SUBROUTINE draw(ux,uy)
      IMPLICIT NONE
C----------------------------------------------------------------------
C GREG	Internal routine
C	Draw a vector from current pen position to given
C	point. The current line type is used.
C Arguments :
C	UX,UY	R*8	User coordinates (DRAW)
C----------------------------------------------------------------------
      INCLUDE 'greg.inc'
      REAL*8 ux , uy
      REAL*4 xq , yq

      call rad2det(ux, uy, xq, yq, 1)
      call gdraw(xq,yq)

      return
      end
