      SUBROUTINE relocate(x,y)
      implicit none
C----------------------------------------------------------------------
C GREG	Internal routine
C	Position pen at user coordinate (X,Y)
C Arguments :
C	X,Y	R*8	User coordinate
C	AX,AY	R*4	Plot coordinate
C No subroutine referenced
C----------------------------------------------------------------------
      INCLUDE 'greg.inc'
      REAL*8 x, y
      REAL*4 xp, yp

      call rad2det(x, y, xp, yp, 1)
      call pgmove(xp,yp)

      END
