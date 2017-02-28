      SUBROUTINE gridx(tl,x,y,c)    
      IMPLICIT NONE
C-----------------------------------------------------------------------
C GREG	Internal routine
C	Support routine for the clipping algorithm ; called from DRAW
C	and GRPOLY only.  C is a 4 bit code indicating the relationship
C	between point (X,Y) and the window boundaries ; 0 implies the
C	point is within the window.
C Arguments
C       TL      R*4     Tick length (Defines box inside plot area) In
C	X,Y	R*4	Position of point		Input
C	C	Byte	Code				Output
C (11-Feb-1983)
C-----------------------------------------------------------------------
      INCLUDE 'greg.inc'
      REAL*4 x , y, tl
      INTEGER c
*
      c = 0
      IF ( x.LT.gx1+tl ) THEN
         c = 1
      ELSEIF ( x.GT.gx2-tl ) THEN
         c = 2
      ENDIF
      IF ( y.LT.gy1+tl ) THEN
         c = c + 4
      ELSEIF ( y.GT.gy2-tl ) THEN
         c = c + 8
      ENDIF
      END
