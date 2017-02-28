      SUBROUTINE gdraw(ax,ay)
      IMPLICIT NONE
C----------------------------------------------------------------------
C GREG	Internal routine
C	Draw a vector from current pen position to given
C	point. The current line type is used.
C Arguments :
C	AX,AY	R*4	Plot coordinates
C----------------------------------------------------------------------
      INCLUDE 'greg.inc'

      REAL*4 ax , ay
      INTEGER c0 , c1 , c
      REAL tlx, tly
      REAL x , y , x0 , y0 , x1 , y1, pgx(2), pgy(2)
*
* End-points of line are (X0,Y0), (X1,Y1)
*
      call pgqpos(x0,y0)
      x1 = ax
      y1 = ay

* Change the end-points of the line (X0,Y0) - (X1,Y1)
* to clip the line at the window boundary.
* The algorithm is that of Cohen and Sutherland (ref: Newman & Sproull)
*
      call pgqcs(4, tlx, tly)   ! Length of ticks is char height
      CALL gridx(0.,x0,y0,c0)
      if ( c0.eq.0 ) then
         if ( x0.eq.gx2 ) then
            c0 = 2
         elseif ( y0.eq.gy2 ) then
            c0 = c0 + 8
         endif
      endif
      CALL gridx(0.,x1,y1,c1)
      if ( c1.eq.0 ) then
         if ( x1.eq.gx2 ) then
            c1 = 2
         elseif ( y1.eq.gy2 ) then
            c1 = c1 + 8
         endif
      endif
      IF ( iand(c0,c1).NE.0 ) then
         call pgmove(ax,ay)
         RETURN
      ENDIF

      c = c0
      IF ( c.EQ.0 ) c = c1
      DO 150 WHILE (c.NE.0)
         IF ( iand(c,2).NE.0 ) THEN               ! crosses GX2
            y = y0 + (y1-y0)*(gx2-x0)/(x1-x0)
            x = gx2
            if ( ticks ) then
               call pgmove(x,y)
               call pgdraw(x-tlx,y)
            endif
            CALL gridlab(x,y)
         ELSEIF ( iand(c,8).NE.0 ) THEN           ! crosses GY2
            x = x0 + (x1-x0)*(gy2-y0)/(y1-y0)
            y = gy2
            if ( ticks ) then
               call pgmove(x,y)
               call pgdraw(x,y-tly)
            endif
            CALL gridlab(x,y)
         ELSEIF ( iand(c,1).NE.0 ) THEN           ! crosses GX1
            y = y0 + (y1-y0)*(gx1-x0)/(x1-x0)
            x = gx1
            if ( ticks ) then
               call pgmove(x,y)
               call pgdraw(x+tlx,y)
            endif
         ELSEIF ( iand(c,4).NE.0 ) THEN           ! crosses GY1
            x = x0 + (x1-x0)*(gy1-y0)/(y1-y0)
            y = gy1
            if ( ticks ) then
               call pgmove(x,y)
               call pgdraw(x,y+tly)
            endif
         ENDIF
         if ( c.eq.c0 ) then
            c = c1
            x0 = x
            y0 = y
            c0 = 0
         else
            c = c0
            x1 = x
            y1 = y
            c1 = 0
         endif
  150 CONTINUE
*
      if ( .not.ticks ) then
         pgx(1) = x0
         pgy(1) = y0
         pgx(2) = x1
         pgy(2) = y1
         call pgline(2, pgx, pgy)
      endif
      
      END
