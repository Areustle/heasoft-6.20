C
      SUBROUTINE CONXBUF (K, X, Y, Z)
C
C Fortran routine for use with PGCONX
C
C Uses wcs data structure to do contour overlay.
C Extra complication of conx.inc buffer speeds up contour drawing
C
C Arguments:
C
C K (input, integer): if K=0, move the pen to (X,Y); if K=1, draw
C       a line from the current position to (X,Y); otherwise
C       do nothing.
C X (input, real): X world-coordinate of end point.
C Y (input, real): Y world-coordinate of end point.
C Z (input, real): the value of the contour level, not used
C-----------------------------------------------------------------------
      INCLUDE 'conx.inc'
      INTEGER  K
      REAL     X,Y,Z
C
C  Draw buffered points when full and reset
C
      if ( npts.eq.maxpts ) then
         call wcsconxbuf(npts, xbuf, ybuf, kbuf)
         npts = 0
      endif

      npts = npts + 1
      xbuf(npts) = x
      ybuf(npts) = y
      kbuf(npts) = k

      END
