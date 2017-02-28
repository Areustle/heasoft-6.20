C*PGSETDEFAULT -- define a clipping window
C+
      SUBROUTINE PGSETDEFAULT
C
C GRPCKG: Define a rectangular window in the current plotting area. All
C graphics (except characters written with GRCHAR) will be blanked
C outside this window.  The default window is the full plotting area
C defined by default or by GRSETS.
C
C Arguments:
C
C IDENT (input, integer): the plot identifier, returned by GROPEN.
C X0, Y0 (input, real): the lower left corner of the window, in absolute
C       device coordinates.
C XSIZE, YSIZE (input, real): width and height of the window in absolute
C       coordinates; if either is negative, the window will be reset to
C       the full plotting area.
C--
C  1-Feb-1983 - [TJP].
C 25-Nov-1994 - use floating-point [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'

C Set the default window (unit square).
C
      PGXBLC(PGID) = 0.0
      PGXTRC(PGID) = 1.0
      PGXBLC(PGID) = 0.0
      PGXTRC(PGID) = 1.0
      PGYBLC(PGID) = 0.0
      PGYTRC(PGID) = 1.0
C
      END
