      SUBROUTINE CONXEND ()
c
c  Draw remaining buffered points and reset
c
      INCLUDE 'conx.inc'

      call wcsconxbuf(npts, xbuf, ybuf, kbuf)
      npts = 0
C
      END
