c
c  Buffer used to speed drawing contour overlays
c  Repeated calls to C where the astTran2 routine must be
c   called from is slow.  However, cpgplot has no cpgconx
c   because of the use of a subroutine name as an argument.
c   The solution: build up a large number of conversions
c   to be made all at once
c
c Required routines
c
c FORTRAN:
c  conxbeg (init buffer)
c  conxbuf (buffers points until enough to call wcsconxbuf)
c  conxend (reset buffer)
c
c C (wcsmanager.c):
c  wcsconxbeg (init conversion and call conxbeg)
c  wcsconxbuf (converts coords and draws)
c  wcsconxend (cleanup conversion and call conxend)
c
      integer MAXPTS
      parameter (MAXPTS = 1000)
      integer npts

      real xbuf(MAXPTS)
      real ybuf(MAXPTS)
      integer kbuf(MAXPTS)

      common /conx/ npts, xbuf, ybuf, kbuf
