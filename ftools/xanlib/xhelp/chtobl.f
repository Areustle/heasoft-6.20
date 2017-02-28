      SUBROUTINE chtobl(cray, lray, cbuf, lbuf, ipage, mxbuf, iunit)
      CHARACTER cray*(*), cbuf*(*)
      INTEGER lray, lbuf, ipage, mxbuf, iunit
C---
C SHF subroutine the accumulates bytes into a buffer array and then
C write out sequential blocks into the indicated direct access file.
C---
C CRAY      I    String of bytes to be accumulated
C LRAY      I    No. of bytes to so accumulate, when < 0 then dump
C			the buffer in any case
C CBUF      I/O  Accumulation buffer
C LBUF      I/O  Current location in buffer (must be set to zero before
C		the first call to C1TOBL
C IPAGE     I/O  Current d.a. record in file
C MXBUF     I    Size of buffer (BYTYES)
C IUNIT     I    Fortran unit no.  File must have been previously opened
C		as a Fortran Unformatted direct access file.  Be
C		sure that the record length in the open statement
C		was a number of full-words (4 byte quantitites).
C		(Thus mxbuf must be a multiple of 4)
C---
C 27 Jun 1985 - rashafer
C 25 Feb 1987 to do a record write when lray is a negative number.
C 10 Mar 1987 to properly handle multiple buffer length records
C---
      INTEGER i
C---
      IF (lray.LE.0) THEN
         IF (lbuf.GT.0) THEN
C** non empty buffer so dump it
            ipage = ipage + 1
            WRITE (iunit, REC=ipage) cbuf
            lbuf = 0
         ENDIF
         RETURN
      ENDIF
C---
      DO 190 i = 1, lray
         lbuf = lbuf + 1
         cbuf(lbuf:lbuf) = cray(i:i)
         IF (lbuf.EQ.mxbuf) THEN
            ipage = ipage + 1
            WRITE (iunit, REC=ipage) cbuf
            lbuf = 0
         ENDIF
 190  CONTINUE
      RETURN
      END
