      INTEGER FUNCTION ichbeg(ITAB, CBUF, IADD, NBYTE, LSTADD, IERR)
      CHARACTER   CBUF*(*)
      INTEGER     ITAB(*)
      INTEGER     iadd, nbyte, lstadd, ierr
C---
C DACHE function call to return the buffer address of
C a given DA file address
C---
C ICHBEG   r:   the offset in the cache buffer that points to the first
C               byte needed (byte 0 is the beginning of the buffer
C               and the file)
C ITAB     I/R: See OPNCHE for a description
C CBUF          cache buffer
C IADD     I:   byte file address
C NBYTE    i:   last byte needed
C LSTADD   r:   last buffer address actually in cache block in
C                the needed range
C IERR     r:   error flag 4 - rdpag error
C---
C 28-Jun-85 - rashafer
C---

      INTEGER*4 ipsize, npage
      INTEGER*4 maxoff, icpage, iccp, i, iunit, imax
      INTEGER*4 jmax, iuse, jpage, ipage, iusoff, icoff
      INTEGER*4 juse, jpgoff

C---
C** See OPNCHE for layout of the buffer
      ipsize = ITAB(1)
      npage = ITAB(2)
      iusoff = 5 + npage
      icpage = (iadd/ipsize)
      icoff = iadd - icpage*ipsize
      maxoff = min(icoff+nbyte, ipsize) - 1
      IF (icpage.EQ.ITAB(3)) THEN
         iccp = ITAB(4)
      ELSE
C** find if the current page is in the cache
         iccp = 0
         i = 0
 110     IF ((iccp.EQ.0) .AND. (i.LT.npage)) THEN
            i = i + 1
            IF (ITAB(5+i).EQ.icpage) iccp = i
            GOTO 110
         ENDIF
         IF (iccp.EQ.0) THEN
C** the requested page not in the cache, find free
C** page
            iunit = ITAB(5)
            i = 0
            imax = 0
            jmax = 0
 150        IF ((iccp.EQ.0) .AND. (i.LT.npage)) THEN
               i = i + 1
               iuse = abs(ITAB(i+iusoff))
               IF (iuse.EQ.0) THEN
C** a pristine page
                  iccp = i
               ELSEIF (iuse.GT.imax) THEN
                  imax = iuse
                  jmax = i
               ENDIF
               GOTO 150
            ENDIF
            IF (iccp.EQ.0) THEN
               iccp = jmax
               IF (iccp.LE.0) iccp = 1
               IF (ITAB(iusoff+iccp).LT.0) THEN
C** the cache page has been updated, so first write it back out
                  jpage = ITAB(5+iccp)
                  CALL wrtpag(iunit, CBUF, iccp, ipsize, jpage, ierr)
               ENDIF
            ENDIF
            DO 180 ipage = 1, npage
               iuse = ITAB(ipage+iusoff)
               IF (iuse.NE.0) ITAB(ipage+iusoff)
     &              = sign(abs(iuse)+1, iuse)
 180        CONTINUE
            ITAB(iccp+iusoff) = 1
            CALL rdpag(iunit, CBUF, iccp, ipsize, icpage, ierr)
            ITAB(5+iccp) = icpage
         ELSE
C** mark some other page as more current
            iuse = abs(ITAB(iccp+iusoff))
            DO 220 ipage = 1, npage
               juse = ITAB(ipage+iusoff)
               IF ((abs(juse).LT.iuse) .AND. (juse.NE.0)) THEN
                  ITAB(ipage+iusoff) = sign(abs(juse)+1, juse)
               ENDIF
 220        CONTINUE
         ENDIF
         ITAB(3) = icpage
         ITAB(4) = iccp
      ENDIF
C** calculate the byte address of the 0th byte of the selected page
      jpgoff = ipsize*(iccp-1)
      ICHBEG = jpgoff + icoff
      lstadd = jpgoff + maxoff
      RETURN
      END
