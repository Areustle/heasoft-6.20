      SUBROUTINE opnche(IUNIT, CFILE, ITAB, MXBUF, QRO, LRECL, IERR)
      CHARACTER CFILE*(*)
      INTEGER   mxbuf
      INTEGER*4 iunit, lrecl, ierr
      INTEGER   ITAB(MXBUF)
      LOGICAL   QRO
C---
C DACHE subroutine to open the cached DA file.
C---
C IUNIT   i:   fortran LUN to use
C CFILE   i:   name of file to open
C ITAB()  i/r: the buffer used to hold the cache and associated info (see
C              details below) equivalenced to the integer*4 array ITAB
C MXBUF   i:   the size of the buffer
C QRO     i:   if true, file is to be open readonly
C              before they are overwritten in the cache (obsolete)
C LRECL   i:   number of bytes per record in file to be opened
C IERR      r: error flag, if zero then no error
C              If 1, then unable to open the da file
C                 2 - Unable to determine the recordlength
C                 3 - MXBUF too small to hold a single cache page.
C---
C 28-Jun-1985 - rashafer
C  5-Oct-1986 - Sun UNIX requires recl for direct access opens [kaa]
C 30-Jul-1988 - Add call to OPENWR for general VAX/Sun usage [AFT]
C---
C****** the buffer is divided into a header of 20 bytes:
C ipsize     i4  the size of a page in bytes
C npage      i4  the no. of pages in the cache
C lastpg     i4  the last page accessed
C lastcp     i4  the cache ptr of the last page accessed
C junit      i4  the unit to use for read and writes
C ****** followed by the cache pointers
C icadd(npage)  i4   the page no. currently in the ith position of
C                    the cache
C icuse(npage)  i4   the use index.  The lower the absolute value of the
C                    no., the more recently it has been accessed.  If
C                    a page is empty, the value is 0.  If the page
C                    has been written into, the value is <0.
C****** followed by the cached pages
C---

      INTEGER*4 LENACT
      INTEGER*4 ios, npage, ipage

      IF (LRECL.LE.0) THEN
         IF (IERR.EQ.0) WRITE (*, *) 'OPNCHE: Illegal record length:',
     &                               LRECL
         IERR = 4
         RETURN
      ENDIF
C---
      CLOSE (IUNIT)
      IF (QRO) THEN
         CALL OPENWR(IUNIT, CFILE, 'OLD', 'D', ' ', LRECL, 1, IOS)
      ELSE
         CALL OPENWR(IUNIT, CFILE, 'OLD', 'D', ' ', LRECL, 0, IOS)
      ENDIF
C
      IF (IOS.NE.0) THEN
         IF (IERR.EQ.0) THEN
 160        WRITE (*, 161) CFILE(:LENACT(CFILE)), IOS
 161        FORMAT (' OPNCHE:  Unable to open file ', A, ', IOSTAT=',
     &              I5)
         ENDIF
         IERR = 1
         RETURN
      ENDIF
C
      NPAGE = MXBUF/LRECL
      IF (NPAGE.LE.0) THEN
         IF (IERR.EQ.0) WRITE (*, *) 'OPNCHE: No room for one page of',
     &                               LRECL, ' bytes in buffer of size',
     &                               MXBUF
         IERR = 3
         RETURN
      ENDIF
      ITAB(1) = LRECL
      ITAB(2) = NPAGE
      ITAB(3) = -1
      ITAB(4) = 0
      ITAB(5) = IUNIT
      DO 190 IPAGE = 1, NPAGE
         ITAB(5+IPAGE) = -1
         ITAB(5+NPAGE+IPAGE) = 0
 190  CONTINUE
      IERR = 0
      RETURN
      END
