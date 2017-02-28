      SUBROUTINE wrtpag(IUNIT, C1BUF, ICPAGE, IPSIZE, IFPAGE, IERR)
C---
C DACHE subroutine to readin a page of a DA file into the buffer
C---
C 29-Jun-1985 - rashafer
C---
      INTEGER*4 i, iunit, icpage, ipsize, ifpage, ierr, ios
      character(1) C1BUF(IPSIZE, ICPAGE)
C---
      WRITE (IUNIT, REC=IFPAGE+1, IOSTAT=IOS) (C1BUF(I,ICPAGE), I=1,
     &                                        IPSIZE)
      IF (IOS.NE.0) THEN
         IF (IERR.EQ.0) WRITE (*, 111) IOS, IFPAGE + 1, IUNIT
 111     FORMAT (' *ERROR*:WRTPAG: Write error', I4, ' for record', I4,
     &           ' on unit', I2)
         IERR = 5
      ENDIF
      RETURN
      END
