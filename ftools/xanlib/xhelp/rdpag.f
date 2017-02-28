      SUBROUTINE rdpag(IUNIT, BUF, ICPAGE, IPSIZE, IFPAGE, IER)
C---
C DACHE subroutine to readin a page of a DA file into the buffer.
C---
C 29 June 1985 rashafer
C---
      INTEGER*4 iunit, icpage, ipsize, ifpage, ier, ios, i
      character(1) BUF(IPSIZE, ICPAGE)
C---
      READ (IUNIT, REC=IFPAGE+1, IOSTAT=IOS) (BUF(I,ICPAGE), I=1, IPSIZE
     &                                       )
      IF (IOS.NE.0) THEN
         IF (IER.EQ.0) WRITE (*, 101) IOS, IFPAGE + 1, IUNIT
 101     FORMAT (' *ERROR*:RDPAG: Read error ', I3, ' for record', I4,
     &           ' on unit', I3)
         IER = 4
      ENDIF
      RETURN
      END
