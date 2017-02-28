      SUBROUTINE PTBUF(Cline, Lline)
      CHARACTER Cline*(*)
      INTEGER   Lline
C---
C This routine writes a single line to the current output device.
C If line editing is activated, then the line editing IO routines
C are used.
C---
C Cline   I    The line to be written.
C Lline   I    The number of valid characters in Cline.  Lline=-1
C              -causes LENACT to determine the number of valid characters.
C---
C 2002-May-23 - Adapted from ttwrt [AFT]
C---
      INCLUDE   'edicmn.inc'
      INTEGER   ILF
      PARAMETER (ILF   = 10)
      INTEGER   ICR
      PARAMETER (ICR   = 13)
      INTEGER   LENACT
C
      REAL      rbuf(1)
      INTEGER   itmp
C
      itmp=Lline
      IF ( itmp.LT.0 ) itmp=LENACT(Cline)
      IF ( ICEDIT.EQ.0 ) THEN
         WRITE(*,111) Cline(:itmp)
  111    FORMAT(A)
      ELSE
         IF ( IFTYPE.LT.0 ) CALL PUTSTR(CHAR(ICR)//CHAR(ILF), 2)
         CALL PUTSTR(Cline, itmp)
         CALL PUTSTR(CHAR(ICR), 1)
         IF ( IFTYPE.GT.0 ) CALL PUTSTR(CHAR(ILF),1)
         CALL FLUSH(6)
      END IF
C
C And log
      CALL LOGGER(5,rbuf,0,Cline,itmp)
      RETURN
      END
