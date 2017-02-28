      SUBROUTINE TTWRT(CLINE, LLINE)
      INTEGER   LLINE
      CHARACTER CLINE*(*)
C---
C This routine writes a single line to the current output device.
C If line editing is activated, then the line editing IO routines
C are used.
C---
C CLINE   I    The line to be written.
C LLINE   I    The number of valid characters in CLINE.  LLINE=-1
C              -causes LENACT to determine the number of valid characters.
C---
C 1990-Jun-19 - [AFT]
C---
      INCLUDE   'edicmn.inc'
      INTEGER   ILF
      PARAMETER (ILF   = 10)
      INTEGER   ICR
      PARAMETER (ICR   = 13)
      INTEGER   LENACT
C
      INTEGER   ITMP
C
      ITMP=LLINE
      IF(ITMP.LT.0) ITMP=LENACT(CLINE)
      IF(ICEDIT.EQ.0) THEN
C opps, can't pipe ftools
C         WRITE(*,111) CLINE(:ITMP)
C  111    FORMAT(A)
         call cputs(cline(:ITMP),ITMP)
      ELSE
         IF(IFTYPE.LT.0) CALL PUTSTR(CHAR(ICR)//CHAR(ILF), 2)
C Process carriage control.  '0' means skip a line.
         IF(CLINE(1:1).EQ.'0') THEN
            CALL PUTSTR(CHAR(ICR)//CHAR(ILF), 2)
         END IF
         CALL PUTSTR(CLINE(2:), ITMP-1)
         CALL PUTSTR(CHAR(ICR), 1)
         IF(IFTYPE.GT.0) CALL PUTSTR(CHAR(ILF),1)
         CALL FLUSH(6)
      END IF
      RETURN
      END
