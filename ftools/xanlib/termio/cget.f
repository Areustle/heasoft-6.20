      character(1) FUNCTION CGET(CHR)
      CHARACTER CHR*1
C---
C Read a single character from the terminal.  Note, known escape
C sequences are translated into the corresponding control character.
C---
      INCLUDE 'edicmn.inc'
      INTEGER   ITAB
      PARAMETER (ITAB  =  9)
      INTEGER   ILF
      PARAMETER (ILF   = 10)
      INTEGER   ICR
      PARAMETER (ICR   = 13)
      INTEGER   IESC
      PARAMETER (IESC  = 27)
      INTEGER   ICSI
      PARAMETER (ICSI  =155)
C
      CHARACTER CNEX*1
C
  100 CALL RDCHR (CNEX)
C
      IF (CNEX.EQ.CHAR(IESC) .OR. CNEX.EQ.CHAR(ICSI)) THEN
C Escape or CSI
         IF(CNEX.EQ.CHAR(IESC)) THEN
            CALL RDCHR (CNEX)
            IF(CNEX.NE.'[') THEN
               CHR=CNEX
               GOTO 200
            END IF
         ENDIF
         CALL RDCHR (CNEX)
         IF (CNEX .EQ. 'A') THEN
            CHR = CHAR(IUP)
         ELSEIF (CNEX .EQ. 'B') THEN
            CHR = CHAR(IDOWN)
         ELSEIF (CNEX .EQ. 'C') THEN
            CHR = CHAR(IRIGHT)
         ELSEIF (CNEX .EQ. 'D') THEN
            CHR = CHAR(ILEFT)
         ELSE
            CHR = CNEX
         END IF
      ELSE IF(ICHAR(CNEX).LT.32) THEN
C Control character
         IF (CNEX.EQ.CHAR(ILF) .OR. CNEX.EQ.CHAR(ICR)) THEN
            CHR = CHAR(ICR)
         ELSEIF (CNEX.EQ.CHAR(ITAB)) THEN
            CHR = ' '
         ELSEIF (CNEX.EQ.CHAR(IUP) .OR. CNEX.EQ.CHAR(IDOWN) .OR.
     :         CNEX.EQ.CHAR(IRIGHT) .OR. CNEX.EQ.CHAR(ILEFT) .OR.
     :         CNEX.EQ.CHAR(IBEG) .OR. CNEX.EQ.CHAR(IEND) .OR.
     :         CNEX.EQ.CHAR(IWRITE). OR.
     :         CNEX.EQ.CHAR(IERASE) .OR. CNEX.EQ.CHAR(IEOF)) THEN 
            CHR = CNEX
         ELSE
            GOTO 100
         END IF
      ELSE
C Other
         CHR = CNEX
      ENDIF
C
  200 CGET = CHR
      RETURN
      END
