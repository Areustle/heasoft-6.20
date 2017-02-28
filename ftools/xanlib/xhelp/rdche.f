      SUBROUTINE rdche(ITAB, CBUF, COUT, IADD, NBYTE, IER)
C---
C DACHE subroutine to read from a direct access file
C---
C ITAB  I/O I   See OPNCHE for description
C CBUF	I/O C*	cache buffer
C COUT	  O C*	returned values
C IADD	I   I	starting byte to read (starting with 0)
C NBYTE	I   I	no. of COUT needed
C IER	I/O I	DACHE error flag
C---
C 28-Jun-85 - rashafer
C  1-Aug-88 - Better use of character array [AFT]
C---
      CHARACTER CBUF*(*), COUT*(*)
      INTEGER ITAB(*)
 
      INTEGER*4 iadd, nbyte, ier, icur, jadd, jbyte
      INTEGER*4 jer, jbeg, jend, ichbeg, jtrans
 
C---
      IER = 0
      ICUR = 0
      JADD = IADD
      JBYTE = NBYTE
      JER = IER
C*** REPEAT
 110  CONTINUE
      JBEG = ICHBEG(ITAB, CBUF, JADD, JBYTE, JEND, JER)
      JTRANS = JEND - JBEG + 1
      IF (JER.NE.0) THEN
         IER = JER
         ICUR = NBYTE
      ELSE
         COUT(ICUR+1:MIN(ICUR+JTRANS,NBYTE)) = CBUF(JBEG+1:JEND+1)
         ICUR = MIN(ICUR + JTRANS, NBYTE)
      ENDIF
      JADD = JADD + JTRANS
      JBYTE = JBYTE - JTRANS
C** UNTIL current number in COUT equals total requested
      IF (ICUR.LT.NBYTE) THEN
         GOTO 110
      ENDIF
      RETURN
      END
