      SUBROUTINE ERRLOG(IFUNC, RBUF, NBUF, CBUF, LBUF)
      INTEGER   IFUNC, NBUF, LBUF
      REAL      RBUF(*)
      CHARACTER CBUF*(*)
C---
C Device driver for the error log
C---
C 1991-Nov-21 - [AMTP] adaptation of AFT's LOGGER
c 1992-Nov-21 - NEW increased ctmp and cdnam to 80, so xhelp works
C---
      INTEGER   LENACT
C
      CHARACTER FLAG*1
      PARAMETER (FLAG='^')

      CHARACTER CDNAM*80, CTMP*80
      CHARACTER CACC*10
      CHARACTER CSTAT*8
      INTEGER   IER, ITMP, LUNLOC
      SAVE      LUNLOC, CDNAM
      DATA      LUNLOC/0/, CDNAM/'ERROR.LOG'/
C---
   11 FORMAT(A)
C---
      GOTO( 10, 20, 30, 40, 50) IFUNC
  900 WRITE(*,901) IFUNC
  901 FORMAT('Unimplemented function in ERRLOG device driver:',I5)
      NBUF = -1
      RETURN
C
C--- IFUNC = 1, Set default file name-----------------------------------
C
   10 CONTINUE
      IF(LENACT(CBUF).NE.0) THEN
         CDNAM=CBUF
      END IF
      RETURN
C
C--- IFUNC = 2, Get default name and unit number -----------------------
C
   20 CONTINUE
      RBUF(1)=LUNLOC
      NBUF=1
      IF(LUNLOC.NE.0) THEN
         CBUF=CDNAM
      ELSE
         INQUIRE(UNIT=LUNLOC,NAME=CBUF)
      END IF
      LBUF=LENACT(CDNAM)
      RETURN
C
C--- IFUNC = 3, Open the file ------------------------------------------
C
   30 CONTINUE
      IF(LBUF.GT.0) THEN
         CTMP=CBUF
         CALL XTEND(CTMP,'LOG')
      ELSE
         CTMP=CDNAM
      END IF
C
C If already open, close current file, and open a new version.
      IF(LUNLOC.NE.0) CLOSE(UNIT=LUNLOC)
C User can force the selection of a logical unit number.
      LUNLOC=RBUF(1)
      IF(LUNLOC.NE.0) THEN
         CLOSE(UNIT=LUNLOC)
      ELSE
         CALL GETLUN(LUNLOC)
      END IF
C
      IF(CTMP(1:1).EQ.'+') THEN
         ITMP=2
         CACC='E'
         CSTAT='UNKNOWN'
      ELSE
         ITMP=1
         CACC=' '
         CSTAT='NEW'
      END IF
C
      CALL OPENWR(LUNLOC, CTMP(ITMP:), CSTAT, CACC, 'L', 0, 0, IER)
      IF(IER.EQ.0) THEN
         NBUF=0
         RBUF(1)=LUNLOC
      ELSE
C Clean up after error.
         IF(RBUF(1).EQ.0.) THEN
            CALL FRELUN(LUNLOC)
         END IF
         LUNLOC=0
C Report error to calling program.
         NBUF=-1
         RBUF(1)=0.0
      END IF
      RETURN
C
C--- IFUNC = 4, Close the file -----------------------------------------
C
   40 CONTINUE
      IF(LUNLOC.NE.0) THEN
         CLOSE(UNIT=LUNLOC, STATUS=CBUF)
         CALL FRELUN(LUNLOC)
         LUNLOC=0
      END IF
      RETURN
C
C--- IFUNC = 5, Write a string to the file (if active) -----------------
C
   50 CONTINUE
      ITMP=LBUF
      IF(ITMP.EQ.0) ITMP=LENACT(CBUF)
      IF(LUNLOC.EQ.0) THEN
	 CTMP = FLAG//CBUF(:ITMP)         
	 WRITE(*,11) CTMP(:ITMP+1)
      ELSE
         WRITE(LUNLOC,11) CBUF(:ITMP)
      END IF
      RETURN
      END
