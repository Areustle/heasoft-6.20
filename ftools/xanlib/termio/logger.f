      SUBROUTINE LOGGER(IFUNC, RBUF, NBUF, CBUF, LBUF)
      INTEGER   IFUNC, NBUF, LBUF
      REAL      RBUF(*)
      CHARACTER CBUF*(*)
C---
C Device driver for the LOG file.
C---
C 1990-Sep-16 - [AFT]
C---
      INTEGER   LENACT
C
      CHARACTER(64) CDNAM
      CHARACTER(255) CTMP
      CHARACTER(10) CACC
      CHARACTER(8) CSTAT
      INTEGER   IER, ITMP, LUNLOC, i
      SAVE      LUNLOC, CDNAM
      DATA      LUNLOC/0/, CDNAM/'LOG.LOG'/
C---
   11 FORMAT(A)
C---
C
C--- IFUNC = 1, Set default file name-----------------------------------
C
      IF ( IFUNC .EQ. 1 ) THEN
         IF(LENACT(CBUF).NE.0) THEN
            CDNAM=CBUF
         END IF
C
C--- IFUNC = 2, Get default name and unit number -----------------------
C
      ELSEIF ( IFUNC .EQ. 2 ) THEN
         RBUF(1)=LUNLOC
         NBUF=1
         IF(LUNLOC.EQ.0) THEN
            CBUF=CDNAM
         ELSE
            INQUIRE(UNIT=LUNLOC,NAME=CBUF)
         END IF
         LBUF=LENACT(CBUF)
C
C--- IFUNC = 3, Open the file ------------------------------------------
C
      ELSEIF ( IFUNC .EQ. 3 ) THEN
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
         LUNLOC=INT(RBUF(1))
         IF(LUNLOC.NE.0) THEN
            CLOSE(UNIT=LUNLOC)
         ELSE
            CALL GETLUN(LUNLOC)
C         print *,'getlun called in logger, lunloc is ',lunloc
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
C            print *,'frelun called in logger, lunloc is ',lunloc
               CALL FRELUN(LUNLOC)
            END IF
            LUNLOC=0
C Report error to calling program.
            NBUF=-1
            RBUF(1)=0.0
         END IF
C
C--- IFUNC = 4, Close the file -----------------------------------------
C
      ELSEIF ( IFUNC .EQ. 4 ) THEN
         IF(LUNLOC.NE.0) THEN
            CLOSE(UNIT=LUNLOC, STATUS=CBUF)
C         print *,'frelun called in logger #2, lunloc is ',lunloc
            CALL FRELUN(LUNLOC)
            LUNLOC=0
         END IF
C
C--- IFUNC = 5, Write a string to the file (if active) -----------------
C
      ELSEIF ( IFUNC .EQ. 5 ) THEN
         IF(LUNLOC.NE.0) THEN
            ITMP=LBUF
            IF(ITMP.EQ.0) ITMP=LENACT(CBUF)
            if (itmp .gt. 79) then
               do i=1,itmp,79
                  if (itmp .le. i+78) then
                     WRITE(LUNLOC,11,IOSTAT=ier) CBUF(i:itmp)
                  else
                     WRITE(LUNLOC,11,IOSTAT=ier) CBUF(i:i+78)
                  end if
               end do
            else
               WRITE(LUNLOC,11,IOSTAT=ier) CBUF(:ITMP)
            end if
         END IF
C
C--- IFUNC = 6, return the file name
C
      ELSEIF ( IFUNC .EQ. 6 ) THEN
         IF(LUNLOC.NE.0) THEN
            inquire(unit=LUNLOC,name=cbuf)
         END IF

      ELSE
         WRITE(*,901) IFUNC
 901     FORMAT('Unimplemented function in LOGGER device driver:',I5)
         NBUF = -1

      ENDIF

      RETURN
      END

