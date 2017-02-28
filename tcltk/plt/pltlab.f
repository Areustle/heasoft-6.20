      SUBROUTINE PLTLAB(LUN, ICLAB, CLAB, FLAB, ILAB)
      INTEGER   LUN, ICLAB
      CHARACTER CLAB(*)*(*)
      REAL      FLAB(7,*)
      INTEGER   ILAB(7,*)
C---
C PLT support routine to write out commands of the form LA # 'test'
C---
C LUN     I
C ICLAB   I
C CLAB    I
C FLAB    I
C ILAB    I
C---
C 1990-Mar-01 - New routine [AFT]
C---
      INTEGER   LENACT
C
      CHARACTER CBUF*132
      CHARACTER CHPOS(3)*3, CVPOS(5)*3
      INTEGER   ISAV, LBUF, LTMP
      DATA CHPOS/'Lef','Cen','Rig'/
      DATA CVPOS/'Top','Cap','Hal','Bas','Bot'/
C---
   11 FORMAT(A)
C---
      IF(ILAB(1,ICLAB).NE.0) THEN
         CBUF = 'LAB  '
         LBUF = 5
         CALL CRAMI(ICLAB,CBUF,LBUF)
         LBUF = LBUF+1
         ISAV = LBUF
         IF(ILAB(4,ICLAB).NE.1) THEN
            CBUF(LBUF+1:LBUF+3) = 'COL'
            LBUF = LBUF+4
            CALL CRAMI(ILAB(4,ICLAB), CBUF, LBUF)
            LBUF = LBUF+1
         END IF
         IF(FLAB(4,ICLAB).NE.1.0) THEN
            CBUF(LBUF+1:LBUF+2) = 'CS'
            LBUF = LBUF+3
            CALL CRAMF(FLAB(4,ICLAB), CBUF, LBUF)
            LBUF = LBUF+1
         END IF
         IF( Ilab(7,iclab).GT.0 ) THEN
            CBUF(LBUF+1:LBUF+2) = 'TO'
            LBUF = LBUF+3
            CALL CRAMF(FLAB(5,ICLAB), CBUF, LBUF)
            LBUF = LBUF+1
            CALL CRAMF(FLAB(6,ICLAB), CBUF, LBUF)
            LBUF = LBUF+1
         ELSE IF(FLAB(6,ICLAB).GT.0.0) THEN
            CBUF(LBUF+1:LBUF+3) = 'LIN'
            LBUF = LBUF+4
            CALL CRAMF(FLAB(5,ICLAB), CBUF, LBUF)
            LBUF = LBUF+1
            IF ( ABS(FLAB(6,ICLAB)-0.08).GT.0.00001 ) THEN
               CALL CRAMF(FLAB(6,ICLAB), CBUF, LBUF)
               LBUF = LBUF+1
            END IF
         END IF
         IF(ILAB(5,ICLAB).NE.1) THEN
            CBUF(LBUF+1:LBUF+2) = 'LS'
            LBUF = LBUF+3
            CALL CRAMI(ILAB(5,ICLAB), CBUF, LBUF)
            LBUF = LBUF+1
         END IF
         IF(ILAB(6,ICLAB).GE.0) THEN
            CBUF(LBUF+1:LBUF+3) = 'MAR'
            LBUF = LBUF+4
            CALL CRAMI(ILAB(6,ICLAB), CBUF, LBUF)
            LBUF = LBUF+1
            IF(FLAB(7,ICLAB).NE.1.0) THEN
            CBUF(LBUF+1:LBUF+4) = 'MSIZ'
            LBUF = LBUF+5
            CALL CRAMF(FLAB(7,ICLAB), CBUF, LBUF)
            LBUF = LBUF+1
            END IF
         END IF
         IF(FLAB(3,ICLAB).NE.0.) THEN
            CBUF(LBUF+1:LBUF+3) = 'ROT'
            LBUF = LBUF+4
            CALL CRAMF(FLAB(3,ICLAB), CBUF, LBUF)
            LBUF = LBUF+1
         END IF
C---
C Add Center and Justification to end of string, so Line and Mark
C will not over-ride these settings.
         IF(ILAB(3,ICLAB).NE.3) THEN
            CBUF(LBUF+1:LBUF+7) = 'CEN '//CVPOS(ILAB(3,ICLAB))
            LBUF = LBUF+8
         END IF
         IF(ILAB(2,ICLAB).NE.2) THEN
            CBUF(LBUF+1:LBUF+7) = 'JUS '//CHPOS(ILAB(2,ICLAB))
            LBUF = LBUF+8
         END IF
         IF(LBUF.GT.ISAV) WRITE(LUN,11) CBUF(:LBUF-1)
C---
C Always write a line of the form, LAB # POS # # "label"
         CBUF = 'LAB  '
         LBUF = 5
         CALL CRAMI(ICLAB,CBUF,LBUF)
         LBUF = LBUF+1
         IF(ILAB(1,ICLAB).LT.0) THEN
            CBUF(LBUF+1:LBUF+3) = 'VIE'
         ELSE
            CBUF(LBUF+1:LBUF+3) = 'POS'
         END IF
         LBUF = LBUF+4
         CALL CRAMF(FLAB(1,ICLAB), CBUF, LBUF)
         LBUF = LBUF+1
         CALL CRAMF(FLAB(2,ICLAB), CBUF, LBUF)
         LTMP = MAX(LENACT(CLAB(ICLAB)), 1)
         WRITE(LUN,161) CBUF(:LBUF),CLAB(ICLAB)(:LTMP)
  161    FORMAT(A,' "',A,'"')
      END IF
      RETURN
      END
