      SUBROUTINE WRTCON(LUN, ICNUM, RLEV, ICOCON, ILSCON,
     :      RLWCON, MXLEV, CTMP)
      INTEGER   LUN, ICNUM, ICOCON(*), ILSCON(*), MXLEV
      REAL      RLEV(*), RLWCON(*)
      CHARACTER CTMP*(*)
C---
C Write out lines that describe the current contour plot.
C---
C LUN     I
C ICNUM   I    Contour plot number
C RLEV    I
C ICOCON  I
C ILSCON  I
C RLWCON  I
C MXLEV   I
C CTMP    S    Scratch area
C---
C 1989-Oct-03 - [AFT]
C---
      REAL      NO
      PARAMETER (NO=-1.2E-34)
C
      INTEGER   I, LTMP, NLEV
C---
   11 FORMAT(A)
C---
      NLEV=0
      DO 110 I=1,MXLEV
         IF(NLEV.EQ.0 .AND. RLEV(I).EQ.NO) NLEV=I-1
  110 CONTINUE
      IF(NLEV.EQ.0) NLEV=MXLEV
C
      WRITE(CTMP,121) ICNUM,'LEVEL'
  121 FORMAT('CONT',I3,1X,A5,1X)
      LTMP = 14
      DO 130 I=1,NLEV
         CALL CRAMFF(RLEV(I), 7, 0, CTMP, LTMP)
         LTMP=LTMP+1
  130 CONTINUE
      IF(LUN.EQ.0) THEN
         WRITE(*,11) CTMP(:LTMP-1)
      ELSE
         WRITE(LUN,11) CTMP(:LTMP-1)
      END IF
C
      WRITE(CTMP,121) ICNUM,'COLOR'
      LTMP = 14
      DO 150 I=1,NLEV
         CALL CRAMIF(ICOCON(I), 7, CTMP, LTMP)
         LTMP=LTMP+1
  150 CONTINUE
      IF(LUN.EQ.0) THEN
         WRITE(*,11) CTMP(:LTMP-1)
      ELSE
         WRITE(LUN,11) CTMP(:LTMP-1)
      END IF
C
      WRITE(CTMP,121) ICNUM,'LSTYL'
      LTMP = 14
      DO 170 I=1,NLEV
         CALL CRAMIF(ILSCON(I), 7, CTMP, LTMP)
         LTMP=LTMP+1
  170 CONTINUE
      IF(LUN.EQ.0) THEN
         WRITE(*,11) CTMP(:LTMP-1)
      ELSE
         WRITE(LUN,11) CTMP(:LTMP-1)
      END IF
C
      WRITE(CTMP,121) ICNUM,'LWID'
      LTMP = 14
      DO 190 I=1,NLEV
         CALL CRAMFF(RLWCON(I), 7, 0, CTMP, LTMP)
         LTMP=LTMP+1
  190 CONTINUE
      IF(LUN.EQ.0) THEN
         WRITE(*,11) CTMP(:LTMP-1)
      ELSE
         WRITE(LUN,11) CTMP(:LTMP-1)
      END IF
      RETURN
      END
