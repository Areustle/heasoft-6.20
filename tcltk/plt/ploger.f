      SUBROUTINE PLOGER(CTOK, LTOK, IDOALL, ICWIN, MXWIN,
     :      CXOPT, LOGX, CYOPT, LOGY, IER)
      CHARACTER CTOK*(*), CXOPT(*)*(*), CYOPT(*)*(*)
      INTEGER   LTOK, IDOALL, ICWIN, MXWIN, IER
      INTEGER   LOGX(*), LOGY(*)
C---
C Sets the log flag for use in PLT
C---
      REAL      FPNUM
C
      CHARACTER CPEEK*2, CV*1
      INTEGER   IDOX, IDOY, ILOGIT, ITMP, IWNUM, LPEEK
C---
      IER=0
      IDOX=1
      IDOY=1
  100 CALL GTCHAR(CTOK,LTOK)
      CALL UPC(CTOK)
      IF(CTOK(1:1).EQ.'X') THEN
         IDOX=IDOX+1
         IDOY=IDOY-1
         GOTO 100
      ELSE IF(CTOK(1:1).EQ.'Y') THEN
         IDOX=IDOX-1
         IDOY=IDOY+1
         GOTO 100
      END IF
C---
      CALL GTPEEK(CPEEK, LPEEK)
      IF(CTOK(1:2).EQ.'OF') THEN
         ILOGIT=0
         CV=' '
      ELSE
         ILOGIT=1
         CV='L'
      END IF
C---
      IF(LPEEK.EQ.0 .AND. IDOALL.NE.0) THEN
         DO IWNUM=1,MXWIN
            IF(IDOX.NE.0) THEN
               LOGX(IWNUM)=ILOGIT
               CXOPT(IWNUM)(6:6)=CV
            END IF
            IF(IDOY.NE.0) THEN
               LOGY(IWNUM)=ILOGIT
               CYOPT(IWNUM)(6:6)=CV
            END IF
         END DO
      ELSE
         IWNUM=ICWIN
  150    IF(LPEEK.GT.0) THEN
            CALL GTCHAR(CTOK, LTOK)
            IF(LTOK.LE.0) GOTO 900
            ITMP=FPNUM(CTOK,LTOK,IER)
            IF(IER.NE.0) GOTO 900
            IF(ITMP.GT.0 .AND. ITMP.LE.MXWIN) IWNUM=ITMP
         END IF
         IF(IDOX.NE.0) THEN
            LOGX(IWNUM)=ILOGIT
            CXOPT(IWNUM)(6:6)=CV
         END IF
         IF(IDOY.NE.0) THEN
            LOGY(IWNUM)=ILOGIT
            CYOPT(IWNUM)(6:6)=CV
         END IF
         IF(LPEEK.NE.0) GOTO 150
      END IF
C---
  900 CONTINUE
      RETURN
      END
