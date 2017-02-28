CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C tjdgrd.f
C $Id: tjdgrd.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C VERSION:   1.02      REVISION DATE:  10/29/84
C
C ROUTINE NAME:
C    TJDGRD
C
C ENGLISH NAME:
C    TRUNCATED JULIAN DATE TO GREGORIAN DATE
C
C SYSTEM AND SPACE CRAFT:
C    EGRET
C
C LANGUAGE AND MACHINE:
C    VS FORTRAN, IBM 3081 MVS
C
C FUNCTION:
C    CONVERTS A TRUNCATED JULIAN DATE TO A GREGORIAN DATE
C
C FORTRAN CALLING SEQUENCE:
C    CALL TJDGRD(TJDAY,MILSEC,YEAR,MONTH,DAY,HOUR,MIN,SEC,MILLIS,CODE)
C
C    VARIABLE         TYPE            I/O            DESCRIPTION
C    --------         ----            ---    ---------------------------
C    TJDAY          INTEGER            I     TRUNCATED JULIAN DATE
C    MILSEC         INTEGER            I     MILLISECOND OF DAY
C                                               (0 TO 86399999)
C    YEAR           INTEGER            O     YEAR RETURNED
C    MONTH          INTEGER            O     MONTH RETURNED
C    DAY            INTEGER            O     DAY RETURNED
C    HOUR           INTEGER            O     HOUR RETURNED
C    MIN            INTEGER            O     MINUTE RETURNED
C    SEC            INTEGER            O     SECOND RETURNED
C    MILLIS         INTEGER            O     MILLISECOND RETURNED
C    CODE           INTEGER            O     ERROR CODE
C                                               0 => VALID INPUT
C                                               1 => INVALID MILLISECOND
C COMMON
C    N.A.
C
C SIGNIFICANT VARIABLES:
C    VARIABLE    TYPE      INT. VAL.                 DESCRIPTION
C    --------    ----      ---------         ---------------------------
C    TJD        INTEGER    TJDAY             COPY OF TRUNCATED JUL DAY
C    MIL        INTEGER    MILSEC            COPY OF MILLISECOND OF DAY
C    DAYS(12)   INTEGER                      NUMBER OF DAYS IN MONTHS
C
C SUBROUTINES CALLED:
C    N.A.
C
C RESTRICTIONS:
C    N.A.
C
C ERROR HANDLING:
C    IF THE MILLISECOND OF DAY IS NOT WITHIN (0, 86399999) THE ERROR
C    CODE IS SET TO 1 AND THE SUBROUTINE TERMINATES. NORMAL CODE IS 0.
C
C METHOD:
C    INITIALYZE ARRAY HOLDING NUMBER OF DAYS IN EACH MONTH
C    SET CODE TO 0
C    SET TJD TO THE TRUNCATED JULIAN DATE PASSED
C    TJD  = TJD + 718956.00
C    YEAR = TJD/365.25
C    TJD = TJD - (YEAR*365.25)
C    IF (YEAR MOD 4 = 0) THEN
C       IF (TJD = 59) THEN
C          MONTH = 2
C          DAY = 29
C          GOTO NEXT TO COMPUTE THE MILLISECOND
C       ENDIF
C       IF (TJD < 59) TJD = TJD+1
C    ENDIF
C    INITIALIZE THE CURRENT MONTH, I, TO 1
C LOOP: IF (TJD <= NUMBER OF DAYS IN MONTH I) THEN
C       MONTH = I
C       DAY = TJD
C    ELSE
C       TJD = TJD - NUMBER OF DAYS IN MONTH I
C       INCREMENT THE CURRENT MONTH VALUE, I
C       IF (I < 13) GOTO LOOP
C    ENDIF
C NEXT: SET MIL TO THE MILLISECOND PASSED, MILSEC
C     HOUR = MIL/3600000
C     MIL = MIL - (HOUR*3600000)
C     MIN = MIL/60000
C     MIL = MIL - (MIN*60000)
C     SEC = MIL/1000
C     MILLIS = MIL - (SEC*1000)
C
C     RETURN
C
C REFERENCES:
C    N.A.
C
C PROGRAMMER AND DATE:
C    ALBERT ETIENNE		S.A.R.			04/30/84
C    E.S.Panduranga		S.T.X.			09/15/91
C
C MODIFICATIONS:
C  - LINE 'IF (TJD.LT.59) TJD = TJD + 1' MOVED FROM AFTER
C    LINE 'IF (MOD(YEAR,4).EQ.0) THEN' TO AFTER 'ENDIF' CORRESPONDING
C    TO   'IF (TJD.EQ.59) THEN'
C - CONSTANT 718956.00 CHANGED TO INTEGER
C - FUNCTION INT REMOVED FROM 'INT(HOUR*3600000)', 'INT(MIN*60000)',
C   'INT(SEC*1000)'
C ----------------------------------------------------------------------------
C $Log: tjdgrd.f,v $
C Revision 3.2  2013/05/21 19:08:27  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.1  2002/04/16 20:32:13  irby
C Additions to libgro - previously these codes existed in the following
C libraries:
C
C   libsenstv
C   libsysutil
C   libutil
C   libftio
C
c Revision 2.1  1991/10/08  21:48:29  esp
c First controlled version on the SUN.
c
C ----------------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC PROGRAM CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE TJDGRD(TJDAY,MILSEC,YEAR,MONTH,DAY,HOUR,MIN,
     +                  SEC,MILLIS,CODE)

      INTEGER      TJDAY,TJD,MONTH,DAY,YEAR,DAYS(12),CODE
      INTEGER      MILSEC,MIL,HOUR,MIN,SEC,MILLIS,I

      DATA DAYS/31,28,31,30,31,30,31,31,30,31,30,31/

      character(80)	rcsid
      common	/id/	rcsid
      rcsid = '$Id: tjdgrd.f,v 3.2 2013/05/21 19:08:27 irby Exp $'

      CODE = 0

      IF ((MILSEC.LT.0).OR.(MILSEC.GT.86399999)) THEN
         CODE = 1
         RETURN
      ENDIF

C COMPUTE THE GREGORIAN DATE
      TJD = TJDAY
      TJD = TJD + 718956
      YEAR = TJD/365.25
      TJD = TJD - INT(YEAR*365.25)
      IF (MOD(YEAR,4).EQ.0) THEN
         IF (TJD.EQ.59) THEN
            MONTH = 2
            DAY = 29
            GOTO 15
         ENDIF
         IF (TJD.LT.59) TJD = TJD + 1
      ENDIF
      I = 1
10    IF (TJD.LE.DAYS(I)) THEN
         MONTH = I
         DAY = TJD
      ELSE
         TJD = TJD - DAYS(I)
         I = I + 1
         IF (I.LT.13) GOTO 10
         WRITE(*,'('' PROGRAMMING ERROR IN TJDGRD SUBROUTINE'')')
      ENDIF

C COMPUTE THE TIME
15    MIL = MILSEC
      HOUR = MIL/3600000
      MIL = MIL - (HOUR*3600000)
      MIN = MIL/60000
      MIL = MIL - (MIN*60000)
      SEC = MIL/1000
      MILLIS = MIL - (SEC*1000)

      RETURN

CCCCCCCCCCCCCCCCCCCCCCCCCCC END TJDGRD CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      END
