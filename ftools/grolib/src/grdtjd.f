CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C grdtjd.f
C $Id: grdtjd.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C VERSION:   V1.02        REVISION DATE: 10/29/84
C
C ROUTINE NAME:
C    GRDTJD
C
C ENGLISH NAME:
C    GREGORIAN DATE TO TRUNCATED JULIAN DATE
C
C SYSTEM AND SPACE CRAFT:
C    EGRET
C
C LANGUAGE AND MACHINE:
C    VS FORTRAN, IBM 3081 MVS
C
C FUNCTION:
C    CONVERTS A GREGORIAN DATE TO TRUNCATED JULIAN DATE
C
C FORTRAN CALLING SEQUENCE:
C    CALL GRDTJD(TJDAY,MILSEC,YEAR,MONTH,DAY,HOUR,MIN,SEC,MILLIS,CODE)
C
C    VARIABLE         TYPE            I/O            DESCRIPTION
C    --------         ----            ---    ---------------------------
C    TJDAY          INTEGER            O     TRUNCATED JULIAN DATE
C                                               RETURNED
C    MILSEC         INTEGER            O     MILLISECOND OF DAY
C                                               RETURNED
C    YEAR           INTEGER            I     YEAR ENTERED (4 DIGITS)
C    MONTH          INTEGER            I     MONTH ENTERED
C    DAY            INTEGER            I     DAY ENTERED
C    HOUR           INTEGER            I     HOUR ENTERED
C    MIN            INTEGER            I     MINUTE ENTERED
C    SEC            INTEGER            I     SECOND ENTERED
C    MILLIS         INTEGER            I     MILLISECOND ENTERED
C    CODE           INTEGER            O     ERROR CODE
C                                               0 => GOOD DATE AND TIME
C                                               1 => ERROR IN DATE
C                                               2 => ERROR IN TIME
C
C COMMON
C    N.A.
C
C SIGNIFICANT VARIABLES:
C    VARIABLE    TYPE      INT. VAL.                 DESCRIPTION
C    --------    ----      ---------         ---------------------------
C    DAYS(12)   INTEGER                      NUMBER OF DAYS IN MONTHS
C
C SUBROUTINES CALLED:
C    N.A.
C
C RESTRICTIONS:
C    N.A.
C
C ERROR HANDLING:
C    THE ERROR CODE IS SET TO 1 FOR AN INVALID DATE AND TO 2 FOR AN
C    INVALID TIME. NORMAL RETURN CODE IS 0.
C
C METHOD:
C          INITIALYZE THE ARRAY OF NUMBER OF DAYS IN EACH MONTH
C          CODE = 0
C          IF MONTH>12 OR MONTH<1 GOTO BAD DATE
C          IF DAY>31 OR DAY<1 GOTO BAD DATE
C          IF MONTH = 1,3,5,7,8,10,12 GOTO GOODATE
C          IF MONTH>30 GOTO BAD DATE
C          IF MONTH <> 2 GOTO GOODATE
C          IF DAY>29 GOTO BAD DATE
C          IF (DAY=29) AND (YEAR MOD 4 <> 0) GOTO BAD DATE
C GOODATE: IF HOUR>23 OR HOUR<0 GOTO BAD TIME
C          IF MIN>59 OR MIN<0 GOTO BAD TIME
C          IF SEC>59 OR SEC<0 GOTO BAD TIME
C          IF MILLIS>999 OR MILLIS<0 GOTO BAD TIME
C          TJDAY = DAY + 365.25*(YEAR)
C          IF ((YEAR MOD 4 = 0) AND MONTH <= 2) THEN N = N - 1
C          IF MONTH > 1 THEN
C             FOR I=2 TO MONTH DO
C                TJDAY = TJDAY + NUMBER OF DAYS IN MONTH I-1
C             END FOR
C          ENDIF
C          TJDAY = N - 718956
C          MILSEC = ((((HOUR*60)+MIN)*60+SEC)*1000+MILLIS)
C          RETURN
C
C    BAD DATE: CODE = 1
C    RETURN
C    BAD TIME: CODE = 2
C    RETURN
C
C REFERENCES:
C    N.A.
C
C PROGRAMMER AND DATE:
C    ALBERT ETIENNE		S.A.R.               04/30/84
C    E.S.Panduranga		S.T.X.               09/15/91
C
C MODIFICATIONS:
C    - CONSTANT 718956.00 CHANGED TO INTEGER
C ----------------------------------------------------------------------------
C $Log: grdtjd.f,v $
C Revision 3.2  2013/05/21 19:08:27  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.1  2002/04/16 20:32:12  irby
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

      SUBROUTINE GRDTJD(TJDAY,MILSEC,YEAR,MONTH,DAY,HOUR,MIN,SEC,MILLIS,
     +                  CODE)

      INTEGER      TJDAY,MONTH,DAY,YEAR,DAYS(12),CODE
      INTEGER      MILSEC,HOUR,MIN,SEC,MILLIS,I

      DATA DAYS/31,28,31,30,31,30,31,31,30,31,30,31/

      character(80)	rcsid
      common	/id/	rcsid
      rcsid = '$Id: grdtjd.f,v 3.2 2013/05/21 19:08:27 irby Exp $'

      CODE = 0

C CHECK FOR A VALID DATE
      IF ((MONTH.GT.12).OR.(MONTH.LT.1)) GOTO 30
      IF ((DAY.GT.31).OR.(DAY.LT.1)) GOTO 30
      IF ((MONTH.EQ.1).OR.(MONTH.EQ.3).OR.(MONTH.EQ.5).OR.(MONTH.EQ.7)
     +    .OR.(MONTH.EQ.8).OR.(MONTH.EQ.10).OR.(MONTH.EQ.12)) GOTO 10
      IF (DAY.GT.30) GOTO 30
      IF (MONTH.NE.2) GOTO 10
      IF (DAY.GT.29) GOTO 30
      IF ((DAY.EQ.29).AND.(MOD(YEAR,4).NE.0)) GOTO 30

C CHECK FOR VALID TIME
10    IF ((HOUR.GT.23).OR.(HOUR.LT.0)) GOTO 40
      IF ((MIN.GT.59).OR.(MIN.LT.0)) GOTO 40
      IF ((SEC.GT.59).OR.(SEC.LT.0)) GOTO 40
      IF ((MILLIS.GT.999).OR.(MILLIS.LT.0)) GOTO 40

C COMPUTE THE TRUNCATED JULIAN DATE
      TJDAY = DAY + 365.25*YEAR
      IF ((MOD(YEAR,4).EQ.0).AND.(MONTH.LE.2)) TJDAY=TJDAY-1
      IF (MONTH.GT.1) THEN
         DO 20 I=2,MONTH
            TJDAY = TJDAY + DAYS(I-1)
20       CONTINUE
      ENDIF
      TJDAY = TJDAY - 718956

C CONVERT TO MILLISECOND OF DAY
      MILSEC = (((HOUR*60)+MIN)*60+SEC)*1000+MILLIS

      RETURN

C SET THE CODE FOR INVALID DATE
30    CODE = 1
      RETURN

C SET THE CODE FOR INVALID TIME
40    CODE = 2
      RETURN

CCCCCCCCCCCCCCCCCCCCCCCCCCCCC END GRDTJD CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      END
