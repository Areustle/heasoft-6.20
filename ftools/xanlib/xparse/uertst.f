
C   USAGE               - CALL UERTST (IER,NAME)
C   ARGUMENTS    IER    - ERROR PARAMETER. (INPUT)
C                           IER = I+J WHERE
C                             I = 128 IMPLIES TERMINAL ERROR,
C                             I =  64 IMPLIES WARNING WITH FIX, AND
C                             I =  32 IMPLIES WARNING.
C                             J = ERROR CODE RELEVANT TO CALLING
C                                 ROUTINE.
C                NAME   - A SIX CHARACTER LITERAL STRING GIVING THE
C                           NAME OF THE CALLING ROUTINE. (INPUT)
C   PRECISION/HARDWARE  - SINGLE/ALL
C   REQD. IMSL ROUTINES - UGETIO
C   NOTATION            - INFORMATION ON SPECIAL NOTATION AND
C                           CONVENTIONS IS AVAILABLE IN THE MANUAL
C                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP
C   REMARKS      THE ERROR MESSAGE PRODUCED BY UERTST IS WRITTEN
C                ONTO THE STANDARD OUTPUT UNIT. THE OUTPUT UNIT
C                NUMBER CAN BE DETERMINED BY CALLING UGETIO AS
C                FOLLOWS..   CALL UGETIO(1,NIN,NOUT).
C                THE OUTPUT UNIT NUMBER CAN BE CHANGED BY CALLING
C                UGETIO AS FOLLOWS..
C                                NIN = 0
C                                NOUT = NEW OUTPUT UNIT NUMBER
C                                CALL UGETIO(3,NIN,NOUT)
C                SEE THE UGETIO DOCUMENT FOR MORE DETAILS.
C   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
C   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
C                           APPLIED TO THIS CODE. NO OTHER WARRANTY,
C                           EXPRESSED OR IMPLIED, IS APPLICABLE.
C-----------------------------------------------------------------------
      SUBROUTINE UERTST (IER,NAME)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER            IER
      INTEGER*2          NAME(3)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER*2          NAMSET(3),NAMEQ(3)
	integer*4 ieq, i, ieqdf, levold, level
      DATA               NAMSET/2HUE,2HRS,2HET/
      DATA               NAMEQ/2H  ,2H  ,2H  /
C                                  FIRST EXECUTABLE STATEMENT
      DATA               LEVEL/4/,IEQDF/0/,IEQ/1H=/
      IF (IER.GT.999) GO TO 25
      IF (IER.LT.-32) GO TO 55
      IF (IER.LE.128) GO TO 5
      IF (LEVEL.LT.1) GO TO 30
C                                  PRINT TERMINAL MESSAGE
      IF (IEQDF.EQ.1) WRITE(*,35) IER,NAMEQ,IEQ,NAME
      IF (IEQDF.EQ.0) WRITE(*,35) IER,NAME
      GO TO 30
    5 IF (IER.LE.64) GO TO 10
      IF (LEVEL.LT.2) GO TO 30
C                                  PRINT WARNING WITH FIX MESSAGE
      IF (IEQDF.EQ.1) WRITE(*,40) IER,NAMEQ,IEQ,NAME
      IF (IEQDF.EQ.0) WRITE(*,40) IER,NAME
      GO TO 30
   10 IF (IER.LE.32) GO TO 15
C                                  PRINT WARNING MESSAGE
      IF (LEVEL.LT.3) GO TO 30
      IF (IEQDF.EQ.1) WRITE(*,45) IER,NAMEQ,IEQ,NAME
      IF (IEQDF.EQ.0) WRITE(*,45) IER,NAME
      GO TO 30
   15 CONTINUE
C                                  CHECK FOR UERSET CALL
      DO 20 I=1,3
         IF (NAME(I).NE.NAMSET(I)) GO TO 25
   20 CONTINUE
      LEVOLD = LEVEL
      LEVEL = IER
      IER = LEVOLD
      IF (LEVEL.LT.0) LEVEL = 4
      IF (LEVEL.GT.4) LEVEL = 4
      GO TO 30
   25 CONTINUE
      IF (LEVEL.LT.4) GO TO 30
C                                  PRINT NON-DEFINED MESSAGE
      IF (IEQDF.EQ.1) WRITE(*,50) IER,NAMEQ,IEQ,NAME
      IF (IEQDF.EQ.0) WRITE(*,50) IER,NAME
   30 IEQDF = 0
      RETURN
   35 FORMAT(19H *** TERMINAL ERROR,10X,7H(IER = ,I3,
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)
   40 FORMAT(36H *** WARNING WITH FIX ERROR  (IER = ,I3,
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)
   45 FORMAT(18H *** WARNING ERROR,11X,7H(IER = ,I3,
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)
   50 FORMAT(20H *** UNDEFINED ERROR,9X,7H(IER = ,I5,
     1       20H) FROM IMSL ROUTINE ,3A2,A1,3A2)
C                                  SAVE P FOR P = R CASE
C                                    P IS THE PAGE NAME
C                                    R IS THE ROUTINE NAME
   55 IEQDF = 1
      DO 60 I=1,3
   60 NAMEQ(I) = NAME(I)
   65 RETURN
      END
