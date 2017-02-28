CCCCCCCCCCCCCCCCCCCCCCCC MAPGEN.SOURCE(RLTIME) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  ROUTINE NAME:  RLTIME
CH1
CH1  $Id: rltime.f,v 1.3 2013/05/21 19:08:24 irby Exp $
CH1
CH1  PROGRAMMER(S) AND COMPLETION DATE:
CH1     ALBERT ETIENNE - S.T.X. - 03/27/92
CH1
CH1  FUNCTION: CONVERTS THE STANDARD DATE AND TIME TO "REAL" DATE AND
CH1            TIMES.
CH1
CH1  SOFTWARE SYSTEM AND SPACECRAFT:  EGRET PROJECT
CH1
CH1  COMPUTER AND LANGUAGE:  IBM 3081 - VS FORTRAN
CH1
CH2  CALLING SEQUENCE:  CALL RLTIME(MON,DAY,YEAR,HR,MIN,SEC,MIL,RDATE,
CH2			RTIME)
CH2     ARGUMENT    TYPE   I/O                 DESCRIPTION
CH2     --------    ----   ---  ----------------------------------------
CH2	MON	    I*4     I   MONTH
CH2	DAY	    I*4	    I   DAY
CH2	YEAR	    I*4	    I   YEAR
CH2	HR	    I*4	    I   HOUR
CH2	MIN	    I*4     I   MINUTE
CH2	SEC	    I*4     I   SECOND
CH2	MIL	    I*4     I   MILLISECOND
CH2	RDATE	    R*4     O   DATE IN YEAR.DAY-OF-YEAR FORMAT
CH2	RTIME	    R*4     O   TIME IN SECOND-OF-DAY.MILLESECOND
CH2
CH2  CALLED BY:  WRTFTS
CH2
CH2  CALLS: NONE
CH2
CH3 COMMON USE: NONE
CH3
CH3 SIGNIFICANT LOCAL VARIABLES:  NONE
CH3   VARIABLE   TYPE   INI. VAL.               DESCRIPTION
CH3   --------   ----   ---------  -----------------------------------
CH3
CH4  LOGICAL UNITS USED:   UNIT #                DESCRIPTION
CH4                        ------    -----------------------------------
CH4                        NONE
CH4
CH4  METHOD:
CH4     COMPUTE THE TIME FROM THE HOUR, MINUTE,SECOND AND MILLISECOND
CH4	IF (THE YEAR IS A LEAP YEAR) THEN
CH4	   SET THE ACCUMULATED NUMBER OF DAYS FOR THE CURRENT MONTH
CH4     ELSE
CH4	   SET THE ACCUMULATED NUMBER OF DAYS FOR THE CURRENT MONTH
CH4     ENDIF
CH4     COMPUTE THE REAL DATE FROM THE YEAR AND THE DAYS
CH4  END RLTIME
CH4
CH5 $Log: rltime.f,v $
CH5 Revision 1.3  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.2  1998/10/06 12:52:23  silvis
CH5 A stop was removed from the main program so that the learn feature
CH5 for the par file would work properly and some minor changes were made
CH5 to the par file.
CH5
C Revision 1.1  1992/03/27  17:18:01  albert
C Initial revision
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE RLTIME(MON,DAY,YEAR,HR,MIN,SEC,MIL,RDATE,RTIME)
      implicit none 

       INTEGER MON,DAY,YEAR,HR,MIN,SEC,MIL
      REAL    RDATE,RTIME

      character(80)	id
      common	/id/	id
      id = '$Id: rltime.f,v 1.3 2013/05/21 19:08:24 irby Exp $'

C---> COMPUTE THE SECOND OF DAY 
      RTIME = MIL/1000.0 + SEC + MIN*60 + HR*3600

      IF (MOD(YEAR,4) .EQ. 0) THEN
	 IF (MON .EQ. 1) THEN
	    RDATE = 0
	 ELSE IF (MON .EQ. 2) THEN
	    RDATE = 31
	 ELSE IF (MON .EQ. 3) THEN
	    RDATE = 60
	 ELSE IF (MON .EQ. 4) THEN
	    RDATE = 91
	 ELSE IF (MON .EQ. 5) THEN
	    RDATE = 121
	 ELSE IF (MON .EQ. 6) THEN
	    RDATE = 152
	 ELSE IF (MON .EQ. 7) THEN
	    RDATE = 182
	 ELSE IF (MON .EQ. 8) THEN
	    RDATE = 213
	 ELSE IF (MON .EQ. 9) THEN
	    RDATE = 244
	 ELSE IF (MON .EQ. 10) THEN
	    RDATE = 274
	 ELSE IF (MON .EQ. 11) THEN
	    RDATE = 305
	 ELSE IF (MON .EQ. 12) THEN
	    RDATE = 335
	 END IF

      ELSE
	 IF (MON .EQ. 1) THEN
	    RDATE = 0
	 ELSE IF (MON .EQ. 2) THEN
	    RDATE = 31
	 ELSE IF (MON .EQ. 3) THEN
	    RDATE = 59
	 ELSE IF (MON .EQ. 4) THEN
	    RDATE = 90
	 ELSE IF (MON .EQ. 5) THEN
	    RDATE = 120
	 ELSE IF (MON .EQ. 6) THEN
	    RDATE = 151
	 ELSE IF (MON .EQ. 7) THEN
	    RDATE = 181
	 ELSE IF (MON .EQ. 8) THEN
	    RDATE = 212
	 ELSE IF (MON .EQ. 9) THEN
	    RDATE = 243
	 ELSE IF (MON .EQ. 10) THEN
	    RDATE = 273
	 ELSE IF (MON .EQ. 11) THEN
	    RDATE = 304
	 ELSE IF (MON .EQ. 12) THEN
	    RDATE = 334
	 END IF
      END IF

      RDATE = YEAR + (RDATE+DAY)/1000.0

      RETURN
CHCCCCCCCCCCCCCCCCCCCCC END MAPGEN.SOURCE(RLTIME) CCCCCCCCCCCCCCCCCCCCCC
      END