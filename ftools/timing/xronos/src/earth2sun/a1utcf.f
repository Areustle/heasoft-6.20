      FUNCTION A1UTCF(Jdutc)
C
**==A1UTCF.spg  processed by SPAG 3.09I  at 11:06 on 12 Jan 1994
C
CC  Compute difference between atomic time and UTC (> 1-jan-1972)

c This routine is taken from MIDAS without modification.

c It is called by subroutine readeph.

C
C************************ FFORM VERSION 3.0 ********** 31-JAN-90 09:20
C
CA  author : Lloyd Rawley-STX date: 14-JUL-1989
CU  update : TMB              date: 25-SEP-1990 11:05 EXSAS version
CU  update : TMB              date: 28-JAN-1992 16:32 1991 leap sec.
CU  update : TMB              date: 11-MAR-1992 16:09 GETOUT=.F.
CU  update : TMB              date: 21-OCT-1992 13:51 1992 leap sec.
C
CT  status: not tested
C
C   general description:
CG  This function computes the difference between atomic time (A1)
CG   and UTC for any date since the beginning of 1972.
C
C   call_var.          type I/O description
CP  JDUTC               I   I   UTC JD number (changes at 0h UT)
C
C***********************************************************************
C
C   variables   meaning
C
C    NLEAPS           I*4   (parameter) Number of leap seconds in UTC,
C                            from 1972.
C    A1UTC            R*8   A1-UTC offset at the begining of 1972
C    JDLEAP(NLEAPS)   I*4   array with dates of leap seconds (at 0h UT):
C                               1    2441500    1-jul-1972
C                               2    2441684    1-jan-1973
C                               3    2442049    1-jan-1974
C                               4    2442414    1-jan-1975
C                               5    2442779    1-jan-1976
C                               6    2443145    1-jan-1977
C                               7    2443510    1-jan-1978
C                               8    2443875    1-jan-1979
C                               9    2444240    1-jan-1980
C                              10    2444787    1-jul-1981
C                              11    2445152    1-jul-1982
C                              12    2445517    1-jul-1983
C                              13    2446248    1-jul-1985
C                              14    2447162    1-jan-1988
C                              15    2447893    1-jan-1990
C                              16    2448258    1-jan-1991
C                              17    2448805    1-jul-1992
C                              18    2449170    1-jul-1993
C                              19    2449534    1-jul-1994
C                              20    2450084    1-jan-1996
C                              21    2450631    1-jul-1997
C                              22    2451180    1-jan-1999
C                           (original version updated to 1988,
C                             EXSAS version updated to 1992)
C    A1UTC            R*8   A1-UTC offset at the begining of 1972
C    JDUTC            I*4   Time difference A1-UTC (in seconds)
C    I                I*4   loop index
C    GETOUT           L*4   flag for getting out of the loop
C
      IMPLICIT NONE
      REAL*8 A1UTC
      PARAMETER (A1UTC=10.0343817D0)
      INTEGER*4 NLEAPS
C
      PARAMETER (NLEAPS=22)
      REAL*8 A1UTCF
      INTEGER*4 jdleap(NLEAPS)
      INTEGER*4 Jdutc
      INTEGER*4 i
      character(8) rtname
C
      LOGICAL*4 getout
C
      DATA jdleap/2441500 , 2441684 , 2442049 , 2442414 , 2442779 , 
     &     2443145 , 2443510 , 2443875 , 2444240 , 2444787 , 2445152 , 
     &     2445517 , 2446248 , 2447162 , 2447893 , 2448258 , 2448805 ,
     &     2449170 , 2449535 , 2450084, 2450631, 2451180/
      DATA getout/.FALSE./
C
      DATA rtname/'A1UTCF'/
C
      SAVE 

c  Initialize to avoid warning
      A1UTCF = 0.d0
c  --

C     Initialize logical switch
C------------------------------
      getout = .FALSE.
C-------------------------
C     Loop over leap years
C-------------------------
      DO 100 i = NLEAPS , 1 , -1
         IF ( (Jdutc.GE.jdleap(i)) .AND. (.NOT.getout) ) THEN
            A1UTCF = A1UTC + i
            getout = .TRUE.
         ENDIF
 100  CONTINUE
      IF ( .NOT.getout ) THEN
         A1UTCF = A1UTC
      ENDIF
C
      RETURN
      END
