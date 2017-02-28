C $Log: main.cmn.inc,v $
C Revision 1.2  2013/05/21 19:08:24  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 1.1  1998/09/30 17:01:10  peachey
C New tool, delivered by Jeff Silvis
C
c Revision 2.4  1992/03/27  16:14:00  albert
c Added variables to save the total excluded time and the input file name.
c
c Revision 2.3  1992/01/02  21:05:24  albert
c Added variable to store user selection for requiring the 6 Mev in TASC
c
c Revision 2.2  1991/11/27  18:51:53  albert
c Changed the variable for the input bin size to be an array of 2
c elements in order to hold both the X and Y bin sizes.
c
c Revision 2.1  1991/08/28  19:36:16  esp
c First really controlled version on SUN (ported from IBM 3081)
c
C $Id: main.cmn.inc,v 1.2 2013/05/21 19:08:24 irby Exp $
C---> MAIN VARIABLES COMMON BLOCK
C**********************************************************
C
C     Ftool Change
C
C     The energy return flag option is no longer supported
C but I have not deleted the code in case someone in the 
C future wishes to revive the option.  ERFLAG, ERCLAS and
C ERBITS are no longer inputs to this code.
C
C     J. Silvis
C
C      LOGICAL       ERFLAG
C      CHARACTER     ERBITS(6)
C      INTEGER       ERCLAS
C
C
C     End of Ftool Change
C
C**********************************************************

      REAL          ZENMAX(20)
      REAL          SKYLIM(4),BINSIZ(2),CVTANG,TWOPI,PULVAL(4)
      REAL*8        USRTIM(2),EXTIME
      INTEGER       RETCOD,SYS,NUMSAV,NUMEXC,AXLIMT(2)
      INTEGER       NUMLEV,USTIME(9),UETIME(9),ENRGY1(20),ENRGY2(20)
      INTEGER	    DETMAX
      LOGICAL       ENFLAG(20),TESTPI,PULFLG
      LOGICAL       TSCFLG
      CHARACTER     CSYSTM*4,TIMFMT*4
      CHARACTER     FILNAM*80,EVFILE*80,INFILE*20
      character(64)  SAVEHD(150)

      COMMON /MAIN/ USRTIM,EXTIME,RETCOD,NUMLEV,SKYLIM,BINSIZ,USTIME,
     +              UETIME,ENRGY1,ENRGY2,CVTANG,TWOPI,ENFLAG,
     +              PULFLG,AXLIMT,ZENMAX,DETMAX,TSCFLG,
     +              PULVAL,SYS,TESTPI,
     +              NUMSAV,NUMEXC,CSYSTM,TIMFMT,FILNAM,EVFILE,
     +              INFILE,SAVEHD

