CCCCCCCCCCCCCCCCCCCCCCCC MAPGEN.SOURCE(INITLZ) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  ROUTINE NAME:  INITLZ
CH1
CH1  VERSION: 1.04              REVISION DATE: 05/21/91
CH1  $Id: initlz.f,v 1.6 2013/05/21 19:08:24 irby Exp $
CH1
CH1  PROGRAMMER(S) AND COMPLETION DATE:
CH1     ALBERT ETIENNE - S.T.X. - 02/13/89
CH1     E.S.Panduranga - S.T.X. - 05/21/91 
CH1
CH1  FUNCTION: PERFORMS INITIAL PROCESSINGS FOR THE PROGRAM. READS,
CH1            ECHOES, TESTS AND CONVERTS THE USER INPUT AND FINDS THE
CH1            FIRST INPUT FILE.
CH1
CH1  SOFTWARE SYSTEM AND SPACECRAFT:  EGRET PROJECT
CH1
CH1  COMPUTER AND LANGUAGE:  IBM 3081 - VS FORTRAN
CH1
CH2  CALLING SEQUENCE:  CALL INITLZ
CH2     ARGUMENT    TYPE   I/O                 DESCRIPTION
CH2     --------    ----   ---  ----------------------------------------
CH2
CH2  CALLED BY:  MAPGEN
CH2
CH2  CALLS:
CH2   ZTIME : SYSTEM ROUTINE TO GET THE CURRENT TIME
CH2   GRDTJD: CONVERTS GREGORIAN DATES TO TRUNCATED JULIAN DATES
CH2   TJDGRD: CONVERTS TRUNCATED JULIAN DATES TO GREGORIAN DATES
CH2
CH3 COMMON USE:
CH3 COMMON BLOCK NAME: MAIN          (HOLDS THE MAIN VARIABLES)
CH3 VARIABLE     TYPE                        DEFINITION
CH3   NAME
CH3 --------   ---------   ---------------------------------------------
CH3 USRTIM(2)  REAL*8      COMBINED TJD/MSD USER TIME (1:START, 2:END)
CH3 RETCOD     INTEGER     PROGRAM RETURN CODE (0=NORMAL)
CH3 NUMLEV     INTEGER     NUMBER OF ENERGY LEVELS SELECTED BY THE USER
CH3 SKYLIM(4)  REAL        MAP BOUNDARIES SELECTED BY THE USER
CH3 BINSIZ(2)  REAL        BIN SIZE SELECTED BY THE USER (1:X, 2:Y)
CH3 USTIME(8)  INTEGER     START TIME (TJD,MSD,MONTH,DAY,YEAR,HR,MIN,SC)
CH3 UETIME(8)  INTEGER     END TIME (TJD,MSD,MONTH,DAY,YEAR,HR,MIN,SEC)
CH3 ENRGY1(20) INTEGER     MINIMUM VALUE FOR EACH OF THE ENERGY LEVELS
CH3 ENRGY2(20) INTEGER     MAXIMUM VALUE FOR EACH OF THE ENERGY LEVELS
CH3 CVTANG     REAL        FACTOR TO CONVERT DEGREES TO RADIANS
CH3 TWOPI      REAL        VALUE OF 2 PI IN RADIANS
CH3 PULFLG     LOGICAL     DETERMINES IF PULSAR PARAMETERS MUST BE USED
CH3 AXLIMT(2)  INTEGER     MAXIMUM SIZE OF BIN DATA ARRAY IN 1ST 2 DIM.
CH3 PULVAL(4)  REAL*4      PULSAR PARAMETERS LIMITS
CH3 SYS        INTEGER     INDEX FOR THE COORDINATE SYSTEM TO USE
CH3 TESTPI     LOGICAL     DETERMINES IF PI IS BETWEEN THE USER LIMITS
CH3 NUMSAV     INTEGER     NUMBER OF FITS HEADER LINES SAVED
CH3 NUMEXC     INTEGER     TOTAL NUMBER OF EXCLUDED EVENTS
CH3 CSYSTM     CH*4        COORDINATE SYSTEM TO USE (GAL,CEL,INST,EARTH)
CH3 TIMFMT     CH*8        TIME FORMAT (CAL=CALENDAR, TJD=TJD/MSD)
CH3 FILNAM     CH*8        NAME EXTENSION OF THE OUTPUT FITS FILE
CH3 EVFILE     CH*24       NAME OF THE EVENT INPUT FILE
CH3 SAVEHD(150)CH*64       HEADER LINES SAVED
CH3 ERBITS(6)  CH*1        ENERGY RETURN CODE BITS ('0', '1' OR 'X')
CH3
CH3 COMMON BLOCK NAME: FITSDT        (HOLDS THE FITS HEADER VALUES)
CH3 VARIABLE     TYPE                        DEFINITION
CH3   NAME
CH3 --------   ---------   ---------------------------------------------
CH3 BITPIX     INTEGER*4   NUMBER OF BITS PER PIXELS
CH3 NAXIS      INTEGER*4   NUMBER OF AXIS
CH3 NAXIS1     INTEGER*4   NUMBER OF BINS ON THE FIRST AXIS
CH3 NAXIS2     INTEGER*4   NUMBER OF BINS ON THE SECOND AXIS
CH3 NAXIS3     INTEGER*4   NUMBER OF BINS ON THE THIRD AXIS
CH3 NAXIS4     INTEGER*4   NUMBER OF BINS ON THE FOURTH AXIS
CH3 CRVAL1     REAL*4      COORDINATE OF REFERENCE POINT ON AXIS 1
CH3 CRPIX1     REAL*4      ARRAY INDEX OF REFERENCE POINT ON AXIS 1
CH3 CDELT1     REAL*4      INCREMENT OF COORDINATE ALONG AXIS 1
CH3 CTYPE1     CHAR*4      TYPE OF COORDINATE ON AXIS 1
CH3 CRVAL2     REAL*4      COORDINATE OF REFERENCE POINT ON AXIS 1
CH3 CRPIX2     REAL*4      ARRAY INDEX OF REFERENCE POINT ON AXIS 2
CH3 CDELT2     REAL*4      INCREMENT OF COORDINATE ALONG AXIS 2
CH3 CTYPE2     CHAR*4      TYPE OF COORDINATE ON AXIS 2
CH3 CRVAL3     REAL*4      COORDINATE OF REFERENCE POINT ON AXIS 3
CH3 CRPIX3     REAL*4      ARRAY INDEX OF REFERENCE POINT ON AXIS 3
CH3 CDELT3     REAL*4      INCREMENT OF COORDINATE ALONG AXIS 3
CH3 CTYPE3     CHAR*4      TYPE OF COORDINATE ON AXIS 3
CH3 BUNIT      CHAR*12     DATA UNITS
CH3 BSCALE     REAL*4      SCALE FACTOR TO CONVERT THE BIN DATA
CH3 BZERO      REAL*4      OFFSET TO CONVERT THE BIN DATA
CH3 PCOUNT     INTEGER*4   NUMBER OF PARAMETERS
CH3 GCOUNT     INTEGER*4   NUMBER OF GROUPS
CH3 MAPSIZ     INTEGER*4   NUMBER OF VALUES IN THE MAP (INCLUDING PARMS)
CH3 DATIME(4)  INTEGER*4   DATA START AND END TIMES
CH3 PDIREC(2)  REAL*4      INSTRUMENT POINTING DIRECTION
CH3 FTPARM     REAL*4      5 PARAMETERS (200 GROUPS). INDEX 1 TO 5 ARE:
CH3   (5,200)              1:NUMBER BINS IN GROUP, 2:POSITION ON AXIS1,
CH3                        3:POSITION ON AXIS2, 4:INCREMENT ON AXIS1,
CH3                        5:INCREMENT ON AXIS 2
CH3 BINDAT     INTEGER*2   BIN DATA (200 X 200 X 20 ENERGY LEVELS)
CH3 (200,200,20)
CH3 FITSRC     CHAR*2880   FITS RECORD
CH3 SIMPLE     LOGICAL     DETERMINES IS THE SIMPLE FITS FORMAT IS USED
CH3 GROUPS     LOGICAL     DETERMINES IF GROUPS ARE USED
CH3 STRTIM     CHAR*21     START TIME IN CHARACTERS
CH3 ENDTIM     CHAR*21     END TIME IN CHARACTERS
CH3
CH3 SIGNIFICANT LOCAL VARIABLES:  N.A.
CH3   VARIABLE   TYPE   INI. VAL.               DESCRIPTION
CH3   --------   ----   ---------  -----------------------------------
CH3   IERR       I*4       -       RETRUN CODE FROM TIME CONVERSION
CH3   TODAY      CH*16     -       CURRENT DATE IN CHARACTERS
CH3
CH4  LOGICAL UNITS USED:   UNIT #                DESCRIPTION
CH4                        ------    -----------------------------------
CH4                          1       EVENT FILE
CH4                          5       USER INPUT
CH4                          6       PROGRAM REPORT ON PRINTER
CH4
CH4  METHOD:
CH4     PRINT THE PROGRAM TITLE
CH4     READ THE USER INPUT
CH4     ECHO THE USER INPUT ON THE PRINTER
CH4     CONVERT THE ANGLES IN THE USER INPUT TO RADIANS
CH4     CONVERT THE TIMES TO BOTH GREGORIAN AND TJD/MSD FORMATS
CH4     TEST THAT THE COORDINATE SYSTEM IS VALID
CH4     TEST THAT THE TIME RANGE IS VALID
CH4     INITIALIZE THE BIN DATA ARRAY TO ALL 0'S
CH4  END INITLZ
CH4
CH5  MODIFICATIONS BETWEEN VERSIONS:
CH5     MOD #   MODIFIER    DATE                  DESCRIPTION
CH5     -----   --------  --------   -----------------------------------
CH5     1.01    A.ETIENNE 01/29/90   ADDED VARIABLES FOR THE ENERGY
CH5                                  RETURN CODE, THE PULSAR PARMS,
CH5                                  THE NAME OF THE FIRST EVENT FILE &
CH5                                  THE NUMBER OF EVENT FILES. PRINTED
CH5                                  THE NEW VARIABLES VALUES. ALLOWED
CH5                                  NEW ARRAY BOUNDARIES FOR THE GALAC
CH5                                  DISK MAPS. REMOVED CODE THAT
CH5                                  SEARCHED THE EGRET DB DATABASE.
CH5                                  UPDATED THE DATE AND VERSION #.
CH5     1.02    A.ETIENNE 03/21/91   UPDATED DATE AND VERSION NUMBER
CH5                                  BECAUSE OF CHANGE IN BINEVT
CH5     1.03    A.ETIENNE 04/12/91   ADDED CODE TO PRINT THE ZENITH
CH5                                  ANGLE FOR EACH ENERGY LEVEL FROM
CH5                                  THE USER INPUT. UPDATED DATE AND
CH5                                  VERSION NUMBER IN TITLE.
CH5     2.00    E.S.Panduranga 04/21/91 Changed ZTIME(IBM) to fdate(f77)
CH5				Changed TODAY from character(16) to *24
CH5				Changed version and date on output.
CH5				Opened name list file.
CH5				Merged changes version 1.07 on IBM.
CH5				ALLOWED FOR THE EARTH CUTOFF ANGLES
CH5                             TO BE EITHER READ FROM THE USER OR
CH5                             COMPUTED WITH A SELECTED VALUE OF
CH5                             SIGMA. ALLOWED FOR USER INPUT OF
CH5                             THE MAXIMUM ANGLE FORM THE DETECTOR
CH5                             AXIS. USED NEW EARTH CUTOFF ANGLES COMPUTATION.
CH5 $Log: initlz.f,v $
CH5 Revision 1.6  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.5  2006/08/16 16:35:04  irby
CH5 Remove ugly rcsid echo from program startup.
CH5
CH5 Revision 1.4  2002/12/26 17:16:25  irby
CH5 Fix variable declarations for f90 compatibility, e.g.:
CH5       INTEGER         EVTTJD*2
CH5 is properly declared as:
CH5       INTEGER*2       EVTTJD
CH5
CH5 Revision 1.3  1998/11/18 15:36:28  silvis
CH5 Allow program to write files with just one energy level.
CH5
CH5 Have program check for local version of the timeline file.
CH5 If no local version of the timeline files exists then use the
CH5 copy in greo DATADIR directory.
CH5
CH5 Revision 1.2  1998/10/06 12:52:22  silvis
CH5 A stop was removed from the main program so that the learn feature
CH5 for the par file would work properly and some minor changes were made
CH5 to the par file.
CH5
C Revision 2.7  1997/10/09  13:57:01  wang
C *** empty log message ***
C
C Revision 2.6  1994/02/22  20:48:00  albert
C Modified sigma to be a real number and used new formula to compute
C atmospheric cutoff angles.
C
C Revision 2.5  1993/05/05  12:23:47  albert
C Cosmetic changes in printing program title.
C
C Revision 2.4  1992/03/27  16:09:44  albert
C Used variable dimension arrays for the maps. Allowed first sky limit to =
C last sky limit (eg. 0-360). Removed processing of the map update case
C since done with ADDMAP.
C
C Revision 2.3  1992/01/02  21:03:20  albert
C Added variable for selecting events with 6 Mev in TASC. Printed the user
C selection for this option.
C
C Revision 2.2  1991/11/27  18:51:14  albert
C Changed the variable for the input bin size to be an array of 2
C elements in order to hold both the X and Y bin sizes.
C
C Revision 2.1  1991/08/28  19:36:16  esp
C First really controlled version on SUN (ported from IBM 3081)
C
C**********************************************************
C
C     Ftool Change
C
C  1   The update option was dropped and the code removed.  The
C   update option is handled by eaddmap.
C  2. All output to the screen is now done through 
C     the platform independent function fcecho.
C
C  
C
C
C     J. Silvis
C     RSTX
C     Sept 1998
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE INITLZ
      implicit none 

       REAL*8    CONVRT,ALPHA
       REAL      sigma
C      
C      The character variable below has been added 
C      so that the code can use the fitsio routine 
C      fcecho. 
C      
C      Jeff Silvis 
C      RSTX 
C      21  July  98
 
      character(250) FCECHO_STR 
C 
C     The variables below were added so that the Xanadu Parameter 
C   Interface (ftool interface) could read in the energies for each 
C   level and the zenith angles.
C
      INTEGER WHICH_LEV
      CHARACTER  XPI_QUERY*12,STR_INT*2       
C
C
C
 
      INTEGER   I,IERR
C*************************************************************************
C
C       Ftool change
C
C    Fitsio is now used to find the date.  So the character today
C    has been replaced with integer day,month,year
C
C      character TODAY*24
C
C  JS
C*************************************************************************
      integer day,month,year,status
      INCLUDE 'main.cmn.inc'
      INCLUDE 'fitsdt.cmn.inc'

C**********************************************************
C
C     Ftool Change
C
C     The input variables were originally brought in through
C the namelist file.  They are not brought into the code 
C through XPI.
C
C
C     J. Silvis
C
C    August 1998
C
C
C      NAMELIST /USERIN/ NUMLEV,SKYLIM,BINSIZ,USTIME,UETIME,ENRGY1,
C     &                  ENRGY2,MAPTYP,GRIDTP,CSYSTM,TIMFMT,DETMAX,
C     &                  DMPFLG,PULVAL,ERFLAG,ERCLAS,PULFLG,FILNAM,
C     &                  ZENMAX,MAXFIL,SIGMA,EVFILE,ERBITS,
C     &                  TSCFLG

      character(80)	id
      common	/id/	id
      id = '$Id: initlz.f,v 1.6 2013/05/21 19:08:24 irby Exp $'
      RETCOD = 0
      TWOPI  = 8.0 * ATAN (1.0)
      CVTANG = TWOPI / 360.0
      NUMSAV = 0
      EXTIME = 0.0

C---> PRINT THE PROGRAM TITLE
C***************************************************************************
C    Ftools change
C    fdate is sun dependent and must be replaced with fitsio.
C
C    Write the program title
C      call fdate(today)
C      write(lunout,'(26x,'' A D D M A P   P R O G R A M'',//,28x,a)')
C     &   today
C**************************************************************************
      WRITE(FCECHO_STR,'(24x,'' E M A P G E N   P R O G R A M'')') 
      call FCECHO(FCECHO_STR) 
      call FCECHO("  ")
      call FCECHO("  ")
      call ftgsdt(day,month,year,status)
      write(FCECHO_STR,1011) day,month,year
 1011 format (i2,'/',i2,'/',i4)
      call FCECHO(FCECHO_STR) 
      call FCECHO("  ")
      call FCECHO("  ")

C---> READ THE USER INPUT
Cesp  ! First open the input file !
C
C**********************************************************
C
C     Ftool Change
C
C     The namelist file is no longer used the data are 
C  now brought in through XPI.
C
C     J. Silvis
C
C
C      open(unit=5, file='namelist.map')
C      READ(5,USERIN)
C
C     The input will now come from the parameter file.  
C The functions below link to the parameter file and 
C use the information in that file for input.
C
C     J. Silvis
C
      status = 0
      call uclgst('evfile',EVFILE,status)
      call uclgst('filnam',FILNAM,status)
      call uclgst('csystm',CSYSTM,status)

      call uclgsr('xbinsiz',BINSIZ(1),status)
      call uclgsr('ybinsiz',BINSIZ(2),status)
      call uclgsb('tscflg',TSCFLG,status)
      call uclgsr('albcut',SIGMA,status)   


      call uclgsr('lonmin',SKYLIM(1),status)
      call uclgsr('lonmax',SKYLIM(2),status)
      call uclgsr('latmin',SKYLIM(3),status)
      call uclgsr('latmax',SKYLIM(4),status)

      call uclgst('timfmt',TIMFMT,status)

      if (timfmt .eq. 'TJD') then
            call uclgsi('stjd',USTIME(1),status)
            call uclgsi('etjd',UETIME(1),status)
      else
            call uclgsi('smsd',USTIME(2),status)
            call uclgsi('smonth',USTIME(3),status)                    
            call uclgsi('sday',USTIME(4),status)
            call uclgsi('syr',USTIME(5),status)
            call uclgsi('shr',USTIME(6),status)
            call uclgsi('smin',USTIME(7),status)
            call uclgsi('ssec',USTIME(8),status)

            call uclgsi('emsd',UETIME(2),status)
            call uclgsi('emonth',UETIME(3),status)                    
            call uclgsi('eday',UETIME(4),status)
            call uclgsi('eyr',UETIME(5),status)
            call uclgsi('ehr',UETIME(6),status)
            call uclgsi('emin',UETIME(7),status)
            call uclgsi('esec',UETIME(8),status)
      endif

      call uclgsi('detmax',DETMAX,status)     

      call uclgsb('pulflg',PULFLG,status)
      if (PULFLG) then
            call uclgsr('lpulphase',PULVAL(1),status)
            call uclgsr('upulphase',PULVAL(2),status)
            call uclgsr('lbinphase',PULVAL(3),status)
            call uclgsr('ubinphase',PULVAL(4),status)
      endif


      call uclgsi('numlev',NUMLEV,status)
      do 116  WHICH_LEV=1, NUMLEV

            if (which_lev .lt. 10) then
                 write(str_int,114) which_lev
            else
                 write(str_int,115) which_lev 
            endif

            XPI_QUERY = 'eng1lv' // str_int
            call uclgsi(XPI_QUERY,ENRGY1(WHICH_LEV),status)

            XPI_QUERY = 'eng2lv' // str_int
            call uclgsi(XPI_QUERY,ENRGY2(WHICH_LEV),status)

            if (sigma .eq. 0.0) then
                XPI_QUERY = 'zenmax' // str_int
                call uclgsr(XPI_QUERY,ZENMAX(WHICH_LEV),status)
            endif

 114        FORMAT (I1)
 115        FORMAT (I2)
 116    continue       

C
C     End of Ftool Change
C
C**********************************************************




C---> WRITE THE USER INPUT
      call FCECHO("  ")
      call FCECHO("  ")
      call FCECHO("  ")
      call FCECHO("  ")
      call FCECHO("  ")
      call FCECHO("  ")
      WRITE(FCECHO_STR,'('' USER INPUT'')') 
      call FCECHO(FCECHO_STR) 
      call FCECHO("  ")
      WRITE(FCECHO_STR,'('' ----------'')') 
      call FCECHO(FCECHO_STR) 
      call FCECHO(FCECHO_STR) 

      WRITE(FCECHO_STR,'(3X,''COORDINATE SYSTEM ............... = '',
     &  A4)') CSYSTM
      call FCECHO(FCECHO_STR) 
      WRITE(FCECHO_STR,'(3X,'' EVENT INPUT FILE NAME ..... = '',
     &  A)') EVFILE
      call FCECHO(FCECHO_STR) 
      call FCECHO(FCECHO_STR) 
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
C      IF (ERFLAG) THEN
C       WRITE(FCECHO_STR,'(3X,''ENERGY RET CODE (CLASS, BITS2-7)  = '',
C     &   I1,6A2)')  ERCLAS,(ERBITS(I),I=1,6)
C       call FCECHO(FCECHO_STR) 
C       ENDIF 
C
C     End of Ftool Change
C
C**********************************************************

      IF (PULFLG) THEN
          WRITE(FCECHO_STR,'(3X,''PULSAR PHASE LIMITS ............. = 
     & '',2E11.3)')  PULVAL(1), PULVAL(2)
          call FCECHO(FCECHO_STR) 
          WRITE(FCECHO_STR,'(3X,''PULSAR BINARY LIMITS ............ = 
     & '',2E11.3)')  PULVAL(3), PULVAL(4)
          call FCECHO(FCECHO_STR) 
      END IF
Cesp  WRITE(6,'(3X,''OUTPUT FITS FILE NAME ........... = '',
Cesp &   ''SKYMAP.COUNTS.'',A8)') FILNAM
      WRITE(FCECHO_STR,'(3X,''OUTPUT FITS FILE NAME ........... = '',
     &  A)')  FILNAM
      call FCECHO(FCECHO_STR) 
C---> WRITE THE RECTANGULAR BOUNDARIES. PUT EVERYTHING IN (-180, 180)
         IF (SKYLIM(1).GT.180.0) SKYLIM(1) = SKYLIM(1) - 360.0
         IF (SKYLIM(2).GT.180.0) SKYLIM(2) = SKYLIM(2) - 360.0
         TESTPI = .FALSE.
         IF (SKYLIM(1).GE.SKYLIM(2)) TESTPI = .TRUE.
         WRITE(FCECHO_STR,'(3X,''HORIZONTHAL/LONGITUDE COORDINATES =''
     &     ,2(F9.2))') SKYLIM(1),SKYLIM(2)
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,'(3X,''VERTICAL/LATITUDE COORDINATES ... =''
     &     ,2(F9.2))') SKYLIM(3),SKYLIM(4)
         call FCECHO(FCECHO_STR) 
         IF (SKYLIM(3).EQ.SKYLIM(4)) THEN
            call FCECHO("  ")
            WRITE(FCECHO_STR,'('' ERROR: MAP BOUNDARIES INVALID'')') 
            call FCECHO(FCECHO_STR) 
            GOTO 1010
         ENDIF



C---> WRITE THE BIN SIZE SELECTED BY THE USER
      WRITE(FCECHO_STR,'(3X,''HORIZONTHAL/LONGITUDE BIN SIZE .. ='',
     &  F9.2)') BINSIZ(1)
      call FCECHO(FCECHO_STR) 
      WRITE(FCECHO_STR,'(3X,''VERTICAL/LATITUDE BIN SIZE ...... ='',
     &  F9.2)') 
      call FCECHO(FCECHO_STR) 
      call FCECHO("  ")

C---> WRITE THE ENERGY LEVELS AND THE MAXIMUM ANGLES FROM THE ZENITH
      WRITE(FCECHO_STR,'(3X,''6 MEV REQUIRED IN THE TASC ...... ='',
     &  L6)')  TSCFLG
      call FCECHO(FCECHO_STR) 
      WRITE(FCECHO_STR,'(3X,''MAXIMUM ANGLE FROM DETECTOR AXIS  ='',
     &  I6)')  DETMAX
      call FCECHO(FCECHO_STR) 
      IF (SIGMA .EQ. 0.0) THEN
         WRITE(FCECHO_STR,'(3X,''CUTOFF ANGLES READ FROM USER INPUT 
     & '')') 
         call FCECHO(FCECHO_STR) 
         call FCECHO("  ")
      ELSE
         WRITE(FCECHO_STR,'(3X,''CUTOFF ANGLES COMPUTED WITH SIGMA =''
     &     ,f6.1)') 
         call FCECHO(FCECHO_STR) 
         call FCECHO("  ")
      ENDIF
      DO 10 I=1,NUMLEV
         IF (SIGMA .NE. 0.0) THEN
C           ALPHA = 0.299 +113.4278*ENRGY1(I)**(-.86887)
	    alpha = 5.85 * (ENRGY1(I)/100.0)**(-0.534)
            ZENMAX(I) = 110.0 - SIGMA*ALPHA
            ZENMAX(I) = MIN(105.0,ZENMAX(I) )
            ZENMAX(I) = MAX(3.0,ZENMAX(I) )
C            ZENMAX(I) = MAX(3.0, MIN(105.0, 110.0 - SIGMA*ALPHA))
         END IF
         WRITE(FCECHO_STR,'(3X,''ENERGY RANGE'',I3,'' = '',I5,''-'',I5
     &     ,5X,''MAXANGLE ='',F12.4)')  I,ENRGY1(I),ENRGY2(I),
     &     ZENMAX(I)
         call FCECHO(FCECHO_STR) 
         ZENMAX(I) = ZENMAX(I) * CVTANG
10    CONTINUE

C---> CONVERT THE SKY BOUNDARIES ANGLES FROM DEGREES TO RADIANS
      DO 15 I=1,4
         SKYLIM(I) = SKYLIM(I) * CVTANG
15    CONTINUE
      BINSIZ(1) = BINSIZ(1) * CVTANG
      BINSIZ(2) = BINSIZ(2) * CVTANG

C---> CHECK IF CALENDAR TIME FORMAT HAS BEEN SELECTED
      IF (TIMFMT.EQ.'CAL') THEN
         if (USTIME(5) .le. 65) USTIME(5) = USTIME(5) + 100
         if (UETIME(5) .le. 65) UETIME(5) = UETIME(5) + 100
         USTIME(5) = USTIME(5) + 1900
         UETIME(5) = UETIME(5) + 1900

C------> CONVERT THE TIMES TO CALENDAR DATE/TIME
         CALL GRDTJD(USTIME(1),USTIME(2),USTIME(5),USTIME(3),
     +   USTIME(4),USTIME(6),USTIME(7),USTIME(8),USTIME(9),IERR)
         IF (IERR.NE.0) GOTO 1000
         CALL GRDTJD(UETIME(1),UETIME(2),UETIME(5),UETIME(3),
     +   UETIME(4),UETIME(6),UETIME(7),UETIME(8),UETIME(9),IERR)
         IF (IERR.NE.0) GOTO 1000

C------> ECHO THE TIME RANGE
         call FCECHO("  ")
         WRITE(FCECHO_STR,'(3X,''TIME RANGE SELECTED:'')') 
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,11000) 
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,11001) 
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,21000)  USTIME(3),USTIME(4),MOD(USTIME(5),
     &    100),USTIME(6),USTIME(7),USTIME(8),USTIME(1),USTIME(2)
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,21000)  UETIME(3),UETIME(4),MOD(UETIME(5),
     &    100),UETIME(6),UETIME(7),UETIME(8),UETIME(1),UETIME(2)
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,31000) 
         call FCECHO(FCECHO_STR) 

C---> ELSE THE TIME FORMAT IS TRUNCATED JULIAN DAY/MILLISECOND
      ELSE IF (TIMFMT.EQ.'TJD') THEN

C------> CONVERT THE TIMES TO CALENDAR DATE/TIME
         CALL TJDGRD(USTIME(1),USTIME(2),USTIME(5),USTIME(3),
     +   USTIME(4),USTIME(6),USTIME(7),USTIME(8),USTIME(9),IERR)
         IF (IERR.NE.0) GOTO 1000
         CALL TJDGRD(UETIME(1),UETIME(2),UETIME(5),UETIME(3),
     +   UETIME(4),UETIME(6),UETIME(7),UETIME(8),UETIME(9),IERR)
         IF (IERR.NE.0) GOTO 1000

C------> ECHO THE TIME RANGE
         call FCECHO("  ")
         WRITE(FCECHO_STR,'(3X,''TIME RANGE SELECTED:'')') 
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,10000) 
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,10001) 
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,20000)  USTIME(1),USTIME(2),USTIME(3),
     &    USTIME(4),MOD(USTIME(5),100),USTIME(6),USTIME(7),USTIME(8)
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,20000)  UETIME(1),UETIME(2),UETIME(3),
     &    UETIME(4),MOD(UETIME(5),100),UETIME(6),UETIME(7),UETIME(8)
         call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,30000) 
         call FCECHO(FCECHO_STR) 
      ENDIF

C---> TEST THE COORDINATE SYSTEM AND SAVE THE CORRESPONDING INDEX
      IF (CSYSTM.EQ.'GALA') THEN
         CTYPE1 = 'GLON'
         CTYPE2 = 'GLAT'
         SYS    = 7
      ELSE IF (CSYSTM.EQ.'CELE') THEN
         CTYPE1 = 'RA  '
         CTYPE2 = 'DEC '
         SYS    = 5
      ELSE IF (CSYSTM.EQ.'INST') THEN
         SYS    = 1
         CTYPE1 = 'TETX'
         CTYPE2 = 'TETY'
      ELSE IF (CSYSTM.EQ.'ERTH') THEN
         CTYPE1 = 'LATD'
         CTYPE2 = 'LONG'
         SYS    = 3
      ENDIF

C---> INITIALIZE THE BIN DATA ARRAY
      DO I=1,MAXBIN
         BINDAT(I) = 0
      END DO

C---> TEST FOR A VALID TIME RANGE
      USRTIM(1) = CONVRT(USTIME(1),USTIME(2))
      USRTIM(2) = CONVRT(UETIME(1),UETIME(2))
      IF (USRTIM(1).GT.USRTIM(2)) THEN
         call FCECHO("  ")
         call FCECHO("  ")
         WRITE(FCECHO_STR,'('' ERROR: START TIME GREATER THAN END 
     & TIME'')') 
         call FCECHO(FCECHO_STR) 
         GOTO 1010
      ENDIF

      RETURN

C---> PROCESS TIME CONVERSION ERRORS
1000  WRITE(FCECHO_STR,'(A)') 'ERROR IN CONVERTING THE INPUT TIME IN 
     & INITLZ'
      call FCECHO(FCECHO_STR) 
1010  RETCOD = 8
      RETURN

10000 FORMAT(6X,'+----------------------+--------------------+------',
     +       '----------+----------------+')
10001 FORMAT(6X,'| TRUNCATED JULIAN DAY | MILLISECOND OF DAY | CONVE',
     +       'RTED DATE | CONVERTED TIME |')
20000 FORMAT(6X,'|       ',I8,'       |     ',I8,'       |    ',I2.2,
     +       '/',I2.2,'/',I2.2,'    |    ',I2.2,':',I2.2,':',
     +       I2.2,'    |')
30000 FORMAT(6X,'+----------------------+--------------------+------',
     +       '----------+----------------+')
11000 FORMAT(6X,'+----------------+----------------+---------------+',
     +       '---------------+')
11001 FORMAT('| SELECTED DATE  | SELECTED TIME  | CONVERTED TJD |',
     +       ' CONVERTED MSD |')
21000 FORMAT(6X,'|    ',I2.2,'/',I2.2,'/',I2.2,'    |    ',
     +       I2.2,':',I2.2,':',I2.2,'    |   ',I8,'    |   ',I8,'    |')
31000 FORMAT(6X,'+----------------+----------------+---------------+',
     +       '---------------+')
CCCCCCCCCCCCCCCCCCCC END MAPGEN.SOURCE(INITLZ) CCCCCCCCCCCCCCCCCCCCCCCCC
      END
