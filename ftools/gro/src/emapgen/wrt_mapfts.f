CCCCCCCCCCCCCCCCCCCCCCCC MAPGEN.SOURCE(WRTFTS) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  ROUTINE NAME:  WRT_MAPFTS
CH1
CH1  VERSION: 1.02              DATE: 04/12/91
CH1  VERSION: 1.03              DATE: 05/21/91
CH1  $Id: wrt_mapfts.f,v 1.5 2013/05/21 19:08:24 irby Exp $
CH1
CH1  PROGRAMMER(S) AND COMPLETION DATE:
CH1     ALBERT ETIENNE - S.T.X. - 02/13/89
CH1     E.S.Panduranga - S.T.X. - 05/21/91
CH1
CH1  FUNCTION: WRITES THE FITS FORMAT FILE FROM THE DATA COLLECTED. THE
CH1            FILE HEADER AS WELL AS THE BIN DATA ARE WRITTEN BY THE
CH1            ROUTINE.
CH1
CH1  SOFTWARE SYSTEM AND SPACECRAFT:  EGRET PROJECT
CH1
CH1  COMPUTER AND LANGUAGE:  IBM 3081 - VS FORTRAN
CH1
CH2  CALLING SEQUENCE:  CALL WRTFTS
CH2     ARGUMENT    TYPE   I/O                 DESCRIPTION
CH2     --------    ----   ---  ----------------------------------------
CH2
CH2  CALLED BY:  MAPGEN
CH2
CH2  CALLS:
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
CH3 BINSIZ     REAL        BIN SIZE SELECTED BY THE USER
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
CH3   G          I*4       -       GROUP INDEX
CH3   P          I*4       -       PARAMETER INDEX
CH3   MIL        I*4       -       MILLISECOND OF DAY
CH3   ELEV       I*4       -       ENERGY LEVEL INDEX
CH3   INX        I*4       -       BIN DATA OUTPUT BUFFER INDEX
CH3   MON        I*4       -       MONTH
CH3   DAY        I*4       -       DAY
CH3   YEAR       I*4       -       YEAR
CH3   HR         I*4       -       HOUR
CH3   MIN        I*4       -       MINUTE
CH3   SEC        I*4       -       SECOND
CH3   MIL        I*4       -       MILLISECOND
CH3   FSCALE     I*4       1       SCALE FACTOR FOR THE POLAR PARAMETERS
CH3   IERR       I*4       -       ERROR CODE FROM TIME CONVERSION
CH3   I2BUFF(1440) I*2     -       BUFFER IN I*2 FOR OUTPUT DATA RECORD
CH3   COMENT     CH*47     -       COMMENT STRING FOR FITS HEADER LINE
CH3   STRING     CH*20     -       TEMPORARY STRING FOR NUMERICAL VALUES
CH3   STRNG2     CH*8      -       STRING TO WRITE ENERGY KEYWORDS
CH3
CH4  LOGICAL UNITS USED:   UNIT #                DESCRIPTION
CH4                        ------    -----------------------------------
CH4                          2       FITS OUTPUT FILE
CH4                          6       PROGRAM REPORT ON PRINTER
CH4
CH4  METHOD:
CH4     FIND THE ACTUAL NUMBER OF ENERGY LEVELS USED
CH4     FOR (I=1 TO THE NUMBER OF FITS KEYWORDS TO WRITE) DO
CH4        COMPOSE THE COMMENT FOR THE CURRENT KEYWORD
CH4        CALL FTPKY(IJLS) TO WRITE THE CURRENT FITS LINE
CH4     END FOR
CH4        FOR (E=1 TO THE NUMBER OF ENERGY LEVELS) DO
CH4           IF (ENERGY LEVEL E IS NON-EMPTY) THEN
CH4              FOR (J=1 TO THE NUMBER OF BINS ON AXIS 2) DO
CH4                 FOR (I=1 TO THE NUMBER OF BINS ON AXIS 1) DO
CH4                    CALL array2fcube.f
CH4                    IF (THE OUTPUT RECORD IS FULL) THEN
CH4                       WRITE THE RECORD TO THE FITS FILE
CH4                    ENDIF
CH4                 END FOR
CH4              END FOR
CH4           END IF
CH4        END FOR
CH4        IF (THE OUTPUT RECORD IS NON-EMPTY) THEN WRITE THE RECORD
CH4  END WRTFTS
CH4
CH5  MODIFICATIONS BETWEEN VERSIONS:
CH5     MOD #   MODIFIER    DATE                  DESCRIPTION
CH5     -----   --------  --------   -----------------------------------
CH5     1.01    A.ETIENNE 01/29/90   ADDED NUMBER OF EXCLUDED EVENTS,
CH5                                  EXCLUDED TIME RANGES, ENERGY RETURN
CH5                                  CODE, PULSAR PARAMETERS, SELECT
CH5                                  HEADERS AND ALL UNRECOGNIZED LINES
CH5                                  TO THE HEADER. USED SPECIAL ARRAY
CH5                                  TO WRITE GALACTIC DISK DATA.
CH5     1.03    A.ETIENNE 04/12/91   ADDED CODE TO WRITE THE EARTH CUTOFF
CH5                                  ANGLES IN THE FITS FILE HEADER.
CH5     2.00    E.S.Panduranga 05/21/91
CH5				Fopened the fits file for output.
CH5				Fclosed the fits file after it is written.
CH5				Merged changes from version 1.06 on IBM
CH5				ADDED CODE TO WRITE THE MAXIMUM
CH5				ANGLE FROM THE DETECTOR AXIS IN THE
CH5				FITS HEADER.
CH5 $Log: wrt_mapfts.f,v $
CH5 Revision 1.5  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.4  2006/08/14 15:07:41  irby
CH5 Initialized status variables in binevt and wrt_mapfts to prevent seg
CH5 faults in fitsio calls.  Also, fixed today's date string in wrt_mapfts;
CH5 was writing it as YYYY-DD-MM, needed/intended to be YYYY-MM-DD.
CH5
CH5 Revision 1.3  2002/12/26 17:16:25  irby
CH5 Fix variable declarations for f90 compatibility, e.g.:
CH5       INTEGER         EVTTJD*2
CH5 is properly declared as:
CH5       INTEGER*2       EVTTJD
CH5
CH5 Revision 1.2  1998/10/06 12:52:24  silvis
CH5 A stop was removed from the main program so that the learn feature
CH5 for the par file would work properly and some minor changes were made
CH5 to the par file.
CH5
C Revision 2.10  1998/02/19  18:39:45  wang
C Fixed dlb's error.
C
C Revision 2.9  1994/08/16  15:11:23  programs
C Include a FITS comment to indicate map was created after SENSTV mod.  nal
C
C Revision 2.8  1993/05/05  12:25:20  albert
C Modified to write the output map with the bin center convention.
C
C Revision 2.7  1993/03/04  17:37:27  albert
C Forced maps with negative start and end longitudes to be in the 0-360 system
C
C Revision 2.6  1993/01/29  15:30:04  albert
C Modified so that string values in the FITS header be no less than 8
C characters long as required by the FITS standard.
C
C Revision 2.5  1992/07/02  12:38:51  albert
C Corrected minor problem in FITS header energy keywords when processing 10
C energy levels and 1 level is missing.
C
C Revision 2.4  1992/03/27  17:21:05  albert
C Used new set of keywords and added information in header. Used variable
C dimension arrays for the maps.
C
C Revision 2.3  1992/01/07  19:14:17  albert
C Added extra keyword to FITS file header to specify whether events were
C selected with 6 Mev in the TASC.
C
C Revision 2.2  1991/11/26  19:46:05  esp
C Changed the format for the output file name to accomodate the new longer
C names.
C
C Revision 2.1  1991/08/28  19:36:16  esp
C First really controlled version on SUN (ported from IBM 3081)
C
C**********************************************************
C
C     Ftool Change
C
C     1. Drop atioff and polar mode.  Egret science team says that 
C       they are buggy.  Remove the code.
C     2. Write the routine array2fcube.f so that fits writer can 
C       handle the output.
C
C     J. Silvis
C
C     RSTX
C     Sept 1998
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE WRT_MAPFTS
      implicit none 

       REAL        ZENM,RDATE,RTIME
       REAL*8      OBSTIM,CONVRT
C      
C      The character variable below has been added 
C      so that the code can use the fitsio routine 
C      fcecho. 
C      
C      Jeff Silvis 
C      RSTX 
C      29  July  98
 
      character(250) FCECHO_STR 
      INTEGER	  CDATE(3),I,J
      INTEGER     INX,MON,DAY,YEAR,HR,MIN,SEC,MIL,IERR
      CHARACTER   COMENT*47,STRING*24,STRNG2*8


C**********************************************************
C
C     Ftool Change
C
C     The variables below were added to convert the output 
C   to fitsio
C
C
      integer status, lun_fits, blocksize     
      character history*69
C
C     The variables below are used for the polar map which has 
C  been disabled:
C  
C      INTEGER	  K,N,G,P
C      INTEGER*2   I2BUFF(1440),GETBIN,ELEV,FSCALE
C      EQUIVALENCE (FITSRC,I2BUFF(1))
C     End of Ftool Change
C
C**********************************************************

      INCLUDE 'main.cmn.inc'
      INCLUDE 'fitsdt.cmn.inc'


      character(80)	id
      common	/id/	id
      id = '$Id: wrt_mapfts.f,v 1.5 2013/05/21 19:08:24 irby Exp $'

      status = 0

C---> COUNT THE NUMBER OF NON-EMPTY ENERGY LEVELS
      NAXIS4 = 0
      DO 10 I=1,NUMLEV
         IF (ENFLAG(I)) NAXIS4 = NAXIS4 + 1
         ENFLAG(I) = .true.
10    CONTINUE

C---> TEST IF ANY DATA HAS BEEN COLLECTED
      IF (NAXIS4.EQ.0) THEN
         WRITE(FCECHO_STR,'(A)') 'NO DATA ACCUMULATED'
         call FCECHO(FCECHO_STR) 
         RETCOD = 4
         RETURN
      ENDIF
 
C---> ADJUST THE REFERENCE POINT TO BE AT THE PIXEL CENTER
      CRVAL1 = CRVAL1 + CDELT1/2
      IF (CRVAL1 .GT. 180) CRVAL1 = CRVAL1 - 360
      CRVAL2 = CRVAL2 + CDELT2/2
      IF (CRVAL2 .GT. 180) CRVAL2 = CRVAL2 - 360
      CRVAL3 = CRVAL3 + CDELT3/2


       if ( (crval1.lt.0) .and.  ( (crval1+naxis1*cdelt1).lt. 0) ) 
     &	    crval1 = crval1 + 360.0
       NAXIS3 = NUMLEV
       NAXIS4 = 0

Cesp  ! open the fits file !
C      call fopen(2, 2, FILNAM, 0, 'FB',2880, 2880)
C**********************************************************
C
C     Ftool Change
C
C     
C
C
C     J. Silvis
C
      call ftgiou(lun_fits,status)
      blocksize = 1
      call ftinit (lun_fits,FILNAM,blocksize,status)

C
C     End of Ftool Change
C
C**********************************************************
      call FCECHO("  ")
      call FCECHO("  ")
      WRITE(FCECHO_STR,'('' FITS FILE HEADER GENERATED:'')') 
      call FCECHO(FCECHO_STR) 
      call FCECHO("  ")
C**********************************************************
C
C     Ftool Change
C
C     FITSRC is part of the old fits writer.
C
C
C     J. Silvis
C
C      FITSRC = ' '
C
      COMENT = 'Standard FITS format'
      simple = .TRUE.
      call ftpkyl(lun_fits,'SIMPLE',simple,coment,status)
      COMENT = 'Number of bits per pixel (2-byte integers)'
      call ftpkyj(lun_fits, 'BITPIX',16,coment,status)
      COMENT = 'Number of axis in primary array'
      call ftpkyj(lun_fits, 'NAXIS',NAXIS,coment,status)
      COMENT = 'Number of bins per row'
      call ftpkyj(lun_fits, 'NAXIS1',NAXIS1,coment,status)
      COMENT = 'Number of rows'
      call ftpkyj(lun_fits, 'NAXIS2',NAXIS2,coment,status)
      COMENT = 'Number of energy levels'
      call ftpkyj(lun_fits, 'NAXIS3',NAXIS3,coment,status)
      IF (NAXIS4.GT.0) THEN
         COMENT = 'Number of energy levels'
         call ftpkyj(lun_fits, 'NAXIS4',NAXIS4,coment,status)
      ENDIF
      IF (NAXIS1.GT.0) THEN

         COMENT = 'Min coordinate of reference bin (RA, GLON)'
          call ftpkyf(lun_fits, 'CRVAL1',CRVAL1,2,coment,status)

          COMENT = 'Location of reference point in array'
          call ftpkyf(lun_fits, 'CRPIX1',CRPIX1,2,coment,status)

         COMENT = 'Interval in degrees'
          call ftpkyf(lun_fits, 'CDELT1',CDELT1,2,coment,status)

         COMENT = 'Coordinate type'
         call ftpkys(lun_fits, 'CTYPE1',CTYPE1,coment,status)
      ENDIF

      COMENT = 'Min coordinate of reference bin (DEC, GLAT)'

      call ftpkyf(lun_fits, 'CRVAL2',CRVAL2,2,coment,status)

      COMENT = 'Location of reference point in array'
      call ftpkyf(lun_fits, 'CRPIX2',CRPIX2,2,coment,status)

      COMENT = 'Interval in degrees'
      call ftpkyf(lun_fits, 'CDELT2',CDELT2,2,coment,status)

      COMENT = 'Coordinate type'
      call ftpkys(lun_fits, 'CTYPE2',CTYPE2,coment,status)

      COMENT = 'Units of primary array'
       call ftpkys(lun_fits, 'BUNIT','COUNTS',coment,status)

      COMENT = 'Data scaling (none)'
       call ftpkye(lun_fits, 'BSCALE',1.0,7,coment,status)

      COMENT = 'Data offset (none)'
       call ftpkye(lun_fits, 'BZERO',0.0,7,coment,status)


C
CC---> INDICATE THAT THE REFERENCE POINT IS AT THE CENTER OF THE PIXEL

      COMENT = 'The reference point is at the pixel center'
      call ftpkyl(lun_fits,'PIXCENT',.TRUE.,coment,status)

      CALL TJDGRD(DATIME(1),DATIME(2),YEAR,MON,DAY,HR,MIN,SEC,MIL,IERR)
      IF (YEAR .LE. 1998) THEN
        WRITE(STRTIM,2000) DAY,MON,MOD(YEAR,100),HR,MIN,SEC,MIL
      ELSE
        WRITE(STRTIM,2001) YEAR,MON,DAY,HR,MIN,SEC,MIL
      ENDIF
      CALL RLTIME(MON,DAY,YEAR,HR,MIN,SEC,MIL,RDATE,RTIME)

      COMENT = 'Year.day-of-the-year data begins'
      call ftpkyf(lun_fits, 'STRT-DAY',RDATE,3,coment,status)       

      COMENT = 'Data start time seconds of day'
      call ftpkyf(lun_fits, 'STRT-TIM',RTIME,3,coment,status) 

      CALL TJDGRD(DATIME(3),DATIME(4),YEAR,MON,DAY,HR,MIN,SEC,MIL,IERR)
      IF (YEAR .LE. 1998) THEN
        WRITE(ENDTIM,2000) DAY,MON,MOD(YEAR,100),HR,MIN,SEC,MIL
      ELSE
        WRITE(ENDTIM,2001) YEAR,MON,DAY,HR,MIN,SEC,MIL
      ENDIF
      CALL RLTIME(MON,DAY,YEAR,HR,MIN,SEC,MIL,RDATE,RTIME)

      COMENT = 'Year.day-of-the-year data ends'
      call ftpkyf(lun_fits, 'END-DAY',RDATE,3,coment,status)

      COMENT = 'Data end time seconds of day'
      call ftpkyf(lun_fits, 'END-TIM',RTIME,3,coment,status)

      OBSTIM = (CONVRT(DATIME(3),DATIME(4))-CONVRT(DATIME(1),DATIME(2))
     &	       - EXTIME) * 86400

      COMENT = 'Observation time in sec (good data)'
      call ftpkyf(lun_fits, 'OBS-TIME',SNGL(OBSTIM),3,coment,status)

      IF (YEAR .LE. 1998) THEN

        COMENT = 'Data start date in dd/mm/yy'
         call ftpkys(lun_fits, 'DATE-OBS',STRTIM(1:8),coment,status)

        COMENT = 'Data start time in hh:mm:ss.sss'
         call ftpkys(lun_fits, 'TIME-OBS',STRTIM(10:21),coment,status)

        COMENT = 'Data end date in dd/mm/yy'
         call ftpkys(lun_fits, 'DATE-END',ENDTIM(1:8),coment,status)

        COMENT = 'Data end time in hh:mm:ss.sss'
         call ftpkys(lun_fits, 'TIME-END',ENDTIM(10:21),coment,status)

      ELSE

         COMENT = 'Data start date in yyyy-mm-dd'
         call ftpkys(lun_fits, 'DATE-OBS',STRTIM(1:10),coment,status)

         COMENT = 'Data start time in hh:mm:ss.sss'
         call ftpkys(lun_fits, 'TIME-OBS',STRTIM(10:21),coment,status)

         COMENT = 'Data end date in yyyy-mm-dd'
         call ftpkys(lun_fits, 'DATE-END',ENDTIM(1:10),coment,status)

         COMENT = 'Data end time in hh:mm:ss.sss'
         call ftpkys(lun_fits, 'TIME-END',ENDTIM(10:21),coment,status)
      ENDIF
C
       COMENT = 'Coordinate epoch used'
       call ftpkyj(lun_fits, 'EQUINOX',2000,coment,status)
C

       COMENT = 'Right ascension of spacecraft Z axis'
       call ftpkyf(lun_fits, 'SC-Z-RA',PDIREC(3),3,coment,status) 

       COMENT = 'Declination of spacecraft Z axis'
       call ftpkyf(lun_fits, 'SC-Z-DEC',PDIREC(4),3,coment,status) 

       COMENT = 'Right ascension of spacecraft X axis'
       call ftpkyf(lun_fits, 'SC-X-RA',PDIREC(7),3,coment,status) 

       COMENT = 'Declination of spacecraft X axis'
       call ftpkyf(lun_fits, 'SC-X-DEC',PDIREC(8),3,coment,status) 

       COMENT = 'BII coordinate of spacecraft Z axis'
       call ftpkyf(lun_fits, 'SC-Z-BII',PDIREC(2),3,coment,status) 

       COMENT = 'LII coordinate of spacecraft Z axis'
       call ftpkyf(lun_fits, 'SC-Z-LII',PDIREC(1),3,coment,status) 

       COMENT = 'BII coordinate of spacecraft X axis'
       call ftpkyf(lun_fits, 'SC-X-BII',PDIREC(6),3,coment,status) 

       COMENT = 'LII coordinate of spacecraft X axis'
       call ftpkyf(lun_fits, 'SC-X-LII',PDIREC(5),3,coment,status) 

       COMENT = 'Right ascension of instrument'
       call ftpkyf(lun_fits, 'INSTRA1',PDIREC(3),3,coment,status) 

       COMENT = 'Declination of instrument'
       call ftpkyf(lun_fits, 'INSTDEC1',PDIREC(4),3,coment,status) 
C
      J = 0
      COMENT = 'Minimum energy of map in level xx (Mev)'
      DO I=1,NUMLEV
         IF (ENFLAG(I)) THEN   
            J = J + 1

            WRITE(STRNG2,'(A6,I1,A1)') 'MINENG',J,' '
	    COMENT(2:3) = 'in'
            IF (J.EQ.10) WRITE(STRNG2,'(A6,I2)') 'MINENG',J
            WRITE(COMENT(32:33),'(I2)') J
            call ftpkyj(lun_fits,STRNG2,ENRGY1(I),coment,status)

	    STRNG2(2:3) = 'AX'
	    COMENT(2:3) = 'ax'
            call ftpkyj(lun_fits,STRNG2,ENRGY2(I),coment,status)

	 END IF
      END DO

      J = 0
      COMENT = 'Maximum angle from zenith in level xx'
      DO I=1,NUMLEV
         IF (ENFLAG(I)) THEN
            J = J + 1
            ZENM = ZENMAX(I) / CVTANG

            WRITE(STRNG2,'(A6,I1,A1)') 'ZENMAX',J,' '
            IF (J.EQ.10) WRITE(STRNG2,'(A6,I2)') 'ZENMAX',J
            WRITE(COMENT(36:37),'(I2)') J
            call ftpkyf(lun_fits,STRNG2,ZENM,3,coment,status) 
         END IF
      END DO



C      CALL IDATE(CDATE)
C
C      Get the date using fitsio.
C

      call FTGSDT(CDATE(1),CDATE(2),CDATE(3),STATUS)
      IF ( CDATE(3) .LE. 1998) THEN
        WRITE(STRING,'(I2.2,A1,I2.2,A1,I2.2)') CDATE(1),'/',CDATE(2),
     &   '/',MOD(CDATE(3),100)
        COMENT = 'Current date in dd/mm/yy'
        call ftpkys(lun_fits, 'DATE',STRING,coment,status)

      ELSE
        WRITE(STRING,'(I4.4,A1,I2.2,A1,I2.2)') CDATE(3),'-',CDATE(2),
     &   '-',CDATE(1)
        COMENT = 'Current date in yyyy-mm-dd'
        call ftpkys(lun_fits, 'DATE',STRING,coment,status)
      ENDIF

      COMENT = 'Primary Array'
      call ftpkys(lun_fits, 'PRIMTYPE','RECT_MAP',coment,status)

      COMENT = 'Data type in the file'
      call ftpkys(lun_fits, 'FILETYPE','EGRET_COUNTS_MAP',coment,status)

      COMENT = 'Type of map'
      call ftpkys(lun_fits, 'MAPTYPE','SINGLE_POINTING',coment,status)

      COMENT = 'Number of elements in the map'
       call ftpkyj(lun_fits, 'MAPSIZE',MAPSIZ,coment,status) 
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
C
C      IF (ERFLAG) THEN
C         WRITE(STRING,'(I1,6A2)') ERCLAS,(ERBITS(I),I=1,6)
C         COMENT = 'Energy return code (class, bits 2-7)'
C         call ftpkys(lun_fits, 'E-RETCOD',STRING,coment,status)
C      ENDIF
C
C     End of Ftool Change
C
C**********************************************************


      COMENT = 'Events selected with 6 Mev in TASC'
      call ftpkyl(lun_fits,'TASCTHRS',TSCFLG,coment,status)

      COMENT = 'Maximum angle from detector axis'
      call ftpkyj(lun_fits,'DETMAX',DETMAX,coment,status)

      COMENT = 'File produced at Goddard Space Flight Center'
      call ftpkys(lun_fits,'ORIGIN','GSFC    ',coment,status)

      COMENT = 'Compton Observatory (Gamma Ray Observatory)'
      call ftpkys(lun_fits,'TELESCOP','GRO     ',coment,status)

      COMENT = 'The GRO instrument is EGRET'
      call ftpkys(lun_fits,'INSTRUME','EGRET   ',coment,status)

      COMENT = 'Original FITS file name'
      call ftpkys(lun_fits,'FILENAME',FILNAM,coment,status)


      COMENT = 'PI is Carl Fichtel'
      call ftpkys(lun_fits,'OBSERVER','FICHTEL ',coment,status)

      COMENT = 'Name of object or nominal pointing id'
      call ftpkys(lun_fits,'OBJECT1','       ',coment,status)

      COMENT = 'Name used for file cataloging & archiveing'
      call ftpkys(lun_fits,'FILE-ID','       ',coment,status)

      COMENT = 'Version of file used for cataloging'

       call ftpkys(lun_fits,'FILE-VER','       ',coment,status)
C
C    The name of the input file was passed to this 
C  routine by the common block main.cmn.inc. The 20 
C  Character limit is imposed by fits
C

       write(history,'(A,A,A)') 'IN-FILE = ', evfile(1:20), 
     &           '  / Input file name'
       call ftphis(lun_fits,history,status)

       write(history,'(A,I9,A)') 'EXCLUDED = ',NUMEXC, 
     &  ' / Number of excluded events'
       call ftphis(lun_fits,history,status)

      IF (PULFLG) THEN
         WRITE(STRING,'(2E11.3)') (PULVAL(I),I=1,2)
         COMENT = ' Pulsar phase limits'
          write(history,'(A,A,A)') 'PUL PHAS = ',STRING
     &   ,' Pulsar phase limits'
         call ftphis(lun_fits,history,status)

          WRITE(STRING,'(2E11.3)') (PULVAL(I),I=3,4)
          write(history,'(A,A,A,A)') 'BIN PHAS = ',STRING, ' /' 
     &    , 'Pulsar binary phase limits'
          call ftphis(lun_fits,history,status)
      ENDIF
      DO I=1,NUMSAV
         STRING = SAVEHD(I)(1:24)
         COMENT = SAVEHD(I)(25:64)
         write(history,'(A23,A39)') STRING, COMENT
         call ftphis(lun_fits,history,status)
      END DO
      INX = 1
C
C---> WRITE THE RECTANGULAR MAP DATA TO THE FILE
         call array2fcube(lun_fits,BINDAT,NAXIS1,NAXIS2,NAXIS3,
     &   status)     
C
C  Close the fits file
C

      call ftclos(lun_fits,status)
      call ftfiou(lun_fits,status)
      RETURN

1000  FORMAT(A5,2(2X,A17),1X,2(F6.1),2X,A3,2X,A4,2(3X,'F'),A121,/,
     &       76X,A120)
2000  FORMAT(I2.2,'/',I2.2,'/',I2.2,',',I2.2,':',I2.2,':',I2.2,'.',I3.3)
2001  FORMAT(I4.4,'-',I2.2,'-',I2.2,',',I2.2,':',I2.2,':',I2.2,'.',I3.3)

CCCCCCCCCCCCCCCCCCCC END MAPGEN.SOURCE(WRTFTS) CCCCCCCCCCCCCCCCCCCCCCCCC
      END
