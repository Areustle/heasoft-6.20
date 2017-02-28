CCCCCCCCCCCCCCCCCCCCCCCCC MAPGEN.SOURCE(BINEVT) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  ROUTINE NAME:  BINEVT
CH1
CH1  VERSION: 1.04              REVISION DATE: 05/17/91
CH1  $Id: binevt.f,v 1.7 2013/05/21 19:08:24 irby Exp $
CH1
CH1  PROGRAMMER(S) AND COMPLETION DATE:
CH1     ALBERT ETIENNE - S.T.X. - 02/13/89
CH1     E.S.Panduranga - S.T.X. - 05/17/91
CH1
CH1  FUNCTION: READS EACH EVENT, TESTS IT FOR CORRECT TIME AND ENERGY
CH1            LEVEL AND CALLS THE APPROPRIATE ROUTINE TO BIN THE EVENT
CH1            DEPENDING ON THE GRID/home/egret5/silvis/mgen TYPE SELECTED.
CH1
CH1  SOFTWARE SYSTEM AND SPACECRAFT:  EGRET PROJECT
CH1
CH1  COMPUTER AND LANGUAGE:  IBM 3081 - VS FORTRAN
CH1
CH2  CALLING SEQUENCE:  CALL BINEVT
CH2     ARGUMENT    TYPE   I/O                 DESCRIPTION
CH2     --------    ----   ---  ----------------------------------------
CH2
CH2  CALLED BY:  MAPGEN
CH2
CH2  CALLS:
CH2   BRECTN: BINS THE CURRENT EVENT INTO A RECTANGULAR GRID
CH2   BPOLAR: BINS THE CURRENT EVENT INTO A POLAR  GRID
CH2   FREAD : SYSTEM ROUTINE TO READ A RECORD
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
CH3 MAPTYP     CH*4        MAP TYPE (ALL SKY, SINGLE POINTING, GAL DISK)
CH3 SYS        INTEGER     INDEX FOR THE COORDINATE SYSTEM TO USE
CH3 MAPTP2     CH*4        MAP TYPE (ALL SKY, SINGLE POINTING, GAL DISK)
CH3 TESTPI     LOGICAL     DETERMINES IF PI IS BETWEEN THE USER LIMITS
CH3 NUMSAV     INTEGER     NUMBER OF FITS HEADER LINES SAVED
CH3 CSYSTM     CH*4        COORDINATE SYSTEM TO USE (GAL,CEL,INST,EARTH)
CH3 TIMFMT     CH*8        TIME FORMAT (CAL=CALENDAR, TJD=TJD/MSD)
CH3 FILNAM     CH*8        NAME EXTENSION OF THE OUTPUT FITS FILE
CH3 EVFILE     CH*24       NAME OF THE EVENT INPUT FILE
CH3 SAVEHD(150)CH*64       HEADER LINES SAVED
CH3
CH3 VARIABLE     TYPE                        DEFINITION
CH3   NAME
CH3 --------   ---------   ---------------------------------------------
CH3 EVTMSD     INTEGER*4   EVENT MILLISECOND OF DAY
CH3 EVTMIC     INTEGER*2   EVENT MICROSECOND
CH3 EVTTJD     INTEGER*2   EVENT TRUNCATED JULIAN DAY
CH3 SPARE1(6)  REAL*4      SPARE BYTES
CH3 EVTPOS(8)  REAL*4      EVENT POSITIONS IN ALL COORDINATE SYSTEMS
CH3 EVTENG     REAL*4      EVENT ENERGY
CH3 SPARE2(3)  INTEGER     SPARE BYTES
CH3 EVTBIN     REAL*4      EVENT BINARY PULSAR PARAMETER
CH3 EVTPUL     REAL*4      EVENT PULSAR PARAMETER
CH3 SPARE2(3)  REAL*4      SPARE BYTES
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
CH3   ITJD       I*4       -       TRUNCATED JULIAN DAY IN I*4
CH3   ELEVEL     I*4       0       CURRENT ENERGY LEVEL INDEX
CH3   DISPOS     I*4       -       EVENT DISPOSITION IN I*4
CH3   NUMRED     I*4       0       NUMBER OF EVENTS READ
CH3
CH4
CH4  METHOD:
CH4     OPEN THE FILE USING FITSIO
CH4     WHILE (THE TERMINATE FLAG IS NOT SET) DO
CH4        SET STATISTICS FOR THE NEXT FILE
CH4        OPEN THE CURRENT EVENT FILE
CH4        WHILE (NO END OF FILE) DO
CH4           READ THE NEXT RECORD
CH4           IF (THE EVENT IS BEFORE THE START TIME) READ AGAIN
CH4           IF (THE EVENT IS AFTER END TIME)SET TERMINATE FLAG,EXIT
CH4           IF (THE EVENT IS WITHIN CURRENT EXCLUDED TIME) READ AGAIN
CH4           IF(EVENT DOES NOT HAVE THE CORRECT PULSAR PARMS)READ AGAIN
CH4           I = 0
CH4           WHILE (ENERGY LEVEL HAS NOT BEEN FOUND & I<# OF LEVELS)DO
CH4              IF (THE EVENT'S ENERGY IS WITHIN RANGE I) THEN
CH4                 SET A FLAG TO INDICATE THAT THE LEVEL HAS BEEN FOUND
CH4                 SAVE THE LEVEL INDEX I
CH4              ENDIF
CH4           END WHILE
CH4           IF (THE ENERGY LEVEL HAS BEEN FOUND) THEN
CH4                 CALL BRECTN TO BIN THE EVENT IN A RECTANGULAR GRID
CH4              IF (EVENT HAS BEEN BINNED) INCREMENT # OF EVENTS BINNED
CH4           ENDIF
CH4        END WHILE
CH4        CLOSE THE CURRENT EVENT FILE
CH4        PRINT THE STATISTICS
CH4     IF (A TIME OVERLAP OCCURRED IN MAP UPDATE) WRITE A WARNING MSG
CH4     SAVE THE START AND END TIMES OF THE UPDATED MAP (IF ANY)
CH4  END BINEVT
CH4
CH5  MODIFICATIONS BETWEEN VERSIONS:
CH5     MOD #   MODIFIER    DATE                  DESCRIPTION
CH5     -----   --------  --------   -----------------------------------
CH5     1.01    A.ETIENNE 01/29/90   MODIFIED ROUTINE TO: READ THE
CH5                                  TIMELINE FILE, GET THE EXCLUDED
CH5                                  TIME RANGES, EXCLUDE EVENTS WITHIN
CH5                                  THESE RANGES, READ THE EVENTS FROM
CH5                                  THE NEW SMDB AND FROM THE SELECT
CH5                                  CATALOG, UPDATE THE CATALOG WHEN
CH5                                  NEEDED, SKIP THE SELECT HEADER,
CH5                                  SAVE THE POINTING DIRECTION AND
CH5                                  THE CATALOG FLAGS, TEST ON THE
CH5                                  ENERGY RETURN CODE AND ON THE
CH5                                  PULSAR PARAMETERS, SAVE THE NEW
CH5                                  USER SELECTION, AND THE EXCLUDED
CH5                                  RANGES TO BE WRITTEN TO THE FITS
CH5                                  FILE HEADER.
CH5     1.02    A.ETIENNE 03/21/91   REMOVED CODE THAT READ THE TIMELINE
CH5                                  FILE AND REPLACED BY CALL TO GETEXT
CH5     1.03    A.ETIENNE 04/12/91   ADDED TEST AGAINST THE ZENITH ANGLE
CH5                                  FOR EACH ENERGY LEVEL & STATISTICS
CH5                                  FOR EVENTS ELIMINATED BY THAT TEST.
CH5	2.00	E.S.Panduranga	05/17/91
CH5				Filename has complete pathname and is not of
CH5				fixed length. Hence it is processed differently.
CH5				Fname increased from character(24) to *80.
CH5				Opened catalog file before it can be read.
CH5				Converted input reals to IEEE format.
CH5				Reading event file with direct access.
CH5				Added variable recNum to do the above.
CH5				Checking for hexadecimal 5c instead of *, in
CH5				EVTLIN. Apparently, if this character is found,
CH5				the rest of the line is text and we can convert
CH5				it to ASCII. Since the first field is the time
CH5				field, even if the line is a data line, it
CH5				cannot have x'5c' as valid time data.
CH5				Merged changes from version 1.06 on IBM
CH5				USE NEW DEFINITION OF EVENT CLASS:
CH5				USER INPUT  EVENTS   E_CLASS BITS
CH5				    1         ALL
CH5				    2         A         00
CH5				    3         B         01
CH5				    4         C         10
CH5				    5       A OR C    00 OR 10
CH5				PRINTED ADDITIONAL STATISTICS
CH5 $Log: binevt.f,v $
CH5 Revision 1.7  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.6  2006/08/14 15:07:41  irby
CH5 Initialized status variables in binevt and wrt_mapfts to prevent seg
CH5 faults in fitsio calls.  Also, fixed today's date string in wrt_mapfts;
CH5 was writing it as YYYY-DD-MM, needed/intended to be YYYY-MM-DD.
CH5
CH5 Revision 1.5  2005/08/26 19:36:34  irby
CH5 Purely cosmetic changes to allow compilation with gfortran/g95, mostly
CH5 involving fixes to lines longer than 72 chars that were being truncated,
CH5 but in the case of the CGRO code, also moving misplaced (tabbed) line
CH5 continuation characters to their appropriate position in column 6.
CH5
CH5 Revision 1.4  2002/12/26 17:16:25  irby
CH5 Fix variable declarations for f90 compatibility, e.g.:
CH5       INTEGER         EVTTJD*2
CH5 is properly declared as:
CH5       INTEGER*2       EVTTJD
CH5
CH5 Revision 1.3  1999/03/25 21:40:56  toliver
CH5 removed dead code causing compiler warnings
CH5
c Revision 1.2  1998/10/06  12:52:19  silvis
c A stop was removed from the main program so that the learn feature
c for the par file would work properly and some minor changes were made
c to the par file.
c
C Revision 2.9  1997/10/09  13:59:41  wang
C *** empty log message ***
C
C Revision 2.8  1997/10/09  13:40:57  wang
C Fixed the problems so it can read fit file.
C
C Revision 2.7  1993/09/14  19:24:32  albert
C Increased size of arrays to store excluded time ranges to allow large
C number of input SMDB files to be processed.
C
C Revision 2.6  1992/07/10  14:23:48  albert
C Modified to get the TASC in and out time ranges from the timeline file
C and test events for the 6 Mev flag only if the TASC is in coincidence.
C
C Revision 2.5  1992/04/20  13:53:31  albert
C Corrected for error in reading the SELECT files header and unneccesarily
C converting it to ASCII.
C
C Revision 2.4  1992/03/27  15:41:17  albert
C Read the pointing direction from the timeline file instead of the SMDB
C catalog. Put the pointing direction in both galactic and celestial
C coordinate systems to record them in the header. Save the name of the
C input event file to record it in the header. Compute the total
C excluded time to save it in the header.
C
C Revision 2.3  1992/01/02  21:02:16  albert
C Rejected events that do not have the 6 Mev in TASC if required by the user
C
C Revision 2.2  1991/09/27  20:47:42  albert
C Converted pointing direction to Galactic coordinates if the coordinate
C system is galactic.
C
C Revision 2.1  1991/08/28  19:36:16  esp
C First really controlled version on SUN (ported from IBM 3081)
C
C**********************************************************
C
C     Ftool Change
C
C   There were a large number of changes involved in making
C   binevt part of an ftool.
C   1. All file input is now done with the routine read_qvpfits
C      which is based on fitsio.   All other I/O has been removed.
C   2. The catalog file is no longer called.
C       -it was only used to get the file's name.
C       -the option to update the catalog was dropped
C   3. All output to the screen is now done through 
C      the platform independent function fcecho.
C   4. The EGRET team told me the function BPOLAR did not
C      work.  It was dropped.
C   5. The option to run multi-files was dropped.  This can 
C     be done by putting the ftool in a script.
C   6. The platform dependent function BTEST was dropped and
C     replaced by bit5pull.f
C   7. The EGRET science team told me that the energy return code 
C     is no longer used so the code related to that was dropped.
C   8. Separate lines to pull the header from the binary file were
C     dropped.     
C
C
C     J. Silvis
C
C     RSTX
C     Sept 1998
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE BINEVT
      implicit none 

       REAL	PDIR(2)
C      
C      The character variable below has been added 
C      so that the code can use the fitsio routine 
C      fcecho. 
C      
C      Jeff Silvis 
C      RSTX 
C      29  July  98
 
      character(250) FCECHO_STR 
C**********************************************************
C
C     Ftool Change
C
C     The variables below were added to switch to fitsio
C for the reader.
C
C     J. Silvis
C

      integer status,read_unit,readwrite,blocksize,hdutype
      integer total_rows
      character comment*30
      LOGICAL  NO_6MEV,BIT5PULL
C     End of Ftool Change
C
C**********************************************************

      INTEGER   ITJD,ELEVEL,NUMRED,NUMTS,IERR,NUMCOL
      INTEGER   NX,I,NUMEX,NUMZN,NCOL,NUMOT,IX

      INTEGER         EVTMSD
      INTEGER*2       EVTTJD
      REAL            EVTPUL,EVTBIN,EVTENG,EVTPOS(8)
      INTEGER   NUMEN,NUMAR,NUMRD,ERM(5),ERS(5),NT,IT
      integer*2	TASC(2)
      REAL*8    EVTIME,XSTIME(500),XETIME(500),CONVRT
      REAL*8	TSTIME(100),TETIME(100)
      LOGICAL   DOTEST
      INCLUDE 'main.cmn.inc'
      INCLUDE 'fitsdt.cmn.inc'

      DATA NUMCOL/0/,NCOL/0/,IX/1/, IERR/0/
      DATA ERM/0,3,3,3,1/, ERS/0,0,1,2,0/, IT/1/

      character(80)	id
      common	/id/	id
      id = '$Id: binevt.f,v 1.7 2013/05/21 19:08:24 irby Exp $'

      status = 0

      WRITE(FCECHO_STR,'(''1'')') 
      call FCECHO(FCECHO_STR) 

C---> CALL GETEXT TO GET THE EXCLUDED TIMES FROM THE TIMELINE FILE
      CALL GETEXT(NX,XSTIME,XETIME,EXTIME,USRTIM,PDIREC,CVTANG,SAVEHD,
     &   NUMSAV,NT,TSTIME,TETIME,IERR)
      WRITE(FCECHO_STR,'('' NUMBER OF EXCLUDED TIME RANGES ='',I10)') 
     &  NX
      call FCECHO(FCECHO_STR) 
 
C------> Set up the event counters
       NCOL   = 0
       NUMEX  = 0
       NUMZN  = 0
       NUMRED = 0
       NUMRD = 0
       NUMOT = 0
       NUMEN = 0
       NUMAR = 0
       NUMTS = 0
C
C    The name of the input file was passed to this 
C  routine by the common block main.cmn.inc
C
         call FCECHO("  ")
         WRITE(FCECHO_STR,'('' OPENING EVENT FILE '',A)')  EVFILE
         call FCECHO(FCECHO_STR) 

C------> GET THE FILE POINTING DIRECTION
	 IF (CSYSTM.EQ.'GALA') THEN
	    PDIR(1) = PDIREC(1)*CVTANG
	    PDIR(2) = PDIREC(2)*CVTANG
	 ELSE
	    PDIR(1) = PDIREC(3)*CVTANG
	    PDIR(2) = PDIREC(4)*CVTANG
	 END IF

C**********************************************************
C
C     Ftool Change
C
C     Open the file using standard fitsio.
C
C
C     J. Silvis
C
C       Get an unused Logical Unit Number to use to open the FITS file
c
      call ftgiou(read_unit,status) 
      readwrite=0
      call ftopen(read_unit,evfile,readwrite,blocksize,status)
C
C         
C     move to the binary table extension
C
      call ftmrhd(read_unit,1,hdutype,status)
C
C      The keyword naxis2 stores the number of rows in the binary
C    table.
C
C
      call ftgkyj(read_unit,'NAXIS2',total_rows,comment,status)
C
C     The routine read_qvpfits will read the input fits
C  file and pull the variables from the fits file.
C

20      if (NUMRED .EQ. total_rows) goto 50  

        call read_qvpfits(read_unit, NUMRED,status,
     &    EVTMSD,EVTTJD,TASC,EVTPOS,EVTENG,
     &    EVTPUL,EVTBIN)
         NUMRED = NUMRED + 1

C------> GET THE EVENT TIME
         ITJD = EVTTJD
         EVTIME = CONVRT(ITJD,EVTMSD)

C------> TEST THE EVENT TIME WITH THE USER TIME RANGE (NON-UPDATE MAPS)
         IF (EVTIME.LT.USRTIM(1)) GOTO 20
         IF (EVTIME.GT.USRTIM(2)) GOTO 50
	 NUMRD = NUMRD + 1

C------> SKIP EVENTS WITHIN THE EXCLUDED TIME RANGES
         IF (IX.LE.NX) THEN
            DO WHILE (IX.LE.NX.AND.EVTIME.GT.XETIME(IX))
               IX = IX + 1
            END DO
            IF ( (IX.LE.NX) .AND. (XSTIME(IX).LE.EVTIME) 
     &            .AND. (EVTIME.LE.XETIME(IX))  ) THEN
               NUMEX = NUMEX + 1
               GOTO 20
            ENDIF
         ENDIF


C------> TEST THE PULSAR PARAMETERS
         IF (PULFLG) THEN
	    NUMOT = NUMOT + 1
            IF ( (EVTPUL.LT.PULVAL(1)) .OR. 
     &             (EVTPUL.GT.PULVAL(2))) GOTO 20
            IF ( (EVTBIN.LT.PULVAL(3)) .OR. 
     &         (EVTBIN.GT.PULVAL(4))   ) GOTO 20
	    NUMOT = NUMOT - 1
         ENDIF

C------> TEST THE 6 MEV IN TASC IF THE TASC IS IN COINCIDENCE
         IF (TSCFLG) THEN
	    DOTEST = .TRUE.
            IF (IT.LE.NT) THEN
               DO WHILE ( (IT.LE.NT) .AND. (EVTIME.GE.TETIME(IT) ))
                  IT = IT + 1
               END DO
               IF ( (IT.LE.NT) .AND. (TSTIME(IT).LE.EVTIME) .AND.
     &              (EVTIME.LT.TETIME(IT))  ) DOTEST = .FALSE.
            ENDIF
C       
C
C  The integer*2 array tasc stores the contents of column 10 and 11 
C from the qvp file.  These columns give the status of the 
C TASC.  The tasc has two divisions, so there are two bytes.
C If the fifth bit of either of these bytes is high it means
C that the tasc did not detect 6 MeV.  The function
C bit5pull will be examine the fifth bit to see if it is high.
C

            NO_6MEV =  bit5pull(tasc(1))  .OR. 
     &                 bit5pull(tasc(2))     

            IF (DOTEST.AND. NO_6MEV ) THEN
     	       NUMTS = NUMTS + 1
	       GOTO 20
            ENDIF
         ENDIF

C------> TEST IF THE EVENT ENERGY IS WITHIN 1 OF THE SELECTED LEVELS
         I = 1
         ELEVEL = 0
40       IF ( (ENRGY1(I).LE.EVTENG) .AND. (EVTENG.LE.ENRGY2(I)) ) THEN
            IF (EVTPOS(3).LE.ZENMAX(I)) THEN
               ELEVEL = I
            ELSE
               NUMZN = NUMZN + 1
            END IF
         ELSE
            I = I + 1
            IF (I.LE.NUMLEV) GOTO 40
	    IF (ELEVEL.EQ.0) NUMEN = NUMEN + 1
         ENDIF

C------> CALL THE APPROPRIATE BINNING ROUTINE DEPENDING ON THE GRID
         IF (ELEVEL.GT.0) THEN
            CALL BRECTN(ELEVEL,PDIR(1),PDIR(2),EVTMSD,EVTTJD,EVTPOS)
            IF (RETCOD.EQ.0) THEN
               NCOL = NCOL + 1
               NUMCOL = NUMCOL + 1
            ELSE IF (RETCOD.EQ.1) THEN
	       NUMAR = NUMAR + 1
	    ELSE
               RETURN
            ENDIF
         ENDIF
         RETCOD = 0
         GOTO 20

C------> END OF THE CURENT FILE. CLOSE AND PRINT STATISTICS
50       CONTINUE
C     close the file and free the unit number
         call ftclos(read_unit, status)
         call ftfiou(read_unit, status)


	 WRITE(FCECHO_STR,'('' STATISTICS FOR THE CURRENT FILE'')') 
	 call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,'('' NUMBER OF EVENTS READ            ='',
     &                     I10)') NUMRED
         call FCECHO(FCECHO_STR) 
	 WRITE(FCECHO_STR,'('' NUMBER OF EVENTS IN TIME RANGE   ='',
     &                     I10)') NUMRD
	 call FCECHO(FCECHO_STR) 
	 WRITE(FCECHO_STR,'('' EVENTS IN EXCLUDED TIME RANGES   ='',
     &                     I10)') NUMEX
	 call FCECHO(FCECHO_STR) 
	 WRITE(FCECHO_STR,'('' EVENTS OUTSIDE OF ENERGY LEVELS  ='',
     &                     I10)') NUMEN
	 call FCECHO(FCECHO_STR) 
	 WRITE(FCECHO_STR,'('' EVENTS OUTSIDE OF E_CUTOFF ANGLE ='',
     &                     I10)') NUMZN
	 call FCECHO(FCECHO_STR) 
	 WRITE(FCECHO_STR,'('' EVENTS OUTSIDE OF SELECTED REGION='',
     &                     I10)') NUMAR
	 call FCECHO(FCECHO_STR) 
	 WRITE(FCECHO_STR,'('' EVENTS BELOW 6 MEV IN TASC       ='',
     &                     I10)') NUMTS
	 call FCECHO(FCECHO_STR) 
	 WRITE(FCECHO_STR,'('' EVENTS EXCLUDED FOR OTHER REASON ='',
     &                     I10)') NUMOT
	 call FCECHO(FCECHO_STR) 
         WRITE(FCECHO_STR,'('' NUMBER OF POINTS COLLECTED       ='',
     &                     I10)') NCOL
         call FCECHO(FCECHO_STR) 
      RETURN

C---> PROCESS I/O ERRORS ON EVENT FILE
C100   WRITE(FCECHO_STR,'(A)') 'BINEVT: I/O ERROR IN READING THE EVENT 
C     & INPUT FILE'
C      call FCECHO(FCECHO_STR) 
C      RETCOD = 8
C      RETURN

CHCCCCCCCCCCCCCCCCCCCCC END MAPGEN.SOURCE(BINEVT) CCCCCCCCCCCCCCCCCCCCCC
      END
