CCCCCCCCCCCCCCCCCCCCCCCC MAPGEN.SOURCE(GETEXT) CCCCCCCCCCCCCCCCCCCCCCCCC
C
CH1  ROUTINE NAME:  GETEXT
CH1
CH1  VERSION: 1.02              REVISION DATE: 03/21/91
CH1  VERSION: 1.03              REVISION DATE: 05/21/91
CH1  $Id: getext.f,v 1.8 2013/05/21 19:08:24 irby Exp $
CH1
CH1  PROGRAMMER(S) AND COMPLETION DATE:
CH1     ALBERT ETIENNE - S.T.X. - 02/13/89
CH1     E.S.Panduranga - S.T.X. - 05/21/91 
CH1
CH1  FUNCTION: GET THE THE EXCLUDED TIME RANGES FROM THE TIMELINE FILE.
CH1
CH1  SOFTWARE SYSTEM AND SPACECRAFT:  EGRET PROJECT
CH1
CH1  COMPUTER AND LANGUAGE:  IBM 3081 - VS FORTRAN
CH1
CH2  CALLING SEQUENCE:  CALL GETEXT(NX,XSTIME,XETIME,USRTIM,LINE,NL,IR)
CH2     ARGUMENT    TYPE   I/O                 DESCRIPTION
CH2     --------    ----   ---  ----------------------------------------
CH2     NX          I*4     O   NUMBER OF EXCLUDED TIME INTERVALS
CH2     XSTIME(*)   R*8     O   EXCLUDE INTERVAL START TIME ARRAY
CH2     XETIME(*)   R*8     O   EXCLUDE INTERVAL END TIME ARRAY
CH2     USRTIM(2)   R*8     I   USER TIME RANGE
CH2     LINE(*)     CH*64   O   INFORMATION ABOUT EACH TIME INTERVAL
CH2                             TO SAVE IN THE FITS FILE HEADER
CH2     NL          I*4    I/O  INDEX FOR THE LINE ARRAY
CH2     IR          I*4     O   RETURN CODE (0 = OK)
CH2
CH2  CALLED BY:  BINEVT
CH2
CH2  CALLS:
CH2   GRDTJD: CONVERT TIME FROM CALENDAR TO TJD/MSD FORMAT
CH2
CH3 COMMON USE: NONE
CH3
CH3 SIGNIFICANT LOCAL VARIABLES:  N.A.
CH3     VARIABLE   TYPE   INI. VAL.               DESCRIPTION
CH3     --------   ----   ---------  -----------------------------------
CH3     CURTIM     R*8        0      LAST TIME READ FROM TIMELINE FILE
CH3     NEWTIM     L*4        F      DETERMINES IF A NEW DATE OR TIME
CH3                                  WERE READ FROM TIMELINE FILE
CH3     STRING     CH*8      ' '     KEYWORD TO SEARCH TO END INTERVAL
CH3
CH4
CH4  METHOD:
CH4     WHILE (END TIME OF CURRENT INTERVAL NOT FOUND OR CURRENT TIME
CH4     IS BEFORE USER END TIME) DO
CH4        READ THE NEXT TIMELINE FILE ENTRY (SKIP COMMENT RECORDS)
CH4        SAVE THE CURRENT TIME READ (IF ANY) AND CONVERT IT TO AN R*8
CH4        IF (A "START EXCLUDED INTERVAL" KEYWORD IS FOUND) THEN
CH4           IF (NO EXCLUDED INTERVAL IS CURRENTLY PENDING) THEN
CH4              SAVE THE CURRENT TIME AS THE START TIME OF THE INTERVAL
CH4              SET A FLAG TO DETERMINE THAT AN INTERVAL IS PENDING
CH4           END IF
CH4        ELSE IF (KEYWORD CORRESPONDS TO END OF PENDING INTERVAL)
CH4           IF (INTERVAL IS WITHIN USER TIME RANGE) THEN
CH4              INCREMENT THE NUMBER OF EXCLUDED INTERVALS SAVED
CH4              SAVE THE END TIME OF THE INTERVAL
CH4           END IF
CH4           SET A FLAG TO DETERMINE THAT NO INTERVAL IS PENDING
CH4        END IF
CH4     END WHILE
CH4     IF (AN INTERVAL IS PENDING) THEN
CH4        IF (INTERVAL IS WITHIN USER TIME RANGE) THEN
CH4           INCREMENT THE NUMBER OF EXCLUDED INTERVALS SAVED
CH4           SAVE THE END TIME OF THE INTERVAL
CH4        END IF
CH4     END IF
CH4  END GETEXT
CH4
CH5  MODIFICATIONS BETWEEN VERSIONS:
CH5     MOD #   MODIFIER    DATE                  DESCRIPTION
CH5     -----   --------  --------   -----------------------------------
CH5     1.02    A.ETIENNE 03/21/91   ADDED THIS ROUTINE TO READ THE
CH5                                  TIMELINE FILE AND GET THE EXCLUDED
CH5                                  TIME RANGES.
CH5     2.00    E.S.Panduranga 05/21/91
CH5			Changed character var*siz(dim) to character*siz var(dim)
CH5			Changed constant F to .flase.
CH5			Added timeline variable to open timeline file.
CH5			Merged changes of version 1.04 on IBM
CH5 $Log: getext.f,v $
CH5 Revision 1.8  2013/05/21 19:08:24  irby
CH5 Change character*n to character(n) to silence warnings: "Obsolescent
CH5 feature: Old-style character length".
CH5
CH5 Revision 1.7  2005/08/26 19:52:02  irby
CH5 gfortran/g95 does not allow equivalence statements containing references
CH5 to substrings, e.g. "equivalence (linbuf(32:39),keywrd)", so these have
CH5 been reworked.
CH5
CH5 Revision 1.6  1998/11/18 15:36:27  silvis
CH5 Allow program to write files with just one energy level.
CH5
CH5 Have program check for local version of the timeline file.
CH5 If no local version of the timeline files exists then use the
CH5 copy in greo DATADIR directory.
CH5
CH5 Revision 1.5  1998/10/09 00:31:51  peachey
CH5 Changed references to addbin to addbin_m and celgal to celgal_m
CH5
c Revision 1.4  1998/10/06  18:32:24  silvis
c Have the location of the timeline file come through XPI
c
CH5 Revision 1.3  1998/10/06 12:52:21  silvis
CH5 A stop was removed from the main program so that the learn feature
CH5 for the par file would work properly and some minor changes were made
CH5 to the par file.
CH5
C Revision 2.6  1997/10/09  13:56:51  wang
C *** empty log message ***
C
C Revision 2.5  1993/09/14  19:27:03  albert
C Increased size of arrays to store excluded time ranges to allow large
C number of input SMDB files to be processed.
C
C Revision 2.4  1992/07/14  20:32:15  albert
C Modified to always read the pointing direction of the instrument from the
C timeline file instead of reading it only if within the user time range.
C
C Revision 2.3  1992/07/10  14:27:22  albert
C Modified to read the TASC in and out time ranges from the timeline file.
C
C Revision 2.2  1992/03/27  16:05:57  albert
C Read the pointing direction from the timeline file and convert to both
C galactic and celestial systems. Compute the total excluded time.
C
C Revision 2.1  1991/08/28  19:36:16  esp
C First really controlled version on SUN (ported from IBM 3081)
C
C Replaced UPCASE with the fitsio function FTUPCH to make the 
c keywrd variable uppercase.
C**********************************************************
C
C     Ftool Change
C
C     1. The timeline file must reside in the current directory.
C     2. All output to the screen is now done through 
C      the platform independent function fcecho.
C
C     J. Silvis
C     RSTX
C     Sept 1998
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE GETEXT(NX,XSTIME,XETIME,EXTIME,USRTIM,PDIREC,CVT,LINE,
     &   NL,NT,TSTIME,TETIME,IRET)
      implicit none 

      REAL      RA,DEC,GLON,GLAT,PDIREC(8),CVT
C      
C      The character variable below has been added 
C      so that the code can use the fitsio routine 
C      fcecho. 
C      
C      Jeff Silvis 
C      RSTX 
C      23  July  98
 
      character(250) FCECHO_STR 
      REAL*8    CONVRT,CURTIM,EXTIME,XSTIME(*),XETIME(*),USRTIM(2)
      REAL*8	TSTIME(*),TETIME(*)
      INTEGER   NX,NT,ITJD,IMSD,YY,MM,DD,HR,MN,SC,MIL,IRET
      LOGICAL   NEWTIM,TOUT,HERE
      CHARACTER STRING*8,KEYWRD*8,LINE(*)*64,TLINE*128

Cesp  ! declaring variable NL !
      integer	nl

      character(80)	timeline
      integer status,lun_time

C**********************************************************
C
C     Ftool Change
C
C     i is used to find the name of the timeline file.  
C  This name is now fixed as "timeline".
C
C     J. Silvis
C
C      integer		i
C
C*************************************************************

      DATA STRING/' '/, CURTIM/0/, NEWTIM/.false./

      character(80)	id
      common	/id/	id
      id = '$Id: getext.f,v 1.8 2013/05/21 19:08:24 irby Exp $'

      status=0
C**********************************************************
C
C     Ftool Change
C
C
C    If the timeline file exists ont he current disk then use it. 
C    If not then the timeline file location is called through XPI 
C    and is in the refdata directory. 
C
C     J. Silvis
C

      inquire(file='timeline',exist=here)
      if(here)then
           timeline = 'timeline'
      else
           call uclgst('timeline',timeline,status)
      endif



C******************************************************
C
C            Ftool Change
C
C      The variable lun_time provides a logicial number that is
C      is used for output to an ascii file.  In the original code this
C      was hardwired to be 8.  
C
C
C      Jeff Silvis  28 April 1997
C
C******************************************************
      call ftgiou(lun_time, status)
      open(unit=lun_time, status='old',file=timeline, err=50)

C---> READ EACH TIMELINE FILE ENTRY UNTIL CURRENT TIME > USER END TIME
      NX = 0
      NT = 0
      TOUT = .FALSE.
      DO WHILE (STRING.NE.' ' .OR. CURTIM.LE.USRTIM(2))
         READ(lun_time,'(A80)',END=20) TLINE
         IF (TLINE(1:1) .EQ. '*') GOTO 10

C------> GET THE CURRENT TIME (IF THERE IS ONE ON THIS LINE)
         IF (TLINE(8:15) .NE.' ') THEN
            READ(TLINE(8:15),'(3(I2,1X))') MM,DD,YY
            NEWTIM = .TRUE.
         END IF
         IF (TLINE(18:29).NE.' ') THEN
            READ(TLINE(18:29),'(3(I2,1X),I3)') HR,MN,SC,MIL
            NEWTIM = .TRUE.
         END IF
         IF (NEWTIM) THEN
            if (YY .le. 65) YY = YY + 100
            CALL GRDTJD(ITJD,IMSD,YY+1900,MM,DD,HR,MN,SC,MIL,IRET)
            IF (IRET.NE.0) GOTO 100
            CURTIM = CONVRT(ITJD,IMSD)
            NEWTIM = .FALSE.
         END IF
C
C       Convert keywrd to upper case
C
        KEYWRD=TLINE(32:39)
        call ftupch(KEYWRD)


C------> CHECK IF THE POINTING DIRECTION WAS READ
         IF (KEYWRD.EQ.'SC-Z-CEL') 
     &      READ(TLINE(40:70),*) PDIREC(3),PDIREC(4)
         IF (KEYWRD.EQ.'SC-X-CEL') 
     &      READ(TLINE(40:70),*) PDIREC(7),PDIREC(8)

C------> CHECK IF THE TASC IS COMING IN OR OUT OF COINCIDENCE
	 IF (CURTIM .GE. USRTIM(1)) THEN
            IF (KEYWRD.EQ.'TASCOUT') THEN
               IF (TOUT) THEN
	          WRITE(FCECHO_STR,'(A)') 'GETEXT - WARNING TASC OUT
     &  FOUND TWICE'
	          call FCECHO(FCECHO_STR) 
	       ELSE
	          TSTIME(NT+1) = CURTIM
	          TOUT = .TRUE.
	       ENDIF
            ELSE IF (KEYWRD.EQ.'TASCIN') THEN
               IF (TOUT) THEN
		  NT = NT + 1
		  if (nt .gt. 100) goto 300
	          TETIME(NT) = CURTIM
	          TOUT = .FALSE.
	       ELSE
	          WRITE(FCECHO_STR,'(A)') 'GETEXT - WARNING TASC IN
     &  WITHOUT TASC OUT'
	          call FCECHO(FCECHO_STR) 
	       ENDIF
	    ENDIF
	 ENDIF

C------> CHECK IF ONE OF THE EXCLUDED TIMES KEYWORD WAS READ
         IF (KEYWRD.EQ.'EXCLUDE '.OR.KEYWRD.EQ.'CALIBRAT'.OR.
     &   KEYWRD.EQ.'ALBEDO M'.OR.KEYWRD.EQ.'TEST MOD') THEN
            IF (STRING.EQ.' ') THEN
               XSTIME(NX+1) = CURTIM
               if (nl.lt.70) WRITE(LINE(NL+1),1000)
     &		  KEYWRD,DD,MM,YY,HR,MN,SC,MIL
               STRING = 'END ' // KEYWRD(1:8)
            END IF

C------> IF THE END OF THE CURRENT EXCLUDED TIME IS FOUND, SAVE IT
         ELSE IF ((KEYWRD.EQ.STRING) .AND. (STRING.NE.' ') ) THEN
            IF ( (USRTIM(1).LE.CURTIM) .AND. 
     &            (USRTIM(2).GE.XSTIME(NX+1))  ) THEN
               NX = NX + 1
	       if (nx .gt. 500) goto 200
               XETIME(NX) = CURTIM
	       if (nl .lt. 70) then
                  NL = NL + 1
                  WRITE(FCECHO_STR,2000)  LINE(NL)(1:8),MM,DD,YY,HR,MN
     &             ,SC,MIL,ITJD,IMSD
                  call FCECHO(FCECHO_STR) 
                  WRITE(LINE(NL)(32:64),3000) DD,MM,YY,HR,MN,SC,MIL
	          if (nl .eq. 70) then
                      WRITE(FCECHO_STR,'(A,A)') 'GETEXT - Warning: ',
     &                  'Exclude times will no more be recorded in 
     & header'
                      call FCECHO(FCECHO_STR) 
                  end if
	       end if
	       EXTIME = EXTIME + DMIN1(USRTIM(2),XETIME(NX)) -
     &               DMAX1(USRTIM(1),XSTIME(NX))
            ENDIF
            STRING = ' '
         END IF

10       CONTINUE
      END DO
      
C---> CONVERT THE POINTING DIRECTION TO GALACTIC COORDINATES
      RA = PDIREC(3)*CVT
      DEC = PDIREC(4)*CVT
      CALL CELGAL_M('CG',RA,DEC,GLON,GLAT,IRET)
      PDIREC(1) = GLON/CVT
      PDIREC(2) = GLAT/CVT
      RA = PDIREC(7)*CVT
      DEC = PDIREC(8)*CVT
      CALL CELGAL_M('CG',RA,DEC,GLON,GLAT,IRET)
      PDIREC(5) = GLON/CVT
      PDIREC(6) = GLAT/CVT

      RETURN

C---> AT THE END OF THE LOOP CHECK IS AN EXCLUDED TIME IS PENDING
20    CONTINUE
      IF (STRING.NE.' ') THEN
         IF ( (USRTIM(1).LE.CURTIM) .AND. 
     &     (USRTIM(2).GE.XSTIME(NX+1)) ) THEN
            NX = NX + 1
            NL = NL + 1
            XETIME(NX) = CURTIM
            WRITE(FCECHO_STR,2000)  LINE(NL)(1:8),MM,DD,YY,HR,MN,SC,
     &       MIL,ITJD,IMSD
            call FCECHO(FCECHO_STR) 
            WRITE(LINE(NL)(32:64),3000) MM,DD,YY,HR,MN,SC,MIL
         ENDIF
      END IF

C---> CONVERT THE POINTING DIRECTION TO CELESTIAL COORDINATES
      RA = PDIREC(3)*CVT
      DEC = PDIREC(4)*CVT
      CALL CELGAL_M('CG',RA,DEC,GLON,GLAT,IRET)
      PDIREC(1) = GLON/CVT
      PDIREC(2) = GLAT/CVT
      RA = PDIREC(7)*CVT
      DEC = PDIREC(8)*CVT
      CALL CELGAL_M('CG',RA,DEC,GLON,GLAT,IRET)
      PDIREC(5) = GLON/CVT
      PDIREC(6) = GLAT/CVT

      RETURN

C---> INVALID TIME
 50   CONTINUE
       WRITE(FCECHO_STR,'(A)') ' '
      call FCERR(FCECHO_STR) 
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') 'Error reading the Timeline file.'
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') '**********************************'
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') 'The timeline file is in ascii  ' 
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') 'It records the status of EGRET  ' 
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') 'during an observation.' 
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') '**********************************'
      call FCERR(FCECHO_STR)
      WRITE(FCECHO_STR,'(A)') 'It MUST be in the current directory' 
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') '**********************************'
      call FCERR(FCECHO_STR)
      WRITE(FCECHO_STR,'(A)') 'If it is missing obtain it from the ' 
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') 'HEASARC through W3BROWSE or their' 
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') 'current archive browse system'
      call FCERR(FCECHO_STR) 
      WRITE(FCECHO_STR,'(A)') ' '
      call FCERR(FCECHO_STR) 
      call FCERR(FCECHO_STR) 
       WRITE(FCECHO_STR,'(A)') 'MAPGEN PROGRAM TERMINATED DUE TO ERROR '
      call FCERR(FCECHO_STR)       
      stop


100   CONTINUE
      WRITE(FCECHO_STR,'(A)') 'GETEXT - ERROR: INVALID TIME FOUND IN 
     & TIMELINE FILE'
      call FCECHO(FCECHO_STR) 
      WRITE(FCECHO_STR,'(I12,I12,I12,I12,I12,I12,I12,I12,I12)') MM,DD,
     &  YY,HR,MN,SC,MIL,ITJD,IMSD
      call FCECHO(FCECHO_STR) 
      RETURN

C---> Too many excluded times
200   CONTINUE
      WRITE(FCECHO_STR,'(A)') 'GETEXT - ERROR: more than 500 excluded 
     & times found'
      call FCECHO(FCECHO_STR) 
      iret = 1
      RETURN

C---> Too many TASC in and out of coincidence
300   CONTINUE
      WRITE(FCECHO_STR,'(A)') 'GETEXT - ERROR: more than 100 TASC in 
     & and out'
      call FCECHO(FCECHO_STR) 
      iret = 1
      RETURN

1000  FORMAT(A8,'= ',2(I2.2,'/'),I2.2,',',2(I2.2,':'),I2.2,'.',I3.3)
2000  FORMAT(' EXCLUDED TIME (',A8,') = ',2(I2.2,'/'),I2.2,1X,
     &       2(I2.2,':'),I2.2,'.',I3.3,3X,'(',I5,',',I8,')')
3000  FORMAT(' - ',2(I2.2,'/'),I2.2,',',2(I2.2,':'),I2.2,'.',I3.3)
CCCCCCCCCCCCCCCCCCCC END EXPHST.SOURCE(GETEXT) CCCCCCCCCCCCCCCCCCCCCCCCC
      END
