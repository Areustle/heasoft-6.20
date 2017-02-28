      subroutine FADMAP
C
C----------------------------------------------------------------------
C     THIS PROGRAM SELECTS EVENTS WHICH FULFILL SPECIFIED REQUIREMENTS
C     FOR TIME INTERVALS SELECTED BY OBSERVATION PERIODS IN FITS-FILES.
C     IT PRODUCES COUNT, INTENSITY AND EXPOSURE MAPS FOR SELECTABLE
C     REGIONS OF THE SKY. THE MAPS CAN BE CORRECTED FOR THE EFFECTS
C     OF KNOWN INPUT SPECTRA OF OBSERVED RADIATION AND OF INSTRUMENTAL
C     BACKGROUND. THE KNOWN VARIATIONS OF EXPERIMENT SENSITIVITY AND
C     INSTRUMENTAL BACKGROUND ARE TAKEN INTO ACCOUNT. THE OUTPUT MAPS ARE
C     STORED IN FITS FORMAT, WHICH ALLOWS EASY ACCESS FOR FURTHER
C     PROCESSING. THIS PROGRAM WAS MODIFIED TO WORK ON COS-B OR SAS-2
C     DATA BY PAUL BARRETT AND BRENDAN PERRY AT NASA/GSFC'S HEASARC
C     IN 1993 FROM AN ORIGINAL PROGRAM OBTAINED FROM HANS MAYER-
C     HASSLEWANDER AT INSTITUTE FUR ASTROPHYSIK
C
C     August, 1994 1.0 (EAG) Converted to FTOOL format
C     September, 1994 1.1 (EAG) Increased use of dynamic memory
C     January, 1995 1.2 (EAG) changed name template of calibration files
C                             removed init_real_array to misc.for
C                             increased buffersize for exposures to 40K
C     March, 1998 1.3 (toliver) Revised file read_subs.f, replacing all
C                               calls to obsolete fitsio routine 'ftgbnh'
C                               with calls to new cfitsio function 'ftghbn',
C                               hard-wiring the value "99" for the maximum
C                               columns parameter as I have neither the
C                               ambition nor the level of familiarity with
C                               this code to find all of relevant "99"s for
C                               this task and replace them with a PARAMETER
C                               statement symbol
C
C----------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      INTEGER good_expo, good_phot , icode , xcode
      integer mapp_ptr, sarmat_ptr, bkgmat_ptr, status, nelem
      logical mappok, sarmatok, bkgmatok

      character(40) taskname
      common /task/ taskname

      taskname = 'FADMAP 1.3'
      sarmatok = .false.
      status = 0
      call ftcmsg
C
C START UP THE XPI INTERFACE, THEN GET THE SELECTION CRITERIA
C FROM THE FADMAP.PAR FILE.
C
C      CALL TBLDSTAND('fadmap','LHEAVX$DKA700','PERRY.FTOOLS.UPARM',ierr)
C
C EVPA RETRIEVES ALL THE REMAINING XPI PARAMETERS, INCLUDING THE 'HIDDEN'
C ONES (ONES THAT GENERALLY WILL REMAIN CONSTANT THROUGHOUT PROCESSING OF
C ONE SATELLITE'S DATA. E.G. THE NUMBER OF INCLINATION FILES WILL ALWAYS
C BE 7 FOR COS-B.
C
      CALL EVPA
C
C GET THE EXPOSURE DATA FROM THE 'TSI' EXTENSION OF THE FITS FILE
C
      call fcecho (' ')
      call fcecho (' ...loading exposure data')
      call fcecho (' ')
      X_row = 0
      CALL READ_EXPOSURE
C
C GET THE EVENTS DATA FROM THE 'EVENTS' EXTENSION OF THE FITS FILE
C
      call fcecho (' ')
      call fcecho (' ...loading events data')
      call fcecho (' ')
      P_row = 0
      CALL READ_EVENTS

      if (clo.eq.-99.0.and.cla.eq.-99)then
	 if (coord.eq.1)then
            clo = raob
            cla = decob
	 else
            clo = lob
            cla = bob
	 endif
      endif	

C
C CALFIL READS ALL APPROPRIATE CALIBRATION FILES AND UPDATES ALL THE
C EXPOSURE MAP ARRAYS WITH THE APPROPRIATE DATA
C
      call fcecho (' ')
      call fcecho (' ...loading calibration data')
      call fcecho (' ')
      CALL CALFIL
C
C ESTABLISH THE COORDINATE SYSTEM ARRAYS FOR MAP USE. THIS SUBROUTINE
C GRABS EITHER ALL THE RA/DEC OR L/B VALUES FROM THE FITS FILE DEPENDING
C ON WHICH COORDINATE SYSTEM THE USER DESIRES. IT GRABS THE APPROPRIATE
C COORDINATES FROM BOTH THE EXPOSURE AND PHOTON EXTENSIONS. IT RETURNS
C THE ARRAYS XPCOORD1/XPCOORD2 AND PHCOORD1/PHCOORD2.
C
      CALL SET_COORD
C
C LOOP THROUGH EXPOSURE AND EVENT ROWS
C
      call fcecho (' ')
      call fcecho (' ...one moment...processing data')
      call fcecho (' ')

C dynamically allocate arrays

C mapp (Nla/Dla, Nlo/Dlo, 4)
      mappok = .false.
      mappx = INT(Nla/Dla)
      mappy = INT(Nlo/Dlo)
      nelem = mappx*mappy*4
      mapp_ptr = 0 
      call udmget (nelem, 6, mapp_ptr, status)
      if (status .ne. 0) then
         context = ' error allocating mapp memory'
         call fcerr (context)
         goto 1000
      else
         mappok = .true.
C initialize to 0
         call initrealarray(memr(mapp_ptr), nelem)
      endif

C allocate sarmat array
      sarmatok = .false.
      nelem = mappx*mappy*2
      sarmat_ptr = 0
      call udmget (nelem, 6, sarmat_ptr, status)
      if (status .ne. 0) then 
         context = ' Error allocating sarmat memory'
         call fcerr (context)
         goto 1000
      else
         sarmatok = .true.
         call initrealarray(memr(sarmat_ptr), nelem)
      endif

C allocate bkgmat array
      bkgmatok = .false.
      nelem = mappx*mappy
      bkgmat_ptr = 0
      call udmget (nelem, 6, bkgmat_ptr, status)
      if (status .ne. 0) then 
         context = ' Error allocating bkgmat memory'
         call fcerr (context)
         goto 1000
      else
         bkgmatok = .true.
         call initrealarray(memr(bkgmat_ptr), nelem)
      endif
      
      
      DO WHILE ( X_row.LE.Nrowsxp )
C
C X_ROW IS THE CURRENT EXPOSURE RECORD
C
         X_row = X_row + 1
C
C CHECK_EXPOSURE CHECKS THE VALIDITY OF EACH EXPOSURE IN THE DATA FILE
C WITH THE USER'S INPUT PARAMETERS AND ESTABLISHES SOME CONSTANTS FOR
C LATER PROCESSING, ALSO BASED ON USER INPUT. XCODE = 0 IS INDICATIVE
C OF A GOOD EXPOSURE.
C
         xcode = 0
         CALL CHECK_EXPOSURE(xcode)
         WRITE (context,99001) X_row , P_row + 1
         call fcecho (context)
C
C xcode = 0  exposure is valid, process
C xcode = 1  none of the exposures are within the user's time
C	     constraint, the file itself remains unprocessed and fadmap 
C	     is exited.
C xcode = 2  exposure is valid, but is not processed because it is either
C	     at latitude above or below 65 degrees latitude, or not within
C 	     user specified time constraints 
C xcode = 3  program has processed all exposures within the user specified
C	     time constraint, proceed to fits writing routines
C
         IF ( xcode.EQ.1 ) THEN
            GOTO 999
         ELSEIF ( xcode.EQ.2 ) THEN
            GOTO 100
         ELSEIF ( xcode.EQ.3 ) THEN
            GOTO 200
         ENDIF

         good_expo = good_expo + 1
C
C CALL SENMAT TO FILL ARRAYS USED TO UPDATE THE OTHER THREE MAPS;
C APPLY THE SENSITIVE AREA MAPS CREATED FROM THE CALIBRATION FILES
C
C
         CALL SENMAT (memr(sarmat_ptr), memr(bkgmat_ptr))
C
C UPDATE EXPOSURE AND BACKGROUND WORK ARRAYS, FILL THE EXPOSURE MAP,
C THE BACKGROUND MAP AND THE FLUX MAP MAPP(LA,LO,2:3:4)
C
         CALL EXPBKG(memr(mapp_ptr), memr(sarmat_ptr), memr(bkgmat_ptr))
         
C
C LOOP THROUGH PHOTON EVENTS FROM DATA FILE
C
         DO WHILE ( P_row.LE.Nrowsph )
            
            P_row = P_row + 1
C
C P_ROW IS THE CURRENT PHOTON RECORD BEING PROCESSED
C
C
C CHECK EVENT FOR ACCEPTANCE. ICODE=0 IS ACCEPTABLE.
C NEWCODE = 1 INDICATES ALL THE PHOTONS IN THE CURRENT EXPOSURE
C HAVE BEEN PROCESSED. INCREMENT THE EXPOSURE NUMBER BY ONE
C AND REPEAT PROCESS. WHEN THE CURRENT PHOTON ROW NUMBER EXCEEDS
C THE NUMBER OF EVENT ROWS IN THE FITS FILE (GIVEN BY THE VALUE
C NROWSPH), TERMINATE PROCESSING AND GO TO THE WRITE SECTION)
C
C
            icode = 0
            CALL CHECK_EVENT(icode)
C         WRITE (6,99001) X_row , P_row
C
C icode = 0  exposure is valid, process.
C icode = 1  finished processing all events within current exposure,
C	     get the next exposure.
C icode = 2  current exposure is valid, but current photon within this
C	     exposure has been rejected for some reason, keep getting
C	     photons.
C icode = 3  program has processed all exposures and all photons either
C	     within user specified constraints or within the whole file,
C	     proceed to fits writing routines.
C
            IF ( icode.EQ.1 ) THEN
               GOTO 100 
            ELSEIF ( icode.EQ.2 ) THEN
               GOTO 50
            ELSEIF ( icode.EQ.3 ) THEN
               GOTO 200 
            ENDIF
            
C
C ADD ACCEPTED EVENT TO SOURCE COUNTS PART OF MAPP ARRAY, ICODE = 0
C
            good_phot = good_phot + 1
            CALL ADD_EVENT(memr(mapp_ptr), memr(sarmat_ptr))
 50      ENDDO
 100  ENDDO
C
C     ALL EXPOSURES AND EVENTS HAVE NOW BEEN PROCESSED
C
C     STORE THE 4 MAPS IN 4 SEPARATE FITS FILES
C
 200  IF ( good_phot.NE.0 ) THEN
         call fcecho (' ')
         call fcecho (' ...creating FITS format files')
         call fcecho (' ')
         CALL CREATE_FITS(memr(mapp_ptr), 1)
         CALL CREATE_FITS(memr(mapp_ptr), 2)
         CALL CREATE_FITS(memr(mapp_ptr), 3)
         CALL CREATE_FITS(memr(mapp_ptr), 4)
      ENDIF
      WRITE (context,99003) X_row , good_expo
      call fcecho (context)
      write (context, 99005) P_row , good_phot
      call fcecho (context)
      GO TO 1000 

99001 FORMAT (1x,' processing exposure number: ',i6,5x,'event number: ',
     &     i6)
99002 FORMAT (1x,' writing the ',i6,' photons to a FITS file')
99003 FORMAT (1x,'total exposures: ',i6,2x,'exposures processed: ',i6)
99005 FORMAT (1x,'total photons: ',i6,2x,'photons processed: ',i6)
99004 FORMAT (1x,' None of the exposures of file ',a14, 'are within the
     &     times selected',/,' *** FADMAP exited ***')
 999  WRITE (context,99004) data_file
      call fcerr (context)
 1000 if (mappok) call udmfre (mapp_ptr, 6, status)
      if (sarmatok) call udmfre (sarmat_ptr, 6, status)
      return
      END
