C*********************************************************************
C SELECTOR TASK:
C      			rate
C
C FILE:
C      rate.f
C
C DESCRIPTION:
C      
C
C AUTHOR:
C	Steve Snowden
C       FTOOLS development by Srilal Weera  
C
C MODIFICATION HISTORY:
C       02/18/98: PDW - Redimension TYPE/FORM/UNIT arrays to correct lengths
C       01/05/00: NG  - Changed filenames from uppercase to lowercase.
C
C NOTES:
C   
C
C USAGE:
C      HOST: call rate
C      IRAF: task rate
C
C ARGUMENTS:
C      none
C
C
C CALLED ROUTINES:
C      subroutine grate - gets parameters from parameter file
C      subroutine rdval -  reads valid_times_all.dat file to evaluate
C                            the gain control
C
C******************************************************************************




			 subroutine rate


C        PROGRAM RATE
C
C  Author: Steve Snowden
C  Date:   
C  Update:  1 October 1993
C  Update: 19 April 1994      enable RDF files
C  Update: 18 January 1995    enable point-source masking
C
C  This program sorts through a ROSAT event file and bins the data into
C  count-rates for 30-second intervals in seven pulse-height bands.  It
C  also calculates the particle background for the three bands for the
C  intervals.
C
C  This program has been converted to work on the FITS files provided
C  in the REV0 data sets of both German (files *EVENTS.MT and 
C  *EVENTRATES.MT) US (files *.fits and *.evr) data, and for RDF files 
C  (*.anc and *.bas).  The files must be renamed to xxx_events.fits 
C  and xxx_eventrates.fits for the REV0 data and xxx_anc.fits and 
C  xxx_bas.fits for the RDF data where xxx is any 
C  string up to eight characters long; the string is queried for by the 
C  program.  An accepted time file, "valid_times_all.dat", is also used 
C  by the program.  This file should be in the same format as the output 
C  of the program "get_valid_times.f", which has the required name.  This 
C  file can be created from the accepted times given in the GTI file of 
C  the observation data set.  The valid_times_all.dat file is an ascii 
C  list of counter, start, stop times in S/C seconds.
C
C  Updated to work with RDF files.  *.anc -> xxx_anc.fits,
C  *.bas -> xxx_bas.fits
C
C  The program follows the suggestions of Snowden et al. (1992, ApJ,
C  393 819) and Plucinsky et al. (1993, ApJ, in press) to exclude
C  regions of the PSPC near the edges of the PSPC which are strongly
C  affected by the particle background, the "bright line" regions.
C  The program also assumes that a selection has been done on the
C  data to exclude all events which follow within 0.35 ms of a
C  "precursor" event.  This excludes some of the low pulse-height
C  crud which affects data collected after May 1992.
C
C  Besides FITSIO routines, RATE requires the subroutine LIVTIM which 
C  calculates the deadtime correction and SORT28, a modification
C  of the Numerical Recipes SORT2 routine, which sorts the events
C  into a time-ordered series.
C
C  The output is used in RATE_FIT.FOM
C
        IMPLICIT NONE
C
        INTEGER*2 IPHT(2000000), IDXI, IDYI, INI2(512,512), ISI,IIX,IIY
        INTEGER*4 ACTBEG(100), ACTEND(100), BLOCKA, BLOCKE, END, 
     +      HDUTYPEA, HDUTYPEE, I, IA, IAA, IA1LL, IAEXE, IAXE, 
     +      IB1, IB2, IB3, IDELT, IDX, IDY, IE, IEE, IERR, IEXP, 
     +      INI4(512,512), 
     +      IG, IGG, II, IMASK, INTER, INTERH, IOS, 
     +      IS,  ISTYLE, ITEMP, ITEXP, IX, IY, 
     +      LIVEND, LUN, LUND, LUNE, LUNS, MV, N, NEW, NROWSA, NROWSE, 
     +      NUM, OLD, ROWLENA, ROWLENE, START, STATUS, TBCOLA(10), 
     +      TBCOLE(20), TFIELDSA, TFIELDSE, TIME, VARIDATA, VARIDATE

C       INTEGER*4 IPIOUT, ISCS, IPIIN
C
        REAL*4    A1LL, AEXE, AMV, AVEMV, AXE, DEADTP, DUM1, DUM2, 
     +      DUM3, DUM4, EXP, FLIVE, FLIVE1, FLIVE2, MASK(512,512), 
     +      R1, R2, R3, R4, R5, R6, R7, RAL, REXT, 
     +      RINT, RMV, SP1, SP1S, SP2, SP2S, SP3, SP3S, 
     +      SP4, SP4S, SP5, SP5S, SP6, SP6S, SP7, SP7S, 
     +      TOTEXP, X(5000), XX, YY
        REAL*8 DTIME, DTIMEO, DTEVE(2000000)
C
        character(1) OBS(8), OBS1
        character(2) OBS2
        character(3) OBS3
        character(4) OBS4
        character(5) OBS5
        character(6) OBS6
        character(7) OBS7
        character(8) OBS8
        character(8) TTYPEA(10), TTYPEE(35), TFORMA(10), TFORME(35),
     +      TUNITA(10), TUNITE(35)
        character(20) EXTNAMEA, EXTNAMEE
        character(80) INDAT, INEVE, INMASK, context
        LOGICAL ANYFA, BADSTA, FLAGVALA, ANYFE, FLAGVALE,R1LBAND
C
        EQUIVALENCE (OBS(1),OBS1,OBS2,OBS3,OBS4,OBS5,OBS6,OBS7,OBS8)
        EQUIVALENCE (MASK,INI4,INI2)
C
        DATA DEADTP /234./
        DATA INTER /30/
        DATA LUNS /95/

      	character(40) taskname
      	common /task/ taskname

      	taskname = 'rate v1.0b'
      	call ftcmsg
C Init
	iexp =0
	num = 0
	old =0
	start =0
	amv =0.0
        exp =0.0
	badsta =.false.

C  get parameters from parameter file
       call grate(OBS, ISTYLE, R1LBAND, IMASK, INMASK, STATUS)
       if (status .ne. 0) then
      context = 'rate.f: Error in obtaining parameters'
      call fcerr(context)
       goto 999
      endif

C
        IDELT = INTER - 1
        INTERH = INTER/2
C
C  Read in the observation control
C
C
C  Now set up the input file names
C
        IF(OBS8 .NE. '        ') THEN
            DO I=2,8
                IF((OBS(I) .EQ. ' ') .AND. (OBS(I-1) .NE. ' '))
     +                  NUM = I - 1
            ENDDO
            IF(NUM .EQ. 0) NUM = 8
            IF(NUM .EQ. 1) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INDAT,3010) OBS1
 3010               FORMAT(A1,'_bas.fits')
                    WRITE(INEVE,3015) OBS1
 3015               FORMAT(A1,'_anc.fits')
                ELSE
                    WRITE(INDAT,3020) OBS1
 3020               FORMAT(A1,'_events.fits')
                    WRITE(INEVE,3025) OBS1
 3025               FORMAT(A1,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 2) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INDAT,3030) OBS2
 3030               FORMAT(A2,'_bas.fits')
                    WRITE(INEVE,3035) OBS2
 3035               FORMAT(A2,'_anc.fits')
                ELSE
                    WRITE(INDAT,3040) OBS2
 3040               FORMAT(A2,'_events.fits')
                    WRITE(INEVE,3045) OBS2
 3045               FORMAT(A2,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 3) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INDAT,3050) OBS3
 3050               FORMAT(A3,'_bas.fits')
                    WRITE(INEVE,3055) OBS3
 3055               FORMAT(A3,'_anc.fits')
                ELSE
                    WRITE(INDAT,3060) OBS3
 3060               FORMAT(A3,'_events.fits')
                    WRITE(INEVE,3065) OBS3
 3065               FORMAT(A3,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 4) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INDAT,3070) OBS4
 3070               FORMAT(A4,'_bas.fits')
                    WRITE(INEVE,3075) OBS4
 3075               FORMAT(A4,'_anc.fits')
                ELSE
                    WRITE(INDAT,3080) OBS4
 3080               FORMAT(A4,'_events.fits')
                    WRITE(INEVE,3085) OBS4
 3085               FORMAT(A4,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 5) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INDAT,3090) OBS5
 3090               FORMAT(A5,'_bas.fits')
                    WRITE(INEVE,3095) OBS5
 3095               FORMAT(A5,'_anc.fits')
                ELSE
                    WRITE(INDAT,3100) OBS5
 3100               FORMAT(A5,'_events.fits')
                    WRITE(INEVE,3105) OBS5
 3105               FORMAT(A5,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 6) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INDAT,3110) OBS6
 3110               FORMAT(A6,'_bas.fits')
                    WRITE(INEVE,3115) OBS6
 3115               FORMAT(A6,'_anc.fits')
                ELSE
                    WRITE(INDAT,3120) OBS6
 3120               FORMAT(A6,'_events.fits')
                    WRITE(INEVE,3125) OBS6
 3125               FORMAT(A6,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 7) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INDAT,3130) OBS7
 3130               FORMAT(A7,'_bas.fits')
                    WRITE(INEVE,3135) OBS7
 3135               FORMAT(A7,'_anc.fits')
                ELSE
                    WRITE(INDAT,3140) OBS7
 3140               FORMAT(A7,'_events.fits')
                    WRITE(INEVE,3145) OBS7
 3145               FORMAT(A7,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 8) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INDAT,3150) OBS8
 3150               FORMAT(A8,'_bas.fits')
                    WRITE(INEVE,3155) OBS8
 3155               FORMAT(A8,'_anc.fits')
                ELSE
                    WRITE(INDAT,3160) OBS8
 3160               FORMAT(A8,'_events.fits')
                    WRITE(INEVE,3165) OBS8
 3165               FORMAT(A8,'_eventrates.fits')
                ENDIF
            ENDIF
        ENDIF
C
C  Read in the 'GAIN CONTROL (IGG)' from READVAL subroutine
	call rdval (R1LBAND,IGG, status)
C

        IF(IMASK .EQ. 1) THEN
C  the following is read in grate 
c           call fcecho('Enter mask file name:')
c           READ 1000, INMASK

C
C  Return the mask array
C
            LUN = 90
            CALL RETFIT (LUN,INMASK,MASK,INI2,INI4,1,512,DUM1,
     +          DUM2,DUM3,DUM4,STATUS)
        ELSE
            DO II=1,512
                DO I=1,512
                    MASK(I,II) = 1.
                ENDDO
            ENDDO
        ENDIF
C
C  Open the EVENTS data file.
C

C This is not needed since INDAT=----bas.fits
c       IF(INDAT .EQ. '         ') THEN
c           call fcecho('Enter EVENTS table name:')
c           READ 1000, INDAT
c1000       FORMAT(A80)
C
c       ENDIF

C
        context = INDAT
	call fcecho(context)

        LUND = 91
        CALL FTOPEN(LUND,INDAT,0,BLOCKA,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
	    context = 'Unable to open infile:'//INDAT
	    call fcerr(context)
	    go to 999
        ENDIF
C
        IF(ISTYLE .EQ. 3) THEN
C
C  Set the event file up for RDF data
C
            CALL FTMAHD (LUND,3,HDUTYPEA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
	context = 'Unable to move to the FITS extension:'//INDAT
		call fcerr(context)
		go to 999
            ENDIF
            CALL FTGHBN (LUND,10,NROWSA,TFIELDSA,TTYPEA,
     +                  TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context = 'Unable to read the FITS header'
		call fcerr(context)
		go to 999
            ENDIF
        ELSE
C
C  Skip the first HDU
C
            CALL FTMRHD (LUND,1,HDUTYPEA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
	context='Unable to move to the FITS extension:'//INDAT
            ENDIF
C
C  Read the FITS file header information
C
            IF(ISTYLE .EQ. 1) THEN
C
C  Set the event file up for German data
C
                CALL FTGHTB (LUND,10,ROWLENA,NROWSA,TFIELDSA,TTYPEA,
     +                  TBCOLA,TFORMA,TUNITA,EXTNAMEA,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                    BADSTA = .TRUE.
	context = 'Unable to read the FITS header'//INDAT
		call fcerr(context)
		go to 999
                ENDIF
            ELSE
C
C  Set the event file up for US data
C
                IF(ISTYLE .EQ. 2) THEN
                    CALL FTMRHD (LUND,1,HDUTYPEE,STATUS)
                    CALL FTMRHD (LUND,1,HDUTYPEE,STATUS)
                    IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                        BADSTA = .TRUE.
	context = 'Unable to move to the FITS extension:'//INDAT
		call fcerr(context)
		go to 999
                    ENDIF
                ENDIF
                CALL FTGHBN (LUND,10,NROWSA,TFIELDSA,TTYPEA,
     +                  TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                    BADSTA = .TRUE.
	context = 'Unable to read the FITS header'//INDAT
		call fcerr(context)
		go to 999
                ENDIF
            ENDIF
        ENDIF
C
C  Open the accepted time file
C
        OPEN(UNIT=92,STATUS='OLD',FILE='valid_times_all.dat')
        IOS = 0
        IE = 0
        DO WHILE (IOS .EQ. 0)
            IE = IE + 1
            READ(92,*,IOSTAT=IOS) ITEMP, ACTBEG(IE), ACTEND(IE)
            IF(IOS .EQ. 0) THEN
	write(context,'(''BEGIN = '',i10,'' END = '',i10)')
     &  ACTBEG(IE), ACTEND(IE)
	call fcecho(context)
	ENDIF
        ENDDO
        ACTBEG(IE) = 149999999
        ACTEND(IE) = 150000000
        IEE = IE - 1
	call fcecho(' ')
	write(context,'(''TOTAL NUMBER OF INTERVALS IS:'',i4)') IEE
	call fcecho(context)
	call fcecho(' ')
        IE = 1
C
C  Read in the event data, select, and sort.
C
        IOS = 0
        IAA = 0
        IB1 = 0
        IB2 = 0
        DTIMEO = 0.D0
C
C  Loop over all of the events
C
        DO IA=1,NROWSA
            IF(MOD(IA,10000) .EQ. 0) THEN
	    write(context,'(i10,i10)')IA, IAA
	    call fcecho(context)
	    ENDIF
            IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                CALL FTGCFD(LUND,1,IA,1,1,DTIME,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUND,2,IA,1,1,IS,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUND,3,IA,1,1,IX,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUND,4,IA,1,1,IY,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUND,5,IA,1,1,IDX,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUND,6,IA,1,1,IDY,FLAGVALA,ANYFA,STATUS)
C
C  Flip the Y value
C
                IY = -IY
            ELSEIF(ISTYLE .EQ. 2) THEN
                CALL FTGCFI(LUND,1,IA,1,1,IIX,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFI(LUND,2,IA,1,1,IIY,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFI(LUND,4,IA,1,1,ISI,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFD(LUND,5,IA,1,1,DTIME,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFI(LUND,6,IA,1,1,IDXI,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFI(LUND,7,IA,1,1,IDYI,FLAGVALA,ANYFA,STATUS)
                IX = IIX - 7680
                IY = IIY - 7680
                IDX = IDXI
                IDY = IDYI
                IS = ISI
            ELSEIF(ISTYLE .EQ. 3) THEN
                CALL FTGCFJ(LUND,1,IA,1,1,IX,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUND,2,IA,1,1,IY,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUND,4,IA,1,1,IS,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFD(LUND,5,IA,1,1,DTIME,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUND,6,IA,1,1,IDX,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUND,7,IA,1,1,IDY,FLAGVALA,ANYFA,STATUS)
                IX = IX - 7680
                IY = IY - 7680
            ENDIF
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context='Unable to read FITS file data (Position 1):'
     &            //INEVE
		  call fcerr(context)
		  goto 999
            ENDIF
            IF(DTIME .LT. DTIMEO) IE = 1
            DTIMEO = DTIME
            IG = 1
C
C  Check the detector position of the event and eliminate "bright-
C  line" data
C
            IF(IGG .EQ. 1) THEN
                IF(IDY .LT. 572) IG = 0
                IF(IDY .GT. 7303) IG = 0
                IF((IS .LT. 8) .OR. (IS .GT. 201)) THEN
                    IG = 0
                    IB1 = IB1 + 1
                ENDIF
            ELSEIF(IGG .EQ. 2) THEN
                IF(IDY .LT. 760) IG = 0
                IF(IDY .GT. 7098) IG = 0
                IF((IS .LT. 11) .OR. (IS .GT. 201)) THEN
                    IG = 0
                    IB1 = IB1 + 1
                ENDIF
            ELSEIF(IGG .EQ. 3)THEN
                IF(IDY .LT. 572) IG = 0
                IF(IDY .GT. 7303) IG = 0
                IF((IS .LT. 11) .OR. (IS .GT. 201)) THEN
                    IG = 0
                    IB1 = IB1 + 1
                ENDIF
            ENDIF
C
C  Check the masking
C
            IF((IMASK .EQ. 1) .AND. (IG .EQ. 1)) THEN
                XX = 257. + FLOAT(IX)/29.894656
                IX = INT(XX)
                YY = 257. + FLOAT(IY)/29.894656
                IY = INT(YY)
                IF(MASK(IX,IY) .EQ. 0.) THEN
                    IG = 0
                    IB3 = IB3 + 1
                ENDIF
            ENDIF
C
C  Check to see whether the event is in an accepted time interval
C
            DO WHILE ((IE .LT. IEE) .AND. (ACTEND(IE) .LT. DTIME))
                IE = IE + 1
            ENDDO
C
C  If so, add the event to the arrays
C
            IF((DTIME .GE. ACTBEG(IE)) .AND. (DTIME .LE. ACTEND(IE))
     +              .AND. (IG .EQ. 1)) THEN
                IAA = IAA + 1
                DTEVE(IAA) = DTIME
                IPHT(IAA) = IS
C
C                ISCS = DTIME
C                IPIIN = IS
C                CALL PICOR (LUNS,IDX,IDY,ISCS,IPIIN,IPIOUT)
C
C                print 9999, ISCS,IDX,IDY,IPIIN,IPIOUT,Ipiin-Ipiout
C 9999           format(1h ,6i10)
C
C                IPHT(IAA) = IPIOUT
C
            ELSE
                IB2 = IB2 + 1
            ENDIF
        ENDDO
C
	    write(context,'(i10)') IAA
	    call fcecho(context)
C
C  Sort the events by time
C
        CALL SORT28 (IAA,DTEVE,IPHT)
C
C  Exclude AP events
C
        IE = 1
        DO IA=2,IAA
            IF(DTEVE(IA) .GT. DTEVE(IA-1)+0.00035) THEN
                IE = IE + 1
                DTEVE(IE) = DTEVE(IA)
                IPHT(IE) = IPHT(IA)
            ENDIF
        ENDDO
C
C        PRINT *, NROWSA, IAA, IB1, IB2, IB3, IE
C
        IAA = IE
C
C  Open the event rate file.  The data are used to calculate the
C  livetime fraction
C

C INEVE ----anc.fits. ---- is read in 'grate'
c       IF(INEVE .EQ. '         ') THEN
c           PRINT *, 'Enter EVENTRATES table name'
c           READ 1000, INEVE
c       ENDIF
C
        LUNE = 92
        CALL FTOPEN(LUNE,INEVE,0,BLOCKE,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
	    context = 'Unable to open infile: '//INEVE
	    call fcerr(context)
	    go to 999
        ENDIF
C
        IF(ISTYLE .EQ. 3) THEN
C
C  Skip to the proper extension for RDF data
C
            CALL FTMAHD (LUNE,6,HDUTYPEE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context = 'Unable to move to the FITS extension:'//INEVE
		call fcerr(context)
		go to 999
            ENDIF
            CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                  TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context = 'Unable to read the FITS file header:'//INEVE
		call fcerr(context)
		go to 999
            ENDIF
        ELSE
C
C  Skip an extension
C
            CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context='Unable to move to the FITS extension:'//INEVE
		call fcerr(context)
		go to 999
            ENDIF
C
C  Read the FITS file header information
C
            IF(ISTYLE .EQ. 1) THEN
                CALL FTGHTB (LUNE,20,ROWLENE,NROWSE,TFIELDSE,
     +                  TTYPEE,TBCOLE,TFORME,TUNITE,EXTNAMEE,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                    BADSTA = .TRUE.
	    context = 'Unable to read the FITS file header:'//INEVE
		    call fcerr(context)
		    go to 999
                ENDIF
            ELSE
                CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                  TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                    BADSTA = .TRUE.
		context = 'Unable to read the FITS file header:'//INEVE
		    call fcerr(context)
		    go to 999
                ENDIF
            ENDIF
        ENDIF
        IE = 1
C
C  Read the first entry
C
        CALL FTGCFJ(LUNE,1,IE,1,1,LIVEND,FLAGVALE,ANYFE,STATUS)
        CALL FTGCFJ(LUNE,2,IE,1,1,MV,FLAGVALE,ANYFE,STATUS)
        CALL FTGCFJ(LUNE,3,IE,1,1,IAEXE,FLAGVALE,ANYFE,STATUS)
        CALL FTGCFJ(LUNE,4,IE,1,1,IA1LL,FLAGVALE,ANYFE,STATUS)
        CALL FTGCFJ(LUNE,7,IE,1,1,IAXE,FLAGVALE,ANYFE,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
		context='Unable to read FITS file data (Position 2):'
     &            //INEVE
		  call fcerr(context)
		  goto 999
        ENDIF
C
        A1LL = IA1LL
        AEXE = IAEXE
        AXE = IAXE
        RMV = MV
C
C  Open the output data file
C
        OPEN(UNIT=86,STATUS='UNKNOWN',FILE='RATE1.DAT')
        OPEN(UNIT=87,STATUS='UNKNOWN',FILE='RATE2.DAT')
C
C  Loop over all of the accepted events
C
        N = 0
        END = 0
        IOS = 0
        DO I=1,IAA
            NEW = INT(DTEVE(I))
            IS = IPHT(I)
C
C  The first time through this first part is skipped
C
            IF(NEW .LE. END) THEN
C
C  Separate into seven bands
C
                IF(IS .LE. 19) THEN
                    SP1 = SP1 + 1
                ELSEIF(IS .LE. 41) THEN
                    SP2 = SP2 + 1
                ELSEIF(IS .LE. 51) THEN
                    SP3 = SP3 + 1
                ELSEIF(IS .LE. 69) THEN
                    SP4 = SP4 + 1
                ELSEIF(IS .LE. 90) THEN
                    SP5 = SP5 + 1
                ELSEIF(IS .LE. 131) THEN
                    SP6 = SP6 + 1
                ELSE
                    SP7 = SP7 + 1
                ENDIF
C
C  Find the appropriate eventrates entry.  This little bit of code
C  should find the average MV rate for the interval as well.
C
                IF(NEW .NE. OLD) THEN
   20               CONTINUE
                    DO WHILE ((LIVEND .LT. DTEVE(I)) .AND. 
     +                      (IE .LT. NROWSE))
                        IE = IE + 1
                        CALL FTGCFJ(LUNE,1,IE,1,1,LIVEND,
     +                          FLAGVALE,ANYFE,STATUS)
                        CALL FTGCFJ(LUNE,2,IE,1,1,MV,
     +                          FLAGVALE,ANYFE,STATUS)
                        CALL FTGCFJ(LUNE,3,IE,1,1,IAEXE,
     +                          FLAGVALE,ANYFE,STATUS)
                        CALL FTGCFJ(LUNE,4,IE,1,1,IA1LL,
     +                          FLAGVALE,ANYFE,STATUS)
                        CALL FTGCFJ(LUNE,7,IE,1,1,IAXE,
     +                          FLAGVALE,ANYFE,STATUS)
                        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                            BADSTA = .TRUE.
		context='Unable to read FITS file data (Position 3):'
     &            //INEVE
		  call fcerr(context)
		  goto 999
                        ENDIF
C
                        IF(MV .GT. 500) THEN
	                write(context,'(i10)')MV
	                call fcecho(context)
			ENDIF
                        A1LL = IA1LL
                        AEXE = IAEXE
                        AXE = IAXE
                        RMV = MV
C
C  Determine the deadtime correction
C
                        CALL LIVTIM(A1LL,DEADTP,AXE,AEXE,
     +                                  FLIVE1,FLIVE2,FLIVE,IERR)
                        FLIVE1 = 1. - 0.0001*AEXE
                    ENDDO
C
C  Check for another extension using US data
C
                    IF((ISTYLE .EQ. 2) .AND. (IE .EQ. NROWSE)) THEN
C
C  Skip the first HDU
C
                        CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
                        IF (STATUS .EQ. 0) THEN
                            IE = 0
C
C  Read the FITS file header information
C
                            CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                          TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
                            GO TO 20
                        ENDIF
                    ENDIF
C
                    IEXP = IEXP + 1
                    EXP = EXP + FLIVE*FLIVE1*(NEW - OLD)
                    ITEXP = ITEXP + (NEW - OLD)
                    TOTEXP = TOTEXP + FLIVE*FLIVE1*(NEW - OLD)
                    AVEMV = AVEMV + RMV
                    AMV = AMV + RMV
                ENDIF
                OLD = NEW
            ELSE
                N = N + 1
                X(N) = N
                IF(IEXP .GT. INTERH) THEN
C
C  Calculate the particle background counts
C
                    AMV = AMV/IEXP
C
C  PSPC C
C
                    IF(START .LT. 20563200) THEN
                        RINT = 0.021 + 8.64E-4*AMV
                        REXT = 0.013 + 2.86E-4*AMV
                        RAL = -0.006 + 2.44E-4*AMV
                        R1 = 0.150*RINT + 0.050*REXT
                        R2 = 0.114*RINT + 0.091*REXT
                        R3 = 0.041*RINT + 0.041*REXT
                        R4 = 0.069*RINT + 0.074*REXT
                        R5 = 0.077*RINT + 0.087*REXT
                        R6 = 0.145*RINT + 0.169*REXT + 0.189*RAL
                        R7 = 0.242*RINT + 0.289*REXT + 0.804*RAL
C
C  PSPC B, high gain
C
                    ELSEIF(START .LT. 42940800) THEN
                        RINT = 0.018 + 8.64E-4*AMV
                        REXT = 0.013 + 2.86E-4*AMV
                        RAL = -0.006 + 2.44E-4*AMV
                        R1 = 0.153*RINT + 0.050*REXT
                        R2 = 0.115*RINT + 0.091*REXT
                        R3 = 0.041*RINT + 0.041*REXT
                        R4 = 0.069*RINT + 0.074*REXT
                        R5 = 0.076*RINT + 0.087*REXT
                        R6 = 0.144*RINT + 0.169*REXT + 0.189*RAL
                        R7 = 0.241*RINT + 0.289*REXT + 0.804*RAL
C
C  PSPC B, low gain
C
                    ELSE
                        RINT = 0.018 + 7.37E-4*AMV
                        REXT = 0.007 + 2.21E-4*AMV
                        RAL = -0.004 + 2.29E-4*AMV
                        R1 = 0.185*RINT + 0.044*REXT
                        R2 = 0.148*RINT + 0.105*REXT
                        R3 = 0.049*RINT + 0.047*REXT
                        R4 = 0.081*RINT + 0.084*REXT
                        R5 = 0.090*RINT + 0.096*REXT
                        R6 = 0.168*RINT + 0.182*REXT + 0.189*RAL
                        R7 = 0.270*RINT + 0.293*REXT + 0.804*RAL
                    ENDIF
C
                    SP1S = SQRT(SP1)/EXP
                    SP1 = SP1/EXP
                    SP2S = SQRT(SP2)/EXP
                    SP2 = SP2/EXP
                    SP3S = SQRT(SP3)/EXP
                    SP3 = SP3/EXP
                    SP4S = SQRT(SP4)/EXP
                    SP4 = SP4/EXP
                    SP5S = SQRT(SP5)/EXP
                    SP5 = SP5/EXP
                    SP6S = SQRT(SP6)/EXP
                    SP6 = SP6/EXP
                    SP7S = SQRT(SP7)/EXP
                    SP7 = SP7/EXP
                ELSE
                    SP1 = -9.
                    SP1S = -9.
                    SP2 = -9.
                    SP2S = -9.
                    SP3 = -9.
                    SP3S = -9.
                    SP4 = -9.
                    SP4S = -9.
                    SP5 = -9.
                    SP5S = -9.
                    SP6 = -9.
                    SP6S = -9.
                    SP7 = -9.
                    SP7S = -9.
                    R1 = -9
                    R2 = -9
                    R3 = -9
                    R4 = -9
                    R5 = -9
                    R6 = -9
                    R7 = -9
                ENDIF
                TIME = (OLD + START)/2.
                WRITE(86,2010) TIME, SP1, SP1S, SP2, SP2S, SP3, SP3S, 
     +                    SP4, SP4S, SP5, SP5S, SP6, SP6S, SP7, SP7S
 2010           FORMAT(I10,14F8.3)
                WRITE(87,2015) TIME, R1, R2, R3, R4, R5, R6, R7
 2015           FORMAT(I10,7F7.3)
                START = NEW
                END = START + 29
                OLD = NEW
                IEXP = 1
                SP1 = 0
                SP2 = 0
                SP3 = 0
                SP4 = 0
                SP5 = 0
                SP6 = 0
                SP7 = 0
                IF(IS .LT. 19) THEN
                    SP1 = 1
                ELSEIF(IS .LT. 41) THEN
                    SP2 = 1
                ELSEIF(IS .LT. 51) THEN
                    SP3 = 1
                ELSEIF(IS .LT. 69) THEN
                    SP4 = 1
                ELSEIF(IS .LT. 90) THEN
                    SP5 = 1
                ELSEIF(IS .LT. 131) THEN
                    SP6 = 1
                ELSE
                    SP7 = 1
                ENDIF
   30           CONTINUE
                DO WHILE ((LIVEND .LT. DTEVE(I)) .AND. 
     +                  (IE .LT. NROWSE))
                    IE = IE + 1
                    CALL FTGCFJ(LUNE,1,IE,1,1,LIVEND,
     +                      FLAGVALE,ANYFE,STATUS)
                    CALL FTGCFJ(LUNE,2,IE,1,1,MV,
     +                      FLAGVALE,ANYFE,STATUS)
                    CALL FTGCFJ(LUNE,3,IE,1,1,IAEXE,
     +                      FLAGVALE,ANYFE,STATUS)
                    CALL FTGCFJ(LUNE,4,IE,1,1,IA1LL,
     +                      FLAGVALE,ANYFE,STATUS)
                    CALL FTGCFJ(LUNE,7,IE,1,1,IAXE,
     +                      FLAGVALE,ANYFE,STATUS)
                    IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                        BADSTA = .TRUE.
		context='Unable to read FITS file data (Position 4):'
     &            //INEVE
		  call fcerr(context)
		  goto 999
                    ENDIF
                ENDDO
C
C  Check for another extension using US data
C
                IF((ISTYLE .EQ. 2) .AND. (IE .EQ. NROWSE)) THEN
C
C  Skip the first HDU
C
                    CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
                    IF (STATUS .EQ. 0) THEN
                        IE = 0
C
C  Read the FITS file header information
C
                        CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                          TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
                        GO TO 30
                    ENDIF
                ENDIF
C
                        IF(MV .GT. 500) THEN
	                write(context,'(i10)')MV
	                call fcecho(context)
			ENDIF
                A1LL = IA1LL
                AEXE = IAEXE
                AXE = IAXE
                RMV = MV
                CALL LIVTIM(A1LL,DEADTP,AXE,AEXE,
     +                                  FLIVE1,FLIVE2,FLIVE,IERR)
                FLIVE1 = 1. - 0.0001*AEXE
                IEXP = 1
                EXP = FLIVE*FLIVE1
                ITEXP = ITEXP + 1
                TOTEXP = TOTEXP + FLIVE*FLIVE1
                AVEMV = AVEMV + RMV
                AMV = RMV
            ENDIF
        ENDDO
C
        STATUS = 0
        CALL FTCLOS(LUND,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
	    context='Unable to close  FITS file: '//INDAT
	    call fcerr(context)
	    go to 999
        ENDIF
        CALL FTCLOS(LUNE,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
	    context='Unable to close  FITS file: '//INEVE
	    call fcerr(context)
	    go to 999
        ENDIF
C
 999  continue
       if (status .ne. 0) then
       call fcerrm(status)
       stop
       endif

        STOP
        END


C**********************************************************************
C SUBROUTINE:
C      grate
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C
C NOTES:
C       grate uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C      call grate(OBS, ISTYLE, ,R1LBAND, IMASK, MASKNAME, STATUS)
C
C ARGUMENTS:
C       OBS     - Observation control (prefix of the data files)
C       ISTYLE  - 1 for German data, 2 for US data and 3 for RDF data
C	R1LBAND - If set, process high gain data using R1L band.
C	IMASK 	- 1 for masking, 0 for no masking.
C	MASKNAME - Name of the fits file containg mask.
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*****************************************************************************

      subroutine grate(OBS,ISTYLE,R1LBAND,IMASK,MASKNAME,STATUS)
      character(8) OBSSTR
      character(1) OBS(8)
      character(80) context, maskname
      integer istyle, imask, status
      logical R1LBAND

C  initialize variables
      status = 0
      imask = 0
      istyle = 0
      maskname = ' '

C  get the contents of the OBS string
      call uclgst('obs',obsstr,status)
	    if (status .ne. 0) then
      context = 'could not get OBS parameter'
      call fcerr(context)
       goto 999
      endif
C Write the string into the array to be compatible with the main routine
      DO i=1,8
      obs(i)= obsstr(i:i)
      enddo

C  get the value of the ISTYLE parameter
      call uclgsi('istyle',istyle,status)
	    if (status .ne. 0) then
      context = 'could not get ISTYLE parameter'
      call fcerr(context)
       goto 999
      endif

C  get the IMASK parameter
      call uclgsi('imask',imask,status)
	    if (status .ne. 0) then
      context = 'could not get IMASK parameter'
      call fcerr(context)
       goto 999
      endif
	if (imask .eq. 1) then
C  get the fits file name containing mask 
        call uclgst('maskname',maskname,status)
	    if (status .ne. 0) then
        context = 'could not get MASKNAME parameter'
        call fcerr(context)
        goto 999
        endif
      endif

C  Does the user wish to process the high-gain data using the R1L band?
      call uclgsb('R1LBAND',R1LBAND,status)
	    if (status .ne. 0) then
      context = 'could not get R1LBAND parameter'
      call fcerr(context)
       goto 999
      endif
       
 999  continue
       if (status .ne. 0) then
	call fcerrm(status)
 	 stop
       endif

      return
      end



C******************************************************************************
C SUBROUTINE:
C       rdval
C
C DESCRIPTION:
C       This routine reads valid_times.dat file to evaluate the gain control
C
C       Written by: Srilal Weera  (Nov 1996)
C
C
C NOTES:
C
C The tool looks at the first entry on the first row and the last entry 
C on the last row.  If both are before the break point then it is a high-
C gain observation.  If both are after the the break point then it is a low-
C gain observation.  If the first is before and the last is after, then it 
C is a split observation and the tool should terminate with the warning 
C message.
C
C          1: High gain:   before 11 October 1991
C          2: Low gain :   after  11 October 1991
C	   3: Low gain pulse-height range, high gain area
C	      (For processing high-gain data and using R1L band)

C USAGE:
C       call rdval (R1LBAND,gain, status)
C
C ARGUMENTS:
C	R1LBAND - If set, then process high-gain data using R1L band
C       gain    - Gain Control
C       status  - status of operation
C
C PRIMARY LOCAL VARIABLES:
C	breakpt = 42996070; The time against which start and end observation
C		  times are compared to evaluate 'gain'.
C	infile  - valid_times.dat file containing observation times
C	startt  - Observation start time array
C	endt    - Observation end time array
C
C******************************************************************************
      subroutine  rdval (R1LBAND,gain, status)


      integer icount, iunit, status, gain, breakpt
      integer count(100), startt(100),endt(100)
      character(80) infile, context
      logical R1LBAND
 
      status = 0
      iunit = 18
      breakpt = 42996070

C Open valid_times.dat file
      infile = 'valid_times_all.dat'
      open (unit = iunit, file= infile, status='old', err=999)

C 	First the no. of records are counted and then read

	    icount = 0
220         icount = icount + 1
            read (iunit, *, end=210, err=999) 
     &	    count(icount), startt(icount), endt(icount)
	    goto 220
210         continue
      close (unit=iunit)

            icount = icount - 1
C  Now icount contains the number of records in the file
        

      if ((startt(1) .lt. breakpt) .and. 
     &	(endt(icount) .lt. breakpt)) then
C  If R1LBAND is defined then set gain =3
	if (R1LBAND) then
	gain = 3
	else
	gain = 1
	endif
      elseif ((startt(1) .gt. breakpt) .and. 
     & (endt(icount) .gt. breakpt)) then
 	gain = 2
      else
      context = '  Observation periods need to be rearranged'
      call fcerr (context)
      goto 999
      endif

      return

 999  continue 
      status = 10

      if (status .ne. 0) then
      context = '  Error obtaining data from input file: ' // infile
      call fcerr(context)
      stop
      endif

      return
      end

