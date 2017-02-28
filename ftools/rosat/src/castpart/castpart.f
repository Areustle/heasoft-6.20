
C*********************************************************************
C SELECTOR TASK:
C      castpart
C
C FILE:
C      castpart.f
C
C DESCRIPTION:
C      
C
C AUTHOR:
C	Steve Snowden
C       FTOOLS development by Srilal Weera  
C
C MODIFICATION HISTORY:
C     
C Jan 5 00 - Changed all the filenames from uppercase to lowercase.
C Oct 7 96 - Initialized the variables to avoid core dumping on Linux
C
C NOTES:
C   
C
C USAGE:
C      HOST: call castpt
C      IRAF: task castpt
C
C ARGUMENTS:
C      none
C
C
C CALLED ROUTINES:
C      subroutine gcastpt - gets parameters from parameter file
C      subroutine readval -  reads valid_times.dat file to evaluate 
C			     the gain control
C
C******************************************************************************


 	 subroutine castpt
C        PROGRAM CAST_PART
C
C  Author:  Steve Snowden
C  Date:    18 December 1992
C  Updated: 5 August 1993
C  Revision: 30 November 1993 by Wei Cui
C            To output image FITS files
C  Update: 21 April 1994
C  Update: 2 December 1994    Add more fitsio bells and whistles
C
C  Program CAST_PART is used to cast the particle background maps.
C  It makes images for each of the three particle background components
C  along with an afterpulse image if desired.  It uses the Plucinsky et
C  al. calibration for all aspects.  The modelling for the time period
C  1 June 1991 to 11 October 1991 appears different from the Plucinsky
C  et al. only because the channel range considered is 8-249 instead of
C  18-249.
C
C  This program has been converted to work on the FITS files provided
C  in the REV0 data sets of both German (*ATTITUDE.MT and *EVENTRATES.MT)
C  and US (*.cas and *.evr) data set fits files with FITSIO.  The files
C  must have names of the form xxx_attitude.fits and xxx_eventrates.fits
C  where xxx is any string up to eight characters long; the string is
C  queried for by the program.  An accepted time file, "valid_times.dat",
C  is also used by the program, which should be identical as that used
C  when running cast_data.f, tilt.f, and other.f.  This file should be
C  in the same format as "valid_times_all.dat", the output of the
C  program "get_valid_times.f"  This file can be created from the
C  accepted times given in the GTI file of the observation data set but
C  the quality of the data should be verified (i.e., check for times of
C  strong short-term enhancements or scattered solar X-rays) by eye
C  (see programs rate.f, rate_fit.f, and ao.f).
C
C  CAST_PART has now been updated to work with RDF files.  The *.ANC
C  file must be named xxx_anc.fits
C
C  The fraction of counts for each band are given in the
C  following table.
C
C  BAND  INTERNAL BACKGROUND     EXTERNAL BACKGROUND    AFTERPULSES
C          PSPC C   PSPC B       FLAT*      AL Kalpha
C  High Gain
C  R1      0.150    0.153        0.050        0.000        1.000
C  R1L     0.091    0.092        0.037        0.000        1.000
C  R2      0.114    0.115        0.091        0.000        0.000
C  R3      0.041    0.041        0.041        0.000        0.000
C  R4      0.069    0.069        0.074        0.000        0.000
C  R5      0.077    0.076        0.087        0.000        0.000
C  R6      0.145    0.144        0.169        0.189        0.000
C  R7      0.242    0.241        0.289        0.804        0.000
C
C  R12     0.265    0.268        0.140        0.000        1.000
C  R45     0.145    0.145        0.161        0.000        0.000
C  R67     0.387    0.385        0.458        0.993        0.000
C
C  Low Gain
C  R1L              0.185        0.044        0.000        1.000
C  R2               0.148        0.105        0.000        0.000
C  R3               0.049        0.047        0.000        0.000
C  R4               0.081        0.084        0.000        0.000
C  R5               0.090        0.096        0.000        0.000
C  R6               0.168        0.182        0.189        0.000
C  R7               0.270        0.293        0.804        0.000
C
C  R12              0.333        0.149        0.000        1.000
C  R45              0.171        0.180        0.000        0.000
C  R67              0.438        0.475        0.993        0.000
C
C  *0.00423
C
        IMPLICIT NONE
C
        INTEGER*2 IT2
        INTEGER*2 INI2(512,512)
        INTEGER*4 INI4(512,512)
C
        INTEGER*4 BLOCKA, BLOCKE, HDUTYPEA, HDUTYPEE, IA, IAA, IA1LL, 
     +      IAEXE, IAXE, I, IACTBE(1000), IACTEN(1000), IS, ISIZE, 
     +      ISTOT, IASP(3,10000), IDET, IE, IERR, IG, II, III, IIP, 
     +      ILIVEN, IOS, IP, IP1, IR, IROLL, ISCS, ISCSO, ISTYLE, IT1,
     +      ITEMP, IX, IXX, IY, IYY, LUN, LUNA, LUNE, MV, NB, NB1, NB2, 
     +      NB3, NG, NROWSA, NROWSE, NUM, ROWLENA, ROWLENE, STATUS, 
     +      TBCOLA(10),
     +      TBCOLE(35), TFIELDSA, TFIELDSE, VARIDATA, VARIDATE
C
        REAL*4 A1, A2, A3, A1LL, AEXE, ANGLE, AP, ASP(10000,3), AXE,
     +      BZERO, BSCALE, CDELT1, CDELT2, COSROL, CRVAL1, CRVAL2, 
     +      DEADTP, DELT, DUM1, DUM2, DUM3, DUM4, EXP, EXPARR(512,512), 
     +      FLIVE,FLIVE1, FLIVE2, NMV, PARLT, PARTEX1, PARTEX2, PARTIN,
     +      RMAP(512,512), ROLL, RMV, SCAL, SINROL, TMAP(512,512),
     +      TMV, TOTEX1, TOTEX2, TOTEXP, TOTIME,
     +      TOTIN, X, XX, Y, YY
C
        REAL*8 DSCS
C
        character(1) OBS(8), OBS1
        character(2) OBS2
        character(3) OBS3
        character(4) OBS4
        character(5) OBS5
        character(6) OBS6
        character(7) OBS7
        character(8) OBS8, UNITS
        character(8) TTYPEA(20), TTYPEE(35), TFORMA(20), TFORME(35),
     +      TUNITA(20), TUNITE(35)
        character(20) DIAG, EXTNAMEA, EXTNAMEE
        character(80) DIRSTR
        character(60) COMMENT
        character(80) INANC, INATT, INEVE, INFILE, INSTMP(7), OUTFIL

        character(20) instrume
        character(80) context

        LOGICAL ANYFA, ANYFE, BADSTA, FLAGVALA, FLAGVALE, continue
	LOGICAL EOF
C
        EQUIVALENCE (OBS(1),OBS1,OBS2,OBS3,OBS4,OBS5,OBS6,OBS7,OBS8)
        EQUIVALENCE (RMAP,INI4)
        EQUIVALENCE (RMAP,INI2)
C
c       DATA DIRSTR /'/caldb/data/rosat/pspc/cpf/detmaps/'/
        DATA INSTMP /
     +      'detp_i_h_c.fits',
     +      'detp_i_h_b.fits',
     +      'detp_i_l_b.fits',
     +      'detp_e_h.fits'  ,
     +      'detp_e_l.fits'  ,
     +      'detp_ap_h.fits' ,
     +      'detp_ap_l.fits' /
C
C  The following data statement is for the deadtime scale factor
C
        DATA DEADTP /234./
C
        DATA BADSTA /.FALSE./

C    LENACT is used to correctly concatenate a filename to 'dirstr'
      integer lenact
      external lenact

      character(40) taskname
      common /task/ taskname

      taskname = 'castpart v1.2'
      call ftcmsg

C Initialize:
        ISTYLE = 0
        STATUS = 0
	INATT = ' '
	INANC = ' '
	DIRSTR = ' '
C
C Oct 7 96 - Initialized the variables to avoid core dumping on Linux
C
        DATA BLOCKA, BLOCKE, HDUTYPEA, HDUTYPEE, IA, IAA, IA1LL, 
     +      IAEXE, IAXE, I, IS, ISIZE, 
     +      ISTOT,  IDET, IE, IERR, IG, II, III, IIP, 
     +      ILIVEN, IOS, IP, IP1, IR, IROLL, ISCS, ISCSO, ISTYLE, IT1,
     +      ITEMP, IX, IXX, IY, IYY, LUN, LUNA, LUNE, MV, NB, NB1, NB2, 
     +      NB3, NG, NROWSA, NROWSE, NUM, ROWLENA, ROWLENE, STATUS, 
     +      TFIELDSA, TFIELDSE, VARIDATA, VARIDATE /54*0/
C

        DATA A1, A2, A3, A1LL, AEXE, ANGLE, AP,  AXE,
     +      BZERO, BSCALE, CDELT1, CDELT2, COSROL, CRVAL1, CRVAL2, 
     +       DELT, DUM1, DUM2, DUM3, DUM4, EXP,  
     +      FLIVE,FLIVE1, FLIVE2, NMV, PARLT, PARTEX1, PARTEX2, PARTIN,
     +      ROLL, RMV, SCAL, SINROL, 
     +      TMV, TOTEX1, TOTEX2, TOTEXP, TOTIME,
     +      TOTIN, X, XX, Y, YY /43*0.0/
C



C  get parameters from parameter file
      call gcastpt(OBS, ISTYLE, AP, CONTINUE, DIRSTR, STATUS)

        IF(OBS8 .NE. '        ') THEN
            DO I=2,8
                IF((OBS(I) .EQ. ' ') .AND. (OBS(I-1) .NE. ' '))
     +                  NUM = I - 1
            ENDDO
            IF(NUM .EQ. 0) NUM = 8
            IF(NUM .EQ. 1) THEN
                WRITE(DIAG,3010) OBS1
 3010           FORMAT(A1,'_diag.out')
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3012) OBS1
 3012               FORMAT(A1,'_anc.fits')
                ELSE
                    WRITE(INATT,3015) OBS1
 3015               FORMAT(A1,'_attitude.fits')
                    WRITE(INEVE,3017) OBS1
 3017               FORMAT(A1,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 2) THEN
                WRITE(DIAG,3020) OBS2
 3020           FORMAT(A2,'_diag.out')
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3022) OBS2
 3022               FORMAT(A2,'_anc.fits')
                ELSE
                    WRITE(INATT,3025) OBS2
 3025               FORMAT(A2,'_attitude.fits')
                    WRITE(INEVE,3027) OBS2
 3027               FORMAT(A2,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 3) THEN
                WRITE(DIAG,3030) OBS3
 3030           FORMAT(A3,'_diag.out')
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3032) OBS3
 3032               FORMAT(A3,'_anc.fits')
                ELSE
                    WRITE(INATT,3035) OBS3
 3035               FORMAT(A3,'_attitude.fits')
                    WRITE(INEVE,3037) OBS3
 3037               FORMAT(A3,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 4) THEN
                WRITE(DIAG,3040) OBS4
 3040           FORMAT(A4,'_diag.out')
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3042) OBS4
 3042               FORMAT(A4,'_anc.fits')
                ELSE
                    WRITE(INATT,3045) OBS4
 3045               FORMAT(A4,'_attitude.fits')
                    WRITE(INEVE,3047) OBS4
 3047               FORMAT(A4,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 5) THEN
                WRITE(DIAG,3050) OBS5
 3050           FORMAT(A5,'_diag.out')
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3052) OBS5
 3052               FORMAT(A5,'_anc.fits')
                ELSE
                    WRITE(INATT,3055) OBS5
 3055               FORMAT(A5,'_attitude.fits')
                    WRITE(INEVE,3057) OBS5
 3057               FORMAT(A5,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 6) THEN
                WRITE(DIAG,3060) OBS6
 3060           FORMAT(A6,'_diag.out')
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3062) OBS6
 3062               FORMAT(A6,'_anc.fits')
                ELSE
                    WRITE(INATT,3065) OBS6
 3065               FORMAT(A6,'_attitude.fits')
                    WRITE(INEVE,3067) OBS6
 3067               FORMAT(A6,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 7) THEN
                WRITE(DIAG,3070) OBS7
 3070           FORMAT(A7,'_diag.out')
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3072) OBS7
 3072               FORMAT(A7,'_anc.fits')
                ELSE
                    WRITE(INATT,3075) OBS7
 3075               FORMAT(A7,'_attitude.fits')
                    WRITE(INEVE,3077) OBS7
 3077               FORMAT(A7,'_eventrates.fits')
                ENDIF
            ELSEIF(NUM .EQ. 8) THEN
                WRITE(DIAG,3080) OBS8
 3080           FORMAT(A8,'_diag.out')
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3082) OBS8
 3082               FORMAT(A8,'_anc.fits')
                ELSE
                    WRITE(INATT,3085) OBS8
 3085               FORMAT(A8,'_attitude.fits')
                    WRITE(INEVE,3087) OBS8
 3087               FORMAT(A8,'_eventrates.fits')
                ENDIF
            ENDIF
        ENDIF



        NUM = 0
        SCAL = 0.

        call fcecho(' ')
C
C  Make sure that we have an attitude file name
C
        IF((INATT .EQ. '    ') .AND. (INANC .EQ. '    ')) THEN
            call fcecho('Enter ATTITUDE table name')
            IF(ISTYLE .EQ. 3) THEN
                READ 1000, INANC
            ELSE
                READ 1000, INATT
            ENDIF
 1000       FORMAT(A80)
        ENDIF
C
        LUNA = 91
        IF(ISTYLE .EQ. 3) THEN
C
C  Open the RDF extension for the attitude data
C
            CALL FTOPEN(LUNA,INANC,0,BLOCKA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to open infile (Attitude extension):'
     &	    //INANC
            call fcerr(context)
            go to 999
            ENDIF
C
C  Skip the proper RDF extension
C
            CALL FTMAHD (LUNA,4,HDUTYPEA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to move to the FITS extension'
            call fcerr(context)
            go to 999
            ENDIF
C
C  Read the FITS file header information
C
            CALL FTGHBN (LUNA,23,NROWSA,TFIELDSA,TTYPEA,
     +                  TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information'
            call fcerr(context)
            go to 999
            ENDIF


C     Read the INSTRUME keyword
	CALL FTGKYS(luna, 'INSTRUME', instrume, context, status)
        if (status .ne. 0) then
	 context = 'Error reading INSTRUME keyword'
	 call fcerr(context)
	 goto 999
	endif

C        'Determine  detector control'
C        '1 => PSPC C    before 26 January 1991'
C        '2 => PSPC B    after 26 January 1991'

        if     (instrume(5:5) .eq. 'C') then
	   IDET = 1
        elseif (instrume(5:5) .eq. 'B') then
	   IDET = 2
	else
	   context = 'Error reading detector control'
	   call fcerr(context)
	   goto 999
	endif

C  Read the nominal pointing direction (RA, DEC) in degrees
C
	CALL FTGKYE(luna, 'RA_NOM', CRVAL1, context, status)
        if (status .ne. 0) then
	 context = 'Error reading RA_NOM keyword'
	 call fcerr(context)
	 goto 999
	endif
	CALL FTGKYE(luna, 'DEC_NOM', CRVAL2, context, status)
        if (status .ne. 0) then
	 context = 'Error reading DEC_NOM keyword'
	 call fcerr(context)
	 goto 999
	endif

        ELSE
C
C  Open the German or US attitude file
C
            context = INANC
            call fcerr(context)
            CALL FTOPEN(LUNA,INATT,0,BLOCKA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to open infile: '//INATT
            call fcerr(context)
            go to 999
            ENDIF
C
C  Skip the first HDU for German and US data
C
            CALL FTMRHD (LUNA,1,HDUTYPEA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to move to the FITS extension'
            call fcerr(context)
            go to 999
            ENDIF
C
C  Read the FITS file header information
C
            IF(ISTYLE .EQ. 1) THEN
                CALL FTGHTB (LUNA,23,ROWLENA,NROWSA,TFIELDSA,TTYPEA,
     +                  TBCOLA,TFORMA,TUNITA,EXTNAMEA,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                    BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information'
            call fcerr(context)
            go to 999
                ENDIF
            ELSE
                CALL FTGHBN (LUNA,23,NROWSA,TFIELDSA,TTYPEA,
     +                  TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                    BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information'
            call fcerr(context)
            go to 999
                ENDIF
            ENDIF
        ENDIF
C
C  Make sure that we have an eventrate file name
C
        IF((INEVE .EQ. '    ') .AND. (INANC .EQ. '    ')) THEN
	    call fcecho('Enter ATTITUDE table name')
            IF(ISTYLE .EQ. 3) THEN
                READ 1000, INANC
            ELSE
                READ 1000, INEVE
            ENDIF
        ENDIF
C
        LUNE = 92
        IF(ISTYLE .EQ. 3) THEN
C
C  Open the RDF extension for the eventrate data
C
            context = INANC
            call fcecho(context)
            CALL FTOPEN(LUNE,INANC,0,BLOCKE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to open infile (Eventrate extension):'
     &	    //INANC
            call fcerr(context)
            go to 999
            ENDIF
C
C  Skip the proper RDF extension
C
            CALL FTMAHD (LUNE,6,HDUTYPEE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to move to the FITS extension'
            call fcerr(context)
            go to 999
            ENDIF
C
C  Read the FITS file header information
C
            CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +          TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information'
            call fcerr(context)
            go to 999
            ENDIF
C
C  Open the German or US eventrate file
C
        ELSE
            context = INEVE
            call fcecho(context)
            CALL FTOPEN(LUNE,INEVE,0,BLOCKE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to open infile :'
     &	    //INEVE
            call fcerr(context)
            go to 999
            ENDIF

            CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to move to the FITS extension'
            call fcerr(context)
            go to 999
            ENDIF
C
C  Read the FITS file header information
C
            IF(ISTYLE .EQ. 1) THEN
                CALL FTGHTB (LUNE,35,ROWLENE,NROWSE,TFIELDSE,TTYPEE,
     +                  TBCOLE,TFORME,TUNITE,EXTNAMEE,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information'
            call fcerr(context)
            go to 999
                ENDIF
            ELSE
                CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                  TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
            context = 'Unable to read the FITS file header information'
            call fcerr(context)
            go to 999
                ENDIF
            ENDIF
        ENDIF
C
C  Open the accepted time file
C
        INFILE = 'valid_times.dat'
        OPEN(UNIT=93,STATUS='OLD',FORM='FORMATTED',
     +          FILE=INFILE)
C
C  Read in the accepted time intervals
C
        IS = 0
        IOS = 0
        DO WHILE (IOS .EQ. 0)
            IS = IS + 1
            READ(93,*,IOSTAT=IOS) ITEMP, IACTBE(IS), IACTEN(IS)
            IF(IOS .EQ. 0) then
        write(context,'(''BEGIN = '',i10,'' END = '',i10)') 
     &  IACTBE(IS), IACTEN(IS)
 	call fcecho(context)
	    ENDIF
        ENDDO
        IACTBE(IS) = 199999999
        IACTEN(IS) = 200000000
        ISTOT = IS - 1
        IS = 1
        ISCSO = 0

        call fcecho(' ')
        write(context,'(''TOTAL NUMBER OF INTERVALS IS:'',i4)') ISTOT
 	call fcecho(context)
        call fcecho(' ')
C
C  Start the loop over the attitude file
C
   10   CONTINUE
        DO IAA=1,NROWSA
C
C  Read in the attitude entry
C
C      ITEMP  line counter from the MIDAS output, ignore
C      DSCS   double precision space craft clock seconds
C      IX     pointing offset from nominal position, X direction
C      IY     pointing offset from nominal position, Y direction
C      IROLL  detector roll angle
C
            IF(ISTYLE .EQ. 1) THEN
                CALL FTGCFD(LUNA,1,IAA,1,1,DSCS,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUNA,2,IAA,1,1,IX,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUNA,3,IAA,1,1,IY,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUNA,4,IAA,1,1,IROLL,FLAGVALA,ANYFA,STATUS)
                STATUS = 0
            ELSEIF(ISTYLE .EQ. 2) THEN
                CALL FTGCFJ(LUNA,1,IAA,1,1,IT1,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFI(LUNA,2,IAA,1,1,IT2,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUNA,3,IAA,1,1,IROLL,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUNA,4,IAA,1,1,IX,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFJ(LUNA,5,IAA,1,1,IY,FLAGVALA,ANYFA,STATUS)
                DSCS = IT1 + IT2/64.D0
            ELSEIF(ISTYLE .EQ. 3) THEN
                CALL FTGCFD(LUNA,1,IAA,1,1,DSCS,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFE(LUNA,4,IAA,1,1,X,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFE(LUNA,5,IAA,1,1,Y,FLAGVALA,ANYFA,STATUS)
                CALL FTGCFE(LUNA,6,IAA,1,1,ROLL,FLAGVALA,ANYFA,STATUS)
                IX = 7200.*X
                IY = 7200.*Y
                IROLL = 7200.*ROLL
            ENDIF
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
	    context = 'Unable to get parameters from the FITS file:'
     &      //INANC
            call fcerr(context)
            go to 999
            ENDIF
C
C  Determine the delta time from the last entry
C
            ISCS = DSCS
            IF (ISCS .LT. ISCSO) IS = 1
            DELT = ISCS - ISCSO
            ISCSO = ISCS
C
C  Flip the Y value for consistency.  Increasing Y in SASS is downward
C  in declination.  The instrument map has already been flipped
C
            IY = -IY
C
C  Process the attitude step, first check the accepted time file
C  to see if the attitude step is in an accepted time period
C
            IF(DELT .LT. 10.) THEN
                DO WHILE ((ISCS .GT. IACTEN(IS))
     +                      .AND. (IS .LE. ISTOT))
                    IS = IS + 1
                ENDDO
                IF((ISCS .GE. IACTBE(IS)) .AND. (IS .LE. ISTOT)) THEN
                    NUM = NUM + 1
                    IF(MOD(NUM,1000) .EQ. 0)  then
		    write(context,'('' NUM = '',i5)')NUM
		    call fcecho(context)
                    endif
C
C  Accepted time, now find the live time.  First sort through the event
C  rate file to find a close time
C
C      ILIVEN  integer space craft clock seconds
C      IAEXE   AEXE scaler count rate
C      IAXE    AXE scaler count rate
C      IA1LL   A1LL scaler count rate
C      MV      MV (master veto) count rate
C
   30               CONTINUE
                    DO WHILE ((ISCS .GT. ILIVEN) .AND. 
     +                      (IE .LT. NROWSE))
                        IE = IE + 1
                        CALL FTGCFJ(LUNE,1,IE,1,1,ILIVEN,
     +                          FLAGVALE,ANYFE,STATUS)
                        CALL FTGCFJ(LUNE,2,IE,1,1,MV,
     +                          FLAGVALE,ANYFE,STATUS)
                        CALL FTGCFJ(LUNE,3,IE,1,1,IAEXE,
     +                          FLAGVALE,ANYFE,STATUS)
                        CALL FTGCFJ(LUNE,4,IE,1,1,IA1LL,
     +                          FLAGVALE,ANYFE,STATUS)
                        CALL FTGCFJ(LUNE,7,IE,1,1,IAXE,
     +                          FLAGVALE,ANYFE,STATUS)
                        A1LL = IA1LL
                        AEXE = IAEXE
                        AXE = IAXE
                        RMV = MV
C
C  For US data, see if there is another OBI
C
                        IF((ISTYLE .EQ. 2) .AND.
     +                          (IE .EQ. NROWSE)) THEN
C
C  Skip to the next HDU
C
                            CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
                            IF (STATUS .EQ. 0) THEN
                                IE = 0
C
C  Read the FITS file header information
C
                                CALL FTGHBN (LUNE,10,NROWSE,TFIELDSE,
     +                                  TTYPEE,TFORME,TUNITE,EXTNAMEE,
     +                                  VARIDATE,STATUS)
                            ENDIF
                            IF ((STATUS .NE. 0) .AND.
     +                              (STATUS .NE. 107)) THEN
                                IF(.NOT. BADSTA) THEN
                                    BADSTA = .TRUE.
	    context = 'Unable to read parameters from the FITS header'
            call fcerr(context)
            go to 999
                                ENDIF
                                STOP
                            ELSE
                                STATUS = 0
                            ENDIF
                        ENDIF
                    ENDDO
                    IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
C                       PRINT *, 'Not necessarily fatal'
        write(context,
     &	'(''Unable to move to the FITS header; Status='' ,i5)')STATUS
        call fcecho(context)
                    ENDIF
C
C  Check to see if the detector was actually on
C
                    IF(A1LL .GT. 10.) THEN
C
C  Calculate the live time fraction.  This is a GRH routine
C
                        CALL LIVTIM(A1LL,DEADTP,AXE,AEXE,FLIVE1,
     +                      FLIVE2,FLIVE,IERR)
C
C  Correct for the MV deadtime in the particle background calibration.
C  Without this factor, the deadtime is corrected for twice.
C
                        PARLT = 1. - 230.E-6*RMV
C
C  Calculate the live time fraction with the additional 0.0005 sec
C  deadtime per accepted event
C
                        FLIVE1 = 1. - 0.0001*AEXE
C
                        IF(IG .EQ. 1) THEN
C
C  High gain data
C
                            IF(IDET .EQ. 1) THEN
                                PARTIN = 0.021 + 0.000864*RMV
                            ELSEIF(IDET .EQ. 2) THEN
                                PARTIN = 0.018 + 0.000864*RMV
                            ENDIF
                            PARTIN = PARTIN/PARLT
                            PARTEX1 = (0.013 + 0.000286*RMV)/PARLT
                            PARTEX2 = (-0.006 + 0.000244*RMV)/PARLT
                        ELSE
C
C  Low gain data
C
                            PARTIN = (0.018 + 0.000737*RMV)/PARLT
                            PARTEX1 = (0.007 + 0.000221*RMV)/PARLT
                            PARTEX2 = (-0.004 + 0.000229*RMV)/PARLT
                        ENDIF
C
C  The attitude steps should be on 1-second intervals, calculate the
C  exposure
C
                        EXP = DELT*FLIVE*FLIVE1
                        TOTIME = TOTIME + DELT
                        TOTEXP = TOTEXP + EXP
                        PARTIN = PARTIN*EXP
                        TOTIN = TOTIN + PARTIN
                        PARTEX1 = PARTEX1*EXP
                        TOTEX1 = TOTEX1 + PARTEX1
                        PARTEX2 = PARTEX2*EXP
                        TOTEX2 = TOTEX2 + PARTEX2
C
C  Sum the MV count rate to find an average value
C
                        TMV = TMV + RMV
                        NMV = NMV + 1.
C
C  Set the X, Y, and ROLL values for the aspect array.  X,Y Steps are
C  in units of 14.94733 arc seconds (historical reasons).  ROLL steps
C  are in units of 0.2076 degrees.
C
                        X = 1000. + FLOAT(IX)/29.894656
                        IX = INT(X) - 1000
                        Y = 1000. + FLOAT(IY)/29.894656
                        IY = INT(Y) - 1000
                        ROLL = 20000.5 + FLOAT(IROLL)/1494.733
                        IR = INT(ROLL) - 20000
C
C  Add to the aspect list
C

                        IF(IA .GT. 0) THEN
                            NB1 = 1
                            DO I=1,IA
                                IF((IX .EQ. IASP(1,I)) .AND. 
     +                                  (IY .EQ. IASP(2,I)) .AND.
     +                                  (IR .EQ. IASP(3,I))) THEN
                                    ASP(I,1) = ASP(I,1) + PARTIN
                                    ASP(I,2) = ASP(I,2) + PARTEX1
                                    ASP(I,3) = ASP(I,3) + PARTEX2
                                    NB1 = 0
                                ENDIF
                            ENDDO
                            IF(NB1 .EQ. 1) THEN
                                IA = IA + 1
                                IASP(1,IA) = IX
                                IASP(2,IA) = IY
                                IASP(3,IA) = IR
                                ASP(IA,1) = ASP(IA,1) + PARTIN
                                ASP(IA,2) = ASP(IA,2) + PARTEX1
                                ASP(IA,3) = ASP(IA,3) + PARTEX2
                            ENDIF
                        ELSE
                            IA = 1
                            IASP(1,IA) = IX
                            IASP(2,IA) = IY
                            IASP(3,IA) = IR
                            ASP(IA,1) = ASP(IA,1) + PARTIN
                            ASP(IA,2) = ASP(IA,2) + PARTEX1
                            ASP(IA,3) = ASP(IA,3) + PARTEX2
                        ENDIF
                        NG = NG + 1
                    ELSE
                        NB = NB + 1
                    ENDIF
                ENDIF
            ENDIF
        ENDDO
C
C  For US data, see if there is another OBI
C
        IF(ISTYLE .EQ. 2) THEN
            STATUS = 0
C
C  Skip the first HDU
C
            CALL FTMRHD (LUNA,1,HDUTYPEA,STATUS)
            IF (STATUS .EQ. 0) THEN
C
C  Read the FITS file header information
C
                CALL FTGHBN (LUNA,10,NROWSA,TFIELDSA,TTYPEA,
     +                  TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
                IF(IE .EQ. NROWSE) THEN
                    CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
                    IE = 0
C
C  Read the FITS file header information
C
                    CALL FTGHBN (LUNE,10,NROWSE,TFIELDSE,TTYPEE,
     +                  TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
                ENDIF
                GO TO 10
            ENDIF
        ENDIF
        IF ((STATUS .NE. 0) .AND. (STATUS .NE. 107)) THEN
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
        BADSTA = .TRUE.
 	context = 'Unable to read parameters from the FITS header: '
     &  //INANC
        call fcerr(context)
        go to 999
        ENDIF
        STOP
        ELSE
            STATUS = 0
        ENDIF
C
C  Close the input fits files
C
        IF(ISTYLE .EQ. 3) THEN
            CALL FTCLOS(LUNE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
        write(context,
     &	'(''Unable to close  FITS file; Status= '' ,i5)')STATUS
        call fcerr(context)
	context = 'Eventrate extension; FITS file: '//INANC
        call fcerr(context)
        go to 999
            ENDIF
            CALL FTCLOS(LUNA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
        write(context,
     &	'(''Unable to close  FITS file; Status='',i5)')STATUS
        call fcerr(context)
	context = 'Attitude extension; FITS file: '//INANC
        call fcerr(context)
        go to 999
            ENDIF
        ELSE
            CALL FTCLOS(LUNE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
        write(context,
     &	'(''Unable to close  FITS file; Status= '',i5)')STATUS
        call fcerr(context)
	context = ' FITS file:'//INEVE
        call fcerr(context)
        go to 999
            ENDIF
            CALL FTCLOS(LUNA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
        write(context,
     &	'(''Unable to close  FITS file; Status= '',i5)')STATUS
        call fcerr(context)
	context = ' FITS file:'//INATT
        call fcerr(context)
        go to 999
            ENDIF
        ENDIF
C
C  Save and print out diagnostic information
C
        
        call fcecho(' ')
        context =  'File for storing diagnostic information: '
     &	//DIAG
        call fcecho(context)
        call fcecho(' ')
C       OPEN (11,FILE=DIAG,STATUS='OLD',ACCESS='APPEND')
C access='append' not recognized by f90, use this method instead:
        OPEN (11,FILE=DIAG,STATUS='OLD')
C move to end of file
        EOF = .false.
        DO WHILE (.NOT. EOF)
           READ (11,'( )',END=99)
        END DO
  99    CONTINUE

        WRITE (11,*)' '
        WRITE (11,*)'From cast_part.f: '
        WRITE (11,*) 'LIST LENGTH =', IA
        WRITE (11,*) 'GOOD STEPS = ', NG
        WRITE (11,*) 'BAD STEPS =  ', NB
        WRITE (11,*) 'TOTAL TIME = ', TOTIME
        WRITE (11,*) 'TOTAL EXP =  ', TOTEXP
        TMV = TMV/NMV
        WRITE (11,*) 'AVERAGE MV = ', TMV
        WRITE (11,*) 'TOTAL     =  ', TOTIN+TOTEX1+TOTEX2
        WRITE (11,*) 'TOTAL INT =  ', TOTIN
        WRITE (11,*) 'TOTAL EX1 =  ', TOTEX1
        WRITE (11,*) 'TOTAL EX2 =  ', TOTEX2
        CLOSE (11)
C
        write(context,'('' LIST LENGTH = '',i10)')IA
        call fcecho(context)
        write(context,'('' GOOD STEPS = '',i10)')NG
        call fcecho(context)
        write(context,'('' BAD STEPS = '',i10)')NB
        call fcecho(context)
        write(context,'('' TOTAL TIME = '',f10.5)')TOTIME
        call fcecho(context)
        write(context,'('' TOTAL EXP = '',f10.5)')TOTEXP
        call fcecho(context)
        write(context,'('' AVERAGE MV = '',f10.5)')TMV
        call fcecho(context)
        write(context,'('' TOTAL = '',f10.5)')TOTIN+TOTEX1+TOTEX2
        call fcecho(context)
        write(context,'('' TOTAL INT = '',f10.5)')TOTIN
        call fcecho(context)
        write(context,'('' TOTAL EX1 = '',f10.5)')TOTEX1
        call fcecho(context)
        write(context,'('' TOTAL EX2 = '',f10.5)')TOTEX2
        call fcecho(context)
        call fcecho(' ')

C
C  Sort on roll angle
C
        DO I=IA-1,2,-1
            DO II=1,I
                IF(IASP(3,II) .GT. IASP(3,II+1)) THEN
                    NB1 = IASP(1,II)
                    NB2 = IASP(2,II)
                    NB3 = IASP(3,II)
                    A1 = ASP(II,1)
                    A2 = ASP(II,2)
                    A3 = ASP(II,3)
                    IASP(1,II) = IASP(1,II+1)
                    IASP(2,II) = IASP(2,II+1)
                    IASP(3,II) = IASP(3,II+1)
                    ASP(II,1) = ASP(II+1,1)
                    ASP(II,2) = ASP(II+1,2)
                    ASP(II,3) = ASP(II+1,3)
                    IASP(1,II+1) = NB1
                    IASP(2,II+1) = NB2
                    IASP(3,II+1) = NB3
                    ASP(II+1,1) = A1
                    ASP(II+1,2) = A2
                    ASP(II+1,3) = A3
                ENDIF
            ENDDO
        ENDDO



C


C  Read in the 'GAIN CONTROL (IG)' from READVAL subroutine
        IF(IDET .EQ. 2) THEN
        call readval (IG, status)
        ENDIF

C  Read in the detector map
C
        LUN = 90
        IF(AP .GT. 0.) THEN
C
C  If selected, start with the AP map
C
            IF(IG .EQ. 1) THEN
                INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(6)
		context = ' Selection: '// INFILE
		call fcecho(context)
            ELSE
                INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(7)
		context = ' Selection: '// INFILE
		call fcecho(context)
            ENDIF



C Does the user wish to exit now before any calculations are done?
        if (.not. continue) then
         go to 999
        endif


C
C  Return the detector-map array
C
            CALL RETFIT (LUN,INFILE,RMAP,INI2,INI4,1,512,DUM1,
     +          DUM2,DUM3,DUM4,STATUS)
C
C  Center the instrument map, invert the Y-axis, scale it,
C  and turn it real
C
            DO I=20,490
                DO II=30,460
                    TMAP(I,514-II-12) = RMAP(I,II)
                ENDDO
            ENDDO
        ELSE
C
C  Or else start with the internal map
C
            IF(IDET .EQ. 1) THEN
C
C  PSPC C
C
                INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(1)
		context = ' Selection: '// INFILE
		call fcecho(context)

            ELSE
C
C  PSPC B
C
                IF(IG .EQ. 1) THEN
C
C  High Gain
C
                    INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(2)
		context = ' Selection: '// INFILE
		call fcecho(context)
                ELSE
C
C  Low Gain
C
                    INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(3)
		context = ' Selection: '// INFILE
		call fcecho(context)
                ENDIF
            ENDIF
C
C  Return the detector-map array
C
            CALL RETFIT (LUN,INFILE,RMAP,INI2,INI4,1,512,DUM1,
     +          DUM2,DUM3,DUM4,STATUS)
C
C  Center the instrument map, invert the Y-axis, scale it,
C  and turn it real
C
            DO I=20,490
                DO II=30,460
                    TMAP(I,514-II-12) = RMAP(I,II)
                ENDDO
            ENDDO
        ENDIF
C


C
C  Now cast the particle background
C
        STATUS = 0
        IP1 = 1
        IF(AP .GT. 0.) IP1 = 0
        DO IIP=IP1,3
            IF(IIP .EQ. 0) THEN
C
C  If needed, after pulses are cast first.  The code assumes that the
C  instantaneous afterpulse rate is proportional to the internal
C  component rate.  Thus the internal component aspect list is scaled
C  and cast with the AP detector map.
C
                IP = 1
                SCAL = AP/TOTIN
            ELSE
                IP = IIP
                SCAL = 1.
            ENDIF
            NB = IA/10
            NG = 0
            NB1 = -100000
            DO I=1,IA

C call xclock to output the processing status (% completed)
		    call XCLOCK(I,100,10)
                IF(MOD(I,NB) .EQ. 0) THEN
                    NG = NG + 1
                ENDIF
                IX = IASP(1,I)
                IY = IASP(2,I)
                IR = IASP(3,I)
                IF(IR .NE. NB1) THEN
C
C  First nonzero aspect point with this roll angle, make a rotated map
C
                    ANGLE = IR*0.2076017
c MJT 12July96 (g77/linux) cosd nonportable, using cos (radians)
c                   COSROL = COSD(ANGLE)
c                   SINROL = SIND(ANGLE)
                    COSROL = COS(ANGLE*3.14159265/180.0)
                    SINROL = SIN(ANGLE*3.14159265/180.0)
C
C  Zero the temp array
C
                    DO II=25,488
                        DO III=25,488
                            RMAP(II,III) = 0.
                        ENDDO
                    ENDDO
C
C  Calculate the rotated array only once for each roll angle
C
                    DO II=25,488
                        DO III=25,488
                            IF(TMAP(II,III) .NE. 0.) THEN
                                X = (II - 257.)
                                Y = (III - 257.)
                                XX = COSROL*X + SINROL*Y
                                YY = COSROL*Y - SINROL*X
                                IXX = INT(XX + 257.5)
                                IYY = INT(YY + 257.5)
                                RMAP(IXX,IYY) =
     +                              RMAP(IXX,IYY) + TMAP(II,III)
                            ENDIF
                        ENDDO
                    ENDDO
                ENDIF
                NB1 = IR
C
C  Nonzero element, cast it
C
                DO II=25,488
                    IXX = II + IX
                    IF((IXX .GE. 1) .AND. (IXX .LE. 512)) THEN
                        DO III=25,488
                            IF(RMAP(II,III) .NE. 0.) THEN
                                IYY = III + IY
                                IF((IYY .GE. 1) .AND.
     +                                      (IYY .LE. 512)) THEN
                                    EXPARR(IXX,IYY) = EXPARR(IXX,IYY) +
     +                                  SCAL*ASP(I,IP)*RMAP(II,III)
                                ENDIF
                            ENDIF
                        ENDDO
                    ENDIF
                ENDDO
            ENDDO
C
C  Fits output
C
            UNITS = 'counts'
            ISIZE = 512
            BZERO = 0.0
            BSCALE = 1.0
            CDELT1 = -4.15203e-3
            CDELT2 = 4.15203e-3
            LUN = 94
            IF (STATUS .NE. 0) THEN
		write(context,'('' STATUS = '',i5)')STATUS
		call fcecho(context)
                STATUS = 0
            ENDIF
            IF(IIP .EQ. 0) THEN
C
C  AP processing done, write the output file for the model AP map
C
                OUTFIL = 'partap.fits'
                COMMENT = 'Afterpulse background count map'

                CALL PUTFIT(LUN,OUTFIL,EXPARR,INI2,INI4,1,UNITS,
     +              COMMENT,ISIZE,BZERO,BSCALE,CRVAL1,CDELT1,CRVAL2,
     +              CDELT2,STATUS)

C
C  Read in the internal map
C
                IF(IDET .EQ. 1) THEN
                    INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(1)
		context = ' Selection: '// INFILE
		call fcecho(context)
                ELSE
                    IF(IG .EQ. 1) THEN
                     INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(2)
		context = ' Selection: '// INFILE
		call fcecho(context)
                    ELSE
                        INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(3)
		context = ' Selection: '// INFILE
		call fcecho(context)
                    ENDIF
                ENDIF
            ELSEIF(IIP .EQ. 1) THEN
C
C  Internal PB processing done, open the output file for the model
C  internal PB map
C
                OUTFIL = 'partin.fits'
                COMMENT = 'Internal particle background count map'

                CALL PUTFIT(LUN,OUTFIL,EXPARR,INI2,INI4,1,UNITS,
     +              COMMENT,ISIZE,BZERO,BSCALE,CRVAL1,CDELT1,CRVAL2,
     +              CDELT2,STATUS)

C
C  Read in the external map
C
                IF(IG .EQ. 1) THEN
                    INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(4)
		context = ' Selection: '// INFILE
		call fcecho(context)
                ELSE
                    INFILE = DIRSTR(1:lenact(dirstr))//INSTMP(5)
		context = ' Selection: '// INFILE
		call fcecho(context)
                ENDIF
            ELSEIF(IIP .EQ. 2) THEN
C
C  External flat PB processing done, open the output file for the model
C  external flat PB map
C
                OUTFIL = 'partex1.fits'
                COMMENT = 'External #1 particle background count map'

                CALL PUTFIT(LUN,OUTFIL,EXPARR,INI2,INI4,1,UNITS,
     +              COMMENT,ISIZE,BZERO,BSCALE,CRVAL1,CDELT1,CRVAL2,
     +              CDELT2,STATUS)

            ELSEIF(IIP .EQ. 3) THEN
C
C  External Al K alpha PB processing done, open the output file for
C  the model external Al K alpha PB map
C
                OUTFIL = 'partex2.fits'
                COMMENT = 'External #2 particle background count map'

                CALL PUTFIT(LUN,OUTFIL,EXPARR,INI2,INI4,1,UNITS,
     +              COMMENT,ISIZE,BZERO,BSCALE,CRVAL1,CDELT1,CRVAL2,
     +              CDELT2,STATUS)

            ENDIF
C
C  Open the FITS file for the instrument map
C
            IF(IIP .LT. 3) THEN
                LUN = 90
C
C  Return the detector-map array
C
                CALL RETFIT (LUN,INFILE,RMAP,INI2,INI4,1,512,DUM1,
     +              DUM2,DUM3,DUM4,STATUS)
C
C  Center the instrument map, invert the Y-axis, and turn it real
C
                DO I=20,490
                    DO II=30,460
                        TMAP(I,514-II-12) = RMAP(I,II)
                    ENDDO
                ENDDO
            ENDIF
C
C  Zero the output array
C
            IF(IIP .LE. 3) THEN
                DO I=1,512
                    DO II=1,512
                        EXPARR(I,II) = 0.
                    ENDDO
                ENDDO
            ENDIF
        ENDDO
C
999     continue
        STOP
        END


C**********************************************************************
C SUBROUTINE:
C      gcastpt
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C
C NOTES:
C       gcastpt uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C     call gcastpt(OBS, ISTYLE, AP, CONTINUE, CALDBDIR, STATUS)
C
C ARGUMENTS:
C      OBS     - Observation control (prefix of the data files)
C      ISTYLE  - 1 for German data, 2 for US data and 3 for RDF data
C      AP      - The number of AP counts
C      CALDBDIR- The directory structure for caldb files
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

      subroutine gcastpt(OBS, ISTYLE, AP, CONTINUE, CALDBDIR, STATUS)
      character(8) OBSSTR
      character(1) OBS(8)
      character(80) context
      character(80) caldbdir
      integer istyle, status
      real*4 ap
      logical continue

C  initialize variables
      status = 0

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

C  get the  number of AP counts 
      call uclgsr('ap',ap,status)
	    if (status .ne. 0) then
      context = 'could not get AP parameter'
      call fcerr(context)
       goto 999
      endif

C  Does the user wish to continue?
      call uclgsb('continue',continue,status)
	    if (status .ne. 0) then
      context = 'could not get CONTINUE parameter'
      call fcerr(context)
       goto 999
      endif
       
C  get the contents of the CALDBDIR string
      call uclgst('caldbdir',caldbdir,status)
	    if (status .ne. 0) then
      context = 'could not get CALDBDIR parameter'
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
C       readval
C
C DESCRIPTION:
C       This routine reads valid_times.dat file to evaluate the gain control
C
C       Written by: Srilal Weera  (May 1996)
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
C           High gain:   before 11 October 1991
C           Low gain :   after  11 October 1991

C USAGE:
C       call readval (gain, status)
C
C ARGUMENTS:
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
      subroutine  readval (gain, status)


      integer icount, iunit, status, gain, breakpt
      integer count(100), startt(100),endt(100)
      character(80) infile, context
 
      status = 0
      iunit = 18
      breakpt = 42996070

C Open valid_times.dat file
      infile = 'valid_times.dat'
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
	gain = 1
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

