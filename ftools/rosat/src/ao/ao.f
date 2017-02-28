C $Id: ao.f,v 1.10 2013/05/21 19:08:36 irby Exp $
C*********************************************************************
C SELECTOR TASK:
C      			ao
C
C FILE:
C      ao.f
C
C DESCRIPTION:
C      
C
C AUTHOR:
C	Steve Snowden
C       FTOOLS development: Srilal Weera  
C
C MODIFICATION HISTORY:
C
C Ning Gan, RSTX, 05-Jan-2000
C 1.3   Changed all the filenames from uppercase to lowercase.
C James Peachey, HEASARC/GSFC/NASA, Hughes STX / 19-SEP-1997
C 1.2   Replaced included file CCONST.INC and DATA statements with
C       new included file ROSCONST.INC. Files CPOS.CMN, CDENS.CMN and
C       ROSCONST.INC are now located in roslib/include. Moved subroutines
C       access_caldb, readfits, read_sgdat, and rsp_read to roslib/src/gen
C     
C 1.1 corrected double precision manipulations in ao.f.
C
C
C NOTES:
C   
C
C USAGE:
C      HOST: call ao
C      IRAF: task ao
C
C ARGUMENTS:
C      none
C
C
C CALLED ROUTINES:
C      subroutine gao - gets parameters from parameter file
C      subroutine access_caldb - gets the caldb files using gtcalf
C      subroutine readfits - reads a column of data from a FITS file
C      subroutine rsp_read - reads a response matrix
C      subroutine read_sgdat - reads data from caldb FITS file for SG.DAT
C      subroutine ssx - main calculations are done here (see ancillary.f) 
C      
C
C******************************************************************************




			 subroutine ao

C        PROGRAM AO
C
CC  Driving program for the calculation of scattered X-ray intensity
C
C************************ FFORM VERSION 1.2 ************ 25-NOV-92 12:22
C
CA  author : SLS        date: 18-DEC-1987 09:06
CU  update : SLS        date: 25-FEB-1988 13:19
CU  update : SXRB       date: 24-NOV-1992 11:47
CU  update : SNOWDEN    date:  1-OCT-1993
CU  update : CUI	date:  6-DEC-1993
CU           Use "valid_times_all.dat" (output of get_valid_time.f)
CU           for better time selection.
CU  update : SNOWDEN    date: 27-JAN-1994
CU           More hacking to get it working on unix
CU  update : SNOWDEN    date: 19-APR-1994
CU           add fitsio error status, enable RDF, and fitsio of DRM.FITS
C
CT  status : not tested
C
C   general description
CG  This program organizes the calculation of atmospheric column
CG  densities along the line of sight of the observation and the
CG  intensity of scattered solar X-rays from those column densities
CG  for pointed observations.  It uses the subroutine SSX to calculate
CG  the scattered solar spectrum.  It also calculates the count rates
CG  in the seven standard pulse-height bands.  The count rates are not
CG  normalized but do tend to have required scale factors near 1.  The
CG  program is written to use the linear combination of two thermal
CG  spectra to model the solar spectrum.  The output of ao.f is used
CG  by the program rate_fit.f to fit the observation light curve.
CG
CG  The program has been adapted to both US and German data sets for
CG  input with the following assignments.
CG  German data
CG  ATTITUDE.MT   xxx_attitude.fits    xxx is an arbitrary string up
CG                                         to 8 characters long and is
CG                                         queried for
CG  ORBIT.MT      xxx_orbit.fits
CG  EVENTRATES.MT xxx_eventrates.fits
CG
CG  US data
CG  *.sa          xxx_attitude1.fits   * is the data set name
CG  *.so          xxx_orbit.fits
CG  *.evr         xxx_eventrates.fits
CG
CG  Other required input
CG  valid_times.dat           an ascii list of accepted times
CG                            n  begin end     in S/C seconds
CG  DRM.DAT                   detector response matrix
CG  WINDOW_B.DAT              window transmission
CG  EFF_AREA.DAT              on-axis effective areas
CG  GAS_EFF.DAT               gas stopping efficiency
CG
CG  Besides FITSIO routines, AO requires the SSX and ASTROLIB librarys.
C
C   call_var.          type I/O description
CP
C
C   include_block name          description
CI  CCONST.CMN                   contains useful constants
C   CPOS.CMN
C   CGENL.CMN
C   CDENS.CMN
C
C   routines_called    type     description
CR  ANGSEP              SR      ASTROLIB - returns angle between two
CR                                   directions
CR  BAN                 SR      bins the fine bin input spectrum into
CR                                   coarse bands
CR  DATEJD              SR      ASTROLIB - date conversion to Julian
CR  DEL_ANG             SR      calculates angle between two directions
CR  FTOPEN              SR      FITSIO - opens fits files
CR  FTGCFD              SR      FITSIO - read R*8 entry in fits table
CR  FTGCFE              SR      FITSIO - read R*4 entry in fits table
CR  FTGCFJ              SR      FITSIO - read I*4 entry in fits table
CR  FTGHBN              SR      FITSIO - reads binary fits heade
CR  FTGHTB              SR      FITSIO - reads ascii fits header
CR  FTMRHD              SR      FITSIO - skips HDU
CR  GETRAY_N            SR      returns R&S thermal spectra
CR  LIVTIM              SR      returns PSPC livetime scale factors
CR  SSX                 SR      driving routine
C
C   extensions/system calls     description
CX
C
C***********************************************************************
C
C   variables   meaning
C
        IMPLICIT NONE
C
C       PARAMETER NUM=729, NUM1=256
C
C  Include common blocks
C
        INCLUDE '../../../roslib/include/ROSCONST.INC'
        INCLUDE '../../../roslib/include/CPOS.CMN'
        INCLUDE '../../../roslib/include/CDENS.CMN'

C JP 19-SEP-1997: deleted DATA statements here
C
C  Delete the following line when compiling under VMS.
C       REAL*4 DEG, VALMAX, VALMIN, VALEPS
C
        INTEGER*2 IT2
        INTEGER*4 BITPIX, BLOCK, BLOCKA, BLOCKE, BLOCKO, DATE, DEC, 
     +      GCOUNT, HDUTYPEE, HDUTYPEA, HDUTYPEO, I, IA, IA1LL, 
     +      IACCOF, IACTBE(1000), IACTEN(1000), IAXE, IE, IERR, 
     +      IFLAG, IH, II, ILIVEN, IM, IS, ISTOT, 
     +      IO, IOS, ISEL, ISTYLE, IT1, ITEMP, K, LUN, LUNA,ivalid, 
     +      LUNE, LUNO, MV, N, NAXIS, NAXES(10), NN, NROWSA, NROWSE, 
     +      NROWSO, PCOUNT, RA,
     +      ROWLENA, ROWLENE, ROWLENO, SEC1, SEC2, STATUS, IAEXE,
     +      TBCOLA(20), TBCOLE(35),
     +      TBCOLO(23), TFIELDSA, TFIELDSE, TFIELDSO, TIME,
     +      VARIDATA, VARIDATE, VARIDATO
C
        REAL*4 A1LL, AEXE, ALTITUDE, AREA(729), AXE, BAND(8),
     +      DATREF, DEADTP, DECLOOK, DECSAT, DECSUN,
     +      DENSIT(6), FLIVE, FLIVE1, FLIVE2,
     +      GAS(729), HOUR, HOURANGLE, LATITUDE, LONGITUDE, R, RALOOK,
     +      RASAT, RASUN, RMV, SCALE1, SCALE2,
     +      SPECIN(729), SPECIN1(729), SPECIN2(729), SPCOUT(729),
     +      TLOG1, TLOG2, WINDOW(729), X, XSUN, Y, YSUN, Z, ZSUN
C
        REAL*8 DJACBE, DJACEN, DJUL, DJULAT, DJLIEN, DSCS, DT, SEC,
     +		SCSO, SECD
       	REAL*4 ENRGLO(729), ENRGHI(729)
C
        character(1) OBS(8), OBS1
        character(2) OBS2
        character(3) OBS3
        character(4) OBS4
        character(5) OBS5
        character(6) OBS6
        character(7) OBS7
        character(8) OBS8
        character(20) TTYPEA(20), TTYPEE(35), TTYPEO(23), TFORMA(20),
     +      TFORME(35), TFORMO(23), TUNITA(20), TUNITE(35), TUNITO(23)
        character(20) EXTNAMEA, EXTNAMEE, EXTNAMEO
	character(80) context, DIRSTR
        character(80) INANC, INATT, INEVE, INFILE, INORB
        character(80) calfile, valfile
        LOGICAL  ANYFA, ANYFE, ANYFO, BADSTA, EXTEND, 
     +      FLAGVALA, FLAGVALE, FLAGVALO, LCHECK, SIMPLE
	integer UNIT1, UNIT2, chatter
C
        EQUIVALENCE (OBS(1),OBS1,OBS2,OBS3,OBS4,OBS5,OBS6,OBS7,OBS8)
C
C The following are for the common block GEOSL1
C JULFTMP is REAL*4 since DATE_JULIAN column is real*4.
        INTEGER*4 IDATE(3000)
	REAL*4 F107(3000), F107AV(3000)
	REAL*4 JULFTMP(3000)
	REAL*4 KP1(3000), KP2(3000), KP3(3000), KP4(3000)
	REAL*4 KP5(3000), KP6(3000), KP7(3000), KP8(3000)
C
	DATA DIRSTR /' '/
        DATA DEADTP /234./
        DATA LCHECK /.TRUE./
        DATA BADSTA /.FALSE./
        DATA IA, IE, IO /0, 0, 0/
C
C  This common block passes the values of the arrays read by read_sgdat
C  routine to the GEOSL1 subroutine in ancillary.f

       COMMON /GEOSL1/ IDATE, JULFTMP, F107, F107AV,
     &        KP1, KP2, KP3, KP4, KP5, KP6, KP7, KP8


C
C    LENACT is used to correctly concatenate a filename to 'dirstr'
      integer lenact
      external lenact
      	character(40) taskname
      	common /task/ taskname

      	taskname = 'ao v1.3'
      	call ftcmsg

C Initialize:
	ISTYLE = 0
	STATUS = 0
	DIRSTR = ' '
	chatter = 9
	NN = 0
	djulat = 0.d0
	djlien = 0.d0

C  get parameters from parameter file
       call gao(OBS, ISTYLE, IFLAG, TLOG1, SCALE1,
     & TLOG2, SCALE2, DIRSTR, STATUS)
       if (status .ne. 0) then
      context = 'ao.f: Error in obtaining parameters'
      call fcerr(context)
       goto 999
      endif


        ISEL = 0
C



C
C  Create the file names
C
        IF(OBS8 .NE. '        ') THEN
            DO I=2,8
                IF((OBS(I) .EQ. ' ') .AND. (OBS(I-1) .NE. ' '))
     +                  NN = I - 1
            ENDDO
            IF((OBS8 .NE. '        ') .AND. (NN .EQ. 0)) NN = 8
            IF(NN .EQ. 1) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3010) OBS1
 3010               FORMAT(A1,'_anc.fits')
                ELSE
                    WRITE(INORB,3015) OBS1
 3015               FORMAT(A1,'_orbit.fits')
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        WRITE(INATT,3020) OBS1
 3020                   FORMAT(A1,'_attitude.fits')
                    ELSE
                        WRITE(INATT,3025) OBS1
 3025                   FORMAT(A1,'_attitude1.fits')
                    ENDIF
                    WRITE(INEVE,3030) OBS1
 3030               FORMAT(A1,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 2) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3035) OBS2
 3035               FORMAT(A2,'_anc.fits')
                ELSE
                    WRITE(INORB,3040) OBS2
 3040               FORMAT(A2,'_orbit.fits')
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        WRITE(INATT,3045) OBS2
 3045                   FORMAT(A2,'_attitude.fits')
                    ELSE
                        WRITE(INATT,3050) OBS2
 3050                   FORMAT(A2,'_attitude1.fits')
                    ENDIF
                    WRITE(INEVE,3055) OBS2
 3055               FORMAT(A2,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 3) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3060) OBS3
 3060               FORMAT(A3,'_anc.fits')
                ELSE
                    WRITE(INORB,3065) OBS3
 3065               FORMAT(A3,'_orbit.fits')
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        WRITE(INATT,3070) OBS3
 3070                   FORMAT(A3,'_attitude.fits')
                    ELSE
                        WRITE(INATT,3075) OBS3
 3075                   FORMAT(A3,'_attitude1.fits')
                    ENDIF
                    WRITE(INEVE,3080) OBS3
 3080               FORMAT(A3,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 4) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3085) OBS4
 3085               FORMAT(A4,'_anc.fits')
                ELSE
                    WRITE(INORB,3090) OBS4
 3090               FORMAT(A4,'_orbit.fits')
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        WRITE(INATT,3095) OBS4
 3095                   FORMAT(A4,'_attitude.fits')
                    ELSE
                        WRITE(INATT,3100) OBS4
 3100                   FORMAT(A4,'_attitude1.fits')
                    ENDIF
                    WRITE(INEVE,3105) OBS4
 3105               FORMAT(A4,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 5) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3110) OBS5
 3110               FORMAT(A5,'_anc.fits')
                ELSE
                    WRITE(INORB,3115) OBS5
 3115               FORMAT(A5,'_orbit.fits')
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        WRITE(INATT,3120) OBS5
 3120                   FORMAT(A5,'_attitude.fits')
                    ELSE
                        WRITE(INATT,3125) OBS5
 3125                   FORMAT(A5,'_attitude1.fits')
                    ENDIF
                    WRITE(INEVE,3130) OBS5
 3130               FORMAT(A5,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 6) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3135) OBS6
 3135               FORMAT(A6,'_anc.fits')
                ELSE
                    WRITE(INORB,3140) OBS6
 3140               FORMAT(A6,'_orbit.fits')
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        WRITE(INATT,3145) OBS6
 3145                   FORMAT(A6,'_attitude.fits')
                    ELSE
                        WRITE(INATT,3150) OBS6
 3150                   FORMAT(A6,'_attitude1.fits')
                    ENDIF
                    WRITE(INEVE,3155) OBS6
 3155               FORMAT(A6,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 7) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3160) OBS7
 3160               FORMAT(A7,'_anc.fits')
                ELSE
                    WRITE(INORB,3165) OBS7
 3165               FORMAT(A7,'_orbit.fits')
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        WRITE(INATT,3170) OBS7
 3170                   FORMAT(A7,'_attitude.fits')
                    ELSE
                        WRITE(INATT,3175) OBS7
 3175                   FORMAT(A7,'_attitude1.fits')
                    ENDIF
                    WRITE(INEVE,3180) OBS7
 3180               FORMAT(A7,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 8) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INANC,3185) OBS8
 3185               FORMAT(A8,'_anc.fits')
                ELSE
                    WRITE(INORB,3190) OBS8
 3190               FORMAT(A8,'_orbit.fits')
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        WRITE(INATT,3195) OBS8
 3195                   FORMAT(A8,'_attitude.fits')
                    ELSE
                        WRITE(INATT,3200) OBS8
 3200                   FORMAT(A8,'_attitude1.fits')
                    ENDIF
                    WRITE(INEVE,3205) OBS8
 3205               FORMAT(A8,'_eventrates.fits')
                ENDIF
            ENDIF
        ENDIF
C

C
C  Set program control IFLAG=1 short, IFLAG=2 long
C
C  Set accepted time offset, usually 30 seconds, the program will start
C  its calculation IACCOF seconds ahead of the starting accepted time
C  and end IACCOF seconds after the ending accepted time.
C
C        PRNT *, 'Enter accepted time offset'
C        READ *, IACCOF
        IACCOF = 60
C
C  Open position input file.  This is an ascii file derived from
C  the ORBIT file provided by SASS
C
        IF(OBS8 .EQ. '        ') THEN
            call fcecho('Enter ORBIT table name')
            READ 1000, INORB
 1000       FORMAT(A80)
        ENDIF
        context = INORB
	call fcecho(context)

C       LUNO = 91
	call cgetlun(LUNO)
C
        IF(ISTYLE .EQ. 3) THEN
C
C  Open the RDF extension
C
            CALL FTOPEN(LUNO,INANC,0,BLOCKO,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to open infile (Orbit extension):'
     &	    //INANC
            call fcerr(context)
            go to 999
            ENDIF
C
C  Skip the proper RDF extension
C
            CALL FTMAHD (LUNO,3,HDUTYPEO,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to move to the FITS extension'
            call fcerr(context)
            go to 999
            ENDIF
C
C  Read the FITS file header information
C
            CALL FTGHBN (LUNO,23,NROWSO,TFIELDSO,TTYPEO,
     +          TFORMO,TUNITO,EXTNAMEO,VARIDATO,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information'
            call fcerr(context)
            go to 999
            ENDIF
C
C  Open the German or US orbit file
C
        ELSE
            CALL FTOPEN(LUNO,INORB,0,BLOCKO,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to open infile: '//INORB
            call fcerr(context)
            go to 999
            ENDIF
C
C  Skip the first HDU
C
            CALL FTMRHD (LUNO,1,HDUTYPEO,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context='Unable to move to the FITS extension (Position 1):'
     &	    //INORB
            call fcerr(context)
            go to 999
            ENDIF
C
C  Read the FITS file header information
C
            IF(ISTYLE .EQ. 1) THEN
                CALL FTGHTB (LUNO,23,ROWLENO,NROWSO,TFIELDSO,TTYPEO,
     +              TBCOLO,TFORMO,TUNITO,EXTNAMEO,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information:'
     &	    //INORB
            call fcerr(context)
            go to 999
                ENDIF
            ELSE
                CALL FTGHBN (LUNO,23,NROWSO,TFIELDSO,TTYPEO,
     +              TFORMO,TUNITO,EXTNAMEO,VARIDATO,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information:'
     &	    //INORB
            call fcerr(context)
            go to 999
                ENDIF
            ENDIF
        ENDIF
C
C  Open the attitude data file.  The first three reads are to skip
C  past some header lines produced by the MIDAS prnt statement.
C
        IF(OBS8 .EQ. '        ') THEN
            call fcecho('Enter ATTITUDE table name')
            READ 1000, INATT
        ENDIF
            context = INATT
            call fcecho(context)
C       LUNA = 92
	call cgetlun(LUNA)
C
        IF(ISTYLE .EQ. 3) THEN
C
C  Open the RDF extension
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
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to move to the FITS extension: '
     &	    //INANC
            call fcerr(context)
            go to 999
            ENDIF
C
C  Read the FITS file header information
C
            CALL FTGHBN (LUNA,20,NROWSA,TFIELDSA,TTYPEA,
     +          TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information:'
     &	    //INANC
            call fcerr(context)
            go to 999
            ENDIF
C
C  Open the German or US orbit file
C
        ELSE
            CALL FTOPEN(LUNA,INATT,0,BLOCKA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context = 'Unable to open infile :'
     &	    //INATT
            call fcerr(context)
            go to 999
            ENDIF
C
C  Skip the first HDU
C
            CALL FTMRHD (LUNA,1,HDUTYPEA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
            context='Unable to move to the FITS extension (Position 2):'
     &	    //INATT
            call fcerr(context)
            go to 999
            ENDIF
C
C  Read the FITS file header information
C
            IF(ISTYLE .EQ. 1) THEN
                CALL FTGHTB (LUNA,20,ROWLENA,NROWSA,TFIELDSA,TTYPEA,
     +              TBCOLA,TFORMA,TUNITA,EXTNAMEA,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information:'
     &	    //INATT
            call fcerr(context)
            go to 999
                ENDIF
            ELSE
                CALL FTGHBN (LUNA,20,NROWSA,TFIELDSA,TTYPEA,
     +              TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information:'
     &	    //INATT
            call fcerr(context)
            go to 999
                ENDIF
            ENDIF
        ENDIF
C
C  Open the event rate file.  The data are used to calculate the
C  livetime fraction
C
        IF(OBS8 .EQ. '        ') THEN
            call fcecho('Enter EVENTRATE table name')
            READ 1000, INEVE
        ENDIF
            context = INEVE
            call fcecho(context)
c       LUNE = 93
	call cgetlun(LUNE)
C
        IF(ISTYLE .EQ. 3) THEN
C
C  Open the RDF extension
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
            context = 'Unable to move to the FITS extension:'
     &	    //INANC
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
            context = 'Unable to read the FITS file header information:'
     &	    //INANC
            call fcerr(context)
            go to 999
            ENDIF
C
C  Open the German or US orbit file
C
        ELSE
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
            context='Unable to move to the FITS extension (Position 3):'
     &	    //INEVE
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
                    BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information'
            call fcerr(context)
            go to 999
                ENDIF
            ENDIF
        ENDIF
C
C  Open accepted time input file.  This can be created from the SASS
C  provided data or modified to your heart's desire.
C
C  IACTBE = start of accepted time interval in S/C seconds
C  IACTEN = end of accepted time interval in S/C seconds
C
        VALFILE = 'valid_times_all.dat'
	call cgetlun(IVALID)
        OPEN(UNIT=IVALID,STATUS='OLD',
     +          FILE=VALFILE,FORM='FORMATTED')
C
C  Read in the accepted time intervals
C
        IS = 0
        IOS = 0
        DO WHILE (IOS .EQ. 0)
            IS = IS + 1
            READ(IVALID,*,IOSTAT=IOS) ITEMP, IACTBE(IS), IACTEN(IS)
            IF(IOS .EQ. 0) then
        write(context,'(''BEGIN = '',i10,'' END = '',i10)') 
     &  IACTBE(IS), IACTEN(IS)
 	call fcecho(context)
	    ENDIF
        ENDDO
	CLOSE(IVALID)
        ISTOT = IS - 1

        call fcecho(' ')
        write(context,'(''TOTAL NUMBER OF INTERVALS IS:'',i4)') ISTOT
 	call fcecho(context)
        call fcecho(' ')
        
C
C  Open the output files
C     Output #1, AO_SSX_SC.OUT:  Rates in the seven energy bands plus
C         total band for each time step listed by S/C seconds
C     Output #2, AO_SSX_COL.OUT:  Column densities for the steps
C
C  Open output files
C
	call cgetlun(UNIT1)
        OPEN(UNIT=UNIT1,STATUS='UNKNOWN',FILE='AO_SSX_SC.OUT',
     &	FORM='FORMATTED')
	call cgetlun(UNIT2)
        OPEN(UNIT=UNIT2,STATUS='UNKNOWN',FILE='AO_COL.OUT',
     &	FORM='FORMATTED')

C
C  Enter the XRT/PSPC parameter files to calculate the grasp
C
C  Read in the on-axis effective area
C
C   Access CALDB to get the name of the fixed input file for  EFFAREA:  
C
C Note: 'eff_area' fits file has INSTRUME='XRT' 
C       This adjustment is made inthe access_caldb routine
       call access_caldb(INANC, 'EFFAREA', calfile, status)
       if (status .ne. 0) then
       context = 'ao.f: Error in accessing CALDB file for EFFAREA'
       call fcerr(context)
       goto 999
       endif
C
C
C Use 'readfits' routine to read the column of data from fits file
C note: since we need to extract just one column, the same info is repeated
C twice below. One is discarded.
C ( The readfits routine retrieves two columns, which is needed by other files.)
C
      call readfits(calfile,'EFFAREA',AREA,729,
     &  'EFFAREA',AREA,729,status)
C
C
C  Read in the window transmission
C 
C   Access CALDB to get the name of the fixed input file for  WTRANS:  
       call access_caldb(INANC, 'WTRANS', calfile,  status)
       if (status .ne. 0) then
       context = 'ao.f: Error in accessing CALDB file for WTRANS'
       call fcerr(context)
       goto 999
       endif
C
C
C Use 'readfits' routine to read the column of data from fits file
C note: since we need to extract just one column, the same info is repeated
C twice below. One is discarded.
C ( The readfits routine can retrieve two columns.)
C
      call readfits(calfile,'TRANSMIS',WINDOW,729,
     &  'TRANSMIS',WINDOW,729,status)
C
C
C  Read in the gas efficiency
C
C   Access CALDB to get the name of the fixed input file for  DET_EFF:  
       call access_caldb(INANC, 'DET_EFF', calfile, status)
       if (status .ne. 0) then
       context = 'ao.f: Error in accessing CALDB file for DET_EFF'
       call fcerr(context)
       goto 999
       endif
C
C
C Use 'readfits' routine to read the column of data from fits file
C note: since we need to extract just one column, the same info is repeated
C twice below. One is discarded.
C ( The readfits routine can retrieve two columns.)
C
      call readfits(calfile,'DET_EFF',GAS,729,
     &  'DET_EFF',GAS,729,status)

C
C  Get the ROSAT energy grid 
C (Note this used to be done in SSX/GETGRD routine -Srilal)
C
C   Access CALDB to get the name of the fixed input file for 'ENERGY_GRID' :  
       call access_caldb(INANC, 'ENERGY_GRID', calfile, status)
       if (status .ne. 0) then
       context = 'ao.f: Error in accessing CALDB file for ENERGY_GRID'
       call fcerr(context)
       goto 999
       endif
C
C Use 'readfits' routine to read the columns of data from fits file
C note: We need to read two columns this time !
C (to evaluate DELE and ECEN based on ENERG_LO & ENERG_HI)
C
      call readfits(calfile,'ENERG_LO',enrglo,729,
     &  'ENERG_HI',enrghi,729,status)
C
C Evaluate DELE and ECEN from 'ENERG_LO' and 'ENERG_HI' (convert kev)
C
        do  510 I= 1, 729
       ECEN(i) = (ENRGHI(i) + ENRGLO(i))* 1000.0/2.0
       DELE(i) = (ENRGHI(i) - ENRGLO(i))* 1000.0
 510       continue

C
C Read caldb Solar-geophys-data file
C
C   Access the CALDB to get the name of the input file for 'SOLAR_GEOPHYS' :  
       call access_caldb(INANC,'SOLAR_GEOPHYS',calfile,status)
       if (status .ne. 0) then
       context='ao.f: Error in accessing file SOLAR_GEOPHYS'
       call fcerr(context)
       goto 999
       endif

c       calFILE = DIRSTR(1:lenact(dirstr))//'solar_geophys_data_v1.fits'

 	 call read_sgdat(calfile,'DATE',IDATE,2099,'DATE_JULIAN',JULFTMP 
     &	 ,2099,'SOLAR_FLUX',F107,2099, 'AVERAGE_SOLAR_FLUX',F107AV,
     &   2099, 'KP1',KP1,2099, 'KP2',KP2,2099, 'KP3',KP3,2099,
     &   'KP4',KP4,2099, 'KP5',KP5,2099,'KP6',KP6,2099,'KP7',KP7,2099,
     &   'KP8',KP8,2099,status)

C
C  Read the Detector Response Matrix from CALDB  (~DRM.FITS)
C
C   Access CALDB to get the name of the fixed input file for 'MATRIX'  
C
       call access_caldb(INANC,'MATRIX',calfile,status)
       if (status .ne. 0) then
       context = 'ao.f: Error in accessing CALDB file for RSP MATRIX'
       call fcerr(context)
       goto 999
       endif
C
c       LUN = 95

c       calFILE=DIRSTR(1:lenact(dirstr))//'DRM.FITS'
c     calFILE='/caldb/data/rosat/pspc/cpf/matrices/pspcb_gain2_256.rmf'

	call cgetlun(LUN)
        CALL FTOPEN(LUN,CALFILE,0,BLOCK,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
            context = 'Unable to open infile :'
     &	    //CALFILE
            call fcerr(context)
            go to 999
        ENDIF
C
C  Read the FITS file header information
C
        CALL FTGHPR (LUN,10,SIMPLE,BITPIX,NAXIS,NAXES,
     +      PCOUNT,GCOUNT,EXTEND,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
            context = 'Unable to read the FITS file header information:'
     &	    //CALFILE
            call fcerr(context)
            go to 999
        ENDIF
cc
C
C   move to the extension in the input file
C
           CALL FTMRHD (LUN,1,HDUTYPEA,STATUS)
      if ( status .ne. 0 ) then
         context = 'Error moving to the specified extension'
         call fcerr(context)
         goto 999
      endif
C
C  Read the data file
C

      call rsp_read(lun,729,256, DRM , chatter, status)


c       CALL FTG2DE (LUN,0,0,NAXES(1),NAXES(1),NAXES(2),DRM,
c    +          ANYF,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
            context = 'Unable to read the Data file :'
     &	    //CALFILE
            call fcerr(context)
            go to 999
        ENDIF
C
C  Close the FITS file
C
        CALL FTCLOS(LUN,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
        context='Unable to close  FITS file: '//CALFILE
        call fcerr(context)
        go to 999
        ENDIF



C
C  Put it all together.  The solid angle of the XRT/PSPC is 0.0008728 sr
C  and the effective vignetting and obscuration by the window support
C  structure below ~1 keV is ~0.625.  Putting the two together yields
C  the 0.0005455 scale factor.
C
        DO I=1,729
            CTRRSP(I) = WINDOW(I)*AREA(I)*GAS(I)
            DO II=1,256
                DRM(II,I) = DRM(II,I)*CTRRSP(I)*0.0005455
            ENDDO
        ENDDO
C
C  Enter a temperature (log10 T) for the solar spectrum, units of
C  photons/cm**2/s/bin.  The scale factor of 1.E23 is a rough
C  normalization for the solar spectrum which makes later scale
C  factors come out to something like 1.
C

        CALL GETRAY_N(INANC, TLOG1,SPECIN1, dirstr)
        DO I=1,729
            SPECIN(I) = 1.E23*SCALE1*SPECIN1(I)
        ENDDO
C
C  Enter a temperature (log10 T) for the solar spectrum, units of
C  photons/cm**2/s/bin.  Combine with the first spectrum.
C  (These are read separately in gao routine - Srilal)

        IF((TLOG2 .GT. 0.) .AND. (SCALE2 .GT. 0.)) THEN
            CALL GETRAY_N(INANC, TLOG2,SPECIN2, dirstr)
            DO I=1,729
                SPECIN(I) = SPECIN(I) + 1.E23*SCALE2*SPECIN2(I)
            ENDDO
        ENDIF
C
C  Write out the spectrum if desired
C
        IF(ISEL .EQ. 1) THEN
            WRITE(82) SPECIN
            CLOSE(82)
            ISEL = 0
        ENDIF
C
C  Start the loop over the data
C
   10   CONTINUE
        IS = 1
	SCSO = 0.D0
        DO WHILE ((STATUS .EQ. 0) .AND. (IO .LT. NROWSO))
C
C  Read in the ORBIT position information
C
C  DJUL = Julian date of observation step
C  RASAT = right ascension of satellite in degrees
C  DECSAT = declination of satellite in degrees
C  ALT = altitude of satellite in km
C  RASUN = right ascension of the SUN in degrees
C  DECSUN = declination of the SUN in degrees
C
            IO = IO + 1
            IF(ISTYLE .EQ. 3) THEN
                CALL FTGCFJ(LUNO,1,IO,1,1,DATE,FLAGVALO,ANYFO,STATUS)
                CALL FTGCFD(LUNO,2,IO,1,1,SECD,FLAGVALO,ANYFO,STATUS)
                DJUL = 2400000.5 + DATE + SECD
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
        context='Unable to read FITS file data (Position 4):'
     &	    //INANC
        call fcerr(context)
	goto 999
                ENDIF
            ELSE
                IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                    CALL FTGCFJ(LUNO,1,IO,1,1,DATE,FLAGVALO,
     +                      ANYFO,STATUS)
                    CALL FTGCFD(LUNO,2,IO,1,1,SEC,FLAGVALO,
     +                      ANYFO,STATUS)
                ELSEIF(ISTYLE .EQ. 2) THEN
                    CALL FTGCFJ(LUNO,1,IO,1,1,DATE,FLAGVALO,
     +                      ANYFO,STATUS)
                    CALL FTGCFJ(LUNO,2,IO,1,1,SEC1,FLAGVALO,
     +                      ANYFO,STATUS)
                    CALL FTGCFJ(LUNO,3,IO,1,1,SEC2,FLAGVALO,
     +                      ANYFO,STATUS)
                    SEC = SEC1 + SEC2/10000.D0
                ENDIF
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
        context='Unable to read FITS file data (Position 5):'
     &	    //INORB
        call fcerr(context)
	goto 999
                ENDIF
C
C  Translate the date
C
                DT = 19000000.D0 + DATE
                IH = SEC/3600.
                IM = (SEC - 3600.*IH)/60.
                SEC = (SEC - 3600.*IH - 60.*IM)
                DT = DT + IH/100.D0 + IM/10000.D0 + SEC/1000000.D0
C
C  Find the Julian day
C
                CALL DATEJD(DT,LCHECK,DJUL,IERR)
            ENDIF
C
            IF (DJUL .LT. SCSO) IS = 1
            SCSO = DJUL
            DJACBE = 2448044.379733750D0+(IACTBE(IS)-IACCOF)/86400.D0
            DJACEN = 2448044.379733750D0+(IACTEN(IS)+IACCOF)/86400.D0
C
C  Check whether the observation in in an accepted time
C
            DO WHILE ((IS .LE. ISTOT) .AND. (DJUL .GT. DJACEN))
		IS = IS + 1
C
C  Convert the S/C clock to Julian date
C
                DJACBE = 2448044.379733750D0 + (IACTBE(IS)-IACCOF)
     +			/86400.D0
                DJACEN = 2448044.379733750D0 + (IACTEN(IS)+IACCOF)
     +			/86400.D0
            ENDDO
            IF((DJUL .GT. DJACBE) .AND. (IS .LE. ISTOT)) THEN
C
C  We've got a good time, so process
C  Set the hour angle
C
C  changed HOURANGLE  to HOUR in the following line - Srilal 06/96
                IF(ISTYLE .EQ. 3) THEN
                    CALL FTGCFE(LUNO,15,IO,1,1,HOUR,FLAGVALO,
     +                  ANYFO,STATUS)
                    CALL FTGCFE(LUNO,3,IO,1,1,X,FLAGVALO,
     +                  ANYFO,STATUS)
                    CALL FTGCFE(LUNO,4,IO,1,1,Y,FLAGVALO,
     +                  ANYFO,STATUS)
                    CALL FTGCFE(LUNO,5,IO,1,1,Z,FLAGVALO,
     +                  ANYFO,STATUS)
                    IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                        BADSTA = .TRUE.
        context='Unable to read FITS file data (Position 6):'
     &	    //INANC
        call fcerr(context)
	goto 999
                    ENDIF
                ELSE
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        CALL FTGCFE(LUNO,15,IO,1,1,HOURANGLE,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,3,IO,1,1,XSUN,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,4,IO,1,1,YSUN,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,5,IO,1,1,ZSUN,FLAGVALO,
     +                      ANYFO,STATUS)
                    ELSE
                        CALL FTGCFE(LUNO,17,IO,1,1,HOURANGLE,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,5,IO,1,1,XSUN,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,6,IO,1,1,YSUN,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,7,IO,1,1,ZSUN,FLAGVALO,
     +                      ANYFO,STATUS)
                    ENDIF
                    IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                        BADSTA = .TRUE.
            context='Unable to read FITS file data (Position 7):'
     &	    //INORB
            call fcerr(context)
            go to 999
                    ENDIF
                    HOUR = HOURANGLE/36000.
                    X = XSUN/1.E8
                    Y = YSUN/1.E8
                    Z = ZSUN/1.E8
                ENDIF
C
C  Set the solar coords
C
                R = SQRT(X*X + Y*Y + Z*Z)

C (g77/linux) ASIND and ATAN2D nonportable
C		DECSUN = ASIND(Z/R)
C		RASUN = ATAN2D(Y,X) + HOUR

                DECSUN = (180./3.141592654) * ASIN(Z/R) 
                RASUN = ((180./3.141592654) * ATAN2(Y,X)) + HOUR

                IF(RASUN .LT. 0.) RASUN = RASUN + 360.
                IF(RASUN .GE. 360.) RASUN = RASUN - 360.
C
C  Set the altitude
C
                IF(ISTYLE .EQ. 3) THEN
                    CALL FTGCFE(LUNO,14,IO,1,1,ALTITUDE,FLAGVALO,
     +                      ANYFO,STATUS)
                    CALL FTGCFE(LUNO,12,IO,1,1,RASAT,FLAGVALO,
     +                      ANYFO,STATUS)
                    CALL FTGCFE(LUNO,13,IO,1,1,DECSAT,FLAGVALO,
     +                      ANYFO,STATUS)
                    IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                        BADSTA = .TRUE.
            context='Unable to read FITS file data (Position 8):'
     &	    //INANC
        call fcerr(context)
	goto 999
                    ENDIF
                ELSE
                    IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                        CALL FTGCFE(LUNO,14,IO,1,1,ALTITUDE,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,12,IO,1,1,LONGITUDE,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,13,IO,1,1,LATITUDE,FLAGVALO,
     +                      ANYFO,STATUS)
                    ELSE
                        CALL FTGCFE(LUNO,16,IO,1,1,ALTITUDE,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,14,IO,1,1,LONGITUDE,FLAGVALO,
     +                      ANYFO,STATUS)
                        CALL FTGCFE(LUNO,15,IO,1,1,LATITUDE,FLAGVALO,
     +                      ANYFO,STATUS)
                    ENDIF
                    IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                        BADSTA = .TRUE.
            context='Unable to read FITS file data (Position 9):'
     &	    //INORB
        call fcerr(context)
	goto 999
                    ENDIF
                    DECSAT = LATITUDE/36000.
                    RASAT = LONGITUDE/36000.
                ENDIF
C
C  Set the satellite position
C
                ALT = ALTITUDE/1000.
                RASAT = RASAT + HOUR
                IF(RASAT .LT. 0.) RASAT = RASAT + 360.
                IF(RASAT .GE. 360.) RASAT = RASAT - 360.
C
C  Find the look direction.  Search though the ATTITUDE data for
C  a close match.
C  DJULAT = Julian date of observation step
C  RALOOK = right ascension of look direction in degrees
C  DECLOOK = declination of look direction in degrees
C
   20           CONTINUE
                DO WHILE ((IA .LT. NROWSA) .AND. (DJUL .GT. DJULAT))
                    IA = IA + 1
                    IF(ISTYLE .EQ. 3) THEN
                        CALL FTGCFJ(LUNA,2,IA,1,1,DATE,FLAGVALA,
     +                          ANYFA,STATUS)
                        CALL FTGCFD(LUNA,3,IA,1,1,SECD,FLAGVALA,
     +                          ANYFA,STATUS)
                        DJULAT = 2400000.5D0 + DATE + SECD
                        CALL FTGCFE(LUNA,9,IA,1,1,RALOOK,FLAGVALA,
     +                          ANYFA,STATUS)
                        CALL FTGCFE(LUNA,10,IA,1,1,DECLOOK,FLAGVALA,
     +                          ANYFA,STATUS)
                        IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                            BADSTA = .TRUE.
            context='Unable to read FITS file data (Position 10):'
     &	    //INANC
        call fcerr(context)
	goto 999
                        ENDIF
                    ELSE
                        IF((ISTYLE .EQ. 1) .OR. (ISTYLE .EQ. 4)) THEN
                            CALL FTGCFD(LUNA,1,IA,1,1,DSCS,FLAGVALA,
     +                              ANYFA,STATUS)
                            CALL FTGCFJ(LUNA,7,IA,1,1,RA,FLAGVALA,
     +                              ANYFA,STATUS)
                            CALL FTGCFJ(LUNA,8,IA,1,1,DEC,FLAGVALA,
     +                              ANYFA,STATUS)
                        ELSE
                            CALL FTGCFJ(LUNA,4,IA,1,1,IT1,FLAGVALA,
     +                              ANYFA,STATUS)
                            CALL FTGCFI(LUNA,5,IA,1,1,IT2,FLAGVALA,
     +                              ANYFA,STATUS)
                            DSCS = IT1 + IT2/64.D0
                            CALL FTGCFJ(LUNA,6,IA,1,1,RA,FLAGVALA,
     +                              ANYFA,STATUS)
                            CALL FTGCFJ(LUNA,8,IA,1,1,DEC,FLAGVALA,
     +                              ANYFA,STATUS)
                        ENDIF
                        IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                            BADSTA = .TRUE.
            context='Unable to read FITS file data (Position 11):'
     &	    //INATT
        call fcerr(context)
	goto 999
                        ENDIF
                        DJULAT = 2448044.379733750 + DSCS/86400.D0
C
C  Convert look direction to degrees
C
                        RALOOK = RA/7200.
                        DECLOOK = DEC/7200.
                    ENDIF
                ENDDO
                IF((ISTYLE .EQ. 2) .AND. (IA .EQ. NROWSA)) THEN
C
C  Skip to the next HDU
C
                    CALL FTMRHD (LUNA,1,HDUTYPEA,STATUS)
                    IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                        BADSTA = .TRUE.
         context='Unable to move to the FITS extension (Position 12):'
     &	    //INATT
        call fcecho(context)
	context = ' Not usually a fatal error: '//INATT
        call fcecho(context)
                    ENDIF
                    IF (STATUS .EQ. 0) THEN
                        IA = 0
C
C  Read the FITS file header information
C
                        CALL FTGHBN (LUNA,10,NROWSA,TFIELDSA,TTYPEA,
     +                          TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
                        IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                            BADSTA = .TRUE.
            context = 'Unable to read the FITS  header information:'
     &	    //INATT
            		call fcerr(context)
            		go to 999
                        ENDIF
                        GO TO 20
                    ENDIF
                ENDIF
C
C  Convert to radians
C
                DECLO = RAD*DECLOOK
                DECSA = RAD*DECSAT
                DECSU = RAD*DECSUN
                RALO = RAD*RALOOK
                RASA = RAD*RASAT
                RASU = RAD*RASUN
C
C  Now find the live time.  Read in the event rate data.
C
C  ILIVEN = time in  S/C seconds
C  IAEXE = AEXE count rate
C  IAXE = AXE count rate
C  IA1LL = A1LL count rate
C  MV = MV count rate
C
   30           CONTINUE
                DO WHILE ((DJUL .GT. DJLIEN) .AND. (IE .LT. NROWSE))
                    IE = IE + 1
                    CALL FTGCFJ(LUNE,1,IE,1,1,ILIVEN,
     +                          FLAGVALE,ANYFE,STATUS)
                    IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                        BADSTA = .TRUE.
        context='Unable to read FITS file data (Position 13):'
     &	//INANC
        call fcerr(context)
	goto 999
                    ENDIF
C
C  Convert S/C seconds to Julian date
C
                    DJLIEN = 2448044.379733750D0 + (ILIVEN-1)/86400.D0
                ENDDO
                IF((ISTYLE .EQ. 2) .AND. (IE .EQ. NROWSE)) THEN
C
C  Skip the first HDU
C
                    CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
                    IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                        BADSTA = .TRUE.
        context='Unable to move to the FITS extension (Position 14):'
     &	    //INEVE
                    ENDIF
                    IF (STATUS .EQ. 0) THEN
                        IE = 0
C
C  Read the FITS file header information
C
                        CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                          TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
                        IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                            BADSTA = .TRUE.
            context = 'Unable to read the FITS header information:'
     &	    //INEVE
            		call fcerr(context)
            		go to 999
                        ENDIF
                        GO TO 30
                    ENDIF
                ENDIF
                IF(IE .EQ. 0) IE = 1
                CALL FTGCFJ(LUNE,2,IE,1,1,MV,FLAGVALE,ANYFE,STATUS)
                CALL FTGCFJ(LUNE,3,IE,1,1,IAEXE,FLAGVALE,ANYFE,STATUS)
                CALL FTGCFJ(LUNE,4,IE,1,1,IA1LL,FLAGVALE,ANYFE,STATUS)
                CALL FTGCFJ(LUNE,7,IE,1,1,IAXE,FLAGVALE,ANYFE,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
         context = 'Unable to read the FITS file data (Position 15):'
     &	 //INANC
            		call fcerr(context)
            		go to 999
                ENDIF
                RMV = MV
                A1LL = IA1LL
                AEXE = IAEXE
                AXE = IAXE
C
C  Find the live time
C
                CALL LIVTIM(A1LL,DEADTP,AXE,AEXE,FLIVE1,
     +                          FLIVE2,FLIVE,IERR)
                FLIVE = FLIVE*(1. - 0.0001*AEXE)
C
C  Call the driving routine that does all the work
C
                CALL SSX (infile, DJUL,RALO,DECLO,RASA,DECSA,ALT,
     +      RASU,DECSU,SPECIN,SPCOUT,DENSIT,IFLAG,IERR, dirstr)
C
C  Write out the scattered spectrum if desired
C
                IF((ISEL .EQ. 1) .AND. (IERR .EQ. 0)) THEN
                    WRITE(83) SPCOUT
                    CLOSE(83)
                    ISEL = 0
                ENDIF
C
                IF(IERR .LE. 1) THEN
                    N = N + 1
                    DATREF = SNGL(DJUL - 2448000.5D0)
C
C  Call the routine which folds the scattered spectrum through the
C  counter response matrix and efficiencies to get the count rates
C  in the seven bands.
C
                    CALL BAN(729,SPCOUT,BAND,DRM)
C
C  Scale the count rates by the livetime
C
                    DO K=1,8
                        BAND(K) = FLIVE*BAND(K)
                    ENDDO
C
C  Write out the results in various formats
C
C  Modified Julian date
C
                    write(context,2020) DATREF, BAND
 2020               FORMAT(1H ,F10.5,5F8.3,2F8.4,F8.3)
                    call fcecho(context)
C
C  S/C seconds
C
                    DT = (DJUL - 2448044.379733750D0)*86400.D0
                    TIME = DT
                    WRITE(UNIT1,2025) TIME, BAND
 2025               FORMAT(1H ,I15,5F8.3,2F8.4,F8.3)
C
C  Write out the look direction line-of-sight densities
C
                    WRITE(UNIT2,2030) TIME, DENSIT
 2030               FORMAT(1H ,I15,6(1PE10.3))
                ENDIF
            ENDIF
        ENDDO
C
C  For US data, see if there is another OBI
C
        IF(ISTYLE .EQ. 2) THEN
C
C  Skip the first HDU
C
            CALL FTMRHD (LUNO,1,HDUTYPEO,STATUS)
            IF (STATUS .EQ. 0) THEN
                IO = 0
C
C  Read the FITS file header information
C
                CALL FTGHBN (LUNO,10,NROWSO,TFIELDSO,TTYPEO,
     +                  TFORMO,TUNITO,EXTNAMEO,VARIDATO,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
         context = 'Unable to read the FITS file header:'
     &	  //INORB
            		call fcerr(context)
            		go to 999
                ENDIF
                GO TO 10
            ENDIF
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
C      gao
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C
C NOTES:
C       gao uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C     call gao(OBS, ISTYLE, IFLAG, TLOG1, SCALE1,
C    & TLOG2, SCALE2, CALDBDIR, STATUS)
C
C ARGUMENTS:
C      OBS     - Observation control (prefix of the data files)
C      ISTYLE  - 1 for German data, 2 for US data and 3 for RDF data
C      IFLAG   - Program control:  IFLAG=1 short, IFLAG=2 long
C      TLOG1, SCALE1 -  First temperature in log10 and scale factor'
C      TLOG2, SCALE2 -  Second temperature in log10 and scale factor'
C      CALDBDIR - Directory structure for fixed inputs
C
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst(i,r) - get parameter value
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*****************************************************************************

      subroutine gao(OBS, ISTYLE, IFLAG, TLOG1, SCALE1,
     & TLOG2, SCALE2, CALDBDIR, STATUS)
      character(8) OBSSTR
      character(1) OBS(8)
      character(80) context
      character(80) caldbdir
      integer istyle, iflag, status
      real*4  tlog1, scale1, tlog2, scale2

C  initialize variables
      status = 0

C  get the contents of the OBS string
      call uclgst('obs',obsstr,status)
	    if (status .ne. 0) then
      context = 'could not get OBS parameter'
      call fcerr(context)
       goto 999
      endif
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

C  get the value of the IFLAG parameter
      call uclgsi('iflag',iflag,status)
C    IFLAG = 1 or IFLAG = 2 only are accepted
      if (iflag .ne. 1 .and. iflag .ne. 2) status =1
      if (status .ne. 0) then
      context = 'could not get IFLAG parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the TLOG1 parameter
      call uclgsr('tlog1',tlog1,status)
	    if (status .ne. 0) then
      context = 'could not get TLOG1 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the SCALE1 parameter
      call uclgsr('scale1',scale1,status)
	    if (status .ne. 0) then
      context = 'could not get SCALE1 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the TLOG2 parameter
      call uclgsr('tlog2',tlog2,status)
	    if (status .ne. 0) then
      context = 'could not get TLOG2 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the SCALE2 parameter
      call uclgsr('scale2',scale2,status)
	    if (status .ne. 0) then
      context = 'could not get SCALE2 parameter'
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
