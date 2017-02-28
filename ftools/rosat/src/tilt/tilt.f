
C*********************************************************************
C SELECTOR TASK:
C      			tilt
C
C FILE:
C      tilt.f
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
C 1.2   Changed all the filenames from uppercase to lowercase.
C
C James Peachey, HEASARC/GSFC/NASA, Hughes STX / 19-SEP-1997
C 1.1   Replaced included file CCONST.INC and DATA statements with
C       new included file ROSCONST.INC. Files CPOS.CMN, CDENS.CMN and
C       ROSCONST.INC are now located in roslib/include. Moved subroutines
C       access_caldb, readfits, read_sgdat, and rsp_read to roslib/src/gen
C 
C     
C
C NOTES:
C   
C
C USAGE:
C      HOST: call tilt
C      IRAF: task tilt
C
C ARGUMENTS:
C      none
C
C
C CALLED ROUTINES:
C
C      subroutine gtilt - gets parameters from parameter file
C      subroutine access_caldb - gets the caldb files using gtcalf
C      subroutine readfits - reads a column of data from a FITS file
C      subroutine rsp_read - reads a response matrix
C      subroutine read_sgdat - reads data from caldb FITS file for SG.DAT
C      subroutine ssx - main calculations are done here (see ancillary.f) 
C      
C
C******************************************************************************



		subroutine tilt


c        PROGRAM TILT
C
CC  Calculation of counts and distribution of SSX
C
C************************ FFORM VERSION 1.0 ************ 25-FEB-88 13:30
C
CA  author : SLS               date: 18-DEC-1987 09:06
CU  update : SLS               date: 25-FEB-1988 13:19
CU  update : SXRB              date: 10-MAY-1992 14:29 
CU  update : SNOWDEN           date: 27-JAN-1994        change to unix
CU  update : SNOWDEN           date: 28-APR-1994        bug fix
C
CT  status : not tested
C
C   general description 
CG  This program uses the results of rate_fit.f to determine the 
CG  scattered solar contamination in the observation and the 
CG  orientation of the gradient.  It evaluates the effective count rate 
CG  both along the line of sight and one degree closer to the horizon.  
CG  This allows the determination of the gradient of the model counts 
CG  across the observation and the rotation angle.  The output of tilt
CG  is cast using the program cast_ssx.f
CG
CG  This program organizes the calculation of atmospheric column
CG  densities along the line of sight of the observation and the
CG  intensity of scattered solar X-rays from those column densities.
CG  Due to certain approximations that have been made (the assumption
CG  that the atmosphere is optically thin) this program is only good
CG  for zenith angles less than >100 degrees when run in the simple
CG  mode.  The column densities calculated for O2, N2, O, and A are 
CG  good to four significant digits while He is between 80 and 90% of 
CG  the actual value and H is somewhere about 50%.  This is due to 
CG  limiting the integral to 2000 km.  The column densities of 
CG  both He and H are small enough to be ignored for the calculation 
CG  of scattered solar X-ray intensity.  At the altitudes which ROSAT 
CG  will fly oxygen dominates all of the scattering.
CG  The main part of the program is the same as ao.f.
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
CG  RDF data
CG  *.anc         xxx_anc.fits
CG
CG  Other required input
CG  valid_times.dat           an ascii list of accepted times
CG                            n  begin end     in S/C seconds
CG                            this file should be identical as that
CG                            used for cast_data.f, cast_exp.f, etc.
CG  DRM.DAT                   detector response matrix
CG  WINDOW.DAT                window transmission
CG  EFF_AREA.DAT              on-axis effective areas
CG  GAS_EFF.DAT               gas stopping efficiency
CG
CG  Finally, tilt requires the fitted parameters from rate_fit.f as input
CG
C
C   call_var.          type I/O description 
CP 
C
C   include_block name          description 
CI  CDATA.INC                   contains data values
CI  CDENS.CMN                   contains atmospheric density data
CI  CONST.CMN                   contains useful constants
CI  CPOS.CMN                    contains position information
CI  CSUNAC.CMN                  contains solar and geophysical params
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
C
C***********************************************************************
C
        IMPLICIT NONE
C
C  Include common blocks
C
        INCLUDE '../../../roslib/include/ROSCONST.INC'
        INCLUDE '../../../roslib/include/CPOS.CMN'
        INCLUDE '../../../roslib/include/CDENS.CMN'
        INCLUDE 'CSUNAC.CMN'

C JP 19-SEP-1997: deleted DATA statements here
C
C  Delete the following line when compiling under VMS.
C        REAL*4 ALT, CTRRSP(729), DRM(256,729)
C
        INTEGER*2 IT2
        INTEGER*4 BITPIX, BLOCK, BLOCKA, BLOCKE, BLOCKO, DATE, DEC, 
     +      GCOUNT, HDUTYPEE, HDUTYPEA, HDUTYPEO, I, IA, IA1LL, 
     +      IACTBE(1000), IACTEN(1000), IAEXE, IAXE, IE, IERR, IFCOM, 
     +      IFLAG, IH, II, ILIVEN, IM, INCOM, IO, IOS, IS, ISTOT, 
     +      ISTYLE, IT1, ITEMP, LUN, LUNA, LUNE, LUNO, N, NAXIS, 
     +      NAXES(10), NN, NROWSA, NROWSE, NROWSO, NUM, PCOUNT, 
     +      RA, ROWLENA, ROWLENE, ROWLENO, SEC1, SEC2, STATUS, 
     +      TBCOLA(20), TBCOLE(35),TBCOLO(23), TFIELDSA, TFIELDSE, 
     +      TFIELDSO, VARIDATA, VARIDATE, VARIDATO, ivalid
C
        REAL*4 A, A1LL, AEXE, ALTITUDE, AREA(729), AXE, 
     +      BAND(8), BAND1(8), BAND2(8), 
     +      DATREF, DEADTP, DECLOOK, DECSAT, DECSUN, 
     +      DENSIT(6), FLIVE1, FLIVE2, FLIVE, GAS(729), 
     +      HOUR, HOURANGLE, IS01, IS02, IS04, 
     +      IS11, IS12, IS14, LATITUDE, 
     +      LONGITUDE, R, RALOOK, RASAT, RASUN, SCALE1, SCALE2, 
     +      SC1, SC2, SC3, SC4, 
     +      SPECIN(729), SPECIN1(729), SPECIN2(729), 
     +      SPCOUT(729), T0, T1, T2, T3, T4, T5, T6, T7, 
     +      TIME, TLOG1, TLOG2, TOTCTS, 
     +      WINDOW(729), WT1, WT2, WT3, WT4, WT5, WT6, WT7, 
     +      X, XSUN, Y, YSUN, Z, ZSUN
C
        REAL*8 B1, B11, B2, B21, B3, B31, B4, B41, B5, B51, B6, B61, 
     +      B7, B71, BB1, BB11, BB2, BB21, BB3, BB31, BB4, BB41, 
     +      BB5, BB51, BB6, BB61, BB7, BB71, DANG, DB1, DB2, DB3, DB4, 
     +      DB5, DB6, DB7, DE, DECE, DECLON, DELRA, DELDEC, 
     +      DELINC, DIF(3), DJACBE, DJACEN, DJLIEN, DJUL, DJULAT, 
     +      DSCS, DT, EXP, EXP1, FRAC, LO(3), RAE, RALON,  SA(3), 
     +      SCALE, SCSO, SEC, SECD, TC1, TC2, TC3, TC4, TC5, TC6, TC7, 
     +      THETA, TS1, TS2, TS3, TS4, TS5, TS6, TS7
C
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
        character(8) TTYPEA(20), TTYPEE(35), TTYPEO(23), TFORMA(20),
     +      TFORME(35), TFORMO(23), TUNITA(20), TUNITE(35), TUNITO(23)
        character(20) EXTNAMEA, EXTNAMEE, EXTNAMEO
        character(80) INANC, INATT, INEVE, INFILE, INORB
	character(80) calfile, valfile, context, dirstr
	integer chatter
        LOGICAL ANYF, ANYFA, ANYFE, ANYFO, BADSTA, EXTEND, 
     +      FLAGVALA, FLAGVALE, FLAGVALO, LCHECK, SIMPLE
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

        DATA DEADTP /234./
        DATA NUM /729/
C
C  This common block passes the values of the arrays read by read_sgdat
C  routine to the GEOSL1 subroutine in ancillary.f

        COMMON /GEOSL1/ IDATE, JULFTMP, F107, F107AV,
     &        KP1, KP2, KP3, KP4, KP5, KP6, KP7, KP8


        DATA DIRSTR /'/ftoolsdev/srilal/caldb/'/
c       DATA DIRSTR /'$DISK2:[SNOWDEN.CODE.ESAS.INPUT]'/
        DATA SCSO /0.D0/
C
C    LENACT is used to correctly concatenate a filename to 'dirstr'
      integer lenact
      external lenact
      character(40) taskname
      common /task/ taskname
        taskname = 'tilt v1.2'
	call ftcmsg

C Initialize:
	ISTYLE = 0
	STATUS = 0
	DIRSTR = ' '
	chatter = 9
	ifcom = 0
        nn = 0
        b11 =0.0
        b21 =0.0
        b3  =0.0
        b31 =0.0
        b41 =0.0
        b51 =0.0
        b61 =0.0
        b71 =0.0
        bb1 =0.0
        bb11=0.0
	bb2 =0.0
	bb21=0.0
        bb4 =0.0
        bb41=0.0
        bb5 =0.0
        bb51=0.0
        bb6 =0.0
        bb61=0.0
        bb7 =0.0
        bb71=0.0
	djlien =0.d0
	djulat =0.d0
        exp1 =0.
        tc1 =0.
        tc2 =0.
        tc3 =0.
        tc4 =0.
        tc5 =0.
        tc6 =0.
        tc7 =0.
        ts1 =0.
        ts2 =0.
        ts3 =0.
        ts4 =0.
        ts5 =0.
        ts6 =0.
        ts7 =0.
	badsta =.false.
        

C
C  get parameters from parameter file
C
       call gtilt(OBS, ISTYLE, IFLAG,
     & T0, IS01, IS11, IS02, IS12, IS04, IS14,
     & TLOG1, SCALE1, TLOG2, SCALE2, DIRSTR, STATUS)
         if (status .ne. 0) then
	 context = 'tilt.f: Error in obtaining parameters'
         call fcerr(context)
         goto 999
         endif

C  Select on WG or US data
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
C  Make sure that we have an orbit file name
C
        IF((INORB .EQ. '    ') .AND. (INANC .EQ. '    ')) THEN
            call fcecho ('Enter ORBIT table name')
            IF(ISTYLE .EQ. 3) THEN
                READ 1000, INANC
            ELSE
                READ 1000, INORB
		context = INORB
		call fcecho(context)
            ENDIF
 1000       FORMAT(A80)
        ENDIF
C
c       LUNO = 91
         call cgetlun(LUNO)

        IF(ISTYLE .EQ. 3) THEN
C
C  Open the RDF extension for orbit data
C
		context = INANC
		call fcecho(context)
            CALL FTOPEN(LUNO,INANC,0,BLOCKO,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context = 'Unable to open infile (Orbit extension):'
     &      //INANC
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
	    goto 999
            ENDIF
C
C  Read the FITS file header information
C
C
            CALL FTGHBN (LUNO,23,NROWSO,TFIELDSO,TTYPEO,
     +          TFORMO,TUNITO,EXTNAMEO,VARIDATO,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
	    context = 'Unable to read the FITS header '//INANC
	    call fcerr(context)
	    go to 999
            ENDIF
C
C  Open the German or US orbit file
C
        ELSE
		context = INORB
		call fcecho(context)
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
     &      //INORB
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
     &      //INORB
	    call fcerr(context)
	    go to 999
                ENDIF
            ELSE
                CALL FTGHBN (LUNO,23,NROWSO,TFIELDSO,TTYPEO,
     +              TFORMO,TUNITO,EXTNAMEO,VARIDATO,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
		   context = 'Unable to read the FITS file header information:'
     &      //INORB
	           call fcerr(context)
	           go to 999
                ENDIF
            ENDIF
        ENDIF
C
C  Make sure that we have an attitude file name
C
        IF((INORB .EQ. '    ') .AND. (INANC .EQ. '    ')) THEN
            call fcecho ('Enter ATTITUDE table name')
            IF(ISTYLE .EQ. 3) THEN
                READ 1000, INANC
            ELSE
                READ 1000, INATT
            ENDIF
        ENDIF
C
c       LUNA = 92
         call cgetlun(LUNA)

        IF(ISTYLE .EQ. 3) THEN
C
C  Open the RDF extension for the attitude data
C
	context = INANC
	call fcecho(context)
            CALL FTOPEN(LUNA,INANC,0,BLOCKA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
        	context = 'Unable to open infile (Attitude extension):'
     &      //INANC
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
     &         //INANC
	    call fcerr(context)
	    go to 999
            ENDIF
C
C  Read the FITS file header information
C
            CALL FTGHBN (LUNA,23,NROWSA,TFIELDSA,TTYPEA,
     +          TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
	    context = 'Unable to read the FITS header:Attitude extension'
     &      //INANC
	    call fcerr(context)
	    go to 999
            ENDIF
C
C  Open the German or US attitude file
C
        ELSE
	context = INATT
	call fcecho(context)
            CALL FTOPEN(LUNA,INATT,0,BLOCKA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
	    context = 'Unable to open infile :'//INATT
	    call fcerr(context)
	    go to 999
            ENDIF
C
C  Skip the first HDU
C
            CALL FTMRHD (LUNA,1,HDUTYPEA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
	    context='Unable to move to the FITS extension (Position 1):'
     &      //INATT
	    call fcerr(context)
	    go to 999
            ENDIF
C
C  Read the FITS file header information
C
            IF(ISTYLE .EQ. 1) THEN
                CALL FTGHTB (LUNA,23,ROWLENA,NROWSA,TFIELDSA,TTYPEA,
     +              TBCOLA,TFORMA,TUNITA,EXTNAMEA,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
		context = 'Unable to read the FITS file header information:'
     &      //INATT
	    call fcerr(context)
	    go to 999
                ENDIF
            ELSE
                CALL FTGHBN (LUNA,23,NROWSA,TFIELDSA,TTYPEA,
     +              TFORMA,TUNITA,EXTNAMEA,VARIDATA,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
		context = 'Unable to read the FITS file header information:'
     &      //INATT
	         call fcerr(context)
	         go to 999
                ENDIF
            ENDIF
        ENDIF
C
C  Make sure that we have an eventrate file name
C
        IF((INEVE .EQ. '    ') .AND. (INANC .EQ. '    ')) THEN
            call fcecho ('Enter ATTITUDE table name')
            IF(ISTYLE .EQ. 3) THEN
                READ 1000, INANC
            ELSE
                READ 1000, INEVE
            ENDIF
        ENDIF
C
c       LUNE = 93
         call cgetlun(LUNE)

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
     &      //INANC
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
     &      //INANC
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
	    context = 'Unable to read the FITS file header '//INANC
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
	    context = 'Unable to open infile :' //INEVE
	    call fcerr(context)
	    go to 999
            ENDIF
            CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                BADSTA = .TRUE.
	    context='Unable to move to the FITS extension (Position 1):'
     &      //INEVE
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
		context = 'Unable to read the FITS file header'//INEVE
	        call fcerr(context)
	        go to 999
                ENDIF
            ELSE
                CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                  TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
		context = 'Unable to read the FITS file header '//INEVE
	        call fcerr(context)
	        go to 999
                ENDIF
            ENDIF
        ENDIF

C
C  Open accepted time input file
C
        VALFILE = 'valid_times.dat'
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
            READ(IVALID,*,IOSTAT=IOS)ITEMP, IACTBE(IS), IACTEN(IS)
          IF(IOS .EQ. 0) then
	    write(context,'(''BEGIN = '',i10,'' END = '',i10)')
     &      IACTBE(IS), IACTEN(IS)
	    call fcecho(context)
	  ENDIF
         ENDDO
	CLOSE(IVALID)

        DJACBE = 2448044.379733750 + (IACTBE(1))/86400.D0
        DJACEN = 2448044.379733750 + (IACTEN(1))/86400.D0
        IACTBE(IS) = 149999999
        IACTEN(IS) = 150000000
        ISTOT = IS - 1
        IS = 1
	call fcecho(' ')
	write(context,'(''TOTAL NUMBER OF INTERVALS IS:'',i4)') ISTOT
	call fcecho(context)
	call fcecho(' ')
C
C  Read in the on-axis effective area
C
C   Access CALDB to get the name of the fixed input file for  EFFAREA:
C
C Note: 'eff_area' fits file has INSTRUME='XRT'
C       This adjustment is made inthe access_caldb routine
C
        call access_caldb(INANC, 'EFFAREA', calfile, status)
	     if (status .ne. 0) then
	context = 'tilt.f: Error in accessing CALDB file for EFFAREA'
	call fcerr(context)
	goto 999
        endif
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
      context = 'tilt.f: Error in accessing CALDB file for WTRANS'
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
       context = 'tilt.f: Error in accessing CALDB file for DET_EFF'
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
      context='tilt.f: Error in accessing CALDB file for ENERGY_GRID'
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
       context='tilt.f: Error in accessing file SOLAR_GEOPHYS'
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
       context = 'tilt.f: Error in accessing CALDB file for RSP MATRIX'
       call fcerr(context)
       goto 999
       endif
C

c       calFILE=DIRSTR(1:lenact(dirstr))//'DRM.FITS'
c     calFILE='/caldb/data/rosat/pspc/cpf/matrices/pspcb_gain2_256.rmf'

c       LUN = 95
	call cgetlun(LUN)
        CALL FTOPEN(LUN,CALFILE,0,BLOCK,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN 
	BADSTA = .TRUE.
	 context = 'Unable to open infile :'
     &      //CALFILE
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
     &      //CALFILE
            call fcerr(context)
            go to 999
        ENDIF

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
c

        CALL FTG2DE (LUN,0,0,NAXES(1),NAXES(1),NAXES(2),DRM,
     +          ANYF,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
	context = 'Unable to read the Data file :'
     &      //CALFILE
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
C  Put it all together
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

        CALL GETRAY_N(inanc, TLOG1,SPECIN1, dirstr)
        DO I=1,729
            SPECIN(I) = 1.E23*SCALE1*SPECIN1(I)
        ENDDO
C
C  Enter a temperature (log10 T) for the solar spectrum, units of
C  photons/cm**2/s/bin.  Combine with the first spectrum.
C

        IF((TLOG2 .GT. 0.) .AND. (SCALE2 .GT. 0.)) THEN

            CALL GETRAY_N(inanc, TLOG2,SPECIN2, dirstr)
            DO I=1,729
                SPECIN(I) = SPECIN(I) + 1.E23*SCALE2*SPECIN2(I)
            ENDDO
        ENDIF
C
C  Set calculation parameters
C
   10   CONTINUE
        IOS = 0
        DO IO=1,NROWSO
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
            IF(ISTYLE .EQ. 3) THEN
                CALL FTGCFJ(LUNO,1,IO,1,1,DATE,FLAGVALO,ANYFO,STATUS)
                CALL FTGCFD(LUNO,2,IO,1,1,SECD,FLAGVALO,ANYFO,STATUS)
                DJUL = 2400000.5 + DATE + SECD
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
		context='Unable to read FITS file data (Position 1):'
     &	    //INANC    
            call fcerr(context)
            go to 999
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
		context='Unable to read FITS file data (Position 2):'
     &		//INORB
            call fcerr(context)
            go to 999
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
            IF (DJUL .LT. SCSO) THEN
                IS = 1
                DJACBE = 2448044.379733750D0 + (IACTBE(IS))/86400.D0
                DJACEN = 2448044.379733750D0 + (IACTEN(IS))/86400.D0
            ENDIF
            SCSO = DJUL
C
C  Check for an accepted time
C
            DO WHILE ((IS .LE. ISTOT) .AND. 
     +                  (DJUL .GT. DJACEN))
                IS = IS + 1
                DJACBE = 2448044.379733750D0 + (IACTBE(IS))/86400.D0
                DJACEN = 2448044.379733750D0 + (IACTEN(IS))/86400.D0
            ENDDO
            IF((DJUL .GT. DJACBE) .AND. 
     +              (IS .LE. ISTOT)) THEN
C
C  We've got a good time, so process
C  Set the hour angle
C
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
	    context='Unable to read FITS file data (Position 3):'
     &      //INANC
            call fcerr(context)
            go to 999
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
	    context='Unable to read FITS file data (Position 4):'
     &      //INORB
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

c               DECSUN = ASIND(Z/R)
c               RASUN = ATAN2D(Y,X) + HOUR
c redefined to prevent errors on Linux
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
	    context='Unable to read FITS file data (Position 5):'
     &      //INANC
            call fcerr(context)
            go to 999
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
	    context='Unable to read FITS file data (Position 6):'
     &      //INORB
            call fcerr(context)
            go to 999
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
	    context='Unable to read FITS file data (Position 7):'
     &      //INANC
            call fcerr(context)
            go to 999
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
	    context='Unable to read FITS file data (Position 8):'
     &      //INATT
            call fcerr(context)
            go to 999
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
	context='Unable to move to the FITS extension (Position 2):'
     &      //INATT
            call fcerr(context)
            go to 999
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
     &      //INATT
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
C
   30           CONTINUE
                DO WHILE ((DJUL .GT. DJLIEN) .AND. (IE .LT. NROWSE))
                    IE = IE + 1
                    CALL FTGCFJ(LUNE,1,IE,1,1,ILIVEN,
     +                          FLAGVALE,ANYFE,STATUS)
C
C  Convert S/C seconds to Julian date
C
                    DJLIEN = 2448044.379733750D0 + (ILIVEN-1.)/86400.D0
                ENDDO
                IF((ISTYLE .EQ. 2) .AND. (IE .EQ. NROWSE)) THEN
C
C  Skip the first HDU
C
                    CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
                    IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                        BADSTA = .TRUE.
	context='Unable to move to the FITS extension (Position 2):'
     &      //INEVE
            call fcerr(context)
            go to 999
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
		context = 'Unable to read the FITS file header '//INEVE
	        call fcerr(context)
	        go to 999
                        ENDIF
                        GO TO 30
                    ENDIF
                ENDIF
                IF(IE .EQ. 0) IE = 1
                CALL FTGCFJ(LUNE,3,IE,1,1,IAEXE,FLAGVALE,ANYFE,STATUS)
                CALL FTGCFJ(LUNE,4,IE,1,1,IA1LL,FLAGVALE,ANYFE,STATUS)
                CALL FTGCFJ(LUNE,7,IE,1,1,IAXE,FLAGVALE,ANYFE,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT.BADSTA)) THEN
                    BADSTA = .TRUE.
	context='Unable to read data : (Position 10):'//INEVE
            call fcerr(context)
            go to 999
                ENDIF
                A1LL = IA1LL
                AEXE = IAEXE
                AXE = IAXE
C
C  Use the live time SCS for the scale factor
C
                SC1 = IS01 + IS11*(ILIVEN - T0)*1.E-5
                SC2 = IS02 + IS12*(ILIVEN - T0)*1.E-5
                SC4 = IS04 + IS14*(ILIVEN - T0)*1.E-5
                SC3 = (SC2 + SC4)/2.
C
C  Check to see if the interval is completely within the accepted time
C
                IF (DJUL .LT. DJACBE + 6.93D-4) THEN
                    IF (DJUL .LT. DJACEN - 13.86D-4) THEN
                        DELINC = 3.47222D-4 + (DJUL - DJACBE)
                        EXP = 86400.D0*DELINC
                        INCOM = 1
                    ELSEIF (DJUL .LT. DJACEN - 6.94D-4) THEN
                        EXP = 86400.D0*(DJACEN - DJACBE)
                        INCOM = 4
                    ELSE
                        EXP = 86400.D0*(DJACEN - DJACBE)
                        INCOM = 3
                    ENDIF
                ELSEIF (DJUL .GT. DJACEN - 6.7D-4) THEN
                    INCOM = 2
                    DELINC = 3.47222D-4 + (DJACEN - DJUL)
                    EXP =  86400.D0*DELINC
                ELSE
                    EXP = 60.
                    INCOM = 0
                ENDIF
C
C  Find the live time
C
                CALL LIVTIM(A1LL,DEADTP,AXE,AEXE,FLIVE1,
     +                          FLIVE2,FLIVE,IERR)
                FLIVE = FLIVE*(1. - 0.0001*AEXE)
                EXP = EXP*FLIVE
                TIME = TIME + EXP
C
C                PRINT *, DJACBE, DJUL, DJACEN, EXP
C
                CALL SSX (infile, DJUL,RALO,DECLO,RASA,DECSA,ALT,
     +          RASU,DECSU,SPECIN,SPCOUT,DENSIT,IFLAG,IERR,dirstr)
C
                IF(IERR .EQ. 0) THEN
                    N = N + 1
                    DATREF = SNGL(DJUL - 2448000.5D0)
                    CALL BAN(NUM,SPCOUT,BAND,DRM)
                    BAND1(1) = SC1*BAND(1)
                    BAND1(2) = SC2*BAND(2)
                    BAND1(3) = SC3*BAND(3)
                    BAND1(4) = SC4*BAND(4)
                    BAND1(5) = SC4*BAND(5)
                    BAND1(6) = SC4*BAND(6)
                    BAND1(7) = SC4*BAND(7)
                    BAND1(8) = BAND1(1) + BAND1(2) + BAND1(3) + 
     +                  BAND1(4) + BAND1(5) + BAND1(6) + BAND1(7)

		    write(context,2025) DATREF, BAND1
 2025               FORMAT(1H ,F15.6,8F8.3)
		    call fcecho(context)
C
                    RAE = RASA + PI
                    IF(RAE .GT. PI2) RAE = RAE - PI2
                    DECE = -DECSA
C
C  Get the angle between the two directions, look direction and Earth-
C  center direction
C
                    CALL ANGSEP (RALO,DECLO,RAE,DECE,DANG,IERR)
                    SCALE = 0.0174533/DANG
C
C  Calculate the fraction distance along the chord connecting
C  the two directions at the unit sphere
C
                    FRAC = 2.*DCOS((0.5D0-SCALE)*DANG)*
     +                          DSIN(DANG/2.D0)
                    IF(FRAC .NE. 0.) THEN
                        FRAC = DSIN(SCALE*DANG)/FRAC
C
C  Get the cartesian coords of the two directions
C
                        LO(1) = DCOS(RALO)*DCOS(DECLO)
                        LO(2) = DSIN(RALO)*DCOS(DECLO)
                        LO(3) = DSIN(DECLO)
C
                        SA(1) = DCOS(RAE)*DCOS(DECE)
                        SA(2) = DSIN(RAE)*DCOS(DECE)
                        SA(3) = DSIN(DECE)
C
                        DO I=1,3
                            DIF(I) = LO(I) + FRAC*(SA(I) - LO(I))
                        ENDDO
C
C  Get the spherical coords of the new direction
C
                        DE = DSQRT(DIF(1)**2 + DIF(2)**2)
                        RALON = DATAN2(DIF(2),DIF(1))
                        IF(RALON .LT. 0) RALON = RALON + PI2
                        DECLON = DATAN2(DIF(3),DE)
                    ELSE
                        RALON = RALO
                        DECLON = DECLO
                    ENDIF
C
                    DELRA = (RALON - RALO)*DCOS(DECLO)
                    DELDEC = DECLON - DECLO
                    THETA = DATAN2(DELDEC,DELRA)
C
                    CALL SSX (infile,DJUL,RALON,DECLON,RASA,DECSA,ALT,
     +              RASU,DECSU,SPECIN,SPCOUT,DENSIT,IFLAG,IERR,dirstr)
C
                    N = N + 1
                    DATREF = SNGL(DJUL - 2448000.5D0)
                    CALL BAN(NUM,SPCOUT,BAND,DRM)
                    BAND2(1) = SC1*BAND(1)
                    BAND2(2) = SC2*BAND(2)
                    BAND2(3) = SC3*BAND(3)
                    BAND2(4) = SC4*BAND(4)
                    BAND2(5) = SC4*BAND(5)
                    BAND2(6) = SC4*BAND(6)
                    BAND2(7) = SC4*BAND(7)
                    BAND2(8) = BAND2(1) + BAND2(2) + BAND2(3) +
     +                  BAND2(4) + BAND2(5) + BAND2(6) + BAND2(7)
		    write(context,2025) DATREF, BAND2
		    call fcecho(context)
C
C  Time interval is completely contained within accepted time or an
C  interval with only one orbit entry
C
                    IF((INCOM .EQ. 0) .OR. (INCOM .EQ. 3)) THEN
                        IF(IFCOM .EQ. 1) THEN
C
C  Take care of the incomplete first interval
C  First find a reasonable approximation to the proper count rate
C  for the look direction.  This tries to take into account that for
C  an incomplete interval, the value at the orbit time is not the best
C  value to use
C
                            SCALE = 0.5*(1.0 - EXP1/EXP)
                            B1 = (B11 + SCALE*(BAND1(1) - B11))*EXP1
                            B2 = (B21 + SCALE*(BAND1(2) - B21))*EXP1
                            B3 = (B31 + SCALE*(BAND1(3) - B31))*EXP1
                            B4 = (B41 + SCALE*(BAND1(4) - B41))*EXP1
                            B5 = (B51 + SCALE*(BAND1(5) - B51))*EXP1
                            B6 = (B61 + SCALE*(BAND1(6) - B61))*EXP1
                            B7 = (B71 + SCALE*(BAND1(7) - B71))*EXP1
C
C  Next, find a reasonable approximation to the proper count rate
C  for the off-look direction.
C
                            BB1 = (BB11 + SCALE*(BAND1(1) - BB11))*EXP1
                            BB2 = (BB21 + SCALE*(BAND2(2) - BB21))*EXP1
                            BB3 = (BB31 + SCALE*(BAND2(3) - BB31))*EXP1
                            BB4 = (BB41 + SCALE*(BAND2(4) - BB41))*EXP1
                            BB5 = (BB51 + SCALE*(BAND2(5) - BB51))*EXP1
                            BB6 = (BB61 + SCALE*(BAND2(6) - BB61))*EXP1
                            BB7 = (BB71 + SCALE*(BAND2(7) - BB71))*EXP1
C
C  Add to the total number of counts
C
                            TOTCTS = TOTCTS + B1 + B2 + B3 +
     +                              B4 + B5 + B6 + B7
C
C  Add to the total weighting
C
                            WT1 = WT1 + B1
                            WT2 = WT2 + B2
                            WT3 = WT3 + B3
                            WT4 = WT4 + B4
                            WT5 = WT5 + B5
                            WT6 = WT6 + B6
                            WT7 = WT7 + B7
C
C  Find the difference in the number of counts
C
                            DB1 = BB1 - B1
                            DB2 = BB2 - B2
                            DB3 = BB3 - B3
                            DB4 = BB4 - B4
                            DB5 = BB5 - B5
                            DB6 = BB6 - B6
                            DB7 = BB7 - B7
C
C  Add to the weighting for the number of delta counts
C
                            T1 = T1 + DB1
                            T2 = T2 + DB2
                            T3 = T3 + DB3
                            T4 = T4 + DB4
                            T5 = T5 + DB5
                            T6 = T6 + DB6
                            T7 = T7 + DB7
C
C  Add to the weighted rotation sin and cos
C
                            TS1 = TS1 + DB1*DSIN(THETA)
                            TC1 = TC1 + DB1*DCOS(THETA)
                            TS2 = TS2 + DB2*DSIN(THETA)
                            TC2 = TC2 + DB2*DCOS(THETA)
                            TS3 = TS3 + DB3*DSIN(THETA)
                            TC3 = TC3 + DB3*DCOS(THETA)
                            TS4 = TS4 + DB4*DSIN(THETA)
                            TC4 = TC4 + DB4*DCOS(THETA)
                            TS5 = TS5 + DB5*DSIN(THETA)
                            TC5 = TC5 + DB5*DCOS(THETA)
                            TS6 = TS6 + DB6*DSIN(THETA)
                            TC6 = TC6 + DB6*DCOS(THETA)
                            TS7 = TS7 + DB7*DSIN(THETA)
                            TC7 = TC7 + DB7*DCOS(THETA)
c 
  		    write(context,2030) EXP, B1, DB1, B2, DB2, B4, DB4
 		    call fcecho(context)
 		    write(context,2030) EXP, B5, DB5, B6, DB6, B7, DB7
 		    call fcecho(context)
 2030                       FORMAT(7F10.2)
C
                            IFCOM = 0
                        ENDIF
C
C  Now do the present interval
C  Turn B* into the number of counts for the interval for the look
C  direction
C
                        B1 = BAND1(1)*EXP
                        B2 = BAND1(2)*EXP
                        B3 = BAND1(3)*EXP
                        B4 = BAND1(4)*EXP
                        B5 = BAND1(5)*EXP
                        B6 = BAND1(6)*EXP
                        B7 = BAND1(7)*EXP
C
C  Add to the total number of counts
C
                        TOTCTS = TOTCTS + B1 + B2 + B3 +
     +                          B4 + B5 + B6 + B7
C
C  Add to the total weighting
C
                        WT1 = WT1 + B1
                        WT2 = WT2 + B2
                        WT3 = WT3 + B3
                        WT4 = WT4 + B4
                        WT5 = WT5 + B5
                        WT6 = WT6 + B6
                        WT7 = WT7 + B7
C
C  Get the number of counts for the offset position
C
                        BB1 = BAND2(1)*EXP
                        BB2 = BAND2(2)*EXP
                        BB3 = BAND2(3)*EXP
                        BB4 = BAND2(4)*EXP
                        BB5 = BAND2(5)*EXP
                        BB6 = BAND2(6)*EXP
                        BB7 = BAND2(7)*EXP
C
C  Find the difference in the number of counts
C
                        DB1 = BB1 - B1
                        DB2 = BB2 - B2
                        DB3 = BB3 - B3
                        DB4 = BB4 - B4
                        DB5 = BB5 - B5
                        DB6 = BB6 - B6
                        DB7 = BB7 - B7
C
C  Add to the weighting for the number of delta counts
C
                        T1 = T1 + DB1
                        T2 = T2 + DB2
                        T3 = T3 + DB3
                        T4 = T4 + DB4
                        T5 = T5 + DB5
                        T6 = T6 + DB6
                        T7 = T7 + DB7
C
C  Add to the weighted rotation sin and cos
C
                        TS1 = TS1 + DB1*DSIN(THETA)
                        TC1 = TC1 + DB1*DCOS(THETA)
                        TS2 = TS2 + DB2*DSIN(THETA)
                        TC2 = TC2 + DB2*DCOS(THETA)
                        TS3 = TS3 + DB3*DSIN(THETA)
                        TC3 = TC3 + DB3*DCOS(THETA)
                        TS4 = TS4 + DB4*DSIN(THETA)
                        TC4 = TC4 + DB4*DCOS(THETA)
                        TS5 = TS5 + DB5*DSIN(THETA)
                        TC5 = TC5 + DB5*DCOS(THETA)
                        TS6 = TS6 + DB6*DSIN(THETA)
                        TC6 = TC6 + DB6*DCOS(THETA)
                        TS7 = TS7 + DB7*DSIN(THETA)
                        TC7 = TC7 + DB7*DCOS(THETA)
c 
  		    write(context,2030) EXP, B1, DB1, B2, DB2, B4, DB4
 		    call fcecho(context)
 		    write(context,2030) EXP, B5, DB5, B6, DB6, B7, DB7
 		    call fcecho(context)
C
                        EXP1 = EXP
C
C  Time interval is not complete on low time side
C
                    ELSEIF((INCOM .EQ. 1) .OR. (INCOM .EQ. 4)) THEN
                        IFCOM = 1
                        EXP1 = EXP
                        B11 = BAND1(1)
                        B21 = BAND1(2)
                        B31 = BAND1(3)
                        B41 = BAND1(4)
                        B51 = BAND1(5)
                        B61 = BAND1(6)
                        B71 = BAND1(7)
                        BB11 = BAND2(1)
                        BB21 = BAND2(2)
                        BB31 = BAND2(3)
                        BB41 = BAND2(4)
                        BB51 = BAND2(5)
                        BB61 = BAND2(6)
                        BB71 = BAND2(7)
C
C  Time interval is not complete on high time side
C
                    ELSEIF(INCOM .EQ. 2) THEN
                        IF(IFCOM .EQ. 1) THEN
C
C  The previous interval was incomplete on the low side, take the average
C  over the two samples
C
                            B1 = 0.5*(B11 + BAND1(1))*EXP
                            B2 = 0.5*(B21 + BAND1(2))*EXP
                            B3 = 0.5*(B31 + BAND1(3))*EXP
                            B4 = 0.5*(B41 + BAND1(4))*EXP
                            B5 = 0.5*(B51 + BAND1(5))*EXP
                            B6 = 0.5*(B61 + BAND1(6))*EXP
                            B7 = 0.5*(B71 + BAND1(7))*EXP
                            BB1 = 0.5*(BB11 + BAND2(1))*EXP
                            BB2 = 0.5*(BB21 + BAND2(2))*EXP
                            BB3 = 0.5*(BB31 + BAND2(3))*EXP
                            BB4 = 0.5*(BB41 + BAND2(4))*EXP
                            BB5 = 0.5*(BB51 + BAND2(5))*EXP
                            BB6 = 0.5*(BB61 + BAND2(6))*EXP
                            BB7 = 0.5*(BB71 + BAND2(7))*EXP
C
C  Add to the total number of counts
C
                            TOTCTS = TOTCTS + B1 + B2 + B3 +
     +                              B4 + B5 + B6 + B7
C
C  Add to the total weighting
C
                            WT1 = WT1 + B1
                            WT2 = WT2 + B2
                            WT3 = WT3 + B3
                            WT4 = WT4 + B4
                            WT5 = WT5 + B5
                            WT6 = WT6 + B6
                            WT7 = WT7 + B7
C
C  Find the difference in the number of counts
C
                            DB1 = BB1 - B1
                            DB2 = BB2 - B2
                            DB3 = BB3 - B3
                            DB4 = BB4 - B4
                            DB5 = BB5 - B5
                            DB6 = BB6 - B6
                            DB7 = BB7 - B7
C
C  Add to the weighting for the number of delta counts
C
                            T1 = T1 + DB1
                            T2 = T2 + DB2
                            T3 = T3 + DB3
                            T4 = T4 + DB4
                            T5 = T5 + DB5
                            T6 = T6 + DB6
                            T7 = T7 + DB7
C
C  Add to the weighted rotation sin and cos
C
                            TS1 = TS1 + DB1*DSIN(THETA)
                            TC1 = TC1 + DB1*DCOS(THETA)
                            TS2 = TS2 + DB2*DSIN(THETA)
                            TC2 = TC2 + DB2*DCOS(THETA)
                            TS3 = TS3 + DB3*DSIN(THETA)
                            TC3 = TC3 + DB3*DCOS(THETA)
                            TS4 = TS4 + DB4*DSIN(THETA)
                            TC4 = TC4 + DB4*DCOS(THETA)
                            TS5 = TS5 + DB5*DSIN(THETA)
                            TC5 = TC5 + DB5*DCOS(THETA)
                            TS6 = TS6 + DB6*DSIN(THETA)
                            TC6 = TC6 + DB6*DCOS(THETA)
                            TS7 = TS7 + DB7*DSIN(THETA)
                            TC7 = TC7 + DB7*DCOS(THETA)
c
  		    write(context,2030) EXP, B1, DB1, B2, DB2, B4, DB4
 		    call fcecho(context)
 		    write(context,2030) EXP, B5, DB5, B6, DB6, B7, DB7
 		    call fcecho(context)
                            IFCOM = 0
                        ELSE
C
C  Rescale the count rates from the previous complete interval
C
                            B11 = B1/EXP1
                            B21 = B2/EXP1
                            B31 = B3/EXP1
                            B41 = B4/EXP1
                            B51 = B5/EXP1
                            B61 = B6/EXP1
                            B71 = B7/EXP1
                            BB11 = BB1/EXP1
                            BB21 = BB2/EXP1
                            BB31 = BB3/EXP1
                            BB41 = BB4/EXP1
                            BB51 = BB5/EXP1
                            BB61 = BB6/EXP1
                            BB71 = BB7/EXP1
C
C  Take care of the incomplete first interval
C  First find a reasonable approximation to the proper count rate
C  for the look direction.  This tries to take into account that for
C  an incomplete interval, the value at the orbit time is not the best
C  value to use
C
                            SCALE = 0.5*(1.0 + EXP/EXP1)
                            B1 = (B11 + SCALE*(BAND1(1) - B11))*EXP
                            B2 = (B21 + SCALE*(BAND1(2) - B21))*EXP
                            B3 = (B31 + SCALE*(BAND1(3) - B31))*EXP
                            B4 = (B41 + SCALE*(BAND1(4) - B41))*EXP
                            B5 = (B51 + SCALE*(BAND1(5) - B51))*EXP
                            B6 = (B61 + SCALE*(BAND1(6) - B61))*EXP
                            B7 = (B71 + SCALE*(BAND1(7) - B71))*EXP
C
C  Next, find a reasonable approximation to the proper count rate
C  for the off-look direction.
C
                            BB1 = (BB11 + SCALE*(BAND2(1) - BB11))*EXP
                            BB2 = (BB21 + SCALE*(BAND2(2) - BB21))*EXP
                            BB3 = (BB31 + SCALE*(BAND2(3) - BB31))*EXP
                            BB4 = (BB41 + SCALE*(BAND2(4) - BB41))*EXP
                            BB5 = (BB51 + SCALE*(BAND2(5) - BB51))*EXP
                            BB6 = (BB61 + SCALE*(BAND2(6) - BB61))*EXP
                            BB7 = (BB71 + SCALE*(BAND2(7) - BB71))*EXP
C
C  Add to the total number of counts
C
                            TOTCTS = TOTCTS + B1 + B2 + B3 +
     +                              B4 + B5 + B6 + B7
C
C  Add to the total weighting
C
                            WT1 = WT1 + B1
                            WT2 = WT2 + B2
                            WT3 = WT3 + B3
                            WT4 = WT4 + B4
                            WT5 = WT5 + B5
                            WT6 = WT6 + B6
                            WT7 = WT7 + B7
C
C  Find the difference in the number of counts
C
                            DB1 = BB1 - B1
                            DB2 = BB2 - B2
                            DB3 = BB3 - B3
                            DB4 = BB4 - B4
                            DB5 = BB5 - B5
                            DB6 = BB6 - B6
                            DB7 = BB7 - B7
C
C  Add to the weighting for the number of delta counts
C
                            T1 = T1 + DB1
                            T2 = T2 + DB2
                            T3 = T3 + DB3
                            T4 = T4 + DB4
                            T5 = T5 + DB5
                            T6 = T6 + DB6
                            T7 = T7 + DB7
C
C  Add to the weighted rotation sin and cos
C
                            TS1 = TS1 + DB1*DSIN(THETA)
                            TC1 = TC1 + DB1*DCOS(THETA)
                            TS2 = TS2 + DB2*DSIN(THETA)
                            TC2 = TC2 + DB2*DCOS(THETA)
                            TS3 = TS3 + DB3*DSIN(THETA)
                            TC3 = TC3 + DB3*DCOS(THETA)
                            TS4 = TS4 + DB4*DSIN(THETA)
                            TC4 = TC4 + DB4*DCOS(THETA)
                            TS5 = TS5 + DB5*DSIN(THETA)
                            TC5 = TC5 + DB5*DCOS(THETA)
                            TS6 = TS6 + DB6*DSIN(THETA)
                            TC6 = TC6 + DB6*DCOS(THETA)
                            TS7 = TS7 + DB7*DSIN(THETA)
                            TC7 = TC7 + DB7*DCOS(THETA)
c
  		    write(context,2030) EXP, B1, DB1, B2, DB2, B4, DB4
 		    call fcecho(context)
 		    write(context,2030) EXP, B5, DB5, B6, DB6, B7, DB7
 		    call fcecho(context)
                        ENDIF
                    ENDIF
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
		context = 'Unable to read the FITS file header '//INORB
	        call fcerr(context)
	        go to 999
                ENDIF
                GO TO 10
            ENDIF
            STATUS = 0
        ENDIF
C
C  Close the fits files
C
        IF(ISTYLE .EQ. 3) THEN
            CALL FTCLOS(LUNA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context = 'Unable to close the attitide extension '//INANC
	        call fcerr(context)
	        go to 999
            ENDIF
C
            CALL FTCLOS(LUNE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
	 context = 'Unable to close the eventrate extension '//INANC
	        call fcerr(context)
	        go to 999
            ENDIF
C
            CALL FTCLOS(LUNO,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
	 context = 'Unable to close the orbit extension '//INANC
	        call fcerr(context)
	        go to 999
            ENDIF
        ELSE
            CALL FTCLOS(LUNA,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
	 context = 'Unable to close the fits file '//INATT
	        call fcerr(context)
	        go to 999
            ENDIF
C
            CALL FTCLOS(LUNE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
	 context = 'Unable to close the fits file '//INEVE
	        call fcerr(context)
	        go to 999
            ENDIF
C
            CALL FTCLOS(LUNO,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
	 context = 'Unable to close the fits file '//INORB
	        call fcerr(context)
	        go to 999
            ENDIF
        ENDIF
C
        call fcecho(' ')
        write(context,'(''TOTAL TIME:'',F10.5)') TIME
 	call fcecho(context)
        call fcecho(' ')
        
        call fcecho(' ')
        write(context,'(''TOTAL COUNTS:'',F10.5)') TOTCTS
 	call fcecho(context)
        call fcecho(' ')
        
C
C  Process the low band
C
        T1 = T1/WT1
        call fcecho(' ')
        write(context,'(''R1 TOTAL ='',F10.5)') WT1
 	call fcecho(context)
        call fcecho(' ')
C
        TS1 = TS1/WT1
        TC1 = TC1/WT1
        A = DSQRT(TS1*TS1 + TC1*TC1)
        THETA = DATAN2(TS1,TC1)
        IF(THETA .LT. 0.) THETA = THETA + PI2
        call fcecho(' ')
        write(context,'(''R1 THETA ='',F10.5)') 180.*THETA/3.1415926
 	call fcecho(context)
        call fcecho(' ')
C
        call fcecho(' ')
        write(context,'(''R1 TILT ='',F10.5)') A
 	call fcecho(context)
        call fcecho(' ')
C
C
C  Process the medium band
C
        T2 = T2/WT2
C
        call fcecho(' ')
        write(context,'(''R2 TOTAL ='',F10.5)') WT2
 	call fcecho(context)
        call fcecho(' ')
        TS2 = TS2/WT2
        TC2 = TC2/WT2
        A = DSQRT(TS2*TS2 + TC2*TC2)
        THETA = DATAN2(TS2,TC2)
        IF(THETA .LT. 0.) THETA = THETA + PI2
C
        call fcecho(' ')
        write(context,'(''R2 THETA ='',F10.5)') 180.*THETA/3.1415926
 	call fcecho(context)
        call fcecho(' ')
C
        call fcecho(' ')
        write(context,'(''R2 TILT ='',F10.5)') A
 	call fcecho(context)
        call fcecho(' ')
C
C  Process the R4 band
C
        T4 = T4/WT4
C
        call fcecho(' ')
        write(context,'(''R4 TOTAL ='',F10.5)') WT4
 	call fcecho(context)
        call fcecho(' ')
        TS4 = TS4/WT4
        TC4 = TC4/WT4
        A = DSQRT(TS4*TS4 + TC4*TC4)
        THETA = DATAN2(TS4,TC4)
        IF(THETA .LT. 0.) THETA = THETA + PI2
C
        call fcecho(' ')
        write(context,'(''R4 THETA ='',F10.5)') 180.*THETA/3.1415926
 	call fcecho(context)
        call fcecho(' ')
C
        call fcecho(' ')
        write(context,'(''R4 TILT ='',F10.5)') A
 	call fcecho(context)
        call fcecho(' ')
C
C  Process the R5 band
C
        T5 = T5/WT5
C
        call fcecho(' ')
        write(context,'(''R5 TOTAL ='',F10.5)') WT5
 	call fcecho(context)
        call fcecho(' ')
        TS5 = TS5/WT5
        TC5 = TC5/WT5
        A = DSQRT(TS5*TS5 + TC5*TC5)
        THETA = DATAN2(TS5,TC5)
        IF(THETA .LT. 0.) THETA = THETA + PI2
C
        call fcecho(' ')
        write(context,'(''R5 THETA ='',F10.5)') 180.*THETA/3.1415926
 	call fcecho(context)
        call fcecho(' ')
C
        call fcecho(' ')
        write(context,'(''R5 TILT ='',F10.5)') A
 	call fcecho(context)
        call fcecho(' ')
C
C  Process the R6 band
C
        T6 = T6/WT6
C
        call fcecho(' ')
        write(context,'(''R6 TOTAL ='',F10.5)') WT6
 	call fcecho(context)
        call fcecho(' ')
        TS6 = TS6/WT6
        TC6 = TC6/WT6
        A = DSQRT(TS6*TS6 + TC6*TC6)
        THETA = DATAN2(TS6,TC6)
        IF(THETA .LT. 0.) THETA = THETA + PI2
C
        call fcecho(' ')
        write(context,'(''R6 THETA ='',F10.5)') 180.*THETA/3.1415926
 	call fcecho(context)
        call fcecho(' ')
C
        call fcecho(' ')
        write(context,'(''R6 TILT ='',F10.5)') A
 	call fcecho(context)
        call fcecho(' ')
C
C  Process the R7 band
C
        T7 = T7/WT7
C
        call fcecho(' ')
        write(context,'(''R7 TOTAL ='',F10.5)') WT7
 	call fcecho(context)
        call fcecho(' ')
        TS7 = TS7/WT7
        TC7 = TC7/WT7
        A = DSQRT(TS7*TS7 + TC7*TC7)
        THETA = DATAN2(TS7,TC7)
        IF(THETA .LT. 0.) THETA = THETA + PI2
C
        call fcecho(' ')
        write(context,'(''R7 THETA ='',F10.5)') 180.*THETA/3.1415926
 	call fcecho(context)
        call fcecho(' ')
C
        call fcecho(' ')
        write(context,'(''R7 TILT ='',F10.5)') A
 	call fcecho(context)
        call fcecho(' ')
C

 999    continue
	if (status .ne. 0) then
        call fcerrm(status)
        stop
        endif
C
        STOP
        END


C**********************************************************************
C SUBROUTINE:
C      gtilt
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C
C NOTES:
C       gtilt uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C     call gtilt(OBS, ISTYLE, IFLAG,
C    & T0, IS01, IS11, IS02, IS12, IS04, IS14,
C    & TLOG1, SCALE1, TLOG2, SCALE2, DIRSTR, STATUS)
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

      subroutine gtilt(OBS, ISTYLE, IFLAG,
     & T0, IS01, IS11, IS02, IS12, IS04, IS14,
     & TLOG1, SCALE1, TLOG2, SCALE2, caldbdir, STATUS)
C
      character(8) OBSSTR
      character(1) OBS(8)
      character(80) context
      character(80) caldbdir
      integer istyle, iflag, status
      real*4  T0, IS01, IS11, IS02, IS12, IS04, IS14
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

C  get the value of the T0 parameter
      call uclgsr('t0',t0,status)
	    if (status .ne. 0) then
      context = 'could not get T0 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the IS01 parameter
      call uclgsr('IS01',IS01,status)
	    if (status .ne. 0) then
      context = 'could not get IS01 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the IS11 parameter
      call uclgsr('IS11',IS11,status)
	    if (status .ne. 0) then
      context = 'could not get IS11 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the IS02 parameter
      call uclgsr('IS02',IS02,status)
	    if (status .ne. 0) then
      context = 'could not get IS02 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the IS12 parameter
      call uclgsr('IS12',IS12,status)
	    if (status .ne. 0) then
      context = 'could not get IS12 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the IS04 parameter
      call uclgsr('IS04',IS04,status)
	    if (status .ne. 0) then
      context = 'could not get IS04 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the IS14 parameter
      call uclgsr('IS14',IS14,status)
	    if (status .ne. 0) then
      context = 'could not get IS14 parameter'
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
