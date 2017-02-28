      SUBROUTINE SOSTA( CMDID, MAP, SZX, SZY, MAPID, STATUS )
C**********************************************************************
C  Calculates source statistics
C
C  I  CMDID    (I)  Command id
C  I  MAP      (R)  Image map
C  I  SZX/Y    (I)  Size of maps
C  I  MAPID    (S)  Map id string
C  O  STATUS   (I)  Error flag (0 = OK)
C**********************************************************************
      IMPLICIT NONE

C*****************
C Input variables
C*****************
      CHARACTER*(*) MAPID
      
      INTEGER*4 CMDID
      INTEGER*4 SZX
      INTEGER*4 SZY
      INTEGER*4 STATUS
      
      REAL*4 MAP(SZX, SZY)

C*****************
C Common includes
C*****************
      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/dynmem.inc'
      INCLUDE 'backgd.inc'
      INCLUDE 'detect.inc'

C*****************************
C Parameters and related vars
C*****************************
      LOGICAL OPTIMIZE
      LOGICAL DETECT
      LOGICAL BOX_BCK
      LOGICAL EXPOWGT
      
      CHARACTER*(MAX_FILELEN) BGREGION
      CHARACTER*(MAX_FILELEN) SRCREGION

      INTEGER*4 ARGC
      
      REAL*4 SOXPIX
      REAL*4 SOYPIX
      REAL*4 RADSRC
      REAL*4 RADEEF0
      REAL*4 EEFBAK0
      REAL*4 BACKGD
      REAL*4 RADIN0
      REAL*4 RADOUT0
      
C*****************
C Local variables
C*****************
      LOGICAL LOCAL_BCK
      LOGICAL LOOP
      LOGICAL GLOBAL
      LOGICAL BADEXPO
      LOGICAL NPSF1ST
      LOGICAL VIGN1ST
      LOGICAL UNKNOWN
      LOGICAL VIGNAPP

      CHARACTER*(MAX_IDSTR) BGMAPID
      CHARACTER*(MAX_IDSTR) EXMAPID

      INTEGER EXSZX
      INTEGER EXSZY
      INTEGER EXMAPPTR
      INTEGER BGSZX
      INTEGER BGSZY
      INTEGER NUMREG
      INTEGER SLEN

      INTEGER*4 I
      INTEGER*4 N
      INTEGER*4 ITEL
      INTEGER*4 P_VIGN
      INTEGER*4 IFRAT
      INTEGER*4 IVIGN

      REAL*4 RADIN
      REAL*4 RADOUT
      REAL*4 PNT(2)
      REAL*4 RADEEF
      REAL*4 EEFBAK
      REAL*4 XPIX
      REAL*4 YPIX
      REAL*4 ERRBG
      REAL*4 SOURC
      REAL*4 ERRSRC
      REAL*4 SFA
      REAL*4 RADEB
      REAL*4 PROBA
      REAL*4 PROBGA
      REAL*4 VCOR
      REAL*4 FRACC
      REAL*4 TSIG
      REAL*4 CNTSRC
      REAL*4 AREASRC

      REAL*8 EXPOSURE
      REAL*8 DTIME
      REAL*8 ZMX
      REAL*8 ZMY
      REAL*8 XCEN
      REAL*8 YCEN
      REAL*8 BGZMX
      REAL*8 BGZMY
      REAL*8 BGXCEN
      REAL*8 BGYCEN

C****************
C Plot variables
C****************
      INTEGER COLOR
      INTEGER LWIDTH
      INTEGER LSTYLE
      INTEGER EXCOLOR
      INTEGER EXLWIDTH
      INTEGER EXLSTYLE

C***********
C Functions
c***********
      LOGICAL ISDISPLAY
      LOGICAL ISMOUSE
      LOGICAL ISLOADED
      LOGICAL ISRNULL
      LOGICAL ISCPMAPID

      INTEGER LENACT

      REAL*4 RNULL

C************
C Dummy vars
C************
      character(40) DS

      INTEGER DI

C***********************************************************
C Save last values of XPIX/YPIX for default in manual entry
C***********************************************************
      DATA XPIX/0.0/
      DATA YPIX/0.0/
      SAVE XPIX
      SAVE YPIX

C***********************
C Initialize parameters
C***********************
      SOXPIX    = RNULL( )
      SOYPIX    = RNULL( )
      BACKGD    = -1.
      LOCAL_BCK = .FALSE.
      BOX_BCK   = .FALSE.
      OPTIMIZE  = .FALSE.
      RADSRC    = -1.
      RADIN0    = -1.
      RADOUT0   = -1.
      RADEEF0   = 0.66
      EEFBAK0   = 0.90
      DETECT    = .FALSE.
      BGREGION  = ' '
      SRCREGION = ' '
      EXPOWGT   = .FALSE.

C**********************
C Get input parameters
C**********************
      STATUS = 0
      CALL NUMCARG( CMDID, ARGC, STATUS )
      IF ( ARGC.NE.0 ) THEN
         CALL WRONGARGS( CMDID, STATUS )
      ENDIF

      CALL GPARR( CMDID, 'XPIX', SOXPIX, STATUS )
      CALL GPARR( CMDID, 'YPIX', SOYPIX, STATUS )
      CALL GPARR( CMDID, 'SOURCE_BOX_RADIUS', RADSRC, STATUS )
      CALL GPARR( CMDID, 'EEF_SOURCE', RADEEF0, STATUS )
      CALL GPARR( CMDID, 'EEF_BACKGROUND', EEFBAK0, STATUS )
      CALL GPARL( CMDID, 'BOX_BACKGROUND', BOX_BCK, STATUS )
      CALL GPARL( CMDID, 'DETECT_SOURCES', DETECT, STATUS )
      CALL GPARL( CMDID, 'OPTIMIZE', OPTIMIZE, STATUS )
      CALL GPARR( CMDID, 'BACKGROUND_LEVEL', BACKGD, STATUS )
      CALL GPARR( CMDID, 'INNER_RADIUS', RADIN0, STATUS )
      CALL GPARR( CMDID, 'OUTER_RADIUS', RADOUT0, STATUS )
      CALL GPARS( CMDID, 'BGREGION', BGREGION, STATUS )
      CALL GPARS( CMDID, 'SRCREGION', SRCREGION, STATUS )
      CALL GPARL( CMDID, 'EXPOWGT', EXPOWGT, STATUS )
      IF ( STATUS.NE.0 ) THEN
         RETURN
      ENDIF

C**************************************************
C Set flag value of -1 for plotreg to use defaults
C**************************************************
      LWIDTH   = -1
      LSTYLE   = -1
      EXCOLOR  = -1
      EXLWIDTH = -1
      EXLSTYLE = -1

C********************
C Check input map(s)
C********************
      WRITE( ZWRITE, '(2A)' ) ' Using ', MAPID(:LENACT(MAPID))
      CALL XWRITE( ZWRITE, 10 )

      IF ( .NOT.ISLOADED( MAPID ) ) THEN
         CALL XWRITE( ' Image not loaded', 10 )
         STATUS = -1
         RETURN
      ENDIF

      EXMAPID = ' '
      CALL GHEADS( MAPID, 'EXMAPID', EXMAPID, 0, STATUS )

C************************
C Check input parameters
C************************
      IF ( RADEEF0.LT.0.1 .OR. RADEEF0.GT.0.98 ) THEN
         CALL XWRITE( ' EEF_SOURCE must be between 0.1 and 0.98', 10 )
         STATUS = 1
         RETURN
      ENDIF
 
      IF ( EEFBAK0.LT.0.1 .OR. EEFBAK0.GT.0.98 ) THEN
         CALL XWRITE( ' EEF_BACK must be between 0.1 and 0.98', 10 )
         STATUS = 1
         RETURN
      ENDIF

      IF ( DETECT ) THEN
         IF ( NUMDET.EQ.0 ) THEN
            CALL XWARN( ' No detected sources', 10 )
            CALL XWRITE( ' Run SEARCH or DETECT command', 10 )
            STATUS = -1
            RETURN
         ENDIF
      ENDIF

C****************************
C Check how to do background
C****************************
      IF ( BACKGD.GT.0.0 ) THEN
         CALL XWRITE( ' Using constant background...', 10 )
      ELSEIF ( BOX_BCK ) THEN
         CALL XWRITE( ' Using box background...', 10 )
      ELSEIF ( BGREGION.NE.' ' ) THEN
         CALL XWRITE( ' Using background defined by region...', 10 )
      ELSE
         CALL XWRITE( ' Using a locally computed background', 10 )
         LOCAL_BCK = .TRUE.
      ENDIF

C     If SRCREGION specified, we can't do local 
C     background or optimization
      IF ( SRCREGION.NE.' ' ) THEN
         IF ( LOCAL_BCK ) THEN
            CALL XWRITE( ' Specify BACKGROUND_LEVEL, BOX_BACKGROUND,'//
     &                   ' or BGREGION with SRCREGION', 10 )
            STATUS = -1
            RETURN
         ENDIF
         IF ( OPTIMIZE ) THEN
            OPTIMIZE = .FALSE.
         ENDIF
      ENDIF

C********************
C Get background map
C********************
      BGMAPID = 'BGMAP'
      CALL GETBGMAP( MAPID, BGMAP, BGSZ, STATUS )
      IF ( STATUS.NE.0 ) THEN
         RETURN
      ENDIF

C**********************************************
C Set reference frame for image and background
C**********************************************
      CALL GET_REFRAM( MAPID, DI, DI, ZMX, ZMY, XCEN, YCEN, STATUS )
      CALL GET_REFRAM( BGMAPID, BGSZX, BGSZY, BGZMX, BGZMY, BGXCEN,
     &                 BGYCEN, STATUS )

C     Check the zoom is not zero
      IF ( ZMX.EQ.0.0D0 .OR. ZMY.EQ.0.0D0 .OR. 
     &     BGZMX.EQ.0.0D0 .OR. BGZMY.EQ.0.0D0 ) THEN
         CALL XAERROR( ' Zoom factor is zero', 10 )
         STATUS = -1
         RETURN
      ENDIF

C     Which telescope?
      CALL GET_ITEL( MAPID, ITEL )

C*************************************
C Get the exposure time and dead-time
C*************************************
      BADEXPO  = .FALSE.
      EXPOSURE = 0.0
      CALL GHEADD( MAPID, 'EXPOSURE', EXPOSURE, 0, STATUS )
      IF ( EXPOSURE.LE.0.0D0 ) THEN
         BADEXPO  = .TRUE.
         EXPOSURE = 1
      ENDIF

      CALL GHEADD( MAPID, 'DTIME', DTIME, 0, STATUS )

C************************
C Get the exposure mapid
C************************
      IF ( EXMAPID.NE.' ' ) THEN
         CALL GHEADI( EXMAPID, 'EXPMAP', DI, 0, STATUS )
         IF ( STATUS.NE.0 .OR. DI.LT.1 ) THEN
            WRITE( ZWRITE, '(2A)' ) ' Invalid exposure map: ', EXMAPID
            CALL XWRITE( ZWRITE, 10 )
            EXMAPID = ' '
         ENDIF
      ENDIF

C********************************************************************
C Init NPSF1ST for NPSF() routine inside SOSTA_SOURCE()
C Init VIGN1ST and P_VIGN for GET_VIGN() routine inside SOSTA_MISC()
C********************************************************************
      NPSF1ST = .TRUE.
      VIGN1ST = .TRUE.
      P_VIGN  = -1

C*************************************************************
C Clear sosta array from Tcl environment (set in SOSTA_OUT())
C*************************************************************
      GLOBAL = .FALSE.
      CALL TCLUNSET( 'sosta', GLOBAL, STATUS )
      STATUS = 0

C**********************************************
C Loop on sources and do the source statistics
C**********************************************
      I = 0
      LOOP = .FALSE.
      DO WHILE ( .TRUE. )

         RADIN  = RADIN0
         RADOUT = RADOUT0
         RADEEF = RADEEF0
         EEFBAK = EEFBAK0

C        If running on detected sources, use next source coords
         IF ( DETECT ) THEN

            I = I + 1
            RADSRC = -1.0
            IF ( I.GT.NUMDET ) THEN
               GOTO 200
            ENDIF
            IF ( HOT(I).NE.1 ) THEN
               GOTO 100
            ENDIF
            XPIX = DTSOX(I)
            YPIX = DTSOY(I)
            LOOP = .TRUE.

C        Use input coordinates if given
         ELSEIF ( .NOT.( ISRNULL( SOXPIX ).OR.ISRNULL( SOYPIX ) ) ) THEN

            XPIX = SOXPIX
            YPIX = SOYPIX

C        Region file specified
         ELSEIF ( SRCREGION.NE.' ' ) THEN

            XPIX = RNULL( )
            YPIX = RNULL( )

C        Get coordinates interactively from user
         ELSE

            IF ( .NOT. ( ISDISPLAY( ) .AND. ISMOUSE( ) ) ) THEN
               CALL XWRITE( ' No interactive display', 10 )
               STATUS = -1
               RETURN
            ENDIF
            IF ( .NOT.ISCPMAPID( 'DIS', MAPID ) ) THEN
               CALL XWRITE( ' Current map is not display map', 10 )
               CALL MAPRCMD( 1 )
               STATUS = -1
               RETURN
            ENDIF
            CALL XWRITE( ' Select the center of source box '//
     &                   '(Right button exits)', 10 )
            CALL TCLRESLR( 'select', PNT, N, 2, STATUS )
            IF ( STATUS.NE.0 ) THEN
               GOTO 200
            ENDIF
            XPIX = PNT(1)
            YPIX = PNT(2)

         ENDIF

C        Call the source statistics routine
         CALL SOSTA_SOURCE( MAPID, MAP, SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                      XPIX, YPIX, SRCREGION, ITEL, NPSF1ST,
     &                      RADSRC, RADEEF, EEFBAK, EXPOWGT, SFA, 
     &                      RADEB, CNTSRC, AREASRC, UNKNOWN, STATUS )
         IF ( STATUS.NE.0 ) THEN
            RETURN
         ENDIF

C        Plot the source region
         IF ( ISCPMAPID( 'DIS', MAPID ) .AND. ISDISPLAY( ) ) THEN
            COLOR = 16
            CALL GET_COLOR( COLOR )
            CALL PLOTREG( 1, COLOR, LWIDTH, LSTYLE, EXCOLOR, EXLWIDTH,
     &                    EXLSTYLE, STATUS )
         ENDIF

C        If doing local background, and inner/outer radii not set
C        use twice the half-box found by SOSTA_SOURCE()
         IF ( LOCAL_BCK .AND. RADIN.LT.0.0 .AND. RADOUT.LT.0.0 ) THEN

            IF ( UNKNOWN ) THEN
               CALL XAERROR( ' Unknown PSF: background must be'//
     &                       ' specified independent of PSF', 10 )
               STATUS = -1
               RETURN
            ENDIF
 
            RADOUT = RADEB * 2.0
            RADIN  = RADOUT / 1.9

         ENDIF

C        Get exposure time and vignapp from the exposure map
         VIGNAPP = .FALSE.
         IF ( EXMAPID.NE.' ' .AND. .NOT.EXPOWGT ) THEN
            CALL GHEADI( EXMAPID, 'MAPPTR', EXMAPPTR, 0, STATUS )
            CALL GHEADI( EXMAPID, 'SZX', EXSZX, 0, STATUS )
            CALL GHEADI( EXMAPID, 'SZY', EXSZY, 0, STATUS )
            BADEXPO = .FALSE.
            CALL GET_EXPOSURE( MEMR(EXMAPPTR), EXSZX, EXSZY, EXMAPID,
     &                         XPIX, YPIX, RADSRC, EXPOSURE )
            IF ( EXPOSURE.LE.0 ) THEN
               CALL XWARN( ' Exposure from map equals zero', 5 )
               BADEXPO  = .TRUE.
               EXPOSURE = 1.0
            ENDIF

            IVIGN = 0
            CALL GHEADI( EXMAPID, 'VIGNAPP', IVIGN, 0, STATUS )
            IF ( IVIGN.GT.0 ) THEN
               VIGNAPP = .TRUE.
            ENDIF
         ELSEIF ( EXMAPID.NE.' ' ) THEN
            IVIGN = 0
            CALL GHEADI( EXMAPID, 'VIGNAPP', IVIGN, 0, STATUS )
            IF ( IVIGN.GT.0 ) THEN
               VIGNAPP = .TRUE.
            ENDIF
         ENDIF
 
C        Do the background statistics
         CALL SOSTA_BACK( MAPID, MAP, SZX, SZY, ZMX, ZMY, XCEN, YCEN, 
     &                    BGMAP, BGSZX, BGSZY, BGZMX, BGZMY, 
     &                    BGXCEN, BGYCEN, XPIX, YPIX, LOCAL_BCK,
     &                    RADIN, RADOUT, BOX_BCK, BGREGION, CNTSRC,
     &                    AREASRC, BACKGD, ERRBG, SOURC, ERRSRC, NUMREG,
     &                    STATUS)
         IF ( STATUS.NE.0 ) THEN
            RETURN
         ENDIF

C        Plot the background region
         IF ( ISCPMAPID( 'DIS', MAPID ) .AND. ISDISPLAY( ) ) THEN
            COLOR = 14
            CALL GET_COLOR( COLOR )
            DO N = 1, NUMREG
               CALL PLOTREG( N, COLOR, LWIDTH, LSTYLE, EXCOLOR,
     &                       EXLWIDTH, EXLSTYLE, STATUS )
            ENDDO
         ENDIF
 
C        Do some more miscellaneous computations 
C        (upper limits, probabilities, vignetting..)
         CALL SOSTA_MISC( MAPID, XPIX, YPIX, ITEL, VIGNAPP, CNTSRC,
     &                    AREASRC, RADSRC, SFA, EXPOSURE, DTIME, BACKGD,
     &                    SOURC, ERRSRC, EXPOWGT, UNKNOWN, IFRAT, PROBA,
     &                    PROBGA, VCOR, FRACC, TSIG, VIGN1ST, P_VIGN )

C        If OPTIMIZE, then re-do the statistics using 
C        the optimal half-box size
         IF ( OPTIMIZE ) THEN
            CALL XWRITE( '  ', 10 )

            CALL XDSTR( DBLE( RADSRC ), -1, DS, SLEN )
            WRITE ( ZWRITE, '(3A)' ) ' Optimum half box size is: ',
     &                               DS(:SLEN), ' pixels'
            CALL XWRITE( ZWRITE, 10 )
            CALL XWRITE( ' Starting 2nd iteration ', 10 )

C           Get source stats
            CALL SOSTA_SOURCE( MAPID, MAP, SZX, SZY, ZMX, ZMY, 
     &                         XCEN, YCEN, XPIX, YPIX, SRCREGION, ITEL,
     &                         NPSF1ST, RADSRC, RADEEF, EEFBAK, EXPOWGT,
     &                         SFA, RADEB, CNTSRC, AREASRC,
     &                         UNKNOWN, STATUS )

C           Get background stats if required
            IF ( LOCAL_BCK ) THEN
               CALL SOSTA_BACK( MAPID, MAP, SZX, SZY, ZMX, ZMY, 
     &                          XCEN, YCEN, BGMAP, BGSZX, BGSZY, BGZMX,
     &                          BGZMY, BGXCEN, BGYCEN, XPIX, YPIX,
     &                          LOCAL_BCK, RADIN, RADOUT, BOX_BCK,
     &                          BGREGION, CNTSRC, AREASRC, BACKGD,
     &                          ERRBG, SOURC, ERRSRC, NUMREG, STATUS )
            ENDIF

C           Do misc computations
            CALL SOSTA_MISC( MAPID, XPIX, YPIX, ITEL, VIGNAPP, CNTSRC,
     &                       AREASRC, RADSRC, SFA, EXPOSURE, DTIME,
     &                       BACKGD, SOURC, ERRSRC, EXPOWGT, UNKNOWN,
     &                       IFRAT, PROBA, PROBGA, VCOR, FRACC, TSIG,
     &                       VIGN1ST, P_VIGN )
         ENDIF

C        Write output
         CALL SOSTA_OUT( XPIX, YPIX, ITEL, DETECT, VIGNAPP, RADSRC,
     &                   BADEXPO, EXPOSURE, EXPOWGT, DTIME, CNTSRC, 
     &                   AREASRC, BACKGD, ERRBG, SOURC, ERRSRC, IFRAT,
     &                   PROBA, PROBGA, VCOR, FRACC, TSIG, STATUS )

         IF ( .NOT.(LOOP) ) THEN
            GOTO 200
         ENDIF

C     END loop over sources
 100  ENDDO

 200  CONTINUE

C**************************************
C Free vignetting map from GET_VIGN( )
C**************************************
      CALL RALLOC( 0, -1, -1, P_VIGN, STATUS )
 
C******
C Done
C******
      RETURN

      END
