      SUBROUTINE SOSTA_BACK( MAPID, MAP, SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                       BGMAP, BGSZX, BGSZY, BGZMX, BGZMY,
     &                       BGXCEN, BGYCEN, XPIX, YPIX, LOCAL_BCK,
     &                       RADIN, RADOUT, BOX_BCK, BGREGION, CNTSRC,
     &                       AREASRC, BACKGD, ERRBG, SOURC, ERRSRC,
     &                       NUMREG, STATUS )
C**********************************************************************
C  Compute background for sosta
C
C  I  MAPID     (S)  Map ID string
C  I  MAP       (R)  Image map
C  I  SZX/Y     (I)  Size of map
C  I  ZMX/Y     (D)  Zoom of map
C  I  X/YCEN    (D)  Center of map
C  I  BGMAP     (R)  Background map
C  I  BGSZX/Y   (I)  Size of background map
C  I  BGZMX/Y   (D)  Zoom of background map
C  I  BGX/YCEN  (D)  Center of background map
C  I  X/YPIX    (R)  Source box center in original pixels
C  I  LOCAL_BCK (L)  Whether to calculate background locally
C  I  RADIN     (R)  Inner box radius for local bg calculation
C  I  RADOUT    (R)  Outer box radius for local bg calculation
C  I  BOX_BCK   (L)  Whether to use user-defined boxes
C  I  BGREGION  (S)  Regionfile used to calculate background
C  I  CNTSRC    (R)  Sum of source counts
C  I  AREASRC   (R)  Number of detector pixels summed
C I/O BACKGD    (R)  Computed background
C  O  ERRBG     (R)  Background error
C  O  SOURC     (R)  Source value
C  O  ERRSRC    (R)  Source error
C  O  NUMREG    (I)  Number of background regions
C  O  STATUS    (I)  Error flag (0=OK)
C**********************************************************************
      IMPLICIT NONE

C*****************
C Input variables
C*****************
      LOGICAL LOCAL_BCK
      LOGICAL BOX_BCK

      CHARACTER*(*) BGREGION
      CHARACTER*(*) MAPID

      INTEGER*4 SZX
      INTEGER*4 SZY
      INTEGER*4 BGSZX
      INTEGER*4 BGSZY
      INTEGER*4 NUMREG
      INTEGER*4 STATUS

      REAL*4 XPIX
      REAL*4 YPIX
      REAL*4 RADIN
      REAL*4 RADOUT
      REAL*4 CNTSRC
      REAL*4 AREASRC
      REAL*4 BACKGD
      REAL*4 ERRBG
      REAL*4 SOURC
      REAL*4 ERRSRC

      REAL*4 MAP(SZX, SZY)
      REAL*4 BGMAP(BGSZX, BGSZY)

      REAL*8 XCEN
      REAL*8 YCEN
      REAL*8 BGXCEN
      REAL*8 BGYCEN
      REAL*8 ZMX
      REAL*8 ZMY
      REAL*8 BGZMX
      REAL*8 BGZMY

C***************
C Include files
C***************
      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'

C*****************
C Local variables
C*****************
      character(40) DS1
      character(40) DS2

      CHARACTER*(MAX_IDSTR) WCSID

      INTEGER IXWID
      INTEGER IYWID

      INTEGER*4 I
      INTEGER*4 N

      REAL*4 CNTIN
      REAL*4 CNTOUT
      REAL*4 CNTTOT
      REAL*4 AREAIN
      REAL*4 AREAOUT
      REAL*4 AREATOT
      REAL*4 WGT
      REAL*4 BGXIMG
      REAL*4 BGYIMG
      REAL*4 SCALBG
      REAL*4 CNTBG
      REAL*4 VAR
      REAL*4 XIMG
      REAL*4 YIMG
      REAL*4 RADIMGX
      REAL*4 RADIMGY
      REAL*4 RADACTX
      REAL*4 RADACTY
      REAL*4 XP1
      REAL*4 YP1
      REAL*4 XP2
      REAL*4 YP2

      REAL*4 PNTS(4)

      REAL*8 XBWID
      REAL*8 YBWID
      REAL*8 XBCEN
      REAL*8 YBCEN

C**********
C Funcions
C**********
      LOGICAL ISDISPLAY
      LOGICAL ISMOUSE

      INTEGER SLEN1
      INTEGER SLEN2

C**********************
C Initialize variables
C**********************
      STATUS  = 0
      CNTTOT  = 0.0
      AREATOT = 0.0


C*******************************
C Cleanup any allocated regions
C*******************************
      CALL XFREEREG(STATUS)

C********************************************
C Calculate background weight for xpix, ypix
C********************************************
      CALL CALIMGPIX( BGSZX, BGSZY, BGZMX, BGZMY, BGXCEN, BGYCEN,
     &                XPIX, YPIX, BGXIMG, BGYIMG, 2 )

      WGT = BGMAP( MIN( MAX( NINT( BGXIMG ), 1 ), BGSZX ),
     &             MIN( MAX( NINT( BGYIMG ), 1 ), BGSZY ) )

C********************************************
C Do the background calculation that we want
C********************************************

      IF ( LOCAL_BCK ) THEN
         
C******************
C Local background
C******************

C        Chat a little
         WRITE( ZWRITE, 99003 ) RADIN, RADOUT
         CALL XWRITE( ZWRITE, 10 )

C        Print actual radii used based on source center and image rebin
C
C        Note: x and y direction may not be same size depending
C              on where edge of box falls.  Pixels are counted
C              if their center falls in the box.
         CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                   XPIX, YPIX, XIMG, YIMG, 2 )

         RADIMGX = RADIN / ZMX
         RADIMGY = RADIN / ZMY
         IXWID   = INT( XIMG + RADIMGX ) - INT( XIMG - RADIMGX )
         IYWID   = INT( YIMG + RADIMGY ) - INT( YIMG - RADIMGY )
         RADACTX = FLOAT( IXWID ) * ZMX / 2.0
         RADACTY = FLOAT( IYWID ) * ZMY / 2.0

         CALL XDSTR( DBLE( RADACTX ), -1, DS1, SLEN1 )
         CALL XDSTR( DBLE( RADACTY ), -1, DS2, SLEN2 )
         WRITE( ZWRITE, '(5A)' ) ' Actual inner radius x: ',
     &                           DS1(:SLEN1), ' y: ', DS2(:SLEN2),
     &                           ' based on rebin and center'
         CALL XWRITE( ZWRITE, 15 )
         
         RADIMGX = RADOUT / ZMX
         RADIMGY = RADOUT / ZMY
         IXWID   = INT( XIMG + RADIMGX ) - INT( XIMG - RADIMGX )
         IYWID   = INT( YIMG + RADIMGY ) - INT( YIMG - RADIMGY )
         RADACTX = FLOAT( IXWID ) * ZMX / 2.0
         RADACTY = FLOAT( IYWID ) * ZMY / 2.0

         CALL XDSTR( DBLE( RADACTX ), -1, DS1, SLEN1 )
         CALL XDSTR( DBLE( RADACTY ), -1, DS2, SLEN2 )
         WRITE( ZWRITE, '(5A)' ) ' Actual outer radius x: ',
     &                           DS1(:SLEN1), ' y: ', DS2(:SLEN2),
     &                           ' based on rebin and center'
         CALL XWRITE( ZWRITE, 15 )

C        Calculate the inner box region
         XBWID = RADIN * 2.0D0
         YBWID = RADIN * 2.0D0
         CALL XBOXREG( 0, DBLE( XPIX ), DBLE( YPIX ), XBWID, YBWID,
     &                 0.0D0, NUMREG, STATUS )

C        Calculate the outer box region
         XBWID = RADOUT * 2.0D0
         YBWID = RADOUT * 2.0D0
         CALL XBOXREG( 1, DBLE( XPIX ), DBLE( YPIX ), XBWID, YBWID,
     &                 0.0D0, NUMREG, STATUS )

C        Check that we have both, and no errors
         IF ( NUMREG.NE.2 ) THEN
            STATUS = 1
         ENDIF
         IF ( STATUS.NE.0 ) THEN
            RETURN
         ENDIF

C        Get the background counts in the inner region
         CALL BGCNTREG( 1, MAP, SZX, SZY, ZMX, ZMY, XCEN, YCEN, BGMAP,
     &                  BGSZX, BGSZY, BGZMX, BGZMY, BGXCEN, BGYCEN,
     &                  WGT, CNTIN, AREAIN, STATUS )
         IF ( STATUS.NE.0 ) THEN
            RETURN
         ENDIF

C        Get the background counts in the outer region
         CALL BGCNTREG( 2, MAP, SZX, SZY, ZMX, ZMY, XCEN, YCEN, BGMAP,
     &                  BGSZX, BGSZY, BGZMX, BGZMY, BGXCEN, BGYCEN,
     &                  WGT, CNTOUT, AREAOUT, STATUS )
         IF ( STATUS.NE.0 ) THEN
            RETURN
         ENDIF

C        Calculate the net counts and area
         AREAIN  = AREAIN * ( ZMX * ZMY )
         AREAOUT = AREAOUT * ( ZMX * ZMY )
         CNTTOT  = CNTOUT - CNTIN
         AREATOT = AREAOUT - AREAIN

         IF ( AREATOT.LE.0.0 ) THEN
            CNTTOT = 1.0
            CALL XWRITE( ' Error: zero or -ve background area', 10 )
         ENDIF

C        Print results of counting outer-inner box
         CALL XDSTR( DBLE( CNTIN ), -1, DS1, SLEN1 )
         CALL XISTR( INT( AREAIN ), DS2, SLEN2 )
         WRITE( ZWRITE, 99004 ) DS1(:SLEN1), DS2(:SLEN2)
         CALL XWRITE( ZWRITE, 10 )
         
         CALL XDSTR( DBLE( CNTOUT ), -1, DS1, SLEN1 )
         CALL XISTR( INT( AREAOUT ), DS2, SLEN2 )
         WRITE( ZWRITE, 99005 ) DS1(:SLEN1), DS2(:SLEN2)
         CALL XWRITE( ZWRITE, 10 )
         
         CALL XDSTR( DBLE( CNTTOT ), -1, DS1, SLEN1 )
         CALL XISTR( INT( AREATOT ), DS2, SLEN2 )
         WRITE( ZWRITE, 99006 ) DS1(:SLEN1), DS2(:SLEN2)
         CALL XWRITE( ZWRITE, 10 )

      ELSEIF ( BOX_BCK ) THEN

C**********************************************
C Do the background from a box (user selected)
C**********************************************

C        Check that we have a display
         IF ( .NOT.( ISDISPLAY( ) .AND. ISMOUSE( ) ) ) THEN
            CALL XWRITE( ' Interactive display unavailable', 10 )
            STATUS = -1
            RETURN
         ENDIF

C        Get the boxes from the user
         CALL XWRITE( ' Select background boxes (Right button exits)',
     &                10 )
         NUMREG = 0

         DO WHILE ( .TRUE. )
            CALL XWRITE( ' Select rectangular area for background '//
     &                   '(Right-click to exit)', 10 )
            CALL TCLRESLR( 'select box', PNTS, N, 4, STATUS )
            IF ( STATUS.NE.0 ) THEN
               GOTO 500
            ENDIF
            XP1 = PNTS(1)
            YP1 = PNTS(2)
            XP2 = PNTS(3)
            YP2 = PNTS(4)

            XBWID = ABS( XP1 - XP2 )
            YBWID = ABS( YP1 - YP2 )
            XBCEN = ( XP1 + XP2 ) / 2.0D0
            YBCEN = ( YP1 + YP2 ) / 2.0D0

C           Make the requested region
            CALL XBOXREG( 1, XBCEN, YBCEN, XBWID, YBWID,
     &                    0.0D0, NUMREG, STATUS )
         ENDDO

 500     CONTINUE

         STATUS = 0
         IF ( NUMREG.LE.0 ) THEN
            CALL XWRITE( ' Failed to define background boxes', 10 )
            STATUS = -1
            RETURN
         ENDIF

C        Get the counts in the regions
         CNTTOT  = 0.0
         AREATOT = 0.0
         DO I = 1, NUMREG
            CALL BGCNTREG( 1, MAP, SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                     BGMAP, BGSZX, BGSZY, BGZMX, BGZMY, BGXCEN,
     &                     BGYCEN, WGT, CNTOUT, AREAOUT, STATUS )

            CNTTOT = CNTTOT + CNTOUT

            IF ( STATUS.NE.0 ) THEN
               RETURN
            ENDIF
            AREATOT = AREATOT + AREAOUT
         ENDDO

C        Calculate the total unzoomed area used
         AREATOT = AREATOT * ( ZMX * ZMY )

      ELSEIF ( BGREGION.NE.' ' ) THEN

C*************************************
C Use user supplied background region
C*************************************

C        Setup the region WCS
         CALL GHEADS( MAPID, 'WCSID', WCSID, 0, STATUS )
         CALL SETREGWCS( WCSID, STATUS )

C        Parse the region file
         CALL XINITREG( BGREGION, NUMREG, STATUS )
         IF ( STATUS.NE.0 ) THEN
            RETURN
         ENDIF

C        Get the counts in the supplied regions
         CNTTOT  = 0.0
         AREATOT = 0.0
         DO I = 1, NUMREG
            CALL BGCNTREG( 1, MAP, SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                     BGMAP, BGSZX, BGSZY, BGZMX, BGZMY, BGXCEN,
     &                     BGYCEN, WGT, CNTOUT, AREAOUT, STATUS )
            IF ( STATUS.NE.0 ) THEN
               RETURN
            ENDIF
            CNTTOT  = CNTTOT + CNTOUT
            AREATOT = AREATOT + AREAOUT
         ENDDO

C        Calculate the total unzoomed area
         AREATOT = AREATOT * ( ZMX * ZMY )
 
      ELSEIF ( BACKGD.GT.0.0 ) THEN

C***************************
C Background value is given
C***************************
         NUMREG  = 0
         CNTTOT  = BACKGD * AREASRC
         AREATOT = AREASRC

      ENDIF

C*********************************
C Calculate the scaled background
C*********************************
      IF ( AREATOT.GT.0.0 ) THEN
         SCALBG = AREASRC / AREATOT
         BACKGD = CNTTOT / AREATOT
      ELSE
         SCALBG = 0.0
         BACKGD = 0.0
      ENDIF
      CNTBG = CNTTOT * SCALBG
      SOURC = CNTSRC - CNTBG
      IF ( BACKGD.GT.0 ) THEN
         ERRBG = SQRT( CNTTOT ) / AREATOT
      ELSE
         CALL XWRITE( 'warning: Negative Background Value', 10 )
         ERRBG = 0
      ENDIF

C*******************************************
C Calculate the error based on the variance
C*******************************************
      VAR = CNTSRC + CNTTOT * SCALBG * SCALBG
      IF ( VAR.GT.0 ) THEN
         ERRSRC = SQRT( VAR )
      ELSE
         CALL XWRITE( 'warning: Intensity Source Negative ', 10 )
         ERRSRC = 0
      ENDIF

C******
C Done
C******
      RETURN

C********************************************
C Various formatted output FORMAT statements
C********************************************
99003 FORMAT( ' Background inner radius:', F7.1, 
     &        ' pixels; outer radius:', F7.1,' pixels' )

99004 FORMAT( ' Innerbox counts ', A, ' in ', A, ' sq or pixels' )
99005 FORMAT( ' Outerbox counts ', A, ' in ', A, ' sq or pixels' )
99006 FORMAT( ' Background counts ', A, ' in ', A, ' sq pixels' )

      END
