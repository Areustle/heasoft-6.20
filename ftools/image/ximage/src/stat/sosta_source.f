      SUBROUTINE SOSTA_SOURCE( MAPID, MAP, SZX, SZY, ZMX, ZMY, XCEN,
     &                         YCEN, XPIX, YPIX, SRCREGION, ITEL,
     &                         NPSF1ST, RADSRC, RADEEF, EEFBAK, 
     &                         EXPOWGT, SFA, RADEB, CNTSRC, AREASRC,
     &                         UNKNOWN, STATUS )
C**********************************************************************
C  Determine box radius of source and count the contents
C
C  I  MAPID     (S)  Map id string
C  I  MAP       (R)  Image map
C  I  SZX/Y     (I)  Size of map
C  I  ZMX/Y     (D)  Zoom of map
C  I  X/YCEN    (D)  Center of map
C  I  X/YPIX    (R)  Source center in original pixel coordinates
C  I  SRCREGION (S)  Source region file
C  I  ITEL      (I)  Telescope index
C I/O NPSF1ST   (L)  Set to true before sosta loop, so npsf
C                    knows when to do one-time lookup
C I/O RADSRC    (R)  Source box radius 
C  I  RADEEF    (R)  Fractional EEF used to get source box size
C  I  EEFBAK    (R)  Fractional EEF used to select where the
C                    inner background box size begins
C  I  EXPOWGT   (L)  Do exposure map weighting?
C  O  SFA       (R)  SQRT(SFA) is zoom of FRAC
C  O  RADEB     (R)  Half-box corresponding to eefbak
C  O  CNTSRC    (R)  Sum of weighted source counts
C  O  AREASRC   (R)  Number of detector pixels summed
C  O  UNKNOWN   (L)  Whether PSF is unknown
C  O  STATUS    (I)  Error flag  (0=OK)
C**********************************************************************
      IMPLICIT NONE

C*****************
C Input variables
C*****************
      LOGICAL NPSF1ST
      LOGICAL UNKNOWN
      LOGICAL EXPOWGT
      
      CHARACTER*(*) MAPID
      CHARACTER*(*) SRCREGION
      
      INTEGER*4 SZX
      INTEGER*4 SZY
      INTEGER*4 ITEL
      INTEGER*4 STATUS
      
      REAL*8 XCEN
      REAL*8 YCEN
      REAL*8 ZMX
      REAL*8 ZMY
      REAL*4 MAP(SZX, SZY)
      REAL*4 XPIX
      REAL*4 YPIX
      REAL*4 RADSRC
      REAL*4 RADEEF
      REAL*4 EEFBAK
      REAL*4 SFA
      REAL*4 RADEB
      REAL*4 CNTSRC
      REAL*4 AREASRC

C*****************
C Common includes
C*****************
      INCLUDE '../include/io.inc'
      INCLUDE '../include/maxvals.inc'
      INCLUDE 'ximpsf.inc'

C*****************
C Local variables
C*****************
      character(40) DS1
      character(40) DS2

      CHARACTER*(MAX_IDSTR) WCSID

      INTEGER*4 I
      INTEGER*4 IEB1
      INTEGER*4 IEB2
      INTEGER*4 IRE1
      INTEGER*4 IRE2
      INTEGER*4 NUMREG
      INTEGER*4 SLEN1
      INTEGER*4 SLEN2
      INTEGER*4 IXWID
      INTEGER*4 IYWID

      REAL*4 RADTMP
      REAL*4 DEL
      REAL*4 AREAIMG
      REAL*4 XIMG
      REAL*4 YIMG
      REAL*4 RADIMGX
      REAL*4 RADIMGY
      REAL*4 RADACTX
      REAL*4 RADACTY
      REAL*4 EBSPAN
      REAL*4 RESPAN

      REAL*8 XBWID
      REAL*8 YBWID
      REAL*8 XMIN
      REAL*8 XMAX
      REAL*8 YMIN
      REAL*8 YMAX

C***********
C Functions
C***********
      LOGICAL ISDNULL

      STATUS = 0

C*****************************************************
C If no source region defined, do the box calculation
C*****************************************************
      IF ( SRCREGION.EQ.' ' ) THEN

C        Chat a little
         CALL XDSTR( DBLE( XPIX ), -1, DS1, SLEN1 )
         CALL XDSTR( DBLE( YPIX ), -1, DS2, SLEN2 )
         WRITE( ZWRITE, 99003 ) DS1(:SLEN1), DS2(:SLEN2)
         CALL XWRITE( ZWRITE, 10 )

C        Get the EEF as a function of radius
         CALL NPSF( MAPID, XPIX, YPIX, ITEL, SFA, -1.0, ' ', NPSF1ST,
     &              EXPOWGT, UNKNOWN, STATUS )

C        Compute EEF radius to use as source radius
         IF ( UNKNOWN ) THEN

            IF ( RADSRC.LT.0.0 ) THEN
               CALL XAERROR( ' Unknown PSF: source size must be '//
     &                       ' specified independent of PSF', 10 )
               STATUS = -1
               RETURN
            ENDIF

            RADEB = -1.0

         ELSE

            I    = 0
            IRE1 = 0
            IEB1 = 0
            DEL  = FRAC(0)
            DO WHILE ( DEL.LT.EEFBAK .AND. I.LE.MAXPSF )
               IF ( DEL.LT.RADEEF ) THEN
                  IRE1 = I
               ENDIF
               IEB1 = I
               I = I + 1
               DEL = FRAC(MIN( I, MAXPSF ))
            ENDDO
            
            IEB2   = MIN( I, MAXPSF )
            IRE2   = MIN( IRE1 + 1, MAXPSF )
            EBSPAN = FRAC(IEB2) - FRAC(IEB1)
            IF ( EBSPAN.NE.0. ) THEN
               RADEB = ( EEFBAK - FRAC(IEB1) ) / EBSPAN + FLOAT( IEB1 )
               RADEB = RADEB / SQRT( SFA )
            ELSE
               RADEB = FLOAT( IEB1 ) / SQRT( SFA )
            ENDIF

C           If RADSRC not set, use NPSF-determined value
            RESPAN = FRAC(IRE2) - FRAC(IRE1)
            IF ( RESPAN.NE.0.0 ) THEN
               RADTMP = ( RADEEF - FRAC(IRE1) ) / RESPAN + FLOAT( IRE1 )
               RADTMP = RADTMP / SQRT( SFA )
            ELSE
               RADTMP = FLOAT( IRE1 ) / SQRT( SFA )
            ENDIF
            IF ( RADSRC.LT.0.0 ) THEN
               RADSRC = MAX( RADTMP, 0.5 )
            ENDIF
            RADEEF = FRAC(MIN( MAXPSF, INT( RADSRC * SQRT( SFA ) ) ))

C           Chat a little
            WRITE( ZWRITE, 99005 ) RADEEF, RADSRC
            CALL XWRITE( ZWRITE, 10 )
            WRITE( ZWRITE,99006 ) EEFBAK, RADEB
            CALL XWRITE( ZWRITE, 10 )

         ENDIF

C        Generate a box region
         XBWID = RADSRC * 2.0D0
         YBWID = RADSRC * 2.0D0
         CALL XBOXREG( 0, DBLE( XPIX ), DBLE( YPIX ), XBWID, YBWID,
     &                 0.0D0, NUMREG, STATUS )

C        Print actual source radius used based 
C        on center and image rebin
         CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                   XPIX, YPIX, XIMG, YIMG, 2 )
         RADIMGX = RADSRC / ZMX
         RADIMGY = RADSRC / ZMY
         IXWID = INT( XIMG + RADIMGX ) - INT( XIMG - RADIMGX )
         IYWID = INT( YIMG + RADIMGY ) - INT( YIMG - RADIMGY )
         RADACTX = FLOAT( IXWID ) * ZMX / 2.0
         RADACTY = FLOAT( IYWID ) * ZMY / 2.0
         CALL XDSTR( DBLE( RADACTX ), -1, DS1, SLEN1 )
         CALL XDSTR( DBLE( RADACTY ), -1, DS2, SLEN2 )
         WRITE( ZWRITE, '(5A)' ) ' Actual radius x: ', DS1(:SLEN1),
     &                           ' y: ', DS2(:SLEN2), 
     &                           ' based on rebin and center'
         CALL XWRITE( ZWRITE, 15 )

C*****************************************************************
C If region is defined do the calculation in the specified region
C*****************************************************************
      ELSE

C        Setup the region WCS
         CALL GHEADS( MAPID, 'WCSID', WCSID, 0, STATUS )
         CALL SETREGWCS( WCSID, STATUS )

C        Parse the region file
         CALL XINITREG( SRCREGION, NUMREG, STATUS )
         IF ( STATUS.NE.0 ) THEN
            RETURN
         ENDIF

C        Determine the bounding box of the region
         CALL BBOXREG( 1, XMIN, XMAX, YMIN, YMAX, STATUS )
         IF ( STATUS.NE.0 ) THEN
            RETURN
         ENDIF

         IF ( ISDNULL( XMIN ) .OR. ISDNULL( XMAX ) .OR.
     &        ISDNULL( YMIN ) .OR. ISDNULL( YMAX ) ) THEN
            CALL XWRITE( ' ERROR: Source region is unbounded', 10 )
            STATUS = -1
            RETURN
         ENDIF

C        Calculate the region center, so we can do the psf calc
         XPIX = ( XMIN + XMAX ) / 2.0
         YPIX = ( YMIN + YMAX ) / 2.0

C        Do the psf calc
         CALL XWRITE( ' Calculate psf for optimum half-box...', 10 )
         CALL NPSF( MAPID, XPIX, YPIX, ITEL, SFA, -1.0, ' ', NPSF1ST,
     &              EXPOWGT, UNKNOWN, STATUS )

         CALL XDSTR( DBLE( XPIX ), -1, DS1, SLEN1 )
         CALL XDSTR( DBLE( YPIX ), -1, DS2, SLEN2 )
         WRITE( ZWRITE, 99004 ) DS1(:SLEN1), DS2(:SLEN2)
         CALL XWRITE( ZWRITE, 10 )

C        Use max side of bounding box for psf
         RADSRC = MAX( ABS( XMAX - XMIN ) / 2.0,
     &                 ABS( YMAX - YMIN ) / 2.0 )
         WRITE( ZWRITE, 99008 ), RADSRC
         CALL XWRITE( ZWRITE, 20 )

      ENDIF

      IF ( STATUS.NE.0 ) THEN
         RETURN
      ENDIF

      IF ( NUMREG.GT.1 ) THEN
         CALL XWRITE( ' Ignoring all but first region in list', 10 )
      ENDIF

C     Check that the maximum exposure contained within this
C     radius is .GE. 0.0
      I = RADSRC * SQRT( SFA ) - 1
      IF ( EXPOWGT .AND. EXPOMAXS(I).LE.0.0D0 ) THEN
         CALL XWRITE( ' ERROR: Exposure from expomap is <= 0.0!', 10 )
         STATUS = -1
         RETURN
      ENDIF

C     Get the counts in this region
      CALL CNTREG( 1, MAP, SZX, SZY, ZMX, ZMY, XCEN, YCEN, CNTSRC,
     &             AREAIMG, STATUS )

C     Get the source area (unzoomed)
      AREASRC = AREAIMG * ( ZMX * ZMY )

C     Chat a little more
      CALL XDSTR( DBLE( CNTSRC ), -1, DS1, SLEN1 )
      CALL XISTR( INT( AREASRC ), DS2, SLEN2 )
      WRITE( ZWRITE, 99007 ) DS1(:SLEN1), DS2(:SLEN2)
      CALL XWRITE( ZWRITE, 10 )

C******
C Done
C******
      RETURN

C********************************************
C Various formatted output FORMAT statements
C********************************************
99003 FORMAT( 20X, 'X = ', A, 4X, 'Y = ', A )
99004 FORMAT( ' Center of bounding box: X = ', A, 4X, 'Y = ', A )
99005 FORMAT( ' Source half-box for ', F4.2, ' EEF is', 
     &        F7.1, ' pixels' )
99006 FORMAT( '        Half-box for ', F4.2, ' EEF is',
     &        F7.1, ' pixels' )
99007 FORMAT( ' Total # of counts ', A, ' (in ', A, 
     &        ' elemental sq pixels)' )
99008 FORMAT( ' Using PSF radius of ', F7.1, ' pixels' )

      END
