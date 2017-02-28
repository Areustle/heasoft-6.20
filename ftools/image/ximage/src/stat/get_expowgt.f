      SUBROUTINE GET_EXPOWGT( EXMAP, SZX, SZY, EXMAPID, XPIX, YPIX,
     &                        RADIUS, EXPOWGT, WGTCALCD, STATUS )

C**********************************************************************
C
C  Calculates the mean exposure weight at successive radii 
C  (in elemental pixels) from a central pixel. Also calculates
C  the maximum exposure contained within a each radius.
C
C  NOTE: The exposure weights must be normalized to the maximum
C        exposure contained within the radius of interest. This is 
C        NOT done here, since we don't know the radius of interest.
C  
C  Parameters:
C
C  I  EXMAP     (R)  Exposure map
C  I  SZX       (I)  X size of exposure map
C  I  SZY       (I)  Y size of exposure map
C  I  EXMAPID   (S)  Exposure Map ID string
C  I  X/YPIX    (R)  Source position (detector pixels)
C  I  RADIUS    (I)  Radius (detector pixels) to use
C I/O EXPOWGT   (D)  Un-normalized exposure weight for RADIUS
C I/O WGTCALCD  (L)  Have we already calc'd the weights?
C  O  STATUS    (I)  Error flag (0=OK)
C
C**********************************************************************

      IMPLICIT NONE

C***********
C Functions
C***********
      LOGICAL ISRNULL

C*****************
C Input arguments
C*****************
      LOGICAL WGTCALCD

      CHARACTER*(*) EXMAPID

      INTEGER*4 SZX
      INTEGER*4 SZY
      INTEGER*4 RADIUS
      INTEGER*4 STATUS

      REAL*4 XPIX
      REAL*4 YPIX

      REAL*4 EXMAP(SZX, SZY)

      REAL*8 EXPOWGT
      REAL*8 EXPOMAX

C***************
C Include files
C***************
      INCLUDE '../include/maxvals.inc'
      INCLUDE '../include/startup.inc'
      INCLUDE '../include/sitedef.inc'
      INCLUDE '../include/io.inc'
      INCLUDE 'ximpsf.inc'

C*****************
C Local variables
C*****************

      INTEGER*4 I
      INTEGER*4 J
      INTEGER*4 ILO
      INTEGER*4 IHI
      INTEGER*4 JLO
      INTEGER*4 JHI
      INTEGER*4 IIMG
      INTEGER*4 JIMG
      INTEGER*4 IDUMMY
      INTEGER*4 IDIST

      INTEGER*4 NUMPIX(0:MAXPSF)

      REAL*4 IPIX
      REAL*4 JPIX
      REAL*4 ILOR
      REAL*4 IHIR
      REAL*4 JLOR
      REAL*4 JHIR
      REAL*4 IIMGR
      REAL*4 JIMGR
      REAL*4 DIST
      REAL*4 RDUM1
      REAL*4 RDUM2
      REAL*4 RDUM3

      REAL*8 XCEN
      REAL*8 YCEN
      REAL*8 ZMX
      REAL*8 ZMY

C*****************
C Saved variables
C*****************

C The un-normalized exposure weights as a function of radius
      REAL*8 EXPOWGTS(0:MAXPSF)

      SAVE EXPOWGTS

C*****************************************************
C If we've already calculated the weight for this map
C just give back what we know
C*****************************************************
      IF ( WGTCALCD ) THEN
         EXPOWGT = EXPOWGTS(RADIUS)
         RETURN
      ENDIF

C************************************
C Get the map center and zoom if any
C************************************
      CALL GET_REFRAM( EXMAPID, IDUMMY, IDUMMY, 
     &                 ZMX, ZMY, XCEN, YCEN, STATUS )

      IF ( ZMX.NE.ZMY ) THEN
         WRITE( ZWRITE, 99000 ) ZMX, ZMY
         CALL XWARN( ZWRITE, 10 )
         WRITE( ZWRITE, * ) 'Exposure correction results '//
     &                      'may be suspect'
         CALL XWARN( ZWRITE, 10 )
      ENDIF

C----------------------------------------------------------------------
C***************************
C Do the weight calculation
C***************************

C************
C Initialize
C************
      EXPOMAX = -1.0D29

      RDUM1 = 0.0
      RDUM2 = 0.0
      RDUM3 = 0.0

      DO I = 0, MAXPSF
         EXPOWGTS(I) = 0.0
         EXPOMAXS(I) = 0.0D0
         NUMPIX(I)   = 0
      ENDDO

C******************************************************************
C Calculate detector coords box to loop over (box radius = MAXPSF)
C This can cause the weight calculation to take a long time if the
C image binning is very high, as is the case for XMM's default
C binning (80)
C******************************************************************
      ILOR = XPIX - MAXPSF - 1.0
      IHIR = XPIX + MAXPSF + 1.0
      JLOR = YPIX - MAXPSF - 1.0
      JHIR = YPIX + MAXPSF + 1.0

      ILO = INT( ILOR )
      IHI = NINT( IHIR )
      JLO = INT( JLOR )
      JHI = NINT( JHIR )

C     Check the lower bounds versus the image boundary
      CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                ILOR, JLOR, IIMGR, JIMGR, 2 )

      IF ( IIMGR.LT.1.0 ) THEN
         RDUM2 = 0.0
         RDUM3 = 0.0
         CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                   IPIX, RDUM1, RDUM2, RDUM3, 1 )

         ILO = INT( IPIX )
         ILO = MAX( ILO, 1 )
      ENDIF

      IF ( JIMGR.LT.1.0 ) THEN
         RDUM2 = 0.0
         RDUM3 = 0.0
         CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                   RDUM1, JPIX, RDUM2, RDUM3, 1 )

         JLO = INT( JPIX )
         JLO = MAX( JLO, 1 )
      ENDIF

C     Check the upper bounds versus the image boundary
      CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                IHIR, JHIR, IIMGR, JIMGR, 2 )

      IF ( IIMGR.GT.FLOAT( SZX ) ) THEN
         CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN, IPIX,
     &                   RDUM1, FLOAT( SZX ), FLOAT( SZY ), 1 )

         IHI = INT( IPIX )
         IHI = MIN( IHI, INT( SZX * ZMX ) )
      ENDIF

      IF ( JIMGR.GT.FLOAT( SZY ) ) THEN
         CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN, RDUM1,
     &                   JPIX, FLOAT( SZX ), FLOAT( SZY ), 1 )

         JHI = INT( JPIX )
         JHI = MIN( JHI, INT( SZY * ZMY ) )
      ENDIF

C***************************
C Loop over detector pixels 
C***************************
      DO I = ILO, IHI

         DO J = JLO, JHI

C           Calculate the distance from the input coordinates
C           Do the calculation in detector coordinates to match FRAC()
            IPIX = FLOAT( I )
            JPIX = FLOAT( J )
            
            DIST = SQRT( ( XPIX - IPIX )**2 + ( YPIX - JPIX )**2 )

C           Get the index into the FRACW array
            IDIST = INT( DIST )

C           Convert to image pixels
            CALL CALIMGPIX( SZX, SZY, ZMX, ZMY, XCEN, YCEN,
     &                      IPIX, JPIX, IIMGR, JIMGR, 2 )
            IIMG = INT( IIMGR )
            JIMG = INT( JIMGR )

C           Are we within the allowed range for FRACW?
            IF ( IDIST.LE.MAXPSF ) THEN

C              Do we have a map value here?
               IF ( IIMG.GE.1 .AND. IIMG.LE.SZX .AND.
     &              JIMG.GE.1 .AND. IIMG.LE.SZY .AND.
     &              .NOT.ISRNULL( EXMAP(IIMG, JIMG) ) ) THEN

C                 Add to the corresponding bin
                  EXPOWGTS(IDIST) = EXPOWGTS(IDIST) +
     &                              DBLE( EXMAP(IIMG, JIMG) )

C                 Count the pixels in this bin
                  NUMPIX(IDIST) = NUMPIX(IDIST) + 1

C                 Get the maximum exposure
                  EXPOMAX = MAX( EXPOMAX, DBLE( EXMAP(IIMG, JIMG) ) )

C                 Get the maximum exposure at this radius
                  EXPOMAXS(IDIST) = MAX( EXPOMAXS(IDIST),
     &                                   DBLE( EXMAP(IIMG, JIMG) ) )
               ENDIF
            ENDIF
         ENDDO
      ENDDO

      WRITE( ZWRITE, * ) ' Maximum exposure = ', EXPOMAX, ' s'
      CALL XWRITE( ZWRITE, 15 )

C***********************************************************
C Get the average exposure per pixel at each radius
C And get the maximum exposure contained within each radius
C***********************************************************
      DO I = 0, MAXPSF

C        Get max exposure up to this radius
         DO J = 0, I
            IF ( EXPOMAXS(J).GT.EXPOMAXS(I) ) THEN
               EXPOMAXS(I) = EXPOMAXS(J)
            ENDIF
         ENDDO

C        Get the mean exposure for this radius
         IF ( NUMPIX(I).GT.0 ) THEN
            EXPOWGTS(I) = EXPOWGTS(I) / DBLE( NUMPIX(I) )
         ELSE
            EXPOWGTS(I) = 0.0
         ENDIF

C        Debugging output
         WRITE( ZWRITE, 99010 ) I, EXPOWGTS(I)
         CALL XWRITE( ZWRITE, 50 )

      ENDDO

C***************************
C END of weight calculation
C***************************

C----------------------------------------------------------------------

C*************************
C Give back what we found
C*************************
      EXPOWGT = EXPOWGTS(RADIUS)

      WGTCALCD = .TRUE.

      RETURN

C********************************************
C Various formatted output FORMAT statements
C********************************************
99000 FORMAT( 'Mismatch in X and Y zoom factors: ', 
     &        F5.0, ' and ', F5.0 )
99010 FORMAT( ' Exposure weight at R = ', I8, ' (pixels): ', F14.6 )

      END

