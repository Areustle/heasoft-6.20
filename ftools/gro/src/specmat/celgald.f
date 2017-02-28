C        =================================================================
C        | Subroutine CELGALd transforms coordinates between celestial and
C        | galactic systems.  The routine is based on equations given in
C        | "Handbook Of Space Astromomy And Astophysics", M. V. Zombeck,
C        | Cambridge University Press, 1982, p.71.  A data statement
C        | establishes the relationship between the two coordinate systems,
C        | and this varies slowly with time due to precession of the Earth's
C        | pole of rotation.
C        -------------------------------------------------------------------
C        | Written 10/1/87.  D.L.Bertsch
C        | Modifications:
C        |  This version for degrees instead of radians by P.L.Nolan 1991
C        |   @(#) celgald.f 1.2@(#)
C        -------------------------------------------------------------------
C        | Calling Sequence:
C        |
C        |     CALL CELGALd( CODE, RTASN, DECLN, GLON, GLAT, IRET)
C        |
C        | Argument List:
C        |   
C        |          CODE    CHAR*2  INPUT   Controls the direction of trans-
C        |                                  formation.  CODE = 'CG' (or 'cg')
C        |                                  transforms from celestial to
C        |                                  galactic and CODE = 'GC' (or 'gc')
C        |                                  transforms from galactic to
C        |                                  celestial.  Other values of CODE
C        |                                  produce a return code error with no
C        |                                  transformation calculations.
C        |          RTASN   R*4     I/O     Celestial right ascension in
C        |                                  degrees.  Range: 0 to 360.
C        |          DECLN   R*4     I/O     Celestial declination north of the
C        |                                  equatorial plane in degrees.
C        |                                  Range: -90 to 90.
C        |          GLON    R*4     I/O     Galactic longitude in degrees.
C        |                                  Range: 0 to 360.
C        |          GLAT    R*4     I/O     Galactic latitude in degrees.
C        |                                  Range: -90 to 90.
C        |          IRET    I*4     OUTPUT  Return code.  If IRET = 0, the
C        |                                  transformation was done.  If IRET
C        |                                  > 0, no evaluation was done since 
C        |                                  CODE value was incorrectly given.
C        =====================================================================
C        | 
C        | Significant Variables Used Locally: 
C        |
C        |     FIRST    A flag used to initalize parameters with the first
C        |              execution.
C        |     NGPRA    North galactic pole right ascension in degrees.
C        |     NGPDC    North galactic pole declination in degrees.
C        |     SCPDC    Sine of the complement of the galactic pole decl.
C        |     CCPDC    Cosine of the complement of the galactic pole
C        |              declination.
C        |     GLON0    Offset to the galactic longitude.
C        |     RTASN0   Offset to the celestial right ascension.
C        |     SDRA     Sine of the right ascension less its offset.
C        |     CDRA     Cosine of the right ascension less its offset.
C        |     SDEC     Sine of the declination.
C        |     CDEC     Cosine of the declination.
C        |     SDLON    Sine of the galactic longitude less its offset.
C        |     CDLON    Cosine of the galactic longitude less its offset.
C        |     SLAT     Sine of the galactic latitude.
C        |     CLAT     Cosine of the galactic latitude.
C        ======================================================================
 
 
      SUBROUTINE CELGALd(CODE,RTASN,DECLN,GLON,GLAT,IRET)
 
 
      implicit     none
      LOGICAL*1    FIRST
      character(2)  CODE
      INTEGER*4    IRET
      REAL*4       RTASN, DECLN, GLON, GLAT, NGPRA, NGPDC, GALXDC
      REAL*4       SCPDC, CCPDC, GLON0, RTASN0, SDRA, CDRA, SDEC, CDEC
      REAL*4       SDLON, CDLON, SLAT, CLAT
      real         sind, cosd, todeg

      EXTERNAL     sind, cosd

      save
 
      DATA FIRST/.TRUE./, NGPRA/192.8592/, NGPDC/27.12832/,
     &     GALXDC/-28.93643/

C==>      In this data statement, the north galactic pole right ascension
C         and declination and the declination of the galactic X axis are
C         specified for eopch 2000.0 in degrees.  Epoch 1950.0 values for 
C         these parameters would be 192.25, 27.4, and -28.917.
 
 
C==>      Initialization

      IF (FIRST) THEN
         todeg = 45.0/atan(1.0)
         FIRST = .FALSE.
         SCPDC = SINd (90.0 - NGPDC)
         CCPDC = COSd (90.0 - NGPDC)
         
         GLON0 = -todeg*ASIN(SINd (GALXDC) / SCPDC)
         RTASN0 = 90.0 + NGPRA
      ENDIF

C==>       Transformations

      IF (CODE .EQ. 'CG' .OR. CODE .EQ. 'cg' ) THEN

C     Celestial to galactic transformation.  RTASN and DECLN are
C     input and GLON and GLAT are output.

         IRET = 0
         SDRA = SINd( RTASN - RTASN0 )
         CDRA = COSd( RTASN - RTASN0 )
         SDEC = SINd( DECLN )
         CDEC = COSd( DECLN )

         GLAT = todeg*ASIN( SDEC*CCPDC - CDEC*SDRA*SCPDC )
         GLON = GLON0 + todeg*ATAN2( (CDEC*SDRA*CCPDC + SDEC*SCPDC) ,
     &        CDEC*CDRA )

         IF (GLON.LT.0.0 )   GLON = GLON + 360.0
         IF (GLON.GT.360.0 ) GLON = GLON - 360.0

         RETURN

      ELSE IF (CODE .EQ. 'GC' .OR. CODE .EQ. 'gc') THEN

C     Galactic to celestial transformation.  GLON and GLAT are
C     input and RTASN and DECLN are output.

         IRET = 0
         SDLON = SINd( GLON - GLON0 )
         CDLON = COSd( GLON - GLON0 )
         SLAT  = SINd( GLAT )
         CLAT  = COSd( GLAT )

         DECLN = todeg*ASIN( CLAT*SDLON*SCPDC + SLAT*CCPDC )
         RTASN = RTASN0 + todeg*ATAN2( (CLAT*SDLON*CCPDC - 
     &        SLAT*SCPDC) ,CLAT*CDLON )

         IF( RTASN.LT.0.0 )   RTASN = RTASN + 360.0
         IF( RTASN.GT.360.0 ) RTASN = RTASN - 360.0
         RETURN

      ELSE

C==>       Error in input.
         
         IRET = 1

      ENDIF

      RETURN
      END


