c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C       SUBROUTINE CELGAL
C
C
C  $Id: celgal.f,v 1.2 2013/05/21 19:08:25 irby Exp $
c
C        =================================================================
C        ' Subroutine CELGAL transforms coordinates between celestial and
C        ' galactic systems.  The routine is based on equations given in
C        ' 'Handbook Of Space Astromomy And Astophysics', M. V. Zombeck,
C        ' Cambridge University Press, 1982, p.71.  A data statement
C        ' establishes the relationship between the two coordinate systems,
C        ' and this varies slowly with time due to precession of the Earth's
C        ' pole of rotation.
C        -------------------------------------------------------------------
C        ' Written 10/1/87.  D.L.Bertsch
C        ' Modifications:  9/23/91.  Added protection to ASIN for angles at
C        '                           the poles.
C        '
C        -------------------------------------------------------------------
C        ' Calling Sequence:
C        '
C        '     CALL CELGAL( CODE, RTASN, DECLN, GLON, GLAT, IRET)
C        '
C        ' Argument List:
C        '
C        '        CODE    CHAR*2  INPUT   Controls the direction of the trans-
C        '                                formation.  CODE = 'CG' (or 'cg')
C        '                                transforms from celestial to
C        '                                galactic and CODE = 'GC' (or 'gc')
C        '                                transforms from galactic to
C        '                                celestial.  Other values of CODE
C        '                                produce a return code error with no
C        '                                transformation calculations.
C        '        RTASN   R*4     I/O     Celestial right ascension in
C        '                                radians.  Range: 0 to 2pi.
C        '        DECLN   R*4     I/O     Celestial declination north of the
C        '                                equatorial plane in radians.
C        '                                Range: -pi/2 to pi/2.
C        '        GLON    R*4     I/O     Galactic longitude in radians.
C        '                                Range: 0 to 2pi.
C        '        GLAT    R*4     I/O     Galactic latitude in radians.
C        '                                Range: -pi/2 to pi/2.
C        '        IRET    I*4     OUTPUT  Return code.  If IRET = 0, the
C        '                                transformation was done.  If IRET
C        '                                > 0, no evaluation was done since
C        '                                CODE value was incorrectly given.
C        =====================================================================
C        '
C        ' Significant Variables Used Locally:
C        '
C        '     FIRST    A flag used to initalize parameters with the first
C        '              execution.
C        '     NGPRA    North galactic pole right ascension in degrees.
C        '     NGPDC    North galactic pole declination in degrees.
C        '     TWOPI    Two times pi.
C        '     CONV     Conversion factor from degrees to radians.
C        '     SCPDC    Sine of the complement of the galactic pole declination.
C        '     CCPDC    Cosine of the complement of the galactic pole
C        '              declination.
C        '     GLON0    Offset to the galactic longitude.
C        '     RTASN0   Offset to the celestial right ascension.
C        '     SDRA     Sine of the right ascension less its offset.
C        '     CDRA     Cosine of the right ascension less its offset.
C        '     SDEC     Sine of the declination.
C        '     CDEC     Cosine of the declination.
C        '     SDLON    Sine of the galactic longitude less its offset.
C        '     CDLON    Cosine of the galactic longitude less its offset.
C        '     SLAT     Sine of the galactic latitude.
C        '     CLAT     Cosine of the galactic latitude.
C        ======================================================================
c
C
C
C  $Log: celgal.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:28  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  18:57:27  jae
c added COMMON cnfrep.copy and line for LOC
c
c Revision 5.1  1996/02/29  20:47:02  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:10  jae
c Subroutine Module for like V5.00
c
C
c
      SUBROUTINE CELGAL(CODE,RTASN,DECLN,GLON,GLAT,IRET)
c
c
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      character(80) id
      common /id/id
      LOGICAL*1    FIRST
      character(2)  CODE
      INTEGER*4    IRET
      REAL       RTASN, DECLN, GLON, GLAT, NGPRA, NGPDC, GALXDC
c
      save

      DATA FIRST/.TRUE./, NGPRA/192.8592/, NGPDC/27.12832/,
     &     GALXDC/-28.93643/
c
C==>      In this data statement, the north galactic pole right ascension
C     and declination and the declination of the galactic X axis are
C     specified for eopch 2000.0 in degrees.  Epoch 1950.0 values for
C     these parameters would be 192.25, 27.4, and -28.917.
c
c
C==>      Initialization
c
      id = '$Id: celgal.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='CELGAL'

      if (jae_celgal)write(*,'("In routine ",a)') LOC

      IF (FIRST) THEN
         FIRST = .FALSE.
         TWOPI = 8.0 * ATAN (1.0)
         CONV = TWOPI / 360.0
         SCPDC = SIN (CONV * (90.0 - NGPDC) )
         CCPDC = COS (CONV * (90.0 - NGPDC) )
         arg = SIN (CONV * GALXDC) / SCPDC
         GLON0 = -ASIN (min(1.0,max(-1.0,arg)))
         RTASN0 = CONV * (90.0 + NGPRA)
      ENDIF
c
C==>       Transformations
c
      IF (CODE .EQ. 'CG' .OR. CODE .EQ. 'cg' ) THEN
c
C     Celestial to galactic transformation.  RTASN and DECLN are
C     input and GLON and GLAT are output.
c
         IRET = 0
c
         SDRA = SIN( RTASN - RTASN0 )
         CDRA = COS( RTASN - RTASN0 )
         SDEC = SIN( DECLN )
         CDEC = COS( DECLN )
c
         arg = SDEC*CCPDC - CDEC*SDRA*SCPDC
         GLAT = ASIN( min(1.0,max(-1.0,arg)) )
         GLON = GLON0 + ATAN2( (CDEC*SDRA*CCPDC + SDEC*SCPDC) ,
     &        CDEC*CDRA )
c
         IF (GLON.LT.0.0 )   GLON = GLON + TWOPI
         IF (GLON.GT.TWOPI ) GLON = GLON - TWOPI
c
         RETURN
c
      ELSE IF (CODE .EQ. 'GC' .OR. CODE .EQ. 'gc') THEN
c     
C     Galactic to celestial transformation.  GLON and GLAT are
C     input and RTASN and DECLN are output.
c
         IRET = 0
         SDLON = SIN( GLON - GLON0 )
         CDLON = COS( GLON - GLON0 )
         SLAT  = SIN( GLAT )
         CLAT  = COS( GLAT )
c
         DECLN = ASIN( CLAT*SDLON*SCPDC + SLAT*CCPDC )
         RTASN = RTASN0 + ATAN2( (CLAT*SDLON*CCPDC - SLAT*SCPDC) ,
     &        CLAT*CDLON )
c
         IF ( RTASN.LT.0.0 )   RTASN = RTASN + TWOPI
         IF ( RTASN.GT.TWOPI ) RTASN = RTASN - TWOPI
c
         RETURN
      ELSE
c
C==>       Error in input.
c
         IRET = 1
      ENDIF
c
      RETURN
      END
c
