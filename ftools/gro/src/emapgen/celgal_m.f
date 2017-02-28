C***********************************************************************
C   SUBROUTINE CELGAL_M
C $Id: celgal_m.f,v 1.3 2013/05/21 19:08:24 irby Exp $
C***********************************************************************
C=================================================================
C| Subroutine CELGAL_M transforms coordinates between celestial and
C| galactic systems.  The routine is based on equations given in
C| "Handbook of Space Astromomy and Astophysics", M. V. Zombeck,
C| Cambridge University Press, 1982, p.71.  A DATA statement
C| establishes the relationship between the two coordinate systems,
C| and this varies slowly with time due to precession of the earth's
C| pole of rotation.
C-------------------------------------------------------------------
C| WRITTEN 10/1/87.  D.L.BERTSCH
C| MODIFICATIONS:
C|	E.S.Panduranga	STX	06/21/91
C|		Moved code from IBM to the SUN.
C|		Stripped off line numbers and trailing blanks.
C|		Declared variables not declared on the IBM.
C|
C| $Log: celgal_m.f,v $
C| Revision 1.3  2013/05/21 19:08:24  irby
C| Change character*n to character(n) to silence warnings: "Obsolescent
C| feature: Old-style character length".
C|
C| Revision 1.2  1998/09/30 18:24:17  peachey
C| Changed addbin to addbin_m and celgal to celgal_m
C|
c Revision 1.1  1998/09/30  17:01:03  peachey
c New tool, delivered by Jeff Silvis
c
C Revision 2.1  1991/10/01  14:12:21  esp
C First controlled version on the Sun.
C
C-------------------------------------------------------------------
C| CALLING SEQUENCE:
C|
C|     CALL CELGAL_M( CODE, RTASN, DECLN, GLON, GLAT, IRET)
C|
C| ARGUMENT LIST:
C|
C|          CODE    CHAR*2  INPUT   CONTROLS THE DIRECTION OF THE TRANS-
C|                                  FORMATION.  CODE = 'CG' (OR 'CG')
C|                                  TRANSFORMS FROM CELESTIAL TO
C|                                  GALACTIC AND CODE = 'GC' (OR 'GC')
C|                                  TRANSFORMS FROM GALACTIC TO
C|                                  CELESTIAL.  OTHER VALUES OF CODE
C|                                  PRODUCE A RETURN CODE ERROR WITH NO
C|                                  TRANSFORMATION CALCULATIONS.
C|          RTASN   R*4     I/O     CELESTIAL RIGHT ASCENSION IN
C|                                  RADIANS.  RANGE: 0 TO 2PI.
C|          DECLN   R*4     I/O     CELESTIAL DECLINATION NORTH OF THE
C|                                  EQUATORIAL PLANE IN RADIANS.
C|                                  RANGE: -PI/2 TO PI/2.
C|          GLON    R*4     I/O     GALACTIC LONGITUDE IN RADIANS.
C|                                  RANGE: 0 TO 2PI.
C|          GLAT    R*4     I/O     GALACTIC LATITUDE IN RADIANS.
C|                                  RANGE: -PI/2 TO PI/2.
C|          IRET    I*4     OUTPUT  RETURN CODE.  IF IRET = 0, THE
C|                                  TRANSFORMATION WAS DONE.  IF IRET
C|                                  > 0, NO EVALUATION WAS DONE SINCE
C|                                  CODE VALUE WAS INCORRECTLY GIVEN.
C=====================================================================
C|
C| SIGNIFICANT VARIABLES USED LOCALLY:
C|
C|     FIRST    A FLAG USED TO INITALIZE PARAMETERS WITH THE FIRST
C|              EXECUTION.
C|     NGPRA    NORTH GALACTIC POLE RIGHT ASCENSION IN DEGREES.
C|     NGPDC    NORTH GALACTIC POLE DECLINATION IN DEGREES.
C|     TWOPI    TWO TIMES PI.
C|     CONV     CONVERSION FACTOR FROM DEGREES TO RADIANS.
C|     SCPDC    SINE OF THE COMPLEMENT OF THE GALACTIC POLE DECLINATION.
C|     CCPDC    COSINE OF THE COMPLEMENT OF THE GALACTIC POLE
C|              DECLINATION.
C|     GLON0    OFFSET TO THE GALACTIC LONGITUDE.
C|     RTASN0   OFFSET TO THE CELESTIAL RIGHT ASCENSION.
C|     SDRA     SINE OF THE RIGHT ASCENSION LESS ITS OFFSET.
C|     CDRA     COSINE OF THE RIGHT ASCENSION LESS ITS OFFSET.
C|     SDEC     SINE OF THE DECLINATION.
C|     CDEC     COSINE OF THE DECLINATION.
C|     SDLON    SINE OF THE GALACTIC LONGITUDE LESS ITS OFFSET.
C|     CDLON    COSINE OF THE GALACTIC LONGITUDE LESS ITS OFFSET.
C|     SLAT     SINE OF THE GALACTIC LATITUDE.
C|     CLAT     COSINE OF THE GALACTIC LATITUDE.
C======================================================================


      SUBROUTINE CELGAL_M(CODE,RTASN,DECLN,GLON,GLAT,IRET)
      implicit none 

 
      LOGICAL      FIRST
      CHARACTER    CODE*2
      INTEGER*4    IRET
      REAL*4       RTASN, DECLN, GLON, GLAT, NGPRA, NGPDC, GALXDC

      DATA FIRST/.TRUE./, NGPRA/192.8592/, NGPDC/27.12832/,
     &     GALXDC/-28.93643/

C==>      IN THIS DATA STATEMENT, THE NORTH GALACTIC POLE RIGHT ASCENSIO
C         AND DECLINATION AND THE DECLINATION OF THE GALACTIC X AXIS ARE
C         SPECIFIED FOR EOPCH 2000.0 IN DEGREES.  EPOCH 1950.0 VALUES FO
C         THESE PARAMETERS WOULD BE 192.25, 27.4, AND -28.917.

Cesp  ! Declaring variables not declared on IBM !
      real	twopi, scpdc, conv, ccpdc, glon0, sdra, rtasn0, cdra
      real	sdec, cdec, sdlon, cdlon, slat, clat

      character(80)	id
      common	/id/id
      id = '$Id: celgal_m.f,v 1.3 2013/05/21 19:08:24 irby Exp $'

C==>  INITIALIZATION
      IF (FIRST) THEN
          FIRST = .FALSE.
          TWOPI = 8.0 * ATAN (1.0)
          CONV = TWOPI / 360.0
          SCPDC = SIN (CONV * (90.0 - NGPDC) )
          CCPDC = COS (CONV * (90.0 - NGPDC) )

          GLON0 = -ASIN (SIN (CONV * GALXDC) / SCPDC)
          RTASN0 = CONV * (90.0 + NGPRA)
      ENDIF

C==>       TRANSFORMATIONS

      IF (CODE .EQ. 'CG') THEN

C          CELESTIAL TO GALACTIC TRANSFORMATION.  RTASN AND DECLN ARE
C          INPUT AND GLON AND GLAT ARE OUTPUT.

           IRET = 0
           SDRA = SIN( RTASN - RTASN0 )
           CDRA = COS( RTASN - RTASN0 )
           SDEC = SIN( DECLN )
           CDEC = COS( DECLN )

           GLAT = ASIN( SDEC*CCPDC - CDEC*SDRA*SCPDC )
           GLON = GLON0 + ATAN2( (CDEC*SDRA*CCPDC + SDEC*SCPDC) ,
     &                           CDEC*CDRA )

           IF (GLON.LT.0.0 )   GLON = GLON + TWOPI
           IF (GLON.GT.TWOPI ) GLON = GLON - TWOPI

      ELSE IF (CODE .EQ. 'GC') THEN

C          GALACTIC TO CELESTIAL TRANSFORMATION.  GLON AND GLAT ARE
C          INPUT AND RTASN AND DECLN ARE OUTPUT.

           IRET = 0
           SDLON = SIN( GLON - GLON0 )
           CDLON = COS( GLON - GLON0 )
           SLAT  = SIN( GLAT )
           CLAT  = COS( GLAT )

           DECLN = ASIN( CLAT*SDLON*SCPDC + SLAT*CCPDC )
           RTASN = RTASN0 + ATAN2( (CLAT*SDLON*CCPDC - SLAT*SCPDC) ,
     &                             CLAT*CDLON )

           IF( RTASN.LT.0.0 )   RTASN = RTASN + TWOPI
           IF( RTASN.GT.TWOPI ) RTASN = RTASN - TWOPI

      ELSE

C==>       ERROR IN INPUT.

           IRET = 1

      ENDIF

      RETURN
C*************************** END CELGAL_M ********************************
      END
