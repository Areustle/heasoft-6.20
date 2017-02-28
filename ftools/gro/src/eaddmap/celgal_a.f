C***********************************************************************
C   SUBROUTINE CELGAL_A
C $Id: celgal_a.f,v 1.5 1998/09/30 18:22:03 peachey Exp $
C***********************************************************************
C=================================================================
c| Subroutine CELGAL_A transforms coordinates between celestial and
c| galactic systems.  The routine is based on equations given in
c| "Handbook of Space Astromomy and Astophysics", M. V. Zombeck,
c| Cambridge University Press, 1982, p.71.  A DATA statement
c| establishes the relationship between the two coordinate systems,
c| and this varies slowly with time due to precession of the earth's
c| pole of rotation.
C-------------------------------------------------------------------
C| WRITTEN 10/1/87.  D.L.BERTSCH
C|
C| $Log: celgal_a.f,v $
C| Revision 1.5  1998/09/30 18:22:03  peachey
C| Changed addbin to addbin_a and celgal to celgal_a
C|
c Revision 1.4  1997/11/07  22:23:50  silvis
c There was a single line that was too long
c
C| Revision 1.3  1997/09/18 19:38:10  silvis
C| A large number of changes were made to the code to make it compatible
C| with g77.  Most of these changes involved shorting certain lines.
C|
C| Jeff Silvis
C| HSTX
C| 18 Sept. 1997
C|
C| Revision 1.2  1997/09/05 19:58:59  silvis
C| Several changes were made to the above code to make it run on Linux.  It
C| still does not run on Linux but I wanted to archive the code and test it on
C| solaris and sun to confirm that it still runs there.
C|
C|
C| Jeff Silvis
C|
C| Revision 1.1  1997/09/03 20:15:53  silvis
C| This is the intial input to CVS of the ftool eaddmap.  This Ftool will add
C| two EGRET maps.
C|
C| Jeff Silvis
C| 3 Sept 1997
C| Hughes STX
C|
c Revision 1.2  1993/05/24  13:53:08  albert
c Changed reals to double precision for increased accuracy.
c
c Revision 1.1  1992/01/29  19:54:11  albert
c Initial revision
c
C-------------------------------------------------------------------
C| CALLING SEQUENCE:
C|
C|     CALL CELGAL_A( CODE, RTASN, DECLN, GLON, GLAT, IRET)
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
C|                                  RADIANS.  RANGE: -PI TO PI.
C|          DECLN   R*4     I/O     CELESTIAL DECLINATION NORTH OF THE
C|                                  EQUATORIAL PLANE IN RADIANS.
C|                                  RANGE: -PI/2 TO PI/2.
C|          GLON    R*4     I/O     GALACTIC LONGITUDE IN RADIANS.
C|                                  RANGE: -PI TO PI.
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


      SUBROUTINE CELGAL_A(CODE,RTASN,DECLN,GLON,GLAT,IRET)
      implicit none 

 
      LOGICAL      FIRST
      CHARACTER    CODE*2
      INTEGER*4    IRET
      DOUBLE PRECISION RTASN, DECLN, GLON, GLAT

Cesp  ! Declaring variables not declared on IBM !
c      real	twopi, scpdc, conv, ccpdc, glon0, sdra, rtasn0, cdra
c      real	sdec, cdec, sdlon, cdlon, slat, clat, NGPRA, NGPDC, GALXDC
      double precision 	twopi,scpdc,conv,ccpdc,glon0,sdra,rtasn0,cdra
      double precision 	sdec,cdec,sdlon,cdlon,slat,clat,NGPRA,NGPDC,
     &                 GALXDC

      DATA FIRST/.TRUE./, NGPRA/192.8592/, NGPDC/27.12832/,
     &     GALXDC/-28.93643/

C==>      IN THIS DATA STATEMENT, THE NORTH GALACTIC POLE RIGHT ASCENSIO
C         AND DECLINATION AND THE DECLINATION OF THE GALACTIC X AXIS ARE
C         SPECIFIED FOR EOPCH 2000.0 IN DEGREES.  EPOCH 1950.0 VALUES FO
C         THESE PARAMETERS WOULD BE 192.25, 27.4, AND -28.917.


       character        id*80
      common /id/id
      id= '$Id: celgal_a.f,v 1.5 1998/09/30 18:22:03 peachey Exp $'

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

           IF (GLON.GT.TWOPI/2.0 ) GLON = GLON - TWOPI

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

           IF( RTASN.GT.TWOPI/2.0 ) RTASN = RTASN - TWOPI

      ELSE

C==>       ERROR IN INPUT.

           IRET = 1

      ENDIF

      RETURN
C*************************** END CELGAL_A ********************************
      END
