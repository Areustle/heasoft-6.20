      SUBROUTINE SOSTA_MISC( MAPID, XPIX, YPIX, ITEL, VIGNAPP, CNTSRC,
     &                       AREASRC, RADSRC, SFA, EXPOSURE, DTIME,
     &                       BACKGD, SOURC, ERRSRC, EXPOWGT, UNKNOWN,
     &                       IFRAT, PROBA, PROBGA, VCOR, FRACC, TSIG,
     &                       VIGN1ST, P_VIGN )
C**********************************************************************
C Compute miscellaneous values for sosta: probablities, upper limits,
C vignetting, optimum box size..
C
C  I  MAPID     (S)  Map id string
C  I  X/YPIX    (R)  Source box center
C  I  ITEL      (I)  Telescope index
C  I  VIGNAPP   (L)  Whether vignetting has been applied (exposure map)
C  I  CNTSRC    (R)  Sum of weighted source counts
C  I  AREASRC   (R)  Number of image pixels summed
C I/O RADSRC    (R)  Source box radius
C  I  SFA       (R)  ?Area
C  I  EXPOSURE  (D)  Exposure time
C  I  DTIME     (D)  Dead time correction
C  I  BACKGD    (R)  Computed background 
C  I  SOURC     (R)  Source value
C  I  ERRSRC    (R)  Source error
C  I  EXPOWGT   (L)  Do exposure weighting?
C  I  UNKNOWN   (L)  Whether PSF is unknown
C  O  IFRAT     (I)  Ratio flag
C  O  PROBA     (R)  Probability that source is fluctuation
C  O  PROBGA    (R)  Probability that source is fluctuation (cumga)
C  O  VCOR      (R)  Vignetting correction
C  O  FRACC     (R)  EEF value
C  O  TSIG      (R)  Three sigma upper limit
C I/O VIGN1ST   (L)  Set to true before sosta loop, so get_vign
C                    knows when to do one-time vignetting read
C I/O P_VIGN    (I)  Pointer to vignetting correction map
C**********************************************************************
      IMPLICIT NONE

C*****************
C Input variables
C*****************
      LOGICAL EXPOWGT
      LOGICAL UNKNOWN
      LOGICAL VIGNAPP
      LOGICAL VIGN1ST
      
      CHARACTER*(*) MAPID

      INTEGER P_VIGN

      INTEGER*4 ITEL
      INTEGER*4 IFRAT

      REAL*4 XPIX
      REAL*4 YPIX
      REAL*4 CNTSRC
      REAL*4 AREASRC
      REAL*4 RADSRC
      REAL*4 SFA
      REAL*4 BACKGD
      REAL*4 SOURC
      REAL*4 ERRSRC
      REAL*4 PROBA
      REAL*4 PROBGA
      REAL*4 VCOR
      REAL*4 FRACC
      REAL*4 TSIG

      REAL*8 EXPOSURE
      REAL*8 DTIME

C***************
C Include files
C***************
      INCLUDE 'ximpsf.inc'

C*****************
C Local variables
C*****************
      INTEGER*4 ICNTS
      INTEGER*4 III
      INTEGER*4 OPIXBOX
      INTEGER*4 STATUS

      REAL*4 RAT
      REAL*4 ZZ
      REAL*4 BBBA
      REAL*4 RES
      REAL*4 CNTBG

C***********
C Functions
C***********
      REAL*4 XPOLOG

C************
C Initialize
C************
      CNTBG  = BACKGD * AREASRC
      IFRAT  = 0
      PROBA  = 0.0
      PROBGA = 0.0
      TSIG   = 0.0
      BBBA   = 0.0

C**********
C Calc S/N
C**********
      IF ( ERRSRC.GT.0.0 ) THEN
         RAT = SOURC / ERRSRC
      ELSE
         RAT = 0
      ENDIF

C********************************************************
C If S/N < 5.0 calculate the probability that the source
C is a background fluctuation
C********************************************************
      IF ( RAT.LT.5.0 ) THEN

         IFRAT = 1

C        Warn if background < 0
         IF ( CNTBG.LT.0.0 ) THEN
            CALL XWRITE( ' Warning: Background less than zero', 5 )
         ENDIF

C        Subtract background if > 0, if not
C        use ABS() to protect the SQRT()
         IF ( CNTBG.EQ.0.0 ) THEN
            ZZ = CNTSRC
         ELSE
            ZZ = ( CNTSRC - CNTBG ) / SQRT( ABS( CNTBG ) )
         ENDIF

C        Calculate the probability that this is a background fluct.
         CALL CUMGA( ZZ, PROBGA )
         ICNTS = CNTSRC
         BBBA  = CNTBG
         PROBA = XPOLOG( ICNTS, BBBA, 1 )

      ENDIF

C***************************
C Get vignetting correction
C***************************
      IF ( VIGNAPP ) THEN
           CALL XWRITE( ' Vignetting is part of exposure map', 10 )
           VCOR = 1.0
      ELSE
           CALL GET_VIGN( MAPID, XPIX, YPIX, ITEL, VCOR, 
     &                    VIGN1ST, P_VIGN, STATUS )
           IF ( STATUS.NE.0 ) THEN
              CALL XWRITE( ' Failed to get vignetting correction', 10 )
              VCOR = 1.0
           ENDIF
      ENDIF

C*******************************
C Get the EEF (weighted or not)
C*******************************
      IF ( UNKNOWN ) THEN
         FRACC = 1.0
      ELSE
         III = RADSRC * SQRT( SFA ) - 1
         IF ( III.LT.0 ) THEN
            III = 0
         ENDIF
         IF ( III.GT.( MAXPSF - 1 ) ) THEN
            III = MAXPSF - 1
         ENDIF
         IF ( EXPOWGT ) THEN
C           Normalize the weighted EEF
            FRACC = FRACW(III) / EXPOMAXS(III)
         ELSE
            FRACC = FRAC(III)
         ENDIF
            
         IF ( FRACC.EQ.0.0 ) THEN
            FRACC = 1.0
         ENDIF
      ENDIF

C***********************************************************
C If the probability that the source is a background fluct.
C is > 3-sigma, get a 3-sigma upper limit
C***********************************************************
      IF ( PROBA.GT.1.3D-3 ) THEN
         IF ( CNTSRC.LT.CNTBG ) THEN
            CNTSRC = CNTBG
         ENDIF

C        Alway use sigma=3 confidence level for sosta
C        uplimit for more control
         CALL ULLIM( CNTSRC, CNTBG, 0.99865, RES, STATUS )
         STATUS = 0

C        Correct the upper limit for exposure, psf,
C        vignetting, and dead-time (if applicable)
         IF ( DTIME.NE.0.0 .AND. FRACC.NE.0.0 ) THEN
            TSIG = DTIME * VCOR * RES / ( EXPOSURE * FRACC )
         ELSEIF ( DTIME.NE.0.0 ) THEN
            CALL XWARN( 'Upper limit not corrected for PSF', 5 )
            TSIG = DTIME * VCOR * RES / EXPOSURE
         ELSEIF ( FRACC.NE.0.0 ) THEN
            TSIG = VCOR * RES / ( EXPOSURE * FRACC )
         ELSE
            CALL XWARN( 'Upper limit not corrected for PSF', 5 )
            TSIG = VCOR * RES / EXPOSURE
         ENDIF
      ENDIF
      
C**************************
C Get the optimal box size
C**************************
      CALL NOBOX( SFA, BACKGD, SOURC, OPIXBOX )
      RADSRC = FLOAT( OPIXBOX ) / 2.0
 
C******
C Done
C******
      RETURN
      END
