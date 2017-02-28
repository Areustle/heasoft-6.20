      SUBROUTINE SOSTA_OUT( XPIX, YPIX, ITEL, DETECT, VIGNAPP, RADSRC,
     &                      BADEXPO, EXPOSURE, EXPOWGT, DTIME, CNTSRC,
     &                      AREASRC, BACKGD, ERRBG, SOURC, ERRSRC,
     &                      IFRAT, PROBA, PROBGA, VCOR, FRACC, TSIG,
     &                      STATUS )
C**********************************************************************
C  Source statistic output (and various _important_ calculations)
C
C  I  X/YPIX    (R)  Source box center
C  I  ITEL      (I)  Telescope index
C  I  DETECT    (L)  Whether using detected sources
C  I  VIGNAPP   (L)  Whether vignetting is applied (exposure map)
C  I  RADSRC    (R)  Source box size
C  I  BADEXPO   (L)  If true, no valid exposure
C  I  EXPOSURE  (D)  Exposure time
C  I  EXPOWGT   (L)  Was exposure weighting done?
C  I  DTIME     (D)  Dead time correction
C  I  CNTSRC    (R)  Raw source counts
C  I  AREASRC   (R)  Area of source (det-pix)
C  I  BACKGD    (R)  Computed background
C  I  SOURC     (R)  Source value
C  I  ERRSRC    (R)  Source error
C  I  IFRAT     (I)  Ratio flag
C  I  PROBA     (R)  Probability that source is fluctuation
C  I  PROBGA    (R)  Probability that source is fluctuation (cumga)
C  I  VCOR      (R)  Vignetting correction
C  I  FRACC     (R)  EEF value
C  I  TSIG      (R)  Three sigma upper limit
C I/O STATUS    (I)  Error flag (0=OK)
C**********************************************************************
      IMPLICIT NONE

C*****************
C Input variables
C*****************
      LOGICAL DETECT
      LOGICAL VIGNAPP
      LOGICAL BADEXPO
      LOGICAL EXPOWGT

      INTEGER*4 ITEL
      INTEGER*4 IFRAT
      INTEGER*4 STATUS

      REAL*4 XPIX
      REAL*4 YPIX
      REAL*4 RADSRC
      REAL*4 CNTSRC
      REAL*4 AREASRC
      REAL*4 BACKGD
      REAL*4 ERRBG
      REAL*4 SOURC
      REAL*4 ERRSRC
      REAL*4 PROBA
      REAL*4 PROBGA
      REAL*4 VCOR
      REAL*4 FRACC
      REAL*4 TSIG

      REAL*8 EXPOSURE
      REAL*8 DTIME

C*****************
C Common includes
C*****************
      INCLUDE '../include/io.inc'

C*****************
C Local variables
C*****************
      LOGICAL READONLY
      LOGICAL GLOBAL

      character(100) DS

      INTEGER SLEN

      REAL*4 SOURI
      REAL*4 ERRSI
      REAL*4 BACKI
      REAL*4 ERBAI
      REAL*4 SOGOOD
      REAL*4 SOGOD1
      REAL*4 ERGOOD
      REAL*4 ERGOD1
      REAL*4 CSOURI
      REAL*4 CSOUR1
      REAL*4 CERRSI
      REAL*4 CERRS1
      REAL*4 SNRAT
      REAL*4 FROCC
      REAL*4 ERRAD

      REAL*8 TRUEEXPO

C***********
C Functions
C***********
      REAL*4 ERROR_RADIUS

C************
C Initialize
C************
      STATUS = 0

C******************************
C Do final source calculations
C******************************

C     Source count rate and error
      SOURI = SOURC / EXPOSURE
      ERRSI = ERRSRC / EXPOSURE

C     Background count rate and error
      BACKI = BACKGD / EXPOSURE
      ERBAI = ERRBG / EXPOSURE

C     PSF corrected source counts and error
      SOGOOD = SOURC / FRACC
      ERGOOD = ERRSRC / FRACC

C     PSF, dead time, vignetting 
C     corrected source counts and error
      SOGOD1 = SOURC / FRACC * DTIME * VCOR
      ERGOD1 = ERRSRC / FRACC * DTIME * VCOR

C     PSF corrected source rate and error
      CSOURI = SOURI / FRACC
      CERRSI = ERRSI / FRACC

C     PSF, dead time, vignetting
C     corrected source rate and error
      CSOUR1 = SOURI / FRACC * DTIME * VCOR
      CERRS1 = ERRSI / FRACC * DTIME * VCOR

C***********************
C Write out the results
C***********************

C     If sources not being detected, write 
C     counts and rate statistics for this source
      IF ( .NOT.DETECT ) THEN

C        Background counts
         WRITE( ZWRITE, 99010 ) BACKGD, ERRBG
         CALL XWRITE( ZWRITE, 10 )

C        If the exposure is OK, write background rate info
         IF ( .NOT.BADEXPO ) THEN
            WRITE( ZWRITE, 99020 ) BACKI, ERBAI
            CALL XWRITE( ZWRITE, 10 )
         ENDIF
         WRITE( ZWRITE, 99000 )
         CALL XWRITE( ZWRITE, 10 )

C        Source counts
         WRITE( ZWRITE, 99030 ) SOURC, ERRSRC
         CALL XWRITE( ZWRITE, 10 )

C        If no vignetting applied to exposure map, write stats for
C        PSF correction only. Otherwise we can't disentagle them.
         IF( .NOT.VIGNAPP ) THEN
            WRITE( ZWRITE, 99040 ) SOGOOD, ERGOOD
            CALL XWRITE( ZWRITE, 10 )
         ENDIF 

C        Fully corrected source stats (counts)
         WRITE( ZWRITE, 99050 )
         CALL XWRITE( ZWRITE, 10 )
         WRITE( ZWRITE, 99060 ) SOGOD1, ERGOD1
         CALL XWRITE( ZWRITE, 10 )

C        If the exposure is OK, write source rate info
         IF ( .NOT.BADEXPO ) THEN
            WRITE( ZWRITE, 99070 ) SOURI, ERRSI
            CALL XWRITE( ZWRITE, 10 )

C           If no vignetting applied, write stats for PSF 
C           correction only. Otherwise we can't disentagle them
            IF ( .NOT.VIGNAPP ) THEN
               WRITE( ZWRITE, 99080 ) CSOURI, CERRSI
               CALL XWRITE( ZWRITE, 10 )
            ENDIF
         ENDIF
      ENDIF

C     Write intensity info
      IF ( .NOT.BADEXPO ) THEN
         WRITE( ZWRITE, 99090 )
         CALL XWRITE( ZWRITE, 10 )
         WRITE( ZWRITE, 99100 ) CSOUR1, CERRS1
         CALL XWRITE( ZWRITE, 10 )
      ENDIF

C     Calc S/N and write it out
      IF ( CERRS1.NE.0.0 ) THEN
         SNRAT = CSOUR1 / CERRS1
      ELSE
         CALL XWRITE( ' source has zero counts', 10 )
         SNRAT = 0.0 
      ENDIF 
      WRITE( ZWRITE, 99110 ) SNRAT
      CALL XWRITE( ZWRITE, 10 )

C     Write detection probabilities
      IF ( IFRAT.EQ.1 ) THEN
         WRITE( ZWRITE, 99120 )
         CALL XWRITE( ZWRITE, 10 )
         WRITE( ZWRITE, 99130 ) PROBA, PROBGA
         CALL XWRITE( ZWRITE, 10 )
      ENDIF

C     Write intensity correction info
      FROCC = 1.0 / FRACC
      IF ( .NOT.DETECT ) THEN
         WRITE( ZWRITE, 99000 )
         CALL XWRITE( ZWRITE, 10 )
         WRITE( ZWRITE, 99000 )
         CALL XWRITE( ZWRITE, 10 )

         TRUEEXPO = EXPOSURE
         IF ( BADEXPO ) THEN
            TRUEEXPO = 0.0D0
         ENDIF
         IF ( VIGNAPP .AND. .NOT.EXPOWGT ) THEN
            WRITE( ZWRITE, 99140 ) TRUEEXPO
            CALL XWRITE( ZWRITE, 10 )
         ELSEIF ( VIGNAPP ) THEN
            WRITE( ZWRITE, 99150 ) TRUEEXPO
            CALL XWRITE( ZWRITE, 10 )
         ELSE
            WRITE( ZWRITE, 99150 ) TRUEEXPO
            CALL XWRITE( ZWRITE, 10 )
            WRITE( ZWRITE, 99160 ) VCOR
            CALL XWRITE( ZWRITE, 10 )
         ENDIF
         WRITE( ZWRITE, 99170 ) DTIME
         CALL XWRITE( ZWRITE, 10 )
         IF ( EXPOWGT ) THEN
            WRITE( ZWRITE, 99185 ) FROCC
            CALL XWRITE( ZWRITE, 10 )
         ELSE
            WRITE( ZWRITE, 99180 ) FROCC
            CALL XWRITE( ZWRITE, 10 )
         ENDIF
      ENDIF

C     Write upper limit if we have one
      IF ( PROBA.GT.1.3D-3 .AND. IFRAT.EQ.1 ) THEN
         WRITE( ZWRITE, 99000 )
         CALL XWRITE( ZWRITE, 10 )
         IF ( .NOT.BADEXPO ) THEN
            WRITE( ZWRITE, 99190 ) TSIG
         ELSE
            WRITE( ZWRITE, 99200 ) TSIG
         ENDIF
         CALL XWRITE( ZWRITE, 10 )
      ENDIF

C     Write optimal half-box for this source
C     and save in Tcl
      IF ( .NOT.DETECT .AND. RADSRC.GT.0.0 ) THEN
         CALL XDSTR( DBLE( RADSRC ), -1, DS, SLEN )
         WRITE( ZWRITE, 99210 ) DS(:SLEN)
         CALL XWRITE( ZWRITE, 10 )
         CALL TCLAVARR( 'sosta(optboxhalf)', RADSRC, READONLY, GLOBAL, 
     &                  STATUS )
      ENDIF

C     Write error radius and save in Tcl
      ERRAD = ERROR_RADIUS( XPIX, YPIX, SNRAT, ITEL )
      IF ( ERRAD.GT.0.0 ) THEN
         WRITE( ZWRITE, 99220 ) ERRAD
         CALL XWRITE( ZWRITE, 10 )
         CALL TCLAVARR( 'sosta(errrad)', ERRAD, READONLY, GLOBAL, 
     &                  STATUS )
      ENDIF

C******************************************************
C Record data in sosta Tcl variables
C In detect case, appends to create list for each stat
C******************************************************
      GLOBAL   = .FALSE.
      READONLY = .FALSE.

      CALL TCLAVARR( 'sosta(rawcnt)', CNTSRC, READONLY, GLOBAL, STATUS )
      CALL TCLAVARR( 'sosta(srcarea)', AREASRC, READONLY, GLOBAL,
     &               STATUS )
      CALL TCLAVARR( 'sosta(background)', BACKGD, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(bgerr)', ERRBG, READONLY, GLOBAL, STATUS )
      CALL TCLAVARR( 'sosta(bgrate)', BACKI, READONLY, GLOBAL, STATUS )
      CALL TCLAVARR( 'sosta(bgrateerr)', ERBAI, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srccnt)', SOURC, READONLY, GLOBAL, STATUS )
      CALL TCLAVARR( 'sosta(srccnterr)', ERRSRC, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srccntpsf)', SOGOOD, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srccntpsferr)', ERGOOD, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srccntpdv)', SOGOD1, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srccntpdverr)', ERGOD1, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srcint)', SOURI, READONLY, GLOBAL, STATUS )
      CALL TCLAVARR( 'sosta(srcinterr)', ERRSI, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srcintpsf)', CSOURI, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srcintpsferr)', CERRSI, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srcintpdv)', CSOUR1, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(srcintpdverr)', CERRS1, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(snr)', SNRAT, READONLY, GLOBAL, STATUS )
      CALL TCLAVARR( 'sosta(probpois)', PROBA, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(probgauss)', PROBGA, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(uplim)', TSIG, READONLY, GLOBAL, STATUS )
      CALL TCLAVARD( 'sosta(exposure)', EXPOSURE, READONLY, GLOBAL, 
     &               STATUS )
      CALL TCLAVARR( 'sosta(psfcor)', FROCC, READONLY, GLOBAL, STATUS )
      CALL TCLAVARD( 'sosta(dtimecor)', DTIME, READONLY, GLOBAL,
     &               STATUS )
      CALL TCLAVARR( 'sosta(vigncor)', VCOR, READONLY, GLOBAL, STATUS )

C******
C Done
C******
      RETURN

C********************************************
C Various formatted output FORMAT statements
C********************************************
99000 FORMAT( '  ' )
99010 FORMAT( ' Background/elemental sq pixel : ', T48, 1PE10.3,
     &        ' +/- ', E7.1 )
99020 FORMAT( ' Background/elemental sq pixel/sec : ', T48, 1PE10.3,
     &        ' +/- ', E7.1 )
99030 FORMAT( ' Source counts : ', T48, 1PE10.3, ' +/- ', E7.1 )
99040 FORMAT( ' s.c. corrected for PSF : ', T48, 1PE10.3, 
     &        ' +/- ', E7.1 )
99050 FORMAT( ' s.c. corrected for PSF + sampling dead time' )
99060 FORMAT( '                                + vignetting   ', T48,
     &        1PE10.3, ' +/- ', E7.1 )
99070 FORMAT( ' Source intensity : ', T48, 1PE10.3, ' +/- ', E7.1,
     &        ' c/sec' )
99080 FORMAT( ' s.i. corrected for PSF ', T48, 1PE10.3, ' +/- ', E7.1,
     &        ' c/sec' )
99090 FORMAT( ' s.i. corrected for PSF + sampling dead time ' )
99100 FORMAT( '                                + vignetting ->', T48,
     &        1PE10.3, ' +/- ', E7.1, ' c/sec <-' )
99110 FORMAT( ' Signal to Noise Ratio             : ', T48, 1PE10.3 )
99120 FORMAT( 48X, ' Poisson', 2X, '  Gauss ' )
99130 FORMAT( ' Pr. that source is a fluctuation of back. :', T48,
     &        1PE9.2, 2X, E9.2 )
99140 FORMAT( '    Vignetting corrected exposure : ', F14.3, ' s' )
99150 FORMAT( '    Exposure time                 : ', F14.3, ' s' )
99160 FORMAT( '    Vignetting correction         : ', F10.3 )
99170 FORMAT( '    Sampling dead time correction : ', F10.3 )
99180 FORMAT( '    PSF correction                : ', F10.3 )
99185 FORMAT( '    PSF correction (weighted)     : ', F10.3 )
99190 FORMAT( 6X, 'Three sigma upper limit :', 1PE9.2, ' cts/s' )
99200 FORMAT( 6X, 'Three sigma upper limit :', 1PE9.2, ' cts' )
99210 FORMAT( '    Optimum half box size is      : ', A, 
     &        ' orig pixels' )
99220 FORMAT( '     Error radius : ', F5.0, ' arcsecs ' )

      END
