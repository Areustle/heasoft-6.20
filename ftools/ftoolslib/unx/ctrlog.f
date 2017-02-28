*+CTRLOG
      SUBROUTINE CTRLOG(CBUF, LBUF, CRET, LRET)
      CHARACTER CBUF*(*), CRET*(*)
      INTEGER   LBUF, LRET, FCSTLN
C-----------------------------------------------------------------------
C Description: Translate the logical name CBUF(:LBUF) to return 
C              CRET(:LRET)
C
C Arguments:   CBUF  (i):  The string to translate
C              LBUF  (i):  The number of valid characters in CBUF 
C                          (can be zero)
C              CRET  (r):  The translated string
C              LRET  (r):  The number of valid characters in CRET 
C                          (can be zero)
C
C Origin:      Swiped from the Xanadu Library for Calibration Library
C
C Authors/Modifcation History:
C              Ron Zellar (1993 Feb 3) Modifed for inclusion in the 
C                   Calibration Library
C-----------------------------------------------------------------------
*-Version 1.0

      CALL GETENV(CBUF(:LBUF),CRET)
      LRET=FCSTLN(CRET)
      RETURN
      END
