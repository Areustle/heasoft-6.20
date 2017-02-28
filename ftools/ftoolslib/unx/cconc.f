*+CCONC
      SUBROUTINE CCONC(CBUF)
      CHARACTER CBUF*(*)
C-----------------------------------------------------------------------
C Description: Convert case of CBUF to default case of system.
C              This ought to be obsolete.
C              Modified for the Calibration Database... Used by 
C              the CPTEND software
C
C Arguments:   CBUF    I/O  The file name to be converted.
C
C Origin:      Swiped from Xanadu Library for Calibration Database
C
C Authors/Modification History:
C              Xanadu Library
C              Ron Zelar (1993, Feb 3) Modified for inclusion in 
C                   Calibration Library
C-----------------------------------------------------------------------
*-Version 1.0

C      CALL FTUPCH(CBUF)
      RETURN
      END
