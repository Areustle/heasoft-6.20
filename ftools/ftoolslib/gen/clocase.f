*+CLOCASE
      SUBROUTINE CLOCASE(CBUF)
      CHARACTER CBUF*(*)
C-----------------------------------------------------------------------
C Description: Converts CBUF to lower case.
C
C Arguments:   CBUF  (i/r): The character array to convert to lower case
C
C Origin:      Swiped from the Xanadu Library for Calibration Library
C
C Authors/Modification History:
C              Xanadu Library
C              Ron Zellar (1993 Feb 3) Modified for inclusion in the
C                   Calibration Library
C-----------------------------------------------------------------------
*-Version 1.0

      INTEGER   I, ITMP
C---
      DO 120 I=1,LEN(CBUF)
         ITMP=ICHAR(CBUF(I:I))
         IF(ITMP.GT.64 .AND. ITMP.LT.91) CBUF(I:I)=CHAR(ITMP+32)
  120 CONTINUE
      RETURN
      END
