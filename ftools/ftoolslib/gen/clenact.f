      INTEGER FUNCTION CLENACT(CBUF)
      CHARACTER CBUF*(*)
C---
C --- RY, Taken routine from Xanadu for CALLIB
C NOTE : Routine should be used with care, as null characters
C at the end of a string can give a larger active length than
C expected. This can be avoided by initialising the string before
C using it.
C
C Function to return the active length of a character string, not
C counting any trailing blanks.  N.B. an all blank string will
C return ZERO as the length.
C---
C CBUF    I    String whose length is to be measured.
C---
C 1988-Jun-13 - Standard Fortran version [AFT]
C---
      INTEGER   I
C---
      DO 190 I=LEN(CBUF),1,-1
         IF(CBUF(I:I).NE.' ') THEN
            CLENACT=I
            RETURN
         END IF
  190 CONTINUE
      CLENACT=0
      RETURN
      END
