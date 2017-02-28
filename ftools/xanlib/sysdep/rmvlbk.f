**==RMVLBK.spg  processed by SPAG 3.09I  at 12:41 on 28 Aug 1992
      SUBROUTINE RMVLBK(String)
c
      IMPLICIT NONE
c
c  kills leading blanks in a character string
c      string can be up to 400 bytes long
c
c   pads end of string with ascii blanks
c
c
      CHARACTER*(*) String
      INTEGER*4 i
      INTEGER*4 length
      INTEGER*4 LENACT
      length = LENACT(String)
      IF ( length.NE.0 ) THEN
         i = 1
         DO WHILE ( String(i:i).EQ.' ' )
            i = i + 1
         ENDDO
         IF ( i.GT.1 ) String = String(i:)
      ENDIF
      RETURN
      END
