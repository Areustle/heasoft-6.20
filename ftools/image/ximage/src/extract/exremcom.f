c
c
c
      SUBROUTINE EXREMCOM(Cstring,Nchar,Comm)
      implicit none
c
c ls  20/9/88 to remove comments (if any) from a character string
c
c  I/O  cstring = character string
c  I    nchar = no. of chars in string
c  I    comm = comment char after which the string is cleared (usually '!')
c
      CHARACTER Cstring*(*)
      character(1) Comm
      INTEGER*4 Nchar , k , idum
c
c     remove all chars after comment (!), if there is a comment
c
      idum = 0
      DO 100 k = 1 , Nchar
         IF ( Cstring(k:k).EQ.Comm ) idum = 1
         IF ( idum.EQ.1 ) Cstring(k:k) = ' '
 100  CONTINUE
      RETURN
      END
