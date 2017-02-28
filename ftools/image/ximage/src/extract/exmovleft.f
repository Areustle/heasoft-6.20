c
      SUBROUTINE EXMOVLEFT(Cstring,Nchar)
      implicit none
c
c ls  12/12/88 to move to the left the first non-blank char. of a char. string
c
c  I/O  cstring = character string
c  I    nchar = no. of chars in string
c
      CHARACTER Cstring*(*)
      INTEGER*4 Nchar , k , ifir
c
c determine index of first non-blank char
c
c      write(*,'(''  :'',a)') cstring
      ifir = 0
      DO 100 k = Nchar , 1 , -1
         IF ( Cstring(k:k).NE.' ' ) ifir = k
 100  CONTINUE
c
c shift string to the left by ifir-1 characters and replace rest with blanks
c
      IF ( ifir.GT.1 ) THEN
         DO 150 k = 1 , Nchar - ifir + 1
            Cstring(k:k) = Cstring(k+ifir-1:k+ifir-1)
 150     CONTINUE
         Cstring(Nchar-ifir+2:) = ' '
      ENDIF
c      write(*,'(''  :'',a)') cstring
      RETURN
      END
