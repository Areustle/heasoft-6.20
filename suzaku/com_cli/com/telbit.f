       SUBROUTINE TELBIT(TITLE,NBITS,TEXT,BITS)
       INTEGER*4 NBITS, BITS
       CHARACTER*(*) TITLE, TEXT(NBITS)
c      include 'comode.inc'
       include 'comode.inc'
C
C..... SHOW A BIT STRING
C
       ICOMER = 1
       WRITE (LUNCOM,100) TITLE
100    FORMAT (1X,A)
       WRITE (LUNCOM,101) (IAND(ISHFT(BITS,1-I),1),TEXT(I),I=1,NBITS)
101    FORMAT (4(I3,1X,A))
       END
