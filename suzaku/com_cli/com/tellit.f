C   11/11/86 611112250  MEMBER NAME  TELLIT   (FORT)     M  FORTRAN
C+++++ THIS IS TELLIT.FOR
C
       SUBROUTINE TELLIT(TEXT)
       CHARACTER*(*) TEXT
c      include 'comode.inc'
       include 'comode.inc'
C
C..... SHOW A CHARACTER STRING
C
       ICOMER = 1
       WRITE (LUNCOM,101)
       WRITE (LUNCOM,101) TEXT
101    FORMAT (1X,A)
       END
C=====
       SUBROUTINE TELALL(NDIM,TEXT)
       CHARACTER*(*) TEXT(NDIM)
c      include 'comode.inc'
       include 'comode.inc'
C
C..... SHOW AN ARRAY OF CHARACTER STRINGS
C
       ICOMER = 1
       WRITE (LUNCOM,101)
       DO 10 I = 1,NDIM
         WRITE (LUNCOM,101) TEXT(I)
10     CONTINUE
101    FORMAT (1X,A)
       END
C=====
