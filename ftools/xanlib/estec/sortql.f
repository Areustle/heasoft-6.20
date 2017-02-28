**==SORTQL.spg  processed by SPAG 3.09I  at 09:47 on 20 Aug 1992
      SUBROUTINE SORTQL(Qualifiers,Nqual)
c
c  does a binary search to place qualifiers in alphabetic order
c
      CHARACTER*(*) Qualifiers(*)
      character(36) stor
      INTEGER*4 Nqual , min , i , j , k , in1 , in2 , in3
c
      IF ( Nqual.LE.1 ) RETURN
c
      DO 100 i = 1 , Nqual - 1
         min = i
         j = i + 1
         DO 50 k = j , Nqual
c
c  find the aphanumeric part of the qualifier
c
            in1 = INDEX(Qualifiers(k),' ') - 1
            in2 = INDEX(Qualifiers(min),' ') - 1
            IF ( in1.GT.in2 ) THEN
               in3 = in1
            ELSE
               in3 = in2
            ENDIF
c	  in3 = jmax0 ( in1, in2)
c
            IF ( LLT(Qualifiers(k)(1:in3),Qualifiers(min)(1:in3)) ) THEN
               min = k
            ENDIF
 50      CONTINUE
         stor = Qualifiers(i)
         Qualifiers(i) = Qualifiers(min)
         Qualifiers(min) = stor
 100  CONTINUE
c
      RETURN
      END
