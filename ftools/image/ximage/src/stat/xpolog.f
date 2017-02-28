      FUNCTION XPOLOG(Ib,Amea1,Iflag)
C
C  I  ib    (i)  
C  I  amea1 (r)
C  I  iflag (i)  1 for integral distribution   p(n>=ib,amea)
C                2 for differential distribution   p(n=ib,amea)
C
      INTEGER*4 Ib , Iflag
      REAL*4 Amea1 , XPOLOG
C
C  Local variables
C
      INTEGER*4 ir
      REAL*8 ba , DFACL , sum , par , ar , amea

      ba = 1.
      amea = Amea1
c      write(*,*)' xpolog',ib,amea1,iflag
      IF ( Iflag.EQ.2 ) THEN
         IF ( Ib.EQ.0 ) THEN
            XPOLOG = DEXP(-amea)
            RETURN
         ELSE
            XPOLOG = DEXP(Ib*DLOG(amea)-DFACL(Ib)-amea)
            RETURN
         ENDIF
      ELSEIF ( Ib.EQ.1 ) THEN
         XPOLOG = 1. - DEXP(-amea)
         RETURN
      ELSEIF ( Ib.EQ.0 ) THEN
         XPOLOG = 1.
         RETURN
      ELSE
         IF ( amea.LT.86. ) THEN
            sum = DEXP(-amea)
         ELSE
            sum = 0.
         ENDIF
         par = 0.
         DO 50 ir = 1 , Ib - 1
            ar = ir
            par = par + DLOG(ar)
            sum = sum + DEXP(ir*DLOG(amea)-par-amea)
 50      CONTINUE
         XPOLOG = 1. - sum
         if ( XPOLOG.lt.0. ) XPOLOG = 0.
         RETURN
      ENDIF
      END
