**==single.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE SINGLE(T,Dene,No,Io,Iprint,Jprint,Jcont,Iphot,Idens,
     &                  Icx)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABUnd , ABUnj , ALF , ALPHADI , BINmin , BINsyz , C , C1 , 
     &     C2 , cn , CONce , CTH , Dene , DNE , E , E3 , ES , F , g , 
     &     GAUNT
      REAL GNDrec , HENeut , HEPlus , PCOol , pdum , PM , POPm , POT , 
     &     POU , POWer , pw , RE , RHY , S , SIG , st , T , TU , WAVe , 
     &     wdum
      INTEGER Icx , Idens , iik , il , Io , Iphot , Iprint , ix , iy , 
     &        Jcont , jdum , Jprint , k , l , ldum , LL , loc , n , 
     &        NBIn , NJ
      INTEGER nn , No
C*** End of declarations inserted by SPAG
      INTEGER bb
      COMMON /DAT   / E(30) , S(30) , C(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , SIG(30) , ALF(30) , ES(30)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /RATES / C1(30) , C2(30) , CTH
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , POU , POT , RE , 
     &                TU , PM(4)
      COMMON /STRMET/ POPm(4,12)
      DIMENSION jdum(5) , ldum(5) , wdum(5) , pdum(5)

c suppress compiler warning
      k = io
      k = jprint
 
      PCOol = 0.
      ABUnd = 12.
      st = SQRT(T)
C  IONIZATION STATE OF H MUST BE PASSED FROM OUTSIDE
      RE = 0.
      TU = 0.
      ABUnd = ABUnj(No)
      n = NJ(No)
      ix = 0
      nn = n + 1
      E(n+1) = 0.
      LL(n+1) = 0
      CALL NQUIL(T,n,Dene,Jcont,Iphot,Idens,Icx)
      POPm(1,No) = PM(1)
      POPm(2,No) = PM(2)
      POPm(3,No) = PM(3)
      POPm(4,No) = PM(4)
      loc = 0
      IF ( Iprint.NE.0 ) THEN
         WRITE (7,99001) n , ABUnd , T , Dene
         IF ( n.GE.6 ) WRITE (7,99002) n , PM
         WRITE (7,99003) (k,CONce(k),k=1,nn)
         IF ( Iprint.GE.3 ) WRITE (7,99003) (k,C1(k),k=1,nn)
         IF ( Iprint.GE.3 ) WRITE (7,99003) (k,C2(k),k=1,nn)
      ENDIF
      iik = ix
      ix = 0
      DO 100 il = 1 , n
         iy = 3*LL(il)
         IF ( iy.GT.0. ) THEN
            cn = CONce(il)
            pw = 0.
            DO 20 l = 1 , iy
               bb = ix + l
               IF ( cn.GE..00001 ) THEN
                  IF ( WAVe(bb).LE.0. ) GOTO 20
                  g = GAUNT(T,E3(bb),n,il,l,Dene)
                  pw = 10.**(ABUnd+11.)
     &                 *cn*(ALPHADI(n,il,l,bb,T)*E3(bb)*1.602E-12+
     &                 2.72E-15*F(bb)*EXP(-11590*E3(bb)/T)*g/st)
     &                 *12398./(WAVe(bb)*E3(bb))
               ENDIF
               POWer(bb) = pw
               PCOol = PCOol + pw
 20         CONTINUE
            IF ( il.EQ.n-1 ) CALL HESEQ(n,il,T,Dene,ix)
            IF ( il.EQ.n ) CALL HYSEQ(n,il,T,Dene,ix)
         ENDIF
         ix = ix + iy
 100  CONTINUE
      IF ( Iprint.GT.1 ) THEN
         ix = 0
         DO 150 il = 1 , n
            iy = 3*LL(il)
            IF ( CONce(il).GT..001 ) THEN
               DO 110 l = 1 , iy
                  bb = ix + l
                  IF ( WAVe(bb).GT.0. ) THEN
                     loc = loc + 1
                     jdum(loc) = il
                     ldum(loc) = l
                     wdum(loc) = WAVe(bb)
                     pdum(loc) = POWer(bb)
                     IF ( loc.EQ.5 ) WRITE (7,99004) n , 
     &                    (jdum(k),ldum(k),wdum(k),pdum(k),k=1,5)
                     loc = MOD(loc,5)
                  ENDIF
 110           CONTINUE
            ENDIF
            ix = ix + iy
 150     CONTINUE
         IF ( loc.NE.0 ) WRITE (7,99004) n , 
     &                          (jdum(k),ldum(k),wdum(k),pdum(k),k=1,
     &                          loc)
      ENDIF
      IF ( Iprint.NE.0 ) WRITE (7,99005) PCOol , RE , TU
      RETURN
99001 FORMAT ('0LINES FOR N=',I2,'     ABUND=',F5.2,'     T=',E9.3,
     &        '     DENE=',E9.3)
99002 FORMAT (' CONCE FOR N=',I3,' METASTABLES ',4E10.3)
99003 FORMAT (10(I3,E10.2),/,10(I3,E10.2),/,10(I3,E10.2),/)
99004 FORMAT (I4,5(I4,I3,F8.2,E10.3))
99005 FORMAT (' PCOOL=',E10.3,' RE=',E10.3,' TU=',E10.3)
      END
