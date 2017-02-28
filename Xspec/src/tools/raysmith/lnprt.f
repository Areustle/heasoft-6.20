**==lnprt.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE LNPRT(Num)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABIn , ABUnd , ABUnj , ALFj , ALFly , ALFlyc , BIN , BINmin , 
     &     BINsyz , c , C1J , C2J , CJ , CNC , E3J , EJ , ESJ , EXIt , 
     &     FCOol , FJ
      REAL FOUt , FTOt , FWV , HALf , HALfc , HBEt , HBEtc , PDUm , 
     &     PTOt , PWR , SIGj , SJ , TWOp , WDUm , WJ
      INTEGER i , il , ix , iy , j , JDUm , JMX , jt , k , l , LDUm , 
     &        LLJ , loc , n , NBIn , NF , nhigh , NJ , nlow , no
      INTEGER Num
C*** End of declarations inserted by SPAG
 
      INCLUDE 'rayspec.inc'
 
      COMMON /HLN   / EXIt(11) , HALf , HALfc , ALFly , ALFlyc , HBEt , 
     &                HBEtc , TWOp
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /FLN   / FWV(320) , FTOt(320) , FOUt(320) , FCOol
      COMMON /FEL   / WJ(12,220) , FJ(12,220) , E3J(12,220) , 
     &                PWR(12,220) , C1J(12,30) , C2J(12,30) , EJ(12,30)
     &                , SJ(12,30) , CJ(12,30) , LLJ(12,30) , ALFj(12,30)
     &                , ESJ(12,30) , SIGj(12,30)
      COMMON /COM   / CNC(12,30) , PTOt(12,220) , ABIn(MXBINS) , 
     &                BIN(MXBINS)
      INTEGER bb
      COMMON /LNPR  / JMX(11) , NF(11) , LDUm(5) , JDUm(5) , WDUm(5) , 
     &                PDUm(5)
      WRITE (7,99001) HBEt , HBEtc , TWOp
      c = 100./(HBEt+HBEtc)
      DO 100 no = 1 , Num
         n = NJ(no)
         WRITE (7,99002) n
         loc = 1
         ix = 0
         jt = INT(AMIN0(JMX(no),n))
         DO 50 j = 1 , jt
            iy = LLJ(no,j)*3
            IF ( iy.GT.0 ) THEN
               DO 10 l = 1 , iy
                  bb = ix + l
                  IF ( WJ(no,bb).GT.0 ) THEN
                     LDUm(loc) = l
                     JDUm(loc) = j
                     WDUm(loc) = WJ(no,bb)
                     PDUm(loc) = PTOt(no,bb)*c
                     IF ( loc.EQ.5 ) WRITE (7,99003) n , 
     &                    (JDUm(k),LDUm(k),WDUm(k),PDUm(k),k=1,5)
                     loc = MOD(loc,5) + 1
                  ENDIF
 10            CONTINUE
            ENDIF
            ix = ix + iy
 50      CONTINUE
         il = loc - 1
         IF ( loc.NE.1 ) WRITE (7,99003) n , 
     &                          (JDUm(k),LDUm(k),WDUm(k),PDUm(k),k=1,il)
         IF ( n.NE.2 ) THEN
            WRITE (7,99004) n
            nlow = NF(no-1) + 1
            nhigh = NF(no)
            loc = 1
            DO 60 i = nlow , nhigh
               IF ( FWV(i)*FTOt(i).GT.0. ) THEN
                  LDUm(loc) = i
                  WDUm(loc) = FWV(i)
                  PDUm(loc) = FTOt(i)*c
                  IF ( loc.EQ.5 ) WRITE (7,99005) n , 
     &                 (LDUm(k),WDUm(k),PDUm(k),k=1,5)
                  loc = MOD(loc,5) + 1
               ENDIF
 60         CONTINUE
            il = loc - 1
            IF ( loc.NE.1 ) WRITE (7,99005) n , 
     &                             (LDUm(k),WDUm(k),PDUm(k),k=1,il)
         ENDIF
 100  CONTINUE
      RETURN
99001 FORMAT ('    HBET, HBETC, TWOP    ',3E10.3)
99002 FORMAT (' LINES FOR ELEMENT',I5)
99003 FORMAT (I4,5(I4,I3,F8.2,E10.3))
99004 FORMAT ('  ***** FORBIDDEN LINES ***** N=',I3)
99005 FORMAT (1X,I2,5(I6,F9.1,E10.3))
      END
 
