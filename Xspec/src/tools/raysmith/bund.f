**==bund.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      SUBROUTINE BUND(No)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABIn , ABUnd , ABUnj , ALF , BIN , BINmin , BINsyz , C , 
     &     CNC , CONce , DNE , e , E3 , ES , F , GNDrec , HENeut , 
     &     HEPlus , PCOol , PM
      REAL POT , POWer , PPU , PTOt , RE , RHY , S , SIG , TU , V , WAVe
      INTEGER IBN , ik , kedge , l , LL , NBIn , NJ , No
C*** End of declarations inserted by SPAG
C  PUTS EMISSION LINES INTO ARRAY BIN.
 
      INCLUDE 'rayspec.inc'
 
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /COM   / CNC(12,30) , PTOt(12,220) , ABIn(MXBINS) , 
     &                BIN(MXBINS)
      COMMON /DAT   / V(30) , S(30) , C(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , SIG(30) , ALF(30) , ES(30)
      COMMON /RESULT/ CONce(30) , GNDrec(30) , POWer(220) , RHY , 
     &                HENeut , HEPlus , DNE , PCOol , PPU , POT , RE , 
     &                TU , PM(4)

c suppress compiler warning
      l = No

      DO 100 l = 1 , 220
         IF ( WAVe(l).GT.0 ) THEN
            e = 12399./WAVe(l)
            ik = IBN(e)
            IF ( ik.GE.1 ) THEN
               IF ( ik.LE.NBIn ) THEN
C
C  MOVE PHOTONS IN SAME BIN AS POPULAR K EDGE BUT HIGHER ENERGY UP
C  TO NEXT HIGHER BIN:  B,C,N,O,F,NE,AL EDGES
C
                  kedge = IBN(187.03)
                  IF ( ik.EQ.kedge .AND. e.GT.187.03 ) ik = ik + 1
                  kedge = IBN(284.05)
                  IF ( ik.EQ.kedge .AND. e.GT.284.05 ) ik = ik + 1
                  kedge = IBN(400.07)
                  IF ( ik.EQ.kedge .AND. e.GT.400.06 ) ik = ik + 1
                  kedge = IBN(532.09)
                  IF ( ik.EQ.kedge .AND. e.GT.532.09 ) ik = ik + 1
                  kedge = IBN(692.14)
                  IF ( ik.EQ.kedge .AND. e.GT.692.14 ) ik = ik + 1
                  kedge = IBN(874.16)
                  IF ( ik.EQ.kedge .AND. e.GT.874.16 ) ik = ik + 1
                  kedge = IBN(1559.3)
                  IF ( ik.EQ.kedge .AND. e.GT.1559.3 ) ik = ik + 1
                  BIN(ik) = BIN(ik) + POWer(l)
               ENDIF
            ENDIF
         ENDIF
 100  CONTINUE
      RETURN
      END
 
      INTEGER FUNCTION IBN(E)
      REAL E

      REAL ABUnj, ABUnd, BINmin, BINsyz
      INTEGER NJ, NBIn

      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn

      IBN = INT((E-BINmin)/BINsyz + 1)
      RETURN
      END
