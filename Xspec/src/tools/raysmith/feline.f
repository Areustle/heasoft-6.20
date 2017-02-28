**==feline.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE FELINE(T,Dene,Num,Ipr,Jpr,Jcont,Iphot,Idens,Icx)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABIn , ABUnd , ABUnj , BIN , BINmin , BINsyz , BLN , BLNmin , 
     &     BLNsyz , BRMev , C1 , C1J , C2 , C2J , CA , CAJ , CNC , 
     &     CONce , CTH , Dene
      REAL DNE , E , E3 , E3J , EA , EAJ , EJ , F , FJ , HENeut , 
     &     HEPlus , hnu , hnuu , pc , PCOol , PM , POT , POU , POWer , 
     &     PTOt
      REAL PWR , RE , RECev , RHY , S2 , S2J , S3 , S3J , S4 , S4J , 
     &     S5 , S5J , st , T , TU , TUFev , WAVe , WJ
      INTEGER i , ib , Icx , Idens , io , Iphot , Ipr , j , jb , Jcont , 
     &        Jpr , l , LL , LLJ , n , NBIn , NBLn , NJ , nlines , nn
      INTEGER no , np , Num
C*** End of declarations inserted by SPAG
 
      INCLUDE 'rayspec.inc'
 
      COMMON /BLN   / BLN(MXBINS) , BLNmin , BLNsyz , NBLn
      COMMON /FEL   / WJ(12,220) , FJ(12,220) , E3J(12,220) , 
     &                PWR(12,220) , C1J(12,30) , C2J(12,30) , EJ(12,30)
     &                , EAJ(12,30) , S2J(12,30) , LLJ(12,30) , 
     &                S3J(12,30) , S4J(12,30) , S5J(12,30)
      COMMON /DAT   / E(30) , EA(30) , S2(30) , WAVe(220) , E3(220) , 
     &                F(220) , LL(30) , S3(30) , S4(30) , S5(30)
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /RESULT/ CONce(30) , CA(30) , POWer(220) , RHY , HENeut , 
     &                HEPlus , DNE , PCOol , POU , POT , RE , TU , PM(4)
      COMMON /AUG   / CAJ(12,30)
      COMMON /RATES / C1(30) , C2(30) , CTH
      COMMON /COM   / CNC(12,30) , PTOt(12,220) , ABIn(MXBINS) , 
     &                BIN(MXBINS)
      COMMON /CONTIN/ BRMev(MXBINS) , RECev(MXBINS) , TUFev(MXBINS)
      DIMENSION pc(12)
C
      DO 100 i = 1 , NBIn
         RECev(i) = 0.
         TUFev(i) = 0.
         BRMev(i) = 0.
         BIN(i) = 0.
 100  CONTINUE
      nlines = 220
      st = SQRT(T)
      PCOol = 0.
      ABUnd = 12.
      CONce(2) = RHY/(1.+RHY)
      IF ( NBIn.NE.0 ) CALL BREMS(T,1)
      IF ( NBIn.NE.0 ) CALL RECEMS(T,1,0,0)
C
C  ELEMENT LOOP
      DO 400 no = 1 , Num
         RE = 0.
         TU = 0.
         ABUnd = ABUnj(no)
         DO 150 l = 1 , nlines
            POWer(l) = 0.
            WAVe(l) = WJ(no,l)
            F(l) = FJ(no,l)
            E3(l) = E3J(no,l)
 150     CONTINUE
         n = NJ(no)
         nn = n + 1
         DO 200 j = 1 , n
            E(j) = EJ(no,j)
            EA(j) = EAJ(no,j)
            S2(j) = S2J(no,j)
            CONce(j) = CNC(no,j)
            LL(j) = LLJ(no,j)
            S3(j) = S3J(no,j)
            S4(j) = S4J(no,j)
            S5(j) = S5J(no,j)
 200     CONTINUE
         E(n+1) = 0.
         LL(n+1) = 0
         CONce(n+1) = CNC(no,n+1)
C
         CALL SINGLE(T,Dene,no,io,Ipr,Jpr,Jcont,Iphot,Idens,Icx)
C
         IF ( no.EQ.1 ) HENeut = CONce(1)
         IF ( no.EQ.1 ) HEPlus = CONce(2)
         DO 250 j = 1 , nn
            IF ( Jcont.EQ.0 ) CNC(no,j) = CONce(j)
            CAJ(no,j) = CA(j)
            C1J(no,j) = C1(j)
            C2J(no,j) = C2(j)
 250     CONTINUE
         IF ( n.NE.18 .AND. n.NE.20 .AND. n.NE.28 ) THEN
            IF ( NBIn.NE.0 ) CALL BREMS(T,n)
            IF ( NBIn.NE.0 ) CALL RECEMS(T,n,0,0)
         ENDIF
C
         DO 300 l = 1 , 220
            PWR(no,l) = POWer(l)
 300     CONTINUE
         IF ( NBIn.NE.0 ) CALL BUND(no)
         pc(no) = PCOol + TU
 400  CONTINUE
      PCOol = 0.
      DO 500 no = 1 , Num
         PCOol = PCOol + pc(no)
 500  CONTINUE
      IF ( NBIn.NE.0 ) THEN
         DO 550 i = 1 , NBIn
            BIN(i) = BIN(i) + RECev(i) + BRMev(i) + TUFev(i)
 550     CONTINUE
         IF ( Jpr.NE.0 ) THEN
            WRITE (7,99001) T , BINmin , BINsyz
            np = NBIn/2
            DO 560 i = 1 , np
               jb = i*2
               ib = jb - 1
               hnu = BINmin + ib*BINsyz - BINsyz
               hnuu = hnu + BINsyz
               WRITE (7,99002) ib , hnu , RECev(ib) , BRMev(ib) , 
     &                         TUFev(ib) , BIN(ib) , jb , hnuu , 
     &                         RECev(jb) , BRMev(jb) , TUFev(jb) , 
     &                         BIN(jb)
 560        CONTINUE
         ENDIF
      ENDIF
      IF ( NBLn.NE.0 ) CALL BLND(Num)
      RETURN
99001 FORMAT ('1',' TEMP',F10.0,'    BINMIN',F7.2,'    BINSYZ',F7.2,/,
     &        2('  BIN    HNU',9X,
     &        'RECEMS     BREMS    2 PHOT     TOTAL    '))
99002 FORMAT (2(1X,I5,F10.1,3X,4E10.3,1X))
      END
 
