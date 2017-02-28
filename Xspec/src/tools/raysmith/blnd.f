**==blnd.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      SUBROUTINE BLND(Num)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABUnd , ABUnj , ALFj , BINmin , BINsyz , BLN , BLNmin , 
     &     BLNsyz , BRMev , C1J , C2J , CJ , de , E3J , EJ , ephot , 
     &     ESJ , FJ , PWR , RECev
      REAL SIGj , SJ , TUFev , wbln , WJ , wli , wna
      INTEGER ibln , ibn , l , l1 , l2 , LLJ , NBIn , NBLn , NJ , no , 
     &        Num
C*** End of declarations inserted by SPAG
 
C  COMPUTES EMISSION SPECTRUM IN WAVELENGTH BINS : FIRST BLN EXTENDS FROM
C  BLNMIN TO BLNMIN+BLNSYZ
C  RESOLUTION AND RANGE OF CONTINUUM ARE ONLY AS GOOD AS THE
C  RESOLUTION AND RANGE OF THE ENERGY BIN COMPUTATION.  BE SURE THAT
C  NBIN, BINMIN AND BINSYZ PROVIDE THE NECESSARY INTERVAL.  BLN'S
C  OUTSIDE THE RANGE OF THE ENERGY BINS WILL HAVE NO CONTINUUM
C  CONTRIBUTION, BUT PLACEMENT OF LINES WILL NOT BE AFFECTED.
 
C  SEPARATES RESONANCE DOUBLETS OF LI-LIKE AND NA-LIKE LINES
C  FEB. 1983
 
      INCLUDE 'rayspec.inc'
 
      COMMON /BLN   / BLN(MXBINS) , BLNmin , BLNsyz , NBLn
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /CONTIN/ BRMev(MXBINS) , RECev(MXBINS) , TUFev(MXBINS)
      COMMON /FEL   / WJ(12,220) , FJ(12,220) , E3J(12,220) , 
     &                PWR(12,220) , C1J(12,30) , C2J(12,30) , EJ(12,30)
     &                , SJ(12,30) , CJ(12,30) , LLJ(12,30) , SIGj(12,30)
     &                , ALFj(12,30) , ESJ(12,30)
      DIMENSION l1(12) , l2(12) , wli(2,12) , wna(2,12)
      DATA l1/0 , 28 , 37 , 43 , 52 , 85 , 103 , 127 , 88 , 148 , 184 , 
     &     166/
      DATA l2/5*0 , 13 , 28 , 43 , 34 , 64 , 85 , 88/
      DATA wli/0. , 0. , 1548.2 , 1550.8 , 1238.8 , 1242.8 , 1031.9 , 
     &     1037.6 , 770.4 , 780.3 , 609.8 , 624.9 , 499.4 , 520.7 , 
     &     417.6 , 445.8 , 353.9 , 389.1 , 302.2 , 344.8 , 192.03 , 
     &     255.11 , 165.42 , 234.20/
      DATA wna/10*0. , 2796.4 , 2803.5 , 1393.8 , 1402.8 , 933.4 , 
     &     944.5 , 700.4 , 714.0 , 557.7 , 574.0 , 335.4 , 360.8 , 
     &     292.0 , 320.6/
C
C  CONTINUUM
C
      DO 100 ibln = 1 , NBLn
         wbln = (ibln-0.5)*BLNsyz + BLNmin
         ephot = 12399./wbln
         ibn = INT((ephot-BINmin)/BINsyz + 1)
         de = ephot - 12399./(wbln+BLNsyz)
         IF ( ibn.GE.1 .AND. ibn.LE.NBIn ) BLN(ibln)
     &        = (BRMev(ibn)+RECev(ibn)+TUFev(ibn))*de/BINsyz
 100  CONTINUE
C
C  LINES
C
      DO 200 no = 1 , Num
         DO 150 l = 1 , 220
            IF ( l.EQ.l1(no) ) THEN
               ibln = INT((wli(1,no)-BLNmin)/BLNsyz + 1)
               IF ( ibln.GE.1 .AND. ibln.LE.NBLn ) BLN(ibln) = BLN(ibln)
     &              + 0.6666666*PWR(no,l)
               ibln = INT((wli(2,no)-BLNmin)/BLNsyz + 1)
               IF ( ibln.GE.1 .AND. ibln.LE.NBLn ) BLN(ibln) = BLN(ibln)
     &              + 0.333333*PWR(no,l)
            ELSEIF ( l.EQ.l2(no) ) THEN
               ibln = INT((wna(1,no)-BLNmin)/BLNsyz + 1)
               IF ( ibln.GE.1 .AND. ibln.LE.NBLn ) BLN(ibln) = BLN(ibln)
     &              + 0.666666*PWR(no,l)
               ibln = INT((wna(2,no)-BLNmin)/BLNsyz + 1)
               IF ( ibln.GE.1 .AND. ibln.LE.NBLn ) BLN(ibln) = BLN(ibln)
     &              + 0.33333*PWR(no,l)
            ELSE
               ibln = INT((WJ(no,l)-BLNmin)/BLNsyz + 1)
               IF ( ibln.GE.1 .AND. ibln.LE.NBLn ) BLN(ibln) = BLN(ibln)
     &              + PWR(no,l)
            ENDIF
 150     CONTINUE
 200  CONTINUE
      RETURN
      END
