**==twoph.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE TWOPH(Wav2,P2ph,Iii)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ABUnd , ABUnj , BINmin , BINsyz , BRMev , P2ph , r , RECev , 
     &     TUFev , Wav2
      INTEGER Iii , ik , imax , itmax , NBIn , NJ
C*** End of declarations inserted by SPAG
 
      INCLUDE 'rayspec.inc'
 
      COMMON /PARAMS/ NJ(12) , ABUnj(12) , ABUnd , BINmin , BINsyz , 
     &                NBIn
      COMMON /CONTIN/ BRMev(MXBINS) , RECev(MXBINS) , TUFev(MXBINS)

c suppress compiler warning
      ik = Iii

      IF ( P2ph.LE.1.E-10 ) RETURN
      itmax = INT((12399./Wav2-BINmin)/BINsyz)
      imax = MIN0(itmax,NBIn)
      IF ( imax.LE.1 ) RETURN
      DO 100 ik = 1 , imax
         r = (BINmin+BINsyz*(ik-.5))*Wav2/12399.
         TUFev(ik) = TUFev(ik) + 12.*P2ph*r*r*(1.-r)*Wav2*BINsyz/12399.
 100  CONTINUE
      RETURN
      END
