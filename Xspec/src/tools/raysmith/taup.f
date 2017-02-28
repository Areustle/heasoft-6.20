**==taup.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION TAUP(W,It)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      INTEGER It
      REAL RF , TAU , TAUhe , TAUP , TPLus , W
C*** End of declarations inserted by SPAG
      COMMON /PT    / RF(500) , TAU(150) , TAUhe(150) , TPLus(150)
      TAUP = 1.E-8
      IF ( W.GT.912. ) RETURN
      TAUP = TAUP + TAU(It)*(W/912.)**3
      IF ( W.LT.504.6 ) TAUP = TAUP + TAUhe(It)
     &                         *(.763*(W/504.6)**1.99+.237*(W/504.6)
     &                         **2.99)
      IF ( W.LT.228. ) TAUP = TAUP + TPLus(It)*(W/228.)**3
      RETURN
      END
 
