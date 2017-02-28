**==effnew.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION EFFNEW(I,T,Z,N)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL EFFNEW , f1 , f2 , f3 , T , t3 , xx , Z
      INTEGER I , N
C*** End of declarations inserted by SPAG
      
      EFFNEW =0.
 
      t3 = .001*T/Z**2
      xx = .4342*LOG(t3)
      IF ( t3.LE.1. ) THEN
         f1 = .266
         f2 = .13
         f3 = .13
      ELSEIF ( (t3-1.E5).GE.0. ) THEN
         f1 = .80
         f2 = .71
         f3 = .07
      ELSE
         f1 = .266 + .1068*xx - .074*SIN(1.2566*xx)
         f2 = .130 + .1160*xx - .074*SIN(1.2566*xx)
         f3 = .130 - .012*xx + .05*EXP(-(xx-2.)*(xx-2.))
      ENDIF
      IF ( I.EQ.2 .OR. I.EQ.3 ) EFFNEW = 8.*(1.-f1)
      IF ( I.EQ.10 .OR. I.EQ.11 ) EFFNEW = 18.*(1.-f2)
      IF ( I.GE.12 .AND. I.LE.17 ) EFFNEW = 18.*(1.-3.*f3-f2)
      IF ( I.GE.18 ) EFFNEW = (28.-N+Z)*1.8*(1.-3.*f3-f2)
      RETURN
      END
 
