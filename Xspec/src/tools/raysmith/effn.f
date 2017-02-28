**==effn.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION EFFN(I,Zeff,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL EFFN , eye , f1 , f2 , f3 , T , t3 , xx , Zeff
      INTEGER I , no
C*** End of declarations inserted by SPAG
      t3 = T/(Zeff*Zeff*1000.0)
      xx = 0.4342*LOG(t3)
      IF ( t3.LE.1.0 ) THEN
         f1 = 0.266
         f2 = 0.13
         f3 = 0.13
      ELSEIF ( t3.LT.10.**5 ) THEN
         f1 = 0.266 + 0.1068*xx - 0.074*SIN(1.2566*xx)
         f2 = 0.130 + 0.1160*xx - 0.074*SIN(1.2566*xx)
         f3 = 0.130 - 0.0120*xx + 0.050*EXP(-(xx-2.)*(xx-2.))
      ELSE
         f1 = 0.80
         f2 = 0.71
         f3 = 0.07
      ENDIF
      IF ( I.NE.0 ) THEN
         IF ( I.GT.18 ) GOTO 200
         IF ( I.EQ.2 ) THEN
            EFFN = 8.0
         ELSEIF ( I.EQ.3 ) THEN
            EFFN = 8. - (4.*f1)
         ELSEIF ( I.EQ.4 ) THEN
            EFFN = 8.*(1.-f1)
         ELSEIF ( I.EQ.5 ) THEN
            EFFN = 6.6667*(1.-f1)
         ELSEIF ( I.EQ.6 ) THEN
            EFFN = 5.33333*(1.-f1)
         ELSEIF ( I.EQ.7 ) THEN
            EFFN = 4.*(1.-f1)
         ELSEIF ( I.EQ.8 ) THEN
            EFFN = 2.6667*(1.-f1)
         ELSEIF ( I.EQ.9 ) THEN
            EFFN = 1.33333*(1.-f1)
         ELSEIF ( I.EQ.10 ) THEN
            EFFN = 18.
         ELSEIF ( I.EQ.11 ) THEN
            EFFN = 18. - (9.*f2)
         ELSEIF ( I.EQ.12 ) THEN
            EFFN = 18.0*(1.-f2)
         ELSEIF ( I.EQ.13 ) THEN
            EFFN = 18.*(1.-f2) - 1.*(9.*f3)
         ELSEIF ( I.EQ.14 ) THEN
            EFFN = 18.*(1.-f2) - 2.*(9.*f3)
         ELSEIF ( I.EQ.15 ) THEN
            EFFN = 18.*(1.-f2) - 3.*(9.*f3)
         ELSEIF ( I.EQ.16 ) THEN
            EFFN = 18.*(1.-f2) - 4.*(9.*f3)
         ELSEIF ( I.EQ.17 ) THEN
            EFFN = 18.*(1.-f2) - 45.0*f3
         ELSEIF ( I.EQ.18 ) THEN
            GOTO 200
         ELSE
            GOTO 100
         ENDIF
         GOTO 300
      ENDIF
 100  eye = I
      EFFN = 2. - eye
      GOTO 300
 200  no = 28 - I
      EFFN = no*1.8*(1.-3.*f3-f2)
      IF ( EFFN.LT.0 ) THEN
         eye = I
         EFFN = 60. - eye
C     GUARD PACKAGE FOR I = 17
C     PROBABLY UNNECESSARY
         IF ( EFFN.LE.0 ) EFFN = 1.0
      ENDIF
 300  RETURN
      END
 
