**==seaton.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION SEATON(X,Q)
C     R. MOORE OCTOBER 1976
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      DOUBLE PRECISION a10 , a11 , a12 , a13 , a14 , a15 , a20 , a21
      DOUBLE PRECISION a22 , a23 , a24 , a25 , a30 , a31 , a32 , a33 
      DOUBLE PRECISION a34 , a35 , b10 , b11, b12 , b13 , b14 , b20
      DOUBLE PRECISION b21 , b22 , b23 , b24 , b30 , b31 , b32 , b33
      DOUBLE PRECISION b34
      REAL EXINT1 , Q , SEATON , X
C*** End of declarations inserted by SPAG
      DOUBLE PRECISION xs1 , xs2 , x3
      DATA a10/8.7469604697013D-4/ , b10/ - 1.2007397521051D-4/
      DATA a11/.20406771231267D0/ , b11/ - .055223640825293D0/
      DATA a12/ - 2.1524482972354D0/ , b12/.029171138841798D0/
      DATA a13/12.663578339302D0/ , b13/.25091093604147D0/
      DATA a14/ - 43.153566859883D0/ , b14/ - .94344532109356D0/
      DATA a15/61.113098339262D0/
      DATA a20/.011834608639468D0/ , b20/ - 1.2467753947278D-3/
      DATA a21/ - 2.9889195903436D-3/ , b21/ - .043871158058636D0/
      DATA a22/ - .10946027271945D0/ , b22/.013617064094285D0/
      DATA a23/.097410292577482D0/ , b23/ - 5.2824884665512D-3/
      DATA a24/ - .039676727608179D0/ , b24/8.9490487211065D-4/
      DATA a25/6.2318768197420D-3/
      DATA a30/.018985613015081D0/ , b30/ - .010963734653233D0/
      DATA a31/ - .064707516794785D0/ , b31/ - .028119928428050D0/
      DATA a32/6.2172804659938D-3/ , b32/1.1971209805431D-3/
      DATA a33/ - 4.1777961107942D-4/ , b33/ - 4.8739343472085D-5/
      DATA a34/1.5264422582645D-5/ , b34/8.4274360230135D-7/
      DATA a35/ - 2.2708774951499D-7/
      IF ( X.GE.20. ) THEN
         x3 = 3.*X
         xs1 = -
     &         .1728*X**.33333333333333*(1.+(-8.+(70.+(-800.+11440./x3)/
     &         x3)/x3)/x3)
         xs2 = -.0496*X**.66666666666667*(1.+(-3.+(32.-448./x3)/x3)/x3)
      ELSEIF ( X.GT.2. ) THEN
         xs1 = a30 + (a31+(a32+(a33+(a34+a35*X)*X)*X)*X)*X
         xs2 = b30 + (b31+(b32+(b33+b34*X)*X)*X)*X
      ELSEIF ( X.GT.0.2 ) THEN
         xs1 = a20 + (a21+(a22+(a23+(a24+a25*X)*X)*X)*X)*X
         xs2 = b20 + (b21+(b22+(b23+b24*X)*X)*X)*X
      ELSEIF ( X.GT.0.02 ) THEN
         xs1 = a10 + (a11+(a12+(a13+(a14+a15*X)*X)*X)*X)*X
         xs2 = b10 + (b11+(b12+(b13+b14*X)*X)*X)*X
      ELSE
         xs1 = .4629*X*(1.+4.*X)
     &         - 1.0368*X**1.3333333333333*(1.+1.875*X)
         xs2 = -.0672*X*(1.+3.*X) + .1488*X**1.6666666666667*(1.+1.8*X)
      ENDIF
      x3 = X**.33333333333333*Q**.66666666666667
      SEATON = SNGL(EXINT1(X,3) + (xs1+xs2/x3)/x3)
      RETURN
      END
**==p.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
 
      FUNCTION P(Y,Q)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , EXINT1 , P , Q , Y
C*** End of declarations inserted by SPAG
      a = Y/(Q*Q)
      IF ( a.GT.75 ) THEN
         P = 1./Q
         RETURN
      ENDIF
      P = a*(1.-EXINT1(a,3))/Q
      RETURN
      END
**==gnd.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
 
      FUNCTION GND(Num)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL GND
      INTEGER Num
C*** End of declarations inserted by SPAG
      GND = 4.
      IF ( Num.LT.28 ) GND = 3.
      IF ( Num.LT.10 ) GND = 2.
      IF ( Num.LT.2 ) GND = 1.
      RETURN
      END
**==exint1.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
 
      FUNCTION EXINT1(X,Jump)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL EXINT1 , X , x2 , x3 , x4
      INTEGER Jump
C*** End of declarations inserted by SPAG
C     R. MOORE OCTOBER 1976
C   JUMP=1    EXINT1=E1(X)
C   JUMP=2    EXINT1=EXP(X)*E1(X)
C   JUMP=3    EXINT1=X*EXP(X)*E1(X)
      IF ( X.GE.1. ) THEN
         x2 = X*X
         x3 = x2*X
         x4 = x3*X
         EXINT1 = SNGL((x4+8.5733287401D0*x3+18.059016973D0*x2+
     &            8.6347608925D0*X+.2677737343D0)
     &            /(x4+9.5733223454D0*x3+25.6329561486D0*x2+
     &            21.0996530827D0*X+3.9584969228D0))
         IF ( Jump.EQ.2 ) THEN
            EXINT1 = EXINT1/X
         ELSEIF ( Jump.NE.3 ) THEN
            EXINT1 = EXINT1*EXP(-X)/X
            RETURN
         ENDIF
      ELSE
         EXINT1 = SNGL(((((((((7.122452D-7*X-1.766345D-6)*X+
     &            2.928433D-5)*X-
     &            .0002335379D0)*X+.001664156D0)*X-.01041576D0)
     &            *X+.05555682D0)*X-.2500001D0)*X+.9999999D0)*X - LOG(X)
     &            - .57721566490153D0)
         IF ( Jump.EQ.1 ) THEN
         ELSEIF ( Jump.EQ.3 ) THEN
            EXINT1 = X*EXP(X)*EXINT1
            RETURN
         ELSE
            EXINT1 = EXP(X)*EXINT1
            RETURN
         ENDIF
      ENDIF
      RETURN
      END
**==etwo.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
 
      FUNCTION ETWO(X)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL ETWO , EXINT1 , X
C*** End of declarations inserted by SPAG
      ETWO = EXP(-X) - X*EXINT1(X,1)
      RETURN
      END
