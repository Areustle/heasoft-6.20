**==adh.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      FUNCTION ADH(L,N,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , ADH , ex1 , ex2 , ex3 , p , T , z , z2 , z3 , z4 , zpt
      INTEGER L , N
C*** End of declarations inserted by SPAG
C  DIELECTRONIC RECOMBINATION TO HE-LIKE EXCITED STATES
C  USING MEWE&SCHRIJVER
      p = 33*(N-1.)**0.6*T**(-0.3)
      z = N
      z2 = z*z
      z3 = z*z2
      z4 = z*z3
      zpt = (z+.5)*(z+.5)
      a = 6.46E-8*z4*T**(-1.5)
      ex1 = EXP(-78900.*zpt/T)
      ex2 = EXP(-101800.*z2/T)
      ex3 = EXP(-118400.*z2/T)
      IF ( L.EQ.2 ) THEN
         ADH = a*(9.*ex1/(1.+7.E-5*z4)+27.*ex2/(1.+8.0E-5*z4)
     &         +380.*ex3/((1.+p)*(1.+5.0E-3*z3)))
         RETURN
      ELSEIF ( L.EQ.3 ) THEN
         ADH = a*(18.*ex1/9.5+54*ex2/(1.+1.9E-4*z4)
     &         +380.*ex3*p/((1.+p)*(1.+5.0E-3*z3)))
         RETURN
      ELSEIF ( L.NE.4 ) THEN
         ADH = a*(12.*ex1/(1.+6.E-6*z4)+18.*ex2/(1.+3.0E-5*z4)
     &         +69.*ex3/(1.+5.0E-3*z3))
         RETURN
      ENDIF
      ADH = a*(3.*ex1/(1.+3.0E-6*z4)+0.5*ex2/(1.+2.2E-5*z4)
     &      +6.3*ex3/(1.+5.0E-3*z3))
      RETURN
      END
 
