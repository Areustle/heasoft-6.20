
      REAL FUNCTION ERF(X)
      REAL X, GAMMP
      IF(X.LT.0.)THEN
        ERF=-GAMMP(.5,X**2)
      ELSE
        ERF=GAMMP(.5,X**2)
      ENDIF
      RETURN
      END


      REAL FUNCTION ERFC(X)
      REAL X, GAMMP, GAMMQ
      IF(X.LT.0.)THEN
        ERFC=1.+GAMMP(.5,X**2)
      ELSE
        ERFC=GAMMQ(.5,X**2)
      ENDIF
      RETURN
      END

      REAL FUNCTION GAMMP(A,X)
      REAL A, X, GLN, GAMSER, GAMMCF
      IF ( X .LT. 0. ) CALL xwrite('GAMMP: X < 0', 10)
      IF ( A .LE. 0. ) CALL xwrite('GAMMP: A =< 0', 10)
      IF(X.LT.A+1.)THEN
        CALL GSER(GAMSER,A,X,GLN)
        GAMMP=GAMSER
      ELSE
        CALL GCF(GAMMCF,A,X,GLN)
        GAMMP=1.-GAMMCF
      ENDIF
      RETURN
      END

      REAL FUNCTION GAMMQ(A,X)
      REAL A, X, GLN, GAMSER, GAMMCF
      IF ( X .LT. 0. ) CALL xwrite('GAMMQ: X < 0', 10)
      IF ( A .LE. 0. ) CALL xwrite('GAMMQ: A =< 0', 10)
      IF(X.LT.A+1.)THEN
        CALL GSER(GAMSER,A,X,GLN)
        GAMMQ=1.-GAMSER
      ELSE
        CALL GCF(GAMMCF,A,X,GLN)
        GAMMQ=GAMMCF
      ENDIF
      RETURN
      END

      SUBROUTINE GSER(GAMSER,A,X,GLN)
      REAL GAMSER, A, X, GLN
      REAL EPS, AP, SUM, DEL, GAMMLN
      INTEGER ITMAX, N
      PARAMETER (ITMAX=10000,EPS=3.E-7)
      GLN=GAMMLN(A)
      IF(X.LE.0.)THEN
        IF ( X .LT. 0. ) CALL xwrite('GSER: X < 0', 10)
        GAMSER=0.
        RETURN
      ENDIF
      AP=A
      SUM=1./A
      DEL=SUM
      DO 11 N=1,ITMAX
        AP=AP+1.
        DEL=DEL*X/AP
        SUM=SUM+DEL
        IF(ABS(DEL).LT.ABS(SUM)*EPS)GO TO 1
11    CONTINUE
      CALL xwrite(
     & 'GSER: too many iterations before required accuracy achieved', 
     & 10)
1     GAMSER=SUM*EXP(-X+A*LOG(X)-GLN)
      RETURN
      END

      SUBROUTINE GCF(GAMMCF,A,X,GLN)
      REAL GAMMCF, A, X, GLN
      REAL EPS, GOLD, A0, A1, B0, B1, FAC
      REAL ANF, AN, ANA, G, GAMMLN
      INTEGER ITMAX, N
      PARAMETER (ITMAX=10000,EPS=3.E-7)
      GLN=GAMMLN(A)
      GOLD=0.
      A0=1.
      A1=X
      B0=0.
      B1=1.
      FAC=1.
      DO 11 N=1,ITMAX
        AN=FLOAT(N)
        ANA=AN-A
        A0=(A1+A0*ANA)*FAC
        B0=(B1+B0*ANA)*FAC
        ANF=AN*FAC
        A1=X*A0+ANF*A1
        B1=X*B0+ANF*B1
        IF(A1.NE.0.)THEN
          FAC=1./A1
          G=B1*FAC
          IF(ABS((G-GOLD)/G).LT.EPS)GO TO 1
          GOLD=G
        ENDIF
11    CONTINUE
      CALL xwrite(
     & 'GCF: too many iterations before required accuracy achieved', 
     & 10)
1     GAMMCF=EXP(-X+A*ALOG(X)-GLN)*G
      RETURN
      END

      REAL FUNCTION FACTRL(N)
      INTEGER N
      REAL A(33), GAMMLN
      INTEGER NTOP, J
      DATA NTOP,A(1)/0,1./
      IF (N.LT.0) THEN
        CALL xwrite('FACTRL: N < 0', 10)
      ELSE IF (N.LE.NTOP) THEN
        FACTRL=A(N+1)
      ELSE IF (N.LE.32) THEN
        DO 11 J=NTOP+1,N
          A(J+1)=J*A(J)
11      CONTINUE
        NTOP=N
        FACTRL=A(N+1)
      ELSE
        FACTRL=EXP(GAMMLN(N+1.))
      ENDIF
      RETURN
      END

