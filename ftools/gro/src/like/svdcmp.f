c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE SVDCMP(A,M,N,MP,NP,W,V)
C
C
C  $Id: svdcmp.f,v 1.2 2013/05/21 19:08:27 irby Exp $
c
C=======================================================================
C
C       NUMERICAL RECIPES code used by EGRET LIKE point source analysis
C=======================================================================
C++        EFFECT:
C++        Adapted from Numerical Recipes, Press etal., 1992,
C++        for the EGRET LIKE program. When modifyed, the original code
C++        is left as a comment. Press etal. have given the Compton SSC
C++	   a limited license to distribute these routines for use with
C++	   the EGRET LIKE program. NEITHER THIS FILE NOR ANY OF ITS
C++	   CONTENTS ARE TO PLACED IN AN ANONYMOUS FTP DIRECTORY, NOR
C++	   OTHERWISE BE MADE PUBLIC. To do so would violate the 
C++	   copyright of Press etal. 
c
c Description:	
C
C
C
C
C  $Log: svdcmp.f,v $
C  Revision 1.2  2013/05/21 19:08:27  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:44  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:33  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:32  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE SVDCMP(A,M,N,MP,NP,W,V)

C     Common blocks used:
      include '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/errrep.copy'

      save

      character(80) id
      common /id/id
      PARAMETER (NMAX=100)
      DIMENSION A(MP,NP),W(NP),V(NP,NP),RV1(NMAX)

      id = '$Id: svdcmp.f,v 1.2 2013/05/21 19:08:27 irby Exp $'


      G=0.0
      SCALE=0.0
      ANORM=0.0

      DO 25 I=1,N
         L=I+1
         RV1(I)=SCALE*G
         G=0.0
         S=0.0
         SCALE=0.0
         IF (I.LE.M) THEN
            DO 11 K=I,M
               SCALE=SCALE+ABS(A(K,I))
 11         CONTINUE
            IF (SCALE.NE.0.0) THEN
               DO 12 K=I,M
                  A(K,I)=A(K,I)/SCALE
                  S=S+A(K,I)*A(K,I)
 12            CONTINUE
               F=A(I,I)
               G=-SIGN(SQRT(S),F)
               H=F*G-S
               A(I,I)=F-G
               IF (I.NE.N) THEN
                  DO 15 J=L,N
                     S=0.0
                     DO 13 K=I,M
                        S=S+A(K,I)*A(K,J)
 13                  CONTINUE
                     F=S/H
                     DO 14 K=I,M
                        A(K,J)=A(K,J)+F*A(K,I)
 14                  CONTINUE
 15               CONTINUE
               ENDIF
               DO 16 K= I,M
                  A(K,I)=SCALE*A(K,I)
 16            CONTINUE
            ENDIF
         ENDIF
         W(I)=SCALE *G
         G=0.0
         S=0.0
         SCALE=0.0
         IF ((I.LE.M).AND.(I.NE.N)) THEN
            DO 17 K=L,N
               SCALE=SCALE+ABS(A(I,K))
 17         CONTINUE
            IF (SCALE.NE.0.0) THEN
               DO 18 K=L,N
                  A(I,K)=A(I,K)/SCALE
                  S=S+A(I,K)*A(I,K)
 18            CONTINUE
               F=A(I,L)
               G=-SIGN(SQRT(S),F)
               H=F*G-S
               A(I,L)=F-G
               DO 19 K=L,N
                  RV1(K)=A(I,K)/H
 19            CONTINUE
               IF (I.NE.M) THEN
                  DO 23 J=L,M
                     S=0.0
                     DO 21 K=L,N
                        S=S+A(J,K)*A(I,K)
 21                  CONTINUE
                     DO 22 K=L,N
                        A(J,K)=A(J,K)+S*RV1(K)
 22                  CONTINUE
 23               CONTINUE
               ENDIF
               DO 24 K=L,N
                  A(I,K)=SCALE*A(I,K)
 24            CONTINUE
            ENDIF
         ENDIF
         ANORM=MAX(ANORM,(ABS(W(I))+ABS(RV1(I))))
 25   CONTINUE
      DO 32 I=N,1,-1
         IF (I.LT.N) THEN
            IF (G.NE.0.0) THEN
               DO 26 J=L,N
                  V(J,I)=(A(I,J)/A(I,L))/G
 26            CONTINUE
               DO 29 J=L,N
                  S=0.0
                  DO 27 K=L,N
                     S=S+A(I,K)*V(K,J)
 27               CONTINUE
                  DO 28 K=L,N
                     V(K,J)=V(K,J)+S*V(K,I)
 28               CONTINUE
 29            CONTINUE
            ENDIF
            DO 31 J=L,N
               V(I,J)=0.0
               V(J,I)=0.0
 31         CONTINUE
         ENDIF
         V(I,I)=1.0
         G=RV1(I)
         L=I
 32   CONTINUE
      DO 39 I=N,1,-1
         L=I+1
         G=W(I)
         IF (I.LT.N) THEN
            DO 33 J=L,N
               A(I,J)=0.0
 33         CONTINUE
         ENDIF
         IF (G.NE.0.0) THEN
            G=1.0/G
            IF (I.NE.N) THEN
               DO 36 J=L,N
                  S=0.0
                  DO 34 K=L,M
                     S=S+A(K,I)*A(K,J)
 34               CONTINUE
                  F=(S/A(I,I))*G
                  DO 35 K=I,M
                     A(K,J)=A(K,J)+F*A(K,I)
 35               CONTINUE
 36            CONTINUE
            ENDIF
            DO 37 J=I,M
               A(J,I)=A(J,I)*G
 37         CONTINUE
         ELSE
            DO 38 J= I,M
               A(J,I)=0.0
 38         CONTINUE
         ENDIF
         A(I,I)=A(I,I)+1.0
 39   CONTINUE
      DO 49 K=N,1,-1
         DO 48 ITS=1,30
            DO 41 L=K,1,-1
               NM=L-1
               IF ((ABS(RV1(L))+ANORM).EQ.ANORM)  GO TO 2
               IF ((ABS(W(NM))+ANORM).EQ.ANORM)  GO TO 1
 41         CONTINUE
 1          C=0.0
            S=1.0
            DO 43 I=L,K
               F=S*RV1(I)
               IF ((ABS(F)+ANORM).NE.ANORM) THEN
                  G=W(I)
                  H=SQRT(F*F+G*G)
                  W(I)=H
                  H=1.0/H
                  C= (G*H)
                  S=-(F*H)
                  DO 42 J=1,M
                     Y=A(J,NM)
                     Z=A(J,I)
                     A(J,NM)=(Y*C)+(Z*S)
                     A(J,I)=-(Y*S)+(Z*C)
 42               CONTINUE
               ENDIF
 43         CONTINUE
 2          Z=W(K)
            IF (L.EQ.K) THEN
               IF (Z.LT.0.0) THEN
                  W(K)=-Z
                  DO 44 J=1,N
                     V(J,K)=-V(J,K)
 44               CONTINUE
               ENDIF
               GO TO 3
            ENDIF
c     IF (ITS.EQ.30) PAUSE 'No convergence of in 30 iterations'
            IF (ITS.EQ.30) then
               signal='S'
               sigmsg=
     &              'ERRMAP:No convergence of SVD in 30 iterations'
               return
            ENDIF
            X=W(L)
            NM=K-1
            Y=W(NM)
            G=RV1(NM)
            H=RV1(K)
            F=((Y-Z)*(Y+Z)+(G-H)*(G+H))/(2.0*H*Y)
            G=SQRT(F*F+1.0)
            F=((X-Z)*(X+Z)+H*((Y/(F+SIGN(G,F)))-H))/X
            C=1.0
            S=1.0
            DO 47 J=L,NM
               I=J+1
               G=RV1(I)
               Y=W(I)
               H=S*G
               G=C*G
               Z=SQRT(F*F+H*H)
               RV1(J)=Z
               C=F/Z
               S=H/Z
               F= (X*C)+(G*S)
               G=-(X*S)+(G*C)
               H=Y*S
               Y=Y*C
               DO 45 NM=1,N
                  X=V(NM,J)
                  Z=V(NM,I)
                  V(NM,J)= (X*C)+(Z*S)
                  V(NM,I)=-(X*S)+(Z*C)
 45            CONTINUE
               Z=SQRT(F*F+H*H)
               W(J)=Z
               IF (Z.NE.0.0) THEN
                  Z=1.0/Z
                  C=F*Z
                  S=H*Z
               ENDIF
               F= (C*G)+(S*Y)
               X=-(S*G)+(C*Y)
               DO 46 NM=1,M
                  Y=A(NM,J)
                  Z=A(NM,I)
                  A(NM,J)= (Y*C)+(Z*S)
                  A(NM,I)=-(Y*S)+(Z*C)
 46            CONTINUE
 47         CONTINUE
            RV1(L)=0.0
            RV1(K)=F
            W(K)=X
 48      CONTINUE
 3       CONTINUE
 49   CONTINUE
      RETURN
      END
c
