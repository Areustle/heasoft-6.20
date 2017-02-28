c      SUBROUTINE FFA8(B,NPTS)
      subroutine xrfft8 (b,npts)
c
c Name adapted to xronos on 15/9/88 ls : single precision fft
c
C--- FAST FOURIER ANALYSISS:  RADIX 8 METHOD
C--- SEE BERGLAND IN IEEE TRANS. AUDIO & ELECTROACOUSTICS
C--- VOL. AU-17, NO. 2, PP138-144
C--- INPUT:
C--- B(NPTS)	The data to be transformed.  NPTS must be a power of 2.
C---
C--- OUTPUT:
C--- B		Replaced by its transform.  B(1) is the DC leval, B(2) is
C---		the high frequency term, B(odd index>1) imaginary term
C---		B(even index>2) real term.
C---
c      implicit real*8(a-h,o-z)
      INTEGER*4 npts,m,n,n8pow,nn,int,it
      REAL*4 b(npts)
c      DIMENSION B(NPTS)
C---
	M=NINT(LOG(FLOAT(NPTS))/LOG(2.))
      N=2**M
      N8POW=M/3
C
      IF(M-N8POW*3-1) 30,20,10
10         NN=4
           INT=N/NN
           CALL DR4TR1(INT,B(1),B(INT+1),B(2*INT+1),B(3*INT+1))
           GOTO 40
20       NN=2
           INT=N/NN
           CALL DR2TR1(INT,B(1),B(INT+1))
           GOTO 40
30       NN=1
C--- PERFORM RADIX 8 ITERATIONS
40      IF(N8POW.LE.0) GOTO 60
      DO 50 IT=1,N8POW
           NN=NN*8
           INT=N/NN
           CALL DR8TR1(INT,NN,B(1),B(INT+1),B(2*INT+1),B(3*INT+1),
     1          B(4*INT+1),B(5*INT+1),B(6*INT+1),B(7*INT+1),
     2          B(1),B(INT+1),B(2*INT+1),B(3*INT+1),B(4*INT+1),
     3          B(5*INT+1),B(6*INT+1),B(7*INT+1))
50         CONTINUE
60    CALL DORD11(M,B)
      CALL DORD21(M,B)
70    RETURN
      END
C*********
      SUBROUTINE DR2TR1(INT,B0,B1)
c      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER*4 k,int
      REAL*4 b0(int),b1(int),t
c            DIMENSION B0(2),B1(2)
            DO 100 K=1,INT
                  T=B0(K)+B1(K)
                  B1(K)=B0(K)-B1(K)
                  B0(K)=T
100               CONTINUE
            RETURN
      END
C*********
      SUBROUTINE DR4TR1(INT,B0,B1,B2,B3)
c      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER*4 k,int
      REAL*4 b0(int),b1(int),b2(int),b3(int),r0,r1
c      DIMENSION B0(2),B1(2),B2(2),B3(2)
      DO 200 K=1,INT
           R0=B0(K)+B2(K)
           R1=B1(K)+B3(K)
           B2(K)=B0(K)-B2(K)
           B3(K)=B1(K)-B3(K)
           B0(K)=R0+R1
200         B1(K)=R0-R1
      RETURN
      END
C*********
      SUBROUTINE DR8TR1(INT,NN,BR0,BR1,BR2,BR3,BR4,BR5,BR6,BR7,
     1     BI0,BI1,BI2,BI3,BI4,BI5,BI6,BI7)
c      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER*4 l(15),nn,k,ji,jl,jr,j1,l1,j2,l2,j3,l3,j4,l4,j5,l5,j6
     &         ,l6,j7,l7,j8,l8,j9,l9,j10,l10,j11,l11,j12,l12,j13
     &         ,l13,j14,l14,jthet,l15,int,k0,kl,int8,j0,jlast,j,ks
      REAL*4  piovn,p7,pr,pi,tr0,ti0,tr2,ti2,tr1,ti1,tr3,ti3,
     &        tr4,ti4,tr5,ti5,tr6,ti6,tr7,ti7,
     &        c22,s22,th2,t0,t1,t2,t3,t4,t5,t6,t7,arg
      REAL*4  bi2(int),bi6(int),bi0(int),bi4(int),bi3(int),bi7(int),
     &        bi1(int),bi5(int),br0(int),br4(int),br1(int),br5(int),
     &        br2(int),br6(int),br3(int),br7(int)
      REAL*4  c1,s1,c2,s2,c3,s3,c4,s4,c5,s5,c6,s6,c7,s7
c      DIMENSION L(15),BR0(2),BR1(2),BR2(2),BR3(2),BR4(2),BR5(2)
c     1     ,BR6(2),BR7(2),BI0(2),BI1(2),BI2(2),BI3(2),BI4(2),BI5(2),
c     2     BI6(2),BI7(2)
      EQUIVALENCE (L15,L(1)),(L14,L(2)),(L13,L(3)),(L12,L(4)),
     1     (L11,L(5)),(L10,L(6)),(L9,L(7)),(L8,L(8)),(L7,L(9)),
     2     (L6,L(10)),(L5,L(11)),(L4,L(12)),(L3,L(13)),(L2,L(14)),
     3     (L1,L(15))
      L(1)=NN/8
      DO 10 K=2,15
           IF(L(K-1)-2) 11,12,13
11               L(K-1)=2
12           L(K)=2
             GOTO 10
13         L(K)=L(K-1)/2
10         CONTINUE
      PIOVN=3.14159265358979323846D0/FLOAT(NN)
      P7=0.70710678118654752440D0
      C22=.92387953251128675613D0
      S22=.38268343236508977172D0
      JI=3
      JL=2
      JR=2
C
      DO 70 J1=2,L1,2
      DO 70 J2=J1,L2,L1
      DO 70 J3=J2,L3,L2
      DO 70 J4=J3,L4,L3
      DO 70 J5=J4,L5,L4
      DO 70 J6=J5,L6,L5
      DO 70 J7=J6,L7,L6
      DO 70 J8=J7,L8,L7
      DO 70 J9=J8,L9,L8
      DO 70 J10=J9,L10,L9
      DO 70 J11=J10,L11,L10
      DO 70 J12=J11,L12,L11
      DO 70 J13=J12,L13,L12
           DO 70 J14=J13,L14,L13
      DO 70 JTHET=J14,L15,L14
           TH2=JTHET-2
           IF(TH2) 71,71,76
71         DO 72 K=1,INT
                T0=BR0(K)+BR4(K)
                T1=BR1(K)+BR5(K)
                T2=BR2(K)+BR6(K)
                T3=BR3(K)+BR7(K)
               T4=BR0(K)-BR4(K)
                T5=BR1(K)-BR5(K)
                T6=BR2(K)-BR6(K)
                T7=BR3(K)-BR7(K)
                BR2(K)=T0-T2
                BR3(K)=T1-T3
                T0=T0+T2
                T1=T1+T3
                BR0(K)=T0+T1
                BR1(K)=T0-T1
                PR=P7*(T5-T7)
                PI=P7*(T5+T7)
                BR4(K)=T4+PR
                BR7(K)=T6+PI
                BR6(K)=T4-PR
                BR5(K)=PI-T6
72         CONTINUE
           IF(NN-8) 70,70,73
73         K0=INT*8+1
           KL=K0+INT-1
           DO 75 K=K0,KL
                PR=P7*(BI2(K)-BI6(K))
                PI=P7*(BI2(K)+BI6(K))
                TR0=BI0(K)+PR
                TI0=BI4(K)+PI
                TR2=BI0(K)-PR
                TI2=BI4(K)-PI
                PR=P7*(BI3(K)-BI7(K))
                PI=P7*(BI3(K)+BI7(K))
                TR1=BI1(K)+PR
                TI1=BI5(K)+PI
                TR3=BI1(K)-PR
                TI3=BI5(K)-PI
                PR=TR1*C22-TI1*S22
                PI=TI1*C22+TR1*S22
                BI0(K)=TR0+PR
                BI6(K)=TR0-PR
                BI7(K)=PI+TI0
                BI1(K)=PI-TI0
                PR=-TR3*S22-TI3*C22
                PI= TR3*C22-TI3*S22
                BI2(K)=TR2+PR
                BI4(K)=TR2-PR
                BI5(K)=TI2+PI
                BI3(K)=PI-TI2
75              CONTINUE
           GOTO 70
76         ARG=TH2*PIOVN
           C1=COS(ARG)
           S1=SIN(ARG)
           C2=C1*C1-S1*S1
           S2=C1*S1*2.0D0
           C3=C1*C2-S1*S2
           S3=C2*S1+S2*C1
           C4=C2*C2-S2*S2
           S4=C2*S2*2.0D0
           C5=C2*C3-S2*S3
           S5=C3*S2+S3*C2
           C6=C3*C3-S3*S3
           S6=C3*S3*2.0D0
           C7=C3*C4-S3*S4
           S7=C4*S3+S4*C3
           INT8=INT*8
           J0=JR*INT8+1
          K0=JI*INT8+1
           JLAST=J0+INT-1
           DO 77 J=J0,JLAST
                KS=K0+J-J0
                DO 77 K=KS,KS
                TR1=BR1(J)*C1-BI1(K)*S1
                TI1=BR1(J)*S1+BI1(K)*C1
                TR2=BR2(J)*C2-BI2(K)*S2
                TI2=BR2(J)*S2+BI2(K)*C2
                TR3=BR3(J)*C3-BI3(K)*S3
                TI3=BR3(J)*S3+BI3(K)*C3
                TR4=BR4(J)*C4-BI4(K)*S4
                TI4=BR4(J)*S4+BI4(K)*C4
                TR5=BR5(J)*C5-BI5(K)*S5
                TI5=BR5(J)*S5+BI5(K)*C5
                TR6=BR6(J)*C6-BI6(K)*S6
                TI6=BR6(J)*S6+BI6(K)*C6
                TR7=BR7(J)*C7-BI7(K)*S7
                TI7=BR7(J)*S7+BI7(K)*C7
                T0=BR0(J)+TR4
                T1=BI0(K)+TI4
                TR4=BR0(J)-TR4
                TI4=BI0(K)-TI4
                T2=TR1+TR5
                T3=TI1+TI5
                TR5=TR1-TR5
                TI5=TI1-TI5
                T4=TR2+TR6
                T5=TI2+TI6
                TR6=TR2-TR6
                TI6=TI2-TI6
                T6=TR3+TR7
                T7=TI3+TI7
                TR7=TR3-TR7
                TI7=TI3-TI7
                TR0=T0+T4
                TI0=T1+T5
                TR2=T0-T4
                TI2=T1-T5
                TR1=T2+T6
                TI1=T3+T7
             TR3=T2-T6
                TI3=T3-T7
                T0=TR4-TI6
                T1=TI4+TR6
                T4=TR4+TI6
                T5=TI4-TR6
                T2=TR5-TI7
                T3=TI5+TR7
                T6=TR5+TI7
                T7=TI5-TR7
                BR0(J)=TR0+TR1
                BI7(K)=TI0+TI1
                BI6(K)=TR0-TR1
                BR1(J)=TI1-TI0
                BR2(J)=TR2-TI3
                BI5(K)=TI2+TR3
                BI4(K)=TR2+TI3
                BR3(J)=TR3-TI2
                PR=P7*(T2-T3)
                PI=P7*(T2+T3)
                BR4(J)=T0+PR
                BI3(K)=T1+PI
                BI2(K)=T0-PR
                BR5(J)=PI-T1
                PR=-P7*(T6+T7)
                PI=P7*(T6-T7)
                BR6(J)=T4+PR
                BI1(K)=T5+PI
                BI0(K)=T4-PR
                BR7(J)=PI-T5
77         CONTINUE
           JR=JR+2
              JI=JI-2
           IF(JI-JL) 78,78,70
78              JI=2*JR-1
                JL=JR
70     CONTINUE
      RETURN
      END
C*********
      SUBROUTINE DORD11(M,B)
c      IMPLICIT REAL*8(A-H,O-Z)
C           INPLACE REORDERING SUBROUTINES, DORD1, AND DORD2
            INTEGER*4 k,kl,n,m,j
            REAL*4 t,b(*)  
c            DIMENSION B(2)
            K=4
            KL=2
            N=2**M
            DO 94 J=4,N,2
                  IF(K-J)92,92,91
91                      T=B(J)
                        B(J)=B(K)
                        B(K)=T
92                K=K-2
                  IF(K-KL) 93,93,94
93                      K=2*J
                        KL=J
94                CONTINUE
            RETURN
      END
C*********
      SUBROUTINE DORD21(M,B)
c      IMPLICIT REAL*8(A-H,O-Z)
C                 THE SECOND INPLACE REORDERING ROUTINE
       INTEGER*4  l(20),n,m,k,ij,j1,j2,j3,j4,j5,j6,j7,j8,j9,j10,
     &            j11,j12,j13,j14,j15,j16,j17,j18,ji,l1,l2,l3,l4,l5,
     &            l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19
       REAL*4 b(*),t
c            DIMENSION L(20),B(2)
      EQUIVALENCE (L19,L(1)),(L18,L(2)),(L17,L(3)),(L16,L(
     1    4)),(L15,L(5)),(L14,L(6)),(L13,L(7)),(L12,L(8)),(L11,L(
     1      9)),(L10,L(10)),(L9,L(11)),(L8,L(12)),(L7,L(13)),(L6,L(
     1      14)),(L5,L(15)),(L4,L(16)),(L3,L(17)),(L2,L(18)),(L1,L(19))
            N=2**M
100         L(1)=N
            DO 101 K=2,M
101               L(K)=L(K-1)/2
            DO 102 K=M,19
102               L(K+1)=2
            IJ=2
            DO 103 J1=2,L1,2
             DO 103 J2=J1,L2,L1
              DO 103 J3=J2,L3,L2
               DO 103 J4=J3,L4,L3
                DO 103 J5=J4,L5,L4
                 DO 103 J6=J5,L6,L5
                  DO 103 J7=J6,L7,L6
                   DO 103 J8=J7,L8,L7
                    DO 103 J9=J8,L9,L8
                     DO 103 J10=J9,L10,L9
                      DO 103 J11=J10,L11,L10
                       DO 103 J12=J11,L12,L11
                        DO 103 J13=J12,L13,L12
                         DO 103 J14=J13,L14,L13
                          DO 103 J15=J14,L15,L14
                           DO 103 J16=J15,L16,L15
                            DO 103 J17=J16,L17,L16
                             DO 103 J18=J17,L18,L17
                              DO 103 JI=J18,L19,L18
                                    IF(IJ-JI) 108,103,103
108                                       T=B(IJ-1)
                                          B(IJ-1)=B(JI-1)
                                          B(JI-1)=T
                                          T=B(IJ)
                                          B(IJ)=B(JI)
                                          B(JI)=T
103                                 IJ=IJ+2
            RETURN
      END
