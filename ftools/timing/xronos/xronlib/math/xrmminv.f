
C
      SUBROUTINE xrMMINV(ARRAY,NORDER,DET)
c
C  ls 17/9/88 Subroutine for matrix inversion in double precision
c            (from Bevington)
c
c
C  A(NORDER,NORDER) = matrix of coefficients (input = normal, output = i
C                       (input = normal, output = inverse)
C  NORDER = matrix order
C  DET = determinant of the matrix
C
C  Dimension ok up to order 10
C
C  from Bevington ;  7-6-83     (LS)
C
c      DOUBLE PRECISION ARRAY,AMAX,SAVE
c      DIMENSION ARRAY(10,10),IK(10),JK(10)
      real*8 array(10,10),amax,save
      real*4 det 
      integer*4 ik(10),jk(10),k,i,norder,j,l
10    DET=1.0
C
C find largest element array in input matrix
11    DO 100 K=1,NORDER
         AMAX=0.
21       DO 30 I=K,NORDER
         DO 30 J=K,NORDER
23       IF(DABS(AMAX)-DABS(ARRAY(I,J)))24,24,30
24         AMAX=ARRAY(I,J)
           IK(K)=I
           JK(K)=J
30       CONTINUE
C
C interchange rows and columns to put amax in ARRAY(K,K)
         IF(AMAX)41,32,41
32         DET=0.
          GO TO 140
41       I=IK(K)
         IF(I-K)21,51,43
43         DO 50 J=1,NORDER
             SAVE = ARRAY(K,J)
             ARRAY(K,J)=ARRAY(I,J)
             ARRAY(I,J)=-SAVE
50         CONTINUE
51         J=JK(K)
           IF(J-K)21,61,53
53           DO 60 I=1,NORDER
               SAVE=ARRAY(I,K)
               ARRAY(I,K)=ARRAY(I,J)
               ARRAY(I,J)=-SAVE
60           CONTINUE
C
C Accumulate elements of inverse matrix
61           DO 70 I=1,NORDER
               IF(I-K)63,70,63
63               ARRAY(I,K)=-ARRAY(I,K)/AMAX
70           CONTINUE
71           DO 80 I=1,NORDER
             DO 80 J=1,NORDER
               IF(I-K)74,80,74
74               IF(J-K)75,80,75
75                 ARRAY(I,J)=ARRAY(I,J)+ARRAY(I,K)*ARRAY(K,J)
80           CONTINUE
81           DO 90 J=1,NORDER
               IF(J-K)83,90,83
83               ARRAY(K,J)=ARRAY(K,J)/AMAX
90           CONTINUE
           ARRAY(K,K)=1./AMAX
100        DET=DET*AMAX
C
C Restore ordering of matrix
101        DO 130 L=1,NORDER
             K=NORDER-L+1
             J=IK(K)
             IF(J-K)111,111,105
105            DO 110 I=1,NORDER
                 SAVE=ARRAY(I,K)
                 ARRAY(I,K)=-ARRAY(I,J)
                 ARRAY(I,J)=SAVE
110            CONTINUE
111            I=JK(K)
               IF(I-K)130,130,113
113              DO 120 J=1,NORDER
                   SAVE=ARRAY(K,J)
                   ARRAY(K,J)=-ARRAY(I,J)
                   ARRAY(I,J)=SAVE
120              CONTINUE
130         CONTINUE
140    RETURN
       END
C
C
C
