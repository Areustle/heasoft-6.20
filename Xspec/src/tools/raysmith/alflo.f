**==alflo.spg  processed by SPAG 4.50J  at 14:49 on 30 Jun 1995
      FUNCTION ALFLO(N,J,T)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , ALFLO , b , c , d , f , T , t4
      INTEGER iion , ij , J , N
C*** End of declarations inserted by SPAG
c  Low Temperature Dielectronic Recombination from Nussbaumer and
c  Storey: C - Si for 1000 - 60,000 K
 
      DIMENSION iion(8,16) , a(25) , b(25) , c(25) , d(25) , f(25)
 
      DATA iion/40*0 , 0 , 1 , 2 , 3 , 0 , 0 , 0 , 0 , 0 , 4 , 5 , 6 , 
     &     7 , 0 , 0 , 0 , 0 , 8 , 9 , 10 , 11 , 12 , 0 , 0 , 8*0 , 0 , 
     &     13 , 14 , 15 , 16 , 17 , 18 , 19 , 8*0 , 0 , 20 , 6*0 , 0 , 
     &     21 , 22 , 5*0 , 0 , 23 , 24 , 25 , 4*0 , 8*0 , 8*0/
 
      DATA a/.0108 , 1.8267 , 2.3196 , 0.0 , 0.0320 , -.8806 , .4134 , 
     &     0.0 , -.0036 , 0.0 , .0061 , -2.8425 , 0.0 , .0129 , 3.6781 , 
     &     -.0254 , -.0141 , 19.928 , 5.4751 , 1.2044 , .0219 , .7086 , 
     &     -.0219 , 3.2163 , .1203/
 
      DATA b/ - .1075 , 4.1012 , 10.7328 , .6310 , -.6624 , 11.2406 , 
     &     -4.6319 , .0238 , .7519 , 21.879 , .2269 , .2283 , 0.0 , 
     &     -.1779 , 14.1481 , 5.5365 , 33.8479 , 235.0536 , 203.9751 , 
     &     -4.6836 , -.4528 , -3.1083 , .4364 , -12.0571 , -2.690/
 
      DATA c/.2810 , 4.8443 , 6.8830 , .1990 , 4.3191 , 30.7066 , 
     &     25.9172 , .0659 , 1.5252 , 16.2730 , 32.1419 , 40.4072 , 0. , 
     &     .9353 , 17.1175 , 17.0727 , 43.1608 , 152.5096 , 86.9016 , 
     &     7.6620 , 2.5427 , 7.0422 , .0684 , 16.2118 , 19.1943/
 
      DATA d/ - .0193 , .2261 , -.1824 , -.0197 , .0003 , -1.1721 , 
     &     -2.2290 , .0349 , -.0838 , -.7020 , 1.9939 , -3.4956 , 0. , 
     &     -.0682 , -.5017 , -.7225 , -1.6072 , 9.1413 , -7.4568 , 
     &     -.5930 , -.1678 , .5998 , -.0032 , -.5886 , -.1479/
 
      DATA f/ - .1127 , .5960 , .4101 , .4398 , .5946 , .6127 , .2360 , 
     &     .5334 , .2769 , 1.1899 , -.0646 , 1.7558 , 0. , .4516 , 
     &     .2313 , .1702 , .1942 , .1282 , 2.5145 , 1.6260 , .2276 , 
     &     .4194 , .1342 , .5613 , .1118/
 
      ALFLO = 0.
      IF ( J.EQ.1 .OR. J.GT.8 ) RETURN
      IF ( N.GT.14 ) RETURN
 
      t4 = T*.0001
 
      IF ( t4.LT.0.1 .OR. t4.GT.6. ) RETURN
 
      IF ( N.EQ.6 .AND. J.EQ.2 .AND. t4.LT.0.2 ) RETURN
      IF ( N.EQ.8 .AND. J.EQ.4 .AND. t4.LT.0.2 ) RETURN
      IF ( N.EQ.8 .AND. J.EQ.6 .AND. t4.LT.0.4 ) RETURN
      IF ( N.EQ.10 .AND. J.EQ.8 .AND. t4.LT.0.2 ) RETURN
      IF ( N.EQ.12 .AND. J.EQ.2 .AND. t4.LT.0.15 ) RETURN
      IF ( N.EQ.14 .AND. J.EQ.3 .AND. t4.LT.0.15 ) RETURN
 
      ij = iion(J,N)
      IF ( ij.EQ.0 ) RETURN
 
      ALFLO = 1.0E-12*(a(ij)/t4+b(ij)+c(ij)*t4+d(ij)*t4*t4)
     &        *EXP(-f(ij)/t4)/t4**1.5
 
      RETURN
      END
 
c      FUNCTION ALFLO(N,J,T)
C  LOW TEMPERATURE DIELECTRONIC RECOMBINATION FROM NUSSBAUMER AND STOREY
C  C THROUGH O AND T < 60000 K
 
c      DIMENSION IION(8,3),A(12),B(12),C(12),D(12),F(12)
c        dimension asi(4),bsi(4),csi(4),dsi(4),fsi(4)
c      DATA IION/0,1,2,3,0,0,0,0, 0,4,5,6,7,0,0,0, 0,8,9,10,11,12,0,0/
c      DATA A/.0108,1.8267,2.3196,0.0,.032,-.8806,.4134,0.,-.0036,0.,
c     1  .0061,-2.8425/
c      DATA B/-.1075,4.1012,10.7328,.6310,-.6624,11.2406,-4.6319,.0238,
c     1  .7519,21.8790,.2269,.2283/
c      DATA C/.2810,4.8443,6.883,.199,4.3191,30.7066,25.9172,.0659,
c     1  1.5252,16.273,32.1419,40.407/
c      DATA D/-.0193,.2261,-.1824,-.0197,.0003,-1.1721,-2.229,.0349,
c     1  -.0838,-.7020,1.9939,-3.4956/
c      DATA F/-.1127,.5960,.4101,.4398,.5946,.6127,.2360,.5334,.2769,
c     1  1.1899,-.0646,1.7558/
c        DATA ASI/0.,-.0219,3.2163,0.1203/
c        DATA BSI/0.,.4364,-12.0571,-2.690/
c        DATA CSI/0.,.0684,16.2118,19.1943/
c        DATA DSI/0.,-.0032,-.5886,-.0099/
c        DATA FSI/0.,.1342,.5613,.1118/
c	ALFLO = 0.
c	IF (J .EQ. 1 .OR. N-J .LE. 1) RETURN
c	T4 = T *.0001
c	IF (T4 .GT. 6.0) RETURN
c	IF(T4 .LT. 0.1) RETURN
c        IF (N .EQ. 6 .AND. J .EQ. 2 .AND. T4 .LT. 0.2) RETURN
c        IF (N .EQ. 8 .AND. J .EQ. 4 .AND. T4 .LT. 0.2) RETURN
c        IF (N .EQ. 8 .AND. J .EQ. 6 .AND. T4 .LT. 0.4) RETURN
c       IF (N .EQ. 12 .OR. N .EQ. 14) GO TO 900
cIF (N .LT. 6 .OR. N .GT. 8) RETURN
cIJ = IION(J,N-5)
cALFLO = 1.0E-12 * (A(IJ)/T4 + B(IJ) + C(IJ)*T4 + D(IJ)*T4**2) *
c    1  EXP(-F(IJ) / T4) / T4 ** 1.5
c 900   IF (N .EQ. 14) GO TO 950
c       IF (J .NE. 2) RETURN
c       ALFLO = 1.0E-12 * (1.2044/T4 -4.6836 +7.6620*T4 -.5930*T4**2) *
c    1  EXP(-1.6260 / T4) / T4 ** 1.5
c       RETURN
c 950   IF (J .GT. 4) RETURN
c       ALFLO = 1.0E-12*(ASI(J)/T4 +BSI(J) +CSI(J)*T4 +DSI(J)*T4**2) *
c    1  EXP(-FSI(J) / T4) / T4 ** 1.5
c	RETURN
cEND
 
