**==delt.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      FUNCTION DELT(N,J,L)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL DELT
      INTEGER i , J , L , N
C*** End of declarations inserted by SPAG
C  OUR VERY OWN MODIFICATIONS TO JACOBS
C  DENSITY DEPENDENCE EXTERNAL
C  TAKE 0.5 FOR MERTS ET AL   DELTA N .NE. 0 EXCEPT S
C  ASSUME 3D OF N,O,F INTERP BETWEEN C AND NE ISOSEQUENCES
C  THIS USES YOUNGER'S CLAIM THAT DELT=1 FOR HE-LIKE RESONANCE LINE;
C  TRY BELY-DUBAU??????
      DELT = 0.
      IF ( J.EQ.1 ) RETURN
      i = N - J + 1
      IF ( L.EQ.12 .AND. i.NE.10 ) THEN
         DELT = 1.
      ELSE
         IF ( L.EQ.1 .AND. i.GT.2 ) THEN
            IF ( i.NE.13 ) THEN
               DELT = 1.
               GOTO 100
            ENDIF
         ENDIF
         IF ( i.GE.19 ) THEN
            DELT = 0.25
         ELSEIF ( i.EQ.18 ) THEN
            DELT = 0.25
            IF ( L.LE.3 ) DELT = 1.
            IF ( N.NE.20 ) RETURN
            DELT = 0.
            IF ( L.EQ.6 ) DELT = 0.05
         ELSE
            IF ( i.LT.15 ) THEN
               IF ( i.EQ.2 ) THEN
                  IF ( L.EQ.4 .OR. L.EQ.5 ) DELT = 0.1
                  IF ( L.EQ.9 ) DELT = 1.0
               ELSEIF ( i.EQ.3 ) THEN
                  IF ( L.GE.2 .AND. L.LE.4 )
     &                 DELT = .5*(-.35+1.26*N/(N+11.))
               ELSEIF ( i.EQ.4 ) THEN
                  IF ( L.EQ.7 ) DELT = AMAX1(0.5*(-.55+1.50*N/(N+11.)),
     &                                 0.05)
               ELSEIF ( i.EQ.5 ) THEN
                  IF ( L.LE.6 ) DELT = 1.
                  IF ( L.EQ.4 .OR. L.EQ.5 )
     &                 DELT = 0.5*(-.54+2.2*N/(N+11.))
               ELSEIF ( i.EQ.6 ) THEN
                  IF ( L.LE.5 ) DELT = 1.
                  IF ( L.EQ.4 ) DELT = AMAX1(0.5*(-1.28+3.2*N/(N+11)),
     &                                 0.05)
               ELSEIF ( i.EQ.7 ) THEN
                  IF ( L.EQ.4 ) DELT = 1.
                  IF ( L.EQ.5 ) DELT = AMAX1(0.5*(-1.44+3.4*N/(N+11.)),
     &                                 0.05)
                  IF ( L.EQ.7 ) DELT = AMAX1(0.25*(-1.44+3.4*N/(N+11.)),
     &                                 0.025)
               ELSEIF ( i.EQ.8 ) THEN
                  IF ( L.LE.3 ) DELT = 0.5*(-1.60+3.6*N/(N+11.))
                  IF ( L.EQ.4 .OR. L.EQ.5 ) DELT = 1.
                  IF ( L.EQ.7 ) DELT = .25*(-1.6+3.6*N/(N+11.))
                  IF ( L.EQ.15 ) DELT = 1.
               ELSEIF ( i.EQ.9 ) THEN
                  IF ( L.LE.3 ) DELT = 0.5*(-1.75+3.8*N/(N+11.))
                  IF ( L.EQ.5 ) DELT = 1.
                  IF ( L.EQ.6 ) DELT = 0.25*(-1.75+3.8*N/(N+11.))
               ELSEIF ( i.EQ.10 ) THEN
                  IF ( L.LE.2 ) DELT = 1.
                  IF ( L.GE.3 .AND. L.LE.5 )
     &                 DELT = 0.5*(-1.9+4.0*N/(N+11.))
                  IF ( L.GE.6 .AND. L.LT.8 )
     &                 DELT = .25*(-1.9+4.*N/(N+11.))
                  IF ( L.EQ.14 ) DELT = 1.
               ELSEIF ( i.EQ.11 ) THEN
                  IF ( L.EQ.2 .OR. L.EQ.3 .OR. L.EQ.6 ) DELT = .25
               ELSEIF ( i.EQ.12 ) THEN
                  IF ( L.EQ.2 ) DELT = .25
               ELSEIF ( i.EQ.13 ) THEN
                  IF ( L.EQ.2 .OR. L.GE.9 ) DELT = 1.
                  IF ( L.EQ.3 .OR. L.EQ.5 ) DELT = .5
                  IF ( L.EQ.7 ) DELT = 0.25
               ELSEIF ( i.EQ.14 ) THEN
                  GOTO 20
               ELSE
C   BURGESS & TWORKOWSKI ARE IN ALPHADI
                  IF ( L.EQ.1 ) DELT = 1.
                  IF ( L.GT.1 ) DELT = 0.1
                  IF ( L.EQ.5 ) DELT = 0.
               ENDIF
               GOTO 100
            ENDIF
 20         IF ( L.LE.2 ) DELT = 1.
            IF ( L.EQ.3 .OR. L.EQ.4 ) DELT = .25
         ENDIF
      ENDIF
 100  RETURN
      END
