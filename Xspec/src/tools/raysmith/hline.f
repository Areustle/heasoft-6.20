**==hline.spg  processed by SPAG 4.50J  at 14:50 on 30 Jun 1995
      SUBROUTINE HLINE(Rhy,A,T,St,Beta,Alter)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL A , ALFly , ALFlyc , Alter , Beta , cc , chi , ex , EXIt , 
     &     HALf , HALfc , HBEt , HBEtc , om , P , rh , Rhy , SEATON , 
     &     SECONT , St
      REAL sum , T , tp , TWOp , y
C*** End of declarations inserted by SPAG
      COMMON /HLN   / EXIt(11) , HALf , HALfc , ALFly , ALFlyc , HBEt , 
     &                HBEtc , TWOp
      rh = Rhy/(1.+Rhy)
      y = .75*157890./T
      cc = ALOG((y+1.)/y) - 0.4/(y+1.)**2
C     CONTINUUM TO N .GE. 3
      EXIt(1) = 3.46*rh/St
C     CONTINUUM TO N .EQ. 2
      EXIt(2) = (45.0/St)*SECONT(Beta,2.)*rh/8.
C     CONTINUUM TO GROUND
      EXIt(3) = 45.0*SECONT(Beta,1.)*rh/St
      IF ( Alter.GE.1. ) EXIt(3) = 0.
C     TOTAL RECOMBINATION LINES
      chi = (.735+ALOG(Beta)+.3333/Beta)/2.
      IF ( Beta.LE..5 ) chi = 1.2*Beta + (.55+1.04*ALOG(Beta))
     &                        *Beta*Beta + (1.01*ALOG(Beta)-0.43)
     &                        *Beta**3
      chi = chi - P(Beta,1.)
      ex = .4288 + ALOG(Beta)/2. + 0.469*Beta**(-.333) - SEATON(Beta,1.)
      EXIt(4) = (2.06*1.607*13.6/St)
     &          *(ex+chi/Beta-SECONT(Beta,2.)/8.-.077)*rh
C     HBETA FROM RECOMBINATION
      EXIt(5) = .01275*EXP(-410./T)*rh/(.0001*T)**.937
C     LYMAN ALPHA FROM RECOMBINATION
      EXIt(6) = 2.06*1.6027*13.6*.75*ex*rh/St
C     TOTAL FROM COLLISIONAL EXCITATION
C     STILL USING OLD LYMAN ALPHA EXCITATION
      sum = .416*EXP(-.75*Beta)*1.195 + .07912*EXP(-.889*Beta)
     &      *1.076 + .02899*EXP(-.9375*Beta)
     &      *1.041 + .01394*EXP(-.96*Beta)
     &      *1.026 + .007799*EXP(-.972*Beta)*1.018
      EXIt(7) = 4.5E-6*1.6027E-12*(1.E23/SQRT(13.6))
     &          *sum/((1.+Rhy)*Beta**.12)
C     HBETA FROM COLLISIONAL EXCITATION : OLD H BETA
      sum = (.02899*.739*3/16.)*EXP(-.9375*Beta)
     &      *1.110 + .0745*.01394*EXP(-.96*Beta)
     &      *1.068 + .007799*.079*EXP(-.972*Beta)*1.047
      EXIt(8) = (4.5E-6*1.6027E11/SQRT(13.6))*sum/((1+Rhy)*Beta**.12)
C     LYMAN ALPHA FROM COLLISIONAL EXCITATION
      sum = .0791*EXP(-.889*Beta)*1.210 + .02899*EXP(-.9375*Beta)
     &      *1.110 + .01394*EXP(-.96*Beta)
     &      *1.068 + .007799*EXP(-.972*Beta)*1.047
      EXIt(9) = (4.5E-6*1.6027E-12*1.E23/SQRT(13.6))
     &          *sum/(Beta**.12*(1.+Rhy))
C  AGGARWAL FITS TO MCDOWELL AND CALLAWAY WITH OLD CASCADES
      tp = AMIN1(T,5.0E5)
      om = .5*(.335+1.45E-5*tp+1.39E-10*tp*tp-5.66E-15*tp*tp*tp)
      IF ( tp.GE.25000. ) om = .5*(.328+1.43E-5*tp-6.55E-12*tp*tp-
     &                         2.69E-18*tp*tp*tp)
      IF ( T.GE.5.0E5 ) om = 8.05*.276*cc
      EXIt(9) = EXIt(9) + 8.63E-6*om*10.2*1.6E11*EXP(-118000./T)
     &          /(SQRT(T)*(1.+Rhy))
C  H ALPHA EXCITATION; AGGARWAL ; CASE B, NO CASCADES
      om = (.175-1.31E-7*tp-4.08E-11*tp*tp+3.10E-15*tp*tp*tp)
      IF ( tp.GE.25000. ) om = (.138+2.50E-6*tp-4.43E-12*tp*tp+3.59E-18*
     &                         tp*tp*tp)
      EXIt(10) = 8.63E-6*om*1.89*1.6E11*EXP(-140300./T)
     &           /(SQRT(T)*(1.+Rhy))
C  TWO PHOTON FROM AGGARWAL;  NO CASCADES INCLUDED
      om = .5*(.195+1.28E-5*tp-4.69E-10*tp*tp+6.19E-15*tp*tp*tp)
      IF ( tp.GE.25000. ) om = .5*(.308+3.58E-7*tp+6.15E-13*tp*tp-
     &                         1.08E-18*tp*tp*tp)
      EXIt(11) = 8.63E-6*om*10.2*1.6E11*EXP(-118000./T)
     &           /(SQRT(T)*(1.+Rhy))
      HALf = HALf + EXIt(5)*A*(2.654+1730./T)
      HALfc = HALfc + EXIt(10)*A
      HBEt = HBEt + EXIt(5)*A
      HBEtc = HBEtc + EXIt(8)*A
      ALFly = ALFly + EXIt(6)*A
      ALFlyc = ALFlyc + EXIt(9)*A
      TWOp = TWOp + EXIt(11)*A
      RETURN
      END
 
