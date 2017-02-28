**==compbb.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
      SUBROUTINE COMPBB(Ear,Ne,Param,Ifl,Photar,Photer)
      IMPLICIT NONE
 
      INTEGER Ne , Ifl
      REAL Ear(0:Ne) , Param(3) , Photar(Ne) , Photer(Ne)
 
C     Param(1) = Tbb
C     Param(2) = Te
C     Param(3) = tau
 
C     Originally coded by Kazu Mitsuda for
C     ISAS SPFD on FACOM mainframe  Ported to xspec format
C     by Ken Ebisawa. The architecture specific
C     numerical library (FUJITSU SSL) was removed.
C
C     Last modified on 1999-07-21.

C     Modification on 2004-02-24 by Ken Ebisawa
C     The energy range of the model calculation were hard coded in
C     the previous version as between 0.1 keV and 70 keV. 
C     (This was adequate for Tenma and Ginga!)
C     Now, these values are taken from Ear(0) and Ear(Ne), namely,
C     lower and upper boundaries of the input energy array.
 
      DOUBLE PRECISION fl_low , fl_up, E0, Em
      INTEGER i

c included to suppress compilation warning
      i = ifl

c this model has no errors
      DO i = 1, Ne
         Photer(i) = 0.0
      ENDDO

      E0 = dble(ear(0))
      Em = dble(ear(ne))

      CALL CMPBBK(DBLE(Param(1)),DBLE(Param(2)),DBLE(Param(3)),
     &            DBLE(Ear(0)),fl_low,E0,Em)
      DO i = 1 , Ne
         CALL CMPBBK(DBLE(Param(1)),DBLE(Param(2)),DBLE(Param(3)),
     &               DBLE(Ear(i)),fl_up,E0,Em)
         Photar(i) = SNGL((fl_low+fl_up)*(Ear(i)-Ear(i-1))/2.0d0)
         fl_low = fl_up
      ENDDO
 
      END
**==cmpbbk.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      SUBROUTINE CMPBBK(Ts,Te,Tau,E,Fl,E0,Em)
 
      IMPLICIT NONE
      DOUBLE PRECISION Ts , Te , Tau , E , Fl
      DOUBLE PRECISION FLUxs(0:511) 

      INTEGER N2
      DOUBLE PRECISION E0 , DX, Em
 
      INTEGER j
      INTEGER icon , i
      DOUBLE PRECISION x(0:511) , c(512) , v
      DOUBLE PRECISION ts1 , te1 , tau1, e01, em1

      LOGICAL qnewe

      SAVE ts1, te1, tau1
      SAVE x, c, FLUxs
      SAVE DX, N2

      DATA ts1/0.0/ , te1/0.0/ , tau1/ - 0.01/
      DATA e01/-1.0/, em1/-1.0/


      IF ( Ts.NE.ts1 .OR. Te.NE.te1 .OR. Tau.NE.tau1 .OR.
     &     E0.NE.e01 .OR. Em.NE.em1 ) THEN
         IF ( Tau.NE.tau1 ) CALL SETSPB(Tau)
         qnewe = .FALSE.
         IF ( E0 .NE. e01 .OR. Em .NE. em1 ) qnewe = .TRUE.
         CALL SCATS(Ts,Te,ts1,te1,FLUxs,E0,DX,N2,Em,qnewe)

         ts1 = Ts
         te1 = Te
         tau1 = Tau
         e01 = E0
         em1 = Em
         DO j = 0 , N2 - 1
            x(j) = DX*j
         ENDDO
         CALL DBIC3(FLUxs,N2,c,icon)
         IF ( icon.NE.0 ) WRITE (6,*) 'DBIC3 RETURN CODE:' , icon
      ENDIF

      v = LOG(E/E0)
      i = INT(v/DX) + 1
      IF ( i.LT.1 .OR. i.GE.N2 ) THEN
         Fl = 0.0
      ELSE
         CALL DBIF3(x,N2,c,v,i,Fl,icon)
         IF ( icon.NE.0 ) WRITE (6,*) 'DBIF3 RETURN CODE:' , icon
      ENDIF

cd
cd      WRITE(6,*) 'E,V,I,FL=',E,V,I,FL
cd
 
      RETURN
      END
**==scats.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
 
      SUBROUTINE SCATS(Ts,Te,Ts1,Te1,FLUxs,E0,DX,N2,Em,Qnewe)
      IMPLICIT NONE
      DOUBLE PRECISION Ts , Te , Ts1 , Te1, E0, DX
      DOUBLE PRECISION FLUxs(0:511)
      INTEGER N2
      LOGICAL Qnewe
 
C     SUBROUTINE TO CALCULTE COMPTONIZED BLACK BODY SPECTRUM
C     BY FFT.  THE FORMULA FOR THE ENERGY DISTRIBUTION OF THE ONCE-
C     SCATTERED PHOTON IS GIVEN BY NEGLECTING THE DEPENDENCE ON THE
C     SCATTERING ANGLE ON THE ELECTRON REST FRAME.
C     THE DITRIBUTION IS AVERAGED FOR ALL INCIDENT AND SCATTERING ANGLE
C     OF THE PHOTON.  FOR THE ONCE SCATTERED PHOTON THE DISTRIBUTION
C     FOR THE FRONT SCATTERING IS USED. THIS IS VALID WHEN THE ORIGINAL
C     PHOTON COMES FROM THE BOTTOM OF THE HOT GAS, SO THE MOST OF ONCE
C     SCATTERED PHOTON EXPERINCED FRONT SCATTERING.
C     ESCAPE PROBABILTY OF N-TIME SCATTERED PHOTON
C     SHOULD BE GIVEN BY THE SUBROUTINE SETSPB STORED IN
C     'SBSG010.SCAT.FORT77(PROB)'
C     THE POWER LAW DEPENDENCE OF PROBABILTY ON TIMES OF SCATTERING
C     N IS ASSUMED WHEN N IS LARGER THAN AN CERTAIN VALUE WHICH DEPENDS
C     ON TAU.
C     BY K.MITSUDA
 
C     <<< CAUTION >>>
C     THE NAMES OF THE SUBROUTINES AND COMMONS ARE ALMOST THE SAME AS THOSE
C     OF THE MSCATFFT, SO THIS PROGRAM CANNOT BE USED AT THE SAME TIME WITH
C     THE PROGRAM 'MSCATFFT'.
 
C     OUTPUT SPECTRUM IS STORED IN FLUXS(0:511)
C     FOR THE NORMALIZATION OF 'FLUXS', PLEASE SEE SUBROUTINE RESFFT.
 
      COMMON /PROBAB/ SPB
      DOUBLE PRECISION SPB(0:50)
      COMMON /PROBAA/ RATspb
      DOUBLE PRECISION RATspb
      COMMON /MAXTIM/ FMAx
      INTEGER FMAx
      DOUBLE PRECISION GN , FLUx0(0:2047) , F0R(0:2047) , F0I(0:2047) , 
     &       GFR(0:2047) , GFI(0:2047) , FFR(0:2047) , 
     &       FFI(0:2047) , FFR1(0:2047) , FFI1(0:2047)
      DOUBLE PRECISION rr , ii
      INTEGER N , N1
 
c      DOUBLE PRECISION    FNR(0:2047),FNI(0:2047),W,EPS/0.003/,NORM
      DOUBLE PRECISION fnr(0:2047) , fni(0:2047) , w , norm
      DOUBLE PRECISION xm , em

c      INTEGER J,F,CONVFG
      INTEGER j , f
      INTEGER icon

      SAVE GN , FLUx0 , F0R , F0I , GFR , GFI , FFR , FFI , FFR1 , FFI1
      SAVE N
 
c      N=2048
      N = 512
c      N2=512
      N2 = 128

      xm = LOG(em/E0)
      DX = xm/N2
      N1 = N - 1
 
      CALL RESFFT(Ts,Te,Ts1,Te1,GN,FLUx0,F0R,F0I,GFR,GFI,FFR,FFI,FFR1,
     &            FFI1,E0,DX,Qnewe,N2,N1,N)
 
 
      norm = GN*SPB(1)
      DO j = 0 , N1
         fnr(j) = FFR1(j)*norm
         fni(j) = FFI1(j)*norm
      ENDDO

      DO f = 2 , FMAx
         DO j = 0 , N1
            w = GFR(j)*FFR(j) - GFI(j)*FFI(j)
            FFI(j) = GFI(j)*FFR(j) + GFR(j)*FFI(j)
            FFR(j) = w
         ENDDO
         norm = GN**f*SPB(f)
         IF ( f.LT.FMAx ) THEN
            DO j = 0 , N1
               fnr(j) = fnr(j) + FFR(j)*norm
               fni(j) = fni(j) + FFI(j)*norm
            ENDDO
         ELSE
            DO j = 0 , N1
               rr = 1.0D0 - GFR(j)*RATspb*GN
               ii = -GFI(j)*RATspb*GN
               norm = GN**f*SPB(f)/(rr*rr+ii*ii)
               fnr(j) = (fnr(j)+(FFR(j)*rr+FFI(j)*ii)*norm)/N
               fni(j) = (fni(j)+(FFI(j)*rr-FFR(j)*ii)*norm)/N
            ENDDO
         ENDIF
      ENDDO
 
C     ... FFT:  FNR,FNI (input) => FNR,FNI (output)
C     real imag.         real imag.
      CALL DCFTN(fnr,fni,N,-1,icon)
      IF ( icon.NE.0 ) THEN
         WRITE (6,*) 'DCFTN (FNR,FNI,)  RETURN CODE:' , icon
         STOP
      ENDIF
      CALL DPNR(fnr,fni,N,-1,icon)
C     ... FFT output.  with FNR and FNI in order of increasing wave number

      DO j = 0 , N2 - 1
         FLUxs(j) = FLUx0(j)*SPB(0) + fnr(j)
      ENDDO
      RETURN
C      DEBUG SUBCHK
      END
**==resfft.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      SUBROUTINE RESFFT(Ts,Te,Ts1,Te1,GN,FLUx0,F0R,F0I,GFR,GFI,FFR,FFI,
     &                  FFR1,FFI1,E0,DX,Qnewe,N2,N1,N)
      IMPLICIT NONE
      DOUBLE PRECISION Ts , Te , Ts1 , Te1
      DOUBLE PRECISION GN , FLUx0(0:2047) , F0R(0:2047) , F0I(0:2047) , 
     &       GFR(0:2047) , GFI(0:2047) , FFR(0:2047) , 
     &       FFI(0:2047) , FFR1(0:2047) , FFI1(0:2047)
      INTEGER N , N1 , N2
      DOUBLE PRECISION E0 , DX
      LOGICAL Qnewe

      DOUBLE PRECISION mcc, pi, ONCEF
      PARAMETER(mcc=511.0D0, pi=3.14159265358979D0)
      PARAMETER (ONCEF=7.0D0/16.0D0)
 
      INTEGER j , icon
      DOUBLE PRECISION e , x , tm, w
      DOUBLE PRECISION gfr1(0:2047) , gfi1(0:2047)
      DOUBLE PRECISION tm2 , stm2 , spi , z , ae , norm , itm2 , istm2
      DOUBLE PRECISION ERF_CMPBB , ERFC_COMPBB

      SAVE gfr1, gfi1

 
      IF ( Te1.NE.Te .OR. Qnewe ) THEN
         GN = DX
         Te1 = Te
         tm = Te/mcc
         tm2 = tm*2.0D0
         stm2 = SQRT(tm2)
         spi = SQRT(pi)
         istm2 = 1.0D0/stm2
         itm2 = 1.0D0/tm2
         norm = ERF_CMPBB(istm2) - 2.0D0*EXP(-itm2)/stm2/spi
         DO j = 0 , N1
            IF ( j.LT.N/2 ) THEN
               x = DX*j
            ELSE
               x = DX*(j-N)
            ENDIF
            e = EXP(x)
            ae = ABS(e-1)/(e+1)
            z = ae/stm2
C     ... BETA = 0->1
            GFR(j) = (e+1.0D0)/(2.0D0*stm2)
     &               *((EXP(-z*z)*(1.0D0-tm2)-(ae-tm2)*EXP(-itm2))
     &               /spi-z*(ERFC_COMPBB(z)-ERFC_COMPBB(istm2))
     &               *(1.0D0-tm2/2.0D0))
            GFR(j) = GFR(j)/norm
            GFI(j) = 0.0
         ENDDO
         CALL DCFTN(GFR,GFI,N,1,icon)
         CALL DPNR(GFR,GFI,N,1,icon)
c      WRITE(6,*) 'FFT OF RESPONSE FUNCTION OF SCATTERING ENDED'
c      WRITE(6,*) '---GFR---'
c      WRITE(6,'(10D13.5)') (GFR(J),J=0,N1,10)
c      WRITE(6,*) '---GFI---'
c      WRITE(6,'(10D13.5)') (GFI(J),J=0,N1,10)
C     ... RESPONCE FOR THE FRONT SCATTERING
         tm = Te/mcc*ONCEF
         tm2 = tm*2.0D0
         stm2 = SQRT(tm2)
         istm2 = 1.0D0/stm2
         itm2 = 1.0D0/tm2
         norm = ERF_CMPBB(istm2) - 2.0D0*EXP(-itm2)/stm2/spi
         DO j = 0 , N1
            IF ( j.LT.N/2 ) THEN
               x = DX*j
            ELSE
               x = DX*(j-N)
            ENDIF
            e = EXP(x)
            ae = ABS(e-1)/(e+1)
            z = ae/stm2
C     ... BETA = 0->1
            gfr1(j) = (e+1.0D0)/(2.0D0*stm2)
     &                *((EXP(-z*z)*(1.0D0-tm2)-(ae-tm2)*EXP(-itm2))
     &                /spi-z*(ERFC_COMPBB(z)-ERFC_COMPBB(istm2))
     &                *(1.0D0-tm2/2.0D0))
            gfr1(j) = gfr1(j)/norm
            gfi1(j) = 0.0
         ENDDO
         CALL DCFTN(gfr1,gfi1,N,1,icon)
         CALL DPNR(gfr1,gfi1,N,1,icon)
      ENDIF
 
      IF ( Ts1.NE.Ts .OR. Qnewe ) THEN
         Ts1 = Ts
         DO j = 0 , N2 - 1
            x = j*DX
            e = E0*EXP(x)
            w = e/Ts
            IF ( w.LT.170.0 ) THEN
               FLUx0(j) = e*e/(EXP(w)-1.0)
            ELSE
               FLUx0(j) = e*e*EXP(-w)
            ENDIF
C     Normalization photons/cm2/s/keV for R=1km at D=10kpc
            FLUx0(j) = FLUx0(j)*1.04E-3
            F0R(j) = FLUx0(j)
            F0I(j) = 0.0
         ENDDO
         DO j = N2 , N1
            FLUx0(j) = 0.0
            F0R(j) = FLUx0(j)
            F0I(j) = 0.0
         ENDDO
         CALL DCFTN(F0R,F0I,N,1,icon)
         IF ( icon.NE.0 ) THEN
            WRITE (6,*) 'DCFTN (F0R,F0I,)  RETURN CODE:' , icon
            STOP
         ENDIF
         CALL DPNR(F0R,F0I,N,1,icon)
C     WRITE(6,*) 'FFT OF B-B SPECTRUM ENDED'
C     WRITE(6,'(10D13.5)') (F0R(J),J=0,N1,50)
C     WRITE(6,'(10D13.5)') (F0I(J),J=0,N1,50)
 
      ENDIF
      DO j = 0 , N1
         FFR(j) = GFR(j)*F0R(j) - GFI(j)*F0I(j)
         FFI(j) = GFI(j)*F0R(j) + GFR(j)*F0I(j)
         FFR1(j) = gfr1(j)*F0R(j) - gfi1(j)*F0I(j)
         FFI1(j) = gfi1(j)*F0R(j) + gfr1(j)*F0I(j)
      ENDDO
      RETURN
C      DEBUG SUBCHK
      END
**==setspb.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
C     ESCAPE PROBABILTY FOR N-TIMES SCATTERED PHOTON  (VER 2.2)
C     ASSUMING ISOTOROPIC SCATTERING (CROSS SECTION= SOLID ANGLE/4PI)
C     AND PLANE PARARELL GEOMETRY AND ISOTOROPIC PHOTON SOURCE
C     AT THE BOTTOM SURFACE OF THE HOT PLASMA CLOUD.
 
C     COPY RIGHT  K.MITSUDA  (ISAS)    1985  AUG.
 
C     SETSPB SETS THE ESCAPE PROBABILY IN /PROBAB/ SPB(0:50),
C     WHERE SPB(J) IS THE PROBABILY FOR J-TIMES SCATTERED PHOTON.
C     INPUT PARAMETER TAU IS THE OPTICAL DEPTH, SHOULD BE < 90.0 .
C     RATSPB= LIMIT(N->INFINITE) SPB(N+1)/SPB(N)
 
      SUBROUTINE SETSPB(Tau)
      IMPLICIT NONE
      DOUBLE PRECISION Tau
      COMMON /PROBAB/ SPB
      DOUBLE PRECISION SPB(0:50)
      COMMON /PROBAA/ RATspb
      DOUBLE PRECISION RATspb
      COMMON /MAXTIM/ FMAx
      INTEGER FMAx
 
      COMMON /OPTDEP/ TAU1
      DOUBLE PRECISION TAU1
      COMMON /CURDEP/ XX
      DOUBLE PRECISION XX
      EXTERNAL SPB1 , J2A , J2B , SPBN , JNA , JNB , E3
      DOUBLE PRECISION SPB1 , J2A , J2B , SPBN , JNA , JNB , E3
      DOUBLE PRECISION x1, x2 , xd
      DOUBLE PRECISION j(0:99) , w1 , w2
      INTEGER icon , nbin , n
      COMMON /SPLINE_COMPBB/ X , C , NBIn1 , MB
      DOUBLE PRECISION X(0:99) , C(100)
      INTEGER NBIn1 , MB , i
      DOUBLE PRECISION ratc0 , ratc1 , rata
      DATA nbin/20/
      DATA x1/0.0D0/ 
      DATA ratc0/ - 4.0711636/ , ratc1/ - 0.089848064/ , rata/0.6524/
 
C     I cannot find this subroutine (Ken Ebisawa 97/05/21)
c      CALL SETEPC
 
      IF ( Tau.LE.0.0 ) THEN
         SPB(0) = 1D0
         DO i = 1 , 50
            SPB(i) = 0D0
         ENDDO
      ELSE
         RATspb = (1.0D0+Tau**rata*EXP(ratc0+ratc1*Tau))
     &            *(1.0D0-(0.5D0-E3(Tau))/Tau)
 
         IF ( Tau.LE.0.07 ) THEN
            FMAx = 1
         ELSEIF ( Tau.LE.1.0 ) THEN
            FMAx = INT(9.3*Tau**0.499)
         ELSEIF ( Tau.LE.3.0 ) THEN
            FMAx = INT(9.3*Tau**0.855)
         ELSE
            FMAx = INT(7.586*Tau**1.041)
            IF ( FMAx.GT.50 ) FMAx = 50
         ENDIF
 
C     ... FMAX>=2 FOR THE CURRENT VERSION OF THE SUBROUTINE FOR FFT
         IF ( FMAx.LT.2 ) FMAx = 2
C     FMAX=50
         TAU1 = Tau
         MB = 5
         NBIn1 = nbin + 1
 
         SPB(0) = 2.0D0*E3(Tau)
 
         CALL DAQE(x1,Tau,SPB1,SPB(1),icon)
         IF ( icon.GT.10000 ) WRITE (6,*) ' DAQE FOR SPB1,  ICON = ' , 
     &                               icon
 
         X(0) = 0.0D0
         xd = Tau/nbin
         DO i = 1 , nbin - 1
            X(i) = X(i-1) + xd
         ENDDO
         X(nbin) = Tau
 
         DO i = 0 , nbin
            XX = X(i)
            CALL DAQE(x1,X(i),J2A,w1,icon)
            IF ( icon.GT.10000 ) WRITE (6,*) '  EAQE FOR J2A, ICON = ' , 
     &                                  icon
            x2 = Tau - X(i)
            CALL DAQE(x1,x2,J2B,w2,icon)
            IF ( icon.GT.10000 ) WRITE (6,*) '  EAQE FOR J2B, ICON = ' , 
     &                                  icon
            j(i) = (w1+w2)/4.0D0
         ENDDO
 
         CALL DBIC3(j,NBIn1,C,icon)
         IF ( icon.GT.0 ) WRITE (6,*) '  DBIC3 FOR J2, ICON = ' , icon
C     WRITE(6,*) 'CHKSPL: J2 '
C     CALL CHKSPL(J)
         CALL DAQE(x1,Tau,SPBN,SPB(2),icon)
         IF ( icon.GT.10000 ) WRITE (6,*) ' DAQE FOR SPB2,  ICON = ' , 
     &                               icon
         SPB(2) = SPB(2)*2.0D0
 
         DO n = 3 , FMAx
            DO i = 0 , nbin
               XX = X(i)
               CALL DAQE(x1,X(i),JNA,w1,icon)
               IF ( icon.GT.10000 ) WRITE (6,*)
     &               '  EAQE FOR JNA, ICON = ' , icon
               x2 = Tau - X(i)
               CALL DAQE(x1,x2,JNB,w2,icon)
               IF ( icon.GT.10000 ) WRITE (6,*)
     &               '  EAQE FOR JNB, ICON = ' , icon
               j(i) = (w1+w2)/2.0D0
            ENDDO
            CALL DBIC3(j,NBIn1,C,icon)
            IF ( icon.GT.0 ) WRITE (6,*) '  DBIC3 FOR JN, ICON = ' , 
     &                              icon
C     WRITE(6,*) 'CHKSPL: JN , N= ',N
C     CALL CHKSPL(J)
            CALL DAQE(x1,Tau,SPBN,SPB(n),icon)
            IF ( icon.GT.10000 ) WRITE (6,*)
     &                                  ' DAQE FOR SPBN,  ICON = ' , 
     &                                  icon
            SPB(n) = SPB(n)*2.0D0
         ENDDO
      ENDIF
 
c$$$      WRITE(6,*) '!PROBABILITIES:'
c$$$      WRITE(6,*) '!tau =', tau
c$$$      do i = 0, 50
c$$$         WRITE(6,'(i2,D18.9)') i, max(REAL(SPB(i)),1E-10)
c$$$      end do
 
      RETURN
      END
**==spb1.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      FUNCTION SPB1(X)
      IMPLICIT NONE
      DOUBLE PRECISION SPB1 , X
 
C     FUNCTION SUBROUTINE FOR THE ONCE SCATTERED PHOTON
 
      COMMON /OPTDEP/ TAU1
      DOUBLE PRECISION TAU1
      EXTERNAL E2_func
      DOUBLE PRECISION E2_func
      DOUBLE PRECISION x1
 
      x1 = TAU1 - X
      SPB1 = E2_func(x1)*E2_func(X)
 
      RETURN
      END
**==spbn.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
      FUNCTION SPBN(Z)
      IMPLICIT NONE
      DOUBLE PRECISION SPBN , Z
 
C     FUNCTION SUBROUTINE FOR THE N-TIMES SCATTERED PHOTON
 
      COMMON /OPTDEP/ TAU1
      DOUBLE PRECISION TAU1
      DOUBLE PRECISION x1 , f
      INTEGER i , isw
 
      COMMON /SPLINE_COMPBB/ X , C , NBIn1 , MB
      DOUBLE PRECISION X(0:99) , C(100)
      INTEGER NBIn1 , MB
      EXTERNAL E2_func
      DOUBLE PRECISION E2_func
      INTEGER icon
      DATA i/1/ , isw/0/
 
      x1 = TAU1 - Z
      CALL DBIF3(X,NBIn1,C,x1,i,f,icon)
      IF ( icon.GT.10000 ) WRITE (6,*) '  DBIF3 IN SPBN,  ICON = ' , 
     &                                 icon
      SPBN = f*E2_func(Z)
 
      RETURN
      END
**==j2a.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
      FUNCTION J2A(X)
      IMPLICIT NONE
      DOUBLE PRECISION J2A , X
 
C     FUNCTION SUBROUTINE FOR THE SOURCE FUNCTION OF
C     THE TWICE SCATTERED PHOTON, PART ONE
 
      COMMON /CURDEP/ XX
      DOUBLE PRECISION XX
      EXTERNAL E1 , E2_func
      DOUBLE PRECISION E1 , E2_func
      DOUBLE PRECISION x1
 
      x1 = XX - X
      J2A = E2_func(x1)*E1(X)
 
      RETURN
      END
**==j2b.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
      FUNCTION J2B(X)
      IMPLICIT NONE
      DOUBLE PRECISION J2B , X
 
C     FUNCTION SUBROUTINE FOR THE SOURCE FUNCTION OF
C     THE TWICE SCATTERED PHOTON, PART TWO
 
      COMMON /CURDEP/ XX
      DOUBLE PRECISION XX
      EXTERNAL E1 , E2_func
      DOUBLE PRECISION E1 , E2_func
      DOUBLE PRECISION x1
 
      x1 = XX + X
      J2B = E2_func(x1)*E1(X)
 
      RETURN
      END
**==jna.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
      FUNCTION JNA(Z)
      IMPLICIT NONE
      DOUBLE PRECISION JNA , Z
C     FUNCTION SUBROUTINE FOR THE SOURCE FUNCTION OF
C     THE N-TIMES SCATTERED PHOTON, PART ONE
 
      COMMON /SPLINE_COMPBB/ X , C , NBIn1 , MB
      DOUBLE PRECISION X(0:99) , C(100)
      INTEGER NBIn1 , MB
      COMMON /CURDEP/ XX
      DOUBLE PRECISION XX
      EXTERNAL E1
      DOUBLE PRECISION E1
 
      DOUBLE PRECISION x1 , f
      INTEGER i , isw
      INTEGER icon
      DATA i/1/ , isw/0/
 
      x1 = XX - Z
      CALL DBIF3(X,NBIn1,C,x1,i,f,icon)
      IF ( icon.GT.10000 ) WRITE (6,*) '  DBIF3 IN JNA,  ICON = ' , icon
      JNA = f*E1(Z)
 
      RETURN
      END
**==jnb.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
      FUNCTION JNB(Z)
      IMPLICIT NONE
      DOUBLE PRECISION JNB , Z
C     FUNCTION SUBROUTINE FOR THE SOURCE FUNCTION OF
C     THE N-TIMES SCATTERED PHOTON,  PART TWO
 
      COMMON /SPLINE_COMPBB/ X , C , NBIn1 , MB
      DOUBLE PRECISION X(0:99) , C(100)
      INTEGER NBIn1 , MB
      COMMON /CURDEP/ XX
      DOUBLE PRECISION XX
      EXTERNAL E1
      DOUBLE PRECISION E1
 
      DOUBLE PRECISION x1 , f
      INTEGER i , isw
      INTEGER icon
      DATA i/1/ , isw/0/
 
      x1 = XX + Z
      CALL DBIF3(X,NBIn1,C,x1,i,f,icon)
      IF ( icon.GT.10000 ) WRITE (6,*) '  DBIF3 IN JNB,  ICON = ' , icon
      JNB = f*E1(Z)
 
      RETURN
      END
 
      DOUBLE PRECISION FUNCTION E1(X)
C     E_n(x) = \int_1^\infinity{exp(-xs)s^{-n}}\;ds
      IMPLICIT NONE
      DOUBLE PRECISION X
      INTEGER i , n
c      double precision t
      DOUBLE PRECISION s_low , s_high , delta , step
 
      E1 = 0.0D0
      IF ( X.GE.100.0 ) THEN
         RETURN
      ELSEIF ( X.LT.0.3 ) THEN
         s_low = LOG(X)
         n = 10
         DO i = 1 , n
            s_high = s_low + 1.0D0
            E1 = E1 + (s_high-s_low)
     &           *0.5D0*(EXP(-(EXP(s_low)))+EXP(-(EXP(s_high))))
            s_low = s_high
         ENDDO
      ELSE
c$$$         n = 100
c$$$         step=1.1D0
c$$$         delta = 0.001D0
c$$$         do i = 1, n
c$$$            s_low  = 1.0D0+step**(i-1)*delta
c$$$            s_high = 1.0D0+step**(i  )*delta
c$$$            E1=E1+(s_high-s_low)*(exp(-s_low*x)
c$$$     $           / s_low+exp(-s_high*x)/s_high)/2.0D0
c$$$         end do
         n = 20
         step = 1.2D0
         s_high = 1.0D0
         DO i = 1 , n
            s_low = s_high
            delta = 0.07D0*step**(i-1)
            s_high = s_low + delta
            E1 = E1 + (s_high-s_low)
     &           *(EXP(-s_low*X)/s_low+EXP(-s_high*X)/s_high)/2.0D0
c            write(*,*) i, delta, s_low, s_high, E1
         ENDDO
      ENDIF
c      write(*,*) 'debug E1', X, E1
      END
**==e2.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      DOUBLE PRECISION FUNCTION E2_func(X)
      IMPLICIT NONE
      DOUBLE PRECISION X , E1
      EXTERNAL E1
 
      E2_func = EXP(-X) - X*E1(X)
 
      END
**==e3.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      DOUBLE PRECISION FUNCTION E3(X)
      IMPLICIT NONE
      DOUBLE PRECISION X , E2_func
      EXTERNAL E2_func
 
      E3 = 1.0D0/2.0D0*(EXP(-X)-X*E2_func(X))
 
      END
**==daqe.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      SUBROUTINE DAQE(Low_b,High_b,FUNCTION,Output,Icon)
      IMPLICIT NONE
      DOUBLE PRECISION Low_b , High_b , FUNCTION , Output
      INTEGER Icon
      EXTERNAL FUNCTION
 
C     Numerical integral.  Original routine is called from FUJUTSU SSL
C     library (source code is not free).
C
C     low_b  (in): lower bounary
C     high_b (in): upper bounary
C     function (in): function to integrate (external)
C     output (out): output integral value
C     icon (out):
 
      DOUBLE PRECISION dx , xm , xr , w(5) , x(5) , low_b_d , high_b_d , 
     &                 h , output_d
      INTEGER j , jj , n
      SAVE w , x
      DATA w/.2955242247D0 , .2692667193D0 , .2190863625D0 , 
     &     .1494513491D0 , .0666713443D0/
      DATA x/.1488743389D0 , .4333953941D0 , .6794095682D0 , 
     &     .8650633666D0 , .9739065285D0/
 
      n = 10
      IF ( High_b.NE.Low_b ) THEN
         h = (High_b-Low_b)/DBLE(n)
         Output = 0.0D0
         DO jj = 1 , n
            low_b_d = Low_b + h*(jj-1)
            high_b_d = Low_b + h*jj
            xm = 0.5D0*(high_b_d+low_b_d)
            xr = 0.5D0*(high_b_d-low_b_d)
            output_d = 0.0D0
            DO j = 1 , 5
               dx = xr*x(j)
               output_d = output_d + w(j)
     &                    *(FUNCTION(xm+dx)+FUNCTION(xm-dx))
            ENDDO
            Output = Output + output_d
         ENDDO
         Output = Output*xr
      ELSE
         Output = 0.0D0
      ENDIF
      Icon = 0
      END
**==erf_cmpbb.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      DOUBLE PRECISION FUNCTION ERF_CMPBB(X)
      IMPLICIT NONE
      EXTERNAL ERFC_COMPBB
      DOUBLE PRECISION X , ERFC_COMPBB
      ERF_CMPBB = 1.0D0 - ERFC_COMPBB(X)
      END
**==erfc_compbb.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      DOUBLE PRECISION FUNCTION ERFC_COMPBB(X)
      IMPLICIT NONE
      DOUBLE PRECISION X
      DOUBLE PRECISION a , b , PI
      PARAMETER (PI=3.14159265358979D0)
 
      INTEGER icon
      EXTERNAL FUNC_ERFC
      DOUBLE PRECISION FUNC_ERFC
 
      a = ATAN(X)
      b = PI/2.0D0
 
      CALL DAQE(a,b,FUNC_ERFC,ERFC_COMPBB,icon)
      ERFC_COMPBB = ERFC_COMPBB*2.0D0/SQRT(PI)
 
      END
**==func_erfc.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      DOUBLE PRECISION FUNCTION FUNC_ERFC(X)
      IMPLICIT NONE
      DOUBLE PRECISION X
      FUNC_ERFC = EXP(-(TAN(X))**2)/(COS(X))**2
 
      END
**==dcftn.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      SUBROUTINE DCFTN(Data_r,Data_i,N,Isign,Icon)
      IMPLICIT NONE
      INTEGER N , Isign , Icon
      DOUBLE PRECISION Data_r(N) , Data_i(N)
 
      INTEGER NMAX , i
      PARAMETER (NMAX=32768)
      DOUBLE PRECISION data(NMAX)
 
      DO i = 1 , N
         data(2*i-1) = Data_r(i)
         data(2*i) = Data_i(i)
      ENDDO
 
      CALL FOUR1(data,N,Isign)
 
      DO i = 1 , N
         Data_r(i) = data(2*i-1)
         Data_i(i) = data(2*i)
      ENDDO
      Icon = 0
      END
**==dpnr.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      SUBROUTINE DPNR(Data_r,Data_i,N,Isign,Icon)
      IMPLICIT NONE
      INTEGER N , Isign , Icon
      DOUBLE PRECISION Data_r(*) , Data_i(*)
 
C     dummy routine - most of this to suppress compiler warnings
      Icon = INT(Data_r(1))
      Icon = INT(Data_i(1))
      Icon = N
      Icon = Isign
      Icon = 0
      END
**==four1.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      SUBROUTINE FOUR1(Data,Nn,Isign)
 
C     Numerical Recipe FFT routine
C     double precision
 
      IMPLICIT NONE
      INTEGER Isign , Nn
      DOUBLE PRECISION Data(2*Nn)
 
      INTEGER i , istep , j , m , mmax , n
      DOUBLE PRECISION tempi , tempr , theta , wi , wpi , wpr , wr , 
     &                 wtemp
 
      n = 2*Nn
      j = 1
      DO i = 1 , n , 2
         IF ( j.GT.i ) THEN
            tempr = Data(j)
            tempi = Data(j+1)
            Data(j) = Data(i)
            Data(j+1) = Data(i+1)
            Data(i) = tempr
            Data(i+1) = tempi
         ENDIF
         m = n/2
 50      IF ( (m.GE.2) .AND. (j.GT.m) ) THEN
            j = j - m
            m = m/2
            GOTO 50
         ENDIF
         j = j + m
      ENDDO
 
      mmax = 2
 
 100  IF ( n.GT.mmax ) THEN
         istep = 2*mmax
         theta = 6.28318530717959D0/(Isign*mmax)
         wpr = -2.0D0*SIN(0.5D0*theta)**2
         wpi = SIN(theta)
         wr = 1.0D0
         wi = 0.0D0
         DO m = 1 , mmax , 2
            DO i = m , n , istep
               j = i + mmax
               tempr = wr*Data(j) - wi*Data(j+1)
               tempi = wr*Data(j+1) + wi*Data(j)
               Data(j) = Data(i) - tempr
               Data(j+1) = Data(i+1) - tempi
               Data(i) = Data(i) + tempr
               Data(i+1) = Data(i+1) + tempi
            ENDDO
            wtemp = wr
            wr = wr*wpr - wi*wpi + wr
            wi = wi*wpr + wtemp*wpi + wi
         ENDDO
         mmax = istep
         GOTO 100
      ENDIF
      RETURN
      END
**==dbic3.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      SUBROUTINE DBIC3(Indata,N,C,Icon)
      IMPLICIT NONE
      INTEGER N , Icon
      DOUBLE PRECISION Indata(N)
      DOUBLE PRECISION C(N)
c     dummy routine for interpolation no.1
c     C and XT are created for interpolation parameters, which are
c     used in DBIF3 to obtain interpolated values.
C     actually, just indata are copied to C.
      INTEGER i
 
      DO i = 1 , N
         C(i) = Indata(i)
      ENDDO
      Icon = 0
      END
**==dbif3.spg  processed by SPAG 4.50J  at 18:42 on 21 Jul 1999
 
      SUBROUTINE DBIF3(X,N,C,V,I_out,Fl,Icon)
      IMPLICIT NONE
      INTEGER N , I_out , Icon
      DOUBLE PRECISION X(N) , C(N), V , Fl
c     dummy routine for interpolation no.2
c     C created in DBIC3 is used in DBIF3 to obtain interpolated values.
      INTEGER i

      DO i = 1 , N - 2
         IF ( V.GE.X(i) .AND. V.LT.X(i+1) ) THEN
            I_out = i
            GOTO 100
         ENDIF
      ENDDO
      i = N - 1
      IF ( V.GE.X(i) .AND. V.LE.X(i+1) ) THEN
         I_out = i
         GOTO 100
      ENDIF
 
 
 
C     This is an error
      WRITE (*,*) 'DBIF3 error. v=' , V
      WRITE (*,*) 'x=' , (X(i),i=1,N)
      WRITE (*,*) 'c=' , (C(i),i=1,N)
      Icon = 9999
      RETURN
 
 100  Fl = (C(I_out+1)*(V-X(I_out))-C(I_out)*(V-X(I_out+1)))
     &     /(X(I_out+1)-X(I_out))
 
      Icon = 0
      END
 
 
 
 
