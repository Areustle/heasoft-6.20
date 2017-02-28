      FUNCTION ASCA_EEF(E,R,Off,Azi)
 
      REAL*4 E , R , Off , Azi , ASCA_EEF
 
      REAL*4 exr1 , exr2 , exo1 , exo2
      REAL*4 rr1 , cc , rr2
 
c     arguments
c     E (in)  : energy (keV)
c     r (in)  : diameter (mm) (0.9823 arc min per mm)
c     off(in) : offset angle (arcmin)
c     azi(in) : azimuthal angle (radian) [DUMMY]
c               This parameter is insensitive for the EEF.
c     asca_eef  : encircled enrgy function normalized at 12 mm (11.8 arc min)
c
c     FUNCTION
c                f(r,r1) - c*f(r,r2)
c         eef = ----------------------
c                f(12,r1)- c*f(12,r2)
c
c         where f(r,R) = 1-(r/R)*exp(-r/R)-exp(-r/R)
c
c     Version 1.0     coded by H. Awaki
c      parameters were calcurated using XRT response made by Tsusaka
      REAL r1(4) , r2(4) , r0 , c(4)
      DATA r1/0.37273 , 0.013695 , 0.0 , 0.0023062/
      DATA c/6.1508 , -0.047136 , -0.062403 , -0.0069433/
      DATA r2/2.2771 , 0.012846 , 0.02 , 0.002/
      DATA r0/12.0/
 
      rr1 = (r1(1)+r1(2)*Off) + (r1(3)+r1(4)*Off)*E
      cc = (c(1)+c(2)*Off) + (c(3)+c(4)*Off)*E
      rr2 = (r2(1)+r2(2)*Off) + (r2(3)+r2(4)*Off)*E
      exr1 = EXP(-R/rr1)
      exr2 = EXP(-R/rr2)
      exo1 = EXP(-r0/rr1)
      exo2 = EXP(-r0/rr2)
      ASCA_EEF = 0
      IF ( R.GT.0.0 ) THEN
         ASCA_EEF = (1.0-R/rr1*exr1-exr1) + cc*(1.0-R/rr2*exr2-exr2)
         ASCA_EEF = ASCA_EEF/((1.0-r0/rr1*exo1-exo1)
     &              +cc*(1.0-r0/rr2*exo2-exo2))
      ELSE
         ASCA_EEF = 0.0
      ENDIF
      RETURN
      END
