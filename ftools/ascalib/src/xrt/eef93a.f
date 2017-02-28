      function eef(E,r,off,azi)
      real E,r,off,azi,eef
c     arguments
c     E (in)  : energy (keV)
c     r (in)  : diameter (mm)
c     off(in) : offset angle (arcmin)
c     azi(in) : azimuthal angle (radian) [DUMMY]
c               This parameter is insensitive for the EEF.
c     eef     : encircled energy function normalized at 12 mm
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

      real r1(4)
      real C (4)
      real r2(4)
      real r0
      data r1 /0.37273, 0.013695, 0.0     , 0.0023062/
      data C  /6.1508, -0.047136, -0.062403, -0.0069433/
      data r2 /2.2771,  0.012846, 0.02    , 0.002    /
      data r0 /12.0/


c      rr1 = (r1(1)+r1(2)*off)+(r1(3)+r1(4)*off)*E
c      CC  = (c (1)+c (2)*off)+(c (3)+c (4)*off)*E
c      rr2 = (r2(1)+r2(2)*off)+(r2(3)+r2(4)*off)*E
      rr1 = (r1(1)+r1(2)*off)+(r1(3)+r1(4)*off)*1.0
      CC  = (c (1)+c (2)*off)+(c (3)+c (4)*off)*1.0
      rr2 = (r2(1)+r2(2)*off)+(r2(3)+r2(4)*off)*1.0

      exr1=exp(-r/rr1)
      exr2=exp(-r/rr2)
      exO1=exp(-r0/rr1)
      exO2=exp(-r0/rr2)

      eef = 0
      if (r.ne.0) then
        eef = (1.0-r/rr1*exr1-exr1)+CC*(1.0-r/rr2*exr2-exr2)
        eef = eef/((1.0-r0/rr1*exO1-exO1)+CC*(1.0-r0/rr2*exO2-exO2))
      endif
      return
      end
