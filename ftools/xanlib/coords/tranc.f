      SUBROUTINE TRANC(Alpha,Delta,Alphap,Deltap,Theta,Thetap,Rinc)
c
c Spherical coordinate transformation
c     IMPLICIT REAL*8(a-h,o-z)
c
      REAL*8 inc , Alpha , Delta , Alphap , Deltap , Theta , Rinc
      REAL*8 pi , factor , t , tp , a , d , cosi , sini , at , cosat , 
     &       sinat
      REAL*8 sind , cosd , sindp , dp , top , bottom , atp , ap , Thetap
c
      DATA pi/3.1415926536D0/
      factor = pi/180.D0
      inc = Rinc
      t = Theta*factor
      tp = Thetap*factor
      a = Alpha*factor
      d = Delta*factor
      inc = inc*factor
      cosi = DCOS(inc)
      sini = DSIN(inc)
      at = a - t
      cosat = DCOS(at)
      sinat = DSIN(at)
      sind = DSIN(d)
      cosd = DCOS(d)
      sindp = sind*cosi - cosd*sini*sinat
      dp = DASIN(sindp)
      Deltap = dp/factor
      top = sind*sini + cosd*cosi*sinat
      bottom = cosd*cosat
      atp = DATAN2(top,bottom)
      ap = atp + tp
      IF ( DABS(ap-pi).GT.pi ) ap = ap - pi*DSIGN(2.D0,ap-pi)
      Alphap = ap/factor
      RETURN
      END
