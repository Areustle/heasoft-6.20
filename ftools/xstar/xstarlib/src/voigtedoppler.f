      real*8 function voigtedoppler(vs,a)
c
c     fake version does doppler
c     the integral of this function over vs (-infinity -> infinity) is sqrt(pi)
c
      real*8 a,vs
c
      voigtedoppler=exp(-vs*vs)
c
      return
      end
