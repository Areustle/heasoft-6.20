c      real*8 function voigte(vs,a)
      real*8 function voigtelorentzian(vs,a)
c
c     fake version does lorentzian
c
c     the integral of this function over vs (-infinity -> infinity) is 1
c
      real*8 a,vs
c
      voigtelorentzian=a/(vs*vs+a*a)/3.14
C
      Return
      end
