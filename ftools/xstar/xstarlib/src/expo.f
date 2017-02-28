      real*8 function expo(x)
c
      implicit none
c
      real*8 x,crit
c
      crit=600.
c      expo=exp(x)
c      return
c      crit=60.
c      if (x.lt.-crit) then
c        expo=1.e-24
c      else
c        xtmp=min(x,crit)
        expo=exp(min(max(x,-crit),crit))
c      endif
c
      return
      end
