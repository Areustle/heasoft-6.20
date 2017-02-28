      subroutine fact(n,x)
c
c to calculate the factorial of an integer n.  the output x is
c the natural log of n factorial.
c
      implicit none
      real*8 x
      integer i,n
c
      x=0.
      if(n.ne.0) then
      do  i=1,n
        x=x+log(float(i))
        enddo
      endif
c
      return
      end
