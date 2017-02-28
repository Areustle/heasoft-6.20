      subroutine dfact(n,x)
c
c to calculate the factorial of an integer n.  the output x is
c the natural log of n factorial, in double precision.
c     author:  T. Kallman
c
      implicit none
      integer n,i
      real*8 x
c
      x=0.
      if(n.eq.0) return
      do i=1,n
       x=x+log(float(i))
       enddo
c
      return
      end
