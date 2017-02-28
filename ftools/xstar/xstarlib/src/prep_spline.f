c_______________________________________________________________
c /* Rewritten from Numerical recipes.  Free version must be obtained */
c /* before release! */
 
      subroutine  prep_spline(x,y,n,y2)
c
      real*8 x(n)
      real*8 y(n)
      integer n
      real*8 y2(n)

      integer i,k
      real*8 p, qn, sig, un
      real*8 u(500)

      y2(1)=0.0
      u(1)=0.0

c      write (6,*)'in prep_spline',n
      do i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.0
        y2(i)=(sig-1.0)/p
        u(i)=(y(i+1)-y(i))/(x(i+1)-x(i)) - (y(i)-y(i-1))/(x(i)-x(i-1))
        u(i)=(6.0*u(i)/(x(i+1)-x(i-1))-sig*u(i-1))/p
c        write (6,*)i,x(i),y(i),sig,p,y2(i),u(i)
      enddo

      qn=0.0
      un=0.0
  
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.0)
c      write (6,*)un,qn,u(n-2),y2(n-2),y2(n-1)
      
      do k=n-1,1,-1
         y2(k)=y2(k)*y2(k+1)+u(k)
c         write (6,*)k,y2(k)
         enddo
         
c      free(u)
      return
      end
