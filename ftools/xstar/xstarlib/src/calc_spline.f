c_______________________________________________________________

      real*8 function calc_spline(xa, ya,y2a,n,x,lpri,lun11)
c 
      implicit none
      integer n,lpri,lun11
      real*8 xa(n)
      real*8 ya(n)
      real*8 y2a(n)
      real*8 x

      integer klo,khi,k
      real*8 h,b,a
      real*8 result

      klo=0
      khi=n

      do while (khi-klo .gt. 1) 
        k=(khi+klo)/2
c        write (6,*)k,khi,klo,xa(k),x
        if (xa(k) .gt. x) then
            khi=k
          else
            klo=k
          endif
        enddo
      h=xa(khi)-xa(klo)
      if (h.eq.0.)then
        write (6,*)"calc_spline: Bad x array input"
        stop
        endif

      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      result=a*ya(klo)+b*ya(khi)
     $ +((a*a*a-a)*y2a(klo)+(b*b*b-b)*y2a(khi))*(h*h)/6.0
      if (lpri.ne.0)
     $ write (lun11,*)'in calc_spline',x,n,result,khi,klo,n,
     $      ya(khi),ya(klo)

      calc_spline=result
      return
      end
