c_______________________________________________________________
      real*8 function interpol_huntd(n,x,y,z)
      integer n
      real*8 x(n)
      real*8 y(n)
      real*8 z
      real*8 grad,d1,df,f,pow
      integer jl, jm, ju
      logical inc
c     
      inc = .false.
      if (x(n-1) .gt. x(1)) inc=.true.
      if (( inc .and.((z .lt. x(1)) .or. (z .gt. x(n-1)))) .or.
     $ (.not.(inc).and.(z .gt. x(1) .or. z .lt. x(n-1)))) then
c        write (6,*)"interpol_huntd: Asking for, min is, max is",z,
c     $        x(1),x(n-1)
c        write (6,*)"interpol_huntd: Cannot extrapolate"
        return
        endif
      jl = 0
      ju = n
      do while (ju - jl .gt. 1)
        jm = (ju + jl) / 2
        if ((z .gt. x(jm)) .eqv. inc) then
          jl = jm
        else 
          ju = jm
        endif
      enddo
c     /* ------	Z is sandwiched between JL and JU ------ */
      if ((x(jl) .gt. 0.).and.(x(ju).gt.0.).and.
     $    (y(jl) .gt. 0.).and.(y(ju) .gt. 0.)) then
        grad = (log10(y(ju)) - log10(y(jl))) /
     $      (log10(x(ju)) - log10(x(jl)))
        df = grad * (log10(z) - log10(x(jl)))
        d1 = log10(y(jl)) + df
        f = pow(10.d0, d1)
      else 
        f = y(jl)+(y(ju)-y(jl))*((z-x(jl))/(x(ju)-x(jl)))
      endif
      interpol_huntd=f
      return
      end
